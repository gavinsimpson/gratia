# Compare integrated intervals with the actual internal coefficient-draw prototype.
# Run outside the sandbox: Rscript this-file.R raw-directory [workers=8] [comma-separated cases].
args <- commandArgs(TRUE)
stopifnot(length(args) >= 1L)
directory <- normalizePath(args[1L], mustWork = FALSE)
workers <- if (length(args) >= 2L) as.integer(args[2L]) else 8L
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
functions_path <- normalizePath("notes/posterior-chains/smoothing-coverage-functions.R")
source(functions_path)
cases <- coverage_cases()
case_names <- if (length(args) >= 3L) strsplit(args[3L], ",", fixed = TRUE)[[1L]] else
  c("normal_known", "poisson", "nb", "beta")
cases <- cases[match(case_names, cases$case), , drop = FALSE]
stopifnot(!anyNA(cases$case))
configurations <- coverage_configurations()[c(2L, 5L), , drop = FALSE]
jobs <- unlist(lapply(seq_len(nrow(cases)), function(i)
  lapply(seq_len(nrow(configurations)), function(j) list(spec = cases[i, , drop = FALSE],
    cfg = configurations[j, , drop = FALSE]))), recursive = FALSE)
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
# These checks rerun all jobs; never overwrite an earlier protocol or its draws.
if (file.exists(file.path(directory, "protocol.rds")) ||
    length(list.files(directory, "draw-check[.]rds$")))
  stop("Draw-check output already exists; use a new directory to preserve the original results.")
saveRDS(list(cases = cases, configurations = configurations, draws = 10000L,
  components = 4096L, R = as.character(getRversion()),
  mgcv = as.character(utils::packageVersion("mgcv")),
  source_hashes = tools::md5sum(c("R/sampling-smoothing.R",
    "R/sampling-smoothing-family.R", "R/sampling-smoothing-audit.R",
    "notes/posterior-chains/smoothing-coverage-functions.R",
    "notes/posterior-chains/smoothing-coverage-draws.R")),
  library_paths = .libPaths()), file.path(directory, "protocol.rds"))
run <- function() {
  profile <- paste0("gratia-coverage-draw-check-", Sys.getpid())
  mirai::daemons(workers, .compute = profile)
  on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
  tasks <- mirai::mirai_map(jobs, function(job, functions_path, library_paths, directory) {
    .libPaths(library_paths); source(functions_path, local = .GlobalEnv)
    spec <- job$spec; cfg <- job$cfg
    fixture <- coverage_data(spec, 1L)
    fit <- coverage_fit(spec, fixture, cfg$engine, cfg$method)
    projection <- coverage_projection(fit, fixture)
    setup <- get("sampling_smoothing_setup", asNamespace("gratia"))(fit)
    integrated <- list(conditional = coverage_normal(coef(fit), vcov(fit), projection),
      unconditional = coverage_normal(coef(fit), vcov(fit, unconditional = TRUE), projection),
      mixture = coverage_mixture(setup, projection, 4096L, 50261015L))
    results <- list(); summaries <- list()
    for (s in c("conditional", "unconditional", "mixture")) {
      beta <- tryCatch({
        if (s == "mixture") get("sampling_smoothing_draws", asNamespace("gratia"))(
          setup, n_draws = 10000L, seed = 60261015L)$bs else
          withr::with_seed(60261015L, mgcv::rmvn(10000L, coef(fit),
            vcov(fit, unconditional = s == "unconditional")))
      }, error = function(e) structure(conditionMessage(e), class = "coverage_error"))
      if (inherits(beta, "coverage_error")) {
        summaries[[s]] <- data.frame(case = spec$case, engine = cfg$engine, method = cfg$method,
          sampler = s, status = as.character(beta), maximum_cdf_error = NA_real_,
          median_cdf_error = NA_real_, maximum_endpoint_error_in_sd = NA_real_)
        results[[s]] <- list(error = as.character(beta), integrated = integrated[[s]])
      } else {
        projected <- sweep(beta %*% t(projection$A), 2L, projection$offset, `+`)
        empirical <- t(apply(projected, 2L, quantile, probs = c(.025, .5, .975), names = FALSE))
        cdf <- vapply(seq_len(ncol(projected)), function(j)
          vapply(integrated[[s]][j, ], function(q) mean(projected[, j] <= q), numeric(1L)), numeric(3L))
        errors <- abs(cdf - c(.025, .5, .975))
        summaries[[s]] <- data.frame(case = spec$case, engine = cfg$engine, method = cfg$method,
          sampler = s, status = "OK", maximum_cdf_error = max(errors),
          median_cdf_error = median(errors), maximum_endpoint_error_in_sd =
            max(abs(empirical[, c(1L, 3L)] - integrated[[s]][, c(1L, 3L)]) /
              apply(projected, 2L, sd)))
        results[[s]] <- list(integrated = integrated[[s]], empirical = empirical, cdf = cdf)
      }
    }
    result <- list(case = spec$case, engine = cfg$engine, method = cfg$method,
      summaries = do.call(rbind, summaries), results = results)
    saveRDS(result, file.path(directory, paste(spec$case, cfg$engine, cfg$method, "draw-check.rds", sep = "-")))
    result$summaries
  }, .args = list(functions_path = functions_path, library_paths = .libPaths(), directory = directory), .compute = profile)
  last <- -1L
  repeat {
    done <- length(tasks) - sum(vapply(tasks, mirai::unresolved, logical(1L)))
    if (done != last) {cat(format(Sys.time(), "%H:%M:%S"), done, "of", length(tasks), "draw checks complete\n"); flush.console(); last <- done}
    if (done == length(tasks)) break
    Sys.sleep(2)
  }
  results <- tasks[]
  if (any(vapply(results, function(x) inherits(x, c("miraiError", "errorValue")), logical(1L)))) stop("A draw-check worker failed.")
  write.csv(do.call(rbind, results), file.path(directory, "smoothing-coverage-draw-check.csv"), row.names = FALSE)
}
run()
