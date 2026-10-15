# Resume a longer study of difficult conditional posteriors. Use the same private
# installed snapshot throughout; changing source does not change a running study.
source("notes/posterior-chains/run-benchmarks.R")
directory <- normalizePath("notes/posterior-chains")
result_file <- file.path(directory, "difficult-benchmarks.csv")
diagnostic_file <- file.path(directory, "difficult-diagnostics.csv")
chain_file <- file.path(directory, "difficult-chains.csv")
failure_file <- file.path(directory, "difficult-failures.csv")
completed <- if (file.exists(result_file)) read.csv(result_file) else NULL
rows <- if (is.null(completed)) list() else list(completed)
diagnostics <- if (file.exists(diagnostic_file)) list(read.csv(diagnostic_file)) else list()
chain_rows <- if (file.exists(chain_file)) list(read.csv(chain_file)) else list()
failures <- if (file.exists(failure_file)) list(read.csv(failure_file)) else list()
if (!is.null(completed)) stopifnot(
  all(completed$draws == as.integer(Sys.getenv("GRATIA_DIFFICULT_DRAWS", "16000"))),
  all(completed$burnin_per_chain == as.integer(Sys.getenv("GRATIA_DIFFICULT_BURNIN", "4000"))))
platform <- Sys.info()[["sysname"]]
for (case in c("sparse", "rare_poisson", "sparse_high_p")) {
  for (seed in 281:283) for (method in c("mgcv", "gratia", "family", "mala", "elliptical")) {
    if (!is.null(completed) && any(completed$case == case & completed$method == method &
      completed$seed == seed)) next
    temp <- tempfile("gratia-difficult-")
    dir.create(temp)
    result <- file.path(temp, "result.csv")
    stdout <- file.path(temp, "stdout.txt")
    stderr <- file.path(temp, "stderr.txt")
    status <- system2("/usr/bin/time", c(if (platform == "Darwin") "-l" else "-v",
      shQuote(file.path(R.home("bin"), "Rscript")),
      shQuote(file.path(directory, "difficult-benchmark.R")),
      case, method, as.character(seed), shQuote(result)), stdout = stdout, stderr = stderr,
      env = c("OMP_NUM_THREADS=1", "OPENBLAS_NUM_THREADS=1", "VECLIB_MAXIMUM_THREADS=1"))
    if (status != 0L) {
      message <- paste(c(readLines(stderr, warn = FALSE), readLines(stdout, warn = FALSE)), collapse = "\n")
      failures[[length(failures) + 1L]] <- data.frame(case = case, method = method,
        seed = seed, message = message)
      write.csv(do.call(rbind, failures), failure_file, row.names = FALSE)
      cat(sprintf("FAILED %s %s seed=%s: %s\n", case, method, seed, message))
      flush.console()
      unlink(temp, recursive = TRUE)
      next
    }
    row <- read.csv(result)
    row$process_peak_mb <- benchmark_peak_mb(readLines(stderr, warn = FALSE), platform)
    rows[[length(rows) + 1L]] <- row
    completed <- do.call(rbind, rows)
    for (suffix in c("diagnostics", "chains")) {
      detail <- read.csv(paste0(result, ".", suffix, ".csv"))
      detail$case <- case
      detail$method <- method
      detail$seed <- seed
      if (suffix == "diagnostics") diagnostics[[length(diagnostics) + 1L]] <- detail else {
        # MH and alternative prototypes expose different sampling information.
        columns <- c(".chain", "fixed_acceptance", "rw_acceptance", "elapsed", "backend",
          "retained", "burnin", "thin", "t_df", "rw_scale", "step_size", "evaluations", "case", "method", "seed")
        for (column in setdiff(columns, names(detail))) detail[[column]] <- NA
        chain_rows[[length(chain_rows) + 1L]] <- detail[, columns]
      }
    }
    write.csv(completed, result_file, row.names = FALSE)
    write.csv(do.call(rbind, diagnostics), diagnostic_file, row.names = FALSE)
    write.csv(do.call(rbind, chain_rows), chain_file, row.names = FALSE)
    cat(sprintf("%s %s seed=%s: %.3fs; coefficient Rhat %.4f; quantity Rhat %.4f\n",
      case, method, seed, row$total_seconds[1L], row$max_rhat[1L], row$max_rhat[2L]))
    flush.console()
    unlink(temp, recursive = TRUE)
  }
}
packages <- c("gratia", "mgcv", "Rcpp", "posterior", "mirai")
versions <- vapply(packages, function(package) as.character(utils::packageVersion(package)), character(1L))
writeLines(trimws(c(paste(names(versions), versions), "", capture.output(sessionInfo())), which = "right"),
  file.path(directory, "difficult-environment.txt"))
