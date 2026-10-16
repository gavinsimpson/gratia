# Paired timings compare the previous public calculation with current output.
# Run from the repository root after installing this branch and setting R_LIBS.
library("gratia")
library("mgcv")
source("notes/posterior-chains/lss-fixtures.R")
ns <- asNamespace("gratia")
legacy_response_draws <- function(context, betas, weights = NULL) {
  rd <- ns$missing_safe_rd(ns$fix_family_rd(context$family)$rd)
  wt <- ns$lss_sampling_weights(context, weights)
  sampler <- function(i) {
    response <- if (all(is.na(context$layout$map))) rep(NA_real_, NROW(context$X)) else {
      mu <- ns$family_prediction_parameters(context$family,
        ns$lss_predictors(context, betas[i, ]))
      rd(mu = mu, wt = wt, scale = context$model$sig2 %||% 1)
    }
    value <- tibble::tibble(.row = seq_len(NROW(context$X)), .draw = i,
      .parameter = "response", .response = as.numeric(response))
    ns$restore_prediction_table(value, context$layout)
  }
  dplyr::bind_rows(lapply(seq_len(NROW(betas)), sampler))
}
`%||%` <- function(x, y) if (is.null(x)) y else x
# Replace only the response-assembly call in a private copy of the public
# method. All validation, coefficient sampling and metadata handling are shared.
reference_environment <- new.env(parent = ns)
reference_environment$lss_response_draws <- legacy_response_draws
worker <- ns$do_posterior_samples
environment(worker) <- reference_environment
reference_environment$do_posterior_samples <- worker
reference <- getS3method("posterior_samples", "gam")
environment(reference) <- reference_environment
methods <- list(previous = reference, current = posterior_samples)
rows <- list()
repetitions <- as.integer(Sys.getenv("GRATIA_RESPONSE_REPETITIONS", "5"))
for (family in c("gaulss", "gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")) {
  fixture <- lss_benchmark_fixture(family)
  for (seed in 281:283) {
    b <- post_draws(fixture$model, n_draws = 1000, chains = 4, seed = seed)
    args <- list(model = fixture$model, data = fixture$data,
      method = "user", draws = b, seed = seed)
    old <- do.call(reference, args)
    new <- do.call(posterior_samples, args)
    stopifnot(identical(old, new))
    for (iteration in seq_len(repetitions)) {
      # Alternate execution order to reduce ordering effects.
      for (method in if (iteration %% 2L) names(methods) else rev(names(methods))) {
        gc()
        seconds <- system.time(value <- do.call(methods[[method]], args))[["elapsed"]]
        stopifnot(identical(value, new))
        rows[[length(rows) + 1L]] <- data.frame(family = family, seed = seed,
          repetition = iteration, method = method, fitting_rows = NROW(fixture$model$model),
          coefficients = length(coef(fixture$model)), evaluation_rows = NROW(fixture$data),
          draws = NROW(b), chains = 4L, seconds = seconds, exact_seeded_match = TRUE)
      }
    }
    cat(family, seed, "completed\n")
    flush.console()
    write.csv(do.call(rbind, rows), "notes/posterior-chains/response-performance.csv", row.names = FALSE)
  }
}
# Profile allocations for one representative posterior transformation. Rprofmem
# measures total allocation traffic, not the process's maximum resident memory.
fixture <- lss_benchmark_fixture("gaulss")
b <- post_draws(fixture$model, n_draws = 1000, chains = 4, seed = 281)
allocations <- lapply(names(methods), function(method) {
  path <- tempfile("gratia-response-allocations-")
  Rprofmem(path)
  value <- methods[[method]](fixture$model, data = fixture$data,
    method = "user", draws = b, seed = 281)
  Rprofmem(NULL)
  lines <- readLines(path)
  unlink(path)
  sizes <- suppressWarnings(as.numeric(sub(" .*", "", lines)))
  data.frame(method = method, allocated_mb = sum(sizes, na.rm = TRUE) / 1e6,
    allocation_count = sum(!is.na(sizes)), output_mb = as.numeric(object.size(value)) / 1e6)
})
write.csv(do.call(rbind, allocations), "notes/posterior-chains/response-allocations.csv", row.names = FALSE)
writeLines(c(capture.output(sessionInfo()), "", paste("repetitions", repetitions),
  "Timings exclude coefficient generation; output identity checked for every run.",
  "BLAS/OpenMP threads limited externally to one."),
  "notes/posterior-chains/response-environment.txt")
