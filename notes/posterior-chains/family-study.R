# Repeated comparisons of effective samples per second on the same fitted targets.
# Completed rows are resumed. Delete both output CSVs to start a fresh study.
# Run from the repository root with R_LIBS pointing at this branch's installation.
source("notes/posterior-chains/run-benchmarks.R")
directory <- normalizePath("notes/posterior-chains")
cases <- data.frame(
  family = c("poisson", "poisson", "binomial", "sparse", "multinomial", "gaulss",
    "cnorm", "tweedie", "grouped"),
  n_obs = c(2000L, 10000L, 10000L, 2000L, 2000L, 2000L, 2000L, 1000L, 2000L),
  basis_size = c(10L, 30L, 10L, 30L, 10L, 10L, 10L, 10L, 10L))
platform <- Sys.info()[["sysname"]]
if (!platform %in% c("Darwin", "Linux")) stop("Peak memory collection supports macOS and Linux.")
result_file <- file.path(directory, "family-benchmarks.csv")
coefficient_file <- file.path(directory, "family-coefficients.csv")
completed <- if (file.exists(result_file)) read.csv(result_file) else NULL
if (!is.null(completed)) {
  for (column in c("fitted_note", "posterior_note"))
    if (!column %in% names(completed)) completed[[column]] <- ""
}
if (!is.null(completed)) {
  stopifnot(all(completed$draws == as.integer(Sys.getenv("GRATIA_BENCH_DRAWS", "4000"))),
    all(completed$burnin_per_chain == as.integer(Sys.getenv("GRATIA_BENCH_BURNIN", "1000"))))
}
rows <- if (is.null(completed)) list() else list(completed)
coefficients <- if (file.exists(coefficient_file)) list(read.csv(coefficient_file)) else list()
for (i in seq_len(nrow(cases))) {
  methods <- if (cases$family[i] %in% c("poisson", "binomial", "sparse")) {
    c("mgcv", "gratia", "family")
  } else c("mgcv", "family")
  for (seed in 281:283) for (backend in methods) {
    if (!is.null(completed) && any(completed$family == cases$family[i] &
        completed$observations == cases$n_obs[i] & completed$backend == backend &
        completed$seed == seed)) next
    temp <- tempfile("gratia-family-benchmark-")
    dir.create(temp)
    result <- file.path(temp, "result.csv")
    stdout <- file.path(temp, "stdout.txt")
    stderr <- file.path(temp, "stderr.txt")
    args <- c(if (platform == "Darwin") "-l" else "-v",
      shQuote(file.path(R.home("bin"), "Rscript")),
      shQuote(file.path(directory, "family-benchmark.R")),
      shQuote(cases$family[i]), as.character(cases$n_obs[i]),
      as.character(cases$basis_size[i]), backend, as.character(seed), shQuote(result))
    status <- system2("/usr/bin/time", args, stdout = stdout, stderr = stderr,
      env = c("OMP_NUM_THREADS=1", "OPENBLAS_NUM_THREADS=1", "VECLIB_MAXIMUM_THREADS=1"))
    if (status != 0L) stop(paste(readLines(stderr), collapse = "\n"))
    row <- read.csv(result)
    row$process_peak_mb <- benchmark_peak_mb(readLines(stderr), platform)
    rows[[length(rows) + 1L]] <- row
    completed <- do.call(rbind, rows)
    coefficient <- read.csv(paste0(result, ".coefficients.csv"))
    coefficient$family <- row$family
    coefficient$observations <- row$observations
    coefficient$backend <- row$backend
    coefficient$seed <- row$seed
    coefficients[[length(coefficients) + 1L]] <- coefficient
    write.csv(do.call(rbind, rows), file.path(directory, "family-benchmarks.csv"), row.names = FALSE)
    write.csv(do.call(rbind, coefficients), file.path(directory, "family-coefficients.csv"), row.names = FALSE)
    cat(sprintf("%s n=%s %s seed=%s: %.3fs, bulk ESS/s %.1f, tail ESS/s %.1f, Rhat %.3f\n",
      row$family, row$observations, backend, seed, row$setup_and_sampling_seconds,
      row$bulk_ess_per_second, row$tail_ess_per_second, row$max_rhat))
    flush.console()
    unlink(temp, recursive = TRUE)
  }
}
packages <- c("gratia", "mgcv", "Rcpp", "posterior", "mirai")
versions <- vapply(packages, function(package) as.character(utils::packageVersion(package)), character(1L))
writeLines(trimws(c(paste(names(versions), versions), "", capture.output(sessionInfo())), which = "right"),
  file.path(directory, "family-benchmark-environment.txt"))
