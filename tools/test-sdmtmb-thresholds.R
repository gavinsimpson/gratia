# Run separately so macOS oldrel stalls retain their last completed assertion.
stopifnot(requireNamespace("sdmTMB", quietly = TRUE),
  packageVersion("sdmTMB") >= "1.2.0", requireNamespace("RTMB", quietly = TRUE))
print(sessionInfo())
for (package in c("sdmTMB", "RTMB", "TMB", "Matrix", "mgcv")) {
  cat(package, as.character(packageVersion(package)), "\n")
}
progress <- Sys.getenv("GRATIA_TEST_PROGRESS")
reporter <- testthat::SummaryReporter$new()
if (nzchar(progress)) {
  source("tests/testthat/helper-builder-diagnostics.R")
  reporter <- testthat::MultiReporter$new(list(reporter,
    builder_progress_reporter(progress)))
}
elapsed <- system.time(results <- testthat::test_local(
  filter = "^sdmtmb-thresholds$", reporter = reporter,
  stop_on_failure = TRUE, stop_on_warning = TRUE))
timings <- as.data.frame(results)
timings <- timings[, c("test", "real", "passed", "failed", "error", "warning", "skipped")]
print(timings, row.names = FALSE)
cat(sprintf("Total threshold test time, including setup: %.1f seconds.\n",
  elapsed[["elapsed"]]))
