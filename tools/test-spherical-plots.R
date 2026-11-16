# Run the spherical tests in isolation for comparison with the full check.
run_spherical_tests <- function() {
  stopifnot(requireNamespace("sf", quietly = TRUE))
  print(sessionInfo())
  print(sf::sf_extSoftVersion())
  for (package in c("sf", "s2", "wk", "ggplot2", "mgcv")) {
    cat(package, as.character(packageVersion(package)), "\n")
  }

  progress <- Sys.getenv("GRATIA_TEST_PROGRESS")
  if (!nzchar(progress)) stop("Set GRATIA_TEST_PROGRESS to a log file path.")

  source("tests/testthat/helper-builder-diagnostics.R")
  local_spatial_trace(progress)
  reporter <- testthat::MultiReporter$new(list(
    testthat::SummaryReporter$new(), builder_progress_reporter(progress)))
  testthat::test_local(filter = "^compare-smooths$", reporter = reporter,
    stop_on_failure = TRUE, stop_on_warning = TRUE)
}

run_spherical_tests()
