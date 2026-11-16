## Test `gratia` using the `testthat` package

## Setup
library("testthat")
library("gratia")

## Runs the tests in tests/testthat
progress <- Sys.getenv("GRATIA_TEST_PROGRESS")
if (nzchar(progress)) {
  local({
    source("testthat/helper-builder-diagnostics.R", local = TRUE)
    local_spatial_trace(Sys.getenv("GRATIA_SPATIAL_TRACE"))
    print(sessionInfo())
    test_check("gratia", reporter = MultiReporter$new(list(
      CheckReporter$new(), builder_progress_reporter(progress))))
  })
} else {
  test_check("gratia")
}
