## Test `gratia` using the `testthat` package

## Setup
library("testthat")
library("gratia")

## Runs the tests in tests/testthat
source("testthat/helper-builder-diagnostics.R")
if (builder_diagnostics_enabled()) {
  print(sessionInfo())
  test_check("gratia", reporter = MultiReporter$new(list(
    CheckReporter$new(), LocationReporter$new(file = stderr()))))
} else {
  test_check("gratia")
}
