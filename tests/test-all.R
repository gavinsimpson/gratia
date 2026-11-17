## Test `gratia` using the `testthat` package

## Setup
library("testthat")
library("gratia")

## Runs the tests in tests/testthat
source("testthat/helper-builder-diagnostics.R")
if (builder_isolate_spatial_tests()) {
  builder_test_check(filter = builder_spatial_test_filter(), invert = TRUE)
} else {
  builder_test_check()
}
