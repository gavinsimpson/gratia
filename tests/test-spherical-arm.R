# R CMD check runs each test script in a fresh R process. On Windows ARM the
# spherical plot builds can stop R when they run with the rest of the suite.
source("testthat/helper-builder-diagnostics.R")
if (builder_isolate_spatial_tests()) {
  library("testthat")
  library("gratia")
  builder_test_check(filter = builder_spatial_test_filter())
}
