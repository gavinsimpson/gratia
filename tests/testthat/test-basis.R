## Test basis() and related functions

## load packages
library("testthat")
library("gratia")
library("mgcv")

set.seed(42)
dat <- gamSim(4, n = 400, verbose = FALSE)

test_that("basis() works with a simple smooth", {
    expect_silent(bs <- basis(s(x0), data = dat))
    expect_s3_class(bs, "basis")
    expect_named(bs, c("smooth", "by_variable", "bf", "value", "x0"))
})

test_that("basis() works with a factor by smooth", {
    expect_silent(bs <- basis(s(x2, by = fac), data = dat))
    expect_s3_class(bs, "basis")
    expect_named(bs, c("smooth", "by_variable", "bf", "value", "x2", "fac"))
})
