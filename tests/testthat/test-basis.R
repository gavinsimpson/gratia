## Test basis() and related functions

## load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("basis() works with a simple smooth", {
    expect_silent(bs <- basis(s(x0), data = su_eg4))
    expect_s3_class(bs, "basis")
    expect_named(bs, c("smooth", "by_variable", "bf", "value", "x0"))
})

test_that("basis() works with a factor by smooth", {
    expect_silent(bs <- basis(s(x2, by = fac), data = su_eg4))
    expect_s3_class(bs, "basis")
    expect_named(bs, c("smooth", "by_variable", "bf", "value", "x2", "fac"))
})
