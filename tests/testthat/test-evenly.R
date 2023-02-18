# Test evenly
# load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("evenly with by works", {
    expect_silent(x <- evenly(1987:1999, by = 2))
    expect_identical(length(x), 7L)
    expect_identical(seq(1987, 1999, by = 2), x)
})
