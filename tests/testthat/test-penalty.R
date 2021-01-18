## Test penalty()

## load packages
library("testthat")
library("gratia")
library("mgcv")

context("test-penalty")

dat <- data_sim("eg4", n = 400, seed = 42)
m <- gam(y ~ s(x0) + s(x1) + s(x2, by = fac),
         data = dat, method = "REML")

test_that("penalty() works with a simple smooth", {
    expect_silent(p <- penalty(m))
    expect_s3_class(p, "tidy_penalty")
    expect_named(p, c("sp", "r", "c", "value"))
})

test_that("penalty() works with a factor by smooth", {
    expect_silent(p <- penalty(m, smooth = "s(x2):fac2"))
    expect_s3_class(p, "tidy_penalty")
    expect_named(p, c("sp", "r", "c", "value"))
})
