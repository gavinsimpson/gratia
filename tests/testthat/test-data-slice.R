## Test data_lice() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")
library("vdiffr")

context("data-slice-methods")

set.seed(1)
dat <- gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("data_slice works for a GAM", {
    expect_silent(ds <- data_slice(m1, var1 = "x1", var2 = "x2"))
    expect_s3_class(ds, "tbl_df")
})
