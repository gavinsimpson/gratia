## Test data_slice() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
## library("ggplot2")
## library("vdiffr")

context("data-slice-methods")

set.seed(1)
dat <- gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("data_slice works for a GAM", {
    expect_silent(ds <- data_slice(m1, var1 = "x1", var2 = "x2"))
    expect_s3_class(ds, "tbl_df")
    expect_named(ds, c("x1", "x2", "x0", "x3"))
    expect_error(data_slice(m1, var1 = "x1", var2 = "foo"),
                 "Variable <foo> not found in data.", fixed = TRUE)
})

set.seed(42)
dat <- gamSim(4, n = 400, verbose = FALSE)
mf <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)

test_that("data_slice works for a GAM with factor by", {
    expect_silent(ds <- data_slice(mf, var1 = "x2", var2 = "fac"))
    expect_s3_class(ds, "tbl_df")
    expect_named(ds, c("x2", "fac", "x0"))
})
