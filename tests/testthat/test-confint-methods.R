## Test confint() methods

## load packages
library("testthat")
library("tsgam")
library("mgcv")

context("confint methods")

set.seed(2)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("Point-wise confidence interval for a GAM works", {
    ci <- confint(mod, parm = "x1", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_named(ci, expected = c("smooth", "x1", "est", "se", "lower", "upper", "crit"))
})

test_that("Simultaneous interval for a GAM works", {
    set.seed(42)
    si <- confint(mod, parm = "x1", type = "simultaneous", nsim = 100)
    expect_s3_class(si, "confint.gam")
    expect_named(si, expected = c("smooth", "x1", "est", "se", "lower", "upper", "crit"))
})
