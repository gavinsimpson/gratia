## Test confint() methods

## load packages
library("testthat")
library("schoenberg")
library("mgcv")

context("confint methods")

set.seed(42)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
## first derivatives of all smooths...
fd <- fderiv(mod)

test_that("Point-wise confidence interval for a first derivatives of a GAM works", {
    ci <- confint(fd, type = "confidence")
    expect_s3_class(ci, "confint.fderiv")
    expect_s3_class(ci, "data.frame")
    expect_named(ci, expected = c("term", "lower", "est", "upper"))
})

test_that("Simultaneous interval for a first derivatives of a GAM works", {
    set.seed(42)
    si <- confint(fd, parm = "x1", type = "simultaneous", nsim = 1000)
    expect_s3_class(si, "confint.fderiv")
    expect_s3_class(si, "data.frame")
    expect_named(si, expected = c("term", "lower", "est", "upper"))
})

set.seed(2)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("Point-wise confidence interval for a GAM works", {
    ci <- confint(mod, parm = "x1", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "data.frame")
    expect_named(ci, expected = c("smooth", "x1", "est", "se", "lower", "upper", "crit"))
})

test_that("Simultaneous interval for a GAM works", {
    set.seed(42)
    si <- confint(mod, parm = "x1", type = "simultaneous", nsim = 100)
    expect_s3_class(si, "confint.gam")
    expect_s3_class(si, "data.frame")
    expect_named(si, expected = c("smooth", "x1", "est", "se", "lower", "upper", "crit"))
})

## confint methods for by variables
set.seed(2)
dat <- gamSim(4, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ fac + s(x2, by = fac), data = dat, method = "REML")

test_that("Point-wise confidence interval for a GAM with factor by variable works", {
    ci <- confint(mod, parm = "x2", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "data.frame")
    expect_named(ci, expected = c("smooth", "x2", "est", "se", "fac",
                                  "lower", "upper", "crit"))
    expect_equal(paste0("s(x2):fac", levels(dat[["fac"]])),
                 levels(ci[["smooth"]]))
})
