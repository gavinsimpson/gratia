## Test confint() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library('gamm4')

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

    expect_warning(confint(fd, level = c(0.95, 0.8), type = "confidence"))

    expect_error(confint(fd, parm = "s(x4)", type = "confidence"))
})

test_that("Simultaneous interval for a first derivatives of a GAM works", {
    set.seed(42)
    ci <- confint(fd, parm = "x1", type = "simultaneous", nsim = 1000)
    expect_s3_class(ci, "confint.fderiv")
    expect_s3_class(ci, "data.frame")
    expect_named(ci, expected = c("term", "lower", "est", "upper"))
})

set.seed(2)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("Point-wise confidence interval for a GAM works", {
    ci <- confint(mod, parm = "s(x1)", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se", "crit", "lower", "upper"))
})

test_that("Simultaneous interval for a GAM works", {
    set.seed(42)
    ci <- confint(mod, parm = "s(x1)", type = "simultaneous", nsim = 100)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se", "crit", "lower", "upper"))
})

## data for a 2d smooth
df_2d <- data_sim("eg2", seed = 2)
mod_te <- gam(y ~ te(x, z), data = df_2d, method = "REML")

test_that("Point-wise confidence interval for a 2d smooth works", {
    ci <- confint(mod_te, parm = "te(x,z)", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x", "z", "est", "se",
                                  "crit", "lower", "upper"))
})

test_that("Simultaneous interval for a 2d smooth works", {
    set.seed(42)
    ci <- confint(mod_te, parm = "te(x,z)", type = "simultaneous", nsim = 100)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x", "z", "est", "se", "crit", "lower", "upper"))
})

mod <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("Point-wise confidence interval for a GAMM works", {
    ci <- confint(mod, parm = "s(x1)", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se", "crit", "lower", "upper"))
})

test_that("Simultaneous interval for a GAMM works", {
    set.seed(42)
    ci <- confint(mod, parm = "s(x1)", type = "simultaneous", nsim = 100)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se", "crit", "lower", "upper"))
})

## confint methods for by variables
set.seed(2)
dat <- gamSim(4, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ fac + s(x2, by = fac), data = dat, method = "REML")

test_that("Point-wise confidence interval for a GAM with factor by variable works", {
    ci <- confint(mod, parm = "s(x2)", type = "confidence", partial_match = TRUE)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x2", "est", "se", "fac",
                                  "crit", "lower", "upper"))
    expect_equal(paste0("s(x2):fac", levels(dat[["fac"]])),
                 unique(ci[["smooth"]]))
})

test_that("Simultaneous confidence interval for a GAM with factor by variable works", {
    ci <- confint(mod, parm = "s(x2)", type = "simultaneous", partial_match = TRUE)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x2", "est", "se", "fac",
                                  "crit", "lower", "upper"))
    expect_equal(paste0("s(x2):fac", levels(dat[["fac"]])),
                 unique(ci[["smooth"]]))
})

## Part of #80
test_that("Point-wise confidence interval for a GAM with selected factor by variable works", {
    ci <- confint(mod, parm = "s(x2):fac1", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x2", "est", "se", "fac",
                                  "crit", "lower", "upper"))
})

set.seed(2)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gamm4::gamm4(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, REML = TRUE)

test_that("Point-wise confidence interval for a GAMM works", {
    ci <- confint(mod, parm = "s(x1)", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se", "crit", "lower", "upper"))
})

test_that("Simultaneous interval for a GAMM works", {
    set.seed(42)
    ci <- confint(mod, parm = "s(x1)", type = "simultaneous", nsim = 100)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se", "crit", "lower", "upper"))
})
