# Test draw() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")
library("vdiffr")

context("evaluate-smooth-methods")

set.seed(1)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
m2 <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("valuate_smooth works for a GAM", {
    sm <- evaluate_smooth(m1, "s(x2)")
    expect_is(sm, "evaluated_1d_smooth")
    expect_is(sm, "evaluated_smooth")
    expect_is(sm, "data.frame")
})

test_that("valuate_smooth works for a GAMM", {
    sm <- evaluate_smooth(m2, "s(x2)")
    expect_is(sm, "evaluated_1d_smooth")
    expect_is(sm, "evaluated_smooth")
    expect_is(sm, "data.frame")
})
