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

test_that("evaluate_smooth works for a GAM", {
    sm <- evaluate_smooth(m1, "s(x2)")
    expect_is(sm, "evaluated_1d_smooth")
    expect_is(sm, "evaluated_smooth")
    expect_is(sm, "data.frame")
})

test_that("evaluate_smooth throws a message with more than one term", {
    expect_message(evaluate_smooth(m1, c("s(x1)", "s(x2)")),
                   "Supplied more than 1 'smooth'; using only the first")
})

test_that("evaluate_smooth works for a GAMM", {
    sm <- evaluate_smooth(m2, "s(x2)")
    expect_is(sm, "evaluated_1d_smooth")
    expect_is(sm, "evaluated_smooth")
    expect_is(sm, "data.frame")
})

test_that("evaluate_1d_smooth fails with multiple smooths that aren't by factor smooths", {
    expect_error(gratia:::evaluate_1d_smooth(m1[["smooth"]]),
                 "Not all of these are 'by' variable smooths")
})

test_that("evaluate_2d_smooth fails with multiple smooths that aren't by factor smooths", {
    expect_error(gratia:::evaluate_2d_smooth(m1[["smooth"]]),
                 "Not all of these are 'by' variable smooths")
})

test_that("evaluate_fs_smooth fails with multiple smooths that aren't by factor smooths", {
    expect_error(gratia:::evaluate_fs_smooth(m1[["smooth"]]),
                 "Not all of these are 'by' variable smooths")
})

test_that("evaluate_re_smooth fails with multiple smooths that aren't by factor smooths", {
    expect_error(gratia:::evaluate_re_smooth(m1[["smooth"]]),
                 "Not all of these are 'by' variable smooths")
})

test_that("evaluate_smooth fails with a trivariate smooth", {
    m <- gam(y ~ s(x0, x1, x2), data = dat, method = "REML")
    expect_error(evaluate_smooth(m, "s(x0,x1,x2)"))
    m <- gam(y ~ te(x0, x1, x2), data = dat, method = "REML")
    expect_error(evaluate_smooth(m, "s(x0,x1,x2)"))
})

test_that("evaluate_re_smooth throws error when passed newdata", {
    ## simulate example... from ?mgcv::random.effects
    set.seed(1)
    dat <- gamSim(1, n = 400, scale = 2, verbose = FALSE) ## simulate 4 term additive truth

    fac <- as.factor(sample(1:20, 400, replace = TRUE))
    dat$X <- model.matrix(~ fac - 1)
    b <- rnorm(20) * 0.5
    dat <- transform(dat, y = y + X %*% b)

    rm1 <- gam(y ~ s(fac, bs = "re") + s(x0) + s(x1) + s(x2) +
                   s(x3), data = dat, method = "ML")

    expect_error(evaluate_smooth(rm1, smooth = "s(fac)", newdata = model.frame(rm1)),
                 "Not yet implemented: user-supplied data in 're' smooth")
})

test_that("evaluate_1d_smooth fails if smooth var not in newdata", {
    m <- gam(y ~ s(x0), data = dat, method = "REML")
    id <- which(names(dat) == "x0")
    expect_error(evaluate_smooth(m, "s(x0)", newdata = dat[, -id]),
                 "Variable x0 not found in 'newdata'.",
                 fixed = TRUE)
})

test_that("evaluate_1d_smooth works with vector newdata", {
    m <- gam(y ~ s(x0), data = dat, method = "REML")
    sm1 <- evaluate_smooth(m, "s(x0)", newdata = dat[, "x0"])
    sm2 <- evaluate_smooth(m, "s(x0)", newdata = dat)
    expect_is(sm1, "evaluated_1d_smooth")
    expect_equal(sm1, sm2)
})

test_that("evaluate_1d_smooth fails if newdata is not data frame or numeric", {
    expect_error(evaluate_smooth(m1, "s(x0)", newdata = list(x0 = dat[, "x0"])),
                 "'newdata', if supplied, must be a numeric vector or a data frame.",
                 fixed = TRUE)
})

test_that("evaluate_2d_smooth fails if smooth var not in newdata", {
    m <- gam(y ~ s(x0, x1), data = dat, method = "REML")
    id <- which(names(dat) == "x0")
    expect_error(evaluate_smooth(m, "s(x0,x1)", newdata = dat[, -id]),
                 "Variable x0 not found in 'newdata'.",
                 fixed = TRUE)
})

test_that("evaluate_2d_smooth fails if newdata is not data frame or numeric", {
    m <- gam(y ~ s(x0, x1), data = dat, method = "REML")
    expect_error(evaluate_smooth(m, "s(x0,x1)", newdata = list(x0 = dat[, "x0"])),
                 "'newdata', if supplied, must be a numeric vector or a data frame.",
                 fixed = TRUE)
})

test_that("evaluate_2d_smooth works for a 2d factor by smooth", {
    set.seed(42)
    dat <- gamSim(4, n = 400, verbose = FALSE)
    mf <- gam(y ~ fac + s(x0, x1, by = fac), data = dat)
    sm <- evaluate_smooth(mf, "s(x0,x1)")
    expect_is(sm, "evaluated_2d_smooth")
    expect_is(sm, "evaluated_smooth")
    expect_is(sm, "data.frame")
})
