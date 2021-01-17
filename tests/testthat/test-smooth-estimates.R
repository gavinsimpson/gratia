# Test draw() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")

context("smooth-estimates-methods")

set.seed(1)
dat <- data_sim("eg1", n = 400, seed = 1)
m0 <- gam(y ~ s(x0), data = dat, method = "REML")
m1 <- gam(y ~ s(x0) + s(x1, bs = 'cr') + s(x2, bs = 'ps') + s(x3, bs = 'bs'),
          data = dat, method = "REML")
m2 <- gamm(y ~ s(x0) + s(x1, bs = 'cr') + s(x2, bs = 'ps') + s(x3, bs = 'bs'),
           data = dat, method = "REML")
m3 <- gam(y ~ s(x0, x1, x2), data = dat, method = "REML")
m4 <- gam(y ~ te(x0, x1, x2), data = dat, method = "REML")

dat_biv <- data_sim("eg2", n = 400, seed = 2)
m_biv <- gam(y ~ s(x, z), data = dat_biv, method = "REML")

set.seed(42)
dat_2d_by <- data_sim("eg4", n = 400, seed = 42)
m_2d_by <- gam(y ~ fac + s(x0, x1, by = fac), data = dat_2d_by)

test_that("smooth_estimates works for a GAM", {
    sm <- smooth_estimates(m1, "s(x2)")
    expect_is(sm, "smooth_estimates")
    expect_is(sm, "tbl_df")
    expect_is(sm, "data.frame")
})

test_that("smooth_estimates works with more than one term", {
    sm <- smooth_estimates(m1, c("s(x1)", "s(x2)"))
    expect_is(sm, "smooth_estimates")
    expect_is(sm, "tbl_df")
    expect_is(sm, "data.frame")    
})

test_that("smooth_estimates throws error if smooth not found", {
    expect_error(smooth_estimates(m1, smooth = "s(z)"),
                 "None of the terms matched a smooth",
                 fixed = TRUE)
})

test_that("smooth_estimates works for a GAMM", {
    sm <- smooth_estimates(m2, "s(x2)")
    expect_is(sm, "smooth_estimates")
    expect_is(sm, "tbl_df")
    expect_is(sm, "data.frame")
})

test_that("smooth_estimates works with a bivariate TPRS smooth", {
    expect_silent(sm <- smooth_estimates(m_biv, "s(x,z)", n = 50))
    expect_is(sm, "smooth_estimates")
    expect_is(sm, "tbl_df")
    expect_is(sm, "data.frame")
    expect_identical(nrow(sm), 2500L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x", "z"))
})

test_that("smooth_estimates fails with a trivariate smooth", {
    expect_silent(sm <- smooth_estimates(m3, "s(x0,x1,x2)", n = 25))
    expect_is(sm, "smooth_estimates")
    expect_is(sm, "tbl_df")
    expect_is(sm, "data.frame")
    expect_identical(nrow(sm), 15625L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x0", "x1", "x2"))
})

test_that("smooth_estimates fails with a trivariate tensor product smooth", {
    skip('Need to implement tensor product smooth handling in smooth_estimates')
    expect_error(smooth_estimates(m4, "te(x0,x1,x2)"))
})

test_that("evaluate_re_smooth throws error when passed newdata", {
    skip('Need to implement ranef smooths handling in smooth_estimates')
    ## simulate example... from ?mgcv::random.effects
    set.seed(1)
    dat <- gamSim(1, n = 400, scale = 2, verbose = FALSE) ## simulate 4 term additive truth

    fac <- as.factor(sample(1:20, 400, replace = TRUE))
    dat$X <- model.matrix(~ fac - 1)
    b <- rnorm(20) * 0.5
    dat <- transform(dat, y = y + X %*% b)

    rm1 <- gam(y ~ s(fac, bs = "re") + s(x0) + s(x1) + s(x2) +
                   s(x3), data = dat, method = "ML")

    expect_error(smooth_estimates(rm1, smooth = "s(fac)", data = model.frame(rm1)),
                 "Not yet implemented: user-supplied data in 're' smooth")
})

test_that("smooth_estimates fails if smooth var not in data", {
    id <- which(names(dat) == "x0")
    expect_error(smooth_estimates(m0, "s(x0)", data = dat[, -id]),
                 "Variable(s) 'x0' not found in 'data'.",
                 fixed = TRUE)
})

test_that("smooth_estimates works with vector newdata", {
    sm1 <- smooth_estimates(m0, "s(x0)", data = dat[, "x0"])
    sm2 <- smooth_estimates(m0, "s(x0)", data = dat)
    expect_is(sm1, "smooth_estimates")
    expect_equal(sm1, sm2)
})

test_that("evaluate_1d_smooth fails if newdata is not data frame or numeric", {
    expect_error(smooth_estimates(m1, "s(x0)", data = list(x0 = dat[, "x0"])),
                 "'data', if supplied, must be a numeric vector or a data frame.",
                 fixed = TRUE)
})


test_that("smooth_estimates works for a 2d factor by smooth", {
    sm <- smooth_estimates(m_2d_by, "s(x0,x1)")
    expect_is(sm, "smooth_estimates")
    expect_is(sm, "tbl_df")
    expect_is(sm, "data.frame")
})

test_that("evaluate_fs_smooth() ", {
    skip("Need to implement 'fs' basis handling in smooth_estimates")
    ## simulate example... from ?mgcv::factor.smooth.interaction
    set.seed(0)
    ## simulate data...
    f0 <- function(x) 2 * sin(pi * x)
    f1 <- function(x, a=2, b=-1) exp(a * x)+b
    f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
                          (10 * x)^3 * (1 - x)^10
    n <- 500
    nf <- 10
    fac <- sample(1:nf, n, replace=TRUE)
    x0 <- runif(n)
    x1 <- runif(n)
    x2 <- runif(n)
    a <- rnorm(nf) * .2 + 2;
    b <- rnorm(nf) * .5
    f <- f0(x0) + f1(x1, a[fac], b[fac]) + f2(x2)
    fac <- factor(fac)
    y <- f + rnorm(n) * 2

    df <- data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, fac = fac)
    mod <- gam(y ~ s(x1, fac, bs="fs", k=5), method = "ML")

    newdf <- data.frame(x4 = 1:10, fac = factor(2, levels = 1:10))

    expect_error( smooth_estimates(mod, "x1", newdata = newdf),
                 "Variable x1 not found in 'newdata'.", fixed = TRUE)

    expect_error( smooth_estimates(mod, "x1", newdata = newdf$x4),
                 "'newdata', if supplied, must be a data frame.", fixed = TRUE)

    newdf <- data.frame(x1 = x1, fac = fac)
    expect_silent( smooth_estimates(mod, "x1", newdata = newdf) )
})
