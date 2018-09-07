## Test draw() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")
library("vdiffr")
theme_set(theme_grey())

context("draw-methods")

test_that("draw.evaluated_1d_smooth() plots the smooth", {
    set.seed(1)
    dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
    m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
    sm <- evaluate_smooth(m1, "s(x2)")
    plt <- draw(sm)
    expect_doppelganger("draw 1d smooth for selected smooth", plt)
})

test_that("draw.evaluated_2d_smooth() plots the smooth & SE", {
    set.seed(1)
    dat <- gamSim(2, n = 4000, dist = "normal", scale = 1, verbose = FALSE)
    m2 <- gam(y ~ s(x, z, k = 40), data = dat$data, method = "REML")
    sm <- evaluate_smooth(m2, "s(x,z)", n = 100)
    plt <- draw(sm)
    expect_doppelganger("draw 2d smooth", plt)
    plt <- draw(sm, show = "se")
    expect_doppelganger("draw std error of 2d smooth", plt)
})

test_that("draw.gam() plots a simple multi-smooth AM", {
    set.seed(1)
    dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
    m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

    plt <- draw(m1)
    expect_doppelganger("draw simple multi-smooth AM", plt)

    plt <- draw(m1, scales = "fixed")
    expect_doppelganger("draw simple multi-smooth AM with fixed scales", plt)
})

test_that("draw.gam() plots an AM with a single 2d smooth", {
    set.seed(1)
    dat <- gamSim(2, n = 4000, dist = "normal", scale = 1, verbose = FALSE)
    m2 <- gam(y ~ s(x, z, k = 30), data = dat$data, method = "REML")

    plt <- draw(m2)
    expect_doppelganger("draw AM with 2d smooth", plt)
})

test_that("draw.gam() plots an AM with a single factor by-variable smooth", {
    set.seed(1)
    dat <- gamSim(4, verbose = FALSE)
    m3 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)

    plt <- draw(m3)
    expect_doppelganger("draw AM with factor by-variable smooth", plt)

    plt <- draw(m3, scales = "fixed")
    expect_doppelganger("draw AM with factor by-variable smooth with fixed scales", plt)
})

## simulate date from y = f(x2)*x1 + error
dat <- gamSim(3, n = 400, verbose = FALSE)
mod <- gam(y ~ s(x2, by = x1), data = dat)

test_that("draw() works with continuous by", {
    plt <- draw(mod)
    expect_doppelganger("draw AM with continuous by-variable smooth", plt)
})

test_that("draw() works with continuous by and fixed scales", {
    plt <- draw(mod, scales = "fixed")
    expect_doppelganger("draw AM with continuous by-var fixed scale", plt)
})

test_that("draw() works with random effect smooths (bs = 're')", {
    ## simulate example... from ?mgcv::random.effects
    dat <- gamSim(1, n = 400, scale = 2, verbose = FALSE) ## simulate 4 term additive truth

    fac <- as.factor(sample(1:20, 400, replace = TRUE))
    dat$X <- model.matrix(~ fac - 1)
    b <- rnorm(20) * 0.5
    dat <- transform(dat, y = y + X %*% b)

    rm1 <- gam(y ~ s(fac, bs = "re") + s(x0) + s(x1) + s(x2) +
                   s(x3), data = dat, method = "ML")

    sm <- evaluate_smooth(rm1, "s(fac)")
    expect_s3_class(sm, "evaluated_re_smooth")

    p1 <- draw(sm)
    expect_doppelganger("draw.evaluated_re_smooth", p1)

    p2 <- draw(rm1, ncol = 3)
    expect_doppelganger("draw.gam model with ranef smooth", p2)

    p3 <- draw(rm1, ncol = 3, scales = "fixed")
    expect_doppelganger("draw.gam model with ranef smooth fixed scales", p3)
})

test_that("draw() with random effect smooths (bs = 're') & factor by variable ", {
    ## simulate example...
    set.seed(1)
    dat1 <- gamSim(4, n = 400, scale = 2, verbose = FALSE) ## simulate 4 term additive truth

    ## random effects
    ranef <- as.factor(sample(1:20, 400, replace = TRUE))
    dat1$X <- model.matrix(~ ranef - 1)
    b <- rnorm(20) * 0.5
    da1 <- transform(dat1, y = y + X %*% b)

    ## fit model
    rm2 <- gam(y ~ fac + s(ranef, bs = "re", by = fac) + s(x0) + s(x1) + s(x2),
               data = dat1, method = "ML")

    sm <- evaluate_smooth(rm2, "s(ranef)")
    expect_s3_class(sm, "evaluated_re_smooth")
    p1 <- draw(sm)
    ## vdiffr::expect_doppelganger("draw.evaluated_re_smooth with factor by", p1)
    p2 <- draw(rm2, ncol = 3)
    expect_doppelganger("draw.gam model with ranef smooth factor by", p2)
    p3 <- draw(rm2, ncol = 3, scales = "fixed")
    expect_doppelganger("draw.gam model with ranef smooth factor by fixed scales", p3)
})

test_that("draw() can handle non-standard names -- a function call as a name", {

    df <- data.frame(y = c(0.15,0.17,0.07,0.17,0.01,0.15,0.18,0.04,-0.06,-0.08,
                           0, 0.03,-0.27,-0.93,0.04,0.12,0.08,0.15,0.04,0.15,
                           0.03,0.09,0.11,0.13,-0.11,-0.32,-0.7,-0.78,0.07,0.04,
                           0.06,0.12,-0.15,0.05,-0.08,0.14,-0.02,-0.14,-0.24,
                           -0.32,-0.78,-0.81,-0.04,-0.25,-0.09,0.02,-0.13,-0.2,
                           -0.04,0,0.02,-0.05,-0.19,-0.37,-0.57,-0.81),
                     time =  rep(2^c(-1, 0, 1, 1.58,2, 2.58, 3, 3.32, 3.58, 4.17,
                                     4.58, 5.58, 6.17, 7.39), 4))
    ## the smooth is of `log2(time)` but this needs special handling
    ## in the `ggplot()` to avoid `ggplot()` looking incorrectly for `time` and
    ## not the correct `log2(time)`
    fit <- gam(y ~ s(log2(time)), data = df, method = "REML")
    p1 <- draw(fit)
    expect_doppelganger("draw.gam model with non-standard names", p1)
})

test_that("draw() works with random effect smooths (bs = 're')", {
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
    mod <- gam(y~s(x0) + s(x1, fac, bs="fs", k=5) + s(x2, k=20),
               method = "ML")

    sm <- evaluate_smooth(mod, "s(x1,fac)")
    expect_s3_class(sm, "evaluated_fs_smooth")

    p1 <- draw(sm)
    expect_doppelganger("draw.evaluated_fs_smooth", p1)

    p2 <- draw(mod, ncol = 2)
    expect_doppelganger("draw.gam model with fs smooth", p2)

    p3 <- draw(mod, ncol = 2, scales = "fixed")
    expect_doppelganger("draw.gam model with fs smooth fixed scales", p3)
})
