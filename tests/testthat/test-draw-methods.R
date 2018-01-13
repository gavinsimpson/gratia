## Test draw() methods

## load packages
library("testthat")
library("schoenberg")
library("mgcv")
library("ggplot2")
theme_set(theme_grey())

context("draw-methods")

test_that("draw.evaluated_1d_smooth() plots the smooth", {
    set.seed(1)
    dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
    m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
    sm <- evaluate_smooth(m1, "s(x2)")
    vdiffr::expect_doppelganger("draw 1d smooth for selected smooth", draw(sm))
})

test_that("draw.evaluated_2d_smooth() plots the smooth & SE", {
    set.seed(1)
    dat <- gamSim(2, n = 4000, dist = "normal", scale = 1, verbose = FALSE)
    m2 <- gam(y ~ s(x, z, k = 40), data = dat$data, method = "REML")
    sm <- evaluate_smooth(m2, "s(x,z)", n = 100)
    vdiffr::expect_doppelganger("draw 2d smooth", draw(sm))
    vdiffr::expect_doppelganger("draw std error of 2d smooth", draw(sm, show = "se"))
})

test_that("draw.gam() plots a simple multi-smooth AM", {
    set.seed(1)
    dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
    m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
    vdiffr::expect_doppelganger("draw simple multi-smooth AM", draw(m1))
    vdiffr::expect_doppelganger("draw simple multi-smooth AM with fixed scales",
                                draw(m1, scales = "fixed"))
})

test_that("draw.gam() plots an AM with a single 2d smooth", {
    set.seed(1)
    dat <- gamSim(2, n = 4000, dist = "normal", scale = 1, verbose = FALSE)
    m2 <- gam(y ~ s(x, z, k = 30), data = dat$data, method = "REML")
    vdiffr::expect_doppelganger("draw AM with 2d smooth", draw(m2))
})

test_that("draw.gam() plots an AM with a single factor by-variable smooth", {
    set.seed(1)
    dat <- gamSim(4, verbose = FALSE)
    m3 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)
    vdiffr::expect_doppelganger("draw AM with factor by-variable smooth",
                                draw(m3))
    vdiffr::expect_doppelganger("draw AM with factor by-variable smooth with fixed scales",
                                draw(m3, scales = "fixed"))
})

## simulate date from y = f(x2)*x1 + error
dat <- gamSim(3, n = 400, verbose = FALSE)
mod <- gam(y ~ s(x2, by = x1), data = dat)

test_that("draw() works with continuous by", {
    vdiffr::expect_doppelganger("draw AM with continuous by-variable smooth",
                                draw(mod))
})

test_that("draw() works with continuous by and fixed scales", {
    vdiffr::expect_doppelganger("draw AM with continuous by-var fixed scale",
                                draw(mod, scales = "fixed"))
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
    vdiffr::expect_doppelganger("draw.evaluated_re_smooth",
                                draw(sm))
    vdiffr::expect_doppelganger("draw.gam model with ranef smooth",
                                draw(rm1, ncol = 3))
    vdiffr::expect_doppelganger("draw.gam model with ranef smooth fixed scales",
                                draw(rm1, ncol = 3, scales = "fixed"))
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
    ## vdiffr::expect_doppelganger("draw.evaluated_re_smooth with factor by",
    ##                             draw(sm))
    vdiffr::expect_doppelganger("draw.gam model with ranef smooth factor by",
                                draw(rm2, ncol = 3))
    vdiffr::expect_doppelganger("draw.gam model with ranef smooth factor by fixed scales",
                                draw(rm2, ncol = 3, scales = "fixed"))
})
