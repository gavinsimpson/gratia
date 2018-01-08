## Test draw() methods

## load packages
library("testthat")
library("tsgam")
library("mgcv")

context("Testing `draw` methods")

test_that("draw.evaluated_1d_smooth() plots the smooth", {
    dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
    m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
    sm <- evaluate_smooth(m1, "s(x2)")
    plt <- draw(sm)
    expect_s3_class(plt, "ggplot")
})

test_that("draw.evaluated_2d_smooth() plots the smooth & SE", {
    dat <- gamSim(2, n = 4000, dist = "normal", scale = 1)
    m2 <- gam(y ~ s(x, z, k = 40), data = dat$data, method = "REML")
    sm <- evaluate_smooth(m2, "s(x,z)", n = 100)
    plt <- draw(sm)
    ## now the standard error the smooth instead
    plt2 <- draw(sm, show = "se")
    expect_s3_class(plt, "ggplot")
    expect_s3_class(plt2, "ggplot")
})

test_that("draw.gam() plots a simple multi-smooth AM", {
    dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
    m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
    plt1 <- draw(m1)
    expect_s3_class(plt1, "ggplot")
    plt1 <- draw(m1, scales = "fixed")
    expect_s3_class(plt1, "ggplot")
})

test_that("draw.gam() plots an AM with a single 2d smooth", {
    dat <- gamSim(2, n = 4000, dist = "normal", scale = 1)
    m2 <- gam(y ~ s(x, z, k = 30), data = dat$data, method = "REML")
    plt2 <- draw(m2)
    expect_s3_class(plt2, "ggplot")
})

test_that("draw.gam() plots an AM with a single factor by-variable smooth", {
    dat <- gamSim(4)
    m3 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)
    plt3 <- draw(m3, scales = "fixed")
    expect_s3_class(plt3, "ggplot")
    plt3 <- draw(m3, scales = "free")
    expect_s3_class(plt3, "ggplot")
})

## simulate date from y = f(x2)*x1 + error
dat <- gamSim(3, n = 400)
mod <- gam(y ~ s(x2, by = x1), data = dat)

test_that("draw() works with continuous by", {
    plt <- draw(mod)
    expect_s3_class(plt, "ggplot")
})

test_that("draw() works with continuous by and fixed scales", {
    plt <- draw(mod, scales = "fixed")
    expect_s3_class(plt, "ggplot")
})
