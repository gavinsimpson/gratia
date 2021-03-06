## Test data_slice() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("gamm4")

## 1d example
set.seed(2)
dat1 <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat1, method = "REML")
m_gamm <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat1, method = "ML")
m_gamm4 <- gamm4(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat1, REML = TRUE)
## 2d example
dat2 <- gamSim(2, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m2 <- gam(y ~ s(x, z), data = dat2$data, method = "REML")

test_that("datagen works for a simple 1d smooth", {
    expect_silent(dg <- datagen(get_smooths_by_id(m1, 1)[[1L]],
                                data = dat1, n = 50))
    expect_s3_class(dg, "data.frame")
    expect_named(dg, c("smooth", "x"))
    expect_identical(nrow(dg), 50L)
    expect_identical(ncol(dg), 2L)
})

test_that("datagen works for a simple 2d smooth", {
    expect_silent(dg <- datagen(get_smooths_by_id(m2, 1)[[1L]],
                                data = dat2$data, n = 10))
    expect_s3_class(dg, "data.frame")
    expect_named(dg, c("smooth", "x1", "x2"))
    expect_identical(nrow(dg), 100L)
    expect_identical(ncol(dg), 3L)
})

test_that("datagen.mgcv.smooth fails for a 3d smooth", {
    m <- gam(y ~ s(x0, x1, x2), data = dat1, method = "REML")
    expect_error(datagen(m, smooth = "s(x0,x1,x2)"),
                 "Cannot handle smooths of three (3) or more terms.",
                 fixed = TRUE)
})

test_that("datagen gam method works for a simple 1d smooth", {
    expect_silent(dg <- datagen(m1, smooth = "s(x0)", n = 50))
    expect_s3_class(dg, "data.frame")
    expect_named(dg, c("smooth", "x"))
    expect_identical(nrow(dg), 50L)
    expect_identical(ncol(dg), 2L)
})

test_that("datagen gam method fails if smooth not specified", {
    expect_error(datagen(m1),
                 "Argument 'smooth' must be specified and not 'NULL'.",
                 fixed = TRUE)
})

test_that("datagen gam method fails if multiple smooths specified", {
    expect_error(datagen(m1, smooth = c("s(x0)", "s(x1)")),
                 "More than one smooth requested in argument 'smooth'.",
                 fixed = TRUE)
})

test_that("datagen gam method works for a simple 2d smooth", {
    expect_silent(dg <- datagen(m2, smooth = "s(x,z)", n = 10))
    expect_s3_class(dg, "data.frame")
    expect_named(dg, c("smooth", "x1", "x2"))
    expect_identical(nrow(dg), 100L)
    expect_identical(ncol(dg), 3L)
})

test_that("datagen gamm method works", {
    expect_silent(dg <- datagen(m_gamm, smooth = "s(x0)", n = 50))
    expect_s3_class(dg, "data.frame")
    expect_named(dg, c("smooth", "x"))
    expect_identical(nrow(dg), 50L)
    expect_identical(ncol(dg), 2L)
})

test_that("datagen gamm methods fails if not supplied a gamm object", {
    expect_error(datagen.gamm(m1),
                 "Model doesn't appear to be a 'gamm()' model object",
                 fixed = TRUE)
})

test_that("datagen gamm4 method works", {
    expect_silent(dg <- datagen(m_gamm4, smooth = "s(x0)", n = 50))
    expect_s3_class(dg, "data.frame")
    expect_named(dg, c("smooth", "x"))
    expect_identical(nrow(dg), 50L)
    expect_identical(ncol(dg), 2L)
})

test_that("datagen gamm methods fails if not supplied a gamm object", {
    expect_error(datagen.list(m1),
                 "Model doesn't appear to be a 'gamm4()' model object",
                 fixed = TRUE)
})
