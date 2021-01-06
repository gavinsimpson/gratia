## Test handling `by` variables in smooths/GAMs

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")

context("test-by-variables")

## Need a local wrapper to allow conditional use of vdiffr
`expect_doppelganger` <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

## simulate date from y = f(x2)*x1 + error
set.seed(42)
dat <- gamSim(3, n = 400, verbose = FALSE)
mod <- gam(y ~ s(x2, by = x1), data = dat)

test_that("draw() works with continuous by", {
    plt <- draw(mod)
    expect_doppelganger("continuous by-variable smmoth",
                                plt)
})

test_that("draw() works with continuous by and fixed scales", {
    plt <- draw(mod, scales = "fixed")
    expect_s3_class(plt, "ggplot")
})

test_that("evaluate_smooth() works with continuous by", {
    sm  <- evaluate_smooth(mod, "s(x2)")
    expect_s3_class(sm, "evaluated_1d_smooth")
})

test_that("is_by_smooth() is TRUE with continuous by", {
    expect_true(is_by_smooth(mod[["smooth"]][[1]]))
})

test_that("is_factor_by_smooth() is FALSE with continuous by", {
    expect_false(is_factor_by_smooth(mod[["smooth"]][[1]]))
})

test_that("is_continuous_by_smooth() is TRUE with continuous by", {
    expect_true(is_continuous_by_smooth(mod[["smooth"]][[1]]))
})

set.seed(42)
dat <- gamSim(4, n = 400, verbose = FALSE)
mf <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)
mfgamm <- gamm(y ~ fac + s(x2, by = fac) + s(x0), data = dat)

test_that("get_by_smooth works", {
    sm <- get_by_smooth(mf, "s(x2)", level = "1")
    expect_is(sm, "mgcv.smooth")
    expect_equal(sm, mf[["smooth"]][[1L]])

    sm <- get_by_smooth(mfgamm, "s(x2)", level = "1")
    expect_is(sm, "mgcv.smooth")
    expect_equal(sm, mfgamm[["gam"]][["smooth"]][[1L]])

    expect_error(get_by_smooth(mf, "s(x4)", level = "1"),
                 "The requested smooth 's(x4)' is not a by smooth.",
                 fixed = TRUE)

    expect_error(get_by_smooth(mf, "s(x2)"),
                 "No value provided for argument 'level':", fixed = TRUE)

    expect_error(get_by_smooth(mf, "s(x2)", level = "4"),
                 "Invalid 'level' for smooth 's(x2)'.",
                 fixed = TRUE)
})

test_that("draw.gam works with select and parametric = TRUE", {
    plt <- draw(mf, select = 's(x2):fac1', parametric = TRUE)
    expect_doppelganger("draw.gam-user-select-and-parametric-true",
                                plt)
})
