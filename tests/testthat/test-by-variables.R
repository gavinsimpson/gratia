## Test Handling `by` variables in smooths/GAMs

## load packages
library("testthat")
library("tsgam")
library("mgcv")

context("Testing `by` variables")

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
