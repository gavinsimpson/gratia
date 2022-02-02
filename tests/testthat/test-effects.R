## Test effects()

## load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("fixef() works for a simple GAM", {
    expect_silent(fe <- fixef(m_gam))

    expect_named(fe, expected = c("(Intercept)"))

    expect_identical(length(fe), 1L)
})

test_that("fixef() works for a simple GAMM", {
    expect_silent(fe <- fixef(m_gamm))

    expect_named(fe, expected = c("(Intercept)"))

    expect_identical(length(fe), 1L)
})

test_that("fixef() works for a factor by GAM", {
    expect_silent(fe <- fixef(su_m_factor_by))

    expect_named(fe, expected = c("(Intercept)", "fac2", "fac3"))

    expect_identical(length(fe), 3L)

    expect_silent(fe <- fixef(su_m_factor_by_x2))

    expect_named(fe, expected = c("(Intercept)", "fac2", "fac3"))

    expect_identical(length(fe), 3L)
})

test_that("fixef() works for a simple GLM", {
    expect_silent(fe <- fixef(m_glm))

    expect_named(fe, expected = c("(Intercept)", "x0", "x1", "x2", "x3"))

    expect_identical(length(fe), 5L)
})

test_that("fixef() works for a simple LM", {
    expect_silent(fe <- fixef(m_lm))

    expect_named(fe, expected = c("(Intercept)", "x0", "x1", "x2", "x3"))

    expect_identical(length(fe), 5L)
})

test_that("fixed_effects() works for a simple GAM", {
    expect_silent(fe <- fixed_effects(m_gam))

    expect_named(fe, expected = c("(Intercept)"))

    expect_identical(length(fe), 1L)
})

test_that("fixed_effects() works for a simple GAMM", {
    expect_silent(fe <- fixed_effects(m_gamm))

    expect_named(fe, expected = c("(Intercept)"))

    expect_identical(length(fe), 1L)
})

test_that("fixed_effects() works for a factor by GAM", {
    expect_silent(fe <- fixed_effects(su_m_factor_by))

    expect_named(fe, expected = c("(Intercept)", "fac2", "fac3"))

    expect_identical(length(fe), 3L)

    expect_silent(fe <- fixed_effects(su_m_factor_by_x2))

    expect_named(fe, expected = c("(Intercept)", "fac2", "fac3"))

    expect_identical(length(fe), 3L)
})

test_that("fixed_effects() works for a simple GLM", {
    expect_silent(fe <- fixed_effects(m_glm))

    expect_named(fe, expected = c("(Intercept)", "x0", "x1", "x2", "x3"))

    expect_identical(length(fe), 5L)
})

test_that("fixed_effects() works for a simple LM", {
    expect_silent(fe <- fixed_effects(m_lm))

    expect_named(fe, expected = c("(Intercept)", "x0", "x1", "x2", "x3"))

    expect_identical(length(fe), 5L)
})
