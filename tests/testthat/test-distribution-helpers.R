# Test rootograms() and related residuals functions

## load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("nb_theta works a nb() model", {
    expect_silent(theta <- nb_theta(m_nb))
})

test_that("nb_theta works a nb() model", {
    expect_silent(theta <- nb_theta(m_negbin))
    # this model sets theta == 25
    expect_equal(theta, 25)
})

test_that("nb_theta fails for a non NB model", {
    expect_error(theta <- nb_theta(m_gam),
                 "Only negative binomial models are supported.",
                 fixed = TRUE)
})

test_that("theta works a nb() model", {
    expect_silent(p <- theta(m_nb))
    expect_type(p, "double")
})

test_that("theta works a nb() model", {
    expect_silent(theta(m_nb))
})

test_that("theta fails for poisson() model", {
    expect_error(theta(b_pois),
                 "No additional parameters available for this model",
                 fixed = TRUE)
})

test_that("has_theta() returns TRUE for a nb() GAM", {
    expect_true(has_theta(m_nb))
})

test_that("has_theta() returns FALSE for a poisson() GAM", {
    expect_false(has_theta(b_pois))
})

test_that("has_theta() returns FALSE for a gaussian() GAM", {
    expect_false(has_theta(m_gam))
})
