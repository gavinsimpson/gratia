## Test rootograms() and related residuals functions

## load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("rootogram works for a continuous Gaussian response", {
    skip_on_cran()
    expect_silent(rg <- rootogram(m_gam))
    expect_doppelganger("draw gaussian rootogram", draw(rg))
})

test_that("rootogram works for a discrete Poisson response", {
    expect_silent(rg <- rootogram(b_pois))
    expect_doppelganger("draw poisson rootogram", draw(rg))
})

test_that("rootogram works for a discrete negative binomial response", {
    skip_on_cran()
    expect_silent(rg <- rootogram(m_nb))
    expect_doppelganger("draw neg bin rootogram", draw(rg))
})

test_that("rootogram fails for a a non-supported response", {
    skip_on_cran()
    expect_error(rootogram(m_tw),
      "Only <Poisson, Negative Binomial, Gaussian> models supported.",
      fixed = TRUE)
})
