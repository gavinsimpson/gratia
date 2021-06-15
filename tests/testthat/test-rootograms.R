## Test rootograms() and related residuals functions

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("vdiffr")

context("Test rootograms")

## Need a local wrapper to allow conditional use of vdiffr
`expect_doppelganger` <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

N <- 500L
df_gauss <- data_sim("eg1", n = N, seed = 42)
df_pois  <- data_sim("eg1", dist = "poisson", n = N, scale = 0.2, seed = 42)
## fit the model
m_gauss <-  gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df_gauss,
                method = "REML", family = gaussian())
b_pois  <-  bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df_pois,
                method = "fREML", family = poisson())
m_nb    <-  gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df_pois,
                method = "REML", family = nb())
m_tw    <-  gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df_pois,
                method = "REML", family = tw())

test_that("rootogram works for a continuous Gaussian response", {
    skip_on_cran()
    expect_silent(rg <- rootogram(m_gauss))
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