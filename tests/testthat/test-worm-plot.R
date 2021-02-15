## Test worm_plot() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
## library("ggplot2")

context("worm_plot-methods")

## Need a local wrapper to allow conditional use of vdiffr
`expect_doppelganger` <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

## simulate binomial data...
set.seed(0)
n.samp <- 200
dat <- data_sim("eg1", n = n.samp, dist = "binary",
                scale = .33, seed = 0)
p <- binomial()$linkinv(dat$f)               # binomial p
n <- sample(c(1, 3), n.samp, replace = TRUE) # binomial n
dat <- transform(dat, y = rbinom(n, n, p), n = n)
m <- gam(y / n ~ s(x0) + s(x1) + s(x2) + s(x3),
         family = binomial, data = dat, weights = n,
         method = "REML")

types <- dQuote(c("deviance", "response", "pearson"))
methods <- dQuote(c("uniform", "simulate", "normal"))

test_that("worm_plot() uniform method works", {
    plt <- worm_plot(m)      # uniform randomisation of uniform quantiles
    expect_doppelganger("worm_plot uniform randomisation", plt)
})

test_that("worm_plot() uniform method works with response residuals", {
    plt <- worm_plot(m, type = "response")
    expect_doppelganger("worm_plot uniform randomisation response residuals", plt)
})

test_that("worm_plot() uniform method works with pearson residuals", {
    plt <- worm_plot(m, type = "pearson")
    expect_doppelganger("worm_plot uniform randomisation pearson residuals", plt)
})

test_that("worm_plot() normal method works", {
    plt <- worm_plot(m, method = "normal") # normality assumption
    expect_doppelganger("worm_plot normality assumption", plt)
})

test_that("worm_plot() normal method works", {
    plt <- worm_plot(m, method = "normal", type = "response")
    expect_doppelganger("worm_plot normality assumption response residuals", plt)
})

test_that("worm_plot() normal method works", {
    plt <- worm_plot(m, method = "normal", type = "pearson")
    expect_doppelganger("worm_plot normality assumption pearson residuals", plt)
})

test_that("worm_plot() simulate method works", {
    plt <- worm_plot(m, method = "simulate") # simulate data to get quantiles
    expect_doppelganger("worm_plot data simulation", plt)
})

test_that("worm_plot() simulate method works", {
    plt <- worm_plot(m, method = "simulate", type = "response")
    expect_doppelganger("worm_plot data simulation response residuals", plt)
})

test_that("worm_plot() simulate method works", {
    plt <- worm_plot(m, method = "simulate", type = "pearson")
    expect_doppelganger("worm_plot data simulation pearson residuals", plt)
})

test_that("worm_plot() fails if unsupported residuals requested", {
    expect_error(worm_plot(m, type = "scaled.pearson"),
                 paste("'arg' should be one of", paste(types, collapse = ', ')),
                 fixed = TRUE)
})

test_that("worm_plot() fails if unsupported method requested", {
    expect_error(worm_plot(m, method = "foo"),
                 paste("'arg' should be one of", paste(methods, collapse = ', ')),
                 fixed = TRUE)
})

test_that("worm_plot() prints message if direct method requested", {
    expect_message(worm_plot(m, method = "direct"),
                   "`method = \"direct\"` is deprecated, use `\"uniform\"`",
                   fixed = TRUE)
})

test_that("worm_plot.default fails with error", {
    expect_error(worm_plot(dat),
                 "Unable to produce a worm plot for <data.frame>")
})

