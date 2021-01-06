## Test qq_plot() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")

context("qq_plot-methods")

## Need a local wrapper to allow conditional use of vdiffr
`expect_doppelganger` <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

## simulate binomial data...
set.seed(0)
n.samp <- 200
dat <- gamSim(1, n = n.samp, dist = "binary", scale = .33, verbose = FALSE)
p <- binomial()$linkinv(dat$f)               # binomial p
n <- sample(c(1, 3), n.samp, replace = TRUE) # binomial n
dat <- transform(dat, y = rbinom(n, n, p), n = n)
m <- gam(y / n ~ s(x0) + s(x1) + s(x2) + s(x3),
         family = binomial, data = dat, weights = n,
         method = "REML")

types <- dQuote(c("deviance", "response", "pearson"))
methods <- dQuote(c("direct", "simulate", "normal"))

test_that("qq_plot() direct method works", {
    plt <- qq_plot(m)      # direct randomisation of uniform quantiles
    expect_doppelganger("qq_plot direct randomisation", plt)
})

test_that("qq_plot() direct method works with response residuals", {
    plt <- qq_plot(m, type = "response")
    expect_doppelganger("qq_plot direct randomisation response residuals", plt)
})

test_that("qq_plot() direct method works with pearson residuals", {
    plt <- qq_plot(m, type = "pearson")
    expect_doppelganger("qq_plot direct randomisation pearson residuals", plt)
})

test_that("qq_plot() normal method works", {
    plt <- qq_plot(m, method = "normal") # normality assumption
    expect_doppelganger("qq_plot normality assumption", plt)
})

test_that("qq_plot() normal method works", {
    plt <- qq_plot(m, method = "normal", type = "response")
    expect_doppelganger("qq_plot normality assumption response residuals", plt)
})

test_that("qq_plot() normal method works", {
    plt <- qq_plot(m, method = "normal", type = "pearson")
    expect_doppelganger("qq_plot normality assumption pearson residuals", plt)
})

test_that("qq_plot() simulate method works", {
    plt <- qq_plot(m, method = "simulate") # simulate data to get quantiles
    expect_doppelganger("qq_plot data simulation", plt)
})

test_that("qq_plot() simulate method works", {
    plt <- qq_plot(m, method = "simulate", type = "response")
    expect_doppelganger("qq_plot data simulation response residuals", plt)
})

test_that("qq_plot() simulate method works", {
    plt <- qq_plot(m, method = "simulate", type = "pearson")
    expect_doppelganger("qq_plot data simulation pearson residuals", plt)
})

test_that("qq_plot() fails if unsupported residuals requested", {
    expect_error(qq_plot(m, type = "scaled.pearson"),
                 paste("'arg' should be one of", paste(types, collapse = ', ')),
                 fixed = TRUE)
})

test_that("qq_plot() fails if unsupported method requested", {
    expect_error(qq_plot(m, method = "foo"),
                 paste("'arg' should be one of", paste(methods, collapse = ', ')),
                 fixed = TRUE)
})

test_that("qq_plot.default fails with error", {
    expect_error(qq_plot(dat),
                 "Unable to produce a Q-Q plot for <data.frame>")
})

test_that("pearson_residuals fails if no var_fun available", {
    expect_error(pearson_residuals(var_fun = NULL),
                 "Pearson residuals are not available for this family.",
                 fixed = TRUE)
})
