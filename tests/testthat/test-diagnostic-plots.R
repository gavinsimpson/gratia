## Test qq_plot() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")
library("vdiffr")

context("diagnostic-plots")
theme_set(theme_grey())

set.seed(2)
## simulate some data...
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

test_that("appraise() works", {
    plt <- appraise(mod)
    expect_doppelganger("appraise diagnostic plots", plt)
})

test_that("appraise() fails if n_bins not numeric or one of character options", {
    msg <- paste("'arg' should be one of",
                 paste(dQuote(c('sturges', 'scott', 'fd')), collapse = ", "))
    expect_error(appraise(mod, n_bins = "foo"), msg, fixed = TRUE)
    msg <- paste("'n_bins' should be a number or one of:",
                 paste(dQuote(c('sturges', 'scott', 'fd')), collapse = ", "))
    expect_error(appraise(mod, n_bins = TRUE), msg, fixed = TRUE)
})

test_that("residuals_hist_plot fails if non-numeric n_bins doesn't match character options", {
    msg <- paste("'arg' should be one of",
                 paste(dQuote(c('sturges', 'scott', 'fd')), collapse = ", "))
    expect_error(residuals_hist_plot(mod, n_bins = "foo"), msg, fixed = TRUE)
    msg <- paste("'n_bins' should be a number or one of:",
                 paste(dQuote(c('sturges', 'scott', 'fd')), collapse = ", "))
    expect_error(residuals_hist_plot(mod, n_bins = TRUE), msg, fixed = TRUE)
})
