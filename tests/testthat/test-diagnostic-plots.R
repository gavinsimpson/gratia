## Test qq_plot() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")
library("vdiffr")

context("diagnostic-plots")
theme_set(theme_grey())

library(mgcv)
set.seed(2)
## simulate some data...
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

test_that("appraise() works", {
    plt <- appraise(mod)
    expect_doppelganger("appraise diagnostic plots", plt)
})
