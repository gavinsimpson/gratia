## Test confint() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")

# Fit an example GJRM model using the example from ?GJRM::gamlss

test_that("draw works for a simple GJRM gamlss", {
    skip_if_not_installed("GJRM", minimum_version = "0.2-6")
    skip_on_cran()
    skip_on_os("win")
    skip_on_os("mac")

    # follow example from ?GJRM::gamlss
    suppressPackageStartupMessages(library("GJRM"))
    set.seed(0)
    n <- 400
    x1 <- round(runif(n))
    x2 <- runif(n)
    x3 <- runif(n)
    f1 <- function(x) cos(pi * 2 * x) + sin(pi * x)
    y1 <- -1.55 + 2 * x1 + f1(x2) + rnorm(n)
    dataSim <- data.frame(y1, x1, x2, x3)
    eq.mu <- y1 ~ x1 + s(x2) + s(x3)
    eq.s  <-    ~ s(x3)
    fl    <- list(eq.mu, eq.s)
    m_gamlss_gjrm <- GJRM::gamlss(fl, data = dataSim)
    expect_silent(plt <- draw(m_gamlss_gjrm))

    expect_doppelganger("draw-simple-gjrm-gamlss", plt)
})
