## load packages
library("testthat")
library("gratia")
library("mgcv")

test_that("draw.gamlss can plot a GJRM gamlss model", {
    testthat::skip_if_not_installed("GJRM")
    skip_on_cran()

    library("GJRM")
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
    m <- gamlss(fl, data = dataSim)

    expect_silent(plt <- draw(m))
    expect_doppelganger("draw_gamlss", plt)
})
