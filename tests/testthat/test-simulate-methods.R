## Test simulate() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("scam")

context("Testing simulate methods")

set.seed(1)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
m2 <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

data(smallAges)
smallAges$Error[1] <- 1.1
sw <- scam(Date ~ s(Depth, k = 5, bs = "mpd"), data = smallAges,
           weights = 1 / smallAges$Error, gamma = 1.4)

test_that("simulate() works with a gam", {
    sims <- simulate(m1, nsim = 5, seed = 42)
    expect_identical(nrow(sims), 400L)
    expect_identical(ncol(sims), 5L)

    sims <- simulate(m1, nsim = 5, seed = 42, newdata = dat)
    expect_identical(nrow(sims), 400L)
    expect_identical(ncol(sims), 5L)
})

test_that("simulate() works with a gamm", {
    sims <- simulate(m2, nsim = 5, seed = 42)
    expect_identical(nrow(sims), 400L)
    expect_identical(ncol(sims), 5L)
})

## monotonic spline age-depth model using scam() from pkg scam
test_that("simulate() works with a scam", {
    sims <- simulate(sw, nsim = 5, seed = 42)
    expect_identical(nrow(sims), 12L)
    expect_identical(ncol(sims), 5L)

    sims <- simulate(sw, nsim = 5, seed = 42, newdata = smallAges)
    expect_identical(nrow(sims), 12L)
    expect_identical(ncol(sims), 5L)
})

test_that("simulate() works with no .Random.seed", {
    rm(".Random.seed", envir = globalenv())
    sims <- simulate(m1, nsim = 5, seed = 42)
    expect_identical(nrow(sims), 400L)
    expect_identical(ncol(sims), 5L)

    rm(".Random.seed", envir = globalenv())
    sims <- simulate(m2, nsim = 5, seed = 42)
    expect_identical(nrow(sims), 400L)
    expect_identical(ncol(sims), 5L)

    rm(".Random.seed", envir = globalenv())
    sims <- simulate(sw, nsim = 5, seed = 42)
    expect_identical(nrow(sims), 12L)
    expect_identical(ncol(sims), 5L)
})

test_that("simulate() works with out a seed", {
    sims <- simulate(m1, nsim = 5)
    expect_identical(nrow(sims), 400L)
    expect_identical(ncol(sims), 5L)

    sims <- simulate(m2, nsim = 5)
    expect_identical(nrow(sims), 400L)
    expect_identical(ncol(sims), 5L)

    sims <- simulate(sw, nsim = 5)
    expect_identical(nrow(sims), 12L)
    expect_identical(ncol(sims), 5L)
})

test_that("simulate() fails if we don't have an rd function", {
    ## Example from ?gevlss
    Fi.gev <- function(z,mu,sigma,xi) {
        ## GEV inverse cdf.
        xi[abs(xi)<1e-8] <- 1e-8 ## approximate xi=0, by small xi
        x <- mu + ((-log(z))^-xi-1)*sigma/xi
    }

    ## simulate test data...
    f0 <- function(x) 2 * sin(pi * x)
    f1 <- function(x) exp(2 * x)
    f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
                          (10 * x)^3 * (1 - x)^10
    set.seed(1)
    n <- 250
    x0 <- runif(n)
    x1 <- runif(n)
    x2 <- runif(n)
    mu <- f2(x2)
    rho <- f0(x0)
    xi <- (f1(x1)-4)/9
    y <- Fi.gev(runif(n), mu, exp(rho), xi)
    dat <- data.frame(y, x0, x1, x2)

    ## fit model....
    mgev <- gam(list(y ~ s(x2), ~ s(x0), ~ s(x1)), family = gevlss, data = dat)

    expect_error(simulate(mgev),
                 "Don't yet know how to simulate from family <gevlss>",
                 fixed = TRUE)
})
