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

test_that("simulate() works with a gam", {
    sims <- simulate(m1, nsim = 5, seed = 42)
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
    data(smallAges)
    smallAges$Error[1] <- 1.1
    sw <- scam(Date ~ s(Depth, k = 5, bs = "mpd"), data = smallAges,
               weights = 1 / smallAges$Error, gamma = 1.4)
    sims <- simulate(sw, nsim = 5, seed = 42)
    expect_identical(nrow(sims), 12L)
    expect_identical(ncol(sims), 5L)
})

