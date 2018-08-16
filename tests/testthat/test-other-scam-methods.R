## Test other scam methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("scam")

context("Testing other scam methods")

data(smallAges)
smallAges$Error[1] <- 1.1
sw <- scam(Date ~ s(Depth, k = 5, bs = "mpd"), data = smallAges,
           weights = 1 / smallAges$Error, gamma = 1.4)

test_that("vcov.scam works", {
    V <- vcov(sw)
    expect_identical(dim(V), c(5L, 5L))
    V <- vcov(sw, dispersion = 2)
    expect_identical(dim(V), c(5L, 5L))
    V <- vcov(sw, freq = TRUE)
    expect_identical(dim(V), c(5L, 5L))
    V <- vcov(sw, freq = TRUE, parametrized = TRUE)
    expect_identical(dim(V), c(5L, 5L))
    V <- vcov(sw, freq = TRUE, parametrized = FALSE)
    expect_identical(dim(V), c(5L, 5L))
    V <- vcov(sw, freq = FALSE, parametrized = FALSE)
    expect_identical(dim(V), c(5L, 5L))
})

test_that("coef.scam works", {
    beta <- coef(sw)
    expect_identical(length(beta), 5L)
    beta <- coef(sw, parametrized = FALSE)
    expect_identical(length(beta), 5L)
})
