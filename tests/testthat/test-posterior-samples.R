## Test posterior sampling functions

## load packages
library("testthat")
library("mgcv")
library("gratia")

context("Testing smooth_samples() methods")

set.seed(12398)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m1 <- gam(y ~ s(x0), data = dat, method = "REML")
m2 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

set.seed(34786)
dat2 <- gamSim(4, verbose = FALSE)
m3 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat2, method = "REML")

test_that("smooth_samples works for a simple GAM", {
    expect_warning(sm <- smooth_samples(m1, n = 5, n_vals = 100, seed = 42),
                   "foo")
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 500 == 1 smooth * 5 * 100
    expect_identical(NROW(sm), 500L)
    expect_identical(NCOL(sm), 7L) # 7 cols, univatiate smooths
})

test_that("smooth_samples works for a multi-smooth GAM", {
    expect_silent(sm <- smooth_samples(m2, n = 5, n_vals = 100, seed = 42))
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 4 smooths * 5 * 100
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 7L) # 7 cols, univatiate smooths
})

test_that("smooth_samples works for a multi-smooth factor by GAM", {
    expect_silent(sm <- smooth_samples(m3, n = 5, n_vals = 100, seed = 42))
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 1 + (1 * 3) smooths * 5 * 100
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 8L) # 8 cols, univatiate smooths with factor
})
