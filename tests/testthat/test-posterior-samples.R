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

set.seed(42)
cont_by_data <- gamSim(3, n = 400, verbose = FALSE)
cont_by_gam <- gam(y ~ s(x2, by = x1), data = cont_by_data)

test_that("smooth_samples works for a continuous by GAM", {
    expect_silent(sm <- smooth_samples(cont_by_gam, n = 5, n_vals = 100, seed = 42))
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 500 == 1 smooth * 5 * 100
    expect_identical(NROW(sm), 500L)
    expect_identical(NCOL(sm), 7L) # 7 cols, univatiate smooths
})

test_that("smooth_samples works for a simple GAM", {
    expect_silent(sm <- smooth_samples(m1, n = 5, n_vals = 100, seed = 42))
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

test_that("smooth_samples() fails if not suitable method available", {
    expect_error(smooth_samples(1:10),
                 "Don't know how to sample from the posterior of <integer>",
                 fixed = TRUE)
})

test_that("smooth_samples sets seed when seed not provided", {
    expect_silent(smooth_samples(m2, seed = NULL))
})

test_that("smooth_samples works with term provided", {
    expect_silent(sm <- smooth_samples(m2, term = "s(x2)", seed = 42))
})

test_that("smooth_samples errors with invalid term provided", {
    expect_error(sm <- smooth_samples(m2, term = "s(x10)", seed = 42),
                 "None of the terms matched a smooth.", fixed = TRUE)
})

context("Testing fitted_samples() methods")

test_that("fitted_samples works for a simple GAM", {
    expect_silent(sm <- fitted_samples(m1, n = 5, seed = 42))
    expect_s3_class(sm, c("fitted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 5 * 400 (nrow(dat))
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "fitted"))
})

test_that("fitted_samples works for a multi-smooth GAM", {
    expect_silent(sm <- fitted_samples(m2, n = 5, seed = 42))
    expect_s3_class(sm, c("fitted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 5 draws * 400 observations in data
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "fitted"))
})

test_that("fitted_samples works for a multi-smooth factor by GAM", {
    expect_silent(sm <- fitted_samples(m3, n = 5, seed = 42))
    expect_s3_class(sm, c("fitted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 5 draws * 400 observations in data
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "fitted"))
})

test_that("fitted_samples sets seed when seed not provided", {
    expect_silent(fitted_samples(m2, seed = NULL))
})

test_that("fitted_samples() fails if not suitable method available", {
    expect_error(fitted_samples(1:10),
                 "Don't know how to sample from the posterior of <integer>",
                 fixed = TRUE)
})

context("Testing predicted_samples() methods")

test_that("predicted_samples works for a simple GAM", {
    expect_silent(sm <- predicted_samples(m1, n = 5, seed = 42))
    expect_s3_class(sm, c("predicted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 5 * 400 (nrow(dat))
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "response"))
})

test_that("predicted_samples works for a multi-smooth GAM", {
    expect_silent(sm <- predicted_samples(m2, n = 5, seed = 42))
    expect_s3_class(sm, c("predicted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 5 draws * 400 observations in data
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "response"))
})

test_that("predicted_samples works for a multi-smooth factor by GAM", {
    expect_silent(sm <- predicted_samples(m3, n = 5, seed = 42))
    expect_s3_class(sm, c("predicted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 5 draws * 400 observations in data
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "response"))
})

test_that("predicted_samples sets seed when seed not provided", {
    expect_silent(predicted_samples(m2, seed = NULL))
})

test_that("predicted_samples() fails if not suitable method available", {
    expect_error(predicted_samples(1:10),
                 "Don't know how to sample from the posterior of <integer>",
                 fixed = TRUE)
})

context("Testing posterior_samples() methods")

test_that("posterior_samples() fails if not suitable method available", {
    expect_error(posterior_samples(1:10),
                 "Don't know how to sample from the posterior of <integer>",
                 fixed = TRUE)
})
