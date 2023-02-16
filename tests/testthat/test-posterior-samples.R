## Test posterior sampling functions

## load packages
library("testthat")
library("mgcv")
library("gratia")

test_that("smooth_samples works for a continuous by GAM", {
    expect_silent(sm <- smooth_samples(su_m_cont_by, n = 5, n_vals = 100,
                                       seed = 42))
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 500 == 1 smooth * 5 * 100
    expect_identical(NROW(sm), 500L)
    expect_identical(NCOL(sm), 8L) # 8 cols, univatiate smooths
    skip_on_cran()
    skip_on_ci()
    expect_snapshot(sm)
})

test_that("smooth_samples works for a simple GAM", {
    expect_silent(sm <- smooth_samples(m_1_smooth, n = 5, n_vals = 100,
                                       seed = 42))
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 500 == 1 smooth * 5 * 100
    expect_identical(NROW(sm), 500L)
    expect_identical(NCOL(sm), 8L) # 8 cols, univatiate smooths
    skip_on_cran()
    skip_on_ci()
    expect_snapshot(sm)
})

test_that("smooth_samples works for a simple GAM multi rng calls", {
    expect_silent(sm <- smooth_samples(m_1_smooth, n = 5, n_vals = 100,
                                       seed = 42, rng_per_smooth = TRUE))
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 500 == 1 smooth * 5 * 100
    expect_identical(NROW(sm), 500L)
    expect_identical(NCOL(sm), 8L) # 8 cols, univatiate smooths
    skip_on_cran()
    skip_on_ci()
    expect_snapshot(sm)
})

test_that("smooth_samples works for a simple GAM MH sampling", {
    expect_silent(sm <- smooth_samples(m_1_smooth, n = 5, n_vals = 100,
                                       method = "mh", seed = 42))
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 500 == 1 smooth * 5 * 100
    expect_identical(NROW(sm), 500L)
    expect_identical(NCOL(sm), 8L) # 8 cols, univatiate smooths
})

test_that("smooth_samples works for a multi-smooth GAM", {
    expect_silent(sm <- smooth_samples(m_gam, n = 5, n_vals = 100, seed = 42))
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 4 smooths * 5 * 100
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 11L) # 11 cols, 4 univatiate smooths
})

test_that("smooth_samples works for a multi-smooth factor by GAM", {
    expect_silent(sm <- smooth_samples(su_m_factor_by, n = 5, n_vals = 50,
                                       seed = 42))
    expect_s3_class(sm, c("smooth_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 1 + (1 * 3) smooths * 5 * 50
    expect_identical(NROW(sm), 1000L)
    expect_identical(NCOL(sm), 10L) # 10 cols, univatiate smooths with factor
})

test_that("smooth_samples() fails if not suitable method available", {
    expect_error(smooth_samples(1:10),
                 "Don't know how to sample from the posterior of <integer>",
                 fixed = TRUE)
})

test_that("smooth_samples sets seed when seed not provided", {
    expect_silent(smooth_samples(m_gam, seed = NULL))
})

test_that("smooth_samples works with term provided", {
    expect_silent(sm <- smooth_samples(m_gam, term = "s(x2)", seed = 42))
})

test_that("smooth_samples errors with invalid term provided", {
    expect_error(sm <- smooth_samples(m_gam, term = "s(x10)", seed = 42),
                 "None of the terms matched a smooth.", fixed = TRUE)
})

# from #121
test_that("smooth_samples gets the right factor by smooth: #121", {
    expect_silent(sm <- smooth_samples(su_m_factor_by, n = 5, n_vals = 100,
                                       term = "s(x2):fac2", seed = 42))
    # factor level of `fac` column should be 2
    expect_identical(all(sm["fac"] == 2), TRUE)
})

# from #121 - problems when model contains ranef smooths
test_that("smooth_samples ignores ranef smooths: #121", {
    expect_message(sm <- smooth_samples(rm1, n = 5, n_vals = 100, seed = 42),
                   "Random effect smooths not currently supported.")
    # given n and n_vals and 4 smooths, nrow == 2000L
    expect_identical(nrow(sm), 2000L)
    # shouldn't have "s(fac)" in sm
    expect_identical(any(sm$smooth == "s(fac)"), FALSE)
})

test_that("smooth_samples fails if no smooths left to sample from", {
    expect_error(sm <- smooth_samples(rm1, term = "s(fac)",
                                        n = 5, n_vals = 100, seed = 42),
                 "No smooths left that can be sampled from.")
})

test_that("fitted_samples works for a simple GAM", {
    expect_silent(sm <- fitted_samples(m_1_smooth, n = 5, seed = 42))
    expect_s3_class(sm, c("fitted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 1000 == 5 * 200 (nrow(dat))
    expect_identical(NROW(sm), 1000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "fitted"))
})

test_that("fitted_samples works for a multi-smooth GAM", {
    expect_silent(sm <- fitted_samples(m_gam, n = 5, seed = 42))
    expect_s3_class(sm, c("fitted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 5000 == 5 draws * 1000 observations in data
    expect_identical(NROW(sm), 5000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "fitted"))
})

test_that("fitted_samples works for a multi-smooth factor by GAM", {
    expect_silent(sm <- fitted_samples(su_m_factor_by, n = 5, seed = 42))
    expect_s3_class(sm, c("fitted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 5 draws * 400 observations in data
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "fitted"))
})

test_that("fitted_samples sets seed when seed not provided", {
    expect_silent(fitted_samples(m_gam, seed = NULL))
})

test_that("fitted_samples() fails if not suitable method available", {
    expect_error(fitted_samples(1:10),
                 "Don't know how to sample from the posterior of <integer>",
                 fixed = TRUE)
})

test_that("predicted_samples works for a simple GAM", {
    expect_silent(sm <- predicted_samples(m_1_smooth, n = 5, seed = 42))
    expect_s3_class(sm, c("predicted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 5 * 100 (nrow(dat))
    expect_identical(NROW(sm), 1000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "response"))
})

test_that("predicted_samples works for a multi-smooth GAM", {
    expect_silent(sm <- predicted_samples(m_gam, n = 5, seed = 42))
    expect_s3_class(sm, c("predicted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 5000 == 5 draws * 1000 observations in data
    expect_identical(NROW(sm), 5000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "response"))
})

test_that("predicted_samples works for a multi-smooth factor by GAM", {
    expect_silent(sm <- predicted_samples(su_m_factor_by, n = 5, seed = 42))
    expect_s3_class(sm, c("predicted_samples", "posterior_samples", "tbl_df",
                          "tbl", "data.frame"))
    ## 2000 == 5 draws * 400 observations in data
    expect_identical(NROW(sm), 2000L)
    expect_identical(NCOL(sm), 3L) # 3 cols
    expect_named(sm, expected = c("row", "draw", "response"))
})

test_that("predicted_samples sets seed when seed not provided", {
    expect_silent(predicted_samples(m_gam, seed = NULL))
})

test_that("predicted_samples() fails if not suitable method available", {
    expect_error(predicted_samples(1:10),
                 "Don't know how to sample from the posterior of <integer>",
                 fixed = TRUE)
})

test_that("posterior_samples() fails if no suitable method available", {
    expect_error(posterior_samples(1:10),
                 "Don't know how to sample from the posterior of <integer>",
                 fixed = TRUE)
})

test_that("fitted_samples example output doesn't change", {
    skip_on_cran()
    skip_on_os("mac")
    fs <- fitted_samples(m_gam, n = 5, seed = 42)
    expect_snapshot(fs)
})

test_that("smooth_samples example output doesn't change", {
    skip_on_cran()
    skip_on_os("mac")
    samples <- smooth_samples(m_gam, term = "s(x0)", n = 5, seed = 42)
    expect_snapshot(samples)
})
