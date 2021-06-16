## Test edf()

## load packages
library("testthat")
library("gratia")
library("mgcv")

context("Test edf()")

test_that("edf() works as expected", {
    skip_on_cran()
    N <- 500L
    df <- data_sim("eg1", n = N, seed = 42)
    ## fit the model
    m_reml <-  gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df,
                   method = "REML")
    m_gcv  <-  gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df,
                   method = "GCV.Cp")
    expect_silent(edfs <- edf(m_reml))
    expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(edfs), 4L)
    expect_identical(ncol(edfs), 2L)
    expect_named(edfs, c("smooth", "edf"))

    expect_silent(edfs <- edf(m_reml, smooth = c("s(x0)", "s(x2)")))
    expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(edfs), 2L)
    expect_identical(ncol(edfs), 2L)
    expect_named(edfs, c("smooth", "edf"))

    expect_silent(edfs <- edf(m_reml, type = "unconditional"))
    expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(edfs), 4L)
    expect_identical(ncol(edfs), 2L)
    expect_named(edfs, c("smooth", "edf"))

    expect_silent(edfs <- edf(m_reml, type = "alternative"))
    expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(edfs), 4L)
    expect_identical(ncol(edfs), 2L)
    expect_named(edfs, c("smooth", "edf"))

    expect_warning(edf(m_gcv, type = "unconditional"),
                   "Smoothness parameter uncertainty unavailable; using `type = \"default\"`",
                   fixed = TRUE)
})