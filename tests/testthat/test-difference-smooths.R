## Test difference_smooths()

## load packages
library("testthat")
library("mgcv")
library("gratia")
library("gamm4")

test_that("difference_smooths() works for a gam model", {
    expect_silent(ds <- difference_smooths(su_m_factor_by, smooth = "s(x2)"))
    expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

    ## plot
    plt <- draw(ds)
    expect_doppelganger("draw difference_smooths gam", plt)
})

test_that("difference_smooths() works for a gam model fixed scales", {
    expect_silent(ds <- difference_smooths(su_m_factor_by, smooth = "s(x2)"))
    expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

    ## plot
    plt <- draw(ds, scales = "fixed")
    expect_doppelganger("draw difference_smooths gam fixed scales", plt)
})

test_that("difference_smooths() works for a bam model", {
    skip_on_cran()
    expect_silent(ds <- difference_smooths(su_m_factor_by_bam,
                                           smooth = "s(x2)"))
    expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

    ## plot
    plt <- draw(ds)
    expect_doppelganger("draw difference_smooths bam", plt)
})

test_that("difference_smooths() works for a gamm model", {
    skip_on_cran()
    skip_on_os(c("windows", "mac"))
    expect_silent(ds <- difference_smooths(su_m_factor_by_gamm,
                                           smooth = "s(x2)"))
    expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

    ## plot
    plt <- draw(ds)
    expect_doppelganger("draw difference_smooths gamm", plt)
})

test_that("difference_smooths() works for a gamm4 model", {
    skip_on_cran()
    skip_on_os(c("windows", "mac"))
    expect_silent(ds <- difference_smooths(su_m_factor_by_gamm4,
                                           smooth = "s(x2)"))
    expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

    ## plot
    plt <- draw(ds)
    expect_doppelganger("draw difference_smooths gamm4", plt)
})
