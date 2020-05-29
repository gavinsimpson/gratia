## Test difference_smooths()

## load packages
library("testthat")
library("mgcv")
library("gratia")
library("gamm4")

context("Test difference_smooths()")

set.seed(23)
df <- data_sim("eg4", n = 100)
m_gamm4 <- gamm4(y ~ fac + s(x2, by = fac) + s(x0), data = df)

test_that("difference_smooths() works for a gam model", {
    m_gam <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")
    expect_silent(ds <- difference_smooths(m_gam, smooth = "s(x2)"))
    expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))
})

test_that("difference_smooths() works for a bam model", {
    skip_on_cran()
    m_bam <- bam(y ~ fac + s(x2, by = fac) + s(x0), data = df)
    expect_silent(ds <- difference_smooths(m_bam, smooth = "s(x2)"))
    expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))
})

test_that("difference_smooths() works for a gamm model", {
    skip_on_cran()
    m_gamm <- gamm(y ~ fac + s(x2, by = fac) + s(x0), data = df)
    expect_silent(ds <- difference_smooths(m_gamm, smooth = "s(x2)"))
    expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))
})

test_that("difference_smooths() works for a gamm4 model", {
    skip_on_cran()
    m_gamm4 <- gamm4(y ~ fac + s(x2, by = fac) + s(x0), data = df)
    expect_silent(ds <- difference_smooths(m_gamm4, smooth = "s(x2)"))
    expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))
})
