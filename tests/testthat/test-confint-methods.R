## Test confint() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library('gamm4')

## first derivatives of all smooths...
test_that("Point-wise confidence interval for a first derivatives of a GAM works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    fd <- fderiv(m_gam)
    ci <- confint(fd, type = "confidence")
    expect_s3_class(ci, "confint.fderiv")
    expect_s3_class(ci, "data.frame")
    expect_named(ci, expected = c("term", "lower", "est", "upper"))

    expect_warning(confint(fd, level = c(0.95, 0.8), type = "confidence"))

    expect_error(confint(fd, parm = "s(x4)", type = "confidence"))
})

test_that("Simultaneous interval for a first derivatives of a GAM works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    fd <- fderiv(m_gam)
    set.seed(42)
    ci <- confint(fd, parm = "x1", type = "simultaneous", nsim = 1000)
    expect_s3_class(ci, "confint.fderiv")
    expect_s3_class(ci, "data.frame")
    expect_named(ci, expected = c("term", "lower", "est", "upper"))
})

test_that("Point-wise confidence interval for a GAM works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    ci <- confint(m_gam, parm = "s(x1)", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se",
                                  "crit", "lower", "upper"))
})

test_that("Simultaneous interval for a GAM works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    set.seed(42)
    ci <- confint(m_gam, parm = "s(x1)", type = "simultaneous", nsim = 100)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se",
                                  "crit", "lower", "upper"))
})

## 2d smooth
test_that("Point-wise confidence interval for a 2d smooth works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    ci <- confint(su_m_bivar_te, parm = "te(x,z)", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x", "z", "est",
                                  "se", "crit", "lower", "upper"))
})

test_that("Simultaneous interval for a 2d smooth works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    set.seed(42)
    ci <- confint(su_m_bivar_te, parm = "te(x,z)", type = "simultaneous",
                  nsim = 100)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x", "z", "est",
                                  "se", "crit", "lower", "upper"))
})

test_that("Point-wise confidence interval for a GAMM works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    ci <- confint(m_gamm, parm = "s(x1)", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se",
                                  "crit", "lower", "upper"))
})

test_that("Simultaneous interval for a GAMM works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    set.seed(42)
    ci <- confint(m_gamm, parm = "s(x1)", type = "simultaneous", nsim = 100)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se",
                                  "crit", "lower", "upper"))
})

## confint methods for by variables
test_that("Point-wise confidence interval for a GAM with factor by variable works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    ci <- confint(su_m_factor_by_x2, parm = "s(x2)", type = "confidence",
                  partial_match = TRUE)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x2", "est", "se",
                                  "fac", "crit", "lower", "upper"))
    expect_equal(paste0("s(x2):fac", levels(su_eg4[["fac"]])),
                 unique(ci[["smooth"]]))
})

test_that("Simultaneous confidence interval for a GAM with factor by variable works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    ci <- confint(su_m_factor_by_x2, parm = "s(x2)", type = "simultaneous",
                  partial_match = TRUE)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x2", "est", "se",
                                  "fac", "crit", "lower", "upper"))
    expect_equal(paste0("s(x2):fac", levels(su_eg4[["fac"]])),
                 unique(ci[["smooth"]]))
})

## Part of #80
test_that("Point-wise confidence interval for a GAM with selected factor by variable works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    ci <- confint(su_m_factor_by_x2, parm = "s(x2):fac1", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x2", "est", "se",
                                  "fac", "crit", "lower", "upper"))
})

test_that("Point-wise confidence interval for a GAMM works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    ci <- confint(m_gamm4, parm = "s(x1)", type = "confidence")
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se",
                                  "crit", "lower", "upper"))
})

test_that("Simultaneous interval for a GAMM works", {
    withr::local_options(lifecycle_verbosity = "quiet")
    set.seed(42)
    ci <- confint(m_gamm4, parm = "s(x1)", type = "simultaneous", nsim = 100)
    expect_s3_class(ci, "confint.gam")
    expect_s3_class(ci, "tbl_df")
    expect_named(ci, expected = c("smooth", "by_variable", "x1", "est", "se",
                                  "crit", "lower", "upper"))
})

## test snapshots...
test_that("confint.fderiv example output", {
    skip_on_cran()
    skip_on_os("mac")

    withr::local_options(lifecycle_verbosity = "quiet")
    # new data to evaluate the derivatives at, say over the middle 50% of range
    # of each covariate
    middle <- function(x, n = 25, coverage = 0.5) {
      v <- (1 - coverage) / 2
      q <- quantile(x, prob = c(0 + v, 1 - v), type = 8)
      seq(q[1], q[2], length = n)
    }
    new_data <- sapply(su_eg1[c("x0", "x1", "x2", "x3")], middle)
    new_data <- data.frame(new_data)
    ## first derivatives of all smooths...
    fd <- fderiv(m_gam, newdata = new_data)
    ## point-wise interval
    ci <- confint(fd, type = "confidence")
    expect_snapshot_output(ci)
    ## simultaneous interval for smooth term of x2
    set.seed(24)
    x2_sint <- confint(fd, parm = "x2", type = "simultaneous",
                       nsim = 10000, ncores = 2)
    expect_snapshot_output(x2_sint)
})
