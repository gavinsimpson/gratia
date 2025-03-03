## Test confint() methods

var_nms <- c(".estimate", ".se", ".crit", ".lower_ci", ".upper_ci")

## first derivatives of all smooths...
test_that("Point-wise confidence interval for a first derivatives of a GAM works", {
  skip_if_not_installed("withr")
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
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  fd <- fderiv(m_gam)
  ci <- withr::with_seed(
    42,
    confint(fd, parm = "x1", type = "simultaneous", nsim = 1000)
  )
  expect_s3_class(ci, "confint.fderiv")
  expect_s3_class(ci, "data.frame")
  expect_named(ci, expected = c("term", "lower", "est", "upper"))
})

test_that("Point-wise confidence interval for a GAM works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- confint(m_gam, parm = "s(x1)", type = "confidence")
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x1", var_nms))
})

test_that("Simultaneous interval for a GAM works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- withr::with_seed(
    42,
    confint(m_gam, parm = "s(x1)", type = "simultaneous", nsim = 100)
  )
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x1", var_nms))
})

## 2d smooth
test_that("Point-wise confidence interval for a 2d smooth works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- confint(su_m_bivar_te, parm = "te(x,z)", type = "confidence")
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x", "z", var_nms))
})

test_that("Simultaneous interval for a 2d smooth works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- withr::with_seed(
    42,
    confint(su_m_bivar_te,
      parm = "te(x,z)", type = "simultaneous",
      nsim = 100
    )
  )
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x", "z", var_nms))
})

test_that("Point-wise confidence interval for a GAMM works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_not_installed("withr")
  ci <- confint(m_gamm, parm = "s(x1)", type = "confidence")
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x1", var_nms))
})

test_that("Simultaneous interval for a GAMM works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- withr::with_seed(
    42,
    confint(m_gamm, parm = "s(x1)", type = "simultaneous", nsim = 100)
  )
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x1", var_nms))
})

## confint methods for by variables
test_that("Point-wise confidence interval for a GAM with factor by variable works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- confint(su_m_factor_by_x2,
    parm = "s(x2)", type = "confidence",
    partial_match = TRUE
  )
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(
    ".smooth", ".type", ".by", "x2", "fac",
    var_nms
  ))
  expect_equal(
    paste0("s(x2):fac", levels(su_eg4[["fac"]])),
    unique(ci[[".smooth"]])
  )
})

test_that("Simultaneous confidence interval for a GAM with factor by variable works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- confint(su_m_factor_by_x2,
    parm = "s(x2)", type = "simultaneous",
    partial_match = TRUE
  )
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x2", "fac", var_nms))
  expect_equal(
    paste0("s(x2):fac", levels(su_eg4[["fac"]])),
    unique(ci[[".smooth"]])
  )
})

## Part of #80
test_that("Point-wise confidence interval for a GAM with selected factor by variable works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- confint(su_m_factor_by_x2, parm = "s(x2):fac1", type = "confidence")
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x2", "fac", var_nms))
})

test_that("Point-wise confidence interval for a GAMM works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- confint(m_gamm4, parm = "s(x1)", type = "confidence")
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x1", var_nms))
})

test_that("Simultaneous interval for a GAMM works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ci <- withr::with_seed(
    42,
    confint(m_gamm4, parm = "s(x1)", type = "simultaneous", nsim = 100)
  )
  expect_s3_class(ci, "confint.gam")
  expect_s3_class(ci, "tbl_df")
  expect_named(ci, expected = c(".smooth", ".type", ".by", "x1", var_nms))
})

## test snapshots...
test_that("confint.fderiv example output", {
  skip_on_cran()
  skip_on_os("win")

  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  # new data to evaluate the derivatives at, say over the middle 50% of range
  # of each covariate
  middle <- function(x, n = 25, coverage = 0.5) {
    v <- (1 - coverage) / 2
    q <- quantile(x, prob = c(0 + v, 1 - v), type = 8)
    seq(q[1], q[2], length = n)
  }
  n_middle <- 25
  new_data <- vapply(su_eg1[c("x0", "x1", "x2", "x3")], FUN = middle,
    FUN.VALUE = numeric(n_middle), n = n_middle)
  new_data <- data.frame(new_data)
  ## first derivatives of all smooths...
  fd <- fderiv(m_gam, newdata = new_data)
  ## point-wise interval
  ci <- confint(fd, type = "confidence")
  expect_snapshot_output(ci)
  ## simultaneous interval for smooth term of x2
  x2_sint <- withr::with_seed(
    24,
    confint(fd,
      parm = "x2", type = "simultaneous", nsim = 10000,
      ncores = 2
    )
  )

  skip_on_ci()
  skip_on_covr()
  skip_on_os(os = c("linux", "windows"))
  expect_snapshot_output(x2_sint)
})
