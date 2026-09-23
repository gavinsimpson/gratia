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

## Issue #365
test_that("shift adds only the intercept to factor-by tensor product smooths", {
  withr::local_seed(1)
  dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
  dat$f <- factor(sample(c("A", "B"), nrow(dat), replace = TRUE))
  fit <- mgcv::gam(y ~ te(x0, x1, by = f) + f, data = dat)
  predgrid <- data.frame(x0 = 0.5, x1 = 0.5, f = factor(c("A", "B")))

  ci <- confint(fit, "x0", partial_match = TRUE, data = predgrid)
  shifted <- confint(fit, "x0", partial_match = TRUE, shift = TRUE,
    data = predgrid)
  intercept <- unname(coef(fit)["(Intercept)"])
  expect_identical(as.character(shifted$f), c("A", "B"))
  for (column in c(".estimate", ".lower_ci", ".upper_ci")) {
    expect_equal(shifted[[column]], ci[[column]] + intercept)
  }
  expect_equal(shifted$.se, ci$.se)

  # Full predictions also include the parametric factor effect.
  pred <- predict(fit, newdata = predgrid, se.fit = TRUE)
  factor_effect <- unname(coef(fit)["fB"])
  expect_gt(abs(factor_effect), 0.01)
  expect_equal(as.numeric(pred$fit) - shifted$.estimate, c(0, factor_effect))

  fv <- fitted_values(fit, data = predgrid, scale = "link")
  expect_equal(fv$.fitted, as.numeric(pred$fit))
  expect_equal(fv$.se, as.numeric(pred$se.fit))
  expect_equal(fv$.lower_ci,
    as.numeric(pred$fit - qnorm(0.975) * pred$se.fit))
  expect_equal(fv$.upper_ci,
    as.numeric(pred$fit + qnorm(0.975) * pred$se.fit))
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

test_that("transformed smooth intervals retain ordered endpoints", {
  for (type in c("confidence", "simultaneous")) {
    original <- withr::with_seed(1001, confint(m_gam, parm = "s(x1)",
      type = type, n = 10, nsim = 200, level = 0.8))
    for (direction in c(1, -1)) {
      transform <- function(x) direction * exp(x)
      transformed <- withr::with_seed(1001, confint(m_gam, parm = "s(x1)",
        type = type, n = 10, nsim = 200, level = 0.8, transform = transform))
      expect_equal(transformed$.estimate, transform(original$.estimate))
      expect_equal(transformed$.lower_ci, transform(if (direction == 1) {
        original$.lower_ci
      } else {
        original$.upper_ci
      }))
      expect_equal(transformed$.upper_ci, transform(if (direction == 1) {
        original$.upper_ci
      } else {
        original$.lower_ci
      }))
      expect_identical(transformed$.se, original$.se)
      expect_true(all(transformed$.lower_ci <= transformed$.estimate &
        transformed$.estimate <= transformed$.upper_ci))
    }
  }
})

test_that("confint uses ordered bounds for a decreasing family inverse link", {
  withr::local_seed(1002)
  d <- data.frame(x = seq(0, 1, length.out = 100))
  d$y <- rgamma(nrow(d), shape = 100, scale = exp(d$x) / 100)
  model <- mgcv::gam(y ~ s(x, k = 5), data = d,
    family = Gamma(link = "inverse"), method = "REML")
  original <- confint(model, parm = "s(x)", n = 10, shift = TRUE)
  transformed <- confint(model, parm = "s(x)", n = 10,
    shift = TRUE, transform = TRUE)
  expect_true(all(original$.lower_ci > 0))
  expect_equal(transformed$.estimate, 1 / original$.estimate)
  expect_equal(transformed$.lower_ci, 1 / original$.upper_ci)
  expect_equal(transformed$.upper_ci, 1 / original$.lower_ci)
  expect_identical(transformed$.se, original$.se)
})

test_that("simultaneous intervals select the current smooth in each iteration", {
  n <- 11L
  nsim <- 200L
  seed <- 6006L
  interval_columns <- c(".estimate", ".se", ".crit", ".lower_ci", ".upper_ci")
  for (model in list(m_gam, su_m_factor_by_x2)) {
    labels <- smooths(model)
    selections <- list(
      single = labels[1L],
      multiple = labels[c(length(labels), 1L)],
      all = labels,
      implicit_all = NULL
    )
    for (selection in selections) {
      expected_labels <- if (is.null(selection)) labels else labels[labels %in% selection]
      actual <- withr::with_seed(seed, confint(model, parm = selection,
        type = "simultaneous", n = n, nsim = nsim, ncores = 1))
      expect_identical(unique(actual$.smooth), expected_labels)
      expect_equal(nrow(actual), n * length(expected_labels))
      expect_true(all(is.finite(as.matrix(actual[, interval_columns]))))
      expect_true(all(actual$.lower_ci <= actual$.estimate &
        actual$.estimate <= actual$.upper_ci))
      for (label in expected_labels) {
        rows <- actual[actual$.smooth == label, ]
        expect_equal(nrow(rows), n)
        # Resetting the seed gives the same full coefficient draws, so each
        # jointly requested interval must match its individually requested one.
        single <- withr::with_seed(seed, confint(model, parm = label,
          type = "simultaneous", n = n, nsim = nsim, ncores = 1))
        expect_equal(rows[, interval_columns], single[, interval_columns])
      }
    }
  }
})
