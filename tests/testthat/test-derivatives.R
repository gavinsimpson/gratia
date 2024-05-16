## Test derivatives functions

test_that("derivatives fails for an unknown object", {
  df <- data.frame(a = 1:10, b = 1:10)
  expect_error(derivatives(df),
    "Don't know how to calculate derivatives for <data.frame>",
    fixed = TRUE
  )
})

test_that("derivatives() fails with inappropriate args", {
  expect_error(derivatives(su_m_univar_4, type = "foo"),
    paste(
      "'arg' should be one of",
      paste(dQuote(c("forward", "backward", "central")),
        collapse = ", "
      )
    ),
    fixed = TRUE
  )

  expect_error(derivatives(su_m_univar_4, order = 3),
    "Only 1st or 2nd derivatives are supported: `order %in% c(1,2)`",
    fixed = TRUE
  )
})

deriv_nms <- c(
  ".smooth", ".by", ".fs", ".derivative", ".se", ".crit",
  ".lower_ci", ".upper_ci"
)

test_that("derivatives() returns derivatives for all smooths in a GAM", {
  expect_silent(df <- derivatives(su_m_univar_4))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_m_univar_4, type = "forward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_m_univar_4, type = "backward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_m_univar_4, type = "central"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_m_univar_4, "x1", partial_match = TRUE))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x1"))

  expect_silent(df <- derivatives(su_m_univar_4, select = "x1",
    type = "forward",
    partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x1"))

  expect_silent(df <- derivatives(su_m_univar_4, "x1",
    type = "backward",
    partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x1"))

  expect_silent(df <- derivatives(su_m_univar_4, "x1",
    type = "central",
    partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x1"))
})

test_that("derivatives() returns second derivatives for all smooths in a GAM", {
  expect_silent(df <- derivatives(su_m_univar_4, order = 2))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_m_univar_4, order = 2, type = "forward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_m_univar_4, order = 2, type = "backward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_m_univar_4, order = 2, type = "central"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_m_univar_4, "x1",
    order = 2,
    partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x1"))

  expect_silent(df <- derivatives(su_m_univar_4, "x1",
    order = 2,
    type = "forward", partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x1"))

  expect_silent(df <- derivatives(su_m_univar_4, "x1",
    order = 2,
    type = "backward", partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x1"))

  expect_silent(df <- derivatives(su_m_univar_4, "x1",
    order = 2,
    type = "central", partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x1"))
})

test_that("derivatives() returns derivatives for all smooths in a GAMM", {
  expect_silent(df <- derivatives(su_gamm_univar_4))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_gamm_univar_4, type = "forward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_gamm_univar_4, type = "backward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_gamm_univar_4, type = "central"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))
})

test_that("derivatives() returns second derivatives for all smooths in a GAMM", {
  expect_silent(df <- derivatives(su_gamm_univar_4, order = 2))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_gamm_univar_4,
    order = 2,
    type = "forward"
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_gamm_univar_4,
    order = 2,
    type = "backward"
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(df <- derivatives(su_gamm_univar_4,
    order = 2,
    type = "central"
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))
})

## confint methods for by variables

test_that("derivatives() fails with inappropriate args", {
  expect_error(derivatives(su_m_factor_by_x2, type = "foo"),
    paste(
      "'arg' should be one of",
      paste(dQuote(c("forward", "backward", "central")),
        collapse = ", "
      )
    ),
    fixed = TRUE
  )

  expect_error(derivatives(su_m_factor_by_x2, order = 3),
    "Only 1st or 2nd derivatives are supported: `order %in% c(1,2)`",
    fixed = TRUE
  )
})

test_that("derivatives() returns derivatives for all smooths in a factor by GAM", {
  expect_silent(df <- derivatives(su_m_factor_by_x2))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, type = "forward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, type = "backward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, type = "central"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, "x2",
    partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, "x2",
    type = "forward",
    partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, "x2",
    type = "backward",
    partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, "x2",
    type = "central",
    partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))
})

test_that("derivatives() returns derivatives for all smooths in a factor by GAM", {
  expect_silent(df <- derivatives(su_m_factor_by_x2, order = 2))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, order = 2, type = "forward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, order = 2, type = "backward"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, order = 2, type = "central"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2,
    order = 2, "x2",
    partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, "x2",
    order = 2,
    type = "forward", partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, "x2",
    order = 2,
    type = "backward", partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))

  expect_silent(df <- derivatives(su_m_factor_by_x2, "x2",
    order = 2,
    type = "central", partial_match = TRUE
  ))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, "x2", "fac"))
})

test_that("internal finite diff functions fail for all factor vars", {
  df <- data.frame(
    a = factor(rep(letters[1:3], 10)),
    b = factor(rep(LETTERS[1:3], 10))
  )

  expect_error(forward_finite_diff1(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(backward_finite_diff1(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(central_finite_diff1(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(forward_finite_diff2(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(backward_finite_diff2(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(central_finite_diff2(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )
})

## for Issue #64
test_that("internal finite diff functions fail for all non-numeric vars", {
  df <- data.frame(
    a = rep(letters[1:3], 10),
    b = rep(LETTERS[1:3], 10)
  )

  expect_error(forward_finite_diff1(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(backward_finite_diff1(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(central_finite_diff1(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(forward_finite_diff2(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(backward_finite_diff2(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )

  expect_error(central_finite_diff2(su_m_factor_by_x2, df),
    "Can't compute finite differences for all non-numeric data.",
    fixed = TRUE
  )
})

test_that("derivatives() returns derivatives with simultaneous intervals for all smooths", {
  expect_silent(df <- derivatives(su_m_univar_4, interval = "simultaneous"))
  expect_s3_class(df, "derivatives")
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(deriv_nms, paste0("x", 0:3)))
})

test_that("derivatives() works for factor by smooths issue 47", {
  skip_on_cran()

  expect_silent(d <- derivatives(su_m_factor_by_x2))
  expect_s3_class(d, "derivatives")
  expect_s3_class(d, "tbl_df")
  expect_named(d, c(deriv_nms, "x2", "fac"))
  plt1 <- draw(d)

  m_47 <- gam(y ~ x1 + s(x2) + fac + s(x0, by = fac),
    data = su_eg4,
    method = "REML"
  )
  expect_silent(d <- derivatives(m_47))
  expect_s3_class(d, "derivatives")
  expect_s3_class(d, "tbl_df")
  expect_named(d, c(deriv_nms, "x2", "x0", "fac"))
  plt2 <- draw(d)

  dat_47 <- transform(su_eg4, ofac = ordered(fac))
  m_47 <- gam(y ~ x1 + s(x2) + ofac + s(x0) + s(x0, by = ofac),
    data = dat_47,
    method = "REML"
  )
  expect_silent(d <- derivatives(m_47))
  expect_s3_class(d, "derivatives")
  expect_s3_class(d, "tbl_df")
  expect_named(d, c(deriv_nms, "x2", "x0", "ofac"))
  plt3 <- draw(d)

  m_47 <- gamm(y ~ x1 + s(x2) + fac + s(x0, by = fac), data = su_eg4)
  expect_silent(d <- derivatives(m_47))
  expect_s3_class(d, "derivatives")
  expect_s3_class(d, "tbl_df")
  expect_named(d, c(deriv_nms, "x2", "x0", "fac"))
  plt4 <- draw(d)

  skip_on_ci() # testing without as moved to mac os x
  skip_on_covr()
  expect_doppelganger("draw issue 47 derivatives for factor by", plt1)
  expect_doppelganger("draw issue 47 derivatives for complex factor by", plt2)
  expect_doppelganger("draw issue 47 derivs for ordered factor by", plt3)
  expect_doppelganger("draw issue 47 derivatives for gamm factor by", plt4)
})

test_that("derivatives() works for fs smooths issue 57", {
  skip_on_cran()
  logistic.growth <- function(t, y0, k, r) {
    return(k * (y0 / (y0 + (k - y0) * exp(-r * t))))
  }
  logistic_growth_sim <- function(seed = 1) {
    d <- withr::with_seed(seed, {
      N <- 16
      n <- 12
      y0 <- 0.5
      r <- 0.25
      k <- rnorm(N, mean = 5, sd = 1)
      d <- data.frame(
        unit = factor(rep(seq_len(N), each = n)),
        t = rep(seq(0, 20, length = n), N)
      )
      d <- transform(d, y = logistic.growth(t, y0, k[unit], r))
      S <- 0.25
      transform(d, y_obs = y + rnorm(nrow(d), sd = S))
    })
    d
  }
  logistic_growth_df <- logistic_growth_sim()
  m_logistic_growth <- gam(y_obs ~ s(t, unit, k = 5, bs = "fs", m = 2),
    data = logistic_growth_df, method = "REML")

  expect_silent(fd <- derivatives(m_logistic_growth))
  expect_s3_class(fd, "derivatives")
  expect_s3_class(fd, "tbl_df")
  expect_named(fd, c(deriv_nms, "t", "unit"))
  plt <- draw(fd) # FIXME: need to update draw(d) so it works with fs smooths
  
  skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw issue 57 derivatives for factor by", plt)
})

## tests for by variables & simultaneous intervals #102
test_that("derivatives with simultaneous intervals works for factor by", {
  skip_on_cran()
  N <- 50
  newd <- with(su_eg4, expand.grid(
    fac = levels(fac),
    x2 = seq(min(x2), max(x2), length = N),
    x0 = mean(x0)
  ))
  expect_message(
    d_pw <- derivatives(su_m_factor_by,
      newdata = newd,
      select = smooths(su_m_factor_by)[1:3]
    ),
    "Use of the `newdata` argument is deprecated.
Instead, use the data argument `data`.\n"
  )
  expect_silent(d_pw <- derivatives(su_m_factor_by,
    data = newd,
    select = smooths(su_m_factor_by)[1:3]
  ))
  expect_s3_class(d_pw, "derivatives")
  expect_s3_class(d_pw, "tbl_df")
  expect_identical(nrow(d_pw), as.integer(N * 3L))

  expect_silent(d_sim <- withr::with_seed(
    15,
    derivatives(su_m_factor_by,
      data = newd,
      select = smooths(su_m_factor_by)[1:3],
      interval = "simultaneous"
    )
  ))
  expect_s3_class(d_pw, "derivatives")
  expect_s3_class(d_pw, "tbl_df")
  expect_identical(nrow(d_pw), as.integer(N * 3L))
})

## tests for models with random effects
test_that("derivatives works with models that include random effects", {
  expect_silent(d <- derivatives(rm1))
  expect_s3_class(d, "derivatives")
  expect_s3_class(d, "tbl_df")
  expect_named(d, c(deriv_nms, paste0("x", 0:3)))

  expect_silent(d <- derivatives(rm2))
  expect_s3_class(d, "derivatives")
  expect_s3_class(d, "tbl_df")
  expect_named(d, c(deriv_nms, paste0("x", 0:2)))
})
