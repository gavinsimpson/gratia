## Test basis() and related functions

## data object to use in tests
add_data <- su_eg1[, c("y", "x0", "x1", "x2", "x3")]

test_that("add_fitted works for a GAM", {
  expect_silent(df <- add_fitted(add_data, m_gam))
  expect_s3_class(df, "tbl_df")
  expect_named(df, c("y", "x0", "x1", "x2", "x3", ".fitted"))
})

test_that("add_fitted works for a GAM with type = 'terms'", {
  expect_silent(df <- add_fitted(add_data, m_gam, type = "terms"))
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(
    "y", "x0", "x1", "x2", "x3", ".constant",
    "s(x0)", "s(x1)", "s(x2)", "s(x3)"
  ))
})

test_that("add_fitted works for a GAM with se.fit = TRUE", {
  expect_silent(df <- add_fitted(add_data, m_gam, se.fit = TRUE))
  expect_s3_class(df, "tbl_df")
  expect_named(df, c("y", "x0", "x1", "x2", "x3", ".fitted"))
})

test_that("add_residuals works for a GAM", {
  expect_silent(df <- add_residuals(add_data, m_gam, type = "pearson"))
  expect_s3_class(df, "tbl_df")
  expect_named(df, c("y", "x0", "x1", "x2", "x3", ".residual"))
})

test_that("add_partial_residuals works for a GAM", {
  expect_silent(df <- add_partial_residuals(add_data, m_gam))
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(
    "y", "x0", "x1", "x2", "x3",
    "s(x0)", "s(x1)", "s(x2)", "s(x3)"
  ))
  expect_silent(df <- add_partial_residuals(add_data, m_gam,
    select = "s(x2)"
  ))
  expect_s3_class(df, "tbl_df")
  expect_named(df, c("y", "x0", "x1", "x2", "x3", "s(x2)"))
})

## test what happens with na.action and NAs in input
miss <- sample(nrow(add_data), 10)
add_data[["x0"]][miss] <- NA
m_na <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = add_data, method = "REML")
m_na_excl <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = add_data, method = "REML",
  na.action = na.exclude
)

test_that("add_residuals works for a GAM with NA in data", {
  expect_error(add_residuals(add_data, m_na, value = "..resid.."),
    "Length of model residuals does not equal number of rows in 'data'",
    fixed = TRUE
  )
  expect_silent(df <- add_residuals(add_data, m_na_excl))
  expect_s3_class(df, "tbl_df")
  expect_named(df, c("y", "x0", "x1", "x2", "x3", ".residual"))
})

test_that("add_partial_residuals works for a GAM with NA in data", {
  skip_on_cran()
  # skip("This needs fixing")
  expect_error(add_partial_residuals(add_data, m_na),
    "Length of model residuals not equal to number of rows in 'data'",
    fixed = TRUE
  )
  expect_silent(df <- add_partial_residuals(add_data, m_na_excl))
  expect_s3_class(df, "tbl_df")
  expect_named(df, c(
    "y", "x0", "x1", "x2", "x3",
    "s(x0)", "s(x1)", "s(x2)", "s(x3)"
  ))
  expect_silent(df <- add_partial_residuals(add_data, m_na_excl,
    select = "s(x2)"
  ))
  expect_s3_class(df, "tbl_df")
  expect_named(df, c("y", "x0", "x1", "x2", "x3", "s(x2)"))
})

test_that("add_confint works for smooth_estimates", {
  expect_silent(sm <- smooth_estimates(m_gam))
  expect_silent(sm <- add_confint(sm, coverage = 0.89))
  expect_s3_class(sm, c("smooth_estimates", "tbl_df", "tbl", "data.frame"))
  expect_named(sm, c(
    ".smooth", ".type", ".by", ".estimate", ".se", "x0",
    "x1", "x2", "x3", ".lower_ci", ".upper_ci"
  ))
  expect_identical(nrow(sm), 400L)
  expect_identical(ncol(sm), 11L)
})

test_that("add_confint works for smooth_estimates", {
  expect_silent(sm <- smooth_estimates(m_gam, unnest = FALSE))
  expect_error(add_confint(sm),
    "Did you use `smooth_estimates(..., unnest = FALSE)`?",
    fixed = TRUE
  )
})

test_that("add_confint.default fails is no est and se", {
  expect_error(
    add_confint(typical_values(m_gam,
      data = su_eg1, envir = teardown_env()
    )),
    "'object' does not contain one or both of '.estimate' or '.se'."
  )
})

test_that("add_constant works for parametric_effects", {
  expect_message(
    pe <- parametric_effects(m_para_sm,
      data = df_2_fac,
      envir = teardown_env()
    ),
    "Interaction terms are not currently supported."
  )
  expect_silent(pe <- add_constant(pe, constant = 10))
  expect_error(pe <- add_constant(pe, constant = "a"),
    "'constant' must be numeric: supplied <a>",
    fixed = TRUE
  )
})

test_that("add_constant works for evaluate_parametric_term", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_silent(pe <- evaluate_parametric_term(m_para_sm, term = "fac"))
  expect_silent(pe <- add_constant(pe, constant = 10))
  expect_error(pe <- add_constant(pe, constant = "a"),
    "'constant' must be numeric: supplied <a>",
    fixed = TRUE
  )
})

test_that("add_constant works for smooth_estimates", {
  expect_silent(sm <- smooth_estimates(m_gam, select = "s(x1)"))
  expect_silent(sm <- add_constant(sm, constant = 10))
  expect_error(sm <- add_constant(sm, constant = "a"),
    "'constant' must be numeric: supplied <a>",
    fixed = TRUE
  )
})

test_that("add_constant works for tbl", {
  expect_silent(tbl <- add_constant(su_eg1, constant = 10, column = "y"))
  expect_error(tbl <- add_constant(tbl, constant = "a", column = "y"),
    "'constant' must be numeric: supplied <a>",
    fixed = TRUE
  )
})

nms <- c(
  ".smooth", ".by", ".fs", ".derivative", ".se",
  ".crit", ".lower_ci", ".upper_ci"
)

test_that("add_sizer derivatives method works", {
  expect_silent(d <- derivatives(m_gam, type = "central"))
  expect_silent(tbl <- add_sizer(d, type = "change"))
  expect_named(tbl, c(nms, ".change", paste0("x", 0:3)))

  expect_silent(tbl <- add_sizer(d, type = "sizer"))
  expect_named(tbl, c(nms, ".decrease", ".increase", paste0("x", 0:3)))
})

test_that("add_sizer smooth_estimates method works", {
  nms <- c(".smooth", ".type", ".by", ".estimate", ".se")
  expect_silent(d <- derivatives(m_gam, type = "central"))
  expect_silent(sm <- smooth_estimates(m_gam))
  expect_silent(tbl <- sm |>
    add_sizer(derivatives = d, type = "change"))
  expect_named(tbl, c(nms, ".change", "x0", "x1", "x2", "x3"))

  expect_silent(tbl <- sm |>
    add_sizer(derivatives = d, type = "sizer"))
  expect_named(tbl, c(nms, ".decrease", ".increase", "x0", "x1", "x2", "x3"))

  expect_error(sm |> add_sizer(type = "change"),
    "An object of class 'derivatives' must be supplied.",
    fixed = TRUE
  )
  expect_error(sm |> add_sizer(type = "sizer"),
    "An object of class 'derivatives' must be supplied.",
    fixed = TRUE
  )
})

## add  methods for posterior samplers
test_that("add_fitted_samples works", {
  expect_silent(fs <- quick_eg1 |>
    add_fitted_samples(m_gam, seed = 2, n = 2))
  expect_identical(nrow(fs), 600L) # 300 data by 2 samples
  expect_named(fs, c(names(quick_eg1), ".row", ".draw", ".parameter", ".fitted"))
  expect_snapshot(print(fs))
})

test_that("add_predicted_samples works", {
  expect_silent(ps <- quick_eg1 |>
    add_predicted_samples(m_gam, seed = 2, n = 2))
  expect_identical(nrow(ps), 600L) # 300 data by 2 samples
  expect_named(ps, c(names(quick_eg1), ".row", ".draw", ".response"))
  expect_snapshot(print(ps))
})

test_that("add_posterior_samples works", {
  expect_silent(ps <- quick_eg1 |>
    add_posterior_samples(m_gam, seed = 2, n = 2))
  expect_identical(nrow(ps), 600L) # 300 data by 2 samples
  expect_named(ps, c(names(quick_eg1), ".row", ".draw", ".response"))
  expect_snapshot(print(ps))
})

test_that("add_smooth_samples works", {
  expect_silent(ss <- quick_eg1 |>
    add_smooth_samples(m_gam, seed = 2, n = 2))
  expect_identical(nrow(ss), 2400L) # 300 data by 4 smooths by 2 samples
  expect_named(ss, c(names(quick_eg1), ".row", ".smooth", ".term", ".draw",
    ".value"))
  expect_snapshot(print(ss))
})

test_that("add_smooth_samples works for selected smooth", {
  expect_silent(ss <- quick_eg1 |>
    add_smooth_samples(m_gam, seed = 2, n = 2, select = "s(x2)"))
  expect_identical(nrow(ss), 600L) # 300 data by 1 smooth by 2 samples
  expect_named(ss, c(names(quick_eg1), ".row", ".smooth", ".term", ".draw",
    ".value"))
  expect_snapshot(print(ss))
})
