# Test posterior sampling functions

test_that("smooth_samples works for a continuous by GAM", {
  expect_silent(sm <- smooth_samples(su_m_cont_by,
    n = 5, n_vals = 100,
    seed = 42
  ))
  expect_s3_class(sm, c(
    "smooth_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 500 == 1 smooth * 5 * 100
  expect_identical(NROW(sm), 500L)
  # 9 cols, 8 for univariate smooths + 1 for cont by var
  expect_identical(NCOL(sm), 9L)
  skip_on_cran()
  skip_on_ci()
  expect_snapshot(sm)
})

test_that("smooth_samples works for a simple GAM", {
  expect_silent(sm <- smooth_samples(m_1_smooth,
    n = 5, n_vals = 100,
    seed = 42
  ))
  expect_s3_class(sm, c(
    "smooth_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 500 == 1 smooth * 5 * 100
  expect_identical(NROW(sm), 500L)
  expect_identical(NCOL(sm), 8L) # 8 cols, univatiate smooths
  skip_on_cran()
  skip_on_ci()
  expect_snapshot(sm)
})

test_that("smooth_samples works for a simple GAM multi rng calls", {
  expect_silent(sm <- smooth_samples(m_1_smooth,
    n = 5, n_vals = 100,
    seed = 42, rng_per_smooth = TRUE
  ))
  expect_s3_class(sm, c(
    "smooth_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 500 == 1 smooth * 5 * 100
  expect_identical(NROW(sm), 500L)
  expect_identical(NCOL(sm), 8L) # 8 cols, univatiate smooths
  skip_on_cran()
  skip_on_ci()
  expect_snapshot(sm)
})

test_that("smooth_samples works for a simple GAM MH sampling", {
  expect_silent(sm <- smooth_samples(m_1_smooth,
    n = 5, n_vals = 100,
    method = "mh", seed = 42
  ))
  expect_s3_class(sm, c(
    "smooth_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 500 == 1 smooth * 5 * 100
  expect_identical(NROW(sm), 500L)
  expect_identical(NCOL(sm), 8L) # 8 cols, univatiate smooths
})

test_that("smooth_samples works for a multi-smooth GAM", {
  expect_silent(sm <- smooth_samples(m_gam, n = 5, n_vals = 100, seed = 42))
  expect_s3_class(sm, c(
    "smooth_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 2000 == 4 smooths * 5 * 100
  expect_identical(NROW(sm), 2000L)
  expect_identical(NCOL(sm), 11L) # 11 cols, 4 univatiate smooths
})

test_that("smooth_samples works for a multi-smooth factor by GAM", {
  expect_silent(sm <- smooth_samples(su_m_factor_by,
    n = 5, n_vals = 50,
    seed = 42
  ))
  expect_s3_class(sm, c(
    "smooth_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 2000 == 1 + (1 * 3) smooths * 5 * 50
  expect_identical(NROW(sm), 1000L)
  expect_identical(NCOL(sm), 10L) # 10 cols, univatiate smooths with factor
})

test_that("smooth_samples works for a multi-smooth SCAM", {
  expect_silent(sm <- smooth_samples(m_scam, n = 5, n_vals = 50, seed = 42))
  expect_s3_class(sm, c("smooth_samples", "tbl_df", "tbl", "data.frame"))
  ## 500 == 2 smooths * 5 * 50
  expect_identical(NROW(sm), 500L)
  expect_identical(NCOL(sm), 9L)
})

test_that("smooth_samples() fails if not suitable method available", {
  expect_error(smooth_samples(1:10),
    "Don't know how to sample from the posterior of <integer>",
    fixed = TRUE
  )
})

test_that("smooth_samples sets seed when seed not provided", {
  expect_silent(smooth_samples(m_gam, seed = NULL))
})

test_that("smooth_samples works with term provided", {
  expect_silent(sm <- smooth_samples(m_gam, select = "s(x2)", seed = 42))
})

test_that("smooth_samples errors with invalid term provided", {
  expect_error(sm <- smooth_samples(m_gam, select = "s(x10)", seed = 42),
    "Failed to match any smooths in model m_gam.
Try with 'partial_match = TRUE'?",
    fixed = TRUE
  )
})

test_that("term argument to smooth_samples is deprecated", {
  expect_warning(sm <- smooth_samples(m_gam, term = "s(x1)", seed = 42),
    "The `term` argument of `smooth_samples()` is deprecated as of gratia 0.8.9.9.
i Please use the `select` argument instead.",
    fixed = TRUE
  )

})

# from #121
test_that("smooth_samples gets the right factor by smooth: #121", {
  expect_silent(sm <- smooth_samples(su_m_factor_by,
    n = 5, n_vals = 100,
    select = "s(x2):fac2", seed = 42
  ))
  # factor level of `fac` column should be 2
  expect_identical(all(sm["fac"] == 2), TRUE)
})

# from #121 - problems when model contains ranef smooths
test_that("smooth_samples ignores ranef smooths: #121", {
  expect_message(
    sm <- smooth_samples(rm1, n = 5, n_vals = 100, seed = 42),
    "Random effect smooths not currently supported."
  )
  # given n and n_vals and 4 smooths, nrow == 2000L
  expect_identical(nrow(sm), 2000L)
  # shouldn't have "s(fac)" in sm
  expect_identical(any(sm$.smooth == "s(fac)"), FALSE)
})

test_that("smooth_samples fails if no smooths left to sample from", {
  expect_error(
    sm <- smooth_samples(rm1,
      select = "s(fac)",
      n = 5, n_vals = 100, seed = 42
    ),
    "No smooths left that can be sampled from."
  )
})

test_that("fitted_samples works for a simple GAM", {
  expect_silent(sm <- fitted_samples(m_1_smooth, n = 5, seed = 42))
  expect_s3_class(sm, c(
    "fitted_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 1000 == 5 * 200 (nrow(dat))
  expect_identical(NROW(sm), 1500L)
  expect_identical(NCOL(sm), 3L) # 3 cols
  expect_named(sm, expected = c(".row", ".draw", ".fitted"))
})

test_that("fitted_samples works for a multi-smooth GAM", {
  expect_silent(sm <- fitted_samples(m_gam, n = 5, seed = 42))
  expect_s3_class(sm, c(
    "fitted_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 5000 == 5 draws * 1000 observations in data
  expect_identical(NROW(sm), 5000L)
  expect_identical(NCOL(sm), 3L) # 3 cols
  expect_named(sm, expected = c(".row", ".draw", ".fitted"))
})

test_that("fitted_samples works for a multi-smooth factor by GAM", {
  expect_silent(sm <- fitted_samples(su_m_factor_by, n = 5, seed = 42))
  expect_s3_class(sm, c(
    "fitted_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 2000 == 5 draws * 400 observations in data
  expect_identical(NROW(sm), 2000L)
  expect_identical(NCOL(sm), 3L) # 3 cols
  expect_named(sm, expected = c(".row", ".draw", ".fitted"))
})

test_that("fitted_samples sets seed when seed not provided", {
  expect_silent(fitted_samples(m_gam, seed = NULL))
})

test_that("fitted_samples() fails if not suitable method available", {
  expect_error(fitted_samples(1:10),
    "Don't know how to sample from the posterior of <integer>",
    fixed = TRUE
  )
})

test_that("predicted_samples works for a simple GAM", {
  expect_silent(sm <- predicted_samples(m_1_smooth, n = 5, seed = 42))
  expect_s3_class(sm, c(
    "predicted_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 2000 == 5 * 100 (nrow(dat))
  expect_identical(NROW(sm), 1500L)
  expect_identical(NCOL(sm), 3L) # 3 cols
  expect_named(sm, expected = c(".row", ".draw", ".response"))
})

test_that("predicted_samples works for a multi-smooth GAM", {
  expect_silent(sm <- predicted_samples(m_gam, n = 5, seed = 42))
  expect_s3_class(sm, c(
    "predicted_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 5000 == 5 draws * 1000 observations in data
  expect_identical(NROW(sm), 5000L)
  expect_identical(NCOL(sm), 3L) # 3 cols
  expect_named(sm, expected = c(".row", ".draw", ".response"))
})

test_that("predicted_samples works for a multi-smooth factor by GAM", {
  expect_silent(sm <- predicted_samples(su_m_factor_by, n = 5, seed = 42))
  expect_s3_class(sm, c(
    "predicted_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 2000 == 5 draws * 400 observations in data
  expect_identical(NROW(sm), 2000L)
  expect_identical(NCOL(sm), 3L) # 3 cols
  expect_named(sm, expected = c(".row", ".draw", ".response"))
})

test_that("predicted_samples sets seed when seed not provided", {
  expect_silent(predicted_samples(m_gam, seed = NULL))
})

test_that("predicted_samples() fails if not suitable method available", {
  expect_error(predicted_samples(1:10),
    "Don't know how to sample from the posterior of <integer>",
    fixed = TRUE
  )
})

test_that("posterior_samples() fails if no suitable method available", {
  expect_error(posterior_samples(1:10),
    "Don't know how to sample from the posterior of <integer>",
    fixed = TRUE
  )
})

test_that("fitted_samples example output doesn't change", {
  skip_on_cran()
  # skip_on_os("mac")
  skip_on_ci()

  fs <- fitted_samples(m_gam, n = 5, seed = 42)
  expect_snapshot(fs)
})

test_that("smooth_samples example output doesn't change", {
  skip_on_cran()
  # skip_on_os("mac")
  skip_on_ci()

  samples <- smooth_samples(m_gam, select = "s(x0)", n = 5, seed = 42)
  expect_snapshot(samples)
})

test_that("posterior_samples works for a simple GAM", {
  expect_silent(sm <- posterior_samples(m_1_smooth, n = 5, seed = 42))
  expect_s3_class(sm, c(
    "posterior_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 1000 == 5 * 200 (nrow(dat))
  expect_identical(NROW(sm), 1500L)
  expect_identical(NCOL(sm), 3L) # 3 cols
  expect_named(sm, expected = c(".row", ".draw", ".response"))
})

test_that("posterior_samples works for a multi-smooth tweedie GAM", {
  expect_silent(sm <- posterior_samples(m_tw, n = 5, seed = 42))
  expect_s3_class(sm, c(
    "posterior_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 2500 == 5 draws * 5000 observations in data
  expect_identical(NROW(sm), 2500L)
  expect_identical(NCOL(sm), 3L) # 3 cols
  expect_named(sm, expected = c(".row", ".draw", ".response"))
})

# test for offset handling
test_that("posterior sampling funs work with offsets in formula issue 233", {
  skip_on_cran()
  skip_on_ci()

  n <- 100
  df <- withr::with_seed(123, {
    data.frame(
      y = rnbinom(n = n, size = 0.9, prob = 0.3),
      x = rnorm(n = n, mean = 123, sd = 66),
      denom = round(rnorm(n = n, mean = 1000, sd = 1))
    )
  })

  mod <- gam(y ~ 1 + offset(log(denom)),
    data = df, family = "nb"
  )

  n_samples <- 5
  expect_silent(ps <- posterior_samples(mod, n = n_samples, seed = 42))
  expect_identical(nrow(ps), as.integer(n * n_samples))

  expect_silent(fs <- fitted_samples(mod, n = n_samples, seed = 42))
  expect_identical(nrow(fs), as.integer(n * n_samples))

  expect_snapshot(print(ps), variant = "posterior", cran = FALSE)
  expect_snapshot(print(fs), variant = "fitted", cran = FALSE)
})

test_that("derivative_samples works for a simple GAM", {
  expect_silent(sm <- derivative_samples(m_1_smooth,
    n = 5, seed = 42,
    type = "central", focal = "x0", eps = 0.01, n_sim = 10,
    data = quick_eg1,
    envir = teardown_env()
  ))
  expect_s3_class(sm, c(
    "derivative_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 3000 == nrow(quick_eg) * n_sim == 300 * 10
  expect_identical(NROW(sm), 3000L)
  expect_identical(NCOL(sm), 5L) # 5 cols
  expect_named(sm, expected = c(
    ".row", ".focal", ".draw", ".derivative",
    "x0"
  ))

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(sm), variant = "m_1_smooth")
})

test_that("derivative_samples works for a NB GAM", {
  expect_silent(sm <- derivative_samples(m_nb,
    n = 5, seed = 42,
    type = "forward", focal = "x0", eps = 0.01, n_sim = 10,
    data = quick_eg1,
    envir = teardown_env()
  ))
  expect_s3_class(sm, c(
    "derivative_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 3000 == nrow(quick_eg) * n_sim == 300 * 10
  expect_identical(NROW(sm), 3000L)
  expect_identical(NCOL(sm), 8L) # 5 cols
  expect_named(sm, expected = c(
    ".row", ".focal", ".draw", ".derivative",
    "x0", "x1", "x2", "x3"
  ))

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(sm), variant = "m_nb-forward")
})

test_that("derivative_samples works for a NB GAM, central, backward", {
  skip_on_cran()

  expect_silent(sm_1 <- derivative_samples(m_nb,
    n = 5, seed = 42,
    type = "backward", focal = "x0", eps = 0.01, n_sim = 10,
    data = quick_eg1,
    envir = teardown_env()
  ))
  expect_s3_class(sm_1, c(
    "derivative_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 3000 == nrow(quick_eg) * n_sim == 300 * 10
  expect_identical(NROW(sm_1), 3000L)
  expect_identical(NCOL(sm_1), 8L) # 8 cols
  expect_named(sm_1, expected = c(
    ".row", ".focal", ".draw", ".derivative",
    "x0", "x1", "x2", "x3"
  ))

  expect_silent(sm_2 <- derivative_samples(m_nb,
    n = 5, seed = 42,
    type = "central", focal = "x0", eps = 0.01, n_sim = 10,
    data = quick_eg1,
    envir = teardown_env()
  ))
  expect_s3_class(sm_2, c(
    "derivative_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 3000 == nrow(quick_eg) * n_sim == 300 * 10
  expect_identical(NROW(sm_2), 3000L)
  expect_identical(NCOL(sm_2), 8L) # 8 cols
  expect_named(sm_2, expected = c(
    ".row", ".focal", ".draw", ".derivative",
    "x0", "x1", "x2", "x3"
  ))

  skip_on_ci()
  expect_snapshot(print(sm_1), variant = "m_nb-backward")
  expect_snapshot(print(sm_2), variant = "m_nb-central")
})

test_that("derivative_samples works for a NB GAM order 2", {
  skip_on_cran()

  expect_silent(sm_1 <- derivative_samples(m_nb,
    n = 5, seed = 42,
    type = "forward", focal = "x0", eps = 0.01, n_sim = 10,
    data = quick_eg1, order = 2,
    envir = teardown_env()
  ))
  expect_s3_class(sm_1, c(
    "derivative_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 3000 == nrow(quick_eg) * n_sim == 300 * 10
  expect_identical(NROW(sm_1), 3000L)
  expect_identical(NCOL(sm_1), 8L) # 8 cols
  expect_named(sm_1, expected = c(
    ".row", ".focal", ".draw", ".derivative",
    "x0", "x1", "x2", "x3"
  ))

  expect_silent(sm_2 <- derivative_samples(m_nb,
    n = 5, seed = 42,
    type = "backward", focal = "x0", eps = 0.01, n_sim = 10,
    data = quick_eg1, order = 2,
    envir = teardown_env()
  ))
  expect_s3_class(sm_2, c(
    "derivative_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 3000 == nrow(quick_eg) * n_sim == 300 * 10
  expect_identical(NROW(sm_2), 3000L)
  expect_identical(NCOL(sm_2), 8L) # 8 cols
  expect_named(sm_2, expected = c(
    ".row", ".focal", ".draw", ".derivative",
    "x0", "x1", "x2", "x3"
  ))

  expect_silent(sm_3 <- derivative_samples(m_nb,
    n = 5, seed = 42,
    type = "central", focal = "x0", eps = 0.01, n_sim = 10,
    data = quick_eg1, order = 2,
    envir = teardown_env()
  ))
  expect_s3_class(sm_3, c(
    "derivative_samples", "tbl_df",
    "tbl", "data.frame"
  ))
  ## 3000 == nrow(quick_eg) * n_sim == 300 * 10
  expect_identical(NROW(sm_3), 3000L)
  expect_identical(NCOL(sm_3), 8L) # 8 cols
  expect_named(sm_3, expected = c(
    ".row", ".focal", ".draw", ".derivative",
    "x0", "x1", "x2", "x3"
  ))

  skip_on_ci()
  expect_snapshot(print(sm_1), variant = "m_nb-forward-order-2")
  expect_snapshot(print(sm_2), variant = "m_nb-backward-order-2")
  expect_snapshot(print(sm_3), variant = "m_nb-central-order-2")
})

test_that("fitted_samples can use mvn_method", {
  skip_on_cran()
  expect_silent(fs1 <- fitted_samples(m_tiny_eg1, n = 10, seed = 2,
    mvn_method = "mgcv"))
  expect_silent(fs2 <- fitted_samples(m_tiny_eg1, n = 10, seed = 2,
    mvn_method = "mvnfast"))
  expect_false(identical(fs1, fs2))
})

test_that("posterior samples can use mvn_method", {
  skip_on_cran()
  expect_silent(ps1 <- posterior_samples(m_tiny_eg1, n = 10, seed = 2,
    mvn_method = "mgcv"))
  expect_silent(ps2 <- posterior_samples(m_tiny_eg1, n = 10, seed = 2,
    mvn_method = "mvnfast"))
  expect_false(identical(ps1, ps2))
})

test_that("smooth samples can use mvn_method", {
  skip_on_cran()
  expect_silent(sm1 <- smooth_samples(m_tiny_eg1, n = 10, seed = 2,
    mvn_method = "mgcv"))
  expect_silent(sm2 <- smooth_samples(m_tiny_eg1, n = 10, seed = 2,
    mvn_method = "mvnfast"))
  expect_false(identical(sm1, sm2))
})

