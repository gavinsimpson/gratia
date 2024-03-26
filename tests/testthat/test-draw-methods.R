## Test draw() methods

test_that("draw.gam works with numeric select", {
  plt1 <- draw(su_m_quick_eg1, select = 2, rug = FALSE)
  plt2 <- draw(su_m_quick_eg1, select = c(1, 2), rug = FALSE)
  skip_on_ci()
  expect_doppelganger("draw gam smooth for selected smooth numeric", plt1)
  expect_doppelganger("draw gam smooth for two selected smooths numeric", plt2)
})

test_that("draw.gam fails with bad select", {
  expect_error(draw(su_m_univar_4, select = 8),
    "One or more indices in 'select' > than the number of smooths in the model.",
    fixed = TRUE
  )
  expect_error(draw(su_m_univar_4, select = c(1, 3, 5, 6)),
    "One or more indices in 'select' > than the number of smooths in the model.",
    fixed = TRUE
  )
  expect_error(draw(su_m_univar_4, select = c(1, 2, 3, 4, 5)),
    "Trying to select more smooths than are in the model.",
    fixed = TRUE
  )
  expect_error(draw(su_m_univar_4, select = TRUE),
    "When 'select' is a logical vector, 'length(select)' must equal
the number of smooths in the model.",
    fixed = TRUE
  )
})

test_that("draw.gam works with character select", {
  plt1 <- draw(su_m_quick_eg1, select = "s(x1)", rug = FALSE)
  plt2 <- draw(su_m_quick_eg1, select = c("s(x0)", "s(x1)"), rug = FALSE)

  skip_on_ci()
  expect_doppelganger("draw gam smooth for selected smooth character", plt1)
  expect_doppelganger(
    "draw gam smooth for two selected smooths character",
    plt2
  )
})

test_that("draw.gam works with logical select single smooth", {
  plt <- draw(su_m_quick_eg1,
    select = c(TRUE, rep(FALSE, 3)),
    rug = FALSE
  )
  skip_on_ci()
  expect_doppelganger("draw gam smooth for selected smooth logical", plt)
})

test_that("draw.gam works with logical select two smooths", {
  plt <- draw(su_m_quick_eg1,
    select = rep(c(TRUE, FALSE), each = 2),
    rug = FALSE
  )
  skip_on_ci()
  expect_doppelganger("draw gam smooth for two selected smooths logical", plt)
})

test_that("draw.gam works with partial_match", {
  plt <- draw(su_m_factor_by,
    select = "x2", partial_match = TRUE,
    rug = FALSE, n = 50
  )
  expect_error(draw(su_m_factor_by, select = "s(x2)", partial_match = FALSE),
    "Failed to match any smooths in model `su_m_factor_by`.\nTry with 'partial_match = TRUE'?",
    fixed = TRUE
  )

  skip_on_ci()
  expect_doppelganger("draw gam with partial match TRUE", plt)
})

test_that("draw.gam works with select and parametric", {
  plt1 <- draw(su_m_factor_by,
    select = "s(x2)", partial_match = TRUE,
    rug = FALSE
  )
  plt2 <- draw(su_m_factor_by,
    select = "s(x2)", partial_match = TRUE,
    parametric = FALSE, data = su_eg4, envir = teardown_env(),
    rug = FALSE
  )
  plt3 <- draw(su_m_factor_by,
    select = "s(x2)", partial_match = TRUE,
    parametric = TRUE, data = su_eg4, envir = teardown_env(),
    rug = FALSE
  )
  plt4 <- draw(su_m_factor_by,
    parametric = TRUE, rug = FALSE,
    data = su_eg4, envir = teardown_env()
  )
  plt5 <- draw(su_m_factor_by,
    parametric = FALSE, rug = FALSE,
    data = su_eg4, envir = teardown_env()
  )

  skip_on_ci()
  expect_doppelganger("draw gam with select and parametric is NULL", plt1)
  expect_doppelganger("draw gam with select and parametric is FALSE", plt2)
  expect_doppelganger("draw gam with select and parametric is TRUE", plt3)
  expect_doppelganger("draw gam without select and parametric is TRUE", plt4)
  expect_doppelganger("draw gam without select and parametric is FALSE", plt5)
})

test_that("draw.gam() plots a simple multi-smooth AM", {
  plt1 <- draw(su_m_quick_eg1, rug = FALSE)
  plt2 <- draw(su_m_quick_eg1, scales = "fixed", rug = FALSE)

  skip_on_ci()
  expect_doppelganger("draw simple multi-smooth AM", plt1)
  expect_doppelganger("draw simple multi-smooth AM with fixed scales", plt2)
})

test_that("draw.gam() can draw partial residuals", {
  plt1 <- draw(m_tiny_eg1, residuals = TRUE, rug = FALSE)
  plt2 <- draw(m_tiny_eg1, residuals = TRUE, scales = "fixed", rug = FALSE)

  skip_on_ci()
  expect_doppelganger("draw simple partial residuals", plt1)
  expect_doppelganger("draw simple partial residuals with fixed scales", plt2)
})

test_that("draw.gam() plots an AM with a single 2d smooth", {
  skip_on_os("mac")
  skip_on_os("windows")
  plt <- draw(su_m_bivar, n = 50, rug = FALSE)

  skip_on_ci()
  expect_doppelganger("draw AM with 2d smooth", plt)
})

test_that("draw.gam() plots an AM with a single factor by-variable smooth", {
  plt1 <- draw(su_m_factor_by, rug = FALSE)
  plt2 <- draw(su_m_factor_by, scales = "fixed", rug = FALSE)

  skip_on_ci()
  expect_doppelganger("draw AM with factor by-variable smooth", plt1)
  expect_doppelganger("draw factor by-variable smooth with fixed scales", plt2)
})

test_that("draw() works with continuous by", {
  plt <- draw(su_m_cont_by, rug = FALSE, n = 50)
  skip_on_ci()
  expect_doppelganger("draw with continuous by-variable smooth", plt)
})

test_that("draw() works with continuous by and fixed scales", {
  plt <- draw(su_m_cont_by, scales = "fixed", rug = FALSE, n = 50)
  skip_on_ci()
  expect_doppelganger("draw with continuous by-var fixed scale", plt)
})

test_that("draw() works with random effect smooths (bs = 're')", {
  p2 <- draw(rm1, ncol = 3, rug = FALSE)
  p3 <- draw(rm1, ncol = 3, scales = "fixed", rug = FALSE)

  skip_on_ci()
  expect_doppelganger("draw.gam model with ranef smooth", p2)
  expect_doppelganger("draw model with ranef smooth fixed scales", p3)
})

test_that("draw() with random effect smooths (bs = 're') & factor by variable ", {
  p2 <- draw(rm2, ncol = 3, rug = FALSE)
  p3 <- draw(rm2, ncol = 3, scales = "fixed", rug = FALSE)

  skip_on_ci()
  expect_doppelganger("draw.gam model with ranef smooth factor by", p2)
  expect_doppelganger("draw with ranef smooth factor by fixed scales", p3)
})

test_that("draw() can handle non-standard names -- a function call as a name", {
  df <- data.frame(
    y = c(
      0.15, 0.17, 0.07, 0.17, 0.01, 0.15, 0.18, 0.04, -0.06, -0.08,
      0, 0.03, -0.27, -0.93, 0.04, 0.12, 0.08, 0.15, 0.04, 0.15,
      0.03, 0.09, 0.11, 0.13, -0.11, -0.32, -0.7, -0.78, 0.07, 0.04,
      0.06, 0.12, -0.15, 0.05, -0.08, 0.14, -0.02, -0.14, -0.24,
      -0.32, -0.78, -0.81, -0.04, -0.25, -0.09, 0.02, -0.13, -0.2,
      -0.04, 0, 0.02, -0.05, -0.19, -0.37, -0.57, -0.81
    ),
    time = rep(2^c(
      -1, 0, 1, 1.58, 2, 2.58, 3, 3.32, 3.58, 4.17,
      4.58, 5.58, 6.17, 7.39
    ), 4)
  )
  ## the smooth is of `log2(time)` but this needs special handling
  ## in the `ggplot()` to avoid `ggplot()` looking incorrectly for `time` and
  ## not the correct `log2(time)`
  fit <- gam(y ~ s(log2(time)), data = df, method = "REML")
  p1 <- draw(fit)
  skip_on_ci()
  expect_doppelganger("draw.gam model with non-standard names", p1)
})

test_that("draw() works with factor-smooth interactions (bs = 'fs')", {
  # skip_on_os("mac") # try without this and check on Simon's mac system
  skip_on_ci()
  skip_if(packageVersion("mgcv") < "1.8.36")
  p2 <- draw(mod_fs, ncol = 2, rug = FALSE)
  p3 <- draw(mod_fs, ncol = 2, scales = "fixed", rug = FALSE)

  skip_on_ci()
  expect_doppelganger("draw.gam model with fs smooth", p2)
  expect_doppelganger("draw model with fs smooth fixed scales", p3)
})

test_that("draw() works with parametric terms", {
  ## fake some data...
  df <- withr::with_seed(0, {
    f1 <- function(x) {
      exp(2 * x)
    }
    f2 <- function(x) {
      0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10
    }
    f3 <- function(x) {
      x * 0
    }

    n <- 200
    sig2 <- 4
    x0 <- rep(1:4, 50)
    x1 <- runif(n, 0, 1)
    x2 <- runif(n, 0, 1)
    x3 <- runif(n, 0, 1)
    e <- rnorm(n, 0, sqrt(sig2))
    y <- 2 * x0 + f1(x1) + f2(x2) + f3(x3) + e
    data.frame(x0 = x0, x1 = x1, x2 = x2, x3 = x3, y = y)
  })

  ## fit
  mod <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), data = df)


  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")

  ## evaluate parametric terms directly
  e1 <- evaluate_parametric_term(mod, term = "x0")
  expect_s3_class(e1, "evaluated_parametric_term")
  expect_equal(ncol(e1), 5L)
  expect_named(e1, c("term", "type", "value", "partial", "se"))
  p1 <- draw(e1, rug = FALSE)

  ## check evaluate_parametric_term works
  p2 <- draw(mod, rug = FALSE)

  ## factor parametric terms
  x0 <- factor(x0)
  df <- data.frame(x0 = x0, x1 = x1, x2 = x2, x3 = x3, y = y)
  ## fit
  mod <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), data = df)

  ## check evaluate_parametric_term works
  p3 <- draw(mod, rug = FALSE)

  ## evaluate parametric terms directly
  e2 <- evaluate_parametric_term(mod, term = "x0")
  expect_s3_class(e2, "evaluated_parametric_term")

  expect_error(evaluate_parametric_term(mod, term = "x1"),
    "Term is not in the parametric part of model: <x1>",
    fixed = TRUE
  )

  expect_warning(evaluate_parametric_term(mod, term = c("x0", "x1")),
    "More than one `term` requested; using the first <x0>",
    fixed = TRUE
  )

  skip_on_ci()
  expect_doppelganger("draw with linear parametric term", p1)
  expect_doppelganger("draw.gam with linear parametric term", p2)
  expect_doppelganger("draw.gam with factor parametric term", p3)
})

test_that("component-wise CIs work without seWithMean", {
  plt <- draw(su_m_univar_4, overall_uncertainty = FALSE, rug = FALSE)

  skip_on_ci()
  expect_doppelganger("draw gam with overall_uncertainty false", plt)
})

test_that("draw.derivates() plots derivatives for a GAM", {
  skip_on_ci()

  d1 <- derivatives(su_m_univar_4, type = "central", n = 200)
  plt1 <- draw(d1)
  plt2 <- draw(d1, scales = "fixed")

  skip_on_ci()
  expect_doppelganger("draw derivatives for a GAM", plt1)
  expect_doppelganger("draw derivatives for a GAM with fixed scales", plt2)
})

test_that("draw.derivates plots derivatives with change indicators", {
  # not on CRAN
  skip_on_cran()
  skip_on_ci() # causing trivial failures on GH

  d1 <- derivatives(m_gam, type = "central", n = 200)
  expect_silent(plt1 <- draw(d1, add_change = TRUE))
  expect_silent(plt2 <- draw(d1, add_change = TRUE, change_type = "sizer"))

  skip_on_ci()
  expect_doppelganger("draw derivatives for a GAM with default change", plt1)
  expect_doppelganger("draw derivatives for a GAM with sizer change", plt2)
})

test_that("draw.derivates() plots derivatives for a GAM rotated labels", {
  skip_on_cran()
  d1 <- derivatives(su_m_univar_4, type = "central", n = 100)
  plt1 <- draw(d1, angle = 45)
  plt2 <- draw(d1, scales = "fixed", angle = 45)

  skip_on_ci()
  expect_doppelganger("draw derivatives for a GAM rotated labels", plt1)
  expect_doppelganger(
    "draw derivatives for a GAM with fixed scales rotated",
    plt2
  )
})

test_that("draw plots partial derivatives for a GAM", {
  d1 <- partial_derivatives(su_m_bivar_te,
    select = "te(x,z)", focal = "z",
    type = "central", n = 100
  )
  plt1 <- draw(d1)

  plt2 <- draw(d1, scales = "fixed")

  skip_on_ci()
  expect_doppelganger("draw partial derivatives for a GAM", plt1)
  expect_doppelganger(
    "draw partial derivatives for a GAM with fixed scales",
    plt2
  )
})

test_that("draw plots partial derivs for GAM rotated labels", {
  skip_on_cran()
  d1 <- partial_derivatives(su_m_bivar_te,
    select = "te(x,z)", focal = "z",
    type = "central", n = 100
  )
  plt1 <- draw(d1, angle = 45)
  plt2 <- draw(d1, scales = "fixed", angle = 45)

  skip_on_ci()
  expect_doppelganger(
    "draw partial derivatives for GAM rotated labels",
    plt1
  )
  expect_doppelganger(
    "draw partial derivatives for GAM fixed scales rotated",
    plt2
  )
})

## test that issue 39 stays fixed
test_that("draw.gam doesn't create empty plots with multiple parametric terms", {
  plt <- draw(m_2_fac, rug = FALSE)
  skip_on_ci()
  expect_doppelganger("draw issue 39 empty plots", plt)
})

test_that("draw.mgcv_smooth() can plot basic smooth bases", {
  skip_on_cran()
  skip_on_ci() # sign differences due to eigendecomposition in TPRS

  bs <- basis(s(x0), data = quick_eg1)
  plt <- draw(bs)
  skip_on_ci()
  expect_doppelganger("draw basic tprs basis", plt)
})

test_that("draw.mgcv_smooth() can plot basic smooth bases with rotated labels", {
  skip_on_cran()
  skip_on_ci() # sign differences due to eigendecomposition in TPRS

  bs <- basis(s(x0), data = quick_eg1)
  plt <- draw(bs, angle = 45)
  skip_on_ci()
  expect_doppelganger("draw basic tprs basis rotated", plt)
})

test_that("draw.mgcv_smooth() can plot by factor basis smooth bases", {
  bs <- basis(s(x2, by = fac), data = su_eg4)
  plt <- draw(bs)

  skip_on_ci()
  skip_on_cran()
  expect_doppelganger("draw by factor basis", plt)
})

test_that("draw() works with a ziplss models; issue #45", {
  plt <- draw(m_ziplss, rug = FALSE)
  skip_on_ci()
  expect_doppelganger("draw ziplss parametric terms issue 45", plt)
})

test_that("draw works for sample_smooths objects", {
  skip_on_cran()
  skip_on_ci() # minor statistical differences

  sm1 <- smooth_samples(su_m_univar_4, n = 5, seed = 23478, n_vals = 50)
  plt1 <- draw(sm1, alpha = 0.7, n_samples = 5, seed = 2635, rug = FALSE)

  sm2 <- smooth_samples(su_m_bivar, n = 4, seed = 23478, n_vals = 50)
  plt2 <- draw(sm2, alpha = 0.7, n_samples = 4, seed = 2635)

  sm3 <- smooth_samples(su_m_factor_by, n = 5, seed = 23478, n_vals = 50)
  plt3 <- draw(sm3, alpha = 0.7, n_samples = 5, seed = 2635, rug = FALSE)

  sm3 <- smooth_samples(su_m_factor_by, n = 5, seed = 23478, n_vals = 50)
  plt4 <- draw(sm3,
    alpha = 0.7, scales = "fixed", n_samples = 10,
    seed = 2635, rug = FALSE
  )

  skip_on_ci()
  expect_doppelganger("draw smooth_samples for GAM m1", plt1)
  expect_doppelganger("draw smooth_samples for GAM m2", plt2)
  expect_doppelganger("draw smooth_samples for GAM m3", plt3)
  expect_doppelganger("draw smooth_samples for GAM m3 fixed scales", plt4)
})

test_that("draw works for sample_smooths objects rotated labels", {
  skip_on_cran()
  skip_on_ci() # minor statistical differences

  sm1 <- smooth_samples(su_m_univar_4, n = 5, seed = 23478, n_vals = 50)
  plt1 <- draw(sm1,
    alpha = 0.7, n_samples = 5, seed = 2635, angle = 45,
    rug = FALSE
  )

  sm2 <- smooth_samples(su_m_bivar, n = 4, seed = 23478, n_vals = 50)
  plt2 <- draw(sm2,
    alpha = 0.7, n_samples = 4, seed = 2635, angle = 45,
    rug = FALSE
  )

  sm3 <- smooth_samples(su_m_factor_by, n = 5, seed = 23478, n_vals = 50)
  plt3 <- draw(sm3,
    alpha = 0.7, n_samples = 5, seed = 2635, angle = 45,
    rug = FALSE
  )

  sm3 <- smooth_samples(su_m_factor_by, n = 5, seed = 23478, n_vals = 50)
  plt4 <- draw(sm3,
    alpha = 0.7, scales = "fixed", n_samples = 5, seed = 2635,
    angle = 45, rug = FALSE
  )

  skip_on_ci()
  expect_doppelganger("draw smooth_samples for GAM m1 rotated", plt1)
  expect_doppelganger("draw smooth_samples for GAM m2 rotated", plt2)
  expect_doppelganger("draw smooth_samples for GAM m3 rotated", plt3)
  expect_doppelganger(
    "draw smooth_samples for GAM m3 fixed scales rotated",
    plt4
  )
})

test_that("draw works for smooth_samples objects", {
  skip_on_os("win")
  skip_on_os("mac")
  sm2 <- smooth_samples(su_m_bivar, n = 2, seed = 23478, n_vals = 50)
  plt <- draw(sm2, alpha = 0.7, contour = TRUE)

  skip_on_ci()
  expect_doppelganger("draw smooth_samples for bivariate GAM contours", plt)
})

test_that("draw works for sample_smooths objects with n_samples", {
  skip_on_cran()
  skip_on_ci() # minor statistical differences

  sm1 <- smooth_samples(su_m_univar_4, n = 5, seed = 23478, n_vals = 50)
  plt1 <- draw(sm1, alpha = 0.7, n_samples = 3, rug = FALSE, seed = 1)

  sm2 <- smooth_samples(su_m_bivar, n = 4, seed = 23478, n_vals = 50)
  plt2 <- draw(sm2, alpha = 0.7, n_samples = 2, rug = FALSE, seed = 14)

  sm3 <- smooth_samples(su_m_factor_by, n = 5, seed = 23478, n_vals = 50)
  plt3 <- draw(sm3, alpha = 0.7, n_samples = 3, rug = FALSE, seed = 19)

  skip_on_ci()
  expect_doppelganger("draw smooth_samples for m1 n_samples", plt1)
  expect_doppelganger("draw smooth_samples for m2 n_samples", plt2)
  expect_doppelganger("draw smooth_samples for GAM n_samples", plt3)
})

test_that("draw works for sample_smooths objects with user specified smooth", {
  skip_on_cran()
  skip_on_ci() # minor statistical differences

  sm3 <- smooth_samples(su_m_factor_by, n = 5, seed = 23478, n_vals = 50)
  plt1 <- draw(sm3, select = "s(x0)", alpha = 0.7, rug = FALSE)

  plt2 <- draw(sm3,
    select = "s(x2)", alpha = 0.7, partial_match = TRUE,
    rug = FALSE
  )

  skip_on_ci()
  expect_doppelganger(
    "draw selected factor by smooth_samples for GAM m3",
    plt2
  )
  expect_doppelganger("draw selected smooth_samples for GAM m3", plt1)
})

## Issue #22
test_that("draw() can handle a mixture of numeric and factor random effects", {
  df <- data_sim("eg4", seed = 42)
  m <- gam(y ~ s(x2, fac, bs = "re"), data = df, method = "REML")
  plt <- draw(m)

  skip_on_ci()
  expect_doppelganger("issue 22 draw with mixed random effects", plt)
})

test_that("draw.gam uses fixed scales if asked for them: #73", {
  skip_on_cran()
  skip_on_ci()
  df <- data_sim("eg1", n = 1000, seed = 1)
  m <- gam(y ~ s(x1) + s(x2) + ti(x1, x2), data = su_eg1, method = "REML")
  plt <- draw(m, scales = "fixed", rug = FALSE)

  skip_on_ci()
  expect_doppelganger(
    "issue 73 draw uses fixed scales if asked for them",
    plt
  )
})

test_that("draw.gam can take user specified scales", {
  skip_on_os(os = "win")
  skip_on_os(os = "mac") # trivial diffs in contours
  plt1 <- draw(su_m_bivar,
    rug = FALSE,
    continuous_fill = scale_fill_distiller(
      palette = "Spectral",
      type = "div"
    )
  )

  skip_if(packageVersion("mgcv") < "1.8.36")
  plt2 <- draw(mod_fs,
    rug = FALSE,
    discrete_colour = ggplot2::scale_colour_viridis_d(option = "plasma")
  )

  skip_on_ci()
  expect_doppelganger("draw 2d smooth with spectral palette", plt1)

  skip_if(packageVersion("mgcv") < "1.8.36")
  expect_doppelganger(
    "draw fs smooth with discrete plasma palette",
    plt2
  )
})

test_that("plotting sos smooths works", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_on_os("mac")
  expect_silent(plt <- draw(m_sos, n = 20))

  skip_on_ci()
  expect_doppelganger("draw works for sos smooths", plt)
})
