test_that("setup models supply raw columns without a teardown environment", {
  # These objects live in setup.R, outside the formula's global environment.
  expected <- parametric_effects(m_para_sm, terms = "x0", data = df_2_fac)
  expect_equal(parametric_effects(m_para_sm, terms = "x0"), expected)
  expect_equal(data_slice(m_para_sm, x0 = evenly(x0, n = 8)),
    data_slice(m_para_sm, x0 = evenly(x0, n = 8), data = df_2_fac))
  expect_equal(typical_values(m_logical), typical_values(m_logical, data = logi_df))
  expect_equal(data_combos(m_2_fac), data_combos(m_2_fac, data = df_2_fac))

  # Raw columns already in the model must not trigger evaluation of its call.
  m <- m_para_sm
  m$call$data <- quote(stop("The fitting data should not be evaluated."))
  expect_equal(parametric_effects(m, terms = "x0"), expected)
  expect_equal(data_slice(m, x0 = evenly(x0, n = 8)),
    data_slice(m_para_sm, x0 = evenly(x0, n = 8)))
})

test_that("parametric effects recover transformed inputs only when needed", {
  expected <- parametric_effects(m_poly, data = df_2_fac)
  # The stored frame has log(x0) and poly(x2, ...), but lacks their raw inputs.
  # Neither helper should guess inverse transformations or search test globals.
  m <- m_poly
  m$call$data <- quote(missing_setup_data)
  expect_error(parametric_effects(m), class = "gratia_data_recovery_error")
  expect_error(data_slice(m, x0 = evenly(x0)), class = "gratia_data_recovery_error")
  env <- list2env(list(missing_setup_data = df_2_fac), parent = globalenv())
  expect_equal(parametric_effects(m, envir = env), expected)
  expect_equal(data_slice(m, x0 = evenly(x0, n = 8), envir = env),
    data_slice(m, x0 = evenly(x0, n = 8), data = df_2_fac))
  expect_equal(parametric_effects(m, data = df_2_fac), expected)
  # Matching row names do not authorize recovery from changed fitting values.
  env$missing_setup_data$x2 <- env$missing_setup_data$x2 + 1
  expect_error(parametric_effects(m, envir = env),
    class = "gratia_data_recovery_error")
})

test_that("parametric effects preserve explicit local evaluation environments", {
  d <- transformed_data()
  fun <- function(z) log(z + 1)
  m <- mgcv::gam(y ~ x + s(fun(z), k = 5), data = d)
  # Explicit data supply raw inputs; envir separately supplies the local call.
  expect_error(parametric_effects(m, data = d), class = "gratia_expression_error")
  expected <- parametric_effects(m, data = d, envir = environment())
  expect_equal(parametric_effects(m, envir = environment()), expected)
  expect_equal(parametric_effects(with_model_envir(m, environment())), expected)
  expect_equal(data_slice(with_model_envir(m, environment()), z = evenly(z, n = 8)),
    data_slice(m, z = evenly(z, n = 8), data = d))
})

test_that("parametric effects use retained observations after missing rows", {
  d <- transformed_data()
  d$z[c(2, 5)] <- NA_real_
  m <- mgcv::gam(y ~ x + s(z, k = 5), data = d, na.action = na.exclude)
  expect_equal(parametric_effects(m),
    parametric_effects(m, data = d[complete.cases(d), ]))
})
