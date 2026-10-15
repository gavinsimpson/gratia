# Evaluate a native conditional parameter state without estimating covariance
# parameters: covariance tests then have a known AR1 coefficient and cannot
# pass merely because a fitted variance collapsed to zero.
sdmtmb_dependence_state <- function(family = gaussian(), iid = FALSE,
                                   spatial = FALSE, temporal = FALSE,
                                   smooth = FALSE, unique_groups = FALSE) {
  locations <- expand.grid(x = c(0, 1), north = c(0, 1))
  d <- locations[rep(seq_len(4), 3), ]
  d$time <- rep(1:3, each = 4)
  d$group <- factor(if (unique_groups) seq_len(nrow(d)) else rep(1:4, 3))
  d$smooth_x <- seq(-1, 1, length.out = nrow(d))
  d$y <- if (isTRUE(family$delta)) rep(c(0, 1, 2), 4) else
    if (family$family == "poisson") rep(1:3, 4) else seq(-1, 1, length.out = nrow(d))
  mesh <- sdmTMB::make_mesh(d, xy_cols = c("x", "north"), cutoff = 0.4)
  formula <- if (iid) y ~ 1 + (1 | group) else if (smooth) y ~ s(smooth_x, k = 4) else y ~ 1
  m <- sdmTMB::sdmTMB(formula, data = d, family = family, mesh = mesh,
    spatial = if (spatial) "on" else "off",
    time = if (temporal) "time" else NULL,
    spatiotemporal = if (temporal) "ar1" else "off", do_fit = FALSE)
  parameter <- m$tmb_obj$par
  parameter[names(parameter) == "ar1_phi"] <- qlogis(0.85) # rho = 0.7
  invisible(m$tmb_obj$fn(parameter))
  m$tmb_obj$env$last.par.best <- m$tmb_obj$env$last.par
  m$last.par.best <- m$tmb_obj$env$last.par.best
  m$parlist <- m$tmb_obj$env$parList()
  m
}

test_that("sdm diagnostic modes use their stated native joint response law", {
  skip_if_not_installed("sdmTMB")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  m <- sdmtmb_dependence_state(iid = TRUE)
  for (setting in list(c("conditional", "mle-eb"), c("conditional", "mle-mvn"),
      c("conditional", "mle-mvn-integrated"), c("marginal", "mle-mvn"))) {
    conditioning <- setting[1L]; method <- setting[2L]
    proxy <- sdmtmb_diagnostic_model(m, conditioning, field_method = method)
    dist <- proxy$distribution
    actual <- withr::with_seed(32, dist$rd())
    expected <- withr::with_seed(32, {
      draw_seed <- sample.int(.Machine$integer.max, 1L)
      as.numeric(simulate(m, nsim = 1, seed = draw_seed,
        type = if (conditioning == "conditional" && method == "mle-mvn-integrated") "mle-mvn" else "mle-eb",
        mcmc_samples = if (is.null(dist$field_state)) NULL else matrix(dist$field_state, ncol = 1),
        mle_mvn_samples = "multiple", re_form = if (conditioning == "marginal") NA else NULL,
        silent = TRUE))
    })
    expect_equal(actual, expected)
    expect_true(dist$joint_available)
    expect_equal(dist$independent, conditioning == "conditional" && method != "mle-mvn-integrated")
    if (conditioning == "marginal" || method == "mle-mvn-integrated") expect_identical(dist$calibration, "simulation ranks")
  }
  plugin <- sdmtmb_diagnostic_model(m, field_method = "mle-eb")
  expect_equal(qnorm(plugin$distribution$cdf(plugin$y)),
    as.vector(residuals(m, type = "mle-eb")), tolerance = 1e-8)
  native <- sdmtmb_diagnostic_model(m)
  expect_false(is.null(native$distribution$field_state))
  expect_true(native$distribution$independent)
  expect_null(native$distribution$calibration)
  expect_match(native$distribution$approximation, "one native joint posterior field draw")
  expected_eta <- predict(m, newdata = m$data, offset = m$offset,
    mcmc_samples = matrix(native$distribution$field_state, ncol = 1), model = 1, nsim = 1)
  native_residual <- residuals(m, type = "mle-mcmc", mcmc_samples = expected_eta)
  expect_equal(qnorm(native$distribution$cdf(native$y)), as.vector(native_residual))
  withr::local_options(gratia.diagnostics.field_seed = 2L)
  expect_false(isTRUE(all.equal(sdmtmb_diagnostic_model(m)$fitted.values, native$fitted.values)))
  integrated <- sdmtmb_diagnostic_model(m, field_method = "mle-mvn-integrated")
  expect_match(integrated$distribution$approximation, "differ from native")
  expect_equal(integrated$eta_label, "Integrated response expectation")
  before <- .Random.seed
  r1 <- quantile_residuals(m, seed = 4)
  expect_equal(r1, quantile_residuals(m, seed = 4))
  expect_identical(.Random.seed, before)
})

test_that("sdm dependence follows shared design and native smoother selection", {
  skip_if_not_installed("sdmTMB")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  m <- sdmtmb_dependence_state(iid = TRUE, unique_groups = TRUE)
  expect_true(sdmtmb_response_dependence(m, "conditional", "mle-mvn-integrated", 1:12)$independent)
  expect_true(sdmtmb_response_dependence(m, "marginal", "mle-mvn", 1:12)$independent)
  m <- sdmtmb_dependence_state(smooth = TRUE)
  expect_false(sdmtmb_response_dependence(m, "conditional", "mle-mvn-integrated", 1:12)$independent)
  expect_true(sdmtmb_response_dependence(m, "marginal", "mle-mvn", 1:12)$independent)
  marginal <- sdmtmb_diagnostic_model(m, conditioning = "marginal")
  expect_true(marginal$distribution$independent)
  expect_null(marginal$distribution$calibration)
})

test_that("native temporal simulations retain AR1 field covariance", {
  skip_if_not_installed("sdmTMB")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  m <- sdmtmb_dependence_state(temporal = TRUE)
  values <- simulate(m, nsim = 2000, seed = 1, re_form = NA,
    observation_error = FALSE, silent = TRUE)
  # Compare on Fisher's z scale, allowing five sampling standard errors.
  # This retains checks for independence and for the wrong correlation at lag 2.
  z_tolerance <- 5 / sqrt(ncol(values) - 3)
  expect_lt(abs(atanh(cor(values[1, ], values[5, ])) - atanh(0.7)),
    z_tolerance)
  expect_lt(abs(atanh(cor(values[1, ], values[9, ])) - atanh(0.7^2)),
    z_tolerance)
  for (conditioning in c("conditional", "marginal")) {
    dist <- sdmtmb_diagnostic_model(m, conditioning, field_method = "mle-mvn-integrated")$distribution
    expect_false(dist$independent)
    expect_true(dist$joint_available)
    expect_identical(dist$calibration, "simulation ranks")
  }
})

test_that("nonlinear spatial and delta laws retain native dependence and means", {
  skip_if_not_installed("sdmTMB")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  for (family in list(poisson(), sdmTMB::delta_gamma(),
      sdmTMB::delta_gamma(type = "poisson-link"))) {
    m <- sdmtmb_dependence_state(family, spatial = TRUE, temporal = TRUE)
    frozen <- sdmtmb_diagnostic_model(m)
    expect_true(frozen$distribution$independent)
    expect_match(frozen$distribution$approximation, "one native joint posterior field draw")
    frozen_actual <- withr::with_seed(17, frozen$distribution$rd())
    frozen_expected <- withr::with_seed(17, {
      draw_seed <- sample.int(.Machine$integer.max, 1L)
      as.numeric(simulate(m, nsim = 1, seed = draw_seed, type = "mle-eb",
        mcmc_samples = matrix(frozen$distribution$field_state, ncol = 1),
        re_form = NULL, silent = TRUE))
    })
    expect_equal(frozen_actual, frozen_expected)
    if (isTRUE(family$delta)) {
      positive <- sdmtmb_diagnostic_model(m, component = "positive")
      state <- matrix(positive$distribution$field_state, ncol = 1)
      eta <- cbind(as.numeric(predict(m, newdata = m$data, mcmc_samples = state,
        model = 1, nsim = 1, offset = m$offset)),
        as.numeric(predict(m, newdata = m$data, mcmc_samples = state,
        model = 2, nsim = 1, offset = m$offset)))
      native <- residuals(m, type = "mle-mcmc", model = 2,
        mcmc_samples = if (family$type == "standard") eta[, 2] else eta)
      keep <- which(!is.na(model_row_map(positive)))
      expect_equal(qnorm(positive$distribution$cdf(positive$y)), native[keep], tolerance = 1e-8)
    }
    for (component in if (isTRUE(family$delta)) c("response", "positive", "binomial") else "response") {
      proxy <- sdmtmb_diagnostic_model(m, "marginal", component)
      dist <- proxy$distribution
      expect_false(dist$independent)
      expect_true(dist$joint_available)
      expect_true(all(is.finite(dist$mean)))
      expect_true(all(is.finite(dist$variance)))
      expect_identical(dist$calibration, "simulation ranks")
      expect_equal(proxy$fitted.values, dist$mean)
      expect_equal(proxy$linear.predictors, dist$mean)
      residual <- quantile_residuals(proxy, seed = 4)
      expect_true(all(is.finite(residual[which(!is.na(model_row_map(proxy)))])))
      # Draw selection and delta combination exactly follow the native joint law.
      actual <- withr::with_seed(7, dist$rd())
      expected <- withr::with_seed(7, {
        draw_seed <- sample.int(.Machine$integer.max, 1L)
        value <- as.numeric(simulate(m, nsim = 1, seed = draw_seed,
          type = "mle-eb", re_form = NA,
          model = if (component == "response") NA_integer_ else if (component == "positive") 2L else 1L,
          silent = TRUE))
        value[which(!is.na(model_row_map(proxy)))]
      })
      expect_equal(actual, expected)
    }
  }
})

test_that("PPC parameter draws do not add a second fitted-field draw", {
  skip_if_not_installed("sdmTMB")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  m <- sdmtmb_dependence_state(iid = TRUE)
  testthat::local_mocked_bindings(diagnostic_joint_parameter_draws = function(p, n, ...) {
    matrix(rep(p, n), nrow = n, byrow = TRUE)
  })
  for (conditioning in c("conditional", "marginal")) {
    samples <- posterior_samples(m, n_draws = 1, seed = 7, conditioning = conditioning)
    plugin <- sdmtmb_diagnostic_model(m, conditioning, field_method = "mle-eb")
    expected <- withr::with_seed(7, plugin$distribution$rd())
    expect_equal(samples$.response, expected)
  }
  expect_error(posterior_samples(m, n_draws = 1, field_method = "mle-mvn"),
    "selects residual diagnostics")
})


test_that("sdm field modes reach ordinary, scalable and prepared plot paths", {
  skip_if_not_installed("sdmTMB")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  m <- sdmtmb_dependence_state(iid = TRUE)
  prepared <- diagnostic_data(m, seed = 4, field_method = "mle-eb")
  for (method in c("mle-eb", "mle-mvn", "mle-mvn-integrated")) {
    ordinary <- qq_plot(m, type = "pit", seed = 4, n_simulate = 3,
      field_method = method)
    scalable <- scalable_qq_plot(m, type = "pit", seed = 4,
      n_simulate = 3, field_method = method)
    expected <- if (method == "mle-eb") "does not recommend" else
      if (method == "mle-mvn") "one native joint posterior" else "differ from[[:space:]]+native"
    expect_match(gsub("[[:space:]]+", " ", ordinary$labels$subtitle), expected)
    expect_match(gsub("[[:space:]]+", " ", scalable$labels$subtitle), expected)
    panel <- residuals_linpred_plot(m, seed = 4, field_method = method)
    expect_match(gsub("[[:space:]]+", " ", panel$labels$subtitle), expected)
    changed <- diagnostic_data(prepared, seed = 4, field_method = method)
    direct <- diagnostic_data(m, seed = 4, field_method = method)
    expect_equal(changed$data, direct$data)
    source <- diagnostic_plot_source(prepared, seed = 4, field_method = method)
    expect_equal(source$distribution$mean, direct$source$distribution$mean)
  }
  marginal <- diagnostic_data(prepared, conditioning = "marginal", seed = 4)
  expect_identical(marginal$source$conditioning, "marginal")
  expect_identical(marginal$source$distribution$calibration, "simulation ranks")
})


test_that("marginal PPC rejects latent blocks outside verified native simulation", {
  skip_if_not_installed("sdmTMB")
  m <- sdmtmb_dependence_state(iid = TRUE)
  testthat::local_mocked_bindings(sdmtmb_random_tags = function(model) "unverified_effect")
  expect_error(posterior_samples(m, n_draws = 1, conditioning = "marginal"),
    "Marginal posterior predictive simulation is unavailable:.*unverified_effect")
})
