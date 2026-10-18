test_that("Student-t distributions retain scale, tails and moment requirements", {
  location <- c(-1, 2, 4)
  scale <- c(0.5, 1, 2)
  for (df in c(1.5, 5)) {
    law <- distribution_from_parameters("student", location, scale, df = df)
    y <- c(-2, 0, 5)
    expect_equal(law$cdf(y), pt((y - location) / scale, df))
    expect_equal(law$cdf(y, log_p = TRUE, lower_tail = FALSE),
      pt((y - location) / scale, df, log.p = TRUE, lower.tail = FALSE))
    expect_equal(law$qf(c(.1, .5, .9)),
      location + scale * qt(c(.1, .5, .9), df))
    expect_equal(law$qf(c(-30, -50, -100), log_p = TRUE, lower_tail = FALSE),
      location + scale * qt(c(-30, -50, -100), df,
        log.p = TRUE, lower.tail = FALSE))
    expect_equal(withr::with_seed(4, law$rd()),
      withr::with_seed(4, location + scale * rt(3, df)))
    expect_equal(law$mean, location)
    if (df > 2) {
      expect_equal(law$variance, scale^2 * df / (df - 2))
    } else {
      expect_null(law$variance)
      expect_error(check_distribution_residual_moments(law, "pearson"),
        "finite response variance.*df > 2")
    }
  }
})

test_that("sdmTMB Student-t quantiles follow fixed and estimated degrees of freedom", {
  skip_if_not_installed("sdmTMB", minimum_version = "1.2.0")
  withr::local_seed(413)
  d <- data.frame(x = runif(250, -1, 1))
  d$y <- 1 + .5 * d$x + .7 * rt(250, df = 4)
  for (df in list(5, NULL, 1.5)) {
    case <- paste("Student-t df", if (is.null(df)) "estimated" else df)
    m <- builder_diagnostic_step(paste(case, "fit"),
      sdmTMB::sdmTMB(y ~ x, data = d, family = sdmTMB::student(df = df),
        spatial = "off", spatiotemporal = "off"))
    pars <- builder_diagnostic_step(paste(case, "get_pars"), sdmTMB::get_pars(m))
    fitted_df <- exp(pars$ln_student_df) + 1
    scale <- exp(pars$ln_phi)
    mu <- builder_diagnostic_step(paste(case, "predict"), predict(m))$est
    law <- builder_diagnostic_step(paste(case, "response_distribution mle-eb"),
      response_distribution(m, field_method = "mle-eb"))
    expect_equal(law$cdf(d$y), pt((d$y - mu) / scale, fitted_df))
    actual <- builder_diagnostic_step(paste(case, "quantile_residuals mle-eb"),
      quantile_residuals(m, type = "quantile", field_method = "mle-eb"))
    expected <- builder_diagnostic_step(paste(case, "native residuals mle-eb"),
      residuals(m, type = "mle-eb"))
    expect_equal(actual, as.vector(expected), tolerance = 1e-8)
    expect_s3_class(builder_diagnostic_step(paste(case, "appraise"),
      appraise(m, seed = 3, n_simulate = 3)), "patchwork")
    if (fitted_df > 2) {
      expect_equal(law$variance, rep(scale^2 * fitted_df / (fitted_df - 2), nrow(d)))
      expect_s3_class(builder_diagnostic_step(paste(case, "diagnostic_data pearson"),
        diagnostic_data(m, type = "pearson")), "gratia_diagnostics")
    } else {
      expect_error(builder_diagnostic_step(paste(case, "diagnostic_data pearson"),
        diagnostic_data(m, type = "pearson")),
        "finite response variance.*df > 2")
    }
    raw <- builder_diagnostic_step(paste(case, "posterior_predictive_plot raw"),
      posterior_predictive_plot(m, n = 2, seed = 8, scalable = FALSE))
    streamed <- builder_diagnostic_step(paste(case, "posterior_predictive_plot streamed"),
      posterior_predictive_plot(m, n = 2, seed = 8, scalable = TRUE))
    expect_equal(lapply(raw$layers, `[[`, "data"),
      lapply(streamed$layers, `[[`, "data"))
    expect_silent(builder_diagnostic_step(paste(case, "ggplot_build"),
      ggplot2::ggplot_build(streamed)))
  }
})

test_that("Student-t fields preserve conditioning and reject infinite Pearson variance", {
  skip_if_not_installed("sdmTMB", minimum_version = "1.2.0")
  withr::local_seed(417)
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  d <- data.frame(group = factor(rep(1:8, each = 12)))
  d$y <- rnorm(8)[d$group] + .5 * rt(96, df = 1.5)
  m <- builder_diagnostic_step("Student-t random intercept fit, df 1.5, seed 417",
    sdmTMB::sdmTMB(y ~ 1 + (1 | group), data = d,
      family = sdmTMB::student(df = 1.5), spatial = "off", spatiotemporal = "off"))
  for (method in c("mle-eb", "mle-mvn", "mle-mvn-integrated")) {
    expect_true(all(is.finite(builder_diagnostic_step(
      paste("Student-t random intercept quantile_residuals", method),
      quantile_residuals(m, type = "quantile", seed = 3, field_method = method)))))
    expect_error(builder_diagnostic_step(
      paste("Student-t random intercept diagnostic_data pearson", method),
      diagnostic_data(m, type = "pearson", field_method = method)),
      "finite response variance.*df > 2")
  }
  expect_true(all(is.finite(builder_diagnostic_step(
    "Student-t random intercept quantile_residuals marginal",
    quantile_residuals(m, type = "quantile", seed = 3, conditioning = "marginal")))))
  expect_error(builder_diagnostic_step(
    "Student-t random intercept diagnostic_data pearson marginal",
    diagnostic_data(m, type = "pearson", conditioning = "marginal")),
    "finite response variance.*df > 2")
  for (conditioning in c("conditional", "marginal")) {
    raw <- builder_diagnostic_step(paste("Student-t random intercept raw", conditioning),
      posterior_predictive_plot(m, n = 2, seed = 5, conditioning = conditioning))
    streamed <- builder_diagnostic_step(
      paste("Student-t random intercept streamed", conditioning),
      posterior_predictive_plot(m, n = 2, seed = 5,
        conditioning = conditioning, scalable = TRUE))
    expect_equal(lapply(raw$layers, `[[`, "data"),
      lapply(streamed$layers, `[[`, "data"))
  }
})

test_that("native predictive families need no analytic diagnostic constructor", {
  skip_if_not_installed("sdmTMB", minimum_version = "1.2.0")
  withr::local_seed(419)
  d <- data.frame(x = runif(100), trials = rep(c(4, 8), 50))
  d$y <- rbinom(100, d$trials, rbeta(100, 3, 2))
  m <- builder_diagnostic_step("beta-binomial fit, seed 419",
    sdmTMB::sdmTMB(cbind(y, trials - y) ~ x, data = d,
      family = sdmTMB::betabinomial(), spatial = "off", spatiotemporal = "off"))
  expect_error(response_distribution(m), "No response-distribution constructor")
  raw <- builder_diagnostic_step("beta-binomial posterior_predictive_plot raw",
    posterior_predictive_plot(m, n = 3, seed = 8, scalable = FALSE))
  streamed <- builder_diagnostic_step("beta-binomial posterior_predictive_plot streamed",
    posterior_predictive_plot(m, n = 3, seed = 8, scalable = TRUE))
  expect_equal(raw$data$.response[raw$data$.draw == 0], d$y / d$trials)
  expect_true(all(raw$data$.response >= 0 & raw$data$.response <= 1))
  expect_equal(raw$labels$y, "Probability")
  expect_equal(lapply(raw$layers, `[[`, "data"),
    lapply(streamed$layers, `[[`, "data"))
  expect_silent(builder_diagnostic_step("beta-binomial ggplot_build",
    ggplot2::ggplot_build(streamed)))
})

test_that("predictive observations retain missing rows and delta component selection", {
  skip_if_not_installed("sdmTMB", minimum_version = "1.2.0")
  withr::local_seed(421)
  d <- data.frame(x = runif(90))
  d$y <- rbinom(90, 1, .7) * rgamma(90, shape = 3)
  d$y[c(7, 31)] <- NA_real_
  m <- builder_diagnostic_step("delta-Gamma fit, seed 421",
    sdmTMB::sdmTMB(y ~ x, data = d, family = sdmTMB::delta_gamma(),
      spatial = "off", spatiotemporal = "off"))
  # Posterior rows index sdmTMB's stored response matrix, which omits these
  # missing input responses before fitting.
  y <- d$y[!is.na(d$y)]
  for (component in c("response", "binomial", "positive")) {
    expected <- switch(component, response = y,
      binomial = as.numeric(y > 0), positive = ifelse(y > 0, y, NA_real_))
    raw <- builder_diagnostic_step(paste("delta-Gamma raw", component),
      posterior_predictive_plot(m, n = 2, seed = 8, component = component))
    observed <- raw$data[raw$data$.draw == 0, ]
    expect_equal(observed$.row, which(!is.na(expected)))
    expect_equal(observed$.response, expected[!is.na(expected)])
    streamed <- builder_diagnostic_step(paste("delta-Gamma streamed", component),
      posterior_predictive_plot(m, n = 2, seed = 8,
        component = component, scalable = TRUE))
    expect_equal(lapply(raw$layers, `[[`, "data"),
      lapply(streamed$layers, `[[`, "data"))
  }
})
