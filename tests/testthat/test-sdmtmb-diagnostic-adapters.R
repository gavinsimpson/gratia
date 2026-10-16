test_that("sdmTMB fitted diagnostics agree with native conditional calibration", {
  skip_if_not_installed("sdmTMB")
  set.seed(123)
  d <- data.frame(x = seq(-1, 1, length.out = 60), offset = 0.3)
  d$y <- rnorm(60, 1 + d$x + d$offset, sd = 0.7)
  m <- sdmTMB::sdmTMB(y ~ x, data = d, offset = d$offset, spatial = "off")
  p <- sdmtmb_diagnostic_model(m, field_method = "mle-eb")
  native <- predict(m, newdata = m$data, offset = m$offset)
  expect_equal(p$fitted.values, native$est)
  expect_equal(qnorm(p$distribution$cdf(p$y)),
    as.vector(residuals(m, type = "mle-eb")), tolerance = 1e-8)
  expect_equal(diagnostic_reference_residuals(p, p$y, "response"),
    as.vector(residuals(m, type = "response")), tolerance = 1e-8)
})

test_that("sdmTMB delta diagnostics calibrate the entire response mixture", {
  skip_if_not_installed("sdmTMB")
  set.seed(345)
  d <- data.frame(x = seq(-1, 1, length.out = 80))
  d$y <- rbinom(80, 1, 0.65) * rgamma(80, shape = 2, scale = 0.8)
  m <- sdmTMB::sdmTMB(y ~ x, data = d, spatial = "off",
    family = sdmTMB::delta_gamma())
  p <- sdmtmb_diagnostic_model(m, field_method = "mle-eb")
  native <- predict(m, type = "response", newdata = m$data, offset = m$offset)
  expect_equal(p$fitted.values, native$est)
  expect_equal(p$distribution$cdf(rep(0, 80)), 1 - native$est1)
  b <- p$distribution$bounds(p$y)
  expect_true(all(b$left_limit[p$y == 0]))
  expect_equal(p$distribution$cdf_left(b$lower)[p$y == 0],
    rep(0, sum(p$y == 0)))
  u <- distribution_quantile_residuals(p$distribution, p$y, "pit", seed = 17)
  expect_true(all(u[d$y == 0] >= 0 & u[d$y == 0] <= 1 - native$est1[d$y == 0]))
  positive <- sdmtmb_diagnostic_model(m, component = "positive", field_method = "mle-eb")
  r <- distribution_quantile_residuals(positive$distribution, positive$y,
    "quantile", seed = 12)
  expect_equal(r, as.vector(residuals(m, type = "mle-eb", model = 2))[d$y > 0],
    tolerance = 1e-8)
  expect_equal(model_row_map(positive),
    ifelse(d$y > 0, cumsum(d$y > 0), NA_integer_))
  expect_s3_class(posterior_predictive_plot(m, n = 2L, seed = 12), "ggplot")
  expect_s3_class(posterior_predictive_plot(m, n = 2L, seed = 12,
    conditioning = "marginal"), "ggplot")
})

test_that("sdmTMB binomial counts use trial-sized probability bounds", {
  skip_if_not_installed("sdmTMB")
  set.seed(42)
  d <- data.frame(x = runif(60), n = sample(3:10, 60, replace = TRUE))
  d$y <- rbinom(60, d$n, plogis(d$x))
  m <- sdmTMB::sdmTMB(cbind(y, n - y) ~ x, data = d,
    spatial = "off", family = binomial())
  p <- sdmtmb_diagnostic_model(m, field_method = "mle-eb")
  expect_equal(p$y, d$y / d$n)
  expect_equal(p$distribution$cdf(p$y),
    pbinom(d$y, d$n, p$fitted.values))
  b <- p$distribution$bounds(p$y)
  expect_equal(p$distribution$cdf(b$lower),
    pbinom(d$y - 1, d$n, p$fitted.values))
  raw <- posterior_samples(m, n_draws = 2L, seed = 10)
  expect_true(all(raw$.response >= 0 & raw$.response <= 1))
  ordinary <- posterior_predictive_plot(m, n = 2L, seed = 10)
  scalable <- posterior_predictive_plot(m, n = 2L, seed = 10,
    scalable = TRUE, batch_size = 1L)
  expect_s3_class(ordinary, "ggplot")
  expect_s3_class(scalable, "ggplot")
})

test_that("sdmTMB diagnostics retain fitted IID effects", {
  skip_if_not_installed("sdmTMB")
  if (!full_test_run()) {
    withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  }
  set.seed(112)
  d <- data.frame(x = runif(80), g = factor(rep(1:8, each = 10)))
  d$y <- d$x + rep(rnorm(8, sd = 2), each = 10) + rnorm(80, sd = 0.4)
  m <- sdmTMB::sdmTMB(y ~ x + (1 | g), data = d, spatial = "off")
  p <- sdmtmb_diagnostic_model(m, field_method = "mle-eb")
  native <- predict(m, newdata = m$data, offset = m$offset)
  expect_equal(p$fitted.values, native$est)
  expect_equal(qnorm(p$distribution$cdf(p$y)),
    as.vector(residuals(m, type = "mle-eb")), tolerance = 1e-8)
  marginal <- sdmtmb_diagnostic_model(m, conditioning = "marginal", field_method = "mle-eb")
  expect_true(is.function(marginal$distribution$cdf))
  expect_true(all(is.finite(quantile_residuals(m, conditioning = "marginal",
    field_method = "mle-eb"))))
})

test_that("sdmTMB spatial calibration retains the fitted field", {
  skip_if_not_installed("sdmTMB")
  set.seed(202)
  d <- data.frame(x = runif(50), north = runif(50))
  d$y <- sin(3 * d$x) + cos(3 * d$north) + rnorm(50, sd = 0.3)
  mesh <- sdmTMB::make_mesh(d, xy_cols = c("x", "north"), cutoff = 0.3)
  m <- sdmTMB::sdmTMB(y ~ 1, data = d, mesh = mesh, spatial = "on")
  p <- sdmtmb_diagnostic_model(m, field_method = "mle-eb")
  native <- predict(m, newdata = m$data, offset = m$offset)
  expect_equal(p$fitted.values, native$est)
  expect_equal(qnorm(p$distribution$cdf(p$y)),
    as.vector(residuals(m, type = "mle-eb")), tolerance = 1e-8)
  expect_s3_class(posterior_predictive_plot(m, n = 2L, seed = 13), "ggplot")
})
