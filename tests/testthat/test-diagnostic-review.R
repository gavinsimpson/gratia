test_that("ordinary-model residual and scalable APIs reject unsupported conditioning", {
  set.seed(72)
  d <- data.frame(x = runif(80), y = rnorm(80))
  models <- list(lm(y ~ x, data = d), glm(y ~ x, data = d),
    mgcv::gam(y ~ s(x, k = 5), data = d))
  for (m in models) {
    expect_error(quantile_residuals(m, conditioning = "marginal"),
      "supports conditioning = 'conditional' only")
    expect_error(scalable_qq_plot(m, conditioning = "marginal"),
      "supports conditioning = 'conditional' only")
    expect_equal(quantile_residuals(m, conditioning = "conditional", seed = 4),
      quantile_residuals(m, seed = 4))
  }
})

test_that("glmmTMB proxy residual APIs honor conditioning overrides", {
  skip_if_not_installed("glmmTMB")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  set.seed(71)
  d <- data.frame(g = factor(rep(1:8, each = 10)))
  d$y <- rpois(80, exp(rep(rnorm(8), each = 10)))
  m <- glmmTMB::glmmTMB(y ~ 1 + (1 | g), family = poisson(), data = d)
  proxy <- glmmtmb_diagnostic_model(m, type = "quantile")
  expect_equal(quantile_residuals(proxy, conditioning = "marginal", seed = 5),
    quantile_residuals(m, conditioning = "marginal", seed = 5))
})


test_that("appraisal retains distribution checks when a response mean is undefined", {
  skip_if_not_installed("glmmTMB")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  set.seed(81)
  d <- data.frame(g = factor(rep(1:8, each = 10)), y = rnorm(80))
  m <- glmmTMB::glmmTMB(y ~ 1 + (1 | g), family = glmmTMB::t_family(), data = d,
    start = list(psi = log(.8), theta = log(.5)),
    map = list(psi = factor(NA), theta = factor(NA)))
  for (scalable in c(FALSE, TRUE)) {
    plot <- appraise(m, type = "quantile", conditioning = "marginal",
      scalable = scalable, seed = 5, n_simulate = 4)
    expect_s3_class(plot, "patchwork")
    expect_equal(plot[[4]]$labels$title, "Observed vs fitted unavailable")
    expect_silent(ggplot2::ggplot_build(plot[[1]]))
    expect_silent(ggplot2::ggplot_build(plot[[4]]))
  }
})


test_that("prepared field diagnostics use requested conventions in every plot path", {
  skip_if_not_installed("sdmTMB", minimum_version = "1.2.0")
  set.seed(86)
  d <- data.frame(group = factor(rep(1:5, each = 6)), y = rnorm(30))
  m <- sdmTMB::sdmTMB(y ~ 1 + (1 | group), data = d, family = gaussian(),
    spatial = "off", spatiotemporal = "off")
  prepared <- diagnostic_data(m, type = "quantile", seed = 3)
  proxy <- prepared$source
  for (input in list(prepared, proxy)) {
    for (plot_fun in list(qq_plot, worm_plot)) {
      plot <- plot_fun(input, type = "quantile", field_method = "mle-eb", seed = 3)
      expect_identical(plot$labels$subtitle, "Method: std. normal")
    }
    plot <- appraise(input, type = "quantile", field_method = "mle-eb", seed = 3,
      scalable = FALSE)
    expect_identical(plot[[1]]$labels$subtitle, "Method: std. normal")
    expected <- quantile_residuals(m, type = "quantile", field_method = "mle-eb", seed = 3)
    expect_equal(sort(plot[[1]]$data$residuals), sort(as.numeric(expected)))
  }
})


test_that("gaulss diagnostic references follow the fitted likelihood weight semantics", {
  set.seed(92)
  d <- data.frame(y = rnorm(100), w = rep(c(1, 9), 50))
  m <- mgcv::gam(list(y ~ 1, ~1), family = mgcv::gaulss(), data = d, weights = w)
  unweighted <- mgcv::gam(list(y ~ 1, ~1), family = mgcv::gaulss(), data = d)
  expect_equal(coef(m), coef(unweighted), tolerance = 1e-9)
  mu <- m$fitted.values[, 1]; sd <- 1 / m$fitted.values[, 2]
  native_loglik <- m$family$ll(m$y, predict(m, type = "lpmatrix"), coef(m),
    m$prior.weights, m$family, deriv = 0)$l
  expect_equal(native_loglik, sum(dnorm(m$y, mu, sd, log = TRUE)))
  law <- response_distribution(m)
  expect_equal(law$cdf(m$y), pnorm(m$y, mu, sd))
  expect_equal(quantile_residuals(m, type = "quantile", seed = 3), (m$y - mu) / sd)
  expect_equal(withr::with_seed(3, law$rd()), withr::with_seed(3, rnorm(100, mu, sd)))
  beta <- matrix(coef(m), nrow = 1)
  raw <- posterior_samples(m, n_draws = 1, method = "user", draws = beta, seed = 3)
  streamed <- withr::with_seed(3, diagnostic_posterior_sampler(m, n = 1,
    method = "user", draws = beta)(1))
  expect_equal(raw$.response, streamed$.response)
  expect_equal(raw$.response, simulate(m, nsim = 1, seed = 3)[[1]])
  overridden <- simulate(m, nsim = 1, seed = 3, weights = 9)[[1]]
  expect_equal(overridden - mu, (raw$.response - mu) / 3)
})
