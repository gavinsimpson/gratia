test_that("latent dependence is determined by actual row covariance", {
  expect_false(row_dependence_from_design(diag(4)))
  expect_false(row_dependence_from_design(matrix(0, 4, 2)))
  expect_true(row_dependence_from_design(matrix(1, 4, 1)))
  # Both latent columns are shared, but their row covariances cancel exactly.
  orthogonal <- matrix(c(1, 1, 1, -1), 2)
  expect_false(row_dependence_from_design(orthogonal))
  expect_true(row_dependence_from_design(rbind(orthogonal, c(1, 0))))
  expect_false(row_dependence_from_design(Matrix::Diagonal(1000)))
})

test_that("native lme variance weights do not imply dependent responses", {
  skip_if_not_installed("nlme")
  set.seed(51)
  d <- data.frame(g = factor(rep(1:8, 10)), time = rep(1:10, each = 8),
    x = runif(80), v = rep(c(1, 4), 40))
  d$y <- d$x + rep(rnorm(8, sd = 2), 10) + rnorm(80, sd = sqrt(d$v))
  independent <- nlme::lme(y ~ x, random = ~1 | g, data = d,
    weights = nlme::varFixed(~v),
    correlation = nlme::corAR1(0, form = ~time | g, fixed = TRUE))
  conditional <- lme_diagnostic_model(independent)$distribution
  expect_true(conditional$independent)
  expect_true(conditional$joint_available)
  expect_true(lme_diagnostic_model(independent, "marginal")$distribution$joint_available)
  expect_false(lme_diagnostic_model(independent, "marginal")$distribution$independent)
  correlated <- update(independent,
    correlation = nlme::corAR1(0.6, form = ~time | g, fixed = TRUE))
  dependent <- lme_diagnostic_model(correlated)$distribution
  expect_false(dependent$independent)
  expect_true(dependent$joint_available)
  expect_match(dependent$dependence_reason, "covariance")
})

test_that("native mer models distinguish conditional and shared marginal effects", {
  skip_if_not_installed("lme4")
  set.seed(72)
  d <- data.frame(g = factor(rep(1:10, each = 10)), x = runif(100))
  d$y <- d$x + rep(rnorm(10, sd = 2), each = 10) + rnorm(100, sd = 0.5)
  m <- lme4::lmer(y ~ x + (1 | g), data = d)
  expect_true(mer_diagnostic_model(m)$distribution$independent)
  expect_false(mer_diagnostic_model(m, "marginal")$distribution$independent)
  expect_equal(mer_diagnostic_model(m)$fitted.values, as.vector(fitted(m)))
  expect_true(mer_diagnostic_model(m, "marginal")$distribution$joint_available)
})

test_that("non-Gaussian marginal mer calibration uses native joint draws", {
  skip_if_not_installed("lme4")
  set.seed(73)
  d <- data.frame(g = factor(rep(1:10, each = 12)), x = runif(120))
  d$y <- rpois(120, exp(0.3 + d$x + rep(rnorm(10, sd = 0.7), each = 12)))
  m <- lme4::glmer(y ~ x + (1 | g), data = d, family = poisson())
  before <- .Random.seed
  p <- mer_diagnostic_model(m, "marginal")
  expect_identical(.Random.seed, before)
  expect_false(p$distribution$independent)
  expect_true(p$distribution$joint_available)
  expect_true(is.function(p$distribution$cdf))
  expect_true(is.function(p$distribution$rd))
  expect_true(all(is.finite(p$fitted.values)))
  expect_true(mean(p$fitted.values) > mean(exp(p$linear.predictors)))
  expect_match(p$distribution$approximation, "simulation|Monte|rank", ignore.case = TRUE)
  conditional <- mer_diagnostic_model(m)
  before <- .Random.seed
  overridden <- response_distribution(conditional, conditioning = "marginal")
  expect_identical(.Random.seed, before)
  expect_equal(overridden$mean, p$fitted.values)
})

test_that("correlated PQL keeps marginal families without inventing a copula", {
  skip_if_not_installed("nlme")
  set.seed(90)
  # Clear curvature and group variation keep both fitted variances away from zero.
  d <- data.frame(g = factor(rep(1:12, each = 12)), time = rep(1:12, 12),
    x = rep(seq(0.05, 0.95, length.out = 12), 12))
  d$y <- rpois(nrow(d), exp(1 + sin(2 * pi * d$x) +
    rep(rnorm(12, sd = 0.7), each = 12)))
  m <- suppressMessages(mgcv::gamm(y ~ s(x, k = 5), data = d,
    random = list(g = ~1), family = poisson(),
    correlation = nlme::corAR1(0.4, form = ~time | g, fixed = TRUE)))
  conditional <- gamm_diagnostic_model(m)$distribution
  expect_false(conditional$independent)
  expect_false(conditional$joint_available)
  expect_null(conditional$rd)
  expect_true(is.function(conditional$marginal_rd))
  expect_true(is.function(conditional$cdf))
  expect_match(conditional$dependence_reason, "working correlation")
  expect_match(conditional$approximation, "PQL")
  expect_s3_class(qq_plot(m, type = "pit"), "ggplot")
  expect_s3_class(qq_plot(m, type = "response", method = "marginal",
    n_simulate = 3L), "ggplot")
  expect_error(qq_plot(m, type = "response", method = "simulate"),
    "Joint response references")
  marginal <- gamm_diagnostic_model(m, "marginal")$distribution
  expect_null(marginal$cdf)
  expect_null(marginal$rd)
  expect_false(marginal$joint_available)
})

test_that("observation-specific GLMM effects leave marginal responses independent", {
  skip_if_not_installed("lme4")
  set.seed(214)
  d <- data.frame(id = factor(seq_len(100)),
    y = rpois(100, exp(rnorm(100, 0.5, 0.8))))
  m <- lme4::glmer(y ~ 1 + (1 | id), data = d, family = poisson(),
    control = lme4::glmerControl(check.nobs.vs.nlev = "ignore",
      check.nobs.vs.nRE = "ignore"))
  expect_true(lme4::getME(m, "theta") > 0)
  p <- mer_diagnostic_model(m, "marginal")
  expect_true(p$distribution$independent)
  expect_true(p$distribution$joint_available)
})

test_that("native lme and merMod dispatch through the shared diagnostics", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("lme4")
  set.seed(39)
  d <- data.frame(g = factor(rep(1:8, each = 10)), x = runif(80))
  d$y <- d$x + rep(rnorm(8, sd = 2), each = 10) + rnorm(80)
  d$y[2L] <- NA_real_
  lme <- nlme::lme(y ~ x, random = ~1 | g, data = d, na.action = na.exclude)
  mer <- lme4::lmer(y ~ x + (1 | g), data = d, na.action = na.exclude)
  for (m in list(lme, mer)) {
    r <- quantile_residuals(m, seed = 15)
    expect_length(r, 80L)
    expect_true(is.na(r[2L]))
    expect_s3_class(qq_plot(m), "ggplot")
    expect_s3_class(appraise(m, scalable = TRUE), "patchwork")
  }
})

test_that("native mixed posterior draws and PPCs reuse joint kernels and rows", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("lme4")
  set.seed(301)
  d <- data.frame(g = factor(rep(1:8, each = 10)), x = runif(80),
    v = rep(c(1, 2), 40), time = rep(1:10, 8))
  d$y <- d$x + rep(rnorm(8, sd = 2), each = 10) + rnorm(80)
  d$y[2L] <- NA_real_
  lme <- nlme::lme(y ~ x, random = ~1 | g, data = d,
    weights = nlme::varFixed(~v),
    correlation = nlme::corAR1(.3, form = ~time | g, fixed = TRUE),
    na.action = na.exclude)
  mer <- lme4::lmer(y ~ x + (1 | g), data = d, weights = 1 / v,
    na.action = na.exclude)
  for (m in list(lme, mer)) for (conditioning in c("conditional", "marginal")) {
    raw <- posterior_samples(m, n = 3L, seed = 18, conditioning = conditioning)
    expect_equal(nrow(raw), 237L)
    expect_true(all(is.finite(raw$.response)))
    expect_setequal(raw$.row, setdiff(seq_len(80), 2L))
    expect_equal(raw, diagnostic_posterior_samples.default(m, n = 3L,
      seed = 18, conditioning = conditioning))
    plot <- posterior_predictive_plot(m, n = 3L, seed = 18,
      conditioning = conditioning)
    expect_equal(plot$data[plot$data$.draw > 0L, names(raw)], raw)
    expect_match(attr(diagnostic_posterior_observed(m, conditioning),
      "diagnostic_description"), conditioning)
  }
  expect_s3_class(appraise(mer, ppcheck = TRUE, n_ppc = 2L, seed = 1), "patchwork")
})

test_that("simulation-integrated laws do not claim native canonical deviances", {
  skip_if_not_installed("lme4")
  set.seed(47)
  d <- data.frame(g = factor(rep(1:10, each = 10)), x = runif(100))
  d$y <- rpois(100, exp(d$x + rep(rnorm(10, sd = .7), each = 10)))
  m <- lme4::glmer(y ~ x + (1 | g), data = d, family = poisson())
  proxy <- mer_diagnostic_model(m, "marginal")
  expect_error(diagnostic_reference_residuals(proxy, proxy$y, "deviance"),
    "Canonical deviance")
  expect_true(all(is.finite(diagnostic_reference_residuals(proxy, proxy$y, "response"))))
  d$y <- d$x + rep(rnorm(10, sd = 2), each = 10) + rnorm(100)
  gaussian <- mer_diagnostic_model(lme4::lmer(y ~ x + (1 | g), data = d), "marginal")
  sd <- sqrt(gaussian$distribution$variance)
  log_saturated <- dnorm(gaussian$y, gaussian$y, sd, log = TRUE)
  log_fitted <- dnorm(gaussian$y, gaussian$fitted.values, sd, log = TRUE)
  expected <- sign(gaussian$y - gaussian$fitted.values) *
    sqrt(pmax(0, 2 * (log_saturated - log_fitted)))
  expect_equal(diagnostic_reference_residuals(gaussian, gaussian$y, "deviance"),
    expected, tolerance = 1e-8)
})

test_that("proxy conditioning overrides reprepare the original native model", {
  skip_if_not_installed("lme4")
  set.seed(55)
  d <- data.frame(g = factor(rep(1:8, each = 10)), x = runif(80))
  d$y <- d$x + rep(rnorm(8, sd = 2), each = 10) + rnorm(80)
  m <- lme4::lmer(y ~ x + (1 | g), data = d)
  conditional <- mer_diagnostic_model(m)
  marginal <- mer_diagnostic_model(m, "marginal")
  expect_equal(response_distribution(conditional, conditioning = "marginal")$mean,
    marginal$distribution$mean)
  expect_equal(quantile_residuals(conditional, conditioning = "marginal", seed = 12),
    quantile_residuals(marginal, seed = 12))
  expect_equal(diagnostic_data(conditional, conditioning = "marginal")$data$fitted,
    marginal$fitted.values)
})

test_that("uncorrelated PQL PPC draws use the approximate family response law", {
  skip_if_not_installed("nlme")
  set.seed(192)
  d <- data.frame(g = factor(rep(1:10, each = 10)), time = rep(1:10, 10),
    x = runif(100), off = 0.3)
  d$y <- rpois(100, exp(d$x + d$off + rep(rnorm(10, sd = .5), each = 10)))
  m <- suppressMessages(mgcv::gamm(y ~ x + offset(off), random = list(g = ~1),
    data = d, family = poisson()))
  expect_error(lme_diagnostic_components(m$lme), "working lme component")
  for (conditioning in c("conditional", "marginal")) {
    raw <- posterior_samples(m, n = 3L, seed = 10, conditioning = conditioning)
    expect_equal(nrow(raw), 300L)
    expect_true(all(raw$.response >= 0 & raw$.response == floor(raw$.response)))
    observed <- diagnostic_posterior_observed(m, conditioning)
    expect_match(attr(observed, "diagnostic_description"), "PQL")
    expect_s3_class(posterior_predictive_plot(m, n = 3L, seed = 10,
      conditioning = conditioning), "ggplot")
  }
  correlated <- suppressMessages(mgcv::gamm(y ~ x + offset(off),
    random = list(g = ~1), data = d, family = poisson(),
    correlation = nlme::corAR1(.4, form = ~time | g, fixed = TRUE)))
  expect_error(posterior_samples(correlated, n = 2L, conditioning = "conditional"),
    "working correlation")
})

test_that("native grouped-binomial posterior draws preserve the trial scale", {
  skip_if_not_installed("lme4")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  set.seed(263)
  d <- data.frame(g = factor(rep(1:12, each = 10)), x = runif(120),
    n = sample(3:10, 120, replace = TRUE))
  d$n[1L] <- 0L
  d$y <- rbinom(120, d$n,
    plogis(d$x + rep(rnorm(12, sd = .7), each = 10)))
  m <- lme4::glmer(cbind(y, n - y) ~ x + (1 | g), data = d,
    family = binomial())
  for (conditioning in c("conditional", "marginal")) {
    proxy <- mer_diagnostic_model(m, conditioning)
    expect_equal(unname(proxy$y[1L]), 0)
    expect_true(all(is.finite(quantile_residuals(proxy, seed = 19))))
    raw <- posterior_samples(m, n = 2L, seed = 19, conditioning = conditioning)
    counts <- raw$.response * d$n[raw$.row]
    expect_true(all(raw$.response >= 0 & raw$.response <= 1))
    expect_equal(counts, round(counts), tolerance = 1e-10)
    expect_s3_class(posterior_predictive_plot(m, n = 2L, seed = 19,
      conditioning = conditioning), "ggplot")
  }
})
