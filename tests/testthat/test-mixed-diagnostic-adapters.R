test_that("gamm conditional diagnostics use the full mixed-model fit", {
  skip_if_not_installed("nlme")
  set.seed(190)
  d <- data.frame(x = runif(80), g = factor(rep(1:8, each = 10)))
  d$y <- sin(d$x) + rep(rnorm(8, sd = 2), each = 10) + rnorm(80)
  m <- mgcv::gamm(y ~ s(x, k = 5), random = list(g = ~1), data = d)
  p <- mixed_diagnostic_model(m)
  expect_equal(p$fitted.values, as.vector(fitted(m$lme)))
  expect_equal(p$y, d$y)
  expect_equal(p$distribution$cdf(d$y),
    pnorm(d$y, fitted(m$lme), m$lme$sigma))
  expect_equal(mixed_diagnostic_model(m, "gam_component"), m$gam)
  marginal <- mixed_diagnostic_model(m, "marginal")
  expect_equal(marginal$fitted.values, as.vector(fitted(m$lme, level = 0)))
  expect_true(all(marginal$distribution$variance > p$distribution$variance))
})

test_that("Gaussian gamm draws retain correlated errors in fitting order", {
  skip_if_not_installed("nlme")
  set.seed(71)
  d <- data.frame(x = runif(80), g = factor(rep(1:8, 10)),
    time = rep(seq_len(10), each = 8))
  # Interleave groups to exercise the native lme ordering map.
  error <- replicate(8, as.numeric(arima.sim(list(ar = 0.7), n = 10)))
  d$y <- sin(d$x) + as.vector(t(error))
  m <- mgcv::gamm(y ~ s(x, k = 5), random = list(g = ~1),
    correlation = nlme::corAR1(form = ~time | g), data = d)
  p <- mixed_diagnostic_model(m)
  draws <- replicate(3000, p$distribution$rd() - p$fitted.values)
  i <- which(d$g == 1)
  expected <- nlme::corMatrix(m$lme$modelStruct$corStruct)[[1]] * m$lme$sigma^2
  expect_equal(unname(cov(t(draws[i, ]))), unname(expected), tolerance = 0.12)
  expect_lt(abs(cor(draws[1, ], draws[2, ])), 0.08)
})

test_that("gamm4 preserves component default and exposes full conditioning", {
  skip_if_not_installed("gamm4", minimum_version = "0.2-7")
  set.seed(107)
  d <- data.frame(x = runif(80), g = factor(rep(1:8, each = 10)))
  d$y <- sin(d$x) + rep(rnorm(8, sd = 2), each = 10) + rnorm(80)
  m <- gamm4::gamm4(y ~ s(x, k = 5), random = ~(1 | g), data = d)
  expect_equal(mixed_diagnostic_model(m), gamm4_gam(m))
  p <- mixed_diagnostic_model(m, "conditional")
  expect_equal(p$fitted.values, as.vector(fitted(m$mer)))
  marginal <- mixed_diagnostic_model(m, "marginal")
  expect_equal(marginal$fitted.values,
    as.vector(predict(m$mer, re.form = NA)))
  expect_true(all(marginal$distribution$variance > p$distribution$variance))
  expect_equal(p$distribution$cdf(d$y),
    pnorm(d$y, fitted(m$mer), sigma(m$mer)))
})

test_that("Gaussian gamm posterior effects retain native joint uncertainty", {
  skip_if_not_installed("nlme")
  set.seed(190)
  d <- data.frame(x = runif(80), g = factor(rep(1:8, each = 10)))
  d$y <- sin(d$x) + rep(rnorm(8, sd = 2), each = 10) + rnorm(80)
  m <- mgcv::gamm(y ~ s(x, k = 5), random = list(g = ~1), data = d)
  joint <- lme_joint_diagnostic_draws(m$lme, n = 5000)
  k <- length(nlme::fixef(m$lme))
  empirical <- cov(t(as.matrix(joint$perturbation[seq_len(k), ])))
  expect_equal(unname(empirical), unname(vcov(m$lme)), tolerance = 0.04)
  samples <- diagnostic_posterior_samples(m, n = 3L, seed = 102)
  expect_equal(nrow(samples), 240L)
  expect_equal(samples, diagnostic_posterior_samples(m, n = 3L, seed = 102))
  expect_s3_class(posterior_predictive_plot(m, n = 3L, seed = 10), "ggplot")
})

test_that("gamm PQL conditional calibration uses full effects and offsets", {
  skip_if_not_installed("nlme")
  set.seed(111)
  d <- data.frame(x = runif(100), g = factor(rep(1:10, each = 10)), off = 0.2)
  d$y <- rpois(100, exp(d$x + rep(rnorm(10, sd = 0.4), each = 10) + d$off))
  m <- suppressMessages(mgcv::gamm(y ~ s(x, k = 5) + offset(off),
    random = list(g = ~1), data = d, family = poisson()))
  p <- mixed_diagnostic_model(m)
  expected <- exp(as.vector(fitted(m$lme)) + d$off)
  expect_equal(p$fitted.values, expected)
  expect_equal(p$distribution$cdf(d$y), ppois(d$y, expected))
  marginal <- mixed_diagnostic_model(m, "marginal")
  expect_match(marginal$distribution$approximation, "PQL")
  expect_true(is.function(marginal$distribution$cdf))
})

test_that("full gamm4 simulations use the diagnostic response scale and rows", {
  skip_if_not_installed("gamm4", minimum_version = "0.2-7")
  set.seed(5)
  d <- data.frame(x = runif(80), g = factor(rep(1:8, each = 10)))
  d$y <- rnorm(80) + as.numeric(d$g)
  d$y[2] <- NA_real_
  m <- gamm4::gamm4(y ~ s(x, k = 5), random = ~(1 | g), data = d,
    na.action = na.exclude)
  r <- quantile_residuals(m, conditioning = "conditional", seed = 1)
  expect_length(r, 80L)
  expect_true(is.na(r[2L]))
  samples <- simulate(m, nsim = 2, seed = 1, conditioning = "conditional")
  expect_equal(dim(samples), c(80L, 2L))
  expect_true(all(is.na(samples[2L, ])))
  expect_s3_class(samples, "simulate_gratia")
  expect_error(simulate(m, conditioning = "conditional", newdata = d),
    "fitting observations")
})

test_that("explicit gamm4 posterior modes share the PPC sampler and rows", {
  skip_if_not_installed("gamm4", minimum_version = "0.2-7")
  set.seed(191)
  d <- data.frame(x = runif(80), g = factor(rep(1:8, each = 10)))
  d$y <- sin(d$x) + as.numeric(d$g) + rnorm(80)
  d$y[2L] <- NA_real_
  m <- gamm4::gamm4(y ~ s(x, k = 5), random = ~(1 | g), data = d,
    na.action = na.exclude)
  for (conditioning in c("conditional", "marginal")) {
    raw <- posterior_samples(m, n = 3L, seed = 42,
      conditioning = conditioning)
    expect_equal(raw, diagnostic_posterior_samples.default(m, n = 3L,
      seed = 42, conditioning = conditioning))
    expect_setequal(raw$.row, setdiff(seq_len(80), 2L))
    plot <- posterior_predictive_plot(m, n = 3L, seed = 42,
      conditioning = conditioning)
    plotted <- plot$data[plot$data$.draw > 0L, names(raw)]
    expect_equal(plotted, raw)
    expect_error(posterior_samples(m, n = 2L, seed = 42,
      conditioning = conditioning, method = "mh"), "method = 'gaussian'")
  }
})

test_that("nonlinear gamm4 marginal simulation stays separate from residual moments", {
  skip_if_not_installed("gamm4", minimum_version = "0.2-7")
  withr::local_seed(123)
  dat <- data.frame(x = runif(160), g = factor(rep(1:8, each = 20)))
  dat$y <- rpois(160, exp(dat$x + rep(rnorm(8, sd = .6), each = 20)))
  model <- gamm4::gamm4(y ~ x, random = ~(1 | g), data = dat, family = poisson())
  expect_s3_class(qq_plot(model, conditioning = "marginal", type = "response"),
    "ggplot")
  expect_s3_class(simulate(model, nsim = 2, seed = 1, conditioning = "marginal"),
    "simulate_gratia")
  expect_s3_class(posterior_predictive_plot(model, n = 2, seed = 1,
    conditioning = "marginal"), "ggplot")
})
