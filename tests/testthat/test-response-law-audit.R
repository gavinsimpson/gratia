test_that("indexed laws retain finite-ensemble rank bounds and capabilities", {
  law <- new_response_distribution(NULL, rd = function() rnorm(3))
  prepared <- prepare_marginal_response_distribution(law, n = 19L)
  combined <- combine_response_distributions(list(prepared), rep(1L, 3))
  y <- c(-.5, 0, .5)
  expect_equal(distribution_quantile_residuals(combined, y, seed = 7),
    distribution_quantile_residuals(prepared, y, seed = 7))
  expect_equal(distribution_quantile_residuals(combined, y, "quantile", seed = 7),
    distribution_quantile_residuals(prepared, y, "quantile", seed = 7))
  expect_identical(combined$calibration, "simulation ranks")
  expect_identical(combined$n_reference, prepared$n_reference)
  expect_match(combined$approximation, "Monte Carlo")
  expect_identical(combined$joint_available, prepared$joint_available)
  prepared$joint_available <- FALSE
  expect_false(combine_response_distributions(list(prepared), rep(1L, 3))$joint_available)
})

test_that("binomial left limits identify representable proportion atoms", {
  q <- 7 / 25
  for (d in list(distribution_from_parameters("binomial", .5, trials = 25),
      distribution_from_family(binomial(), .5, 25, 1))) {
    expect_equal(d$cdf_left(q), pbinom(6, 25, .5))
    expect_equal(d$cdf_left(q + 1e-14), pbinom(7, 25, .5))
    expect_equal(d$cdf_left(q - 1e-14), pbinom(6, 25, .5))
    expect_equal(d$cdf_left(q, log_p = TRUE, lower_tail = FALSE),
      pbinom(6, 25, .5, lower.tail = FALSE, log.p = TRUE))
  }
})

test_that("tiny zero-truncated count variances remain positive and accurate", {
  mu <- c(1e-20, 1e-16, 1e-12, 1e-8)
  poisson <- distribution_from_parameters("truncated_poisson", mu)
  expect_equal(poisson$variance / (mu / 2), rep(1, 4), tolerance = 1e-7)
  nb2 <- distribution_from_parameters("truncated_nbinom2", mu, .5)
  expect_equal(nb2$variance / (1.5 * mu), rep(1, 4), tolerance = 1e-7)
  nb1 <- distribution_from_parameters("truncated_nbinom1", mu, .5)
  mean_limit <- .5 / log1p(.5)
  expect_equal(nb1$variance, rep(mean_limit * (1.5 - mean_limit), 4), tolerance = 1e-7)
  expect_true(all(c(poisson$variance, nb2$variance, nb1$variance) > 0))
})

test_that("native beta observation clamps have endpoint atoms and exact moments", {
  f <- mgcv::betar(theta = .1, eps = .05)
  original <- f
  mu <- rep(.01, 20000)
  d <- distribution_from_family(f, mu, rep(1, length(mu)), 1)
  expect_identical(f, original)
  expect_equal(betar_eps(f), .05)
  expect_equal(d$cdf(.05)[1], pbeta(.05, .001, .099))
  expect_equal(d$cdf_left(.05)[1], 0)
  expect_equal(d$cdf(.95)[1], 1)
  expect_equal(d$cdf_left(.95)[1], pbeta(.95, .001, .099))
  p <- c(.001, .2, .99)
  small <- distribution_from_family(f, rep(.01, 3), 1, 1)
  expect_equal(small$qf(p), f$qf(p, rep(.01, 3), 1, 1))
  moment <- function(power) integrate(function(x) x^power * dbeta(x, .001, .099),
    .05, .95, rel.tol = 1e-10)$value +
    .05^power * pbeta(.05, .001, .099) + .95^power * pbeta(.95, .001, .099, lower.tail = FALSE)
  expect_equal(d$mean[1], moment(1))
  expect_equal(d$variance[1], moment(2) - moment(1)^2)
  y <- withr::with_seed(19, d$rd())
  u <- distribution_quantile_residuals(d, y, seed = 20)
  expect_equal(mean(u), .5, tolerance = .005)
  expect_equal(sd(u), 1 / sqrt(12), tolerance = .005)
  expect_match(d$approximation, "clamped observation approximation")
  custom <- f
  custom$cdf <- function(q, mu, wt, scale, log_p = FALSE) rep(.4, length(mu))
  expect_equal(distribution_from_family(custom, rep(.01, 3), 1, 1)$cdf(0), rep(.4, 3))
  expect_identical(fix_family_cdf(custom)$cdf, custom$cdf)
})

test_that("unrestricted GEV shapes expose moment existence and the Gumbel limit", {
  f <- mgcv::gevlss(link = list("identity", "identity", "identity"))
  for (shape in c(-.2, 0, .2, .5, .8, 1.2)) {
    d <- distribution_from_family(f, matrix(c(2, log(3), shape), nrow = 1), 1, 1)
    expect_identical(d$mean_available, shape < 1)
    expect_identical(d$variance_available, shape < .5)
    expect_true(all(is.finite(distribution_quantile_residuals(d, d$qf(.6), "quantile"))))
    if (shape >= 1) expect_true(is.na(d$mean))
    if (shape >= .5) expect_true(is.na(d$variance))
    if (shape == 0) expect_equal(d$variance, 9 * pi^2 / 6)
    if (shape != 0 && shape < .5) expect_equal(d$variance,
      9 * (gamma(1 - 2 * shape) - gamma(1 - shape)^2) / shape^2)
  }
})

test_that("weighted factor-binomial response laws retain Bernoulli observations", {
  dat <- data.frame(y = factor(rep(c("no", "yes"), 10)), x = seq_len(20))
  for (model in list(glm(y ~ 1, data = dat, weights = rep(2, 20), family = binomial()),
      mgcv::gam(y ~ 1, data = dat, weights = rep(2, 20), family = binomial()))) {
    d <- response_distribution(model)
    expect_equal(d$cdf(.5), unname(pbinom(0, 1, fitted(model))))
    expect_true(all(d$rd() %in% c(0, 1)))
    expect_true(all(d$qf(rep(c(.2, .8), 10)) %in% c(0, 1)))
    expect_equal(unname(model$prior.weights), rep(2, 20))
  }
})


test_that("native general-family residual callbacks use their true response moments", {
  params <- cbind(c(0, 2), 0, c(0, .7), 0)
  f <- mgcv::shash()
  d <- distribution_from_family(f, params, 1, 1)
  mean_oracle <- vapply(seq_len(2), function(i) integrate(function(z)
    (params[i, 1] + sinh(asinh(z) + params[i, 3])) * dnorm(z), -10, 10)$value,
    numeric(1))
  y <- c(.2, 2.3)
  expect_equal(d$mean, mean_oracle, tolerance = 1e-8)
  expect_equal(d$diagnostic_residuals(y, "response"), y - mean_oracle)
  native <- f$residuals(list(y = y, fitted = params), "deviance")
  expect_equal(abs(d$diagnostic_residuals(y, "deviance")), abs(native))
  expect_equal(sign(d$diagnostic_residuals(y, "deviance")), sign(y - mean_oracle))
  expect_error(d$diagnostic_residuals(y, "pearson"), "unavailable")
  custom <- f
  body(custom$residuals) <- quote(rep(17, length(object$y)))
  expect_null(distribution_from_family(custom, params, 1, 1)$diagnostic_residuals)
  gev <- distribution_from_family(mgcv::gevlss(), cbind(c(0, 1), 0, 0), 1, 1)
  y <- c(.2, 1.3)
  expect_equal(gev$diagnostic_residuals(y, "response"), y - gev$mean)
  expect_equal(gev$diagnostic_residuals(y, "pearson"), (y - gev$mean) / sqrt(pi^2 / 6))
  z <- y - c(0, 1)
  expect_equal(gev$diagnostic_residuals(y, "deviance"),
    sqrt(z + exp(-z) - 1) * sign(y - gev$mean))
  for (shape in c(-.2, .2)) {
    f <- mgcv::gevlss()
    parameters <- cbind(c(0, 1), 0, shape)
    d <- distribution_from_family(f, parameters, 1, 1)
    y <- d$qf(c(.3, .8))
    expect_equal(abs(d$diagnostic_residuals(y, "deviance")),
      abs(f$residuals(list(y = y, fitted = parameters), "deviance")))
    expect_equal(d$diagnostic_residuals(y, "pearson"),
      (y - d$mean) / sqrt((gamma(1 - 2 * shape) - gamma(1 - shape)^2) / shape^2))
  }
  boundary <- distribution_from_family(mgcv::gevlss(), matrix(c(0, 0, -1), 1), 1, 1)
  expect_equal(boundary$diagnostic_residuals(c(.5), "deviance"), sqrt(.5))
})

test_that("native negative-binomial GLM families expose one shared response law", {
  skip_if_not_installed("MASS")
  f <- MASS::negative.binomial(theta = 2)
  original <- f
  mu <- c(.5, 2, 5)
  d <- distribution_from_family(f, mu, 1, 1)
  expect_identical(f, original)
  expect_equal(d$cdf(1), pnbinom(1, mu = mu, size = 2))
  expect_equal(d$qf(.8), qnbinom(.8, mu = mu, size = 2))
  expect_true(all(d$rd() >= 0))
  expect_equal(fix_family_cdf(f)$cdf(1, mu, 1, 1), pnbinom(1, mu = mu, size = 2))
  expect_equal(fix_family_qf(f)$qf(.8, mu, 1, 1), qnbinom(.8, mu = mu, size = 2))
  expect_true(is.function(fix_family_rd(f)$rd))
  dat <- withr::with_seed(21, data.frame(x = runif(60), y = rnbinom(60, mu = 2, size = 2)))
  model <- MASS::glm.nb(y ~ x, data = dat)
  expect_true(all(is.finite(quantile_residuals(model, "quantile", seed = 2))))
})


test_that("zero-trial glmmTMB rows have consistent native and marginal residuals", {
  skip_if_not_installed("glmmTMB")
  dat <- withr::with_seed(44, data.frame(x = runif(30), trials = rep(10, 30)))
  dat$trials[c(3, 12)] <- 0
  dat$success <- withr::with_seed(45, rbinom(30, dat$trials, plogis(dat$x)))
  dat$failure <- dat$trials - dat$success
  model <- glmmTMB::glmmTMB(cbind(success, failure) ~ x, data = dat, family = binomial())
  withr::local_options(gratia.diagnostics.marginal_draws = 49L)
  for (conditioning in c("conditional", "marginal")) {
    for (type in c("response", "pearson")) {
      proxy <- glmmtmb_diagnostic_model(model, type, conditioning = conditioning)
      expect_equal(proxy$residuals[c(3, 12)], c(0, 0))
      expect_true(all(is.finite(proxy$residuals)))
      draw <- response_distribution(proxy)$rd()
      expect_equal(diagnostic_reference_residuals(proxy, draw, type)[c(3, 12)], c(0, 0))
    }
  }
})


test_that("grouped beta observed and reference laws share endpoint calibration", {
  skip_if_not(exists("gfam", asNamespace("mgcv"), inherits = FALSE))
  withr::local_seed(51)
  n <- 60
  f <- mgcv::betar(theta = .1, eps = .05)
  dat <- data.frame(y = c(f$rd(rep(.1, n), 1, 1), rnorm(n)), fi = rep(1:2, each = n))
  model <- mgcv::gam(cbind(y, fi) ~ factor(fi), data = dat,
    family = mgcv::gfam(list(mgcv::betar(theta = -.1, eps = .05), gaussian())),
    method = "REML")
  distribution <- response_distribution(model)
  context <- gfam_diagnostic_context(model)
  expect_equal(quantile_residuals(model, "pit", seed = 2),
    distribution_quantile_residuals(distribution, context$y, "pit", seed = 2))
  expect_equal(quantile_residuals(model, "quantile", seed = 2),
    distribution_quantile_residuals(distribution, context$y, "quantile", seed = 2))
  expect_match(distribution$approximation, "epsilon-clamped")
})


test_that("zero-mean NB1 laws remain degenerate through all capabilities", {
  d <- distribution_from_parameters("nbinom1", c(0, 2), c(.5, .5))
  expect_equal(d$rd()[1], 0)
  expect_equal(d$qf(.8)[1], 0)
  expect_equal(d$cdf(0)[1], 1)
  expect_equal(d$cdf(-1)[1], 0)
  expect_equal(d$cdf_left(0)[1], 0)
  expect_equal(d$mean[1], 0)
  expect_equal(d$variance[1], 0)
  expect_true(is.finite(distribution_quantile_residuals(d, c(0, 2), "quantile", seed = 1)[1]))
})


test_that("normalized native inverse CDFs retain both extreme log tails", {
  gaussian <- distribution_from_family(gaussian(), c(0, 1), c(1, 4), 4)
  gaulss <- distribution_from_family(mgcv::gaulss(), cbind(c(0, 1), c(1, 2)), 1, 1)
  gamma <- distribution_from_family(Gamma(), c(2, 3), c(1, 2), .5)
  gammals <- distribution_from_family(mgcv::gammals(), cbind(c(2, 3), log(.5)), 1, 1)
  poisson <- distribution_from_family(poisson(), c(2, 3), 1, 1)
  scat <- distribution_from_family(mgcv::scat(theta = c(4, 2)), c(0, 1), 1, 1)
  cnorm <- distribution_from_family(mgcv::cnorm(theta = 2), c(0, 1), 1, 1)
  for (tail in c(TRUE, FALSE)) {
    expect_equal(gaussian$qf(-1000, log_p = TRUE, lower_tail = tail),
      qnorm(-1000, c(0, 1), c(2, 1), log.p = TRUE, lower.tail = tail))
    expect_equal(gaulss$qf(-1000, log_p = TRUE, lower_tail = tail),
      qnorm(-1000, c(0, 1), c(1, .5), log.p = TRUE, lower.tail = tail))
    expect_equal(gamma$qf(-1000, log_p = TRUE, lower_tail = tail),
      qgamma(-1000, shape = c(2, 4), scale = c(1, .75), log.p = TRUE, lower.tail = tail))
    expect_equal(gammals$qf(-1000, log_p = TRUE, lower_tail = tail),
      qgamma(-1000, shape = 2, scale = c(1, 1.5), log.p = TRUE, lower.tail = tail))
    expect_equal(poisson$qf(-1000, log_p = TRUE, lower_tail = tail),
      qpois(-1000, c(2, 3), log.p = TRUE, lower.tail = tail))
    expect_equal(scat$qf(-1000, log_p = TRUE, lower_tail = tail),
      qt(-1000, 4, log.p = TRUE, lower.tail = tail) * 2 + c(0, 1))
    expect_equal(cnorm$qf(-1000, log_p = TRUE, lower_tail = tail),
      qnorm(-1000, c(0, 1), 2, log.p = TRUE, lower.tail = tail))
  }
  gum <- distribution_from_family(mgcv::gumbls(), cbind(c(0, 1), 0), 1, 1)
  expect_equal(gum$qf(-1000, log_p = TRUE, lower_tail = FALSE),
    c(0, 1) + 1000 - 0.577215664901533)
  for (shape in c(-.2, 0, .2)) {
    gev <- distribution_from_family(mgcv::gevlss(), cbind(c(0, 1), 0, shape), 1, 1)
    expect_equal(gev$qf(-1000, log_p = TRUE), c(0, 1) +
      if (shape == 0) -log(1000) else expm1(-shape * log(1000)) / shape)
    expect_equal(gev$qf(-1000, log_p = TRUE, lower_tail = FALSE), c(0, 1) +
      if (shape == 0) 1000 else expm1(shape * 1000) / shape)
  }
  shash <- distribution_from_family(mgcv::shash(), cbind(c(0, 1), 0, 0, 0), 1, 1)
  expect_equal(shash$qf(-1000, log_p = TRUE, lower_tail = FALSE),
    qnorm(-1000, log.p = TRUE, lower.tail = FALSE) + c(0, 1))
  inverse <- distribution_from_family(inverse.gaussian(), c(1, 2), 1, .5)
  for (tail in c(TRUE, FALSE)) {
    q <- inverse$qf(-1000, log_p = TRUE, lower_tail = tail)
    expect_true(all(is.finite(q)))
    expect_equal(inverse$cdf(q, log_p = TRUE, lower_tail = tail), rep(-1000, 2), tolerance = 1e-8)
  }
})

test_that("native quantile normalization preserves original and custom closures", {
  f <- mgcv::fix.family.qf(gaussian())
  original <- f$qf
  d <- distribution_from_family(f, 0, 1, 1)
  expect_identical(f$qf, original)
  expect_equal(d$qf(-1000, log_p = TRUE), qnorm(-1000, log.p = TRUE))
  expect_equal(d$qf(-Inf, log_p = TRUE), -Inf)
  expect_equal(d$qf(0, log_p = TRUE), Inf)
  expect_equal(d$qf(-Inf, log_p = TRUE, lower_tail = FALSE), Inf)
  expect_equal(d$qf(0, log_p = TRUE, lower_tail = FALSE), -Inf)
  custom <- f
  custom$qf <- function(p, mu, wt, scale) qnorm(p)
  d <- distribution_from_family(custom, 0, 1, 1)
  expect_identical(fix_family_qf(custom)$qf, custom$qf)
  expect_error(d$qf(-1000, log_p = TRUE), "Extreme-tail quantiles are unsupported")
  expect_error(d$qf(-1000, log_p = TRUE, lower_tail = FALSE), "Extreme-tail quantiles are unsupported")
  expect_error(d$qf(1e-100, lower_tail = FALSE), "Extreme-tail quantiles are unsupported")
  expect_equal(d$qf(-Inf, log_p = TRUE), -Inf)
  expect_equal(d$qf(-Inf, log_p = TRUE, lower_tail = FALSE), Inf)
  expect_equal(d$qf(.25), qnorm(.25))
  beta <- mgcv::betar(theta = 3, eps = .05)
  original <- beta$qf
  d <- distribution_from_family(beta, c(.2, .8), 1, 1)
  expect_identical(beta$qf, original)
  expect_equal(d$qf(-1000, log_p = TRUE), c(.05, .05))
  expect_equal(d$qf(-1000, log_p = TRUE, lower_tail = FALSE), c(.95, .95))
})

test_that("unsupported legacy Tweedie extreme inverses fail without false infinity", {
  d <- distribution_from_parameters("tweedie", 2, .5, power = 1.5)
  expect_error(d$qf(-1000, log_p = TRUE, lower_tail = FALSE),
    "Extreme-tail quantiles are unsupported")
  expect_equal(d$qf(-Inf, log_p = TRUE, lower_tail = FALSE), Inf)
  gamma <- distribution_from_parameters("tweedie", 2, .5, power = 2)
  expect_equal(gamma$qf(-1000, log_p = TRUE, lower_tail = FALSE),
    qgamma(-1000, shape = 2, scale = 1, log.p = TRUE, lower.tail = FALSE))
})


test_that("negative native shash pseudo-deviance contrasts are explicitly unsupported", {
  f <- mgcv::shash()
  original <- f
  parameters <- matrix(c(0, log(.1), 0, 0), nrow = 1)
  distribution <- distribution_from_family(f, parameters, 1, 1)
  expect_gt(dnorm(0, sd = .1, log = TRUE), 0)
  # The native hook currently hides the negative contrast behind pmax(0,...).
  expect_equal(f$residuals(list(y = 0, fitted = parameters), "deviance"), 0)
  expect_error(distribution$diagnostic_residuals(0, "deviance"),
    "negative contrast")
  replicated <- withr::with_seed(2, distribution$rd())
  expect_gt(dnorm(replicated, sd = .1, log = TRUE), 0)
  expect_error(distribution$diagnostic_residuals(replicated, "deviance"),
    "zero-log-density reference")
  expect_equal(distribution$diagnostic_residuals(0, "response"), 0)
  expect_true(all(is.finite(distribution_quantile_residuals(distribution, replicated, "pit"))))
  expect_true(all(is.finite(distribution_quantile_residuals(distribution, replicated, "quantile"))))
  # Locally nonnegative native contrasts retain their documented magnitude.
  y <- .3
  expect_equal(abs(distribution$diagnostic_residuals(y, "deviance")),
    abs(f$residuals(list(y = y, fitted = parameters), "deviance")))
  expect_identical(f, original)
  custom <- f
  custom$residuals <- function(object, type = c("deviance", "response")) rep(17, length(object$y))
  augmented <- distribution_from_family(custom, parameters, 1, 1)
  expect_null(augmented$diagnostic_residuals)
  expect_identical(augmented$family$residuals, custom$residuals)
  expect_equal(augmented$family$residuals(list(y = 0, fitted = parameters), "deviance"), 17)
})


test_that("small-scale fitted shash diagnostics reject invalid native deviance only", {
  dat <- withr::with_seed(62, data.frame(y = rnorm(100, sd = .1)))
  model <- mgcv::gam(list(y ~ 1, ~1, ~1, ~1), data = dat,
    family = mgcv::shash(), method = "REML")
  distribution <- response_distribution(model)
  expect_true(all(is.finite(distribution$mean)))
  expect_error(diagnostic_residuals(model, "deviance"), "negative contrast")
  replicated <- distribution$qf(rep(.5, nrow(dat)))
  expect_error(diagnostic_reference_residuals(model, replicated, "deviance"),
    "zero-log-density reference")
  expect_error(qq_plot(model, type = "deviance", n_simulate = 3), "negative contrast")
  expect_error(appraise(model, type = "deviance"), "negative contrast")
  expect_true(all(is.finite(diagnostic_residuals(model, "response"))))
  expect_equal(diagnostic_reference_residuals(model, replicated, "response"),
    replicated - distribution$mean)
  expect_true(all(is.finite(quantile_residuals(model, "pit"))))
  expect_true(all(is.finite(quantile_residuals(model, "quantile"))))
  expect_s3_class(qq_plot(model, type = "quantile"), "ggplot")
  expect_s3_class(appraise(model, type = "quantile"), "patchwork")
})
