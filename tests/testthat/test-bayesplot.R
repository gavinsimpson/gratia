bayesplot_test_prepare <- function(model, type = "dens_overlay", prefix = "ppc",
    n = 3L, seed = 42, group = NULL, x = NULL, response = NULL,
    conditioning = NULL, sampling_args = list(), dots = list()) {
  bayesplot_prepare(model, type, prefix, n, seed, group, x, response,
    conditioning, sampling_args, dots)
}

test_that("bayesplot receives fitting responses and complete replicated datasets", {
  skip_if_not_installed("bayesplot")
  p <- bayesplot_test_prepare(m_tiny_eg1)
  sims <- diagnostic_posterior_samples(m_tiny_eg1, n = 3, seed = 42)
  expect_equal(p$args$y, m_tiny_eg1$y, ignore_attr = TRUE)
  expect_equal(p$args$yrep, matrix(sims$.response[order(sims$.draw, sims$.row)],
    nrow = 3, byrow = TRUE))
  expect_s3_class(pp_check.gam(m_tiny_eg1, n = 3, seed = 42), "ggplot")
  expect_s3_class(pp_check.bam(b_pois, type = "bars", n = 3, seed = 42), "ggplot")
  pred <- bayesplot_test_prepare(m_tiny_eg1, prefix = "ppd")
  expect_false("y" %in% names(pred$args))
  expect_equal(pred$args$ypred, p$args$yrep)
  expect_s3_class(pp_check.gam(m_tiny_eg1, prefix = "ppd", n = 3, seed = 42), "ggplot")
  stat <- pp_check.gam(m_tiny_eg1, type = "stat", n = 3, seed = 42, stat = "mean")
  expect_s3_class(stat, "ggplot")
})

test_that("binomial checks use success counts and fitted trial counts", {
  skip_if_not_installed("bayesplot")
  dat <- data.frame(x = seq(-1, 1, length.out = 50), trials = rep(10L, 50))
  dat$success <- rep(1:10, 5)
  fit <- mgcv::gam(cbind(success, trials - success) ~ x, data = dat,
    family = stats::binomial(), method = "REML")
  p <- bayesplot_test_prepare(fit, type = "bars")
  expect_equal(p$args$y, as.numeric(dat$success))
  expect_true(all(abs(p$args$yrep - round(p$args$yrep)) < 1e-10))
  expect_true(all(p$args$yrep >= 0 & p$args$yrep <= 10))
  weighted <- mgcv::gam(cbind(success, trials - success) ~ x, data = dat,
    weights = rep(3, 50), family = stats::binomial(), method = "REML")
  weighted_check <- bayesplot_test_prepare(weighted, type = "bars")
  expect_equal(weighted_check$args$y, as.numeric(dat$success))
  expect_true(all(weighted_check$args$yrep >= 0 & weighted_check$args$yrep <= 10))
  proportion <- mgcv::gam(I(success / trials) ~ x, data = dat, weights = trials,
    family = stats::binomial(), method = "REML")
  expect_equal(bayesplot_test_prepare(proportion)$args$y, as.numeric(dat$success))
  dat$outcome <- factor(rep(c("no", "yes"), 25))
  factor_fit <- mgcv::gam(outcome ~ x, data = dat, weights = trials,
    family = stats::binomial(), method = "REML")
  factor_check <- bayesplot_test_prepare(factor_fit, type = "bars")
  expect_equal(factor_check$args$y, as.numeric(dat$outcome != "no"))
  expect_true(all(factor_check$args$yrep %in% 0:1))
})

test_that("bayesplot removes excluded rows from all inputs together", {
  skip_if_not_installed("bayesplot")
  dat <- data.frame(x = seq(-1, 1, length.out = 30),
    y = sin(seq(-1, 1, length.out = 30)), group = factor(rep(1:2, 15)))
  dat$x[c(3, 27)] <- NA_real_
  fit <- mgcv::gam(y ~ x + group, data = dat, subset = seq_len(28),
    na.action = stats::na.exclude, method = "REML")
  p <- bayesplot_test_prepare(fit, type = "stat_grouped", group = "group")
  expect_equal(p$args$y, dat$y[setdiff(1:28, c(3, 27))])
  expect_equal(p$args$group, droplevels(dat$group[setdiff(1:28, c(3, 27))]))
  expect_identical(ncol(p$args$yrep), length(p$args$y))
  error <- bayesplot_test_prepare(fit, type = "error_binned", x = "x")
  expect_equal(error$args$x, dat$x[setdiff(1:28, c(3, 27))])
  expected <- fitted_samples(fit, n = 3, seed = 42)
  expected <- expected[is.finite(expected$.fitted), ]
  expect_equal(error$args$yrep,
    matrix(expected$.fitted[order(expected$.draw, expected$.row)], 3, byrow = TRUE))
  with_vector <- bayesplot_test_prepare(fit, type = "error_binned", x = dat$x[1:28])
  expect_equal(with_vector$args$x, error$args$x)
})

test_that("full mixed binomial checks retain actual trials with frequency weights", {
  skip_if_not_installed("bayesplot")
  dat <- withr::with_seed(13, {
    out <- data.frame(x = stats::runif(80), id = factor(rep(1:8, each = 10)),
      trials = rep(5L, 80), frequency = rep(3L, 80))
    out$success <- stats::rbinom(80, size = 5, prob = stats::plogis(out$x))
    out
  })
  fit <- mgcv::gamm(cbind(success, trials - success) ~ s(x, k = 4),
    random = list(id = ~1), data = dat, family = stats::binomial(),
    weights = frequency, verbosePQL = FALSE)
  mixed <- bayesplot_test_prepare(fit, type = "bars", conditioning = "marginal")
  expect_equal(mixed$args$y, as.numeric(dat$success))
  expect_true(all(abs(mixed$args$yrep - round(mixed$args$yrep)) < 1e-10))
  expect_true(all(mixed$args$yrep >= 0 & mixed$args$yrep <= 5))
  skip_if_not_installed("gamm4")
  fit4 <- gamm4::gamm4(cbind(success, trials - success) ~ s(x, k = 4),
    random = ~ (1 | id), data = dat, family = stats::binomial(), weights = frequency)
  mixed4 <- bayesplot_test_prepare(fit4, type = "bars", conditioning = "marginal")
  expect_equal(mixed4$args$y, as.numeric(dat$success))
  expect_true(all(abs(mixed4$args$yrep - round(mixed4$args$yrep)) < 1e-10))
  expect_true(all(mixed4$args$yrep >= 0 & mixed4$args$yrep <= 5))
})

test_that("multivariate and grouped checks require an explicit response", {
  skip_if_not_installed("bayesplot")
  expect_error(bayesplot_test_prepare(m_mvn), "Select a 'response'")
  p <- bayesplot_test_prepare(m_mvn, response = "response2")
  expect_equal(p$args$y, as.numeric(m_mvn$y[, 2L]))
  expect_equal(bayesplot_test_prepare(m_mvn, response = 2)$args, p$args)
  if (exists("gfam_model")) {
    expect_error(bayesplot_test_prepare(gfam_model), "Select a 'response'")
    grouped <- bayesplot_test_prepare(gfam_model, response = "component1")
    i <- gfam_data$family_id == 1L
    expect_equal(grouped$args$y, gfam_data$y[i] * gfam_data$weight[i])
    expect_true(all(abs(grouped$args$yrep - round(grouped$args$yrep)) < 1e-10))
  }
})

test_that("posterior check arguments fail clearly instead of changing the sampler", {
  skip_if_not_installed("bayesplot")
  expect_error(bayesplot_test_prepare(m_tiny_eg1, type = "not_a_plot"), "Available ppc types")
  expect_error(bayesplot_test_prepare(m_tiny_eg1, n = 0), "positive integer")
  expect_error(bayesplot_test_prepare(m_tiny_eg1, type = "stat_grouped"), "'group' is required")
  expect_error(bayesplot_test_prepare(m_tiny_eg1, type = "error_binned", x = "missing"), "absent")
  expect_error(bayesplot_test_prepare(m_tiny_eg1, sampling_args = list(data = tiny_eg1)), "fitting observations")
  expect_error(bayesplot_test_prepare(m_tiny_eg1, sampling_args = list(weights = 2)), "trial counts")
  expect_error(bayesplot_test_prepare(m_tiny_eg1, dots = list(yrep = matrix(1))), "prepared from")
  expect_error(bayesplot_test_prepare(m_tiny_eg1, response = "response2"), "Unknown 'response'")
  draws <- post_draws(m_tiny_eg1, n = 2, seed = 1)
  expect_error(bayesplot_test_prepare(m_tiny_eg1,
    sampling_args = list(method = "user", draws = draws)), "'n' must equal")
  inferred <- bayesplot_test_prepare(m_tiny_eg1, n = NULL,
    sampling_args = list(method = "user", draws = draws))
  expect_equal(nrow(inferred$args$yrep), 2L)
})

test_that("grouped selections retain component numbers and evaluate only their means", {
  skip_if_not_installed("bayesplot")
  dat <- withr::with_seed(278, {
    d <- data.frame(x = seq(-1, 1, length.out = 80),
      component = rep(c(2, 1), 40))
    d$y <- ifelse(d$component == 1, rnorm(80), rpois(80, exp(d$x)))
    d
  })
  normal <- gaussian()
  normal$gratia_diagnostics <- function(mu, wt, scale, y, eta)
    list(mean = NULL, mean_available = FALSE,
      moment_reason = "The unselected component has no analytic mean")
  fit <- mgcv::gam(cbind(y, component) ~ x, data = dat,
    family = mgcv::gfam(list(normal, poisson())), method = "REML")
  p <- bayesplot_test_prepare(fit, response = 1)
  expect_identical(p$rows, which(dat$component == 1L))
  expect_identical(p$rows,
    bayesplot_test_prepare(fit, response = "component1")$rows)
  p <- bayesplot_test_prepare(fit, response = 2, type = "error_binned", n = 1,
    sampling_args = list(method = "user", draws = matrix(coef(fit), 1L)))
  expect_equal(as.numeric(p$args$yrep),
    as.numeric(fitted(fit)[dat$component == 2L]))
  expect_error(bayesplot_test_prepare(gfam_ordinal_model, response = "component1",
    type = "error_binned"), "categorical")
  ordinal <- gfam_ordinal_model
  p <- bayesplot_test_prepare(ordinal, response = "component2",
    type = "error_binned", n = 1,
    sampling_args = list(method = "user", draws = matrix(coef(ordinal), 1L)))
  selected <- attr(ordinal$family$getfl(), "fi") == 2L
  expect_equal(as.numeric(p$args$yrep), exp(ordinal$linear.predictors[selected]))
})

test_that("Gumbel binned errors use the response mean and preserve custom means", {
  skip_if_not_installed("bayesplot")
  draws <- matrix(stats::coef(m_gumbls), nrow = 1L)
  p <- bayesplot_test_prepare(m_gumbls, type = "error_binned", n = 1,
    sampling_args = list(method = "user", draws = draws))
  eta <- matrix(m_gumbls$linear.predictors, ncol = 2L)
  logscale <- stats::family(m_gumbls)$linfo[[2L]]$linkinv(eta[, 2L])
  # Euler's constant is the mean of a standard Gumbel variate.
  expected <- eta[, 1L] + 0.5772156649015329 * exp(logscale)
  expect_equal(as.numeric(p$args$yrep), expected)
  custom <- m_gumbls
  custom$family$gratia_diagnostics <- function(mu, wt, scale, y, eta)
    list(mean = rep(3, nrow(mu)))
  custom_plot <- bayesplot_test_prepare(custom, type = "error_binned", n = 1,
    sampling_args = list(method = "user", draws = draws))
  expect_equal(as.numeric(custom_plot$args$yrep), rep(3, length(expected)))
})

test_that("hurdle Poisson binned errors retain a scalar response", {
  skip_if_not_installed("bayesplot")
  dat <- withr::with_seed(123, {
    d <- data.frame(x = seq(-1, 1, length.out = 120))
    d$y <- rpois(nrow(d), exp(d$x))
    d
  })
  fit <- mgcv::gam(y ~ x, data = dat, family = mgcv::ziP(), method = "REML")
  draws <- matrix(coef(fit), nrow = 1L)
  p <- bayesplot_test_prepare(fit, type = "error_binned", n = 1,
    sampling_args = list(method = "user", draws = draws))
  eta <- as.numeric(fit$linear.predictors)
  theta <- fit$family$getTheta(TRUE)
  lambda <- exp(eta)
  expected <- -expm1(-exp(theta[1L] + theta[2L] * eta)) *
    lambda / -expm1(-lambda)
  expect_equal(as.numeric(p$args$yrep), expected)
  expect_s3_class(bayesplot::pp_check(fit, type = "error_binned", n = 2, seed = 42),
    "ggplot")
})

test_that("posterior check seeds are reproducible without changing RNG state", {
  skip_if_not_installed("bayesplot")
  withr::local_seed(101)
  before <- .Random.seed
  p <- bayesplot_test_prepare(m_tiny_eg1)
  expect_identical(.Random.seed, before)
  expect_equal(bayesplot_test_prepare(m_tiny_eg1)$args, p$args)
  pp_check.gam(m_tiny_eg1, n = 3, seed = 42)
  expect_identical(.Random.seed, before)
})

test_that("leave-one-out checks share coefficient draws with likelihood weights", {
  skip_if_not_installed("bayesplot")
  skip_if_not_installed("loo")
  draws <- post_draws(m_tiny_eg1, n = 100, seed = 11, method = "gaussian")
  options <- list(method = "user", draws = draws, r_eff = 1)
  # Efficiency is a likelihood option; the user draws themselves are unchanged.
  prep <- posterior_likelihood_prepare(m_tiny_eg1, n = 100, seed = 42,
    sampling_args = options[c("method", "draws")])
  p <- suppressWarnings(bayesplot_test_prepare(m_tiny_eg1, type = "loo_pit_overlay",
    n = 100, sampling_args = options))
  ll <- posterior_likelihood_matrix(prep)
  expected <- suppressWarnings(loo::loo(ll, r_eff = 1, save_psis = TRUE))
  expect_equal(p$args$lw, stats::weights(expected$psis_object, log = TRUE))
  response_seed <- withr::with_seed(42, sample.int(.Machine$integer.max, 1L))
  replicated <- diagnostic_posterior_samples(m_tiny_eg1, n = 100, seed = response_seed,
    method = "user", draws = prep$betas)
  expect_equal(p$args$yrep,
    matrix(replicated$.response[order(replicated$.draw, replicated$.row)], 100, byrow = TRUE))
  expect_s3_class(suppressWarnings(pp_check.gam(m_tiny_eg1, type = "loo_pit_overlay",
    n = 100, seed = 42)), "ggplot")
})

test_that("grouped leave-one-out plots receive only their selected PSIS columns", {
  skip_if_not_installed("bayesplot")
  skip_if_not_installed("loo")
  if (!exists("gfam_model")) skip("mgcv has no grouped family support")
  p <- suppressWarnings(bayesplot_test_prepare(gfam_model, type = "loo_intervals",
    response = "component3", n = 100))
  expect_s3_class(p$args$psis_object, "psis")
  expect_equal(dim(stats::weights(p$args$psis_object)),
    c(100L, sum(gfam_data$family_id == 3L)))
  prep <- posterior_likelihood_prepare(gfam_model, n = 100, seed = 42)
  ll <- posterior_likelihood_matrix(prep)[, gfam_data$family_id == 3L]
  expected <- suppressWarnings(loo::loo(ll, r_eff = 1, save_psis = TRUE))
  expect_equal(stats::weights(p$args$psis_object, log = TRUE),
    stats::weights(expected$psis_object, log = TRUE))
})

test_that("multivariate LOO response draws retain drawn covariance parameters", {
  prep <- posterior_likelihood_prepare(m_mvn, n = 2, seed = 10)
  # Make the precision substantially different from the fitted precision.
  d <- n_eta(prep$model)
  at <- ncol(prep$betas) - d * (d + 1L) / 2L + 1L
  prep$betas[, at] <- prep$betas[, at] + log(2)
  draws <- withr::with_seed(100, bayesplot_mvn_samples(prep, 2))
  fits <- fitted_samples(prep$model, n = 2, method = "user", draws = prep$betas)
  expected <- withr::with_seed(100, lapply(1:2, function(i) {
    mu <- vapply(paste0("response", seq_len(d)), function(parameter)
      fits$.fitted[fits$.draw == i & fits$.parameter == parameter],
      numeric(length(prep$rows)))
    R <- posterior_likelihood_mvn_R(prep$betas[i, ], d)
    matrix(stats::rnorm(length(mu)), nrow = nrow(mu)) %*% t(solve(R)) + mu
  }))
  expect_equal(draws$.response, as.vector(unlist(expected)), ignore_attr = TRUE)
})
