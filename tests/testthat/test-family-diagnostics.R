custom_count_diagnostics <- function(mu, wt, scale, y, eta) {
  list(mean = mu, variance = mu,
    cdf_left = function(q, log_p = FALSE, lower_tail = TRUE)
      ppois(ceiling(q) - 1, mu, log.p = log_p, lower.tail = lower_tail))
}

test_that("supplied diagnostics randomise unfamiliar count families and retain hooks", {
  f <- fix_family_rd(fix_family_qf(fix_family_cdf(poisson())))
  f$family <- "unfamiliar count distribution"
  f$gratia_diagnostics <- custom_count_diagnostics
  mu <- c(.5, 2, 4)
  y <- c(0, 2, 5)
  original <- f
  d <- distribution_from_family(f, mu, 1, 1, y)
  expected <- withr::with_seed(8, runif(3, ppois(y - 1, mu), ppois(y, mu)))
  expect_equal(distribution_quantile_residuals(d, y, "pit", seed = 8), expected)
  expect_equal(distribution_quantile_residuals(d, y, "quantile", seed = 8), qnorm(expected))
  expect_equal(distribution_cdf_left(d, c(0, .5, 2)), ppois(c(-1, 0, 1), mu))
  expect_identical(f, original)
  expect_identical(d$family$rd, f$rd)
  expect_equal(d$mean, mu)
  expect_equal(d$variance, mu)
  expect_equal(d$cdf_left(y, log_p = TRUE, lower_tail = FALSE),
    ppois(y - 1, mu, log.p = TRUE, lower.tail = FALSE))
})

test_that("factories receive compact fitted parameters and correct diagnostic panel means", {
  dat <- data.frame(x = 1:12, y = c(0, 1, 2, 3, NA, 2, 4, 5, 3, 4, 6, 8))
  f <- poisson()
  f$gratia_diagnostics <- function(mu, wt, scale, y, eta) {
    stopifnot(length(y) == 11L, length(mu) == 11L, length(wt) == 11L,
      length(eta) == 11L, all.equal(exp(eta), unname(mu)))
    list(mean = 2 * mu, diagnostic_residuals = function(y, type) {
      if (type == "response") y - 2 * mu else if (type == "pearson")
        (y - 2 * mu) / sqrt(2 * mu) else stop("No custom deviance residuals")
    })
  }
  m <- mgcv::gam(y ~ x, family = f, data = dat, na.action = na.exclude)
  expected <- 2 * fitted(m)
  p <- observed_fitted_plot(m)
  expect_equal(p$data$fitted, unname(expected))
  expect_equal(diagnostic_residuals(m, "response"), dat$y - expected, ignore_attr = TRUE)
  used <- model_used_rows(m)
  expect_equal(diagnostic_reference_residuals(m, rep(1, 11), "response"),
    unname(1 - 2 * used$fitted.values))
})

test_that("replacement distributions work without native family distribution functions", {
  f <- poisson()
  f$family <- "unfamiliar continuous distribution"
  f$gratia_diagnostics <- function(mu, wt, scale, y, eta) list(
    mean = mu, variance = rep(4, length(mu)),
    rd = function() rnorm(length(mu), mu, 2),
    qf = function(p, log_p = FALSE, lower_tail = TRUE)
      qnorm(p, mu, 2, log.p = log_p, lower.tail = lower_tail),
    cdf = function(q, log_p = FALSE, lower_tail = TRUE)
      pnorm(q, mu, 2, log.p = log_p, lower.tail = lower_tail))
  d <- distribution_from_family(f, c(1, 2), 1, 1)
  expect_equal(d$cdf(c(25, -22), log_p = TRUE, lower_tail = FALSE),
    pnorm(c(12, -12), log.p = TRUE, lower.tail = FALSE))
  expect_equal(distribution_quantile_residuals(d, c(25, -22), "quantile"), c(12, -12))
  expect_equal(d$qf(c(.25, .75)), qnorm(c(.25, .75), c(1, 2), 2))
  expect_equal(distribution_cdf_left(d, c(1, 2)), c(.5, .5))
  expect_true(d$joint_available)
  expect_equal(withr::with_seed(1, d$rd()), withr::with_seed(1, rnorm(2, c(1, 2), 2)))
})

test_that("bounds functions carry censoring intervals and invalid results are rejected", {
  f <- gaussian()
  f$gratia_diagnostics <- function(mu, wt, scale, y, eta) list(
    bounds = function(y) censored_response_bounds(y))
  y <- structure(c(0, 1, 2, 3), censor = c(-Inf, 1, Inf, 4))
  d <- distribution_from_family(f, rep(1, 4), 1, 1, y)
  expected <- withr::with_seed(9, {
    out <- pnorm(y, 1)
    out[c(1, 3, 4)] <- runif(3, pnorm(c(-Inf, 2, 3), 1), pnorm(c(0, Inf, 4), 1))
    out
  })
  expect_equal(distribution_quantile_residuals(d, y, "pit", seed = 9), as.numeric(expected))
  f$gratia_diagnostics <- function(...) list(bounds = function(y)
    list(lower = y + 1, upper = y))
  d <- distribution_from_family(f, rep(1, 4), 1, 1, y)
  expect_error(distribution_quantile_residuals(d, y), "ordered lower and upper")
  f$gratia_diagnostics <- function(...) list(cdf = function(q, log_p = FALSE, lower_tail = TRUE) rep(2, 4))
  d <- distribution_from_family(f, rep(1, 4), 1, 1, y)
  expect_error(distribution_quantile_residuals(d, y), "invalid probabilities")
})

test_that("unknown fields and malformed factories fail with useful explanations", {
  bad <- list(42, function(...) NULL, function(...) list(unknown = TRUE),
    function(...) list(mean = 1), function(...) list(variance = c(-1, 2)),
    function(...) list(cdf = function(q) q), function(...) list(bounds = 2),
    function(...) list(mean_available = NA), function(...) list(rd = NULL, joint_available = TRUE))
  for (factory in bad) {
    f <- gaussian()
    f$gratia_diagnostics <- factory
    expect_error(distribution_from_family(f, c(1, 2), 1, 1), "gratia_diagnostics")
  }
  f <- gaussian()
  f$gratia_diagnostics <- function(...) list(qf = NULL)
  expect_null(distribution_from_family(f, c(1, 2), 1, 1)$qf)
  f$gratia_diagnostics <- function(...) list()
  expect_equal(distribution_from_family(f, c(1, 2), 1, 1)$mean, c(1, 2))
  f$gratia_diagnostics <- function(...) list(bounds = NULL)
  d <- distribution_from_family(f, c(1, 2), 1, 1)
  expect_equal(distribution_cdf_left(d, c(1, 2)), c(.5, .5))
})

test_that("undefined supplied moments block only residuals that require them", {
  f <- gaussian()
  f$gratia_diagnostics <- function(...) list(mean = c(NA_real_, NA_real_),
    mean_available = FALSE, variance_available = FALSE, moment_reason = "The mean does not exist.")
  d <- distribution_from_family(f, c(1, 2), 1, 1)
  expect_error(check_distribution_residual_moments(d, "response"), "mean does not exist")
  expect_equal(distribution_quantile_residuals(d, c(1, 2), "quantile"), c(0, 0))
})

test_that("traditional reference residuals apply an explicitly supplied observation process", {
  dat <- data.frame(y = seq(-1, 1, length.out = 12))
  m <- mgcv::gam(y ~ 1, data = dat)
  m$family$gratia_diagnostics <- function(mu, wt, scale, y, eta) list(
    observe = function(y) pmax(y, 0))
  draw <- seq(-2, 2, length.out = 12)
  expect_equal(diagnostic_reference_residuals(m, draw, "response"),
    pmax(draw, 0) - fitted(m), ignore_attr = TRUE)
  m$family$gratia_diagnostics <- function(...) list(reference_available = FALSE,
    reference_reason = "Censoring thresholds are unknown.")
  expect_error(qq_plot(m, method = "simulate"), "Censoring thresholds are unknown")
  expect_error(worm_plot(m, method = "uniform"), "Censoring thresholds are unknown")
  expect_s3_class(qq_plot(m, type = "quantile"), "ggplot")
  expect_s3_class(qq_plot(m, method = "normal"), "ggplot")
})

test_that("grouped diagnostics accept family factories and keep their row order", {
  m <- gfam_model
  families <- m$family$getfl()
  families[[3]] <- fix_family_rd(fix_family_qf(fix_family_cdf(families[[3]])))
  families[[3]]$family <- "unfamiliar count distribution"
  families[[3]]$gratia_diagnostics <- custom_count_diagnostics
  m$family$getfl <- function() families
  expect_length(gfam_cdf_support(gfam_diagnostic_context(m)), 0L)
  expect_equal(quantile_residuals(m, type = "pit", seed = 3),
    quantile_residuals(gfam_model, type = "pit", seed = 3))
  expect_equal(qq_plot(m, type = "deviance", by_family = FALSE,
    n_simulate = 3, seed = 3)$data,
    qq_plot(gfam_model, type = "deviance", by_family = FALSE,
      n_simulate = 3, seed = 3)$data)
  expect_equal(diagnostic_data(m)$data$fitted, diagnostic_data(gfam_model)$data$fitted)
  expect_s3_class(appraise(m, type = "quantile", seed = 3), "patchwork")
  families[[3]]$gratia_diagnostics <- function(mu, ...) list(mean = mu)
  expect_match(gfam_cdf_support(gfam_diagnostic_context(m)), "unfamiliar count distribution")
})

test_that("combined observation processes keep censoring bounds and availability", {
  f <- gaussian()
  f$gratia_diagnostics <- function(...) list(
    bounds = function(y) censored_response_bounds(y),
    observe = function(y) cbind(pmax(y, 0), ifelse(y < 0, -Inf, y)))
  a <- distribution_from_family(f, c(1, 2), 1, 1)
  b <- distribution_from_family(poisson(), c(2, 3), 1, 1)
  d <- combine_response_distributions(list(a, b), c(1, 2, 1, 2))
  y <- observe_diagnostic_response(d, c(-1, 2, 3, 4))
  expect_equal(as.numeric(y), c(0, 2, 3, 4))
  expect_equal(attr(y, "censor"), c(-Inf, 2, 3, 4))
  expect_equal(d$bounds(y)$lower, c(-Inf, 1, 3, 3))
  expect_true(all(is.finite(distribution_quantile_residuals(d, y, "quantile", seed = 1))))
  a$reference_available <- FALSE
  a$reference_reason <- "The observation process is unknown."
  d <- combine_response_distributions(list(a, b), c(1, 2, 1, 2))
  expect_error(observe_diagnostic_response(d, c(-1, 2, 3, 4)), "observation process is unknown")
})


test_that("matrix response moments define component plots for unfamiliar families", {
  withr::local_seed(407)
  dat <- data.frame(y1 = rnorm(60), y2 = rnorm(60))
  dat$y1[8] <- NA
  m <- mgcv::gam(list(cbind(y1, y2) ~ 1, ~1), data = dat,
    family = mgcv::mvn(2), na.action = na.exclude)
  m$family$family <- "custom vector response"
  m$family$gratia_diagnostics <- function(mu, wt, scale, y, eta) {
    mean <- mu[, 1:2, drop = FALSE]
    list(mean = mean, variance = mean * 0 + 1, continuous = TRUE,
      independent = FALSE, joint_available = TRUE, qf = NULL,
      cdf = function(q, log_p = FALSE, lower_tail = TRUE)
        pnorm(q, mean, 1, log.p = log_p, lower.tail = lower_tail),
      rd = function() matrix(rnorm(length(mean), mean), nrow(mean)),
      diagnostic_residuals = function(y, type) y - mean)
  }
  used <- model_used_rows(m)
  d <- response_distribution(m)
  expected_mean <- as.vector(restore_model_rows(d$mean, m))
  expect_equal(observed_fitted_plot(m)$data$fitted, expected_mean)
  r <- quantile_residuals(m, type = "quantile")
  expect_equal(dim(r), c(60L, 2L))
  p <- residuals_linpred_plot(m, type = "quantile")
  expect_equal(p$data$eta, expected_mean)
  expect_equal(p$labels$x, "Fitted response mean")
  expect_s3_class(appraise(m, type = "quantile"), "patchwork")
  expect_s3_class(appraise(m, type = "quantile", scalable = TRUE), "patchwork")
})


test_that("grouped reconstruction preserves nested diagnostic parameter closures", {
  f <- mgcv::nb(theta = -2)
  get_theta <- f$getTheta
  f$gratia_diagnostics <- local({
    get <- get_theta
    function(mu, wt, scale, y, eta) list(mean = mu,
      variance = mu + mu^2 / get(TRUE),
      cdf_left = function(q, log_p = FALSE, lower_tail = TRUE)
        pnbinom(ceiling(q) - 1, mu = mu, size = get(TRUE), log.p = log_p, lower.tail = lower_tail))
  })
  rebuilt <- gfam_component_family(f, log(5))
  expect_equal(f$getTheta(TRUE), 2)
  expect_equal(rebuilt$getTheta(TRUE), 5)
  d <- distribution_from_family(rebuilt, c(1, 2), 1, 1)
  expect_equal(d$variance, c(1, 2) + c(1, 2)^2 / 5)
  expect_equal(d$cdf_left(c(0, 2)), pnbinom(c(-1, 1), mu = c(1, 2), size = 5))
})
