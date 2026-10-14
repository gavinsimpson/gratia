test_that("inverse Gaussian family hooks are available and preserve custom hooks", {
  fam <- stats::inverse.gaussian()
  expect_identical(fix_family_cdf(fam)$cdf, cdf_invgaussian)
  expect_identical(fix_family_qf(fam)$qf, qf_invgaussian)
  fam$cdf <- fam$qf <- function(...) 42
  expect_identical(fix_family_cdf(fam)$cdf, fam$cdf)
  expect_identical(fix_family_qf(fam)$qf, fam$qf)
})

test_that("inverse Gaussian CDF reproduces published extreme-tail values", {
  expect_equal(cdf_invgaussian(.001, 1.5, 1, .7) / 3.36757674879789e-312,
    1, tolerance = 1e-12)
  expect_equal(cdf_invgaussian(.0001, 1.5, 1, .7, log_p = TRUE),
    -7146.914162644709, tolerance = 1e-12)
  expect_equal(cdf_invgaussian(110, 1.5, 1, .7, lower_tail = FALSE) /
    2.19691267480251e-18, 1, tolerance = 1e-12)
  # At this dispersion the original expression returned 1.5 at the mean.
  p <- cdf_invgaussian(1.5, 1.5, 1, 1e-20)
  expect_equal(p, .5, tolerance = 1e-9)
  expect_true(p >= 0 && p <= 1)
  # q/mu overflows although the log survival is finite. The Mills ratio
  # difference must use its limiting geometric factor when u/v rounds to 1.
  huge <- cdf_invgaussian(exp(700), exp(-100), 1, exp(700),
    lower_tail = FALSE, log_p = TRUE)
  expect_true(is.finite(huge) && huge < 0)
  expect_equal(huge / (-exp(200) / 2), 1, tolerance = 1e-12)
})

test_that("inverse Gaussian tails satisfy the independent chi-square identity", {
  g <- expand.grid(mu = c(.01, 1.5, 100), phi = c(1e-12, 1e-6, .7, 100,
    1e12, 1e20), ratio = c(.001, .01, .1, .5, .99), wt = c(.25, 4))
  log_left <- cdf_invgaussian(g$mu * g$ratio, g$mu, g$wt, g$phi,
    log_p = TRUE)
  log_right <- cdf_invgaussian(g$mu / g$ratio, g$mu, g$wt, g$phi,
    log_p = TRUE, lower_tail = FALSE)
  actual <- logspace_add(log_left, log_right)
  z <- (g$ratio - 1)^2 / (g$phi / g$wt * g$mu * g$ratio)
  expected <- pchisq(z, df = 1, lower.tail = FALSE, log.p = TRUE)
  expect_lt(max(abs(actual - expected) / pmax(1, abs(expected))), 2e-13)
})

test_that("inverse Gaussian individual tails agree with density integration", {
  # This reference uses neither the normal-CDF formula nor the quantile solver.
  for (phi in c(.1, 1, 10)) {
    density <- function(y) {
      out <- numeric(length(y))
      ok <- which(y > 0 & is.finite(y))
      out[ok] <- exp(-.5 * log(2 * pi * phi) - 1.5 * log(y[ok]) -
        (y[ok] / 2 - 1)^2 / (2 * phi * y[ok]))
      out
    }
    for (q in c(1, 2, 6)) {
      lower <- integrate(density, 0, q, rel.tol = 1e-10, abs.tol = 0)$value
      upper <- integrate(density, q, Inf, rel.tol = 1e-10, abs.tol = 0)$value
      expect_equal(cdf_invgaussian(q, 2, 1, phi) / lower, 1,
        tolerance = 1e-9)
      expect_equal(cdf_invgaussian(q, 2, 1, phi, lower_tail = FALSE) / upper,
        1, tolerance = 1e-9)
    }
  }
})

test_that("inverse Gaussian survival remains accurate when normal tails nearly coincide", {
  # Rescale the density integral so its size stays near one. In this
  # regime q/mu is huge and the two normal-tail arguments round identically.
  u <- c(.1, 1, 5, 15.99, 16, 16.01, 20, 100)
  a <- u^2 / 2
  log_integral <- vapply(a, function(z) {
    log(integrate(function(t) (1 + t / z)^(-1.5) * exp(-t),
      0, Inf, rel.tol = 1e-11, abs.tol = 0, subdivisions = 500L)$value)
  }, numeric(1))
  expected <- -.5 * log(2 * pi) - 200 - log(u) - a - log(a) + log_integral
  actual <- cdf_invgaussian(exp(300 + 2 * log(u)), exp(100), 1, exp(100),
    lower_tail = FALSE, log_p = TRUE)
  expect_lt(max(abs(actual - expected)), 2e-10)
  expect_equal(cdf_invgaussian(exp(200), exp(300), 1, 1,
    lower_tail = FALSE, log_p = TRUE),
    pchisq(exp(-200), 1, log.p = TRUE), tolerance = 1e-12)
})

test_that("inverse Gaussian quantiles retain extreme log probabilities", {
  g <- expand.grid(mu = c(.01, 1.5, 100), phi = c(.01, .7, 100),
    wt = c(.25, 4), logp = c(-1e-100, -.1, -1, -100, -1000, -10000))
  for (tail in c(TRUE, FALSE)) {
    q <- qf_invgaussian(g$logp, g$mu, g$wt, g$phi,
      log_p = TRUE, lower_tail = tail)
    expect_true(all(is.finite(q) & q > 0))
    actual <- cdf_invgaussian(q, g$mu, g$wt, g$phi,
      log_p = TRUE, lower_tail = tail)
    # Compare in the smaller tail, including log probabilities close to zero.
    target <- g$logp
    large <- which(target > -log(2))
    actual[large] <- log1mexp(actual[large])
    target[large] <- log1mexp(target[large])
    expect_lt(max(abs(actual - target)), 2e-9)
  }
  # The example that defeated an earlier Newton implementation in the paper.
  expect_equal(qf_invgaussian(.00013, 1, 1, 1 / 3), .1504,
    tolerance = 5e-4)
  expect_equal(qf_invgaussian(-1e-20, 1.5, 1, .7, log_p = TRUE),
    126.3493351314917, tolerance = 1e-12)
})

test_that("inverse Gaussian quantiles converge at extreme finite parameter scales", {
  # A correction can round to the current log quantile while the other
  # bracket endpoint is still distant. It must not trigger fresh bisection.
  logmu <- c(108.464666549116, 72.6790023967624, -19.7599750477821, 38.3041623048484)
  logphi <- c(213.596253376454, 234.943909943104, 280.297848908231, 275.441234791651)
  target <- c(-1239.78510408084, -794.013935854766, -1242.17606194935, -4895.83331982687)
  expect_no_warning(q <- qf_invgaussian(target, exp(logmu), 1, exp(logphi),
    log_p = TRUE, lower_tail = FALSE))
  expect_true(all(is.finite(q) & q > 0))
  actual <- cdf_invgaussian(q, exp(logmu), 1, exp(logphi),
    log_p = TRUE, lower_tail = FALSE)
  expect_lt(max(abs(actual - target)), 2e-9)
})

test_that("inverse Gaussian helpers recycle parameters and respect weights", {
  p <- c(.01, .5, .99)
  mu <- c(.5, 2, 7)
  wt <- c(.25, 1, 4)
  scale <- c(.1, 1, 10)
  for (tail in c(TRUE, FALSE)) {
    q <- qf_invgaussian(p, mu, wt, scale, lower_tail = tail)
    expect_equal(q, qf_invgaussian(log(p), mu, 1, scale / wt,
      log_p = TRUE, lower_tail = tail))
    expect_equal(cdf_invgaussian(q, mu, wt, scale, lower_tail = tail), p)
    expect_equal(cdf_invgaussian(q, mu, wt, scale, lower_tail = tail),
      exp(cdf_invgaussian(q, mu, 1, scale / wt,
        lower_tail = tail, log_p = TRUE)))
  }
  expect_equal(qf_invgaussian(.5, mu, 1, .7),
    qf_invgaussian(rep(.5, 3), mu, 1, .7))
  named <- setNames(p, letters[1:3])
  expect_named(qf_invgaussian(named, 1, 1, 1), names(named))
  expect_named(cdf_invgaussian(named, 1, 1, 1), names(named))
  expect_length(cdf_invgaussian(numeric(), 1, 1, 1), 0)
  expect_length(qf_invgaussian(.5, numeric(), 1, 1), 0)
})

test_that("inverse Gaussian boundaries and limiting distributions are explicit", {
  q <- c(-Inf, -1, 0, 1, Inf, NA_real_)
  expect_equal(cdf_invgaussian(q, 1, 1, 1)[c(1:3, 5:6)],
    c(0, 0, 0, 1, NA))
  expect_equal(cdf_invgaussian(q, 1, 1, 1, lower_tail = FALSE)[c(1:3, 5:6)],
    c(1, 1, 1, 0, NA))
  expect_equal(cdf_invgaussian(c(0, Inf), 1, 1, 1, log_p = TRUE), c(-Inf, 0))
  expect_equal(qf_invgaussian(c(0, 1, NA), 1, 1, 1), c(0, Inf, NA))
  expect_equal(qf_invgaussian(c(-Inf, 0), 1, 1, 1, log_p = TRUE), c(0, Inf))
  expect_equal(qf_invgaussian(c(0, 1), 1, 1, 1, lower_tail = FALSE), c(Inf, 0))
  expect_equal(cdf_invgaussian(c(.5, 1, 2), 1, 1, 0), c(0, 1, 1))
  expect_equal(cdf_invgaussian(c(.5, 1, 2), 1, 1, 0, lower_tail = FALSE),
    c(1, 0, 0))
  expect_equal(qf_invgaussian(c(.1, .5, .9), 2, 1, 0), rep(2, 3))
  expect_equal(cdf_invgaussian(c(-1, 0, 1), 1, 0, 1), c(0, 1, 1))
  expect_equal(qf_invgaussian(c(.1, .5, .9), 1, 0, 1), rep(0, 3))
  expect_equal(cdf_invgaussian(c(-1, 0, 1), 1, 1, Inf), c(0, 1, 1))
  expect_equal(cdf_invgaussian(c(.1, 1, 10), Inf, 1, .7),
    pchisq(1 / (.7 * c(.1, 1, 10)), 1, lower.tail = FALSE))
  expect_equal(qf_invgaussian(c(.1, .5, .9), Inf, 1, .7),
    1 / (.7 * qchisq(c(.1, .5, .9), 1, lower.tail = FALSE)))
})

test_that("inverse Gaussian invalid and missing elements do not affect valid rows", {
  expect_warning(p <- cdf_invgaussian(1, c(1, NA, -1), 1, 1), "NaNs")
  expect_equal(p[1], cdf_invgaussian(1, 1, 1, 1))
  expect_true(is.na(p[2]) && is.nan(p[3]))
  expect_warning(q <- qf_invgaussian(c(.5, -.1, 1.1, NA), 1, 1, 1), "NaNs")
  expect_true(all(is.na(q[-1])))
  expect_warning(q <- qf_invgaussian(c(-1, .1, Inf), 1, 1, 1,
    log_p = TRUE), "NaNs")
  expect_true(all(is.nan(q[-1])))
  expect_warning(cdf_invgaussian(1, 1, -1, 1), "NaNs")
  expect_warning(qf_invgaussian(.5, 0, 1, 1), "NaNs")
  expect_warning(cdf_invgaussian(1, 1, 0, 0), "NaNs")
  # Tiny dispersion and missing values were problematic in a reference backend.
  expect_no_warning(p <- cdf_invgaussian(1, 1, 1, c(1e-20, NA, -1e-20)[1:2]))
  expect_equal(p, c(.5, NA), tolerance = 1e-9)
  expect_error(cdf_invgaussian(1, 1, 1, 1, lower_tail = NA), "TRUE or FALSE")
  expect_error(qf_invgaussian(.5, 1, 1, 1, log_p = 1), "TRUE or FALSE")
})

test_that("inverse Gaussian fitted diagnostics use native CDF and quantiles", {
  withr::local_seed(82)
  d <- data.frame(x = seq(-1, 1, length.out = 60))
  d$y <- mgcv::fix.family.rd(stats::inverse.gaussian())$rd(
    exp(.2 + .3 * d$x), wt = rep(1, 60), scale = .5)
  d$y[c(5, 20)] <- NA
  m <- mgcv::gam(y ~ x, data = d, family = stats::inverse.gaussian(link = "log"),
    weights = rep(c(.5, 1, 2), 20), method = "REML", na.action = na.exclude)
  r <- quantile_residuals(m, type = "quantile")
  expect_length(r, 60)
  expect_true(all(is.na(r[c(5, 20)])))
  expect_true(all(is.finite(r[-c(5, 20)])))
  pit <- quantile_residuals(m, type = "pit")
  expect_equal(pnorm(r), pit)
  for (type in c("deviance", "response", "pearson")) {
    expect_true(all(is.finite(qq_uniform(m, n = 1, type = type)$theoretical)))
  }
  expect_equal(qq_plot(m, method = "uniform", n_uniform = 1)$labels$subtitle,
    "Method: uniform")
  expect_equal(worm_plot(m, method = "uniform", n_uniform = 1)$labels$subtitle,
    "Method: uniform")
  expect_identical(choose_rd_fun(m), rd_invgaussian)
  used <- model_used_rows(m)
  mu <- used$fitted.values
  wt <- used$prior.weights
  expected <- matrix(NA_real_, 60, 3)
  expected[-c(5, 20), ] <- withr::with_seed(121,
    replicate(3, rd_invgaussian(mu, 1, m$sig2 / wt)))
  actual <- simulate(m, nsim = 3, seed = 121)
  expect_equal(as.matrix(actual), expected, ignore_attr = TRUE)

  expected <- withr::with_seed(121, {
    sims <- replicate(3, sort(rd_invgaussian(mu, 1, m$sig2 / wt) - mu))
    unname(quantile(sims, (seq_along(mu) - .5) / length(mu)))
  })
  actual <- withr::with_seed(121, qq_simulate(m, n = 3, type = "response"))
  expect_equal(unname(actual$theoretical), expected)
  expect_equal(qq_plot(m, method = "simulate", n_simulate = 3)$labels$subtitle,
    "Method: simulate")

  # Fix coefficients so this independently checks the response law and RNG,
  # without coupling its oracle to the coefficient sampler stream.
  draws <- matrix(rep(coef(m), each = 2L), nrow = 2L)
  eta <- fitted_samples(m, n = 2, method = "user", draws = draws,
    scale = "response", mvn_method = "mgcv")
  weights <- restore_model_rows(wt, m)[eta$.row]
  expected <- withr::with_seed(121,
    rd_invgaussian(eta$.fitted, weights, m$sig2))
  actual <- posterior_samples(m, n = 2, seed = 121, method = "user",
    draws = draws, mvn_method = "mgcv")
  expect_equal(actual$.response, expected)

  extreme <- do_quantile_residuals(c(.0001, 110), c(1.5, 1.5), 1, .7,
    stats::inverse.gaussian(), "quantile")
  expect_true(all(is.finite(extreme)))
  expect_lt(extreme[1], -100)
  expect_gt(extreme[2], 8)
})

test_that("inverse Gaussian RNG dispatch preserves supplied functions", {
  fam <- stats::inverse.gaussian()
  fixed <- fix_family_rd(fam)
  expect_identical(fixed$rd, rd_invgaussian)
  expect_identical(fix_family_rd(fixed)$rd, rd_invgaussian)
  fam$rd <- function(mu, wt, scale) rep(42, length(mu))
  expect_identical(fix_family_rd(fam)$rd, fam$rd)
  expect_equal(fix_family_rd(fam)$rd(1:3, 1, 1), rep(42, 3))
})

test_that("inverse Gaussian RNG respects weighted dispersion and recycling", {
  mu <- c(.2, 1.5, 8, 2)
  wt <- c(.25, 1, 4, 2)
  scale <- c(.1, .7, 3, 1)
  weighted <- withr::with_seed(54, rd_invgaussian(mu, wt, scale))
  adjusted <- withr::with_seed(54, rd_invgaussian(mu, 1, scale / wt))
  expect_identical(weighted, adjusted)
  expect_identical(withr::with_seed(54, rd_invgaussian(2, wt, scale)),
    withr::with_seed(54, rd_invgaussian(rep(2, 4), wt, scale)))
  named <- setNames(mu, letters[1:4])
  expect_named(rd_invgaussian(named, 1, 1), names(named))
  expect_length(rd_invgaussian(numeric(), 1, 1), 0)
  expect_length(rd_invgaussian(1, numeric(), 1), 0)
})

test_that("inverse Gaussian RNG agrees with theoretical moments and quantiles", {
  withr::local_seed(639)
  n <- 40000L
  p <- c(.01, .1, .5, .9, .99)
  g <- expand.grid(mu = c(.2, 2, 5), wt = c(.25, 1, 4))
  for (i in seq_len(nrow(g))) {
    mu <- g$mu[i]
    wt <- g$wt[i]
    phi <- .3 / wt
    variance <- phi * mu^3
    y <- rd_invgaussian(rep(mu, n), wt, .3)
    expect_true(all(is.finite(y) & y > 0))
    expect_lt(abs(mean(y) - mu), 6 * sqrt(variance / n))
    # The exact IG fourth central moment gives this Monte Carlo error scale.
    expect_lt(abs(var(y) - variance),
      6 * variance * sqrt((2 + 15 * phi * mu) / n))
    q <- qf_invgaussian(p, mu, wt, .3)
    empirical <- vapply(q, function(x) mean(y <= x), numeric(1))
    expect_true(all(abs(empirical - p) < 6 * sqrt(p * (1 - p) / n)))
    pit <- cdf_invgaussian(y, mu, wt, .3)
    expect_lt(abs(mean(pit) - .5), 6 / sqrt(12 * n))
  }
})

test_that("inverse Gaussian RNG avoids overflow and underflow in its roots", {
  n <- 2000L
  reference <- withr::with_seed(246, rd_invgaussian(rep(1, n), 1, 1))
  # mu^2 respectively overflows and underflows in the unrearranged algorithm.
  for (mu in c(1e-300, 1e300)) {
    actual <- withr::with_seed(246, rd_invgaussian(rep(mu, n), 1, 1 / mu))
    expect_true(all(is.finite(actual) & actual > 0))
    expect_true(any(actual > mu))
    expect_equal(actual / mu, reference, tolerance = 5e-13)
  }
  # phi*mu overflows, but the usual smaller-root draws remain representable.
  actual <- withr::with_seed(246, rd_invgaussian(rep(1e300, n), 1, 1e300))
  expect_true(all(is.finite(actual) & actual > 0))
  expect_true(all(actual < 1e-290))
  # Tiny dispersion should concentrate near mu without cancellation or NaNs.
  actual <- withr::with_seed(246, rd_invgaussian(rep(1.5, n), 1, 1e-20))
  expect_true(all(is.finite(actual) & actual > 0))
  expect_lt(max(abs(actual / 1.5 - 1)), 1e-8)
})

test_that("inverse Gaussian RNG handles limits, missing and invalid inputs", {
  withr::local_seed(91)
  before <- .Random.seed
  expect_identical(rd_invgaussian(c(2, 3, Inf), 1, 0), c(2, 3, Inf))
  expect_identical(rd_invgaussian(c(2, 3, Inf), 1, Inf), rep(0, 3))
  expect_identical(rd_invgaussian(c(2, 3), 0, 1), c(0, 0))
  expect_identical(rd_invgaussian(c(2, 3), Inf, 1), c(2, 3))
  expect_identical(.Random.seed, before)
  expected <- withr::with_seed(91, 1 / (.7 * rnorm(4)^2))
  expect_equal(withr::with_seed(91, rd_invgaussian(rep(Inf, 4), 1, .7)),
    expected, tolerance = 1e-14)

  expect_warning(actual <- withr::with_seed(91,
    rd_invgaussian(c(1, NA, -1, 2), 1, .7)), "NaNs")
  expect_true(is.na(actual[2]) && is.nan(actual[3]))
  expected <- withr::with_seed(91, rd_invgaussian(c(1, 2), 1, .7))
  expect_identical(actual[c(1, 4)], expected)
  expect_no_warning(actual <- rd_invgaussian(1, 1, c(1e-20, NA)))
  expect_true(is.finite(actual[1]) && is.na(actual[2]))
  expect_warning(rd_invgaussian(0, 1, 1), "NaNs")
  expect_warning(rd_invgaussian(1, -1, 1), "NaNs")
  expect_warning(rd_invgaussian(1, 1, -1), "NaNs")
  expect_warning(rd_invgaussian(1, 0, 0), "NaNs")
  expect_error(rd_invgaussian("1", 1, 1), "numeric")
})

test_that("inverse Gaussian grouped draws use the native weighted RNG", {
  families <- list(stats::gaussian(), stats::inverse.gaussian(link = "log"))
  context <- list(index = c(2L, 1L, 2L, 1L, 2L), components = list(
    list(family = families[[1]], scale = .2),
    list(family = families[[2]], scale = .7)))
  eta <- c(-.5, .1, .2, 1, .8)
  wt <- c(.25, 1, 2, .5, 4)
  generators <- lapply(families, function(f) fix_family_rd(f)$rd)
  expect_identical(generators[[2]], rd_invgaussian)
  expected <- withr::with_seed(77, {
    out <- numeric(5)
    out[c(2, 4)] <- rnorm(2, eta[c(2, 4)], sqrt(.2 / wt[c(2, 4)]))
    out[c(1, 3, 5)] <- rd_invgaussian(exp(eta[c(1, 3, 5)]),
      1, .7 / wt[c(1, 3, 5)])
    out
  })
  expect_equal(withr::with_seed(77,
    gfam_response_draw(eta, wt, context, generators)), expected)
})
