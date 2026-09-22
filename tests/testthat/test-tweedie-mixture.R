test_that("mixture quantiles validate parameters and handle boundaries", {
  expect_equal(qtweedie_mixture(numeric(), numeric(), 1.5, numeric()), numeric())
  expect_equal(qtweedie_mixture(c(0, 1, NA), 1, 1.5, 1), c(0, Inf, NA))
  expect_equal(qtweedie_mixture(c(0, .5, 1), 0, 1.5, 1), c(0, 0, 0))
  expect_equal(qtweedie_mixture(.5, c(0, NA, 1), 1.5, c(1, 1, NA)),
    c(0, NA, NA))
  expect_warning(x <- qtweedie_mixture(c(-.1, 1.1, .5), 1, 1.5, 1), "NaNs")
  expect_true(all(is.na(x[1:2])))
  expect_true(is.finite(x[3]))
  expect_warning(x <- qtweedie_mixture(.1, 1, 1.5, 1, log_p = TRUE), "NaNs")
  expect_true(is.na(x))
  for (power in list(1, 2, NA, Inf, c(1.2, 1.5))) {
    expect_error(qtweedie_mixture(.5, 1, power, 1), "scalar in")
  }
  expect_error(qtweedie_mixture(.5, -1, 1.5, 1), "Nonmissing means")
  expect_error(qtweedie_mixture(.5, 1, 1.5, 0), "scales")
  expect_error(qtweedie_mixture(.5, Inf, 1.5, 1), "finite")
  expect_error(qtweedie_mixture(1:3 / 4, 1:2, 1.5, 1), "common nonzero")
  expect_error(qtweedie_mixture(numeric(), 1, 1.5, 1), "common nonzero")
  expect_error(qtweedie_mixture(.5, 1, 1.5, 1, max_terms = 0), "positive integer")
  expect_error(qtweedie_mixture(.5, 1, 1.5, 1, cdf_tol = 0), "Tolerances")
})

test_that("mixture quantiles agree with independent CDF inversion", {
  skip_if_not_installed("tweedie")
  # Ordinary cases on both sides of the median and with varying means/scales.
  for (power in c(1.1, 1.5, 1.9)) {
    mu <- c(.1, 1, 10)
    phi <- c(.3, 1, 2)
    for (u in c(.1, .5, .99)) {
      actual <- qtweedie_mixture(u, mu, power, phi, details = TRUE)
      expected <- tweedie::qtweedie(rep(u, 3), mu = mu, xi = power, phi = phi)
      expect_equal(actual$quantile, expected, tolerance = 1e-7)
      expect_true(all(actual$method %in% c("atom", "mixture")))
      positive <- actual$quantile > 0
      cdf <- tweedie::ptweedie(actual$quantile[positive], mu = mu[positive],
        xi = power, phi = phi[positive])
      expect_equal(cdf, rep(u, sum(positive)), tolerance = 1e-7)
      expect_true(all(actual$log_tail_error[positive] <= 1e-8))
      expect_true(all(actual$log_omitted_mass[positive] <= log(1e-12)))
    }
  }
})

test_that("quantiles respect the atom, monotonicity, and vector recycling", {
  for (power in c(1.01, 1.5, 1.99)) {
    lambda <- 1 / (2 - power)
    log_atom <- -lambda
    lp <- sort(unique(c(log_atom - 1e-8, log_atom, log_atom + 1e-8,
      log(c(.1, .5, .9)), -1e-12, -1e-30)))
    result <- qtweedie_mixture(lp, 1, power, 1, log_p = TRUE, details = TRUE)
    q <- result$quantile
    expect_true(all(diff(q) >= 0))
    expect_true(all(q[lp <= log_atom] == 0))
    expect_true(all(is.finite(result$log_quantile[lp > log_atom])))
    expect_true(all(q[lp > log_atom] > 0 |
      result$method[lp > log_atom] == "underflow"))
    expect_equal(q, vapply(lp, function(p) {
      qtweedie_mixture(p, 1, power, 1, log_p = TRUE)
    }, numeric(1)))
  }
  u <- c(.2, .5, .9)
  expect_equal(qtweedie_mixture(u, 1:3, 1.5, .7),
    qtweedie_mixture(log(u), 1:3, 1.5, rep(.7, 3), log_p = TRUE))
})

test_that("quantiles obey scaling and converge with tighter truncation", {
  for (power in c(1.05, 1.5, 1.95)) {
    u <- c(.4, .8, .99)
    mu <- c(.1, 1, 10)
    base <- qtweedie_mixture(u, mu, power, .7)
    for (scale in c(1e-8, 1e8)) {
      scaled <- qtweedie_mixture(u, scale * mu, power, scale^(2-power) * .7)
      expect_equal(scaled / scale, base, tolerance = 1e-8)
    }
    tighter <- qtweedie_mixture(u, mu, power, .7,
      cdf_tol = 1e-15, probability_tol = 1e-10, quantile_tol = 1e-12)
    expect_equal(tighter, base, tolerance = 1e-8)
  }
})

test_that("extreme tails are computed without rounding to probability endpoints", {
  # Check against a deliberately over-wide independent Poisson-Gamma sum.
  # Its linear arithmetic is sufficient for this upper tail, but 1 - CDF is not.
  lp <- -1e-30
  q <- qtweedie_mixture(lp, 1, 1.5, 1, log_p = TRUE, details = TRUE)
  expect_equal(q$method, "mixture")
  k <- 1:100
  survival <- sum(dpois(k, 2) * pgamma(q$quantile, shape = k,
    scale = .5, lower.tail = FALSE))
  expect_equal(log(survival), log(-expm1(lp)), tolerance = 1e-9)
  # Extremely small lower probability with an even smaller mass at zero.
  lp <- -1000
  q <- qtweedie_mixture(lp, 1, 1.5, .001, log_p = TRUE, details = TRUE)
  expect_equal(q$method, "mixture")
  expect_true(q$quantile > 0 && is.finite(q$quantile))
  expect_lte(q$log_tail_error, 1e-8)
})

test_that("opt-in uses the existing inversion only for unresolved quantiles", {
  calls <- list()
  local_mocked_bindings(tweedie_quantile_fallback = function(logu, mu, power, phi) {
    calls[[length(calls) + 1L]] <<- c(logu, mu, power, phi)
    123
  })
  out <- qtweedie_mixture(c(0, .5, 1), 1, 1.5, 1, max_terms = 1, details = TRUE, fallback = TRUE)
  expect_equal(out$quantile, c(0, 123, Inf))
  expect_equal(out$method, c("boundary", "fallback", "boundary"))
  expect_match(out$reason[2], "work limit")
  expect_length(calls, 1)
  expect_equal(calls[[1]], c(log(.5), 1, 1.5, 1))
})

test_that("mixture CDF provides stable tails and correct support", {
  q <- c(-Inf, -1, 0, 1, 10, Inf, NA_real_)
  f <- ptweedie_mixture(q, 1, 1.5, 1)
  s <- ptweedie_mixture(q, 1, 1.5, 1, lower_tail = FALSE)
  expect_equal(f + s, c(rep(1, 6), NA))
  expect_equal(f[c(1, 2, 3, 6)], c(0, 0, exp(-2), 1))
  expect_equal(ptweedie_mixture(c(-1, 0, 1), 0, 1.5, 1), c(0, 1, 1))
  expect_equal(ptweedie_mixture(numeric(), numeric(), 1.5, numeric()), numeric())
  if (requireNamespace("tweedie", quietly = TRUE)) for (power in c(1.1, 1.5, 1.9)) {
    y <- c(.01, .5, 2, 10)
    expect_equal(ptweedie_mixture(y, 1, power, 1),
      tweedie::ptweedie(y, mu = 1, xi = power, phi = 1), tolerance = 1e-7)
  }
  # Ordinary probability rounds to one; the log survival must remain finite.
  y <- qtweedie_mixture(-1e-30, 1, 1.5, 1, log_p = TRUE)
  expect_equal(ptweedie_mixture(y, 1, 1.5, 1, lower_tail = FALSE, log_p = TRUE),
    log(1e-30), tolerance = 1e-9)
})

test_that("diagnostic tolerances keep normal-score errors below 1e-4", {
  for (power in c(1.1, 1.5, 1.9)) {
    # Includes probabilities much further into the tails than ordinary QQ ranks.
    lp <- c(log(1e-6), log(.1), log(.5), log(.9), -1e-6, -1e-12)
    actual <- qtweedie_mixture(lp, 1, power, 1, log_p = TRUE,
      cdf_tol = 1e-7, probability_tol = 1e-5, quantile_tol = 1e-6)
    positive <- actual > 0
    lf <- ptweedie_mixture(actual, 1, power, 1, log_p = TRUE)
    ls <- ptweedie_mixture(actual, 1, power, 1, log_p = TRUE, lower_tail = FALSE)
    z <- ifelse(lf <= ls, qnorm(lf, log.p = TRUE),
      qnorm(ls, log.p = TRUE, lower.tail = FALSE))
    target <- qnorm(lp, log.p = TRUE)
    expect_lt(max(abs(z[positive] - target[positive])), 1e-4)
    # Relaxing CDF accuracy separately must also preserve normal-score residuals.
    relaxed <- ptweedie_mixture(actual, 1, power, 1, log_p = TRUE,
      lower_tail = FALSE, cdf_tol = 1e-7, probability_tol = 1e-5)
    expect_lt(max(abs(qnorm(relaxed[positive], log.p = TRUE, lower.tail = FALSE) -
      z[positive])), 1e-4)
  }
})

test_that("mixture CDF can serve the existing PIT and quantile residual path", {
  fam <- mgcv::Tweedie(p = 1.5)
  y <- c(0, .1, 1, 10)
  baseline <- do_quantile_residuals(y, rep(1, 4), rep(1, 4), 1, fam, "pit")
  fam$cdf <- function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    ptweedie_mixture(q, mu, 1.5, scale, log_p = log_p, lower_tail = lower_tail,
      cdf_tol = 1e-7, probability_tol = 1e-5)
  }
  actual <- do_quantile_residuals(y, rep(1, 4), rep(1, 4), 1, fam, "pit")
  expect_equal(actual, baseline, tolerance = 1e-6)
  z <- do_quantile_residuals(y, rep(1, 4), rep(1, 4), 1, fam, "quantile")
  expect_equal(z, qnorm(baseline), tolerance = 1e-5)
})

test_that("relaxed roots refine steep tails without invoking inversion", {
  local_mocked_bindings(tweedie_quantile_fallback = function(...) {
    stop("Unexpected inversion fallback")
  })
  for (power in c(1.01, 1.5, 1.99)) {
    for (phi in c(.001, 1, 100)) {
      result <- qtweedie_mixture(c(log(.1), log(.9), -1e-12), 1, power, phi,
        log_p = TRUE, cdf_tol = 1e-7, probability_tol = 1e-5,
        quantile_tol = 1e-6, details = TRUE)
      expect_true(all(result$method %in% c("atom", "mixture", "underflow")))
      expect_true(all(result$log_tail_error[result$method == "mixture"] <= 1e-5))
    }
  }
})

test_that("positive subnormal quantiles retain their logarithm", {
  result <- qtweedie_mixture(-100 + 1e-8, 1, 1.99, 1,
    log_p = TRUE, details = TRUE)
  expect_equal(result$method, "underflow")
  expect_equal(result$quantile, 0)
  expect_true(is.finite(result$log_quantile))
  expect_lt(result$log_quantile, log(.Machine$double.xmin))
})

test_that("fallback limitations are explicit for unrepresentable probabilities", {
  skip_if_not_installed("tweedie")
  expect_warning(result <- tweedie_quantile_fallback(-1e-30, 1, 1.5, 1),
    "cannot represent")
  expect_true(is.na(result))
  expected <- tweedie::qtweedie(.5, mu = 1, xi = 1.5, phi = 1)
  actual <- qtweedie_mixture(.5, 1, 1.5, 1, max_terms = 1, details = TRUE, fallback = TRUE)
  expect_equal(actual$method, "fallback")
  expect_equal(actual$quantile, expected)
})

test_that("small-dispersion CDF agrees with the independent series evaluator", {
  skip_if_not_installed("tweedie")
  # In tweedie 3.1.0 the inversion evaluator differs by about 6e-5 here;
  # both the package's series evaluator and an over-wide direct sum agree.
  y <- 1.7295884004568
  mu <- 2.10911548192
  expected <- tweedie::ptweedie_series(y, mu = mu, power = 1.5, phi = .01)
  actual <- ptweedie_mixture(y, mu, 1.5, .01)
  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("default quantiles return capped approximations without inversion", {
  local_mocked_bindings(tweedie_quantile_fallback = function(...) {
    stop("Unexpected expensive inversion")
  })
  expect_warning(result <- qtweedie_mixture(c(.5, .9), 1, 1.5, 1,
    max_terms = 5, details = TRUE), "fallback = TRUE")
  expect_equal(result$method, rep("approximate", 2))
  expect_equal(result$terms, c(5, 5))
  expect_true(all(is.finite(result$quantile)))
  # With lambda=2 and a five-term budget the retained counts are 1:5.
  # Verify the returned roots invert the actual partial sums, not a normalised
  # substitute distribution. The second root uses direct survival probability.
  k <- 1:5
  q <- result$quantile
  expect_equal(exp(-2) + sum(dpois(k, 2) * pgamma(q[1], k, scale = .5)),
    .5, tolerance = 1e-9)
  expect_equal(sum(dpois(k, 2) * pgamma(q[2], k, scale = .5, lower.tail = FALSE)),
    .1, tolerance = 1e-9)
  expect_equal(exp(result$log_omitted_mass), rep(ppois(5, 2, lower.tail = FALSE), 2))

  # An insufficient partial mass cannot yield a finite root for this target.
  expect_warning(result <- qtweedie_mixture(.5, 1, 1.5, 1,
    max_terms = 1, details = TRUE), "NA where no estimate exists")
  expect_true(is.na(result$quantile))
  expect_equal(result$method, "unresolved")
  expect_match(result$reason, "retained mixture mass")
})

test_that("CDF default returns literal partial sums in either tail", {
  local_mocked_bindings(tweedie_cdf_fallback = function(...) {
    stop("Unexpected expensive inversion")
  })
  expect_warning(f <- ptweedie_mixture(c(1, 2), 1, 1.5, 1, max_terms = 1),
    "partial sums")
  expect_warning(s <- ptweedie_mixture(c(1, 2), 1, 1.5, 1, max_terms = 1,
    lower_tail = FALSE), "fallback = TRUE")
  # Retain the Poisson mode k=2, rather than count one or a renormalised weight.
  expect_equal(f, exp(-2) + dpois(2, 2) * pgamma(c(1, 2), 2, scale = .5))
  expect_equal(s, dpois(2, 2) * pgamma(c(1, 2), 2, scale = .5, lower.tail = FALSE))
  expect_equal(f + s, rep(exp(-2) + dpois(2, 2), 2))
  expect_warning(ls <- ptweedie_mixture(c(1, 2), 1, 1.5, 1, max_terms = 1,
    lower_tail = FALSE, log_p = TRUE), "partial sums")
  expect_equal(ls, log(s))
})

test_that("CDF inversion requires explicit opt-in and only unresolved values", {
  calls <- list()
  local_mocked_bindings(tweedie_cdf_fallback = function(q, mu, power, phi) {
    calls[[length(calls) + 1L]] <<- c(q, mu, power, phi)
    .75
  })
  expect_warning(f <- ptweedie_mixture(c(-1, 0, 1, Inf), 1, 1.5, 1,
    max_terms = 1, fallback = TRUE), "requested ordinary-CDF fallback")
  expect_equal(f, c(0, exp(-2), .75, 1))
  expect_length(calls, 1)
  expect_equal(calls[[1]], c(1, 1, 1.5, 1))
  expect_silent(ptweedie_mixture(1, 1, 1.5, 1, fallback = TRUE))
  expect_length(calls, 1)
})

test_that("large-rate partial sums stay within the budget", {
  lambda <- 1e7
  prepared <- tweedie_mixture_prepare(lambda, 1, log(1e-12), max_terms = 11)
  expect_equal(prepared$terms, 11)
  expect_match(prepared$reason, "work limit")
  k <- (lambda - 5):(lambda + 5)
  expect_equal(exp(prepared$evaluate(0)),
    sum(dpois(k, lambda) * pgamma(1, k, scale = 1/lambda)))
})

test_that("accuracy failures retain estimates and invalid parameters cannot opt in", {
  local_mocked_bindings(
    tweedie_mixture_quantile_one = function(...) list(root = log(2), terms = 3,
      log_omitted = -20, log_error = .01, reason = "accuracy limit"),
    tweedie_quantile_fallback = function(...) stop("Unexpected expensive inversion")
  )
  expect_warning(q <- qtweedie_mixture(.9, 1, 1.5, 1, details = TRUE),
    "Accuracy is not guaranteed")
  expect_equal(q$quantile, 2)
  expect_equal(q$method, "approximate")
  for (bad in list(NA, 1, "TRUE", c(TRUE, FALSE), NULL)) {
    expect_error(qtweedie_mixture(.5, 1, 1.5, 1, fallback = bad), "fallback")
    expect_error(ptweedie_mixture(1, 1, 1.5, 1, fallback = bad), "fallback")
  }
})
