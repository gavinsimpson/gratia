test_that("scat quantiles and CDF use the fitted sigma, not dispersion", {
  p <- c(0, .1, .5, .9, 1)
  mu <- seq_along(p)
  fam <- fix_family_qf(mgcv::scat(theta = c(5, 2)))
  expected <- mu + 2 * qt(p, df = 5)
  expect_equal(fam$qf(p, mu, wt = 3, scale = 17), expected)
  expect_equal(fam$qf(log(p), mu, 3, 17, log_p = TRUE), expected)
  fam <- fix_family_cdf(fam)
  expect_equal(fam$cdf(mu + 1, mu, 3, 17), pt(.5, 5) + mu * 0)
})

test_that("Tweedie factories handle fixed and estimated powers and endpoints", {
  p <- c(.1, .4, .9)
  mu <- c(1, 2, 3)
  phi <- .7
  for (power in c(1.2, 1.5, 1.8)) {
    expected <- tweedie::qtweedie(p, mu = mu, phi = phi, xi = power)
    for (fam in list(mgcv::tw(theta = power), mgcv::Tweedie(p = power))) {
      fam <- fix_family_qf(fam)
      expect_equal(fam$qf(p, mu, 1, phi), expected)
      expect_equal(fam$qf(log(p), mu, 1, phi, log_p = TRUE), expected)
      expect_equal(fam$qf(c(0, 1), mu = 1, wt = 1, scale = phi), c(0, Inf))
      expect_equal(fam$qf(c(-Inf, 0), 1, 1, phi, log_p = TRUE), c(0, Inf))
      expect_equal(fam$qf(c(NA_real_, 0), 1, 1, phi), c(NA_real_, 0))
      expect_equal(fam$qf(-1000, 1, 1, phi, log_p = TRUE), 0)
      expect_warning(q <- fam$qf(c(-.1, 1.1), 1, 1, phi), "NaNs")
      expect_true(all(is.na(q)))
    }
  }
  # The Gamma limit has a native quantile function with log-tail support.
  f <- fix_family_qf(mgcv::Tweedie(p = 2))$qf
  expect_equal(f(log(p), mu, 1, phi, log_p = TRUE),
    qgamma(p, shape = 1 / phi, scale = mu * phi))
})

test_that("GEV quantiles use log scale and the correct shape-dependent support", {
  p <- c(.1, .4, .9)
  mu <- cbind(c(1, 2, 3), log(c(.5, 2, 4)), c(.2, -.3, 0))
  expected <- c(
    1 + .5 / .2 * ((-log(p[1]))^(-.2) - 1),
    2 + 2 / (-.3) * ((-log(p[2]))^.3 - 1),
    3 - 4 * log(-log(p[3]))
  )
  fam <- fix_family_qf(mgcv::gevlss())
  expect_equal(fam$qf(p, mu, wt = 1, scale = 1), expected)
  expect_equal(fam$qf(log(p), mu, 1, 1, log_p = TRUE), expected)
  expect_equal(fam$qf(0, mu, 1, 1), c(1 - .5/.2, -Inf, -Inf))
  expect_equal(fam$qf(1, mu, 1, 1), c(Inf, 2 - 2/(-.3), Inf))
  expect_equal(fam$qf(.5, mu, 1, 1), fam$qf(rep(.5, 3), mu, 1, 1))
  expect_true(all(is.na(fam$qf(NA_real_, mu, 1, 1))))
  expect_warning(q <- fam$qf(c(-.1, 1.1, .5), mu, 1, 1), "NaNs")
  expect_true(all(is.na(q[1:2])))

  # Near zero shape should approach the exact Gumbel limit without cancellation.
  tiny <- cbind(rep(0, 3), log(2), c(-1e-12, 0, 1e-12))
  expect_equal(fam$qf(.8, tiny, 1, 1), rep(-2 * log(-log(.8)), 3),
    tolerance = 1e-10)
  fam <- fix_family_cdf(fam)
  expect_equal(fam$cdf(expected, mu, 1, 1), p)
  expect_equal(fam$cdf(expected, mu, 1, 1, log_p = TRUE), log(p))
  expect_equal(fam$cdf(c(-Inf, Inf, Inf), mu, 1, 1), c(0, 1, 1))
  expect_equal(fam$cdf(c(-10, 20, 0), mu, 1, 1)[1:2], c(0, 1))
})

test_that("location-scale quantiles support log probabilities and endpoints", {
  p <- c(0, .1, .5, .9, 1)
  mu <- cbind(seq_along(p), log(seq_along(p) + 1))
  cases <- list(
    list(fam = mgcv::gaulss(), expected = qnorm(p, mu[, 1], 1 / mu[, 2])),
    list(fam = mgcv::gumbls(), expected = mu[, 1] - exp(mu[, 2]) *
      (0.577215664901533 + log(-log(p)))),
    list(fam = mgcv::gammals(), expected = qgamma(p,
      shape = exp(-mu[, 2]), scale = mu[, 1] * exp(mu[, 2])))
  )
  for (case in cases) {
    f <- fix_family_qf(case$fam)$qf
    expect_equal(f(p, mu, 1, 1), case$expected)
    expect_equal(f(log(p), mu, 1, 1, log_p = TRUE), case$expected)
    expect_equal(f(.5, mu, 1, 1), f(rep(.5, nrow(mu)), mu, 1, 1))
  }
})

test_that("hurdle Poisson quantiles invert independently summed probabilities", {
  # Both sides of 0.5 are needed to catch confusing p with presence probability.
  f <- fix_family_qf(mgcv::ziplss())$qf
  for (lambda in c(.1, 5, 20)) {
    for (presence in c(.2, .8)) {
      y <- 0:100
      mass <- c(1 - presence, presence * dpois(y[-1], lambda) /
        (-expm1(-lambda)))
      cdf <- cumsum(mass)
      p <- c(.05, .25, .3, .4, .6, .85, .99)
      expected <- vapply(p, function(prob) y[which(cdf >= prob)[1]], numeric(1))
      mu <- cbind(rep(log(lambda), length(p)), log(-log1p(-presence)))
      expect_equal(f(p, mu, 1, 1), expected)
      expect_equal(f(log(p), mu, 1, 1, log_p = TRUE), expected)
      expect_equal(f(.3, mu, 1, 1), rep(expected[3], length(p)))
      expect_equal(f(0, mu, 1, 1), rep(0, length(p)))
      expect_equal(f(1, mu, 1, 1), rep(Inf, length(p)))
    }
  }
  mu <- cbind(log(c(1e-12, 5, 5)), log(-log1p(-c(.8, .8, .8))))
  expect_equal(f(c(.9, NA, .1), mu, 1, 1), c(1, NA, 0))
  expect_equal(f(-exp(mu[, 2]), mu, 1, 1, log_p = TRUE), rep(0, 3))
  expect_warning(q <- f(c(-.1, 1.1, .3), mu, 1, 1), "NaNs")
  expect_true(all(is.na(q[1:2])))
  logp <- c(-1e-20, -1e-100)
  mu <- cbind(rep(log(5), 2), log(-log(.2)))
  q <- f(logp, mu, 1, 1, log_p = TRUE)
  expect_true(all(is.finite(q)))
  log_target <- log(-expm1(logp))
  log_survival <- function(y) {
    log(.8) + ppois(y, 5, lower.tail = FALSE, log.p = TRUE) - log(-expm1(-5))
  }
  expect_true(all(log_survival(q) <= log_target))
  expect_true(all(log_survival(q - 1) > log_target))
})

test_that("uniform QQ and worm plots use the extended quantile helpers", {
  withr::local_seed(27)
  n <- 120
  # Real fits exercise the family-specific residual methods as well as routing.
  cases <- list(
    list(fam = mgcv::scat(theta = c(5, 2)), y = 1 + 2 * rt(n, 5), nlp = 1),
    list(fam = mgcv::tw(), y = mgcv::rTweedie(rep(2, n), p = 1.5,
      phi = .5), nlp = 1),
    list(fam = mgcv::Tweedie(p = 1.5), y = mgcv::rTweedie(rep(2, n), p = 1.5,
      phi = .5), nlp = 1),
    list(fam = mgcv::gaulss(), y = rnorm(n, 1, 2), nlp = 2),
    list(fam = mgcv::gumbls(), y = 1 - 2 * log(-log(runif(n))), nlp = 2),
    list(fam = mgcv::gammals(), y = rgamma(n, shape = 2, scale = 2), nlp = 2),
    list(fam = mgcv::gevlss(), y = 1 + 2 / .1 * ((-log(runif(n)))^(-.1) - 1),
      nlp = 3),
    list(fam = mgcv::ziplss(), y = ifelse(runif(n) < .3, 0,
      qpois(runif(n, exp(-4), 1), 4)), nlp = 2)
  )
  for (case in cases) {
    # These are routing/finite-output checks, not distributional accuracy
    # estimates. Tweedie quantiles require costly numerical CDF inversion,
    # so a small real fit is sufficient; keep the other family fixtures intact.
    y <- if (family_type(case$fam) == "tweedie") head(case$y, 12L) else case$y
    n_obs <- length(y)
    formula <- if (case$nlp == 1) y ~ 1 else {
      c(list(y ~ 1), rep(list(~1), case$nlp - 1))
    }
    m <- mgcv::gam(formula, data = data.frame(y = y),
      family = case$fam, method = "REML")
    if (family_type(m) == "tweedie") {
      power <- if (is.null(m$family$getTheta)) 1.5 else m$family$getTheta(TRUE)
      p <- c(.2, .5, .8)
      expect_equal(fix_family_qf(m$family)$qf(p, fitted(m)[1:3], 1, m$sig2),
        tweedie::qtweedie(p, mu = fitted(m)[1:3], phi = m$sig2, xi = power))
    }
    types <- c("deviance", "response", "pearson")
    if (family_type(m) == "ziplss") types <- types[1:2]
    # Only general-family residual methods use the replaced response below.
    # Compute their reference quantiles once, independently of residual type.
    if (case$nlp > 1) {
      q <- fix_family_qf(family(m))$qf(ppoints(n_obs), fitted(m),
        m$prior.weights, m$sig2)
      expected_model <- m
      expected_model$y <- q
    }
    for (type in types) {
      out <- qq_uniform(m, n = 1, type = type)
      expect_equal(nrow(out), n_obs)
      expect_true(all(is.finite(out$theoretical)))
      # Standard-family residuals.gam can use stored working residuals;
      # general families compute their residuals directly from the supplied y.
      if (case$nlp > 1) {
        expected <- residuals(expected_model, type = type)
        actual <- compute_residuals(q, fitted(m), m$prior.weights,
          type = type, dev_resid_fun = m$family$residuals,
          var_fun = m$family$variance, na_action = NULL, model = m)
        expect_equal(actual, expected)
      }
    }
    expect_equal(qq_plot(m, method = "uniform", n_uniform = 1)$labels$subtitle,
      "Method: uniform")
    expect_equal(worm_plot(m, method = "uniform", n_uniform = 1)$labels$subtitle,
      "Method: uniform")
  }
})
