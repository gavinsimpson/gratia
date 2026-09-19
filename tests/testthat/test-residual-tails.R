test_that("continuous normal quantile residuals retain extreme tails", {
  y <- c(-50, -40, -10, 0, 10, 40, 50, NA_real_)
  for (fam in list(gaussian(), mgcv::cnorm(theta = 2))) {
    scale <- if (family_type(fam) == "gaussian") 4 else 1
    expect_equal(do_quantile_residuals(y, 0, 4, scale, fam, "quantile"), y)
    pit <- do_quantile_residuals(y, 0, 4, scale, fam, "pit")
    expect_equal(pit, pnorm(y))
    expect_equal(pit[c(1, 7)], c(0, 1))
  }
  named <- c(left = -40, centre = 0, right = 40)
  expect_equal(do_quantile_residuals(named, 0, 1, 1, gaussian(), "quantile"),
    named)
  y <- c(-1000, 1000)
  fam <- mgcv::clog(theta = 1)
  expected <- c(-1, 1) * qnorm(plogis(-1000, log.p = TRUE),
    lower.tail = FALSE, log.p = TRUE)
  expect_equal(do_quantile_residuals(y, 0, 1, 1, fam, "quantile"), expected)
})

test_that("discrete residuals randomize in log space in both extreme tails", {
  for (fam in list(poisson(), mgcv::cpois(), mgcv::nb(theta = 2),
    mgcv::negbin(theta = 2))) {
    poisson_family <- family_type(fam) %in% c("poisson", "cpois")
    mu <- if (poisson_family) c(1000, 1) else c(1e200, 1)
    y <- c(0, 2000)
    v <- withr::with_seed(42, runif(2))
    # At zero the randomized lower-tail probability is v * P(Y = 0).
    log_mass0 <- if (poisson_family) -mu[1] else 2 * (log(2) - log(2 + mu[1]))
    expected_left <- qnorm(log(v[1]) + log_mass0, log.p = TRUE)
    # For the right tail, use S(y) + (1-v) * P(Y=y), independently of the
    # implementation's interpolation between S(y-1) and S(y).
    log_s <- if (poisson_family) {
      ppois(y[2], mu[2], lower.tail = FALSE, log.p = TRUE)
    } else {
      pnbinom(y[2], size = 2, mu = mu[2], lower.tail = FALSE, log.p = TRUE)
    }
    log_mass <- if (poisson_family) {
      dpois(y[2], mu[2], log = TRUE)
    } else {
      dnbinom(y[2], size = 2, mu = mu[2], log = TRUE)
    }
    log_survival <- log_mass + log(exp(log_s - log_mass) + 1 - v[2])
    expected_right <- qnorm(log_survival, lower.tail = FALSE, log.p = TRUE)
    r <- withr::with_seed(42,
      do_quantile_residuals(y, mu, c(1, 2), 1, fam, "quantile"))
    expect_equal(r, c(expected_left, expected_right))
    expect_true(all(is.finite(r) & abs(r) > 30))
    expect_equal(withr::with_seed(42,
      do_quantile_residuals(y, mu, 1, 1, fam, "pit")), c(0, 1))
    other <- withr::with_seed(43,
      do_quantile_residuals(y, mu, 1, 1, fam, "quantile"))
    expect_true(all(r != other))
  }
})

test_that("censoring intervals retain tail information and randomization", {
  y <- cbind(c(-40, 40, -41, 40, 40, 0), c(-Inf, Inf, -40, 41, 40, 0))
  fam <- mgcv::cnorm(theta = 1)
  r <- withr::with_seed(42, do_quantile_residuals(y, rep(0, 6), 1, 1, fam,
    "quantile"))
  expect_true(all(is.finite(r)))
  expect_lt(r[1], -40)
  expect_gt(r[2], 40)
  expect_true(r[3] > -41 && r[3] < -40)
  expect_true(r[4] > 40 && r[4] < 41)
  expect_equal(r[5:6], c(40, 0))
  other <- withr::with_seed(43, do_quantile_residuals(y, rep(0, 6), 1, 1, fam,
    "quantile"))
  expect_true(all(r[1:4] != other[1:4]))
  expect_equal(r[5:6], other[5:6])
  # The interval spans the entire support, so the PIT is exactly Uniform(0,1).
  full <- cbind(-Inf, Inf)
  expect_equal(withr::with_seed(42,
    do_quantile_residuals(full, 0, 1, 1, fam, "pit")),
    withr::with_seed(42, runif(1)))
})

test_that("ordinary discrete probabilities and public seed handling agree", {
  withr::local_seed(12)
  dat <- data.frame(x = runif(80), exposure = runif(80, 1, 3))
  dat$y <- rpois(80, exp(dat$x) * dat$exposure)
  for (fam in list(poisson(), mgcv::nb(theta = 2), mgcv::negbin(theta = 2))) {
    m <- mgcv::gam(y ~ s(x, k = 5) + offset(log(exposure)), data = dat,
      family = fam, method = "REML")
    cdf <- if (family_type(fam) == "poisson") {
      function(y) ppois(y, fitted(m))
    } else {
      function(y) pnbinom(y, mu = fitted(m), size = 2)
    }
    expected <- withr::with_seed(19, runif(nrow(dat), cdf(dat$y - 1), cdf(dat$y)))
    state <- .Random.seed
    expect_equal(quantile_residuals(m, "pit", seed = 19), expected)
    expect_identical(.Random.seed, state)
    expect_equal(quantile_residuals(m, "quantile", seed = 19), qnorm(expected))
    expect_identical(.Random.seed, state)
  }
  m <- glm(y ~ x, data = dat, family = poisson())
  expect_equal(quantile_residuals(m, "pit", seed = 19),
    withr::with_seed(19, runif(nrow(dat),
      ppois(dat$y - 1, fitted(m)), ppois(dat$y, fitted(m)))))
  expect_equal(quantile_residuals(m, "quantile", seed = 19),
    qnorm(withr::with_seed(19, runif(nrow(dat),
      ppois(dat$y - 1, fitted(m)), ppois(dat$y, fitted(m))))))
})

test_that("grouped binomial residuals randomize over one success", {
  size <- c(1, 5, 10, 20)
  y <- c(0, 2, 8, 20)
  mu <- c(0.3, 0.4, 0.6, 0.8)
  expected <- withr::with_seed(1,
    runif(4, pbinom(y - 1, size, mu), pbinom(y, size, mu)))
  expect_equal(withr::with_seed(1,
    do_quantile_residuals(y / size, mu, size, 1, binomial(), "pit")), expected)
  expect_equal(withr::with_seed(1,
    do_quantile_residuals(y / size, mu, size, 1, binomial(), "quantile")),
    qnorm(expected))
})

test_that("true endpoints and missing values are preserved without clipping", {
  expect_equal(do_quantile_residuals(c(-Inf, Inf, NA), 0, 1, 1,
    gaussian(), "quantile"), c(-Inf, Inf, NA))
  expect_equal(withr::with_seed(1,
    do_quantile_residuals(c(-1, 1, NA), 0, 1, 1, poisson(), "quantile")),
    c(-Inf, Inf, NA))
  expect_equal(logspace_add(c(-Inf, -Inf, 0, NA), c(-Inf, 0, -Inf, 0)),
    c(-Inf, 0, 0, NA))
})

test_that("CDFs without a lower_tail argument keep working", {
  fam <- poisson()
  fam$cdf <- function(q, mu, wt, scale, log_p = FALSE) {
    ppois(q, mu, log.p = log_p)
  }
  y <- c(0, 2, 4)
  expected <- withr::with_seed(1, runif(3, ppois(y - 1, 2), ppois(y, 2)))
  expect_equal(withr::with_seed(1,
    do_quantile_residuals(y, 2, 1, 1, fam, "quantile")), qnorm(expected))
})

test_that("native CDF helpers forward both tails and log probabilities", {
  q <- c(-Inf, 0, 1, 10, Inf)
  cases <- list(
    list(cdf = cdf_poisson, mu = 2, ref = function(lower, log) {
      ppois(q, 2, lower.tail = lower, log.p = log)
    }),
    list(cdf = cdf_gaussian, mu = 2, ref = function(lower, log) {
      pnorm(q, 2, sqrt(3 / 4), lower.tail = lower, log.p = log)
    }),
    list(cdf = make_cdf_cnorm(2), mu = 2, ref = function(lower, log) {
      pnorm(q, 2, 1, lower.tail = lower, log.p = log)
    }),
    list(cdf = make_cdf_clog(2), mu = 2, ref = function(lower, log) {
      plogis(q, 2, 1, lower.tail = lower, log.p = log)
    }),
    list(cdf = cdf_binomial, mu = 0.4, ref = function(lower, log) {
      pbinom(q * 4, 4, 0.4, lower.tail = lower, log.p = log)
    }),
    list(cdf = cdf_gamma, mu = 2, ref = function(lower, log) {
      pgamma(q, shape = 1 / 3, scale = 6, lower.tail = lower, log.p = log)
    }),
    list(cdf = cdf_gaulss, mu = cbind(rep(2, 5), 0.5),
      ref = function(lower, log) {
        pnorm(q, 2, 2, lower.tail = lower, log.p = log)
      }),
    list(cdf = cdf_gammals, mu = cbind(rep(2, 5), log(3)),
      ref = function(lower, log) {
        pgamma(q, shape = 1 / 3, scale = 6, lower.tail = lower, log.p = log)
      }),
    list(cdf = make_cdf_scat(4, 2), mu = 2, ref = function(lower, log) {
      pt((q - 2) / 2, df = 4, lower.tail = lower, log.p = log)
    }),
    list(cdf = make_cdf_nb(2), mu = 2, ref = function(lower, log) {
      pnbinom(q, size = 2, mu = 2, lower.tail = lower, log.p = log)
    }),
    list(cdf = make_cdf_beta(5, 1e-8), mu = 0.4, ref = function(lower, log) {
      pbeta(q, 2, 3, lower.tail = lower, log.p = log)
    })
  )
  for (case in cases) {
    for (lower in c(TRUE, FALSE)) {
      for (log in c(TRUE, FALSE)) {
        expect_equal(case$cdf(q, case$mu, 4, 3,
          log_p = log, lower_tail = lower), case$ref(lower, log))
      }
    }
  }
})

test_that("censored Poisson and logistic intervals work in extreme tails", {
  y <- cbind(c(1000.5, -0.5), c(Inf, 0.5))
  mu <- c(1, 1000)
  fam <- mgcv::cpois()
  r <- withr::with_seed(42,
    do_quantile_residuals(y, mu, 1, 1, fam, "quantile"))
  v <- withr::with_seed(42, runif(2))
  expected <- c(
    qnorm(log1p(-v[1]) + ppois(1000.5, 1, lower.tail = FALSE, log.p = TRUE),
      lower.tail = FALSE, log.p = TRUE),
    qnorm(log(v[2]) - 1000, log.p = TRUE)
  )
  expect_equal(r, expected)
  fam <- mgcv::clog(theta = 1)
  y <- cbind(c(-1001, 1000), c(-1000, 1001))
  r <- withr::with_seed(42,
    do_quantile_residuals(y, c(0, 0), 1, 1, fam, "quantile"))
  lo <- c(qnorm(plogis(-1001, log.p = TRUE), log.p = TRUE),
    qnorm(plogis(1000, lower.tail = FALSE, log.p = TRUE),
      lower.tail = FALSE, log.p = TRUE))
  hi <- c(qnorm(plogis(-1000, log.p = TRUE), log.p = TRUE),
    qnorm(plogis(1001, lower.tail = FALSE, log.p = TRUE),
      lower.tail = FALSE, log.p = TRUE))
  expect_true(all(is.finite(r) & r > lo & r < hi))
})
