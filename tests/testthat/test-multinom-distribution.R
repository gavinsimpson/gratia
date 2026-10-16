multinom_functions <- function(K = 2) {
  fix_family_qf(fix_family_cdf(mgcv::multinom(K = K)))
}

test_that("categorical CDFs accumulate mgcv category probabilities", {
  f <- multinom_functions()
  mu <- log(c(2, 3)) # probabilities 1/6, 2/6, 3/6
  q <- c(-Inf, -1, -.1, 0, .9, 1, 1.9, 2, Inf)
  expected <- c(0, 0, 0, 1/6, 1/6, .5, .5, 1, 1)
  for (lower in c(TRUE, FALSE)) {
    prob <- if (lower) expected else 1 - expected
    expect_equal(f$cdf(q, mu, 4, 9, lower_tail = lower), prob)
    expect_equal(f$cdf(q, mu, 4, 9, log_p = TRUE, lower_tail = lower), log(prob))
  }
  eta <- rbind(log(c(2, 3)), log(c(4, 5)), c(0, 0))
  expect_equal(f$cdf(0, eta, 1, 1), c(1/6, 1/10, 1/3))
  expect_equal(f$cdf(c(0, 1, 2), eta, 1, 1), c(1/6, .5, 1))
  expect_equal(f$cdf(0, matrix(mu, 1), 1, 1), 1/6)
})

test_that("categorical quantiles respect jumps, tails and finite endpoints", {
  f <- multinom_functions()
  mu <- log(c(2, 3))
  p <- c(0, .01, 1/6, 1/6 + 1e-10, .4, .5, .5 + 1e-10, .99, 1)
  expected <- c(0, 0, 0, 1, 1, 1, 2, 2, 2)
  for (lower in c(TRUE, FALSE)) for (logged in c(TRUE, FALSE)) {
    prob <- if (lower) p else 1 - p
    if (logged) prob <- log(prob)
    expect_equal(f$qf(prob, mu, 1, 1, log_p = logged, lower_tail = lower), expected)
  }
  p <- seq(.01, .99, length.out = 100)
  q <- f$qf(p, mu, 1, 1)
  expect_true(all(f$cdf(q, mu, 1, 1) >= p))
  expect_true(all(f$cdf(q - 1, mu, 1, 1) < p))
  expect_equal(f$qf(.4, rbind(mu, c(0, 0)), 1, 1), c(1, 1))
})

test_that("categorical tails retain extreme logits and log probabilities", {
  f <- multinom_functions()
  expect_equal(f$cdf(0, c(1000, 1000), 1, 1, log_p = TRUE), -1000 - log(2))
  expect_equal(f$cdf(1, c(-1000, -1000), 1, 1,
    log_p = TRUE, lower_tail = FALSE), -1000)
  expect_equal(f$cdf(0, c(-700, -700), 1, 1, log_p = TRUE) / (-2 * exp(-700)), 1)
  expect_equal(f$cdf(1, c(1e100, 1e100), 1, 1), .5)
  expect_equal(f$cdf(1, c(-1e100, 1e100), 1, 1, log_p = TRUE), -1e100)
  expect_equal(f$qf(c(-1002, -1000, -1), c(1000, 1000), 1, 1,
    log_p = TRUE), c(0, 1, 1))
  expect_equal(f$qf(c(-1001, -999.5), c(-1000, -1000), 1, 1,
    log_p = TRUE, lower_tail = FALSE), c(2, 1))
  # A large common logit must retain the relative masses of its categories.
  g <- multinom_functions(3)
  expect_equal(g$cdf(2, rep(1e100, 3), 1, 1), 2/3)
  expect_equal(g$qf(-1000, c(-1000, -1000, -1000), 1, 1,
    log_p = TRUE, lower_tail = FALSE), 2)
})

test_that("binary multinom functions agree with Bernoulli distributions", {
  f <- multinom_functions(1)
  eta <- c(-20, -2, 0, 2, 20)
  for (lower in c(TRUE, FALSE)) for (logged in c(TRUE, FALSE)) {
    expect_equal(f$cdf(0, eta, 7, 3, log_p = logged, lower_tail = lower),
      pbinom(0, 1, plogis(eta), log.p = logged, lower.tail = lower),
      tolerance = 1e-7)
    p <- c(.01, .1, .5, .9, .99)
    if (logged) p <- log(p)
    expect_equal(f$qf(p, eta, 7, 3, log_p = logged, lower_tail = lower),
      qbinom(p, 1, plogis(eta), log.p = logged, lower.tail = lower))
  }
  expect_equal(f$qf(c(0, 1), 1000, 1, 1), c(0, 1))
})

test_that("categorical functions handle missing, invalid and shaped inputs", {
  f <- multinom_functions()
  eta <- rbind(c(0, 0), c(NA, 0), c(0, 0))
  expect_equal(f$cdf(c(0, 1, NA), eta, 1, 1), c(1/3, NA, NA))
  expect_equal(f$qf(c(.1, .5, NA), eta, 1, 1), c(0, NA, NA))
  expect_true(is.nan(f$cdf(0, c(Inf, 0), 1, 1)))
  expect_true(is.na(f$qf(.5, c(Inf, 0), 1, 1)))
  expect_warning(q <- f$qf(c(-.1, 1.1, NA, NaN), c(0, 0), 1, 1), "NaNs")
  expect_true(all(is.na(q)))
  expect_warning(q <- f$qf(c(.1, Inf), c(0, 0), 1, 1, log_p = TRUE), "NaNs")
  expect_true(all(is.na(q)))
  expect_error(f$cdf(0, matrix(0, 2, 3), 1, 1), "K logits")
  expect_error(f$qf(.5, 1:3, 1, 1), "K logits")
  expect_error(f$cdf(0:1, eta, 1, 1), "per observation")
  expect_error(f$qf(c(.1, .5), eta, 1, 1), "per observation")
  expect_equal(f$cdf(numeric(), c(0, 0), 1, 1), numeric())
  expect_equal(f$qf(numeric(), c(0, 0), 1, 1), numeric())
  original <- mgcv::multinom(K = 2)
  expect_identical(f$rd, original$rd)
  original$cdf <- function(...) 17
  original$qf <- function(...) 23
  fixed <- fix_family_qf(fix_family_cdf(fix_family_rd(original)))
  expect_identical(fixed$cdf, original$cdf)
  expect_identical(fixed$qf, original$qf)
  expect_identical(fixed$rd, original$rd)
})

test_that("fitted categorical distributions randomise across the observed mass", {
  withr::local_seed(183)
  dat <- data.frame(x = runif(120), y = sample(0:2, 120, replace = TRUE))
  m <- mgcv::gam(list(y ~ x, ~x), data = dat, family = mgcv::multinom(K = 2))
  d <- response_distribution(m)
  prob <- predict(m, type = "response")
  cum <- cbind(0, prob[, 1], prob[, 1] + prob[, 2], 1)
  lower <- cum[cbind(seq_len(nrow(dat)), dat$y + 1)]
  upper <- cum[cbind(seq_len(nrow(dat)), dat$y + 2)]
  expect_equal(d$cdf(dat$y), upper)
  expect_equal(distribution_cdf_left(d, dat$y), lower)
  expect_equal(d$cdf_left(dat$y + .5), d$cdf(dat$y))
  expect_equal(d$cdf_left(dat$y, lower_tail = FALSE), 1 - lower)
  expect_equal(d$cdf_left(dat$y, log_p = TRUE), log(lower))
  expect_equal(d$bounds(dat$y), list(lower = dat$y - 1, upper = dat$y))
  f <- multinom_functions()
  expect_equal(d$cdf(dat$y), f$cdf(dat$y, m$linear.predictors, 1, 1))
  expect_equal(d$qf(.4), f$qf(.4, m$linear.predictors, 1, 1))
  pit <- quantile_residuals(m, seed = 76)
  expected <- withr::with_seed(76, runif(nrow(dat), lower, upper))
  expect_equal(unname(pit), expected)
  expect_equal(unname(quantile_residuals(m, type = "quantile", seed = 76)), qnorm(expected))
  expect_true(all(pit >= lower & pit <= upper))
  expect_equal(d$mean, as.vector(prob %*% 0:2))
  # The native response simulator continues to receive logits.
  expect_equal(withr::with_seed(2, d$rd()),
    withr::with_seed(2, m$family$rd(m$fitted.values, m$prior.weights, m$sig2)))
  expect_s3_class(qq_plot(m, method = "uniform", n_simulate = 3, seed = 2), "ggplot")
  expect_s3_class(qq_plot(m, type = "quantile", seed = 2), "ggplot")
  expect_s3_class(worm_plot(m, type = "pit", seed = 2), "ggplot")

  # Relabelling changes intervals, but each enumeration still partitions [0, 1]
  # into intervals with lengths equal to the corresponding category masses.
  order <- c(3, 1, 2)
  relabelled <- prob[, order]
  eta <- log(relabelled[, -1] / relabelled[, 1])
  for (j in 0:2) {
    lo <- f$cdf(j - 1, eta, 1, 1)
    hi <- f$cdf(j, eta, 1, 1)
    expect_equal(hi - lo, unname(relabelled[, j + 1]))
  }
  changed_y <- match(dat$y + 1, order) - 1
  changed_lower <- f$cdf(changed_y - 1, eta, 1, 1)
  changed_upper <- f$cdf(changed_y, eta, 1, 1)
  expect_equal(changed_upper - changed_lower, upper - lower)
  expect_true(any(abs(changed_lower - lower) > .01))
})

test_that("categorical fitted paths support binary responses and omitted rows", {
  withr::local_seed(184)
  dat <- data.frame(x = runif(80), y = rep(0:1, 40))
  dat$x[4] <- NA_real_
  m <- mgcv::gam(list(y ~ x), data = dat, family = mgcv::multinom(K = 1),
    na.action = na.exclude)
  used <- model_used_rows(m)
  d <- response_distribution(m)
  probability <- predict(used, type = "response")
  expect_equal(d$cdf(0), unname(probability[, 1]))
  expect_equal(d$mean, unname(probability[, 2]))
  expect_equal(d$qf(.4), qbinom(.4, 1, probability[, 2]), ignore_attr = TRUE)
  pit <- quantile_residuals(m, seed = 3)
  expect_identical(which(is.na(pit)), 4L)
  lower <- ifelse(used$y == 0, 0, probability[, 1])
  upper <- ifelse(used$y == 0, probability[, 1], 1)
  expect_equal(unname(pit[-4]), withr::with_seed(3, runif(79, lower, upper)))
  single <- distribution_from_family(m$family,
    matrix(m$linear.predictors[1], nrow = 1), 1, 1)
  expect_length(single$cdf(0), 1)
  expect_equal(single$cdf(0), d$cdf(0)[1])
  expect_equal(single$qf(.4), d$qf(.4)[1])

  custom <- m
  custom$family$cdf <- function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    pbinom(q, 1, plogis(mu), log.p = log_p, lower.tail = lower_tail)
  }
  custom$family$qf <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    qbinom(p, 1, plogis(mu), log.p = log_p, lower.tail = lower_tail)
  }
  expect_equal(response_distribution(custom)$cdf(0), d$cdf(0), ignore_attr = TRUE)
  expect_equal(response_distribution(custom)$qf(.4), d$qf(.4), ignore_attr = TRUE)
})

test_that("categorical randomisation retains tiny observed-category mass", {
  d <- distribution_from_family(mgcv::multinom(K = 2),
    rbind(c(1000, 1000), c(-1000, -1000)), c(1, 1), 1)
  # Category 0 in the first row and category 2 in the second are very rare.
  r <- distribution_quantile_residuals(d, c(0, 2), type = "quantile", seed = 4)
  v <- withr::with_seed(4, runif(2))
  expect_equal(r, c(qnorm(-1000 - log(2) + log(v[1]), log.p = TRUE),
    qnorm(-1000 + log1p(-v[2]), log.p = TRUE, lower.tail = FALSE)))
  expect_true(all(is.finite(r)))
})
