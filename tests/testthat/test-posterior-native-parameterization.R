test_that("glmm posterior responses retain native dispersion parameterizations", {
  skip_if_not_installed("glmmTMB")
  withr::local_seed(871)
  d <- data.frame(x = rep(c(0, 1), each = 40))
  # Fix every fitted parameter to isolate the response law from coefficient
  # uncertainty. Native predictions supply the independent likelihood oracle.
  testthat::local_mocked_bindings(diagnostic_joint_parameter_draws = function(p, n, ...) {
    matrix(rep(p, n), nrow = n, byrow = TRUE)
  })
  for (name in c("gaussian", "Gamma", "lognormal")) {
    d$y <- switch(name,
      gaussian = 2 + d$x + rnorm(nrow(d), sd = .25 * exp(d$x)),
      Gamma = rgamma(nrow(d), shape = 2 + d$x, scale = exp(.2 + d$x) / (2 + d$x)),
      lognormal = rlnorm(nrow(d), meanlog = .2 + d$x, sdlog = .2 + .1 * d$x))
    family <- switch(name, gaussian = gaussian(), Gamma = Gamma(link = "log"),
      lognormal = glmmTMB::lognormal())
    m <- glmmTMB::glmmTMB(y ~ x, dispformula = ~x, family = family, data = d)
    mu <- as.numeric(predict(m, type = "conditional"))
    phi <- as.numeric(predict(m, type = "disp"))
    samples <- posterior_samples(m, n = 4L, seed = 11)
    oracle <- withr::with_seed(11, {
      unlist(lapply(seq_len(4L), function(i) switch(name,
        gaussian = rnorm(nrow(d), mean = mu, sd = phi),
        Gamma = rgamma(nrow(d), shape = 1 / phi^2, scale = mu * phi^2),
        lognormal = {
          sdlog <- sqrt(log1p((phi / mu)^2))
          rlnorm(nrow(d), meanlog = log(mu) - sdlog^2 / 2, sdlog = sdlog)
        })))
    })
    expect_equal(samples$.response, oracle, tolerance = 1e-8, info = name)
    small <- posterior_predictive_plot(m, n = 4L, seed = 11)
    expect_null(small$labels$subtitle)
    expect_identical(small$labels$caption, "Conditioning: conditional")
    marginal <- diagnostic_posterior_observed(m, conditioning = "marginal")
    expect_match(attr(marginal, "diagnostic_description"),
      "effect covariance and other family parameters plugged in")
    streamed <- posterior_predictive_plot(m, n = 4L, seed = 11, scalable = TRUE)
    expect_equal(lapply(small$layers, `[[`, "data"),
      lapply(streamed$layers, `[[`, "data"), info = name)
  }
})

test_that("weighted factor binomial PPC retains Bernoulli observation encoding", {
  withr::local_seed(13)
  d <- data.frame(x = runif(70), weights = rep(c(2, 4), 35))
  d$y <- factor(rbinom(nrow(d), 1, plogis(d$x)))
  m <- glm(y ~ x, data = d, weights = weights, family = binomial())
  testthat::local_mocked_bindings(diagnostic_gaussian_draws = function(n, mu, V) {
    matrix(rep(mu, n), nrow = n, byrow = TRUE)
  })
  raw <- posterior_samples(m, n = 4L, seed = 9)
  # Native simulation returns factor labels; compare their numeric 0/1 codes.
  expected <- as.numeric(as.matrix(simulate(m, nsim = 4L, seed = 9)))
  expect_equal(raw$.response, expected)
  expect_true(all(raw$.response %in% c(0, 1)))

  g <- mgcv::gam(y ~ s(x, k = 4), data = d, weights = weights, family = binomial())
  beta <- matrix(rep(coef(g), 4), nrow = 4L, byrow = TRUE)
  raw <- posterior_samples(g, n = 4L, seed = 9, method = "user", draws = beta)
  native <- withr::with_seed(9, rbinom(4L * nrow(d), 1, rep(fitted(g), 4L)))
  expect_equal(raw$.response, unname(native))
  expect_equal(as.numeric(as.matrix(simulate(g, nsim = 4L, seed = 9))), unname(native))
})

test_that("explicit GAM sampling weights override fitted trial counts and retain exclusions", {
  withr::local_seed(17)
  d <- data.frame(x = runif(70), trials = 3)
  d$y <- rbinom(nrow(d), d$trials, plogis(d$x)) / d$trials
  d$x[4] <- NA_real_
  g <- mgcv::gam(y ~ s(x, k = 4), data = d, weights = trials,
    family = binomial(), na.action = na.exclude)
  beta <- matrix(rep(coef(g), 2), nrow = 2L, byrow = TRUE)
  actual <- posterior_samples(g, n = 2L, seed = 9, weights = 9,
    method = "user", draws = beta)
  mu <- fitted(g)[!is.na(fitted(g))]
  expected <- withr::with_seed(9, {
    values <- matrix(NA_real_, nrow(d), 2)
    values[-4, ] <- matrix(rbinom(2 * length(mu), 9, rep(mu, 2)) / 9, length(mu))
    as.numeric(values)
  })
  expect_equal(actual$.response, expected)
  expect_equal(as.numeric(as.matrix(simulate(g, nsim = 2L, seed = 9, weights = 9))), expected)
  # Default weights continue to describe the numeric proportion's three trials.
  native <- simulate(g, nsim = 2L, seed = 9)
  expect_true(all((as.numeric(as.matrix(native)) * 3) %in% 0:3 | is.na(as.matrix(native))))
})

test_that("AR1 GAM draws honour explicit precision weights without losing correlation", {
  withr::local_seed(27)
  d <- data.frame(x = runif(70), y = rnorm(70), start = c(TRUE, rep(FALSE, 34), TRUE, rep(FALSE, 34)))
  b <- mgcv::bam(y ~ s(x, k = 4), data = d, rho = .4, AR.start = start, discrete = TRUE)
  beta <- matrix(coef(b), nrow = 1L)
  expected <- withr::with_seed(9, {
    error <- rnorm(nrow(d))
    for (i in seq_len(nrow(d))) if (!d$start[i])
      error[i] <- .4 * error[i - 1L] + sqrt(1 - .4^2) * error[i]
    as.numeric(fitted(b)) + sqrt(b$sig2 / 9) * error
  })
  actual <- posterior_samples(b, n = 1L, seed = 9, weights = 9,
    method = "user", draws = beta)
  expect_equal(actual$.response, expected, tolerance = 1e-8)
  expect_equal(as.numeric(simulate(b, nsim = 1L, seed = 9, weights = 9)[[1]]), expected)
})
