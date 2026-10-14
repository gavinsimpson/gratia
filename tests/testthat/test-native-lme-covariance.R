test_that("native blocked covariance uses its fitted predictor column order", {
  skip_if_not_installed("nlme")
  set.seed(212)
  d <- data.frame(g = factor(rep(1:20, each = 10)), x = rnorm(200), z = rnorm(200))
  b <- matrix(rnorm(60), 20, 3)
  d$y <- 1 + d$x + .2 * d$z + 2 * b[d$g, 1] +
    b[d$g, 2] * d$x + .5 * b[d$g, 3] * d$z + rnorm(200, sd = .4)
  m <- nlme::lme(y ~ x + z,
    random = list(g = nlme::pdBlocked(list(nlme::pdIdent(~x - 1),
      nlme::pdSymm(~1 + z)))), data = d)
  conditional <- lme_diagnostic_model(m)
  expect_equal(conditional$fitted.values, as.vector(fitted(m)))
  expect_equal(conditional$distribution$cdf(conditional$y),
    pnorm(conditional$y, fitted(m), sigma(m)))
  marginal <- lme_diagnostic_model(m, "marginal")
  group <- which(d$g == levels(d$g)[1L])
  covariance <- as.matrix(nlme::getVarCov(m, individuals = levels(d$g)[1L],
    type = "marginal")[[1L]])
  expect_equal(unname(marginal$distribution$variance[group]), unname(diag(covariance)))
  draws <- replicate(4000, marginal$distribution$rd() - marginal$fitted.values)
  expect_equal(unname(cov(t(draws[group, ]))), unname(covariance), tolerance = .08)
  joint <- lme_joint_diagnostic_draws(m, n = 6000L)
  k <- length(nlme::fixef(m))
  empirical <- cov(t(as.matrix(joint$perturbation[seq_len(k), ])))
  expect_equal(unname(empirical), unname(vcov(m)), tolerance = .05)
  for (conditioning in c("conditional", "marginal")) {
    raw <- posterior_samples(m, n = 2L, seed = 17, conditioning = conditioning)
    expect_equal(nrow(raw), 400L)
    expect_true(all(is.finite(raw$.response)))
    expect_s3_class(posterior_predictive_plot(m, n = 2L, seed = 17,
      conditioning = conditioning), "ggplot")
  }
})

test_that("native mixed covariance preserves fitted factor contrasts", {
  skip_if_not_installed("nlme")
  withr::local_options(contrasts = c("contr.treatment", "contr.poly"))
  set.seed(317)
  d <- data.frame(g = factor(rep(1:20, each = 12)),
    f = factor(rep(rep(c("A", "B", "C"), 4), 20)), x = rnorm(240))
  b <- matrix(rnorm(60), 20, 3)
  d$y <- 1 + d$x + b[d$g, 1] + (d$f == "B") * 2 * b[d$g, 2] +
    (d$f == "C") * .5 * b[d$g, 3] + rnorm(240, sd = .3)
  m <- nlme::lme(y ~ x + f, random = ~f | g, data = d)
  variance <- lme_random_error(m)$variance
  posterior <- withr::with_seed(7, lme_joint_diagnostic_draws(m, 3L))
  withr::local_options(contrasts = c("contr.sum", "contr.poly"))
  expect_equal(lme_random_error(m)$variance, variance)
  changed <- withr::with_seed(7, lme_joint_diagnostic_draws(m, 3L))
  expect_equal(changed$design, posterior$design)
  expect_equal(changed$perturbation, posterior$perturbation)
})

test_that("native subset and omissions preserve fitted rows", {
  skip_if_not_installed("nlme")
  set.seed(615)
  d <- data.frame(g = factor(rep(1:20, each = 10)), id = 1:200,
    x = rnorm(200))
  d$y <- d$x + rep(rnorm(20), each = 10) + rnorm(200, sd = .3)
  d$y[90L] <- NA_real_
  m <- nlme::lme(y ~ x, random = ~1 | g, data = d,
    subset = id > 40, na.action = na.omit)
  fitted_data <- lme_fitting_data(m)
  expect_equal(fitted_data$id, d$id[d$id > 40 & !is.na(d$y)])
  expect_equal(rownames(fitted_data), rownames(m$groups))
  for (conditioning in c("conditional", "marginal")) {
    proxy <- lme_diagnostic_model(m, conditioning)
    expect_equal(unname(proxy$y), fitted_data$y)
    expect_equal(proxy$fitted.values, as.vector(m$fitted[,
      if (conditioning == "conditional") 2L else 1L]))
    raw <- posterior_samples(m, n = 2L, seed = 17, conditioning = conditioning)
    expect_equal(nrow(raw), 2L * nrow(fitted_data))
    expect_true(all(is.finite(raw$.response)))
    expect_s3_class(posterior_predictive_plot(m, n = 2L, seed = 17,
      conditioning = conditioning), "ggplot")
  }
})

test_that("native subset restores the fitted factor levels", {
  skip_if_not_installed("nlme")
  set.seed(425)
  d <- data.frame(g = factor(rep(1:20, each = 12)),
    f = factor(rep(rep(c("A", "B", "C"), 4), 20)), x = rnorm(240))
  b <- matrix(rnorm(40), 20, 2)
  d$y <- d$x + b[d$g, 1L] + (d$f == "B") * b[d$g, 2L] + rnorm(240, sd = .3)
  m <- nlme::lme(y ~ x + f, random = ~f | g, data = d, subset = f != "C")
  expect_equal(levels(lme_fitting_data(m)$f), c("A", "B"))
  for (conditioning in c("conditional", "marginal")) {
    proxy <- lme_diagnostic_model(m, conditioning)
    expect_equal(unname(proxy$y), d$y[d$f != "C"])
    group_data <- droplevels(d[d$g == "1" & d$f != "C", ])
    z <- model.matrix(~f, data = group_data, contrasts.arg = m$contrasts)
    covariance <- diag(sigma(m)^2, nrow(group_data))
    if (conditioning == "marginal") covariance <- covariance +
      z %*% (nlme::pdMatrix(m$modelStruct$reStruct[[1L]]) * sigma(m)^2) %*% t(z)
    expect_equal(unname(proxy$distribution$variance[seq_len(8L)]),
      unname(diag(covariance)))
    raw <- posterior_samples(m, n = 2L, seed = 17, conditioning = conditioning)
    expect_equal(nrow(raw), 320L)
    expect_true(all(is.finite(raw$.response)))
  }
})

test_that("factor-binomial mixed laws retain one trial despite frequency weights", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")
  set.seed(536)
  d <- data.frame(g = factor(rep(1:20, each = 10)), x = rnorm(200), w = 4L)
  d$y <- factor(rbinom(200, 1, plogis(d$x + rep(rnorm(20, sd = .5), each = 10))),
    labels = c("no", "yes"))
  mer <- lme4::glmer(y ~ x + (1 | g), data = d, weights = w, family = binomial())
  pql <- suppressMessages(mgcv::gamm(y ~ x, random = list(g = ~1), data = d,
    weights = w, family = binomial()))
  for (proxy in list(mer_diagnostic_model(mer), gamm_diagnostic_model(pql))) {
    mu <- proxy$fitted.values
    expect_equal(proxy$distribution$cdf(rep(.5, 200)), 1 - mu)
    expect_equal(proxy$distribution$variance, mu * (1 - mu))
    draws <- replicate(10L, proxy$distribution$rd())
    expect_true(all(draws %in% c(0, 1)))
    expect_equal(unname(proxy$prior.weights), rep(4, 200))
  }
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  for (conditioning in c("conditional", "marginal")) {
    raw <- posterior_samples(pql, n = 3L, seed = 42, conditioning = conditioning)
    expect_true(all(raw$.response %in% c(0, 1)))
  }
})
