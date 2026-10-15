test_that("inlaws count families supply correct randomised response probabilities", {
  cases <- list(
    nbls = list(f = inlaws::nbls(), mu = cbind(c(.5, 2, 4), 2)),
    cmpls = list(f = inlaws::cmpls(), mu = cbind(c(.5, 2, 4), 1.2)),
    pigls = list(f = inlaws::pigls(), mu = cbind(c(.5, 2, 4), .5)),
    zinb = list(f = inlaws::zinb(), mu = cbind(c(.5, 2, 4), .7, 2)),
    zanb = list(f = inlaws::zanb(), mu = cbind(c(.5, 2, 4), .7, 2)),
    cmp = list(f = inlaws::cmp(theta = 1.2), mu = c(.5, 2, 4)),
    pig = list(f = inlaws::pig(theta = .5), mu = c(.5, 2, 4)),
    gpoisson = list(f = inlaws::gpoisson(theta = 2), mu = c(.5, 2, 4)))
  y <- c(0, 2, 5)
  for (name in names(cases)) {
    z <- cases[[name]]
    family <- z$f
    d <- gratia:::distribution_from_family(family, z$mu, 1, 1, y)
    lower <- z$f$cdf(y - 1, z$mu, 1, 1)
    upper <- z$f$cdf(y, z$mu, 1, 1)
    expected <- withr::with_seed(14, runif(3, lower, upper))
    expect_equal(gratia:::distribution_quantile_residuals(d, y, "pit", seed = 14), expected, info = name)
    expect_equal(gratia:::distribution_quantile_residuals(d, y, "quantile", seed = 14), qnorm(expected), info = name)
    expect_identical(d$family$cdf, z$f$cdf)
    expect_identical(d$family$qf, z$f$qf)
    expect_identical(d$family$rd, z$f$rd)
    expect_equal(d$qf(c(.1, .5, .9)), z$f$qf(c(.1, .5, .9), z$mu, 1, 1))
    expect_equal(gratia:::distribution_cdf_left(d, c(0, .5, 2)),
      z$f$cdf(c(-1, 0, 1), z$mu, 1, 1))
    expect_equal(withr::with_seed(14, d$rd()), withr::with_seed(14, z$f$rd(z$mu, 1, 1)))
  }
})

test_that("inlaws mixture and heavy-tail response moments have the required meaning", {
  mu <- cbind(c(.5, 2, 4), c(.2, .5, .8), 2)
  for (name in c("zinb", "zanb")) {
    f <- getExportedValue("inlaws", name)()
    d <- gratia:::distribution_from_family(f, mu, 1, 1)
    positive <- if (name == "zanb") 1 - dnbinom(0, mu = mu[, 1], size = mu[, 3]) else 1
    expect_equal(d$mean, mu[, 1] * mu[, 2] / positive)
    # Sum the count probabilities independently of the moment formula.
    reference <- vapply(seq_len(nrow(mu)), function(i) {
      counts <- 0:1000
      prob <- dnbinom(counts, mu = mu[i, 1], size = mu[i, 3])
      if (name == "zanb") {prob[1] <- 0; prob <- prob / positive[i]}
      prob <- prob * mu[i, 2]; prob[1] <- prob[1] + 1 - mu[i, 2]
      sum(prob * counts^2) - sum(prob * counts)^2
    }, numeric(1))
    expect_equal(d$variance, reference)
  }
  f <- inlaws::gpd()
  pars <- cbind(c(1, 2, 3), c(.2, .6, 1.2))
  d <- gratia:::distribution_from_family(f, pars, 1, 1)
  expect_equal(d$mean[1:2], pars[1:2, 1] / (1 - pars[1:2, 2]))
  expect_false(d$mean_available)
  expect_false(d$variance_available)
  expect_error(gratia:::check_distribution_residual_moments(d, "response"), "GPD means")
  expect_true(all(is.finite(gratia:::distribution_quantile_residuals(d, c(.5, 1, 2), "quantile"))))
  for (name in c("log_logistic", "log_logisticls")) {
    f <- getExportedValue("inlaws", name)()
    if (name == "log_logistic") f <- inlaws::log_logistic(theta = .6)
    mu <- if (name == "log_logisticls") cbind(c(1, 2, 3), .6) else c(1, 2, 3)
    d <- gratia:::distribution_from_family(f, mu, 1, 1)
    expect_true(d$mean_available)
    expect_false(d$variance_available)
    expect_error(gratia:::check_distribution_residual_moments(d, "pearson"), "Log-logistic variance")
  }
})

test_that("inlaws cumulative-link diagnostics use latent predictors and randomise all categories", {
  f <- inlaws::cumulative_link(R = 4, link = "probit")
  eta <- c(-1, 0, 1)
  y <- c(1, 2, 4)
  d <- gratia:::distribution_from_family(f,
    rep(99, 3), 1, 1, y, eta = eta)
  lower <- f$cdf(y - 1, eta); upper <- f$cdf(y, eta)
  expected <- withr::with_seed(8, runif(3, lower, upper))
  expect_equal(gratia:::distribution_quantile_residuals(d, y, "pit", seed = 8), expected)
  expect_true(all(is.finite(gratia:::distribution_quantile_residuals(d, y, "quantile", seed = 8))))
  probabilities <- sapply(1:4, function(k) f$cdf(k, eta) - f$cdf(k - 1, eta))
  expect_equal(d$mean, as.vector(probabilities %*% 1:4))
  expect_equal(d$qf(.5), f$qf(.5, eta))
})

test_that("inlaws ordered beta endpoints use both masses and the complete mean", {
  f <- inlaws::ordbeta()
  mu <- c(.2, .5, .8)
  y <- c(0, .4, 1)
  th <- f$getTheta(TRUE)
  p0 <- plogis(th[2] - qlogis(mu)); p1 <- plogis(qlogis(mu) - th[3])
  d <- gratia:::distribution_from_family(f, mu, 1, 1, y)
  lower <- c(0, f$cdf(y, mu)[2], 1 - p1[3]); upper <- f$cdf(y, mu)
  expected <- withr::with_seed(6, runif(3, lower, upper))
  expect_equal(gratia:::distribution_quantile_residuals(d, y, "pit", seed = 6), expected)
  expect_true(all(is.finite(gratia:::distribution_quantile_residuals(d, y, "quantile", seed = 6))))
  expect_equal(d$mean, p1 + (1 - p0 - p1) * mu)
  expect_equal(gratia:::distribution_cdf_left(d, y), lower)
})

test_that("inlaws censored families randomise intervals and require an observation process", {
  y <- structure(c(.4, 1, 2, 3), censor = c(-Inf, 1, Inf, 4))
  cases <- list(cgamma = list(f = inlaws::cgamma(.6), mu = rep(2, 4)),
    cgammals = list(f = inlaws::cgammals(), mu = cbind(rep(2, 4), .6)),
    clognormal = list(f = inlaws::clognormal(.5), mu = rep(2, 4)),
    clognormalls = list(f = inlaws::clognormalls(), mu = cbind(rep(2, 4), .5)))
  for (name in names(cases)) {
    z <- cases[[name]]
    d <- gratia:::distribution_from_family(z$f, z$mu, 1, 1, y)
    lower <- z$f$cdf(c(-Inf, 1, 2, 3), z$mu); upper <- z$f$cdf(c(.4, 1, Inf, 4), z$mu)
    expected <- upper
    expected[c(1, 3, 4)] <- withr::with_seed(12, runif(3, lower[c(1, 3, 4)], upper[c(1, 3, 4)]))
    expect_equal(gratia:::distribution_quantile_residuals(d, y, "pit", seed = 12), expected, info = name)
    expect_error(gratia:::check_distribution_reference(d), "censoring observation process")
    observe <- function(y) structure(pmax(y, .4), censor = ifelse(y < .4, -Inf, y))
    d <- gratia:::distribution_from_family(getExportedValue("inlaws", name)(observe = observe), z$mu, 1, 1, y)
    expect_equal(gratia:::observe_diagnostic_response(d, c(.1, 1, 2, 3)), observe(c(.1, 1, 2, 3)))
  }
})

test_that("fitted inlaws diagnostics share probabilities and means across plot sizes", {
  withr::local_seed(91)
  dat <- data.frame(x = runif(100))
  dat$y <- rnbinom(100, mu = exp(.5 + dat$x), size = 2)
  m <- mgcv::gam(list(y ~ x, ~1), family = inlaws::nbls(), data = dat, method = "REML")
  lower <- pnbinom(m$y - 1, mu = fitted(m)[, 1], size = fitted(m)[, 2])
  upper <- pnbinom(m$y, mu = fitted(m)[, 1], size = fitted(m)[, 2])
  expected <- qnorm(withr::with_seed(20, runif(100, lower, upper)))
  expect_equal(gratia::quantile_residuals(m, type = "quantile", seed = 20), unname(expected))
  qq <- gratia::qq_plot(m, type = "quantile", seed = 20)
  expect_equal(qq$data$residuals, sort(expected))
  expect_equal(gratia::worm_plot(m, type = "quantile", seed = 20)$data$residuals,
    sort(expected) - qnorm(ppoints(100)))
  expect_equal(gratia::scalable_qq_plot(m, type = "quantile", seed = 20, max_points = 100)$data$residuals,
    qq$data$residuals)
  expect_equal(gratia::observed_fitted_plot(m)$data$fitted, unname(fitted(m)[, 1]))
  expect_s3_class(gratia::appraise(m, type = "quantile", seed = 20), "patchwork")
  expect_silent(ggplot2::ggplot_build(qq))
  expect_s3_class(gratia::qq_plot(m, method = "simulate", n_simulate = 3, seed = 20), "ggplot")
  # Missing fitting responses remain in output, while the factory gets compact rows.
  dat$y[5] <- NA
  m <- mgcv::gam(list(y ~ x, ~1), family = inlaws::nbls(), data = dat,
    method = "REML", na.action = na.exclude)
  r <- gratia::quantile_residuals(m, type = "quantile", seed = 20)
  expect_length(r, 100)
  expect_true(is.na(r[5]))
  expect_true(all(is.finite(r[-5])))
})


test_that("fitted inlaws mixtures, ordinal and censored families produce the intended panels", {
  withr::local_seed(910)
  n <- 160L
  x <- runif(n); mu <- exp(.5 + x)
  cases <- list(
    zinb = list(f = inlaws::zinb(), mu = cbind(mu, .7, 2)),
    zanb = list(f = inlaws::zanb(), mu = cbind(mu, .7, 2)),
    gpd = list(f = inlaws::gpd(), mu = cbind(mu, .2)),
    ordbeta = list(f = inlaws::ordbeta(), mu = plogis(x - .5)),
    cumulative_link = list(f = inlaws::cumulative_link(R = 4, link = "probit"), mu = x - .5),
    cgamma = list(f = inlaws::cgamma(.6, observe = inlaws::censoring_process(lower = 2)), mu = mu))
  for (name in names(cases)) {
    z <- cases[[name]]
    dat <- data.frame(x = x)
    dat$y <- z$f$rd(z$mu, rep(1, n), 1)
    if (name == "cgamma") dat$y <- cbind(pmax(dat$y, 2), ifelse(dat$y < 2, -Inf, dat$y))
    form <- if (inherits(z$f, "general.family")) c(list(y ~ x), rep(list(~1), z$f$nlp - 1L)) else y ~ x
    m <- mgcv::gam(form, data = dat, family = z$f, method = "REML")
    observe <- if (name == "cgamma") function(y)
      structure(pmax(y, 2), censor = ifelse(y < 2, -Inf, y)) else NULL
    expected_mean <- switch(name,
      zinb = fitted(m)[, 1] * fitted(m)[, 2],
      zanb = fitted(m)[, 1] * fitted(m)[, 2] / (1 - dnbinom(0, mu = fitted(m)[, 1], size = fitted(m)[, 3])),
      gpd = fitted(m)[, 1] / (1 - fitted(m)[, 2]),
      ordbeta = as.numeric(predict(m, type = "response")),
      cumulative_link = 1 + rowSums(sapply(1:3, function(k)
        m$family$cdf(k, m$linear.predictors, lower.tail = FALSE))),
      cgamma = fitted(m))
    expect_equal(gratia::observed_fitted_plot(m)$data$fitted, unname(expected_mean), info = name)
    r <- gratia::quantile_residuals(m, type = "quantile", seed = 1)
    expect_true(all(is.finite(r)), info = name)
    p <- gratia::qq_plot(m, type = "quantile", seed = 1)
    expect_equal(p$data$residuals, sort(r), info = name)
    expect_silent(ggplot2::ggplot_build(p))
    expect_s3_class(gratia::appraise(m, type = "quantile", seed = 1, scalable = TRUE), "patchwork")
    if (name == "cgamma") {
      draw <- seq(.1, 4, length.out = n)
      encoded <- observe(draw)
      proxy <- m; proxy$y <- encoded
      expect_equal(as.numeric(gratia:::diagnostic_reference_residuals(m, draw, "deviance")),
        as.numeric(stats::residuals(proxy, type = "deviance")))
      expect_s3_class(gratia::qq_plot(m, method = "simulate", n_simulate = 3, seed = 1), "ggplot")
    }
  }
})

test_that("inlaws factories survive grouped fitting and fitted nuisance-parameter reconstruction", {
  withr::local_seed(19)
  dat <- data.frame(x = runif(120), index = rep(1:2, each = 60))
  dat$y <- c(rpois(60, 2), inlaws::pig(.5)$rd(rep(3, 60)))
  f <- inlaws::pig()
  m <- mgcv::gam(cbind(y, index) ~ s(x, k = 4),
    family = mgcv::gfam(list(poisson(), f)), data = dat, method = "REML")
  expect_true(is.function(m$family$getfl()[[2]]$gratia_diagnostics))
  context <- gratia:::gfam_diagnostic_context(m)
  # Exercise reconstruction even when getfl() contains stale component state.
  stale <- unserialize(serialize(m, NULL))
  stale$family$getfl()[[2]]$putTheta(log(.01))
  rebuilt <- gratia:::gfam_diagnostic_context(stale)
  rows <- which(rebuilt$index == 2L)
  reconstructed <- gratia:::gfam_component_distribution(rebuilt, 2L)
  expect_equal(reconstructed$variance, exp(stale$linear.predictors[rows]) +
    context$families[[2]]$getTheta(TRUE) * exp(2 * stale$linear.predictors[rows]))
  expect_equal(stale$family$getfl()[[2]]$getTheta(TRUE), .01)
  reference <- inlaws::pig(theta = context$families[[2]]$getTheta(TRUE))
  mu <- exp(m$linear.predictors)
  i <- dat$index == 1L
  lower <- upper <- numeric(120)
  lower[i] <- ppois(dat$y[i] - 1, mu[i]); upper[i] <- ppois(dat$y[i], mu[i])
  lower[!i] <- reference$cdf(dat$y[!i] - 1, mu[!i])
  upper[!i] <- reference$cdf(dat$y[!i], mu[!i])
  expected <- qnorm(withr::with_seed(5, runif(120, lower, upper)))
  expect_equal(gratia::quantile_residuals(m, type = "quantile", seed = 5), expected)
  expect_equal(gratia::observed_fitted_plot(m)$data$fitted, as.numeric(mu))
  expect_s3_class(gratia::qq_plot(m, type = "quantile", seed = 5), "ggplot")
  expect_s3_class(gratia::qq_plot(m, type = "deviance", by_family = FALSE,
    method = "uniform", n_uniform = 3, seed = 5), "ggplot")
  expect_s3_class(gratia::qq_plot(m, type = "deviance", by_family = TRUE,
    method = "simulate", n_simulate = 3, seed = 5), "ggplot")
})

test_that("native factories survive construction, cloning, nuisance updates and fitted reconstruction", {
  for (name in c("nbls", "cmp", "cmpls", "gpoisson", "pig", "pigls", "zinb", "zanb",
      "cumulative_link", "ordbeta", "cgamma", "cgammals", "clognormal", "clognormalls",
      "gpd", "log_logistic", "log_logisticls", "betabinomial", "dirichlet", "dirmult")) {
    f <- if (name == "cumulative_link") inlaws::cumulative_link(R = 4) else
      if (name %in% c("dirichlet", "dirmult")) getExportedValue("inlaws", name)(K = 2) else
      getExportedValue("inlaws", name)()
    expect_true(is.function(f$gratia_diagnostics), info = name)
    expect_true(is.function(unserialize(serialize(f, NULL))$gratia_diagnostics), info = name)
  }
  f <- inlaws::pig(.5)
  clone <- unserialize(serialize(f, NULL)); clone$putTheta(log(.8))
  d <- gratia:::distribution_from_family(clone, c(1, 2), 1, 1)
  expect_equal(d$variance, c(1, 2) + .8 * c(1, 2)^2)
  expect_equal(f$getTheta(TRUE), .5)
  withr::local_seed(35)
  dat <- data.frame(x = runif(100), y = inlaws::pig(.5)$rd(rep(2, 100)))
  m <- mgcv::gam(y ~ x, family = inlaws::pig(), data = dat, method = "REML")
  d <- gratia:::response_distribution(unserialize(serialize(m, NULL)))
  expect_equal(d$variance, unname(fitted(m) + m$family$getTheta(TRUE) * fitted(m)^2))
  expect_equal(d$cdf_left(m$y), m$family$cdf(m$y - 1, fitted(m)))
})

test_that("beta-binomial diagnostics respect retained trial totals and fitting order", {
  withr::local_seed(330)
  dat <- data.frame(x = runif(120), trials = sample(3:18, 120, TRUE))
  dat$y <- rbinom(120, dat$trials, rbeta(120, 2, 3))
  dat$y[c(6, 55)] <- NA
  rows <- c(120:61, 1:60)
  m <- mgcv::gam(list(cbind(y, trials - y) ~ x, ~1), data = dat,
    subset = rows, family = inlaws::betabinomial(), method = "REML", na.action = na.exclude)
  used <- gratia:::model_used_rows(m)
  d <- gratia:::response_distribution(m)
  total <- rowSums(model.response(model.frame(used)))
  p <- used$fitted.values[, 1]; phi <- used$fitted.values[, 2]
  upper <- lower <- variance <- numeric(length(p))
  for (i in seq_along(p)) {
    k <- 0:total[i]
    mass <- choose(total[i], k) * beta(k + p[i] * phi[i], total[i] - k + (1 - p[i]) * phi[i]) /
      beta(p[i] * phi[i], (1 - p[i]) * phi[i])
    upper[i] <- sum(mass[k <= used$y[i]]); lower[i] <- sum(mass[k < used$y[i]])
    variance[i] <- sum((k - total[i] * p[i])^2 * mass)
  }
  expect_equal(d$mean, unname(total * p))
  expect_equal(d$variance, variance)
  expect_equal(d$cdf(used$y), upper)
  expect_equal(d$cdf_left(used$y), lower)
  expected <- qnorm(withr::with_seed(2, runif(length(p), lower, upper)))
  r <- gratia::quantile_residuals(m, seed = 2, type = "quantile")
  expect_equal(r, gratia:::restore_model_rows(expected, m))
  expect_true(all(is.finite(r[!is.na(r)])))
  expect_s3_class(gratia::qq_plot(m, method = "simulate", n_simulate = 3, seed = 2), "ggplot")
  expect_s3_class(gratia::appraise(m, type = "quantile", seed = 2, scalable = TRUE), "patchwork")
})

test_that("multivariate native diagnostics retain columns, moments and joint response dependence", {
  withr::local_seed(640)
  n <- 80L
  dat <- data.frame(x = runif(n))
  eta <- cbind(.5 * dat$x, -.3 * dat$x, log(5))
  probability <- exp(cbind(0, eta[, 1:2])); probability <- probability / rowSums(probability)
  for (name in c("dirichlet", "dirmult")) {
    f <- getExportedValue("inlaws", name)(2)
    count <- name == "dirmult"
    total <- sample(5:15, n, TRUE)
    dat$y <- if (count) t(vapply(seq_len(n), function(i)
      as.numeric(rmultinom(1, total[i], rgamma(3, probability[i, ] * 5))), numeric(3))) else
      f$rd(eta, 1, 1)
    dat$y[7, ] <- NA
    m <- mgcv::gam(list(y ~ x, ~1, ~1), data = dat, subset = n:1,
      family = f, method = "REML", na.action = na.exclude)
    used <- gratia:::model_used_rows(m)
    d <- gratia:::response_distribution(m)
    z <- exp(cbind(0, used$linear.predictors[, 1:2])); z <- z / rowSums(z)
    phi <- exp(used$linear.predictors[, 3])
    totals <- if (count) rowSums(used$y) else rep(1, nrow(used$y))
    expect_equal(unname(d$mean), unname(z * totals))
    expect_equal(unname(d$variance), unname(z * (1 - z) *
      if (count) totals * (totals + phi) / (1 + phi) else 1 / (1 + phi)))
    expect_false(d$independent)
    expect_true(d$joint_available)
    expect_null(d$qf)
    expect_equal(rowSums(d$rd()), totals)
    clone <- unserialize(serialize(m, NULL))
    expect_equal(gratia:::response_distribution(clone)$mean, d$mean)
    r <- gratia::quantile_residuals(m, type = "quantile", seed = 4)
    expect_equal(dim(r), c(n, 3L))
    expect_true(all(is.na(r[n - 7 + 1, ])))
    expect_true(all(is.finite(r[-(n - 7 + 1), ])))
    if (!count) {
      expected <- qnorm(pbeta(used$y, z * phi, (1 - z) * phi))
      expect_equal(r, gratia:::restore_model_rows(expected, m))
      expect_error(gratia:::diagnostic_residuals(m, "deviance"), "Dirichlet deviance")
    } else {
      expected <- matrix(0, nrow(used$y), 3)
      lower <- upper <- expected
      for (j in 1:3) for (i in seq_len(nrow(expected))) {
        k <- 0:totals[i]; a <- z[i, j] * phi[i]; b <- (1 - z[i, j]) * phi[i]
        mass <- choose(totals[i], k) * beta(k + a, totals[i] - k + b) / beta(a, b)
        lower[i, j] <- sum(mass[k < used$y[i, j]])
        upper[i, j] <- sum(mass[k <= used$y[i, j]])
      }
      expected[] <- qnorm(withr::with_seed(4, runif(length(expected), lower, upper)))
      expect_equal(r, gratia:::restore_model_rows(expected, m))
    }
    qq <- gratia::qq_plot(m, type = "quantile", seed = 4)
    expect_equal(qq$data$residuals, sort(as.vector(r)))
    expect_s3_class(gratia::worm_plot(m, type = "quantile", seed = 4), "ggplot")
    expect_s3_class(gratia::scalable_qq_plot(m, type = "quantile", seed = 4), "ggplot")
    expect_equal(gratia::observed_fitted_plot(m)$data$fitted,
      as.vector(gratia:::restore_model_rows(d$mean, m)))
    expect_s3_class(gratia::appraise(m, type = "quantile", seed = 4), "patchwork")
    expect_s3_class(gratia::appraise(m, type = "quantile", seed = 4, scalable = TRUE), "patchwork")
    expect_s3_class(gratia::qq_plot(m, type = "response", method = "simulate", n_simulate = 3, seed = 4), "ggplot")
  }
})

test_that("fitted censoring rules remain aligned after subsetting and missing responses", {
  withr::local_seed(180)
  n <- 100L
  dat <- data.frame(x = runif(n))
  process <- inlaws::censoring_process(.8, 4)
  for (name in c("cgamma", "cgammals", "clognormal", "clognormalls")) {
    f <- getExportedValue("inlaws", name)(observe = process)
    parameters <- if (inherits(f, "general.family")) cbind(exp(.5 + dat$x), .5) else exp(.5 + dat$x)
    raw <- f$rd(parameters, 1, 1); encoded <- process(raw)
    dat$y <- cbind(as.numeric(encoded), attr(encoded, "censor"))
    dat$y[13, ] <- NA
    form <- if (inherits(f, "general.family")) list(y ~ x, ~1) else y ~ x
    m <- mgcv::gam(form, data = dat, subset = n:1, family = f, method = "REML", na.action = na.exclude)
    used <- gratia:::model_used_rows(m)
    d <- gratia:::response_distribution(m)
    expect_true(d$reference_available, info = name)
    bound <- attr(used$y, "censor")
    lower <- m$family$cdf(pmin(used$y, bound), used$fitted.values, 1, 1)
    upper <- m$family$cdf(pmax(used$y, bound), used$fitted.values, 1, 1)
    expected <- upper
    random <- which(lower != upper)
    expected[random] <- withr::with_seed(3, runif(length(random), lower[random], upper[random]))
    r <- gratia::quantile_residuals(m, type = "pit", seed = 3)
    expect_equal(r, gratia:::restore_model_rows(as.numeric(expected), m), info = name)
    expect_true(is.na(r[n - 13 + 1]))
    draw <- seq(.1, 6, length.out = n - 1)
    proxy <- used; proxy$y <- process(draw)
    expect_equal(as.numeric(gratia:::diagnostic_reference_residuals(m, draw, "deviance")),
      as.numeric(residuals(proxy, type = "deviance")), info = name)
    expect_s3_class(gratia::qq_plot(m, method = "simulate", n_simulate = 3, seed = 3), "ggplot")
    expect_s3_class(gratia::worm_plot(m, type = "quantile", seed = 3), "ggplot")
    expect_s3_class(gratia::appraise(m, type = "quantile", seed = 3, scalable = TRUE), "patchwork")
  }
})

test_that("ordinary native count and continuous families survive grouped parameter reconstruction", {
  withr::local_seed(690)
  n <- 100L
  for (f in list(inlaws::cmp(), inlaws::gpoisson(), inlaws::log_logistic())) {
    dat <- data.frame(x = runif(n), index = rep(1:2, each = n / 2))
    dat$y <- c(rpois(n / 2, 2), f$rd(rep(3, n / 2), 1, 1))
    dat$y[9] <- NA
    m <- mgcv::gam(cbind(y, index) ~ x, data = dat, subset = n:1,
      family = mgcv::gfam(list(poisson(), f)), method = "REML", na.action = na.exclude)
    context <- gratia:::gfam_diagnostic_context(m)
    d <- gratia:::gfam_component_distribution(context, 2)
    rows <- which(context$index == 2)
    rebuilt <- context$families[[2]]
    expect_true(is.function(rebuilt$gratia_diagnostics))
    expect_equal(d$variance, rebuilt$variance(exp(context$model$linear.predictors[rows])))
    r <- gratia::quantile_residuals(m, type = "quantile", seed = 4)
    expect_length(r, n)
    expect_true(is.na(r[n - 9 + 1]))
    expect_true(all(is.finite(r[-(n - 9 + 1)])))
    expect_s3_class(gratia::qq_plot(m, type = "quantile", seed = 4), "ggplot")
    expect_s3_class(gratia::qq_plot(m, type = "deviance", method = "simulate", n_simulate = 3, seed = 4), "ggplot")
  }
})
