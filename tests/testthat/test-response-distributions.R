test_that("family augmentation preserves native and supplied mgcv hooks", {
  native <- mgcv::shash()
  expect_identical(fix_family_cdf(native)$cdf, native$cdf)
  for (name in c("rd", "cdf", "qf")) {
    f <- stats::gaussian()
    f[[name]] <- function(...) 42
    fixed <- switch(name, rd = fix_family_rd(f), cdf = fix_family_cdf(f),
      qf = fix_family_qf(f))
    expect_identical(fixed[[name]], f[[name]])
  }
  for (f in list(stats::Gamma(), mgcv::twlss(), mgcv::ocat(R = 4), mgcv::ziP())) {
    augmented <- fix_family_rd(fix_family_qf(fix_family_cdf(f)))
    expect_s3_class(augmented, "family")
    expect_true(is.function(augmented$rd))
    expect_true(is.function(augmented$cdf))
    expect_true(is.function(augmented$qf))
    for (name in c("rd", "cdf", "qf"))
      expect_true(all(c("mu", "wt", "scale") %in% names(formals(augmented[[name]]))))
  }
})

test_that("weighted Gamma hooks share native GLM dispersion semantics", {
  mu <- c(1, 2, 4)
  wt <- c(1, 2, 4)
  phi <- .7
  f <- fix_family_rd(fix_family_qf(fix_family_cdf(stats::Gamma())))
  q <- c(.5, 1, 6)
  expect_equal(f$cdf(q, mu, wt, phi),
    pgamma(q, shape = wt / phi, scale = mu * phi / wt))
  expect_equal(f$qf(c(.1, .5, .9), mu, wt, phi),
    qgamma(c(.1, .5, .9), shape = wt / phi, scale = mu * phi / wt))
  set.seed(20)
  expected <- rgamma(3, shape = wt / phi, scale = mu * phi / wt)
  set.seed(20)
  expect_equal(f$rd(mu, wt, phi), expected)
})

test_that("normalized shash preserves finite extreme normal scores", {
  f <- mgcv::shash()
  mu <- cbind(c(0, 1), c(0, .3), c(.4, -.3), c(.1, -.2))
  d <- distribution_from_family(f, mu, 1, 1)
  scores <- c(-12, 12)
  delta <- exp(mu[, 4])
  y <- mu[, 1] + delta * exp(mu[, 2]) *
    sinh((asinh(scores) + mu[, 3]) / delta)
  expect_equal(d$cdf(y), pnorm(scores))
  expect_equal(distribution_quantile_residuals(d, y, "quantile"), scores,
    tolerance = 1e-10)
  expect_identical(f$cdf, fix_family_cdf(f)$cdf)
})

test_that("zero mass and truncation transformations match independent count sums", {
  mu <- c(.1, 2, 10)
  base <- distribution_from_parameters("poisson", mu)
  trunc <- truncate_distribution(base, mean = mu / (-expm1(-mu)))
  y <- c(1, 2, 12)
  expected <- (ppois(y, mu) - exp(-mu)) / (-expm1(-mu))
  expect_equal(trunc$cdf(y), expected)
  expect_equal(trunc$cdf(-1), rep(0, 3))
  zi <- c(.2, .5, .8)
  mixed <- zero_inflated_distribution(trunc, zi)
  expect_equal(mixed$cdf(y), zi + (1 - zi) * expected)
  expect_equal(mixed$cdf(0), zi)
  expect_equal(mixed$cdf(-1), rep(0, 3))
  p <- c(.05, .7, .99)
  q <- mixed$qf(p)
  expect_true(all(mixed$cdf(q) >= p))
  expect_true(all(mixed$cdf(q - 1) < p))
  degenerate <- zero_inflated_distribution(base, rep(1, 3))
  expect_equal(degenerate$qf(.5), rep(0, 3))
  expect_equal(degenerate$mean, rep(0, 3))
})

test_that("conditional mixture transformations retain tiny tails", {
  base <- distribution_from_parameters("Gamma", mu = c(1, 2), dispersion = 2)
  d <- zero_inflated_distribution(base, c(.2, .8))
  q <- c(1e-50, 1000)
  expect_equal(d$cdf(q, log_p = TRUE, lower_tail = FALSE),
    log(c(.8, .2)) + pgamma(q, 2, scale = c(.5, 1), lower.tail = FALSE, log.p = TRUE))
  lp <- c(-1000, -500)
  q <- d$qf(lp, lower_tail = FALSE, log_p = TRUE)
  expect_equal(d$cdf(q, lower_tail = FALSE, log_p = TRUE), lp)
})

test_that("ordered category CDF and quantiles use ordered logistic thresholds", {
  family <- mgcv::ocat(R = 4)
  cuts <- family$getTheta(TRUE)
  mu <- c(-1, 0, 1)
  d <- distribution_from_family(family, mu, 1, 1)
  y <- c(1, 2, 4)
  expect_equal(d$cdf(y), plogis(c(cuts[1], cuts[2], Inf) - mu))
  p <- c(.1, .4, .9)
  q <- d$qf(p)
  expect_true(all(d$cdf(q) >= p))
  expect_true(all(d$cdf(q - 1) < p))
  set.seed(2)
  u <- runif(3, d$cdf(y - 1), d$cdf(y))
  expect_equal(distribution_quantile_residuals(d, y, "pit", seed = 2), u)
})

test_that("single predictor ziP uses the same hurdle observation distribution", {
  f <- mgcv::ziP(theta = c(.3, -.5), b = .2)
  mu <- c(-1, 0, 1)
  theta <- f$getTheta(TRUE)
  presence <- -expm1(-exp(theta[1] + theta[2] * mu))
  lambda <- exp(mu)
  d <- distribution_from_family(f, mu, 1, 1)
  y <- c(0, 1, 3)
  expected <- (1 - presence) + presence *
    (ppois(y, lambda) - exp(-lambda)) / (-expm1(-lambda))
  expect_equal(d$cdf(y), expected)
  expect_equal(d$mean, presence * lambda / (-expm1(-lambda)))
  p <- c(.1, .5, .9)
  q <- d$qf(p)
  expect_true(all(d$cdf(q) >= p))
  expect_true(all(d$cdf(q - 1) < p))
})

test_that("twlss reuses the Tweedie solver with observation-specific power", {
  f <- mgcv::twlss()
  mu <- cbind(c(1, 2, 4), c(-1, 0, 1), log(c(.3, .7, 1)))
  d <- distribution_from_family(f, mu, 1, 1)
  ab <- get_tw_params(f)
  power <- theta_2_power(mu[, 2], ab[1], ab[2])
  y <- c(0, 1, 5)
  expected <- vapply(seq_len(3), function(i)
    ptweedie_mixture(y[i], mu[i, 1], power[i], exp(mu[i, 3])), numeric(1))
  expect_equal(d$cdf(y), expected)
  p <- c(.2, .5, .8)
  q <- d$qf(p)
  expect_equal(d$cdf(q), p, tolerance = 1e-7)
  expect_equal(d$bounds(y)$lower[1], -Inf)
})

test_that("fitted GLM and weighted lm wrappers prepare the common distribution", {
  x <- seq_len(12)
  y <- c(1, 3, 2, 5, 4, 8, 6, 9, 8, 11, 10, 15)
  w <- rep(c(1, 2, 3), 4)
  fit <- lm(y ~ x, weights = w)
  d <- response_distribution.lm(fit)
  expect_equal(d$cdf(y), pnorm(y, fitted(fit), summary(fit)$sigma / sqrt(w)))
  fit <- glm(y ~ x, family = Gamma(link = "log"), weights = w)
  d <- response_distribution.glm(fit)
  phi <- summary(fit)$dispersion
  expect_equal(d$cdf(y), pgamma(y, shape = w / phi, scale = fitted(fit) * phi / w))
  expect_equal(distribution_quantile_residuals(d, y, "quantile"),
    unname(qnorm(d$cdf(y))))
})

test_that("glmmTMB adapters share count mixtures and binomial response scales", {
  skip_if_not_installed("glmmTMB")
  x <- seq(-1, 1, length.out = 30)
  y <- c(0, 1, 0, 3, 1, 5, 0, 2, 1, 4, 3, 0, 2, 5, 0,
    4, 7, 1, 0, 3, 6, 8, 0, 5, 3, 9, 2, 1, 7, 6)
  fit <- glmmTMB::glmmTMB(y ~ x, ziformula = ~1, family = glmmTMB::nbinom2(),
    data = data.frame(y, x))
  d <- response_distribution.glmmTMB(fit)
  mu <- exp(predict(fit, type = "link"))
  size <- predict(fit, type = "disp")
  zero <- predict(fit, type = "zprob")
  expect_equal(d$cdf(y), zero + (1 - zero) * pnbinom(y, mu = mu, size = size))
  expect_equal(unname(d$mean), unname(predict(fit, type = "response")))
  expect_equal(d$bounds(y)$cdf_lower()[y == 0], rep(0, sum(y == 0)))
  r <- distribution_quantile_residuals(d, y, "quantile", seed = 4)
  expect_true(all(is.finite(r)))
  trials <- rep(c(5, 10, 15), 10)
  successes <- pmin(y, trials)
  fit <- glmmTMB::glmmTMB(cbind(successes, trials - successes) ~ x, family = binomial(),
    data = data.frame(successes, trials, x))
  d <- response_distribution.glmmTMB(fit)
  expect_equal(d$cdf(successes / trials),
    pbinom(successes, trials, predict(fit, type = "response")))
  expect_equal(d$bounds(successes / trials)$lower, (successes - 1) / trials)
})

test_that("normalization respects caller-supplied distributional family hooks", {
  f <- mgcv::shash()
  f$cdf <- function(q, mu, wt, scale, logp) {
    value <- rep(.25, max(length(q), NROW(mu)))
    if (logp) log(value) else value
  }
  f$qf <- function(p, mu, wt, scale) rep(17, max(length(p), NROW(mu)))
  d <- distribution_from_family(f, cbind(1:2, 0, 0, 0), 1, 1)
  expect_equal(d$cdf(0), c(.25, .25))
  expect_equal(d$cdf(0, log_p = TRUE), rep(log(.25), 2))
  expect_equal(d$qf(.5), c(17, 17))
})

test_that("extreme-value and shash normalized quantiles retain log tails", {
  gev <- distribution_from_family(mgcv::gevlss(), cbind(c(0, 1), 0, 0), 1, 1)
  expect_equal(gev$cdf(c(1000, 1001), log_p = TRUE, lower_tail = FALSE), c(-1000, -1000))
  expect_true(all(is.finite(distribution_quantile_residuals(gev, c(1000, 1001), "quantile"))))
  gum <- distribution_from_family(mgcv::gumbls(), cbind(c(0, 1), 0), 1, 1)
  expect_equal(gum$cdf(c(1000, 1001), log_p = TRUE, lower_tail = FALSE),
    rep(-1000 - 0.577215664901533, 2))
  shash <- distribution_from_family(mgcv::shash(), cbind(c(0, 1), 0, 0, 0), 1, 1)
  q <- shash$qf(c(-1000, -500), log_p = TRUE, lower_tail = FALSE)
  expect_equal(shash$cdf(q, log_p = TRUE, lower_tail = FALSE), c(-1000, -500))
})

test_that("multivariate normal CDFs are marginal and simulation retains covariance", {
  family <- mgcv::mvn(d = 2)
  V <- matrix(c(1, .4, .4, 4), 2)
  family$data$R <- chol(solve(V))
  mu <- cbind(c(0, 1, 2), c(1, 0, -1))
  d <- distribution_from_family(family, mu, 1, 1)
  y <- mu + cbind(c(-1, 0, 1), c(-2, 0, 2))
  expect_true(is.function(d$family$cdf))
  expect_true(is.function(d$family$qf))
  expect_equal(unname(d$cdf(y)), unname(matrix(pnorm(c(-1, 0, 1, -1, 0, 1)), 3)))
  expect_equal(d$qf(c(.1, .5, .9)), mu + outer(qnorm(c(.1, .5, .9)), c(1, 2)))
  expect_equal(distribution_quantile_residuals(d, y, "quantile"),
    matrix(c(-1, 0, 1, -1, 0, 1), 3))
  expect_equal(dim(d$rd()), c(3L, 2L))
})

test_that("MVN response distributions preserve supplied family functions", {
  family <- mgcv::mvn(d = 2)
  family$data$R <- diag(2)
  family$cdf <- function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    p <- mu * 0 + if (lower_tail) .25 else .75
    if (log_p) log(p) else p
  }
  family$qf <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) mu + 7
  mu <- matrix(1:6, 3, 2)
  d <- distribution_from_family(family, mu, 1, 1)
  expect_identical(d$family$cdf, family$cdf)
  expect_identical(d$family$qf, family$qf)
  expect_equal(d$cdf(mu), matrix(.25, 3, 2))
  expect_equal(d$cdf(mu, log_p = TRUE, lower_tail = FALSE), matrix(log(.75), 3, 2))
  expect_equal(d$qf(.5), mu + 7)
})

test_that("glmmTMB proxy preserves conditioning and native residual definitions", {
  skip_if_not_installed("glmmTMB")
  dat <- glmmTMB::Salamanders
  fit <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    family = poisson(), data = dat)
  conditional <- glmmtmb_diagnostic_model(fit, "response", simulation = TRUE)
  gaussian <- glmmTMB::glmmTMB(count ~ mined + (1 | site), data = dat)
  marginal <- glmmtmb_diagnostic_model(gaussian, "response", simulation = TRUE,
    conditioning = "marginal")
  expect_true(is.function(response_distribution(conditional)$cdf))
  expect_true(is.function(response_distribution(marginal)$cdf))
  expect_true(is.function(response_distribution(marginal)$qf))
  expect_true(all(is.finite(response_distribution(marginal)$mean)))
  expect_identical(response_distribution(marginal)$calibration, "simulation ranks")
  draw <- response_distribution(conditional)$rd()
  expect_equal(diagnostic_reference_residuals.gratia_glmmtmb_diagnostic(
    conditional, draw, "response"), draw - unname(predict(fit, type = "response")))
  set.seed(8)
  before <- .Random.seed
  calibrated <- glmmtmb_diagnostic_model(fit, "quantile", simulation = TRUE)
  expect_identical(.Random.seed, before)
  expect_null(calibrated$residuals)
})

test_that("shared parameter constructors agree with independent native distributions", {
  mu <- c(.4, 2, 5)
  phi <- c(1, 2, 3)
  y <- c(.2, 1, 4)
  cases <- list(
    list(name = "gaussian", ref = pnorm(y, mu, phi)),
    list(name = "Gamma", ref = pgamma(y, shape = phi, scale = mu / phi)),
    list(name = "lognormal", ref = plnorm(y, log(mu) - phi^2 / 2, phi)),
    list(name = "nbinom1", ref = pnbinom(y, size = mu / phi, mu = mu)),
    list(name = "nbinom2", ref = pnbinom(y, size = phi, mu = mu))
  )
  for (case in cases) {
    d <- distribution_from_parameters(case$name, mu, phi)
    expect_equal(d$cdf(y), case$ref)
    q <- d$qf(c(.1, .5, .9))
    if (!startsWith(case$name, "nbinom"))
      expect_equal(d$cdf(q), c(.1, .5, .9)) else {
      expect_true(all(d$cdf(q) >= c(.1, .5, .9)))
      expect_true(all(d$cdf(q - 1) < c(.1, .5, .9)))
    }
  }
  mu <- c(.2, .4, .8)
  d <- distribution_from_parameters("beta", mu, phi)
  expect_equal(d$cdf(mu), pbeta(mu, mu * phi, (1 - mu) * phi))
  expect_equal(d$variance, mu * (1 - mu) / (1 + phi))
})

test_that("zero-truncated constructors carry exact count moments and support", {
  mu <- c(.01, 1, 10)
  for (family in c("poisson", "nbinom1", "nbinom2")) {
    base <- distribution_from_parameters(family, mu, dispersion = 2)
    d <- distribution_from_parameters(paste0("truncated_", family), mu, dispersion = 2)
    survival <- 1 - base$cdf(0)
    expect_equal(d$mean, mu / survival)
    expect_equal(d$variance, (base$variance + mu^2) / survival - (mu / survival)^2)
    expect_equal(d$qf(0), rep(1, 3))
    expect_true(all(d$rd() >= 1))
  }
})


test_that("point-mass mixtures support negative responses and exact zero jumps", {
  mu <- c(-1, 0, 2, 1)
  sd <- c(.5, 1, 2, .8)
  z <- c(.2, .4, .6, .3)
  base <- distribution_from_parameters("gaussian", mu, sd)
  d <- zero_inflated_distribution(base, z)
  y <- c(-2, 0, 0, 3)
  expect_equal(d$cdf(y), (1 - z) * pnorm(y, mu, sd) + z * (y >= 0))
  expect_equal(d$cdf(y, lower_tail = FALSE),
    (1 - z) * pnorm(y, mu, sd, lower.tail = FALSE) + z * (y < 0))
  expect_equal(distribution_cdf_left(d, y),
    (1 - z) * pnorm(y, mu, sd) + z * (y > 0))
  lower <- (1 - z) * pnorm(y, mu, sd) + z * (y > 0)
  upper <- (1 - z) * pnorm(y, mu, sd) + z * (y >= 0)
  set.seed(9)
  expected <- upper
  expected[y == 0] <- runif(sum(y == 0), lower[y == 0], upper[y == 0])
  expect_equal(distribution_quantile_residuals(d, y, "pit", seed = 9), expected)
  p <- c(.01, .4, .65, .99)
  q <- d$qf(p)
  f0 <- (1 - z) * pnorm(0, mu, sd)
  f1 <- f0 + z
  expected <- rep(0, 4)
  below <- p < f0
  above <- p > f1
  expected[below] <- qnorm(p[below] / (1 - z[below]), mu[below], sd[below])
  expected[above] <- qnorm((p[above] - z[above]) / (1 - z[above]), mu[above], sd[above])
  expect_equal(q, expected)
  expect_equal(d$qf(NA_real_), rep(NA_real_, 4))
  lp <- rep(-1000, 4)
  q <- d$qf(lp, log_p = TRUE)
  expect_equal(d$cdf(q, log_p = TRUE), lp)
  q <- d$qf(lp, log_p = TRUE, lower_tail = FALSE)
  expect_equal(d$cdf(q, log_p = TRUE, lower_tail = FALSE), lp)
})


test_that("marginal Gaussian glmmTMB QQ residuals share population centering", {
  skip_if_not_installed("glmmTMB")
  withr::local_seed(2)
  d <- data.frame(g = factor(rep(seq_len(10), each = 20)), x = rnorm(200))
  b <- seq(-3, 3, length.out = 10)
  d$y <- 1 + d$x + b[d$g] + rnorm(200, sd = .3)
  m <- glmmTMB::glmmTMB(y ~ x + (1 | g), data = d)
  proxy <- glmmtmb_diagnostic_model(m, simulation = TRUE, conditioning = "marginal")
  mu <- unname(predict(m, type = "response", re.form = NA))
  expect_identical(proxy$residual_type, "response")
  expect_lt(max(abs(proxy$fitted.values - mu)), .4)
  expect_equal(proxy$residuals, d$y - proxy$fitted.values)
  expect_gt(sd(proxy$residuals), 5 * sd(residuals(m)))
  dist <- response_distribution(proxy)
  draw <- dist$rd()
  expect_equal(diagnostic_reference_residuals(proxy, draw, "response"), draw - proxy$fitted.values)
  simulated <- replicate(200, diagnostic_reference_residuals(proxy, dist$rd(), "response"))
  expect_equal(sd(as.vector(simulated)), sd(proxy$residuals), tolerance = .2)
  expect_s3_class(qq_plot(m, conditioning = "marginal", type = "pearson", n_simulate = 3), "ggplot")
  expect_s3_class(qq_plot(m, conditioning = "marginal", type = "quantile", n_simulate = 3), "ggplot")
  expect_s3_class(qq_plot(m, conditioning = "marginal", n_simulate = 3, seed = 1), "ggplot")
  poisson <- glmmTMB::glmmTMB(abs(round(y)) ~ x + (1 | g), family = poisson(), data = d)
  expect_s3_class(qq_plot(poisson, conditioning = "marginal", n_simulate = 3), "ggplot")
  expect_true(is.function(response_distribution(poisson, "marginal")$rd))
})

test_that("unsupported glmmTMB distributions retain native diagnostic panels", {
  skip_if_not_installed("glmmTMB")
  d <- data.frame(y = rep(c(0L, 1L, 2L, 1L, 3L), 10), x = seq_len(50))
  m <- glmmTMB::glmmTMB(y ~ x, family = glmmTMB::compois(), data = d)
  dist <- response_distribution(m)
  expect_equal(dist$mean, unname(predict(m, type = "response")))
  expect_null(dist$cdf)
  expect_null(dist$qf)
  expect_null(dist$rd)
  expect_s3_class(residuals_hist_plot(m, type = "response"), "ggplot")
  expect_s3_class(observed_fitted_plot(m), "ggplot")
  expect_s3_class(qq_plot(m, type = "response", method = "normal"), "ggplot")
})

test_that("native glmmTMB lognormal dispersion is translated once", {
  skip_if_not_installed("glmmTMB")
  m <- glmmTMB::glmmTMB(Sepal.Length ~ Petal.Length,
    family = glmmTMB::lognormal(), data = iris)
  mu <- unname(predict(m, type = "conditional"))
  phi <- unname(predict(m, type = "disp"))
  sdlog <- sqrt(log1p((phi / mu)^2))
  dist <- response_distribution(m)
  expect_equal(dist$cdf(iris$Sepal.Length),
    plnorm(iris$Sepal.Length, log(mu) - sdlog^2 / 2, sdlog))
  expect_equal(dist$mean, mu)
  expect_equal(dist$variance, phi^2)
})

test_that("nonidentity Gaussian bam AR1 exposes only marginal calibration", {
  withr::local_seed(16)
  d <- data.frame(x = runif(90))
  d$y <- exp(1 + d$x) + rnorm(90, sd = .2)
  m <- mgcv::bam(y ~ s(x, k = 5), family = gaussian(link = "log"),
    rho = .4, discrete = TRUE, method = "fREML", data = d)
  dist <- response_distribution(m)
  expect_false(dist$independent)
  expect_null(dist$rd)
  expect_true(is.function(dist$qf))
  expect_false(dist$joint_available)
  expect_true(is.function(dist$cdf))
})

test_that("normalization honors modified native closure bodies", {
  f <- mgcv::shash()
  body(f$cdf) <- quote(rep(if (logp) log(.25) else .25,
    max(length(q), NROW(mu))))
  body(f$qf) <- quote(rep(17, max(length(p), NROW(mu))))
  dist <- distribution_from_family(f, cbind(1:2, 0, 0, 0), 1, 1)
  expect_equal(dist$cdf(0), rep(.25, 2))
  expect_equal(dist$qf(.5), rep(17, 2))
  expect_identical(fix_family_cdf(f)$cdf, f$cdf)
  expect_identical(fix_family_qf(f)$qf, f$qf)
})

test_that("GEV response expectations retain the Gumbel limit", {
  shape <- c(-1e-16, 0, 1e-16, -.2, .2, 1, 2, NA_real_)
  dist <- distribution_from_family(mgcv::gevlss(),
    cbind(rep(2, length(shape)), log(3), shape), 1, 1)
  expect_equal(dist$mean[1:3], rep(2 + 3 * 0.577215664901533, 3))
  expect_equal(dist$mean[4:5], 2 + 3 * (gamma(1 - shape[4:5]) - 1) / shape[4:5])
  expect_equal(dist$mean[6:8], rep(NA_real_, 3))
})

test_that("gaulss CDF and quantile weights agree with the native generator", {
  mu <- cbind(c(1, 2, 3), c(.5, 1, 2))
  wt <- c(1, 4, 9)
  scale <- 2
  sigma <- sqrt(scale / wt) / mu[, 2]
  f <- mgcv::gaulss()
  dist <- distribution_from_family(f, mu, wt, scale)
  y <- c(0, 1, 4)
  expect_equal(dist$cdf(y), pnorm(y, mu[, 1], sigma))
  expect_equal(dist$qf(c(.1, .5, .9)), qnorm(c(.1, .5, .9), mu[, 1], sigma))
  expected <- withr::with_seed(15, rnorm(3, mu[, 1], sigma))
  expect_equal(withr::with_seed(15, dist$rd()), expected)
  expect_equal(cdf_gaulss(y, mu), pnorm(y, mu[, 1], 1 / mu[, 2]))
  expect_equal(qf_gaulss(.5, mu), mu[, 1])
})

test_that("CDF left limits are exact between count support values", {
  q <- c(0, .5, 1, 1.5, 2)
  base <- distribution_from_parameters("poisson", rep(2, length(q)))
  expect_equal(distribution_cdf_left(base, q), ppois(ceiling(q) - 1, 2))
  mixed <- zero_inflated_distribution(base, .3)
  expect_equal(distribution_cdf_left(mixed, q),
    .7 * ppois(ceiling(q) - 1, 2) + .3 * (q > 0))
  positive <- truncate_distribution(base, mean = rep(2 / (-expm1(-2)), length(q)))
  expect_equal(distribution_cdf_left(positive, q),
    pmax(0, (ppois(ceiling(q) - 1, 2) - ppois(0, 2)) / (-expm1(-2))))
  binomial <- distribution_from_parameters("binomial", rep(.4, 5), trials = 10)
  q <- c(0, .05, .1, .15, .2)
  expect_equal(distribution_cdf_left(binomial, q), pbinom(ceiling(q * 10) - 1, 10, .4))
})

test_that("zero-trial binomial distributions remain degenerate through adapters", {
  mu <- c(.2, .4, .7)
  trials <- c(0, 5, 0)
  base <- distribution_from_parameters("binomial", mu, trials = trials)
  expect_equal(base$mean[c(1, 3)], c(0, 0))
  expect_equal(base$variance[c(1, 3)], c(0, 0))
  expect_equal(base$rd()[c(1, 3)], c(0, 0))
  expect_equal(base$qf(.5)[c(1, 3)], c(0, 0))
  expect_equal(base$cdf(-.1)[c(1, 3)], c(0, 0))
  expect_equal(base$cdf(0)[c(1, 3)], c(1, 1))
  expect_equal(distribution_cdf_left(base, 0)[c(1, 3)], c(0, 0))
  expect_equal(distribution_cdf_left(base, .1)[c(1, 3)], c(1, 1))
  fam <- distribution_from_family(binomial(), mu, trials, 1)
  expect_equal(fam$mean[c(1, 3)], c(0, 0))
  expect_equal(fam$rd()[c(1, 3)], c(0, 0))
  expect_equal(fam$qf(.5)[c(1, 3)], c(0, 0))
  expect_equal(distribution_cdf_left(fam, .1)[c(1, 3)], c(1, 1))
  mixed <- zero_inflated_distribution(base, c(.2, .3, .4))
  expect_equal(mixed$qf(.5)[c(1, 3)], c(0, 0))
  expect_true(all(is.finite(distribution_quantile_residuals(mixed, c(0, .4, 0),
    type = "quantile", seed = 1))))
  indexed <- combine_response_distributions(list(base, fam), c(1, 1, 1, 2, 2, 2))
  expect_equal(indexed$rd()[c(1, 3, 4, 6)], rep(0, 4))
})
