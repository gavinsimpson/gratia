# The full suite checks combinations across models, families and plot options.
# Small function checks remain in test-function-core.R and focused test files.
skip_on_cran()

skip_if_not_installed("glmmTMB")

glmmtmb_dependence_fixture <- function() {
  withr::with_seed(31, {
    d <- data.frame(g = factor(rep(seq_len(8), each = 10)),
      x = rep(seq(-1, 1, length.out = 10), 8), id = factor(seq_len(80)))
    d$y <- .5 + d$x + rep(rnorm(8, sd = 1), each = 10) + rnorm(80, sd = .3)
    d$count <- rpois(80, exp(.7 + .2 * d$x + rep(rnorm(8, sd = .7), each = 10)))
    d
  })
}

test_that("glmmTMB dependence follows fitted latent covariance and conditioning", {
  d <- glmmtmb_dependence_fixture()
  plain <- glmmTMB::glmmTMB(count ~ x, family = poisson(), data = d)
  singleton <- glmmTMB::glmmTMB(count ~ x + (1 | id), family = poisson(), data = d,
    start = list(theta = log(.5)), map = list(theta = factor(NA)))
  slopes <- glmmTMB::glmmTMB(y ~ x + (1 + x | g), data = d,
    start = list(theta = c(0, log(.5), .75)), map = list(theta = factor(rep(NA, 3))))
  for (m in list(plain, singleton, slopes)) {
    conditional <- response_distribution(m, "conditional")
    expect_true(conditional$independent)
    expect_true(conditional$joint_available)
    expect_identical(conditional$conditioning, "conditional")
  }
  expect_true(response_distribution(plain, "marginal")$independent)
  expect_true(response_distribution(singleton, "marginal")$independent)
  marginal <- response_distribution(slopes, "marginal")
  expect_false(marginal$independent)
  expect_true(marginal$joint_available)
  expect_match(marginal$dependence_reason, "cond predictor")
  expect_null(marginal$cdf)
  expect_null(marginal$mean)
  samples <- withr::with_seed(201, replicate(500, marginal$rd()))
  expect_gt(cor(samples[1, ], samples[2, ]), .5)
  expect_lt(abs(cor(samples[1, ], samples[11, ])), .15)
})

test_that("structured temporal covariance remains joint under marginal sampling", {
  withr::local_seed(32)
  time <- seq_len(6)
  rho <- .7
  V <- rho^abs(outer(time, time, `-`))
  d <- data.frame(g = factor(rep(seq_len(10), each = 6)),
    time = factor(rep(time, 10)))
  errors <- replicate(10, as.vector(t(chol(V)) %*% rnorm(6)))
  d$y <- .5 + as.vector(errors) + rnorm(60, sd = .2)
  m <- glmmTMB::glmmTMB(y ~ ar1(time + 0 | g), data = d,
    start = list(theta = c(0, rho / sqrt(1 - rho^2))),
    map = list(theta = factor(c(NA, NA))))
  conditional <- response_distribution(m, "conditional")
  marginal <- response_distribution(m, "marginal")
  expect_true(conditional$independent)
  expect_false(marginal$independent)
  expect_true(marginal$joint_available)
  draws <- replicate(500, marginal$rd())
  empirical <- cor(t(draws[seq_len(6), ]))
  expect_gt(empirical[1, 2], empirical[1, 6] + .2)
  expected <- glmmTMB::VarCorr(m)$cond$g
  diag(expected) <- diag(expected) + sigma(m)^2
  expect_equal(unname(empirical), unname(cov2cor(matrix(expected, 6))), tolerance = .15)
  independent <- replicate(500, conditional$rd())
  expect_lt(abs(cor(independent[1, ], independent[2, ])), .15)
})

test_that("native spatial and slope covariance are not replaced by marginal draws", {
  withr::local_seed(33)
  coordinate <- seq(0, 5)
  V <- exp(-abs(outer(coordinate, coordinate, `-`)) / 2)
  d <- data.frame(g = factor(rep(seq_len(8), each = 6)),
    coordinate = rep(coordinate, 8))
  d$position <- glmmTMB::numFactor(d$coordinate, 0)
  d$y <- .7 + as.vector(replicate(8, t(chol(V)) %*% rnorm(6))) + rnorm(48, sd = .2)
  m <- glmmTMB::glmmTMB(y ~ exp(position + 0 | g), data = d,
    start = list(theta = c(0, log(2))), map = list(theta = factor(c(NA, NA))))
  marginal <- response_distribution(m, "marginal")
  expect_false(marginal$independent)
  expect_true(marginal$joint_available)
  draws <- replicate(500, marginal$rd())
  expect_gt(cor(draws[1, ], draws[2, ]), cor(draws[1, ], draws[6, ]) + .25)
  expect_true(response_distribution(m, "conditional")$independent)
})

test_that("nonlinear marginal residuals share one native integrated response law", {
  withr::local_options(list(gratia.diagnostics.marginal_draws = 99L))
  d <- glmmtmb_dependence_fixture()
  m <- glmmTMB::glmmTMB(count ~ x + (1 | g), family = poisson(), data = d,
    start = list(theta = log(.7)), map = list(theta = factor(NA)))
  proxy <- glmmtmb_diagnostic_model(m, "response", conditioning = "marginal")
  distribution <- response_distribution(proxy)
  expect_false(distribution$independent)
  expect_true(distribution$joint_available)
  expect_true(is.function(distribution$cdf))
  expect_match(distribution$approximation, "Monte Carlo|simulation|ensemble", ignore.case = TRUE)
  # E[exp(X beta + b)] exceeds exp(X beta) for a nondegenerate Gaussian b.
  expect_gt(mean(distribution$mean), 1.1 * mean(predict(m, re.form = NA, type = "response")))
  expect_equal(proxy$residuals, unname(proxy$y - distribution$mean))
  draw <- distribution$rd()
  expect_equal(diagnostic_reference_residuals(proxy, draw, "response"),
    draw - distribution$mean)
  expect_equal(diagnostic_reference_residuals(proxy, draw, "pearson"),
    (draw - distribution$mean) / sqrt(distribution$variance))
  expected <- distribution_quantile_residuals(distribution, proxy$y, "quantile", seed = 4)
  expect_equal(quantile_residuals(m, "quantile", seed = 4, conditioning = "marginal"), expected)
  expect_true(all(is.finite(expected)))
  expect_s3_class(qq_plot(m, conditioning = "marginal", type = "quantile",
    seed = 4, n_simulate = 8), "ggplot")
  expect_s3_class(qq_plot(m, conditioning = "marginal", type = "response",
    seed = 4, n_simulate = 8), "ggplot")
})

test_that("zero-inflation and dispersion shared effects count as dependence", {
  withr::local_seed(34)
  d <- glmmtmb_dependence_fixture()
  d$count[seq(1, 80, 3)] <- 0
  zero <- glmmTMB::glmmTMB(count ~ x, ziformula = ~ (1 | g), family = poisson(), data = d,
    start = list(thetazi = log(.8)), map = list(thetazi = factor(NA)))
  expect_false(response_distribution(zero, "marginal")$independent)
  expect_match(response_distribution(zero, "marginal")$dependence_reason, "zi predictor")
  dispersion <- glmmTMB::glmmTMB(y ~ x, dispformula = ~ (1 | g), data = d,
    start = list(thetadisp = log(.4)), map = list(thetadisp = factor(NA)))
  expect_false(response_distribution(dispersion, "marginal")$independent)
  expect_match(response_distribution(dispersion, "marginal")$dependence_reason, "disp predictor")
  expect_true(response_distribution(dispersion, "conditional")$independent)
})

test_that("native marginal ordinal draws retain the observed category scale", {
  skip_if_not(exists("ordinal", asNamespace("glmmTMB"), inherits = FALSE))
  withr::local_seed(35)
  withr::local_options(list(gratia.diagnostics.marginal_draws = 49L))
  d <- glmmtmb_dependence_fixture()
  latent <- .3 * d$x + rep(rnorm(8, sd = .6), each = 10) + rlogis(80)
  d$category <- ordered(cut(latent, c(-Inf, -.5, .7, Inf), labels = FALSE))
  m <- glmmTMB::glmmTMB(category ~ x + (1 | g), family = glmmTMB::ordinal(), data = d,
    start = list(theta = log(.6)), map = list(theta = factor(NA)))
  proxy <- glmmtmb_diagnostic_model(m, "quantile", conditioning = "marginal")
  expect_equal(proxy$y, as.numeric(d$category))
  expect_true(all(proxy$y %in% 1:3))
  expect_true(all(response_distribution(proxy)$rd() %in% 1:3))
  expect_true(all(is.finite(quantile_residuals(m, "quantile", seed = 2,
    conditioning = "marginal"))))
})

test_that("native nonGaussian temporal draws retain the latent correlation law", {
  withr::local_seed(81)
  withr::local_options(gratia.diagnostics.marginal_draws = 49L)
  d <- data.frame(g = factor(rep(1:8, each = 5)), time = factor(rep(1:5, 8)))
  d$y <- rpois(40, 3)
  rho <- .6; sd <- .5
  m <- glmmTMB::glmmTMB(y ~ ar1(time + 0 | g), data = d, family = poisson(),
    start = list(theta = c(log(sd), rho / sqrt(1 - rho^2))),
    map = list(theta = factor(c(NA, NA))))
  dist <- response_distribution(m, "marginal")
  draws <- replicate(1500, dist$rd())
  mean <- exp(glmmTMB::fixef(m)$cond[[1L]] + sd^2 / 2)
  variance <- mean + mean^2 * expm1(sd^2)
  expected <- mean^2 * expm1(sd^2 * rho) / variance
  expect_equal(cor(draws[1, ], draws[2, ]), expected, tolerance = .08)
  expect_lt(abs(cor(draws[1, ], draws[6, ])), .08)
  expect_true(response_distribution(m, "conditional")$independent)
  p <- qq_plot(m, conditioning = "marginal", type = "pit", n_simulate = 5, seed = 2)
  expect_identical(p$labels$subtitle, "Method: uniform")
  expect_true(all(is.finite(p$data$lower)))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("zero-trial matrix rows have calibrated residuals without native response residuals", {
  withr::local_seed(36)
  withr::local_options(list(gratia.diagnostics.marginal_draws = 49L))
  d <- data.frame(x = runif(30), trials = rep(10, 30))
  d$trials[c(3, 12)] <- 0
  d$success <- rbinom(30, d$trials, plogis(d$x))
  d$failure <- d$trials - d$success
  m <- glmmTMB::glmmTMB(cbind(success, failure) ~ x, family = binomial(), data = d)
  # Native response residuals are undefined at 0/0 rows; calibrated residuals
  # instead use the initialized zero proportion and its degenerate zero law.
  expect_true(any(!is.finite(residuals(m, type = "response"))))
  p <- glmmtmb_diagnostic_model(m, type = "quantile")
  expect_equal(unname(p$y[c(3, 12)]), c(0, 0))
  r <- quantile_residuals(m, type = "quantile", seed = 1)
  expect_true(all(is.finite(r)))
  expected <- withr::with_seed(1, qnorm(runif(30)))
  expect_equal(r[c(3, 12)], expected[c(3, 12)])
  u <- quantile_residuals(m, type = "pit", seed = 1, conditioning = "marginal")
  expect_true(all(is.finite(u)))
  expect_true(all(u >= 0 & u <= 1))
  expect_s3_class(qq_plot(m, type = "quantile", seed = 1), "ggplot")
})


test_that("Student-t population moment capabilities survive marginal calibration", {
  withr::local_seed(91)
  withr::local_options(gratia.diagnostics.marginal_draws = 49L)
  d <- glmmtmb_dependence_fixture()
  for (df in c(.8, 1.5, 4)) {
    m <- glmmTMB::glmmTMB(y ~ x + (1 | g), family = glmmTMB::t_family(), data = d,
      start = list(psi = log(df), theta = log(.5)),
      map = list(psi = factor(NA), theta = factor(NA)))
    expect_equal(unname(glmmTMB::family_params(m))[[1L]], df)
    raw <- response_distribution(m, "marginal")
    expect_identical(raw$mean_available, df > 1)
    expect_identical(raw$variance_available, df > 2)
    expect_true(raw$joint_available)
    expect_true(all(is.finite(raw$rd())))
    conditional <- response_distribution(m)
    expect_identical(conditional$mean_available, df > 1)
    expect_identical(conditional$variance_available, df > 2)
    prepared <- prepare_marginal_response_distribution(raw)
    expect_identical(is.null(prepared$mean), df <= 1)
    expect_identical(is.null(prepared$variance), df <= 2)
    expect_identical(prepare_marginal_response_distribution(prepared), prepared)
    proxy <- glmmtmb_diagnostic_model(m, "quantile", conditioning = "marginal")
    if (df <= 1) {
      expect_true(all(is.na(fitted(proxy))))
      expect_error(glmmtmb_diagnostic_model(m, "response", conditioning = "marginal"),
        "response residuals require a finite response mean")
      expect_error(glmmtmb_residuals(m, "response"), "finite response mean")
    } else {
      expect_equal(glmmtmb_marginal_residuals(prepared, proxy$y, "response"),
        unname(proxy$y - prepared$mean))
    }
    if (df <= 2) {
      expect_error(glmmtmb_marginal_residuals(prepared, proxy$y, "pearson"),
        "pearson residuals require a finite response (mean|variance)")
      expect_error(glmmtmb_residuals(m, "pearson"), "finite response (mean|variance)")
    } else {
      expect_equal(glmmtmb_marginal_residuals(prepared, proxy$y, "pearson"),
        unname((proxy$y - prepared$mean) / sqrt(prepared$variance)))
    }
    for (type in c("pit", "quantile")) {
      expect_true(all(is.finite(quantile_residuals(m, type, seed = 3,
        conditioning = "marginal"))))
      plot <- qq_plot(m, type = type, conditioning = "marginal", seed = 3,
        n_simulate = 4)
      expect_s3_class(plot, "ggplot")
      expect_silent(ggplot2::ggplot_build(plot))
    }
    dat <- diagnostic_data(m, type = "pit", conditioning = "marginal", seed = 3)
    expect_equal(NROW(dat$data), NROW(d))
    if (df <= 1) expect_true(all(is.na(dat$data$fitted)))
  }
})

test_that("shared residual adapters honour nonexistent population moments", {
  law <- new_response_distribution(NULL, rd = function() rcauchy(3),
    mean_available = FALSE, variance_available = FALSE,
    moment_reason = "Cauchy moments do not exist.")
  prepared <- prepare_marginal_response_distribution(law, n = 19L)
  expect_null(prepared$mean)
  expect_null(prepared$variance)
  mixed <- zero_inflated_distribution(prepared, .2)
  expect_null(mixed$mean)
  expect_false(mixed$mean_available)
  expect_false(mixed$variance_available)
  combined <- combine_response_distributions(list(prepared,
    distribution_from_parameters("gaussian", 0)), c(1, 1, 1, 2))
  expect_false(combined$mean_available)
  expect_false(combined$variance_available)
  expect_true(all(is.na(combined$mean[1:3])))
  expect_true(all(is.finite(distribution_quantile_residuals(prepared, 1:3))))
  proxy <- new_diagnostic_proxy(1:3, NULL, 1:3, gaussian(), prepared)
  expect_error(diagnostic_reference_residuals(proxy, 1:3, "response"),
    "finite response mean")
  expect_error(diagnostic_reference_residuals(proxy, 1:3, "pearson"),
    "finite response mean")
})


test_that("prepared glmmTMB diagnostics honor conditioning changes", {
  skip_if_not_installed("glmmTMB")
  withr::local_options(gratia.diagnostics.marginal_draws = 29L)
  set.seed(36)
  d <- data.frame(group = factor(rep(1:8, each = 8)))
  d$y <- rpois(64, exp(rep(rnorm(8, sd = .6), each = 8)))
  m <- glmmTMB::glmmTMB(y ~ 1 + (1 | group), family = poisson(), data = d)
  prepared <- diagnostic_data(m, type = "quantile", seed = 5)
  changed <- diagnostic_data(prepared, conditioning = "marginal", seed = 5)
  direct <- diagnostic_data(m, type = "quantile", conditioning = "marginal", seed = 5)
  expect_identical(changed$source$conditioning, "marginal")
  expect_equal(changed$data, direct$data)
  expect_identical(changed$source$distribution$calibration, "simulation ranks")
  plotted <- scalable_qq_plot(prepared, conditioning = "marginal", seed = 5,
    n_simulate = 3)
  expect_identical(plotted$labels$subtitle, "Method: std. normal")
})
