smoothing_research_fixture <- function(method = "REML", bam = FALSE,
                                       linked = FALSE, fixed = FALSE,
                                       tensor = FALSE, known_scale = FALSE) {
  withr::local_seed(501)
  d <- data.frame(x = runif(180), z = runif(180), w = runif(180, .5, 2))
  d$o <- .2 + .3 * d$x
  d$y <- sin(6 * d$x) + cos(3 * d$z) + d$o + rnorm(180, sd = .3)
  f <- if (tensor) y ~ te(x, z, k = c(4, 4)) + offset(o) else
    if (linked) y ~ s(x, k = 5, id = "same") + s(z, k = 5, id = "same") + offset(o) else
    if (fixed) y ~ s(x, k = 5, sp = .1) + s(z, k = 5) + offset(o) else
      y ~ s(x, k = 5) + s(z, k = 5) + offset(o)
  fitter <- if (bam) mgcv::bam else mgcv::gam
  fit <- fitter(f, data = d, weights = w, method = method,
    scale = if (known_scale) .09 else 0)
  list(model = fit, data = d, formula = f)
}

test_that("smoothing setup preserves fitted Gaussian conditionals and mappings", {
  configurations <- list(list(), list(method = "ML"), list(bam = TRUE, method = "fREML"),
    list(linked = TRUE), list(linked = TRUE, bam = TRUE, method = "fREML"),
    list(fixed = TRUE), list(tensor = TRUE), list(known_scale = TRUE))
  for (configuration in configurations) {
    fit <- do.call(smoothing_research_fixture, configuration)$model
    setup <- sampling_smoothing_setup(fit, block_size = 27L)
    conditional <- sampling_smoothing_conditional(setup, log(fit$sp))
    expect_equal(conditional$mean, unname(coef(fit)), tolerance = 1e-7)
    expect_equal(conditional$covariance, unname(vcov(fit)), tolerance = 1e-6)
    expect_false(setup$audit$dispersion_sampled)
    expect_equal(setup$audit$dispersion, fit$sig2)
    expect_equal(ncol(setup$L), length(fit$sp))
    expect_equal(unname(exp(drop(setup$L %*% setup$log_sp) + setup$lsp0)),
      unname(fit$full.sp %||% fit$sp), tolerance = 1e-10)
  }
})

test_that("smoothing conditionals agree with independent Gaussian refits", {
  fixture <- smoothing_research_fixture()
  fit <- fixture$model
  setup <- sampling_smoothing_setup(fit)
  rho <- setup$log_sp + c(.8, -.7)
  conditional <- sampling_smoothing_conditional(setup, rho)
  refit <- mgcv::gam(fixture$formula, data = fixture$data, weights = w,
    method = "REML", sp = exp(rho), scale = fit$sig2)
  expect_equal(conditional$mean, unname(coef(refit)), tolerance = 1e-7)
  expect_equal(conditional$covariance, unname(vcov(refit)), tolerance = 1e-7)
  # Setup reads fitting quantities directly, without evaluating the model call.
  fit$call$data <- quote(data_that_is_no_longer_available)
  fit$call$weights <- quote(weights_that_are_no_longer_available)
  expect_equal(sampling_smoothing_setup(fit)$XtWy, setup$XtWy)
})

test_that("smoothing derivatives use the fitted coefficient coordinates", {
  for (bam in c(FALSE, TRUE)) for (linked in c(FALSE, TRUE)) {
    fit <- smoothing_research_fixture(bam = bam, linked = linked,
      method = if (bam) "fREML" else "REML")$model
    setup <- sampling_smoothing_setup(fit)
    h <- 1e-5
    numeric_derivative <- vapply(seq_along(setup$log_sp), function(j) {
      delta <- rep(0, length(setup$log_sp))
      delta[j] <- h
      (sampling_smoothing_conditional(setup, setup$log_sp + delta)$mean -
        sampling_smoothing_conditional(setup, setup$log_sp - delta)$mean) / (2 * h)
    }, numeric(length(coef(fit))))
    expect_equal(setup$audit$derivative, numeric_derivative, tolerance = 1e-6)
    if (bam) expect_identical(setup$audit$fitted_derivative_coordinates,
      "internal fitting basis") else expect_lt(setup$audit$derivative_error, 1e-7)
  }
})

test_that("smoothing covariance excludes the separate log-scale component", {
  fit <- smoothing_research_fixture(linked = TRUE)$model
  setup <- sampling_smoothing_setup(fit)
  expect_equal(ncol(setup$log_sp_cov), 1L)
  expect_equal(setup$log_sp_cov[1, 1], fit$V.sp[1, 1])
  expect_equal(setup$audit$V_sp_log_scale_position, 2L)
  expect_equal(setup$audit$mapping, matrix(1, 2, 1))
  bam <- smoothing_research_fixture(bam = TRUE, method = "fREML")$model
  audit <- sampling_smoothing_setup(bam)$audit
  expect_true(is.na(audit$V_sp_log_scale_position))
  expect_equal(audit$sp_vcov_log_scale_position, 3L)
})

test_that("smoothing mixtures are reproducible and preserve RNG state", {
  setup <- sampling_smoothing_setup(smoothing_research_fixture()$model)
  withr::local_seed(110)
  rng <- .Random.seed
  first <- sampling_smoothing_draws(setup, n_draws = 100L, seed = 24L)
  expect_identical(.Random.seed, rng)
  expect_identical(first, sampling_smoothing_draws(setup, n_draws = 100L, seed = 24L))
  expect_identical(dim(first$bs), c(100L, length(setup$names)))
  expect_identical(colnames(first$bs), setup$names)
  expect_equal(first$total_covariance, first$mean_conditional_covariance +
    first$covariance_conditional_means)
  expect_true(all(is.finite(first$bs)))
  expect_error(sampling_smoothing_draws(setup, n_draws = 0), "positive")
  expect_error(sampling_smoothing_draws(setup, log_sp_cov = matrix(-1, 2, 2)),
    "positive semidefinite")
})

test_that("zero log-smoothing uncertainty recovers the conditional Gaussian", {
  setup <- sampling_smoothing_setup(smoothing_research_fixture(linked = TRUE)$model)
  fixed <- sampling_smoothing_conditional(setup, setup$log_sp)
  draws <- sampling_smoothing_draws(setup, n_draws = 5000L, seed = 23L,
    log_sp_cov = matrix(0, 1, 1))
  expect_equal(draws$conditional_means,
    matrix(rep(fixed$mean, each = 5000L), ncol = length(fixed$mean),
      dimnames = list(NULL, setup$names)))
  expect_equal(draws$mean_conditional_covariance, fixed$covariance, tolerance = 1e-10)
  expect_equal(draws$covariance_conditional_means,
    matrix(0, length(fixed$mean), length(fixed$mean),
      dimnames = list(setup$names, setup$names)), tolerance = 1e-10)
  expect_lt(max(abs(colMeans(draws$bs) - fixed$mean) /
    sqrt(diag(fixed$covariance) / 5000)), 4)
  expect_lt(max(abs(diag(cov(draws$bs)) / diag(fixed$covariance) - 1)), .08)
})

test_that("smoothing setup uses retained fitting rows and call-level offsets", {
  withr::local_seed(36)
  d <- smoothing_research_fixture()$data
  d$x[c(5, 13)] <- NA_real_
  fit <- mgcv::gam(y ~ s(x, k = 5) + s(z, k = 5), data = d,
    weights = w, offset = o, na.action = na.exclude, method = "REML")
  setup <- sampling_smoothing_setup(fit, block_size = 23L)
  expect_equal(sampling_smoothing_conditional(setup, setup$log_sp)$mean,
    unname(coef(fit)), tolerance = 1e-7)
})

test_that("smoothing prototype rejects unsupported targets precisely", {
  d <- smoothing_research_fixture()$data
  fit <- mgcv::gam(y ~ s(x, k = 5), data = d, method = "REML")
  fit$family <- poisson()
  expect_error(sampling_smoothing_setup(fit), "Gaussian identity")
  fit$family <- gaussian()
  fit$method <- "GCV.Cp"
  expect_error(sampling_smoothing_setup(fit), "ML, REML, or fREML")
  fit$method <- "REML"
  fit$call$min.sp <- quote(c(.1))
  expect_error(sampling_smoothing_setup(fit), "min.sp")
  fit$call$min.sp <- NULL
  attr(fit$outer.info$hess, "hess1") <- diag(2)
  expect_error(sampling_smoothing_setup(fit), "edge-corrected")
  attr(fit$outer.info$hess, "hess1") <- NULL
  fit$V.sp <- NULL
  expect_error(sampling_smoothing_setup(fit), "covariance")
  discrete <- mgcv::bam(y ~ s(x, k = 5), data = d, discrete = TRUE)
  expect_error(sampling_smoothing_setup(discrete), "compressed fitting design")
  dependent <- mgcv::bam(y ~ s(x, k = 5), data = d, rho = .2)
  expect_error(sampling_smoothing_setup(dependent), "independent observations")
})

test_that("nonzero smoothing mixtures match independent numerical integration", {
  setup <- sampling_smoothing_setup(smoothing_research_fixture(linked = TRUE)$model)
  j <- which.max(diag(setup$audit$conditional_covariance))
  sd_rho <- sqrt(setup$log_sp_cov[1, 1])
  first <- function(z) vapply(z, function(value) {
    conditional <- sampling_smoothing_conditional(setup,
      setup$log_sp + sd_rho * value)
    conditional$mean[j] * dnorm(value)
  }, numeric(1L))
  second <- function(z) vapply(z, function(value) {
    conditional <- sampling_smoothing_conditional(setup,
      setup$log_sp + sd_rho * value)
    (conditional$covariance[j, j] + conditional$mean[j]^2) * dnorm(value)
  }, numeric(1L))
  mean <- integrate(first, -9, 9, rel.tol = 1e-8)$value
  variance <- integrate(second, -9, 9, rel.tol = 1e-8)$value - mean^2
  mixture <- sampling_smoothing_draws(setup, n_draws = 5000L, seed = 810L)
  expect_lt(abs(mean(mixture$conditional_means[, j]) - mean) / sqrt(variance), .02)
  expect_lt(abs(mixture$total_covariance[j, j] / variance - 1), .04)
  expect_lt(abs(var(mixture$bs[, j]) / variance - 1), .08)
})

test_that("legacy smoothing research entry point uses the verified prototype", {
  fit <- smoothing_research_fixture(linked = TRUE)$model
  setup <- sampling_smoothing_setup(fit)
  legacy <- sampling_smoothing_mixture(fit, n = 12L, seed = 27L)
  current <- sampling_smoothing_draws(setup, n_draws = 12L, seed = 27L)
  expect_identical(legacy$bs, current$bs)
  expect_identical(legacy$log_sp, current$log_sp)
  expect_identical(legacy$failures, rep(NA_character_, 12L))
})
