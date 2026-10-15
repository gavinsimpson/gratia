ncv_gaussian_fixture <- function() {
  withr::local_seed(918)
  d <- data.frame(x = runif(100), z = runif(100), w = runif(100, .5, 2))
  mu <- sin(6 * d$x) + cos(7 * d$z)
  d$y <- mu + rnorm(nrow(d), sd = .4 / sqrt(d$w))
  b <- mgcv::gam(y ~ s(x, k = 5) + s(z, k = 5), data = d, weights = w,
    method = "NCV", nei = list(jackknife = FALSE))
  list(model = b, mean = mu)
}

test_that("Gaussian NCV operators reproduce independent leave-one-out fits", {
  fixture <- ncv_gaussian_fixture()
  b <- fixture$model
  setup <- sampling_ncv_gaussian_setup(b)
  op <- sampling_ncv_gaussian_eval(setup)
  X <- predict(b, type = "lpmatrix")
  w <- b$prior.weights
  # Omitting each row and solving separately does not use a leverage formula.
  S <- sampling_target_penalty(b, "smoothing-mixture")
  errors <- vapply(seq_along(b$y), function(i) {
    beta <- solve(crossprod(X[-i, ], w[-i] * X[-i, ]) + S,
      crossprod(X[-i, ], w[-i] * b$y[-i]))
    sqrt(w[i]) * (b$y[i] - drop(X[i, ] %*% beta))
  }, numeric(1L))
  expect_equal(op$criterion, sum(errors^2), tolerance = 1e-9)
  expect_equal(unname(drop(op$B %*% (sqrt(w) * b$y))), errors, tolerance = 1e-9)
  h <- 1e-5
  for (j in seq_along(setup$log_sp)) {
    step <- setup$log_sp * 0; step[j] <- h
    plus <- sampling_ncv_gaussian_eval(setup, setup$log_sp + step)
    minus <- sampling_ncv_gaussian_eval(setup, setup$log_sp - step)
    expect_equal(op$contributions[, j],
      (drop(plus$B %*% (sqrt(w) * b$y))^2 -
        drop(minus$B %*% (sqrt(w) * b$y))^2) / (2 * h), tolerance = 1e-6)
    expect_equal(op$K1[[j]],
      (crossprod(plus$B) - crossprod(minus$B)) / (2 * h), tolerance = 1e-6)
  }
})

test_that("Gaussian NCV gradient moments account for dependent contributions", {
  fixture <- ncv_gaussian_fixture()
  setup <- sampling_ncv_gaussian_setup(fixture$model)
  op <- sampling_ncv_gaussian_eval(setup)
  mu <- sqrt(setup$weights) * fixture$mean
  moments <- sampling_ncv_gaussian_moments(op, mu, .16)
  expect_equal(moments$gradient_covariance,
    moments$within_observation_covariance + moments$between_observation_covariance)
  expect_equal(moments$expected_crossprod,
    moments$within_observation_covariance + moments$mean_outer)
  expect_true(all(diag(moments$between_observation_covariance) < 0))
  # Independent response simulation verifies both the total-gradient covariance
  # and the different expectation of the native row-crossproduct construction.
  withr::local_seed(401)
  n_sim <- 20000L
  gradients <- matrix(NA_real_, n_sim, length(op$B1))
  crossprod_mean <- matrix(0, length(op$B1), length(op$B1))
  for (start in seq.int(1L, n_sim, by = 1000L)) {
    z <- matrix(rnorm(length(mu) * 1000L, sd = .4), length(mu)) + mu
    v <- op$B %*% z
    pieces <- lapply(op$B1, function(Bj) 2 * v * (Bj %*% z))
    gradients[start:(start + 999L), ] <- vapply(pieces, colSums, numeric(1000L))
    for (j in seq_along(pieces)) for (k in seq_along(pieces)) {
      crossprod_mean[j, k] <- crossprod_mean[j, k] +
        sum(pieces[[j]] * pieces[[k]]) / n_sim
    }
  }
  # Compare covariance in variance-standardised coordinates so small absolute
  # scores do not turn a large relative error into a passing tolerance.
  D <- diag(1 / sqrt(diag(moments$gradient_covariance)))
  expect_lt(max(abs(D %*% (cov(gradients) - moments$gradient_covariance) %*% D)), .05)
  D <- diag(1 / sqrt(diag(moments$expected_crossprod)))
  expect_lt(max(abs(D %*% (crossprod_mean - moments$expected_crossprod) %*% D)), .05)
  expect_lt(max(abs(colMeans(gradients) - moments$mean_gradient) /
    sqrt(diag(moments$gradient_covariance) / n_sim)), 4)
})

test_that("Gaussian NCV covariance has consistent response units and no posterior interpretation", {
  fixture <- ncv_gaussian_fixture()
  setup <- sampling_ncv_gaussian_setup(fixture$model)
  first <- sampling_ncv_gaussian_covariance(setup, fixture$mean, .16)
  second_setup <- setup
  second_setup$response <- 3 * setup$response
  second_setup$fitted <- 3 * setup$fitted
  second_setup$phi <- 9 * setup$phi
  second_setup$native$derivative <- 3 * setup$native$derivative
  second <- sampling_ncv_gaussian_covariance(second_setup, 3 * fixture$mean, 9 * .16)
  expect_equal(first$log_sp_covariance, second$log_sp_covariance, tolerance = 1e-7)
  expect_equal(first$coefficient_mean_contribution * 9,
    second$coefficient_mean_contribution, tolerance = 1e-7)
  expect_equal(first$coefficient_mean_contribution,
    setup$native$derivative %*% first$log_sp_covariance %*% t(setup$native$derivative))
  expect_match(first$interpretation, "estimator uncertainty")
  expect_false("bs" %in% names(first))
  expect_equal(first$mean_source, "supplied Gaussian mean")
  expect_equal(sampling_ncv_gaussian_covariance(setup)$mean_source, "fitted Gaussian mean")
  expect_error(sampling_ncv_gaussian_covariance(setup, 1), "one finite Gaussian mean")
  expect_error(sampling_ncv_gaussian_covariance(setup, dispersion = -1), "nonnegative")
  expect_error(sampling_ncv_gaussian_setup(fixture$model, max_n = 20L), "dense observation")
  fixture$model$call$nei <- quote(neighbourhoods_no_longer_available)
  expect_error(sampling_ncv_gaussian_setup(fixture$model), "cannot be verified")
})

test_that("Gaussian NCV research supports one free parameter and rejects changed targets", {
  fixture <- ncv_gaussian_fixture()
  b <- mgcv::gam(y ~ s(x, k = 5), data = fixture$model$model,
    weights = fixture$model$prior.weights, method = "QNCV", gamma = 1L)
  setup <- sampling_ncv_gaussian_setup(b)
  out <- sampling_ncv_gaussian_covariance(setup)
  expect_identical(dim(out$log_sp_covariance), c(1L, 1L))
  expect_identical(dim(out$hessian), c(1L, 1L))
  expect_true(is.finite(out$log_sp_covariance[1, 1]))
  b$call$gamma <- 2
  expect_error(sampling_ncv_gaussian_setup(b), "gamma = 1")
  b$call$gamma <- NULL
  b$prior.weights[1] <- 0
  expect_error(sampling_ncv_gaussian_setup(b), "positive finite precision weights")
})

test_that("NCV jackknife coefficient covariance is separate from smoothing covariance", {
  fixture <- ncv_gaussian_fixture()
  plain <- fixture$model
  jackknife <- mgcv::gam(y ~ s(x, k = 5) + s(z, k = 5),
    data = plain$model, weights = plain$prior.weights,
    method = "NCV", nei = list(jackknife = TRUE))
  for (nm in c("coefficients", "sp", "Vg", "Vc")) {
    expect_equal(jackknife[[nm]], plain[[nm]], tolerance = 1e-10)
  }
  expect_equal(attr(jackknife$Ve, "Vp"), plain$Vp)
  # Independent row-deletion coefficient fits, keeping the fitted smoothing
  # parameters fixed, reconstruct mgcv 1.9-4's Gaussian jackknife calculation.
  X <- predict(plain, type = "lpmatrix")
  w <- plain$prior.weights
  S <- sampling_target_penalty(plain, "smoothing-mixture")
  changes <- t(vapply(seq_along(plain$y), function(i) {
    drop(solve(crossprod(X[-i, ], w[-i] * X[-i, ]) + S,
      crossprod(X[-i, ], w[-i] * plain$y[-i]))) - coef(plain)
  }, numeric(ncol(X))))
  n <- nrow(X)
  Vj <- crossprod(changes) * ((n - 1) / n)^.8
  alpha <- sum(diag(Vj)) / sum(diag(plain$Ve))
  expected <- Vj + (plain$Vp - plain$Ve) * alpha
  expect_equal(attr(jackknife$Ve, "inv.learn"), alpha, tolerance = 1e-8)
  expect_equal(jackknife$Vp, unname(expected), tolerance = 1e-8)
  expect_gt(max(abs(jackknife$Vp - plain$Vp)), 1e-6)
  first <- sampling_ncv_gaussian_setup(plain)
  second <- sampling_ncv_gaussian_setup(jackknife)
  expect_false(first$native$coefficient_covariance_replaced)
  expect_true(second$native$coefficient_covariance_replaced)
  expect_equal(sampling_ncv_gaussian_covariance(first)$log_sp_covariance,
    sampling_ncv_gaussian_covariance(second)$log_sp_covariance)
})
