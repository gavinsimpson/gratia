adjustment_fixture <- function() {
  withr::local_seed(630)
  d <- data.frame(x = seq(0, 1, length.out = 120))
  d$y <- sin(2 * pi * d$x) + rnorm(120, sd = .5)
  mgcv::gam(y ~ s(x, k = 6), data = d, method = "REML")
}

adjustment_draws <- function(fit, n = 800L, chains = 4L) {
  withr::local_seed(631)
  p <- length(coef(fit))
  x <- matrix(rt(n * p, df = 7), n, p) %*% chol(fit$Vp)
  x <- sweep(x, 2, coef(fit), "+")
  colnames(x) <- names(coef(fit))
  sampling_finish(x, sampling_layout(n, chains), info = data.frame(
    .chain = seq_len(chains), fixed_acceptance = .6, rw_acceptance = .3))
}

test_that("one pooled adjustment preserves means and exactly targets Vc", {
  fit <- adjustment_fixture()
  x <- adjustment_draws(fit)
  withr::local_seed(632)
  state <- .Random.seed
  before <- x
  out <- sampling_mh_covariance_adjust(fit, x)
  audit <- attr(out, "sampling")$covariance_adjustment
  expect_identical(.Random.seed, state)
  expect_identical(x, before)
  expect_equal(colMeans(out), colMeans(x), tolerance = 1e-12)
  expect_equal(unname(cov(out)), unname(fit$Vc), tolerance = 1e-10)
  expect_lt(audit$covariance_relative_error, 1e-12)
  expect_identical(dimnames(out), dimnames(x))
  expect_identical(attr(out, "sampling")$draw_ids, attr(x, "sampling")$draw_ids)
  expect_identical(attr(out, "sampling")$chains, attr(x, "sampling")$chains)
  independent <- t(audit$transformation) %*% cov(x) %*% audit$transformation
  expect_equal(unname(independent), unname(fit$Vc), tolerance = 1e-10)
  # An invertible affine map retains the original nonnormal joint geometry.
  restored <- sweep(sweep(out, 2, colMeans(out), "-") %*%
    solve(audit$transformation), 2, colMeans(x), "+")
  expect_equal(unname(restored), unname(x), ignore_attr = TRUE, tolerance = 1e-10)
  expect_equal(sampling_mh_covariance_adjust(fit, x), out)
})

test_that("equal empirical and target covariances leave draws unchanged", {
  fit <- adjustment_fixture()
  x <- adjustment_draws(fit)
  fit$Vc <- cov(x)
  out <- sampling_mh_covariance_adjust(fit, x)
  expect_equal(as.vector(out), as.vector(x), tolerance = 1e-12)
})

test_that("reordered names and unnamed native draws are handled explicitly", {
  fit <- adjustment_fixture()
  x <- adjustment_draws(fit)
  permutation <- rev(seq_len(ncol(x)))
  shuffled <- x[, permutation, drop = FALSE]
  expect_error(sampling_mh_covariance_adjust(fit, shuffled), "fitted coefficient order")
  plain <- x
  attributes(plain) <- list(dim = dim(x))
  out <- sampling_mh_covariance_adjust(fit, plain)
  expect_null(colnames(out))
  expect_identical(attr(out, "sampling")$draw_ids$.chain, rep(1L, nrow(x)))
  expect_equal(unname(cov(out)), unname(fit$Vc), tolerance = 1e-10)
})

test_that("the adjustment refreshes diagnostics and retains the originals", {
  skip_if_not_installed("posterior")
  fit <- adjustment_fixture()
  x <- adjustment_draws(fit)
  x <- sampling_finish(x, attr(x, "sampling")$draw_ids,
    info = attr(x, "sampling")$chains, diagnostics = TRUE,
    coefficient_names = colnames(x))
  out <- sampling_mh_covariance_adjust(fit, x)
  audit <- attr(out, "sampling")$covariance_adjustment
  expect_equal(audit$source_diagnostics, sampling_diagnostics(x))
  direct <- posterior::summarise_draws(as_mcmc_draws(out),
    "mean", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean", "mcse_sd")
  expect_equal(sampling_diagnostics(out)$coefficients, direct)
  expect_false(isTRUE(all.equal(sampling_diagnostics(x)$coefficients$sd,
    sampling_diagnostics(out)$coefficients$sd)))
  no_diagnostics <- sampling_mh_covariance_adjust(fit, x, diagnostics = FALSE)
  expect_null(sampling_diagnostics(no_diagnostics)$coefficients)
  expect_equal(attr(no_diagnostics, "sampling")$covariance_adjustment$source_diagnostics,
    sampling_diagnostics(x))
})

test_that("a pooled map does not separately recenter disagreeing chains", {
  skip_if_not_installed("posterior")
  fit <- adjustment_fixture()
  x <- adjustment_draws(fit)
  ids <- attr(x, "sampling")$draw_ids
  x[, 1] <- x[, 1] + ifelse(ids$.chain <= 2, -3, 3)
  out <- sampling_mh_covariance_adjust(fit, x, diagnostics = TRUE)
  A <- attr(out, "sampling")$covariance_adjustment$transformation
  difference <- colMeans(x[ids$.chain == 1, ]) - colMeans(x[ids$.chain == 4, ])
  observed <- colMeans(out[ids$.chain == 1, ]) - colMeans(out[ids$.chain == 4, ])
  expect_equal(unname(observed), unname(drop(difference %*% A)), tolerance = 1e-10)
  expect_gt(max(sampling_diagnostics(out)$coefficients$rhat), 1.1)
})

test_that("invalid or unavailable covariance cannot silently fall back", {
  fit <- adjustment_fixture()
  x <- adjustment_draws(fit)
  expect_error(sampling_mh_covariance_adjust(fit, x[, -1]), "all model coefficients")
  expect_error(sampling_mh_covariance_adjust(fit, x[1:ncol(x), ]), "More retained draws")
  wrong <- x
  colnames(wrong)[1] <- "wrong"
  expect_error(sampling_mh_covariance_adjust(fit, wrong), "fitted coefficient order")
  wrong <- x
  wrong[1, 1] <- NA_real_
  expect_error(sampling_mh_covariance_adjust(fit, wrong), "finite numeric matrix")
  wrong <- x
  wrong[, 2] <- wrong[, 1]
  expect_error(sampling_mh_covariance_adjust(fit, wrong), "singular")
  expect_error(sampling_mh_covariance_adjust(fit, x, diagnostics = NA), "TRUE or FALSE")
  expect_error(sampling_mh_covariance_adjust(list(), x), "gam or bam")
  fit$Vc <- NULL
  expect_error(sampling_mh_covariance_adjust(fit, x), "no unconditional covariance")
  for (V in list(matrix(1, 2, 2), fit$Vp * NA_real_, -fit$Vp,
      matrix(1, ncol(x), ncol(x)))) {
    fit$Vc <- V
    expect_error(sampling_mh_covariance_adjust(fit, x),
      "finite matrix|positive definite|singular")
  }
  fit$Vc <- fit$Vp
  fit$Vc[1, 2] <- fit$Vc[1, 2] + 1
  expect_error(sampling_mh_covariance_adjust(fit, x), "symmetric")
})

test_that("invalid chain metadata and repeated adjustment are rejected", {
  fit <- adjustment_fixture()
  x <- adjustment_draws(fit)
  out <- sampling_mh_covariance_adjust(fit, x)
  expect_error(sampling_mh_covariance_adjust(fit, out), "already")
  attr(x, "sampling")$draw_ids$.draw[2] <- 1L
  expect_error(sampling_mh_covariance_adjust(fit, x), "Invalid chain mapping")
})

test_that("adjusted MH draws propagate through sampled quantities", {
  skip_if_not_installed("posterior")
  fit <- adjustment_fixture()
  x <- mh_draws(fit, n_draws = 800, chains = 4, burnin = 150,
    mh_backend = "gratia", diagnostics = TRUE, seed = 633)
  out <- sampling_mh_covariance_adjust(fit, x)
  d <- data.frame(x = c(.2, .5, .8))
  X <- predict(fit, newdata = d, type = "lpmatrix")
  fitted <- fitted_samples(fit, data = d, method = "user", draws = out)
  expect_equal(sort(fitted$.fitted), sort(as.vector(X %*% t(out))))
  expect_equal(attr(fitted, "sampling")$covariance_adjustment,
    attr(out, "sampling")$covariance_adjustment)
  for (fun in list(posterior_samples, smooth_samples, derivative_samples)) {
    result <- if (identical(fun, derivative_samples)) {
      fun(fit, data = d, focal = "x", method = "user", draws = out, seed = 634)
    } else {
      fun(fit, data = d, method = "user", draws = out, seed = 634)
    }
    expect_true(all(c(".draw", ".chain", ".iteration") %in% names(result)))
    expect_identical(sort(unique(result$.draw)), seq_len(nrow(out)))
    expect_equal(attr(result, "sampling")$covariance_adjustment,
      attr(out, "sampling")$covariance_adjustment)
    expect_equal(sampling_diagnostics(result), sampling_diagnostics(out))
  }
})

test_that("the adjustment works for BAM and is not a public method", {
  fit <- adjustment_fixture()
  d <- model.frame(fit)
  fit <- mgcv::bam(y ~ s(x, k = 6), data = d, method = "fREML")
  x <- mh_draws(fit, n_draws = 400, chains = 4, burnin = 100,
    mh_backend = "gratia", seed = 635)
  out <- sampling_mh_covariance_adjust(fit, x)
  expect_equal(unname(cov(out)), unname(fit$Vc), tolerance = 1e-9)
  expect_false("sampling_mh_covariance_adjust" %in% getNamespaceExports("gratia"))
  expect_error(post_draws(fit, n_draws = 400, method = "mh", unconditional = TRUE),
    "not used")
})

test_that("unrepresentable covariance adjustments fail explicitly", {
  fit <- adjustment_fixture()
  x <- adjustment_draws(fit)
  overflowing <- x * 1e200
  expect_true(all(is.finite(overflowing)))
  expect_error(sampling_mh_covariance_adjust(fit, overflowing),
    "empirical MH covariance must contain only finite values")
  shifted <- x + 1e12
  fit$Vc <- diag(1e-20, ncol(x))
  expect_true(all(is.finite(cov(shifted))))
  expect_error(sampling_mh_covariance_adjust(fit, shifted),
    "target covariance cannot be represented accurately")
})
