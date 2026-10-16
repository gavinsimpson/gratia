test_that("LOO and WAIC match direct calls with the same coefficient draws", {
  skip_if_not_installed("loo")
  set.seed(132)
  dat <- data.frame(x = seq(-1, 1, length.out = 45))
  dat$y <- sin(dat$x) + rnorm(nrow(dat), sd = 0.5)
  fit <- mgcv::gam(y ~ s(x, k = 5), data = dat, method = "REML")
  ll <- log_lik(fit, n_draws = 400, seed = 41)
  direct <- suppressWarnings(loo::loo(ll, r_eff = 1, save_psis = TRUE,
    cores = 1))
  result <- suppressWarnings(loo::loo(fit, n_draws = 400, seed = 41,
    save_psis = TRUE, cores = 1))
  expect_s3_class(result, "loo")
  expect_equal(result$estimates, direct$estimates)
  expect_equal(result$pointwise, direct$pointwise)
  expect_equal(result$diagnostics, direct$diagnostics)
  expect_equal(stats::weights(result$psis_object),
    stats::weights(direct$psis_object))
  expect_equal(attr(result, "observation"), attr(ll, "observation"))
  expect_equal(attr(result, "gratia"), attr(ll, "gratia"))
  expect_equal(suppressWarnings(loo::waic(fit, n_draws = 400, seed = 41))$estimates,
    suppressWarnings(loo::waic(ll))$estimates)
  expect_identical(getS3method("logLik", "gam"),
    getFromNamespace("logLik.gam", "mgcv"))
})

test_that("BAM methods use retained observations and sampler arguments", {
  skip_if_not_installed("loo")
  set.seed(1)
  dat <- data.frame(x = seq(-1, 1, length.out = 80))
  dat$y <- rpois(80, exp(dat$x))
  dat$x[c(2, 14)] <- NA_real_
  fit <- mgcv::bam(y ~ s(x, k = 5), data = dat, na.action = na.exclude)
  betas <- matrix(rep(coef(fit), each = 100), nrow = 100)
  args <- list(method = "user", draws = betas)
  ll <- do.call(log_lik, c(list(fit), args))
  result <- suppressWarnings(loo::loo(fit, sampling_args = args, r_eff = 1))
  expect_equal(result$pointwise,
    suppressWarnings(loo::loo(ll, r_eff = 1, cores = 1))$pointwise)
  expect_equal(nrow(result$pointwise), 78L)
  expect_equal(suppressWarnings(loo::waic(fit,
    sampling_args = args))$pointwise, suppressWarnings(loo::waic(ll))$pointwise)
  expect_error(loo::loo(fit, sampling_args = args), "chain_id.*r_eff")
})

test_that("LOO computes relative efficiency without reordering MH draws", {
  skip_if_not_installed("loo")
  set.seed(83)
  ll <- cbind(as.numeric(stats::arima.sim(list(ar = 0.6), n = 400)),
    as.numeric(stats::arima.sim(list(ar = 0.3), n = 400))) - 1000
  colnames(ll) <- c("4", "9")
  prep <- list(betas = matrix(0, nrow(ll), 1), rows = c(4L, 9L),
    method = "mh", metadata = list(method = "mh"), ll = ll)
  local_mocked_bindings(posterior_likelihood_evaluate =
    function(prep, rows) prep$ll[, rows, drop = FALSE])
  eff <- loo::relative_eff(exp(sweep(ll, 2, apply(ll, 2, max))),
    chain_id = rep(1L, nrow(ll)), cores = 1)
  actual <- suppressWarnings(posterior_likelihood_loo(prep))
  expected <- suppressWarnings(loo::loo(ll, r_eff = eff, cores = 1))
  expect_equal(actual$estimates, expected$estimates)
  expect_equal(actual$diagnostics, expected$diagnostics)
  expect_equal(attr(actual, "observation"), c(4L, 9L))

  prep$method <- "user"
  chain_id <- rep(1:2, each = nrow(ll) / 2)
  eff <- loo::relative_eff(exp(sweep(ll, 2, apply(ll, 2, max))),
    chain_id = chain_id, cores = 1)
  actual <- suppressWarnings(posterior_likelihood_loo(prep,
    chain_id = chain_id))
  expected <- suppressWarnings(loo::loo(ll, r_eff = eff, cores = 1))
  expect_equal(actual$estimates, expected$estimates)
})

test_that("LOO can select grouped observation positions", {
  skip_if_not_installed("loo")
  set.seed(3)
  prep <- list(betas = matrix(0, 400, 1), rows = c(3L, 7L, 8L),
    method = "gaussian", ll = matrix(rnorm(1200, -1, 0.2), 400))
  colnames(prep$ll) <- as.character(prep$rows)
  local_mocked_bindings(posterior_likelihood_evaluate =
    function(prep, rows) prep$ll[, rows, drop = FALSE])
  actual <- posterior_likelihood_loo(prep, observations = c(3, 1))
  expect_equal(actual$pointwise,
    loo::loo(prep$ll[, c(3, 1)], r_eff = 1, cores = 1)$pointwise)
  expect_identical(attr(actual, "observation"), c(8L, 3L))
  expect_error(posterior_likelihood_loo(prep, observations = 4),
    "observation positions")
})

test_that("LOO rejects missing efficiency information and invalid likelihoods", {
  skip_if_not_installed("loo")
  prep <- list(betas = matrix(0, 100, 1), rows = c(3L, 7L),
    method = "user", ll = matrix(-1, 100, 2))
  local_mocked_bindings(posterior_likelihood_evaluate =
    function(prep, rows) prep$ll[, rows, drop = FALSE])
  expect_error(posterior_likelihood_loo(prep), "chain_id.*r_eff")
  expect_error(posterior_likelihood_loo(prep, chain_id = c(1, 2)),
    "identify each draw")
  expect_error(posterior_likelihood_loo(prep, chain_id = rep(2L, 100)),
    "starting at one")
  expect_error(posterior_likelihood_loo(prep, r_eff = 0), "positive finite")
  expect_error(posterior_likelihood_loo(prep, r_eff = rep(1, 3)),
    "one per observation")
  prep$ll[6, 1] <- -Inf
  expect_error(posterior_likelihood_loo(prep, r_eff = 1),
    "observation 3, draw 6")
})

test_that("likelihoods reject dependent custom response distributions", {
  d <- data.frame(x = seq(-1, 1, length.out = 24))
  d$y <- sin(d$x) + .3 * cos(seq_len(nrow(d)))
  fit <- mgcv::gam(y ~ x, data = d, method = "REML")
  for (has_likelihood in c(FALSE, TRUE)) {
    fit$family$gratia_diagnostics <- local({
      supplied <- has_likelihood
      function(mu, wt, scale, y, eta) {
        result <- list(independent = FALSE,
          dependence_reason = "Responses share residual errors")
        if (supplied) result$log_lik <- function(y)
          dnorm(y, mu, sqrt(scale / wt), log = TRUE)
        result
      }
    })
    expect_error(log_lik(fit, n_draws = 2, seed = 1),
      "independent|dependen|residual errors")
    if (requireNamespace("loo", quietly = TRUE)) {
      expect_error(loo::loo(fit, n_draws = 2, seed = 1),
        "independent|dependen|residual errors")
      expect_error(loo::waic(fit, n_draws = 2, seed = 1),
        "independent|dependen|residual errors")
    }
  }

  d$family_id <- rep(1:2, each = 12)
  grouped <- mgcv::gam(cbind(y, family_id) ~ x, data = d,
    family = mgcv::gfam(list(gaussian(), gaussian())), method = "REML")
  families <- grouped$family$getfl()
  families[[2L]]$gratia_diagnostics <- function(mu, wt, scale, y, eta)
    list(independent = FALSE, dependence_reason = "Shared component errors")
  grouped$family$getfl <- function() families
  expect_error(log_lik(grouped, n_draws = 2, seed = 1),
    "independent|dependen|component errors")
})

test_that("multivariate likelihoods honour explicit family hooks", {
  d <- data.frame(x = seq(-1, 1, length.out = 24))
  d$y1 <- sin(d$x) + .3 * cos(seq_len(nrow(d)))
  d$y2 <- d$x + .4 * sin(seq_len(nrow(d)))
  fit <- mgcv::gam(list(y1 ~ x, y2 ~ x), data = d,
    family = mgcv::mvn(d = 2), method = "REML")
  fit$family$gratia_diagnostics <- function(mu, wt, scale, y, eta)
    list(log_lik = function(y) dnorm(y[, 1], mu[, 1], 1, log = TRUE) +
      dnorm(y[, 2], mu[, 2], 2, log = TRUE))
  ll <- log_lik(fit, method = "user", draws = matrix(coef(fit), 1L))
  mu <- fit$linear.predictors
  expected <- dnorm(d$y1, mu[, 1], 1, log = TRUE) +
    dnorm(d$y2, mu[, 2], 2, log = TRUE)
  expect_equal(as.numeric(ll), expected)
})
