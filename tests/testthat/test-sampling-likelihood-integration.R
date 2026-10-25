# The full suite checks combinations across models, families and plot options.
# Small function checks remain in test-function-core.R and focused test files.
skip_on_cran()

integration_chain_draws <- function(model, diagnostics = FALSE) {
  draws <- post_draws(model, n_draws = 160, chains = 4, seed = 278,
    mvn_method = "mgcv", diagnostics = diagnostics)
  metadata <- attr(draws, "sampling")
  permutation <- rev(seq_len(nrow(draws)))
  draws <- draws[permutation, , drop = FALSE]
  metadata$draw_ids <- metadata$draw_ids[permutation, , drop = FALSE]
  metadata$draw_ids$.draw <- 100L + 3L * metadata$draw_ids$.draw
  if (!is.null(metadata$coefficient_draws))
    metadata$coefficient_draws <- draws
  attr(draws, "sampling") <- metadata
  draws
}

test_that("likelihoods retain coefficient diagnostics and joint draw identities", {
  skip_if_not_installed("posterior")
  model <- sampling_linear_model
  draws <- integration_chain_draws(model, diagnostics = TRUE)
  before <- withr::with_seed(31, .Random.seed)
  withr::local_seed(31)
  ll <- log_lik(model, method = "user", draws = draws)
  expect_identical(.Random.seed, before)
  eta <- predict(model, type = "lpmatrix") %*% t(draws)
  expected <- t(vapply(seq_len(nrow(draws)), function(i)
    dnorm(model$y, eta[, i], sqrt(model$sig2), log = TRUE), numeric(length(model$y))))
  expect_equal(unname(matrix(ll, nrow(ll))), unname(expected))
  expect_identical(attr(ll, "sampling")$draw_ids, attr(draws, "sampling")$draw_ids)
  expect_equal(sampling_diagnostics(ll)$coefficients,
    sampling_diagnostics(draws)$coefficients)
  expect_identical(sampling_diagnostics(ll)$draws,
    sampling_diagnostics(draws)$draws)
  expect_s3_class(draw(sampling_diagnostics(ll)), "ggplot")
  expect_equal(dim(as_mcmc_draws(ll, format = "array")), c(40L, 4L, 32L))
  expect_error(log_lik(model, n_draws = 159, method = "user", draws = draws),
    "n_draws.*agree")
  unretained <- log_lik(model, method = "user", draws = integration_chain_draws(model))
  expect_null(sampling_diagnostics(unretained)$draws)
  expect_error(draw(sampling_diagnostics(unretained)), "Coefficient draws are unavailable")
  if (requireNamespace("loo", quietly = TRUE)) {
    generated <- log_lik(model, n_draws = 160, chains = 4, seed = 278,
      mvn_method = "mgcv", diagnostics = TRUE)
    result <- suppressWarnings(loo::loo(model, n_draws = 160, seed = 278,
      sampling_args = list(chains = 4, mvn_method = "mgcv", diagnostics = TRUE)))
    expect_equal(sampling_diagnostics(result)$coefficients,
      sampling_diagnostics(generated)$coefficients)
  }
  if (requireNamespace("bayesplot", quietly = TRUE)) {
    result <- pp_check.gam(model, seed = 42,
      sampling_args = list(method = "user", draws = draws))
    expect_equal(sampling_diagnostics(result)$coefficients,
      sampling_diagnostics(draws)$coefficients)
  }
})

test_that("likelihood adapters use each MH backend and parallel chain streams", {
  model <- mgcv::gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
  for (backend in c("mgcv", "gratia", "family")) {
    args <- list(model = model, n_draws = 80, chains = 4, method = "mh",
      mh_backend = backend, burnin = 25, thin = 2, seed = 278)
    draws <- do.call(post_draws, args)
    ll <- do.call(log_lik, args)
    direct <- log_lik(model, method = "user", draws = draws)
    expect_identical(as.numeric(ll), as.numeric(direct), info = backend)
    expect_identical(attr(ll, "sampling")$draw_ids,
      attr(draws, "sampling")$draw_ids, info = backend)
  }
  skip_if_not_installed("mirai")
  serial <- log_lik(model, n_draws = 80, chains = 4, method = "mh",
    mh_backend = "family", burnin = 25, seed = 278, n_cores = 1)
  parallel <- log_lik(model, n_draws = 80, chains = 4, method = "mh",
    mh_backend = "family", burnin = 25, seed = 278, n_cores = 2)
  expect_identical(as.numeric(serial), as.numeric(parallel))
  expect_identical(attr(serial, "sampling")$draw_ids,
    attr(parallel, "sampling")$draw_ids)
})

test_that("LOO uses stored iteration order without reordering likelihood weights", {
  skip_if_not_installed("loo")
  model <- sampling_linear_model
  draws <- integration_chain_draws(model)
  ids <- attr(draws, "sampling")$draw_ids
  ll <- log_lik(model, method = "user", draws = draws)
  temporal <- order(ids$.chain, ids$.iteration)
  ordered_ll <- ll[temporal, , drop = FALSE]
  eff <- loo::relative_eff(exp(sweep(ordered_ll, 2, apply(ordered_ll, 2, max))),
    chain_id = ids$.chain[temporal], cores = 1)
  direct <- suppressWarnings(loo::loo(ll, r_eff = eff, save_psis = TRUE, cores = 1))
  actual <- suppressWarnings(loo::loo(model,
    sampling_args = list(method = "user", draws = draws), save_psis = TRUE))
  expect_equal(actual$pointwise, direct$pointwise)
  expect_equal(actual$diagnostics, direct$diagnostics)
  expect_equal(stats::weights(actual$psis_object), stats::weights(direct$psis_object))
  expect_identical(attr(actual, "sampling")$draw_ids, ids)
  expect_error(loo::loo(model, sampling_args = list(method = "user", draws = draws),
    chain_id = rep(1L, nrow(draws)), r_eff = 1), "conflicts")
  waic <- suppressWarnings(loo::waic(model,
    sampling_args = list(method = "user", draws = draws)))
  expect_equal(waic$pointwise, suppressWarnings(loo::waic(ll))$pointwise)
  expect_identical(attr(waic, "sampling")$draw_ids, ids)
})

test_that("bayesplot aligns arbitrary draw IDs with likelihoods and responses", {
  skip_on_cran()
  skip_if_not_installed("bayesplot")
  skip_if_not_installed("loo")
  model <- sampling_linear_model
  draws <- integration_chain_draws(model)
  args <- list(method = "user", draws = draws)
  prepared <- suppressWarnings(bayesplot_prepare(model, "loo_pit_overlay", "ppc",
    NULL, 42, NULL, NULL, NULL, NULL, args, list()))
  response_seed <- withr::with_seed(42, sample.int(.Machine$integer.max, 1L))
  response <- posterior_samples(model, method = "user", draws = draws, seed = response_seed)
  ids <- attr(draws, "sampling")$draw_ids
  ordered <- order(match(response$.draw, ids$.draw), response$.row)
  expect_equal(prepared$args$yrep, matrix(response$.response[ordered], nrow(draws), byrow = TRUE))
  expect_identical(prepared$draw_ids, ids)
  direct <- suppressWarnings(loo::loo(model, sampling_args = args, save_psis = TRUE))
  expect_equal(prepared$args$lw, stats::weights(direct$psis_object, log = TRUE))
  expected <- bayesplot_prepare(model, "error_binned", "ppc", NULL, 42,
    NULL, "wt", NULL, NULL, args, list())
  expect_equal(expected$args$yrep, unname(t(predict(model, type = "lpmatrix") %*% t(draws))))
  plot <- pp_check.gam(model, sampling_args = args, seed = 42)
  expect_identical(attr(plot, "sampling")$draw_ids, ids)
})

test_that("integrated adapters reject obsolete and inactive sampling controls", {
  model <- sampling_linear_model
  expect_error(log_lik(model, n = 20), "removed.*n_draws")
  expect_error(log_lik(model, 20), "must be named")
  expect_error(log_lik(model, n_draws = 20, method = "mh", unconditional = FALSE),
    "unconditional.*not used")
  expect_error(log_lik(model, n_draws = 20, method = "mh", n_threads = 2),
    "n_threads.*not used")
  expect_error(log_lik(model, n_draws = 20, n_cores = 2), "n_threads")
  expect_error(log_lik(model, n_draws = 21, chains = 4), "divisible")
  if (requireNamespace("loo", quietly = TRUE)) {
    expect_error(loo::loo(model, n = 20), "removed.*n_draws")
    expect_error(loo::waic(model, n = 20), "removed.*n_draws")
    expect_error(loo::loo(model, n_draws = 20, sampling_args = list(freq = FALSE)),
      "removed.*frequentist")
  }
  if (requireNamespace("bayesplot", quietly = TRUE)) {
    expect_error(pp_check.gam(model, n = 20), "removed.*n_draws")
    expect_error(pp_check.gam(model, n_draws = 20, sampling_args = list(freq = FALSE)),
      "removed.*frequentist")
    expect_error(pp_check.gam(model, n_draws = 20, sampling_args = list(n = 20)),
      "removed.*n_draws")
    expect_error(pp_check.gam(model, n_draws = 20, sampling_args = list(chains = 3)),
      "divisible")
  }
})
