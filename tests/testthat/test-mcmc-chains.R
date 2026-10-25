sampling_values <- function(x) as.vector(x)

test_that("draw counts, chain IDs, and legacy MH values are correct", {
  m <- sampling_test_model()
  expected <- withr::with_seed(42, mgcv::gam.mh(m, ns = 40, burn = 10, thin = 2))
  actual <- post_draws(m, n_draws = 20, method = "mh", thin = 2, burnin = 10, seed = 42)
  expect_identical(sampling_values(actual), sampling_values(expected$bs))
  expect_equal(attr(actual, "fixed_acceptance"), expected$accept)
  expect_equal(attr(actual, "rw_acceptance"), expected$rw.accept)
  multi <- post_draws(m, n_draws = 20, method = "mh", chains = 4,
    burnin = 10, thin = 2, seed = 42)
  ids <- attr(multi, "sampling")$draw_ids
  expect_identical(dim(multi), c(20L, 5L))
  expect_equal(table(ids$.chain), table(rep(1:4, each = 5)))
  expect_identical(ids$.iteration, rep(1:5, 4))
  expect_error(post_draws(m, n_draws = 21, chains = 4), "divisible")
  expect_error(post_draws(m, n_draws = 0), "positive integer")
  expect_error(post_draws(m, n_draws = 4, chains = 0), "positive integer")
  expect_error(post_draws(m, n_draws = 4, method = "mh", n_cores = 0), "n_cores")
})

test_that("mirai and serial chains have the same draws and preserve caller state", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  m <- sampling_test_model()
  withr::local_seed(123)
  old <- .Random.seed
  kind <- RNGkind()
  for (backend in test_cases_for_cran(c("mgcv", "gratia"), "gratia")) {
    a <- post_draws(m, n_draws = 24, chains = 3, method = "mh", burnin = 10,
      seed = 7, mh_backend = backend)
    b <- post_draws(m, n_draws = 24, chains = 3, n_cores = 2, method = "mh", burnin = 10,
      seed = 7, mh_backend = backend)
    expect_identical(sampling_values(a), sampling_values(b))
    expect_identical(attr(a, "sampling")$draw_ids, attr(b, "sampling")$draw_ids)
    expect_identical(.Random.seed, old)
    expect_identical(RNGkind(), kind)
  }
  expect_false(mirai::daemons_set())
  mirai::daemons(2L, autoexit = NA)
  on.exit(mirai::daemons(0L), add = TRUE)
  expect_silent(post_draws(m, n_draws = 12, chains = 2, n_cores = 2,
    method = "mh", burnin = 5, seed = 7))
  expect_true(mirai::daemons_set())
  # A worker-side failure must not shut down the caller's pool.
  bad <- m
  bad$Vp[1, 1] <- -1
  expect_error(post_draws(bad, n_draws = 12, chains = 2, n_cores = 2,
    method = "mh", burnin = 5, seed = 7), "chain.*failed")
  expect_true(mirai::daemons_set())
})

test_that("posterior samples and derived quantities retain chain structure", {
  skip_if_not_installed("posterior")
  m <- sampling_test_model()
  b <- post_draws(m, n_draws = 40, chains = 4, seed = 5, diagnostics = TRUE)
  fs <- fitted_samples(m, method = "user", draws = b)
  ps <- posterior_samples(m, method = "user", draws = b, seed = 9)
  sm <- smooth_samples(m, method = "user", draws = b, n_grid = 5)
  ds <- derivative_samples(m, focal = "x", method = "user", draws = b, n_draws = 40, n_grid = 5)
  added <- add_posterior_samples(m$model, m, n_draws = 40, chains = 4,
    method = "mh", burnin = 5, seed = 9, diagnostics = TRUE)
  asm <- add_smooth_samples(m$model, m, n_draws = 40, chains = 4, seed = 9)
  for (x in list(fs, ps, sm, ds, added, asm)) {
    expect_true(all(c(".chain", ".iteration", ".draw") %in% names(x)))
    expect_identical(sort(unique(x$.draw)), 1:40)
    expect_identical(dim(as_mcmc_draws(x, format = "array"))[1:2], c(10L, 4L))
  }
  expect_identical(sampling_diagnostics(ps), sampling_diagnostics(b))
  expect_true(all(c("rhat", "ess_bulk", "ess_tail", "mcse_mean") %in%
    names(sampling_diagnostics(ps)$coefficients)))
  # Response noise has its own stream, and is not recycled coefficient noise.
  p1 <- posterior_samples(m, n_draws = 40, chains = 4, method = "mh", burnin = 5,
    seed = 9, diagnostics = TRUE)
  p2 <- posterior_samples(m, n_draws = 40, chains = 4, method = "mh", burnin = 5,
    seed = 9, diagnostics = TRUE)
  expect_identical(p1$.response, p2$.response)
  expect_false(identical(p1$.response[1:60], p1$.response[61:120]))
  expect_false(".chain" %in% names(fitted_samples(m, n_draws = 2, seed = 1)))
})

test_that("diagnostic conversions preserve variables and reject incomplete layouts", {
  skip_if_not_installed("posterior")
  skip_if_not_installed("coda")
  m <- sampling_test_model()
  b <- post_draws(m, n_draws = 40, chains = 4, seed = 11)
  a <- as_mcmc_draws(b, "array")
  expect_identical(dim(a), c(10L, 4L, 5L))
  expect_identical(as.vector(a[, 2, ]), as.vector(b[11:20, ]))
  expect_s3_class(as_mcmc_draws(b, "draws_array"), "draws_array")
  expect_s3_class(as_mcmc_draws(b, "draws_df"), "draws_df")
  expect_s3_class(as_mcmc_draws(b, "mcmc.list"), "mcmc.list")
  fs <- fitted_samples(m, method = "user", draws = b)
  expect_error(as_mcmc_draws(fs[fs$.draw != 1L, ]), "balanced")
  expect_error(as_mcmc_draws(rbind(fs, fs[1, ])), "More than one")
  expect_error(as_mcmc_draws(b[-1, ] |> structure(sampling = attr(b, "sampling"))), "lost rows")
  expect_error(as_mcmc_draws(fs[, setdiff(names(fs), ".chain")]), "Both")
  expect_error(post_draws(m, method = "user", draws = matrix(0, 40, 5), chains = 4), "mapping")
  selected <- post_draws(m, method = "user", draws = b, index = 2:3)
  expect_equal(as_mcmc_draws(selected, "array"), a[, , 2:3, drop = FALSE])
  stuck <- b
  stuck[1:10, ] <- stuck[1:10, ] + 100
  expect_true(any(posterior::summarise_draws(as_mcmc_draws(stuck))$rhat > 1.1))
})

test_that("common-family values, gradients, and compiled evaluations agree", {
  families <- list(gaussian(), poisson(), binomial(), Gamma(link = "log"),
    mgcv::nb(), mgcv::negbin(2))
  for (family in families) {
    m <- sampling_test_model(family)
    target <- sampling_target(m, compiled = FALSE)
    beta <- as.numeric(coef(m)) + seq_along(coef(m)) / 100
    a <- sampling_target_eval(target, beta, gradient = TRUE)
    eps <- 1e-5
    numerical <- vapply(seq_along(beta), function(j) {
      plus <- minus <- beta
      plus[j] <- plus[j] + eps
      minus[j] <- minus[j] - eps
      (sampling_target_eval(target, plus) - sampling_target_eval(target, minus)) / (2 * eps)
    }, numeric(1L))
    expect_equal(a$gradient, numerical, tolerance = 1e-6)
    target$compiled <- TRUE
    expect_equal(sampling_target_eval(target, beta, TRUE), a, tolerance = 1e-10)
    # Independently check density differences using mgcv's deviance and penalty.
    expected <- mgcv:::lpl(m, beta) - mgcv:::lpl(m, coef(m))
    actual <- a$value - sampling_target_eval(target, as.numeric(coef(m)))
    expect_equal(actual, drop(expected), tolerance = 1e-8)
    blocked <- sampling_target(m, dense_limit = 0, block_size = 13, compiled = TRUE)
    expect_null(blocked$X)
    expect_equal(sampling_target_eval(blocked, beta, TRUE), a, tolerance = 1e-10)
  }
})

test_that("penalty mapping, binomial encodings, and bam are supported", {
  withr::local_seed(51)
  d <- data.frame(x = runif(90), z = runif(90), y = rnorm(90))
  for (form in list(y ~ s(x, k = 5, id = 1) + s(z, k = 5, id = 1),
                   y ~ te(x, z, k = c(4, 4)), y ~ s(x, k = 5))) {
    m <- mgcv::gam(form, data = d, method = "REML", select = TRUE)
    t <- sampling_target(m)
    b <- coef(m) + 0.03
    expect_equal(sampling_target_eval(t, b) - sampling_target_eval(t, coef(m)),
      drop(mgcv:::lpl(m, b) - mgcv:::lpl(m, coef(m))), tolerance = 1e-8)
  }
  d$success <- rbinom(90, 5, 0.4)
  m <- mgcv::gam(cbind(success, 5 - success) ~ s(x, k = 5), data = d,
    family = binomial(), method = "REML")
  t <- sampling_target(m)
  b <- coef(m) + 0.1
  X <- predict(m, type = "lpmatrix")
  expected <- sum(dbinom(d$success, 5, plogis(X %*% b), log = TRUE) -
    dbinom(d$success, 5, plogis(X %*% coef(m)), log = TRUE)) -
    (sum(b * (t$S %*% b)) - sum(coef(m) * (t$S %*% coef(m)))) / 2
  expect_equal(sampling_target_eval(t, b) - sampling_target_eval(t, coef(m)), expected)
  for (discrete in c(FALSE, TRUE)) {
    bm <- sampling_test_model(bam = TRUE, discrete = discrete)
    t <- sampling_target(bm, dense_limit = 0, block_size = 15)
    expect_null(t$X)
    expect_identical(dim(post_draws(bm, n_draws = 8, chains = 2, method = "mh",
      mh_backend = "gratia", burnin = 5, seed = 1)), c(8L, 5L))
    expect_error(post_draws(bm, n_draws = 4, method = "mh"), "does not support bam")
    bm$AR1.rho <- 0.2
    expect_error(sampling_target(bm), "independent observations")
  }
  bad <- sampling_test_model()
  bad$call$H <- quote(diag(5))
  expect_error(sampling_target(bad), "paraPen.*H")
  bad$call$H <- NULL
  bad$full.sp <- c(1, 2)
  expect_error(sampling_target(bad), "match fitted smoothing")
})

test_that("MH and the alternative prototypes reproduce known posteriors", {
  skip_on_cran()
  skip_if_not_installed("posterior")
  withr::local_seed(135)
  m <- sampling_test_model()
  b <- post_draws(m, n_draws = 6000, method = "mh", mh_backend = "gratia",
    burnin = 500, seed = 42)
  expect_equal(colMeans(b), unname(coef(m)), tolerance = 0.06, ignore_attr = TRUE)
  expect_equal(unname(cov(b)), unname(vcov(m)), tolerance = 0.12)
  d <- data.frame(y = c(1, 2, 3, 2, 1, 1))
  m <- mgcv::gam(y ~ 1, data = d, family = poisson(), method = "REML")
  target <- sampling_target(m, compiled = TRUE)
  # Under a flat coefficient prior, exp(intercept) is Gamma(sum(y), nrow(d)).
  expected <- digamma(sum(d$y)) - log(nrow(d))
  variance <- trigamma(sum(d$y))
  for (sampler in c("mh", "mala", "elliptical")) {
    result <- switch(sampler,
      mh = sampling_mh(target, 4000, 500, 1, 40, 0.25),
      mala = sampling_mala(target, 4000, 500),
      elliptical = sampling_elliptical(target, 4000, 500))
    expect_equal(mean(result$bs), expected, tolerance = 0.05)
    moment <- (as.vector(result$bs) - expected)^2
    expect_lt(abs(mean(moment) - variance), 5 * posterior::mcse_mean(moment))
  }
})

test_that("chain IDs survive missing rows, offsets, and multinomial predictions", {
  skip_if_not_installed("posterior")
  withr::local_seed(128)
  d <- data.frame(x = runif(70), y = rnorm(70))
  d$x[5] <- NA
  m <- mgcv::gam(y ~ s(x, k = 5), data = d, method = "REML",
    na.action = na.exclude, offset = rep(0.2, 70))
  ps <- posterior_samples(m, n_draws = 24, chains = 3, seed = 9)
  expect_true(all(is.na(ps$.response[ps$.row == 5])))
  expect_identical(sort(unique(ps$.draw)), 1:24)
  expect_identical(dim(as_mcmc_draws(ps, "array")), c(8L, 3L, 70L))
  t <- sampling_target(m)
  b <- coef(m) + 0.1
  expect_equal(sampling_target_eval(t, b) - sampling_target_eval(t, coef(m)),
    drop(mgcv:::lpl(model_used_rows(m), b) - mgcv:::lpl(model_used_rows(m), coef(m))),
    tolerance = 1e-8)
  d <- data.frame(x = runif(90), y = sample(0:2, 90, replace = TRUE))
  m <- mgcv::gam(list(y ~ s(x, k = 4), ~s(x, k = 4)),
    data = d, family = mgcv::multinom(K = 2), method = "REML")
  b <- post_draws(m, n_draws = 24, chains = 3, method = "mh", burnin = 5, seed = 9,
    diagnostics = TRUE)
  expect_identical(dim(as_mcmc_draws(b, "array")), c(8L, 3L, 8L))
  ps <- posterior_samples(m, method = "user", draws = b, seed = 10)
  expect_identical(dim(as_mcmc_draws(ps, "array")), c(8L, 3L, 90L))
  expect_identical(sampling_diagnostics(ps), sampling_diagnostics(b))
  expect_error(post_draws(m, n_draws = 4, method = "mh", mh_backend = "gratia"), "supports Gaussian")
})

test_that("private mirai workers are cleaned up after a chain error", {
  skip_if_not_installed("mirai")
  m <- sampling_test_model()
  m$Vp[1, 1] <- -1
  original <- mirai::daemons
  profiles <- character()
  counts <- integer()
  local_mocked_bindings(daemons = function(n, ..., .compute = NULL) {
    profiles <<- c(profiles, .compute)
    counts <<- c(counts, n)
    original(n, ..., .compute = .compute)
  }, .package = "mirai")
  expect_error(post_draws(m, n_draws = 8, chains = 2, n_cores = 2,
    method = "mh", seed = 9), "chain.*failed")
  expect_identical(counts, c(2L, 0L))
  expect_identical(profiles[1], profiles[2])
  expect_false(mirai::daemons_set(.compute = profiles[1]))
})

test_that("the Poisson target agrees with numerical posterior integration", {
  d <- data.frame(y = c(1, 2, 3, 2, 1, 1))
  m <- mgcv::gam(y ~ 1, data = d, family = poisson(), method = "REML")
  target <- sampling_target(m)
  mode_value <- sampling_target_eval(target, coef(m))
  density <- Vectorize(function(b) exp(sampling_target_eval(target, b) - mode_value))
  normalizer <- integrate(density, -20, 10)$value
  mean <- integrate(function(b) b * density(b), -20, 10)$value / normalizer
  expect_equal(mean, digamma(sum(d$y)) - log(nrow(d)), tolerance = 1e-8)
})

test_that("all bam families and fitted dispersions use the mgcv coefficient target", {
  families <- list(gaussian(), poisson(), binomial(), Gamma(link = "log"),
    mgcv::nb(), mgcv::negbin(2))
  for (family in families) for (discrete in c(FALSE, TRUE)) {
    m <- sampling_test_model(family, bam = TRUE, discrete = discrete)
    target <- sampling_target(m, compiled = TRUE)
    b <- coef(m) + 0.01
    expect_equal(sampling_target_eval(target, b) - sampling_target_eval(target, coef(m)),
      drop(mgcv:::lpl(m, b) - mgcv:::lpl(m, coef(m))), tolerance = 1e-7)
    expect_identical(dim(post_draws(m, n_draws = 8, chains = 2, burnin = 5,
      method = "mh", mh_backend = "gratia", seed = 281)), c(8L, 5L))
  }
  m <- sampling_test_model(poisson())
  # Fitted dispersion is used even when explicitly set away from one.
  m$sig2 <- 1.7
  target <- sampling_target(m, compiled = TRUE)
  b <- coef(m) + 0.01
  expect_equal(sampling_target_eval(target, b) - sampling_target_eval(target, coef(m)),
    drop(mgcv:::lpl(m, b) - mgcv:::lpl(m, coef(m))), tolerance = 1e-8)
})
