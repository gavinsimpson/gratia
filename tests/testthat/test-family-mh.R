expect_family_target <- function(model) {
  target <- sampling_family_target(model)
  compact <- model_used_rows(model)
  X <- mgcv::predict.gam(compact, newdata = compact$model, type = "lpmatrix")
  if (identical(compact$family$family, "Multivariate normal")) {
    attr(X, "XX") <- crossprod(X[, seq_len(max(unlist(attr(X, "lpi")))), drop = FALSE])
  }
  base <- coef(model)
  for (delta in c(-0.03, 0.01, 0.05)) {
    beta <- base + delta * seq_along(base) / length(base)
    actual <- sampling_target_eval(target, beta) - sampling_target_eval(target, base)
    expected <- mgcv:::lpl(compact, beta, X) - mgcv:::lpl(compact, base, X)
    expect_equal(actual, as.numeric(expected), tolerance = 1e-8)
  }
  target
}

test_that("the family backend matches common-family gam and bam targets", {
  for (family in list(gaussian(), poisson(), binomial(), Gamma(link = "log"),
                      mgcv::nb(), mgcv::negbin(2))) {
    for (fit in c("gam", "bam", "discrete")) {
      model <- family_mh_model(family, bam = fit != "gam", discrete = fit == "discrete")
      target <- expect_family_target(model)
      specialised <- sampling_target(model)
      beta <- coef(model) + 0.01
      expect_equal(sampling_target_eval(target, beta) - sampling_target_eval(target, coef(model)),
        sampling_target_eval(specialised, beta) - sampling_target_eval(specialised, coef(model)),
        tolerance = 1e-8)
      family_draws <- post_draws(model, n_draws = 12, chains = 2, method = "mh",
        mh_backend = "family", burnin = 5, seed = 281)
      compiled_draws <- post_draws(model, n_draws = 12, chains = 2, method = "mh",
        mh_backend = "gratia", burnin = 5, seed = 281)
      expect_equal(as.vector(family_draws), as.vector(compiled_draws), tolerance = 1e-10)
      expect_identical(sampling_diagnostics(family_draws)$chains$backend, rep("family", 2))
    }
  }
})

test_that("family callbacks retain grouped and multiple-predictor mappings", {
  for (model in list(m_multinom, m_gaulss, m_mvn, gfam_model)) {
    target <- expect_family_target(model)
    expect_identical(attr(target$X, "lpi"), attr(model.matrix(model), "lpi"))
    draws <- post_draws(model, n_draws = 12, chains = 3, method = "mh",
      mh_backend = "family", burnin = 5, seed = 9)
    expect_identical(dim(draws), c(12L, length(coef(model))))
    expect_true(all(is.finite(draws)))
    response <- posterior_samples(model, n_draws = 12, chains = 3, method = "mh",
      mh_backend = "family", burnin = 5, seed = 9)
    expect_identical(sort(unique(response$.chain)), 1:3)
    expect_identical(sort(unique(response$.draw)), 1:12)
  }
})

test_that("extended families and alternative links use their fitted evaluators", {
  withr::local_seed(13)
  d <- data.frame(x = runif(120), off = runif(120, -0.1, 0.1), w = rep(c(1, 2), 60))
  eta <- sin(2 * pi * d$x) / 2 + d$off
  responses <- list(
    rbeta(120, 4 * plogis(eta), 4 * (1 - plogis(eta))),
    eta + stats::rt(120, 5),
    mgcv::rTweedie(exp(eta), p = 1.5, phi = 0.5),
    sample.int(3, 120, replace = TRUE),
    rbinom(120, 1, pnorm(eta)))
  families <- list(mgcv::betar(), mgcv::scat(), mgcv::tw(), mgcv::ocat(R = 3),
    binomial(link = "probit"))
  for (i in seq_along(families)) {
    d$y <- responses[[i]]
    model <- mgcv::gam(y ~ s(x, k = 5) + offset(off), data = d,
      family = families[[i]], weights = w, method = "REML")
    expect_family_target(model)
    draws <- post_draws(model, n_draws = 4, method = "mh", mh_backend = "family",
      burnin = 2, seed = 9)
    expect_true(all(is.finite(draws)))
  }
})

test_that("censoring and excluded rows survive family setup", {
  withr::local_seed(13)
  d <- data.frame(x = runif(100), off = runif(100, -0.1, 0.1), w = rep(c(1, 2), 50))
  y <- rnorm(100, sin(2 * pi * d$x), 0.7)
  d$y <- cbind(pmax(y, 0), ifelse(y < 0, -Inf, y))
  d$x[7] <- NA
  model <- mgcv::gam(y ~ s(x, k = 5) + offset(off), data = d,
    family = mgcv::cnorm(), weights = w, method = "REML", na.action = na.exclude)
  target <- expect_family_target(model)
  expect_equal(nrow(target$X), 99)
  draws <- post_draws(model, n_draws = 12, chains = 3, method = "mh",
    mh_backend = "family", burnin = 5, seed = 9)
  expect_true(all(is.finite(draws)))
  fitted <- fitted_samples(model, method = "user", draws = draws)
  expect_true(all(is.na(fitted$.fitted[fitted$.row == 7])))
})

test_that("multiple predictors retain fitting offsets and weights", {
  withr::local_seed(13)
  d <- data.frame(x = runif(100), z = runif(100), off = runif(100, -0.1, 0.1),
    w = rep(c(1, 2), 50))
  d$y <- rnorm(100, sin(2 * pi * d$x) + d$off, exp(d$z / 2))
  model <- mgcv::gam(list(y ~ s(x, k = 5) + offset(off), ~s(z, k = 5) + offset(off)),
    data = d, family = mgcv::gaulss(), weights = w, method = "REML")
  expect_type(model$offset, "list")
  expect_family_target(model)
})

test_that("family chains share setup and agree between serial and mirai runs", {
  skip_if_not_installed("mirai")
  withr::local_seed(9)
  original <- sampling_family_target
  setups <- 0L
  local_mocked_bindings(sampling_family_target = function(...) {
    setups <<- setups + 1L
    original(...)
  })
  for (model in list(family_mh_model(poisson()), m_multinom)) {
    before <- .Random.seed
    serial <- post_draws(model, n_draws = 24, chains = 3, method = "mh",
      mh_backend = "family", burnin = 5, thin = 2, seed = 281)
    expect_identical(setups, 1L)
    parallel <- post_draws(model, n_draws = 24, chains = 3, n_cores = 2, method = "mh",
      mh_backend = "family", burnin = 5, thin = 2, seed = 281)
    expect_identical(setups, 2L)
    expect_identical(as.vector(serial), as.vector(parallel))
    expect_identical(attr(serial, "sampling")$draw_ids, attr(parallel, "sampling")$draw_ids)
    expect_identical(.Random.seed, before)
    expect_identical(sampling_diagnostics(parallel)$chains$retained, rep(8, 3))
    setups <- 0L
  }
  expect_false(mirai::daemons_set())
})

test_that("family errors clean up workers without stopping the caller's pool", {
  skip_if_not_installed("mirai")
  model <- family_mh_model(poisson())
  model$family$dev.resids <- local({
    main <- Sys.getpid()
    original <- model$family$dev.resids
    function(...) {
      if (Sys.getpid() != main) stop("family callback failed")
      original(...)
    }
  })
  expect_error(post_draws(model, n_draws = 12, chains = 2, n_cores = 2,
    method = "mh", mh_backend = "family", burnin = 2, seed = 9), "chain.*failed")
  expect_false(mirai::daemons_set())
  mirai::daemons(2L, autoexit = NA)
  on.exit(mirai::daemons(0L), add = TRUE)
  expect_error(post_draws(model, n_draws = 12, chains = 2, n_cores = 2,
    method = "mh", mh_backend = "family", burnin = 2, seed = 9), "chain.*failed")
  expect_true(mirai::daemons_set())
})

test_that("family sampling passes through derived quantities and diagnostics", {
  skip_if_not_installed("posterior")
  model <- family_mh_model(poisson())
  for (fun in list(fitted_samples, smooth_samples, derivative_samples, posterior_samples)) {
    args <- list(model = model, n_draws = 40, chains = 4, method = "mh",
      mh_backend = "family", burnin = 5, seed = 281, diagnostics = TRUE)
    if (identical(fun, derivative_samples)) args$focal <- "x"
    samples <- do.call(fun, args)
    expect_identical(sort(unique(samples$.draw)), 1:40)
    expect_identical(sort(unique(samples$.chain)), 1:4)
    diagnostics <- sampling_diagnostics(samples)
    expect_identical(diagnostics$chains$backend, rep("family", 4))
    expect_true(all(c("rhat", "ess_bulk", "ess_tail", "mcse_mean") %in%
      names(diagnostics$coefficients)))
  }
})

test_that("family MH reproduces analytic Gaussian and Poisson posteriors", {
  withr::local_seed(281)
  y <- rnorm(30, 0.4, 1)
  model <- mgcv::gam(y ~ 1, data = data.frame(y = y), scale = 1)
  b <- post_draws(model, n_draws = 6000, chains = 4, method = "mh",
    mh_backend = "family", burnin = 250, seed = 281)
  expect_equal(mean(b), mean(y), tolerance = 0.015)
  expect_lt(abs(var(as.vector(b)) - 1 / length(y)), 0.004)
  d <- data.frame(y = c(1, 2, 3, 2, 1, 1))
  model <- mgcv::gam(y ~ 1, data = d, family = poisson())
  b <- post_draws(model, n_draws = 6000, chains = 4, method = "mh",
    mh_backend = "family", burnin = 250, seed = 281)
  expect_equal(mean(b), digamma(sum(d$y)) - log(nrow(d)), tolerance = 0.02)
  expect_lt(abs(var(as.vector(b)) - trigamma(sum(d$y))), 0.01)
})

test_that("unsupported models and inactive block controls give clear errors", {
  model <- family_mh_model(poisson())
  for (fun in list(post_draws, fitted_samples, posterior_samples, smooth_samples)) {
    expect_error(fun(model, n_draws = 4, method = "mh", mh_backend = "family",
      dense_limit = 0), "require mh_backend = 'gratia'", fixed = TRUE)
  }
  bad <- model
  bad$AR1.rho <- 0.2
  expect_error(sampling_family_target(bad), "independent observations")
  bad <- model
  bad$call$H <- quote(diag(5))
  expect_error(sampling_family_target(bad), "family MH backend.*paraPen")
  bad <- model
  bad$method <- "lme.REML"
  expect_error(sampling_family_target(bad), "GAMM components")
  bad <- model
  bad$family <- quasipoisson()
  expect_error(sampling_family_target(bad), "quasi families")
  bad$family <- mgcv::cox.ph()
  expect_error(sampling_family_target(bad), "Cox proportional hazards")
  bad$family <- poisson()
  bad$family$dev.resids <- NULL
  expect_error(sampling_family_target(bad), "requires 'll'")
  bad$family$dev.resids <- function(...) list(1)
  expect_error(sampling_family_target(bad), "invalid.*type")
  bad$family$dev.resids <- function(...) rep(Inf, 100)
  expect_error(sampling_family_target(bad), "non-finite posterior density")
  target <- sampling_family_target(model)
  expect_error(sampling_target_eval(target, coef(model), gradient = TRUE), "does not provide.*gradients")
  bad <- model
  bad$Vp[1, 1] <- -1
  expect_error(post_draws(bad, n_draws = 4, method = "mh", mh_backend = "family"),
    "family MH backend requires identifiable")
})

test_that("family penalties retain shared smoothing and tensor terms", {
  withr::local_seed(23)
  d <- data.frame(x = runif(100), z = runif(100), off = runif(100, -0.1, 0.1))
  d$y <- rpois(100, exp(sin(2 * pi * d$x) / 2 + d$off))
  for (formula in list(y ~ s(x, k = 5, id = 1) + s(z, k = 5, id = 1) + offset(off),
                       y ~ te(x, z, k = c(4, 4)) + offset(off))) {
    model <- mgcv::gam(formula, data = d, family = poisson(), method = "REML", select = TRUE)
    expect_family_target(model)
  }
  # Offsets passed as a fitting argument must enter the target as well.
  model <- mgcv::gam(y ~ s(x, k = 5), data = d, offset = off, family = poisson())
  expect_family_target(model)
})

test_that("family callbacks preserve binomial encodings", {
  withr::local_seed(23)
  d <- data.frame(x = runif(100), w = rep(c(0, 1, 2, 1), 25))
  d$success <- rbinom(100, 5, plogis(d$x))
  model <- mgcv::gam(cbind(success, 5 - success) ~ s(x, k = 5), data = d,
    weights = w, family = binomial(), method = "REML")
  expect_family_target(model)
  d$y <- factor(ifelse(d$success > 2, "yes", "no"))
  model <- mgcv::gam(y ~ s(x, k = 5), data = d, weights = w,
    family = binomial(), method = "REML")
  expect_family_target(model)
})

test_that("shared predictors and multivariate covariance coefficients keep their mapping", {
  withr::local_seed(23)
  d <- data.frame(x = runif(120), z = runif(120), a = runif(120),
    y = rnorm(120), y2 = rnorm(120))
  for (family in list(mgcv::gaulss(), mgcv::mvn(d = 2))) {
    formula <- if (identical(family$family, "Multivariate normal")) {
      list(y ~ s(x, k = 5), y2 ~ s(z, k = 5), 1 + 2 ~ 0 + a)
    } else list(y ~ s(x, k = 5), ~s(z, k = 5), 1 + 2 ~ 0 + a)
    model <- mgcv::gam(formula, data = d, family = family, method = "REML")
    target <- expect_family_target(model)
    expect_true(attr(attr(target$X, "lpi"), "overlap"))
    draws <- post_draws(model, n_draws = 8, chains = 2, method = "mh",
      mh_backend = "family", burnin = 3, seed = 9)
    expect_true(all(is.finite(draws)))
  }
})

test_that("family callbacks reject proposals outside a link's domain", {
  withr::local_seed(23)
  d <- data.frame(x = runif(100), y = rpois(100, 3))
  model <- mgcv::gam(y ~ x, data = d, family = poisson(link = "identity"))
  target <- sampling_family_target(model)
  invalid <- coef(model)
  invalid[1] <- -100
  expect_identical(sampling_target_eval(target, invalid), -Inf)
  expect_silent(post_draws(model, n_draws = 20, method = "mh", mh_backend = "family",
    burnin = 5, rw_scale = 100, seed = 9))
})
