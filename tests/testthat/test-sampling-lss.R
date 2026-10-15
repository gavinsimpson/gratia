# All reference calculations below use the distribution definitions, separately
# from the parameter adapter used by the sampling functions.
lss_test_models <- local({
  cache <- new.env(parent = emptyenv())
  function(name = "gaulss") {
    if (exists(name, cache, inherits = FALSE)) return(cache[[name]])
    m <- withr::with_seed(281, {
      n <- 400L
      d <- data.frame(x = runif(n), o1 = runif(n, -.1, .1),
        o2 = runif(n, -.1, .1), o3 = runif(n, -.05, .05),
        o4 = runif(n, -.05, .05), w = rep(c(1, 2), n / 2))
      mu <- .4 + .3 * d$x + d$o1
      d$y <- switch(name,
        gaulss = rnorm(n, mu, .8),
        gammals = rgamma(n, 4, scale = exp(mu) / 4),
        gumbls = mu - .7 * log(-log(runif(n))),
        gevlss = mu + .7 * ((-log(runif(n)))^(-.1) - 1) / .1,
        shash = mu + 1.2 * .7 * sinh((asinh(rnorm(n)) + .4) / 1.2),
        twlss = mgcv::rTweedie(exp(mu), p = 1.5, phi = .6),
        ziplss = {
          u <- runif(n)
          lambda <- exp(mu)
          ifelse(runif(n) < .7, qpois(exp(-lambda) + u * (-expm1(-lambda)), lambda), 0)
        })
      family <- get(name, asNamespace("mgcv"))()
      formulas <- list(y ~ x + offset(o1), ~x + offset(o2),
        ~x + offset(o3), ~x + offset(o4))[seq_len(family$nlp)]
      if (name == "shash") {
        formulas <- list(y ~ x, ~x, ~x, ~x)
        d[c("o1", "o2", "o3", "o4")] <- 0
      }
      # mgcv can try points outside GEV support during optimisation.
      fit <- withCallingHandlers(
        mgcv::gam(formulas, data = d, weights = w, family = family, method = "REML"),
        warning = function(w) {
          if (name == "gevlss" && conditionMessage(w) == "NaNs produced")
            invokeRestart("muffleWarning")
        })
      attr(fit, "lss_test_data") <- d
      fit
    })
    cache[[name]] <- m
    m
  }
})

lss_test_beta <- function(name) {
  switch(name,
    gaulss = c(.4, .3, -.4, .2),
    gammals = c(.4, .3, -1, .2),
    gumbls = c(.4, .3, -1, .2),
    gevlss = c(.4, .3, -1, .2, -.5, .1),
    shash = c(.4, .3, -1, .2, .4, .1, .1, .1),
    twlss = c(.4, .3, .1, .2, -.4, .1),
    ziplss = c(.4, .3, -.1, .2))
}

lss_test_eta <- function(beta, d) {
  do.call(cbind, lapply(seq_len(length(beta) / 2), function(j) {
    beta[2 * j - 1] + beta[2 * j] * d$x + d[[paste0("o", j)]]
  }))
}

lss_test_mean <- function(name, eta) {
  switch(name,
    gaulss = eta[, 1], gammals = exp(eta[, 1]), twlss = exp(eta[, 1]),
    gumbls = eta[, 1] - digamma(1) * (exp(eta[, 2]) + exp(-7)),
    gevlss = {
      xi <- -1 + 1.5 * plogis(eta[, 3])
      eta[, 1] + exp(eta[, 2]) * (gamma(1 - xi) - 1) / xi
    },
    shash = vapply(seq_len(nrow(eta)), function(i) {
      sigma <- exp(eta[i, 2]) + .01
      delta <- exp(eta[i, 4])
      integrate(function(z) (eta[i, 1] + delta * sigma *
          sinh((asinh(z) + eta[i, 3]) / delta)) * dnorm(z), -10, 10,
        rel.tol = 1e-10)$value
    }, numeric(1)),
    ziplss = (-expm1(-exp(eta[, 2]))) * exp(eta[, 1]) / (-expm1(-exp(eta[, 1]))))
}

test_that("every installed LSS family predicts its expectation from joint draws", {
  for (name in c("gaulss", "gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")) {
    m <- lss_test_models(name)
    d <- attr(m, "lss_test_data")[1:3, , drop = FALSE]
    # Genuinely new covariates, without observed responses or fitting weights.
    nd <- data.frame(x = c(.12, .55, 1.1), o1 = .03, o2 = -.04, o3 = .02, o4 = -.01)
    if (name == "shash") nd[c("o1", "o2", "o3", "o4")] <- 0
    beta <- lss_test_beta(name)
    b <- rbind(beta, beta + .01)
    for (data in list(d, nd)) {
      expect_silent(f <- fitted_samples(m, data = data, method = "user", draws = b))
      expected <- unlist(lapply(1:2, function(i) lss_test_mean(name, lss_test_eta(b[i, ], data))))
      expect_equal(f$.fitted, expected, tolerance = 1e-8, info = name)
      link <- fitted_samples(m, data = data, method = "user", draws = b, scale = "linear_predictor")
      expect_equal(link$.fitted, unlist(lapply(1:2, function(i) lss_test_eta(b[i, ], data)[, 1])), info = name)
      expect_silent(p <- posterior_samples(m, data = data, method = "user", draws = b, seed = 281))
      expect_true(all(is.finite(p$.response)), info = name)
      expect_identical(p$.row, rep(1:3, 2))
      expect_identical(p$.draw, rep(1:2, each = 3))
      expect_identical(p, posterior_samples(m, data = data, method = "user", draws = b, seed = 281))
    }
    # Fitting observations retain their full offsets and prior weights.
    f <- fitted_samples(m, method = "user", draws = b)
    expect_equal(f$.fitted[1:nrow(m$model)],
      lss_test_mean(name, lss_test_eta(beta, attr(m, "lss_test_data"))), tolerance = 1e-8, info = name)
    expect_silent(posterior_samples(m, method = "user", draws = b, seed = 281))
    expect_silent(predicted_samples(m, data = nd, n_draws = 2, seed = 281))
  }
})

test_that("gaulss responses use reciprocal SD, weights and offsets in both predictors", {
  m <- lss_test_models()
  d <- data.frame(x = c(.2, .8), o1 = c(.1, -.2), o2 = c(.3, -.1))
  b <- rbind(lss_test_beta("gaulss"), lss_test_beta("gaulss") + .03)
  withr::local_seed(12)
  before <- .Random.seed
  p <- posterior_samples(m, data = d, method = "user", draws = b, weights = c(2, 5), seed = 9)
  expect_identical(.Random.seed, before)
  expected <- withr::with_seed(9, unlist(lapply(1:2, function(i) {
    eta <- lss_test_eta(b[i, ], d)
    rnorm(2, eta[, 1], (exp(eta[, 2]) + .01) / sqrt(c(2, 5)))
  })))
  expect_equal(p$.response, expected, tolerance = 1e-12)
  expected_training <- withr::with_seed(9, unlist(lapply(1:2, function(i) {
    eta <- lss_test_eta(b[i, ], attr(m, "lss_test_data"))
    rnorm(nrow(m$model), eta[, 1], exp(eta[, 2]) + .01)
  })))
  expect_equal(posterior_samples(m, method = "user", draws = b, seed = 9)$.response,
    expected_training, tolerance = 1e-12)
  explicit_training <- posterior_samples(m, method = "user", draws = b,
    seed = 9, weights = m$prior.weights)$.response
  training_mean <- unlist(lapply(1:2, function(i)
    lss_test_eta(b[i, ], attr(m, "lss_test_data"))[, 1]))
  expect_equal(explicit_training - training_mean,
    (expected_training - training_mean) / sqrt(rep(m$prior.weights, 2)), tolerance = 1e-12)
  expect_equal(posterior_samples(m, data = d, method = "user", draws = b, weights = 2, seed = 9)$.response,
    posterior_samples(m, data = d, method = "user", draws = b, weights = rep(2, 2), seed = 9)$.response)
  expect_error(posterior_samples(m, data = d, method = "user", draws = b, weights = 1:3), "one value per prediction row")
})

test_that("LSS response generators have the expected analytic moments", {
  for (name in c("gaulss", "gammals", "gumbls", "shash", "twlss", "ziplss")) {
    m <- lss_test_models(name)
    d <- data.frame(x = rep(.3, 20000), o1 = 0, o2 = 0, o3 = 0, o4 = 0)
    beta <- lss_test_beta(name)
    p <- posterior_samples(m, data = d, method = "user", draws = matrix(beta, 1), seed = 17)$.response
    mean <- lss_test_mean(name, lss_test_eta(beta, d[1, ]))
    expect_true(abs(base::mean(p) - mean) < 6 * sd(p) / sqrt(length(p)), info = name)
    if (name == "gaulss") expect_equal(var(p), (exp(beta[3] + .3 * beta[4]) + .01)^2, tolerance = .03)
    if (name == "gammals") expect_true(all(p > 0))
    if (name == "ziplss") {
      expect_equal(base::mean(p == 0), exp(-exp(beta[3] + .3 * beta[4])), tolerance = .015)
      expect_true(all(p == floor(p)))
    }
  }
})

test_that("Gumbel, GEV, shash and hurdle derivatives include every parameter", {
  for (name in c("gaulss", "gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")) {
    m <- lss_test_models(name)
    b <- rbind(lss_test_beta(name), lss_test_beta(name) + .04)
    nd <- data.frame(x = c(.2, .7), o1 = .02, o2 = -.03, o3 = .01, o4 = 0)
    if (name == "shash") nd[c("o1", "o2", "o3", "o4")] <- 0
    eps <- 1e-5
    actual <- derivative_samples(m, focal = "x", data = nd, type = "central",
      method = "user", draws = b, eps = eps)
    upper <- lower <- nd
    upper$x <- upper$x + eps
    lower$x <- lower$x - eps
    expected <- unlist(lapply(1:2, function(i) {
      (lss_test_mean(name, lss_test_eta(b[i, ], upper)) -
        lss_test_mean(name, lss_test_eta(b[i, ], lower))) / (2 * eps)
    }))
    key <- order(actual$.draw, actual$.row)
    expect_equal(actual$.derivative[key], expected, tolerance = 1e-5, info = name)
    expect_silent(derivative_samples(m, focal = "x", n_grid = 4, method = "user", draws = b))
  }
})

test_that("LSS missing rows, chains, supplied draws and add helpers retain identities", {
  skip_if_not_installed("posterior")
  m <- lss_test_models()
  nd <- data.frame(x = c(.2, NA, .7, .9), o1 = 0, o2 = 0)
  b <- post_draws(m, n_draws = 40, chains = 4, seed = 14, diagnostics = TRUE)
  for (fun in list(fitted_samples, posterior_samples)) {
    out <- fun(m, data = nd, method = "user", draws = b, seed = 9)
    expect_identical(sort(unique(out$.draw)), 1:40)
    expect_identical(sort(unique(out$.chain)), 1:4)
    expect_identical(sort(unique(out$.iteration)), 1:10)
    expect_identical(sampling_diagnostics(out), sampling_diagnostics(b))
    value <- intersect(c(".fitted", ".response"), names(out))
    expect_true(all(is.na(out[[value]][out$.row == 2])))
    expect_true(all(is.finite(out[[value]][out$.row != 2])))
    expect_identical(dim(as_mcmc_draws(out[out$.row == 1, ], "array"))[1:2], c(10L, 4L))
    omitted <- fun(m, data = nd, method = "user", draws = b, seed = 9, na.action = na.omit)
    expect_identical(sort(unique(omitted$.row)), c(1L, 3L, 4L))
    absent <- fun(m, data = nd[2, , drop = FALSE], method = "user", draws = b, seed = 9)
    expect_true(all(is.na(absent[[value]])))
    expect_error(fun(m, data = nd, method = "user", draws = b, n_draws = 39), "n_draws.*agree")
    generated <- fun(m, data = nd, n_draws = 8, chains = 2, seed = 9)
    expect_identical(sort(unique(generated$.chain)), 1:2)
  }
  for (fun in list(add_fitted_samples, add_posterior_samples)) {
    out <- fun(nd, m, method = "user", draws = b, seed = 9)
    expect_identical(nrow(out), 160L)
    expect_identical(out$x, rep(nd$x, each = 40))
    expect_identical(sampling_diagnostics(out), sampling_diagnostics(b))
  }
  p <- predicted_samples(m, data = nd, n_draws = 2, seed = 9, na.action = na.omit)
  expect_identical(sort(unique(p$.row)), c(1L, 3L, 4L))
  a <- add_predicted_samples(nd, m, n_draws = 2, seed = 9, na.action = na.omit)
  expect_true(all(is.na(a$.response[a$.row == 2])))
  derivative <- derivative_samples(m, focal = "x", data = nd, method = "user", draws = b)
  expect_true(all(is.na(derivative$.derivative[derivative$.row == 2])))
  expect_identical(sampling_diagnostics(derivative), sampling_diagnostics(b))
})

test_that("fitting-call offsets and excluded fitting rows are retained for LSS", {
  withr::local_seed(7)
  d <- data.frame(x = runif(120), z = runif(120), off = runif(120), y = rnorm(120))
  d$x[3] <- NA
  m <- mgcv::gam(list(y ~ x, ~z), offset = d$off,
    family = mgcv::gaulss(), data = d, na.action = na.exclude)
  # mgcv 1.9-4 drops call offsets for general.family fits. Represent stored
  # fitting offsets explicitly to exercise recovery independently of that fitter.
  m$linear.predictors[, 1] <- m$linear.predictors[, 1] + d$off[-3]
  m$linear.predictors[, 2] <- m$linear.predictors[, 2] - d$off[-3]
  m$offset <- list(d$off, -d$off)
  b <- matrix(coef(m), 1)
  f <- fitted_samples(m, method = "user", draws = b)
  expected <- as.numeric(coef(m)[1] + coef(m)[2] * d$x + d$off)
  expect_equal(f$.fitted, expected, tolerance = 1e-12)
  p <- posterior_samples(m, method = "user", draws = b, seed = 9)
  expect_true(is.na(p$.response[3]))
  expect_identical(p$.row, 1:120)
  compact <- model_used_rows(m)
  eta <- compact$linear.predictors
  ref <- withr::with_seed(9, rnorm(119, eta[, 1], exp(eta[, 2]) + .01))
  expect_equal(p$.response[-3], ref, tolerance = 1e-12)
  # Supplied data follows predict.gam(): fitting-call offsets are not transferred.
  new <- fitted_samples(m, data = d[1:2, c("x", "z")], method = "user", draws = b)
  expect_equal(new$.fitted, coef(m)[1] + coef(m)[2] * d$x[1:2], ignore_attr = TRUE)
})

test_that("LSS smooth draws use the selected predictor's coefficient block", {
  m <- m_gaulss
  nd <- m$model[1:4, , drop = FALSE]
  b <- post_draws(m, n_draws = 8, chains = 2, seed = 9)
  for (i in seq_along(m$smooth)) {
    sm <- m$smooth[[i]]
    out <- smooth_samples(m, data = nd, select = sm$label, method = "user", draws = b)
    reference <- mgcv::PredictMat(sm, nd) %*% t(b[, sm$first.para:sm$last.para, drop = FALSE])
    key <- order(out$.draw, out$.row)
    expect_equal(out$.value[key], as.numeric(reference))
    expect_identical(sort(unique(out$.chain)), 1:2)
    joined <- add_smooth_samples(nd, m, select = sm$label, method = "user", draws = b)
    expect_equal(joined$.value[order(joined$.draw, joined$.row)], as.numeric(reference))
  }
})

test_that("GEV expectations fail precisely when shape has no finite mean", {
  m <- lss_test_models("gevlss")
  m$family <- mgcv::gevlss(link = c("identity", "identity", "identity"))
  b <- matrix(c(.4, .3, -1, .2, 1.1, .1), 1)
  nd <- data.frame(x = .3, o1 = 0, o2 = 0, o3 = 0)
  expect_error(fitted_samples(m, data = nd, method = "user", draws = b), "expected response does not exist.*shape >= 1")
  expect_error(derivative_samples(m, focal = "x", data = nd, method = "user", draws = b), "expected response does not exist")
  expect_silent(fitted_samples(m, data = nd, method = "user", draws = b, scale = "linear_predictor"))
  expect_true(is.finite(posterior_samples(m, data = nd, method = "user", draws = b, seed = 1)$.response))
})

test_that("LSS generated draws and selected coefficients retain joint sampling", {
  m <- lss_test_models()
  nd <- data.frame(x = c(.2, .7), o1 = .1, o2 = -.1)
  for (method in c("gaussian", "mh")) {
    args <- list(model = m, n_draws = 8, chains = 2, seed = 9, method = method)
    if (method == "mh") args <- c(args, list(mh_backend = "family", burnin = 3))
    b <- do.call(post_draws, args)
    actual <- do.call(fitted_samples, c(args, list(data = nd)))
    supplied <- fitted_samples(m, data = nd, method = "user", draws = b)
    expect_equal(actual$.fitted, supplied$.fitted)
    expect_identical(actual$.chain, supplied$.chain)
    selected <- do.call(post_draws, c(args, list(index = 3:4)))
    if (method == "mh") expect_equal(as.vector(selected), as.vector(b[, 3:4]))
    expect_identical(dim(selected), c(8L, 2L))
    expect_equal(as.vector(user_draws(m, draws = b, index = 3:4)), as.vector(b[, 3:4]))
    expect_identical(sampling_diagnostics(selected)$draws, sampling_diagnostics(b)$draws)
  }
})

test_that("shared LSS coefficient mappings agree with native joint predictions", {
  withr::local_seed(28)
  d <- data.frame(x = runif(160), z = runif(160))
  d$y <- rnorm(160, sin(d$x), exp(d$x / 4))
  m <- mgcv::gam(list(y ~ z, ~1, 1 + 2 ~ s(x, k = 4) - 1),
    family = mgcv::gaulss(), data = d, method = "REML")
  b <- rbind(coef(m), coef(m) + seq_along(coef(m)) / 100)
  nd <- data.frame(x = c(.2, .8), z = c(.1, .9))
  result <- posterior_samples(m, data = nd, method = "user", draws = b, seed = 17)
  reference <- withr::with_seed(17, unlist(lapply(1:2, function(i) {
    copy <- m
    copy$coefficients <- b[i, ]
    parameters <- predict(copy, newdata = nd, type = "response")
    rnorm(2, parameters[, 1], 1 / parameters[, 2])
  })))
  expect_equal(result$.response, reference, tolerance = 1e-12)
})

test_that("LSS response preparation preserves caller-supplied family hooks", {
  m <- lss_test_models("gumbls")
  m$family$predict <- function(family, eta, ...) {
    # This hook supplies generator parameters directly, including the mean.
    list(cbind(eta[, 1] + 100, eta[, 2]))
  }
  m$family$rd <- function(mu, wt, scale) mu[, 1]
  b <- matrix(lss_test_beta("gumbls"), 1)
  nd <- data.frame(x = c(.1, .9), o1 = 0, o2 = 0)
  result <- posterior_samples(m, data = nd, method = "user", draws = b)
  expect_equal(result$.response, .4 + .3 * nd$x + 100)
})

test_that("censored draws are latent and survival sampling reports the missing process", {
  withr::local_seed(18)
  d <- data.frame(x = runif(160))
  y <- rnorm(160)
  d$y <- cbind(pmax(y, 0), ifelse(y < 0, -Inf, y))
  m <- mgcv::gam(y ~ x, data = d, family = mgcv::cnorm())
  nd <- data.frame(x = seq(0, 1, length.out = 50))
  b <- matrix(c(0, 0), 1)
  result <- posterior_samples(m, data = nd, method = "user", draws = b, seed = 9)
  expect_true(all(is.finite(result$.response)))
  expect_true(any(result$.response < 0))
  expect_error(diagnostic_posterior_sampler(m, n = 2), "censoring observation process")
  # A Cox fit has no scalar expected-response or response-generator adapter.
  d$t <- rexp(160)
  cox <- mgcv::gam(t ~ x, family = mgcv::cox.ph(), data = d,
    weights = rep(1, 160))
  expect_error(posterior_samples(cox, data = nd, n_draws = 2), "observation process.*response generator")
  expect_error(fitted_samples(cox, data = nd, n_draws = 2), "Cox survival predictions require evaluation times")
})

test_that("Cox predictor draws remain available without implying a response mean", {
  withr::local_seed(18)
  d <- data.frame(x = runif(100), t = rexp(100))
  m <- mgcv::gam(t ~ x, family = mgcv::cox.ph(), data = d, weights = rep(1, 100))
  b <- matrix(coef(m), 1)
  nd <- data.frame(x = c(.2, .7))
  out <- fitted_samples(m, data = nd, method = "user", draws = b, scale = "linear_predictor")
  expect_equal(out$.fitted, as.numeric(predict(m, newdata = nd, type = "link")))
})

test_that("LSS prediction retains fitted non-default links and bounds", {
  withr::local_seed(28)
  d <- data.frame(x = runif(160), off = runif(160, -.2, .2))
  d$y <- rnorm(160, exp(.4 + .3 * d$x), .7)
  m <- mgcv::gam(list(y ~ x, ~x + offset(off)), data = d,
    family = mgcv::gaulss(link = c("log", "logb"), b = .2))
  b <- matrix(c(.4, .3, -.4, .2), 1)
  nd <- data.frame(x = c(.2, .8), off = c(.1, -.1))
  mean <- exp(.4 + .3 * nd$x)
  expected <- withr::with_seed(9, rnorm(2, mean, exp(-.4 + .2 * nd$x + nd$off) + .2))
  expect_equal(fitted_samples(m, data = nd, method = "user", draws = b)$.fitted, mean)
  expect_equal(posterior_samples(m, data = nd, method = "user", draws = b, seed = 9)$.response, expected)
})

test_that("bounded prediction batches preserve joint LSS draw positions", {
  m <- lss_test_models("gumbls")
  nd <- data.frame(x = seq(.1, .9, length.out = 200), o1 = .03, o2 = -.02)
  b <- outer(seq(-.02, .02, length.out = 1001), rep(1, 4)) +
    matrix(lss_test_beta("gumbls"), 1001, 4, byrow = TRUE)
  out <- fitted_samples(m, data = nd, method = "user", draws = b)
  expected <- as.numeric(vapply(seq_len(nrow(b)), function(i) {
    lss_test_mean("gumbls", lss_test_eta(b[i, ], nd))
  }, numeric(nrow(nd))))
  expect_equal(out$.fitted, expected, tolerance = 1e-12)
  expect_identical(out$.draw, rep(seq_len(nrow(b)), each = nrow(nd)))
  expect_identical(out$.row, rep(seq_len(nrow(nd)), nrow(b)))
})

test_that("missing explicit weights suppress LSS responses consistently", {
  nd <- data.frame(x = c(.2, .5, .8), o1 = .03, o2 = -.02, o3 = 0, o4 = 0)
  for (name in c("gaulss", "gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")) {
    m <- lss_test_models(name)
    b <- matrix(lss_test_beta(name), 1)
    actual <- posterior_samples(m, data = nd, method = "user", draws = b,
      weights = c(1, NA_real_, 1), seed = 29)
    expected <- posterior_samples(m, data = nd[c(1, 3), ], method = "user",
      draws = b, weights = 1, seed = 29)
    expect_true(is.na(actual$.response[2]), info = name)
    expect_equal(actual$.response[c(1, 3)], expected$.response, info = name)
    expect_identical(actual$.row, 1:3)
    absent <- posterior_samples(m, data = nd, method = "user", draws = b,
      weights = rep(NA_real_, 3), seed = 29)
    expect_true(all(is.na(absent$.response)), info = name)
  }
})

test_that("response assembly preserves the original generator calls and RNG state", {
  # Reconstruct the earlier per-draw calculation, including row restoration,
  # without using the optimised design cache or response table assembly.
  legacy <- function(context, b, weights) {
    rd <- missing_safe_rd(fix_family_rd(context$family)$rd)
    wt <- lss_sampling_weights(context, weights)
    dplyr::bind_rows(lapply(seq_len(NROW(b)), function(i) {
      response <- if (all(is.na(context$layout$map))) rep(NA_real_, NROW(context$X)) else {
        mu <- family_prediction_parameters(context$family, lss_predictors(context, b[i, ]))
        rd(mu = mu, wt = wt, scale = context$model$sig2 %||% 1)
      }
      restore_prediction_table(tibble::tibble(.row = seq_len(NROW(context$X)),
        .draw = i, .parameter = "response", .response = as.numeric(response)), context$layout)
    }))
  }
  for (name in c("gaulss", "gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")) {
    m <- lss_test_models(name)
    b <- post_draws(m, n_draws = 12, chains = 3, seed = 91)
    nd <- data.frame(x = c(.2, NA, .7), o1 = .03, o2 = -.02, o3 = 0, o4 = 0)
    for (data in list(NULL, nd, nd[2, , drop = FALSE])) {
      context <- lss_prediction_context(m, data)
      weights <- if (is.null(data)) NULL else rep(c(2, NA_real_, 1), length.out = NROW(data))
      withr::local_seed(93)
      expected <- legacy(context, b, weights)
      state <- .Random.seed
      set.seed(93)
      actual <- lss_response_draws(context, b, weights)
      expect_identical(actual, expected, info = name)
      expect_identical(.Random.seed, state, info = name)
      set.seed(93)
      streamed <- dplyr::bind_rows(lapply(seq_len(NROW(b)), lss_response_sampler(context, b, weights)))
      expect_identical(streamed, expected, info = name)
      expect_identical(.Random.seed, state, info = name)
    }
    # Omitting invalid rows keeps their original row numbers and generator order.
    context <- lss_prediction_context(m, nd, na.action = na.omit)
    expect_identical(withr::with_seed(93, lss_response_draws(context, b)),
      withr::with_seed(93, legacy(context, b, NULL)), info = name)
  }
})


test_that("LSS response assembly rejects malformed family generator output", {
  m <- lss_test_models()
  b <- matrix(lss_test_beta("gaulss"), 1L)
  nd <- data.frame(x = c(.2, .5, .8), o1 = .03, o2 = -.02)
  bad_generators <- list(function(mu, wt, scale) numeric(),
    function(mu, wt, scale) seq_len(2 * NROW(mu)),
    function(mu, wt, scale) matrix(1, NROW(mu), 1),
    function(mu, wt, scale) rep("response", NROW(mu)))
  for (generator in bad_generators) {
    m$family$rd <- generator
    for (weights in list(rep(1, 3), c(1, NA_real_, 1))) {
      expect_error(posterior_samples(m, data = nd, method = "user", draws = b,
        weights = weights), "must return one numeric response per observation")
      context <- lss_prediction_context(m, nd)
      sampler <- lss_response_sampler(context, b, weights)
      expect_error(sampler(1L), "must return one numeric response per observation")
    }
  }
})
