test_that("parameter descriptions distinguish natural and native representations", {
  labels <- list(gaulss = c("mean", "sd"), gammals = c("mean", "dispersion"),
    gumbls = c("location", "scale"), gevlss = c("location", "scale", "shape"),
    shash = c("location", "scale", "skewness", "tailweight"),
    ziplss = c("rate", "presence_probability"), twlss = c("mean", "power", "dispersion"))
  for (name in names(labels)) {
    f <- get(name, asNamespace("mgcv"))()
    info <- family_parameters(f)
    expect_identical(info$parameter, labels[[name]])
    expect_identical(info$predictor, seq_along(labels[[name]]))
    expect_true(all(info$returned))
    expect_identical(attr(info, "package"), "mgcv")
  }
  info <- family_parameters(mgcv::nb())
  expect_identical(info$parameter, c("mean", "shape"))
  expect_identical(info$source, c("predictor", "global"))
  expect_false(info$returned[2L])
  expect_match(family_parameters(mgcv::gaulss())$native[2L], "reciprocal")
  expect_match(family_parameters(mgcv::shash())$definition[4L], "not response kurtosis")
})

test_that("every LSS family returns natural parameters from joint draws", {
  natural <- function(name, eta) {
    p <- eta
    if (name == "gaulss") p[, 2] <- .01 + exp(eta[, 2])
    if (name == "gammals") {
      p[, 1] <- exp(eta[, 1]); p[, 2] <- exp(-7 + log1p(exp(eta[, 2] + 7)))
    }
    if (name == "gumbls") p[, 2] <- exp(-7 + log1p(exp(eta[, 2] + 7)))
    if (name == "gevlss") {
      p[, 2] <- exp(eta[, 2]); p[, 3] <- -1 + 1.5 * plogis(eta[, 3])
    }
    if (name == "shash") {
      p[, 2] <- .01 + exp(eta[, 2]); p[, 4] <- exp(eta[, 4])
    }
    if (name == "ziplss") {
      p[, 1] <- exp(eta[, 1]); p[, 2] <- -expm1(-exp(eta[, 2]))
    }
    if (name == "twlss") {
      p[, 1] <- exp(eta[, 1]); p[, 2] <- 1.01 + .98 * plogis(eta[, 2])
      p[, 3] <- exp(eta[, 3])
    }
    p
  }
  for (name in c("gaulss", "gammals", "gumbls", "gevlss", "shash", "ziplss", "twlss")) {
    model <- lss_test_models(name)
    nd <- data.frame(x = c(.2, .8), o1 = .03, o2 = -.04, o3 = .02, o4 = -.01)
    if (name == "shash") nd[c("o1", "o2", "o3", "o4")] <- 0
    B <- rbind(lss_test_beta(name), lss_test_beta(name) + .02)
    draws <- fitted_samples(model, data = nd, method = "user", draws = B)
    expect_identical(unique(draws$.parameter), family_parameters(model)$parameter)
    for (i in 1:2) {
      eta <- lss_test_eta(B[i, ], nd)
      expect_equal(draws$.fitted[draws$.draw == i], as.numeric(natural(name, eta)), info = name)
    }
    for (scale in c("response", "link", "linear_predictor", "linear predictor")) {
      a <- fitted_samples(model, data = nd, method = "user",
        draws = matrix(coef(model), 1L), scale = scale)
      v <- fitted_values(model, data = nd, scale = scale)
      expect_equal(a$.fitted, v$.fitted, tolerance = 1e-10, info = name)
      expect_identical(a$.parameter, v$.parameter)
    }
    expected <- fitted_samples(model, data = nd, method = "user", draws = B,
      quantity = "expectation")
    expect_identical(unique(expected$.parameter), "expectation")
    for (i in 1:2) expect_equal(expected$.fitted[expected$.draw == i],
      lss_test_mean(name, lss_test_eta(B[i, ], nd)), tolerance = 1e-8, info = name)
    v <- fitted_values(model, data = nd, quantity = "expectation")
    a <- fitted_samples(model, data = nd, quantity = "expectation", method = "user",
      draws = matrix(coef(model), 1L))
    expect_equal(a$.fitted, v$.fitted, tolerance = 1e-10, info = name)
    expect_identical(attr(v, "prediction_info")$se_scale, "response")
  }
})

test_that("expectations reject link scales and retain full joint uncertainty", {
  model <- lss_test_models("ziplss")
  nd <- data.frame(x = c(.2, .8), o1 = 0, o2 = 0)
  for (fun in list(fitted_values, fitted_samples)) for (scale in c("link", "linear_predictor", "linear predictor")) {
    args <- list(model, data = nd, quantity = "expectation", scale = scale)
    if (identical(fun, fitted_samples)) args$n_draws <- 2
    expect_error(do.call(fun, args), "only on the response scale.*quantity = 'parameters'")
  }
  value <- fitted_values(model, data = nd, quantity = "expectation")
  X <- predict(model, nd, type = "lpmatrix")
  evaluate <- function(beta) {
    eta <- cbind(X[, 1:2] %*% beta[1:2], X[, 3:4] %*% beta[3:4])
    rate <- exp(eta[, 1]); presence <- -expm1(-exp(eta[, 2]))
    presence * rate / (-expm1(-rate))
  }
  h <- 1e-5
  J <- vapply(seq_along(coef(model)), function(j) {
    hi <- lo <- coef(model); hi[j] <- hi[j] + h; lo[j] <- lo[j] - h
    (evaluate(hi) - evaluate(lo)) / (2 * h)
  }, numeric(nrow(nd)))
  expected <- sqrt(rowSums((J %*% vcov(model)) * J))
  expect_equal(value$.se, unname(expected), tolerance = 1e-7)
  expect_identical(attr(value, "prediction_info")$quantity, "expectation")
})

test_that("GEV parameter and response draws do not require a finite expectation", {
  model <- lss_test_models("gevlss")
  # A modified shape link allows values above the finite-mean boundary.
  model$family$linfo[[3]]$linkinv <- identity
  model$family$predict <- NULL
  B <- matrix(lss_test_beta("gevlss"), 1L); B[1, 5:6] <- c(1.2, 0)
  nd <- data.frame(x = c(.2, .8), o1 = 0, o2 = 0, o3 = 0)
  expect_silent(fitted_samples(model, data = nd, method = "user", draws = B))
  expect_silent(posterior_samples(model, data = nd, method = "user", draws = B, seed = 1))
  expect_error(fitted_samples(model, data = nd, method = "user", draws = B,
    quantity = "expectation"), "expected response does not exist")
})

test_that("simultaneous intervals respect fixed finite-mean conditions", {
  m <- m_tiny_eg1
  m$family <- mgcv::scat(theta = c(.8, 1), min.df = .1)
  for (interval in c("confidence", "simultaneous")) {
    expect_error(fitted_values(m, data = m$model[1:2, ],
      quantity = "expectation", interval = interval, n_sim = 20, seed = 426),
      "expected response does not exist")
  }
  m <- conditional_difference_fixture()$model
  m$family <- mgcv::scat(theta = c(.8, 1), min.df = .1)
  for (uncertainty in c("delta", "simulation")) {
    expect_error(conditional_differences(m, by = "a", condition = "x",
      uncertainty = uncertainty, n_vals = 2, n_sim = 20),
      "expected response does not exist")
  }
})

test_that("empty and missing rows retain parameter and category identities", {
  model <- lss_test_models("gaulss")
  B <- matrix(coef(model), 1L)
  nd <- data.frame(x = c(.2, NA_real_), o1 = 0, o2 = 0)
  for (q in c("parameters", "expectation")) {
    v <- fitted_values(model, data = nd, quantity = q)
    a <- fitted_samples(model, data = nd, method = "user", draws = B, quantity = q)
    expect_identical(v$.row, a$.row)
    expect_identical(v$.parameter, a$.parameter)
    expect_equal(v$.fitted, a$.fitted)
    expect_true(all(is.na(v$.fitted[v$.row == 2L])))
    for (data in list(nd[2L, , drop = FALSE], nd[FALSE, ])) {
      v <- fitted_values(model, data = data, quantity = q)
      a <- fitted_samples(model, data = data, method = "user", draws = B, quantity = q)
      expect_equal(v$.fitted, a$.fitted)
      expect_true(all(is.na(v$.fitted)))
      expect_true(all(c(".row", ".parameter", ".fitted") %in% names(v)))
    }
  }
})

test_that("category and response identity is separate from parameter meaning", {
  for (model in list(m_ocat, m_multinom, m_mvn)) {
    nd <- model$model[1:3, , drop = FALSE]
    B <- matrix(coef(model), 1L)
    for (q in c("parameters", "expectation")) {
      v <- fitted_values(model, data = nd, quantity = q)
      a <- fitted_samples(model, data = nd, method = "user", draws = B, quantity = q)
      expect_equal(a$.fitted, v$.fitted, tolerance = 1e-9)
      expect_identical(a$.parameter, v$.parameter)
      for (key in intersect(c(".category", ".y"), names(v))) expect_identical(a[[key]], v[[key]])
    }
    if (family_type(model) != "multivariate_normal") {
      a <- fitted_samples(model, data = nd, method = "user", draws = B,
        quantity = "expectation")
      expect_equal(as.numeric(tapply(a$.fitted, a$.row, sum)), rep(1, 3L))
      expect_identical(unique(a$.parameter), "expectation")
    }
  }
})

test_that("family-specific bounds and native extractors are retained", {
  eta <- rbind(c(.4, -.5, .2, -.1), c(.6, -.3, .1, .2))
  families <- list(gaulss = mgcv::gaulss(b = .2),
    gammals = mgcv::gammals(b = -2), gumbls = mgcv::gumbls(b = -2),
    shash = mgcv::shash(b = .3))
  for (name in names(families)) {
    family <- families[[name]]
    predictor <- eta[, seq_len(family$nlp), drop = FALSE]
    expected <- predictor
    if (name == "gaulss") expected[, 2L] <- .2 + exp(predictor[, 2L])
    if (name == "gammals") expected[, 1L] <- exp(predictor[, 1L])
    if (name %in% c("gammals", "gumbls"))
      expected[, 2L] <- exp(-2 + log1p(exp(predictor[, 2L] + 2)))
    if (name == "shash") {
      expected[, 2L] <- .3 + exp(predictor[, 2L])
      expected[, 4L] <- exp(predictor[, 4L])
    }
    expect_equal(unname(distribution_spec(family)$to_parameters(predictor)),
      unname(expected), info = name)
  }
  m <- lss_test_models("twlss")
  m$family <- mgcv::twlss(a = 1.2, b = 1.8)
  nd <- data.frame(x = c(.2, .8), o1 = 0, o2 = 0, o3 = 0)
  B <- matrix(lss_test_beta("twlss"), 1L)
  a <- fitted_samples(m, data = nd, method = "user", draws = B)
  eta <- lss_test_eta(as.numeric(B), nd)
  expect_equal(a$.fitted[a$.parameter == "power"], 1.2 + .6 * plogis(eta[, 2L]))
  expect_equal(a$.fitted[a$.parameter == "dispersion"], exp(eta[, 3L]))
  expect_identical(inv_link(m, parameter = "power"), m$family$linfo[[2L]]$linkinv)
  expect_identical(inv_link(m, parameter = "scale"), m$family$linfo[[3L]]$linkinv)
  expect_match(attr(family_parameters(m), "parameterisation"), "Var")
  expect_identical(family_parameters(m_gamm), family_parameters(m_gamm$gam))
  expect_identical(family_parameters(m_scam)$parameter[1L], "mean")
})

test_that("grouped fitted estimates and draws use explicit component membership", {
  model <- gfam_model
  nd <- gfam_data[c(3, 1, 5), ]
  B <- matrix(coef(model), 1L)
  for (q in c("parameters", "expectation")) {
    v <- fitted_values(model, data = nd, quantity = q)
    a <- fitted_samples(model, data = nd, method = "user", draws = B, quantity = q)
    expect_equal(v$.fitted, a$.fitted)
    expect_identical(v$.parameter, a$.parameter)
    expect_identical(v$.family, nd$family_id)
  }
  nd$x[2L] <- NA_real_
  v <- fitted_values(model, data = nd, na.action = na.omit)
  a <- fitted_samples(model, data = nd, method = "user", draws = B, na.action = na.omit)
  expect_identical(a$.family, nd$family_id[c(1, 3)])
  expect_identical(v$.family, a$.family)
  expect_equal(v$.fitted, a$.fitted)
  expect_identical(v$.parameter, a$.parameter)
})

test_that("category expectation derivatives retain category identifiers", {
  withr::local_seed(426)
  d <- data.frame(x = runif(180, -1, 1), y = sample(0:2, 180, replace = TRUE))
  m <- mgcv::gam(list(y ~ x, ~x), data = d, family = mgcv::multinom(K = 2))
  B <- matrix(c(.2, .3, -.1, .5), 1L)
  nd <- data.frame(x = c(-.4, .6))
  eta <- cbind(0, .2 + .3 * nd$x, -.1 + .5 * nd$x)
  probability <- exp(eta) / rowSums(exp(eta))
  expected <- probability * (rep(c(0, .3, .5), each = 2) - as.numeric(probability %*% c(0, .3, .5)))
  a <- derivative_samples(m, focal = "x", data = nd, method = "user", draws = B)
  expect_identical(sort(unique(a$.category)), paste0("category", 1:3))
  expect_equal(a$.derivative[order(a$.category, a$.row)], as.numeric(expected), tolerance = 1e-5)
})

test_that("model contexts can supply predictors without an mgcv model", {
  x <- c(-.5, .5)
  context <- list(spec = distribution_spec(mgcv::gaulss()), n_rows = 2L,
    predictors = function(beta) cbind(beta[1] + beta[2] * x, rep(beta[3], 2L)),
    layout = list(map = 1:2, rows = 1:2, data = data.frame(x = x)))
  B <- rbind(c(1, 2, -1), c(3, 4, -.5))
  a <- distribution_fitted_draws(context, B, "parameters", "response")
  expect_equal(a$.fitted, c(1 + 2 * x, rep(.01 + exp(-1), 2L),
    3 + 4 * x, rep(.01 + exp(-.5), 2L)))
  expect_identical(a$.draw, rep(1:2, each = 4L))
  e <- distribution_fitted_draws(context, B, "expectation", "response")
  expect_equal(e$.fitted, c(1 + 2 * x, 3 + 4 * x))
  context$spec$parameters$component <- c("cond", "disp")
  a <- distribution_fitted_draws(context, B, "parameters", "response")
  expect_identical(unique(a$.component), c("cond", "disp"))
  expect_false(".y" %in% names(a))
  expect_identical(dim(as_mcmc_draws(a, "array")), c(2L, 1L, 4L))
  context$layout$map <- c(NA_integer_, NA_integer_)
  context$predictors <- function(beta) stop("Missing rows must not be evaluated")
  expect_true(all(is.na(distribution_fitted_draws(context, B,
    "expectation", "response")$.fitted)))
})

test_that("grouped ordinal expectations use fitted thresholds and preserve missing rows", {
  m <- gfam_ordinal_model
  nd <- gfam_ordinal_data[c(1, 2, 4, 5), ]
  nd$x[3L] <- NA_real_
  B <- matrix(coef(m), 1L)
  a <- fitted_samples(m, data = nd, method = "user", draws = B,
    quantity = "expectation")
  v <- fitted_values(m, data = nd, quantity = "expectation")
  expect_equal(a$.fitted, v$.fitted)
  expect_identical(a$.category, v$.category)
  expect_true(all(is.na(a$.fitted[a$.row == 3L])))
  family <- gfam_components(family(m))[[1L]]$family
  cuts <- c(-Inf, family$getTheta(TRUE), Inf)
  eta <- as.numeric(gfam_predict_eta(m, nd[1L, ], type = "link"))
  probability <- diff(plogis(cuts - eta))
  expect_equal(a$.fitted[a$.row == 1L], probability)
  se_eta <- sqrt(as.numeric(predict(m, nd[1L, ], type = "lpmatrix") %*%
    vcov(m) %*% t(predict(m, nd[1L, ], type = "lpmatrix"))))
  slope <- dlogis(cuts[-length(cuts)] - eta) - dlogis(cuts[-1L] - eta)
  expect_equal(v$.se[v$.row == 1L], abs(slope) * se_eta, tolerance = 1e-7)
  info <- attr(a, "prediction_info")
  expect_true(info$capabilities$expectation)
  expect_length(info$expectation_condition, 2L)
  expect_identical(info$se_scale, NULL)
  for (data in list(nd[FALSE, ], nd[3L, , drop = FALSE])) {
    v <- fitted_values(m, data = data, quantity = "expectation")
    a <- fitted_samples(m, data = data, quantity = "expectation",
      method = "user", draws = B)
    expect_equal(v$.fitted, a$.fitted)
    expect_true(all(is.na(a$.fitted)))
    expect_identical(v$.category, a$.category)
  }
  data <- nd[1:2, ]
  h <- 1e-4
  plus <- minus <- data
  plus$x <- plus$x + h
  minus$x <- minus$x - h
  eta_slope <- as.numeric((gfam_test_eta(m, plus, B) -
    gfam_test_eta(m, minus, B)) / (2 * h))
  derivative <- derivative_samples(m, data = data, focal = "x",
    type = "central", eps = h, method = "user", draws = B)
  expect_equal(derivative$.derivative[derivative$.row == 1L],
    slope * eta_slope[1L], tolerance = 1e-7)
  expect_equal(derivative$.derivative[derivative$.row == 2L],
    exp(gfam_test_eta(m, data, B)[2L]) * eta_slope[2L], tolerance = 1e-7)
})
