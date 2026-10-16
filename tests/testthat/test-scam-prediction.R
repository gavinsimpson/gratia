skip_if_not_installed("scam", minimum_version = "1.2-22")

scam_prediction_models <- local({
  s <- mgcv::s
  d <- withr::with_seed(476, {
    d <- data.frame(x = runif(120, 0.05, 0.95), z = runif(120, 0.05, 0.95),
      o = runif(120, 0.1, 0.3))
    d$y <- 1 + exp(d$x) + d$z + d$o + rnorm(120, sd = 0.1)
    d
  })
  forms <- list(
    ordinary = y ~ z + s(x, bs = "mpi", k = 6),
    formula = y ~ z + offset(o) + s(x, bs = "mpi", k = 6),
    no_intercept = y ~ 0 + s(x, bs = "mpi", k = 6),
    no_intercept_formula = y ~ 0 + offset(o) + s(x, bs = "mpi", k = 6))
  models <- list()
  for (name in names(forms)) for (argument_offset in c(FALSE, TRUE)) {
    key <- paste(name, argument_offset)
    models[[key]] <- scam::scam(forms[[name]], data = d,
      offset = if (argument_offset) d$o else NULL)
  }
  models$lipl <- scam::scam(y ~ offset(o) +
    s(x, bs = "lipl", k = 12, xt = list(xc = 0.5)), data = d)
  models$two_lipl <- scam::scam(y ~ offset(o) +
    s(x, bs = "lipl", k = 12, xt = list(xc = 0.5)) +
    s(z, bs = "lipl", k = 12, xt = list(xc = 0.6)), data = d)
  list(models = models, data = d)
})

test_that("SCAM training predictions retain fitted offsets and original fitting coordinates", {
  for (model in scam_prediction_models$models) {
    original <- model[c("coefficients", "coefficients.t", "Vp", "Ve", "Vp.t", "Ve.t")]
    expect_equal(as.numeric(predict_model(model, type = "link")),
      as.numeric(model$linear.predictors), tolerance = 1e-8)
    expect_equal(as.numeric(predict_model(model, type = "response")),
      as.numeric(model$fitted.values), tolerance = 1e-8)
    fitted <- fitted_samples(model, method = "user",
      draws = matrix(coef(model), nrow = 1), scale = "linear_predictor")
    expect_equal(fitted$.fitted, as.numeric(model$linear.predictors), tolerance = 1e-8)
    X <- predict_model(model, type = "lpmatrix")
    expect_equal(as.numeric(attr(X, "model.offset")),
      as.numeric(model$offset %||% rep(0, nrow(X))))
    expect_identical(model[names(original)], original)
  }
})

test_that("SCAM formula offsets enter explicit one-row and multiple-row draws once", {
  nd <- scam_prediction_models$data
  for (model in scam_prediction_models$models) for (n in c(1L, 5L)) {
    data <- nd[seq_len(n), ]
    # Calculate the full function from separate parametric and smooth bases.
    X <- matrix(0, n, length(coef(model)))
    mf <- stats::model.frame(stats::delete.response(model$pterms), data,
      na.action = stats::na.pass)
    if (model$nsdf) X[, seq_len(model$nsdf)] <- stats::model.matrix(stats::delete.response(model$pterms), mf)
    for (sm in model$smooth) {
      X[, smooth_coef_indices(sm)] <- scam_smooth_matrix(sm, mgcv::PredictMat(sm, data))
    }
    offset <- stats::model.offset(mf) %||% rep(0, n)
    B <- rbind(coef(model), coef(model) * 1.1)
    expected <- X %*% t(B) + offset
    actual <- fitted_samples(model, data = data, method = "user", draws = B,
      scale = "linear_predictor")
    expect_equal(actual$.fitted, as.numeric(expected), tolerance = 1e-10)
    expect_equal(as.numeric(predict_model(model, newdata = data, type = "link")),
      expected[, 1], tolerance = 1e-10)
    actual_X <- predict_model(model, newdata = data, type = "lpmatrix")
    expect_equal(unname(actual_X), structure(X, model.offset = offset),
      ignore_attr = TRUE, tolerance = 1e-12)
    expect_equal(as.numeric(attr(actual_X, "model.offset")), as.numeric(offset))
  }
})

test_that("SCAM response draws retain formula offsets for Poisson and Gamma models", {
  s <- mgcv::s
  d <- scam_prediction_models$data
  for (name in c("poisson", "Gamma")) {
    fam <- if (name == "poisson") poisson() else Gamma(link = "log")
    d$y <- withr::with_seed(728, if (name == "poisson") {
      rpois(nrow(d), exp(d$x + d$o))
    } else rgamma(nrow(d), shape = 12, scale = exp(d$x + d$o) / 12))
    model <- scam::scam(y ~ offset(o) + s(x, bs = "mpi", k = 6), data = d,
      family = fam)
    data <- d[1:8, ]
    B <- matrix(rep(coef(model), 3), nrow = 3, byrow = TRUE)
    eta <- as.numeric(predict(model, data, type = "link"))
    expect_equal(fitted_samples(model, data = data, method = "user", draws = B)$.fitted,
      rep(fam$linkinv(eta), 3), tolerance = 1e-12)
    expected <- simulate(model, nsim = 3, data = data, seed = 728)
    actual <- posterior_samples(model, data = data, method = "user", draws = B, seed = 728)
    expect_equal(actual$.response, as.numeric(as.matrix(expected)), tolerance = 1e-12)
  }
})

test_that("SCAM prediction and draws restore excluded fitting and prediction rows", {
  s <- mgcv::s
  d <- scam_prediction_models$data
  d$x[c(2, 17)] <- NA_real_
  withr::local_options(na.action = "na.exclude")
  model <- scam::scam(y ~ offset(o) + s(x, bs = "mpi", k = 6), data = d)
  B <- matrix(coef(model), nrow = 1)
  actual <- fitted_samples(model, method = "user", draws = B, scale = "linear_predictor")
  expect_equal(actual$.row, seq_len(nrow(d)))
  expect_true(all(is.na(actual$.fitted[c(2, 17)])))
  expect_equal(actual$.fitted[-c(2, 17)], as.numeric(model$linear.predictors),
    tolerance = 1e-10)
  data <- scam_prediction_models$data[c(1, 1, 3, 4), ]
  data$x[3] <- NA_real_
  actual <- fitted_samples(model, data = data, method = "user", draws = B)
  expect_equal(actual$.row, seq_len(nrow(data)))
  expect_true(is.na(actual$.fitted[3]))
  expect_equal(actual$.fitted[1], actual$.fitted[2])
  expected <- as.numeric(predict(model, data[-3, ], type = "response"))
  expect_equal(actual$.fitted[-3], expected, tolerance = 1e-10)
})

test_that("canonical SCAM training predictions restore na.exclude once", {
  s <- mgcv::s
  d <- scam_prediction_models$data
  omitted <- c(3L, 31L)
  d$x[omitted] <- NA_real_
  withr::local_options(na.action = "na.exclude")
  model <- scam::scam(y ~ z + offset(o) + s(x, bs = "mpi", k = 6), data = d,
    offset = rep(0.3, nrow(d)))
  used <- model_used_rows(model)
  for (type in c("link", "response", "terms", "iterms", "lpmatrix")) {
    for (se.fit in if (type == "lpmatrix") FALSE else c(FALSE, TRUE)) {
      actual <- predict_model(model, type = type, se.fit = se.fit)
      reference <- predict_model(used, type = type, se.fit = se.fit)
      actual_values <- if (se.fit) actual else list(fit = actual)
      reference_values <- if (se.fit) reference else list(fit = reference)
      for (name in names(actual_values)) {
        value <- actual_values[[name]]
        expected <- reference_values[[name]]
        expect_equal(NROW(value), nrow(d))
        expect_true(all(is.na(if (is.matrix(value)) value[omitted, , drop = FALSE] else
          value[omitted])))
        expect_equal(as.numeric(if (is.matrix(value)) value[-omitted, , drop = FALSE] else
          value[-omitted]), as.numeric(expected), tolerance = 1e-12)
      }
      if (type == "lpmatrix") {
        expect_true(all(is.na(attr(actual, "model.offset")[omitted])))
        expect_equal(attr(actual, "model.offset")[-omitted], used$offset)
      }
    }
  }
})
