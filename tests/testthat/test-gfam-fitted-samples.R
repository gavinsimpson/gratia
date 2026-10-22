test_that("every joint coefficient draw receives its row's response transform", {
  for (model in list(gfam_model, gfam_bam)) {
    draws <- gfam_test_draws(model)
    eta <- gfam_test_eta(model, gfam_data, draws)
    expected <- gfam_test_means(eta, gfam_data$family_id)
    samples <- fitted_samples(model, method = "user", draws = draws)
    expect_s3_class(samples, "fitted_samples")
    expect_identical(samples$.row, rep(seq_len(nrow(gfam_data)), 3))
    expect_identical(samples$.draw, rep(1:3, each = nrow(gfam_data)))
    expect_true(all(samples$.parameter == "mean"))
    expect_equal(gfam_test_matrix(samples), expected, ignore_attr = TRUE)
    expect_equal(gfam_test_matrix(fitted_samples(model, method = "user",
      draws = draws, scale = "linear_predictor")), eta, ignore_attr = TRUE)

    fitted <- fitted_samples(model, method = "user", draws = draws[1, , drop = FALSE])
    response <- predict(model, type = "response", discrete = FALSE)
    expect_equal(fitted$.fitted, as.numeric(response))
  }
})

test_that("explicit prediction membership follows reordered and repeated rows", {
  rows <- c(13, 2, 17, 2, 18)
  for (model in list(gfam_model, gfam_bam)) {
    draws <- gfam_test_draws(model)
    data <- gfam_data[rows, ]
    eta <- gfam_test_eta(model, data, draws)
    expected <- gfam_test_means(eta, data$family_id)
    data$y <- NULL
    samples <- fitted_samples(model, data = data, method = "user", draws = draws)
    expect_equal(gfam_test_matrix(samples), expected, ignore_attr = TRUE)
    expect_identical(samples$.row, rep(seq_along(rows), 3))
    index <- data$family_id
    data$family_id <- NULL
    expect_error(fitted_samples(model, data = data, method = "user", draws = draws),
      "needs family membership")
    samples <- fitted_samples(model, data = data, family_index = index,
      method = "user", draws = draws)
    expect_equal(gfam_test_matrix(samples), expected, ignore_attr = TRUE)
    expect_silent(fitted_samples(model, data = data, method = "user", draws = draws,
      scale = "linear_predictor"))
  }
})

test_that("matrix responses and missing observed y do not prevent fitted draws", {
  rows <- c(17, 1, 12, 1)
  draws <- gfam_test_draws(gfam_matrix_model)
  data <- gfam_matrix_data[rows, ]
  eta <- gfam_test_eta(gfam_matrix_model, data, draws)
  expected <- gfam_test_means(eta, gfam_data$family_id[rows])
  data$response[, 1L] <- NA_real_
  expect_equal(gfam_test_matrix(fitted_samples(gfam_matrix_model, data = data,
    method = "user", draws = draws)), expected, ignore_attr = TRUE)
  data$response <- gfam_data$family_id[rows]
  expect_equal(gfam_test_matrix(fitted_samples(gfam_matrix_model, data = data,
    method = "user", draws = draws)), expected, ignore_attr = TRUE)

  data <- gfam_data[rows, ]
  data$y <- NA_real_
  draws <- gfam_test_draws(gfam_model)
  expect_equal(gfam_test_matrix(fitted_samples(gfam_model, data = data,
    method = "user", draws = draws)), gfam_test_means(
      gfam_test_eta(gfam_model, data, draws), data$family_id), ignore_attr = TRUE)
})

test_that("single rows, groups, and draws retain response dimensions", {
  draws <- gfam_test_draws(gfam_model)
  for (rows in list(2L, c(1L, 4L, 7L))) {
    data <- gfam_data[rows, ]
    for (selected in list(draws, draws[1, , drop = FALSE])) {
      result <- fitted_samples(gfam_model, data = data, method = "user", draws = selected)
      expect_equal(gfam_test_matrix(result), gfam_test_means(
        gfam_test_eta(gfam_model, data, selected), data$family_id), ignore_attr = TRUE)
      expect_identical(dim(gfam_test_matrix(result)), c(length(rows), nrow(selected)))
    }
  }
})

test_that("fitted draws restore omitted training rows exactly once", {
  draws <- gfam_test_draws(gfam_missing_model)
  result <- fitted_samples(gfam_missing_model, method = "user", draws = draws)
  expect_identical(result$.row, rep(seq_len(165), 3))
  expect_true(all(is.na(result$.fitted[result$.row %in% c(7, 16)])))
  rows <- setdiff(seq_len(165), c(7, 16))
  expected <- gfam_test_means(gfam_test_eta(gfam_missing_model,
    gfam_data[rows, ], draws), gfam_data$family_id[rows])
  expect_equal(gfam_test_matrix(result)[rows, ], expected, ignore_attr = TRUE)
  omitted <- gfam_missing_model
  class(omitted$na.action) <- "omit"
  result <- fitted_samples(omitted, method = "user", draws = draws)
  expect_identical(result$.row, rep(seq_along(rows), 3))
  expect_equal(gfam_test_matrix(result), expected, ignore_attr = TRUE)
})

test_that("prediction selectors and predictors share missing-value handling", {
  data <- gfam_data[1:8, ]
  data$family_id[c(2, 5)] <- NA_integer_
  data$x[3] <- NA_real_
  draws <- gfam_test_draws(gfam_model)
  samples <- fitted_samples(gfam_model, data = data, method = "user", draws = draws)
  expect_identical(samples$.row, rep(1:8, 3))
  expect_true(all(is.na(samples$.fitted[samples$.row %in% c(2, 3, 5)])))
  omitted <- fitted_samples(gfam_model, data = data, method = "user", draws = draws,
    na.action = na.omit)
  expect_identical(omitted$.row, rep(c(1L, 4L, 6L, 7L, 8L), 3))
  expect_equal(omitted$.fitted, samples$.fitted[!samples$.row %in% c(2, 3, 5)])
  data$family_id[] <- NA_integer_
  samples <- fitted_samples(gfam_model, data = data, method = "user", draws = draws)
  expect_true(all(is.na(samples$.fitted)))
  expect_identical(samples$.row, rep(1:8, 3))
  expect_error(fitted_samples(gfam_model, data = data, method = "user", draws = draws,
    na.action = na.fail), "missing values")
})

test_that("seeded Gaussian and MH paths preserve joint draws and caller RNG", {
  withr::with_seed(269, {
    before <- .Random.seed
    for (method in c("gaussian", "mh")) {
      args <- c(list(model = gfam_model, n_draws = 3, seed = 42, method = method),
        if (method == "mh") list(burnin = 20) else list(mvn_method = "mgcv"))
      response <- do.call(fitted_samples, args)
      link <- do.call(fitted_samples, c(args, list(scale = "linear_predictor")))
      expect_equal(gfam_test_matrix(response),
        gfam_test_means(gfam_test_matrix(link), gfam_data$family_id))
      expect_identical(do.call(fitted_samples, args), response)
      expect_identical(.Random.seed, before)
    }
    for (freq in c(FALSE, TRUE)) {
      samples <- fitted_samples(gfam_model, n_draws = 3, seed = 13, frequentist = freq,
        mvn_method = "mgcv")
      draws <- generate_draws(gfam_model, n_draws = 3, seed = 13, frequentist = freq,
        mvn_method = "mgcv")
      expect_equal(gfam_test_matrix(samples), gfam_test_means(
        gfam_test_eta(gfam_model, gfam_data, draws), gfam_data$family_id),
        ignore_attr = TRUE)
    }
  })
})

test_that("scalar extended-family means use the correct shared draw", {
  draws <- gfam_test_draws(gfam_extended_model)
  eta <- gfam_test_eta(gfam_extended_model, gfam_extended_data, draws)
  expected <- exp(eta)
  rows <- gfam_extended_data$family_id == 2L
  expected[rows, ] <- eta[rows, , drop = FALSE]
  samples <- fitted_samples(gfam_extended_model, method = "user", draws = draws)
  expect_equal(gfam_test_matrix(samples), expected, ignore_attr = TRUE)
})

test_that("special scalar means use native prediction rather than inverse link", {
  eta <- matrix(c(-0.8, 0.1, 0.6, -0.2, 0.7, 1.2), nrow = 3)
  zip <- mgcv::ziP(theta = c(-0.3, 0.2), b = 0.1)
  context <- list(index = c(1L, 2L, 1L), families = list(zip, gaussian()))
  result <- gfam_response_means(eta, context)
  lambda <- exp(eta[c(1, 3), , drop = FALSE])
  presence <- 1 - exp(-exp(-0.3 + (0.1 + exp(0.2)) * eta[c(1, 3), , drop = FALSE]))
  expected <- eta
  expected[c(1, 3), ] <- presence * lambda / (1 - exp(-lambda))
  expect_equal(result, expected)
  expect_false(isTRUE(all.equal(result, eta)))

  ordinal <- list(index = c(1L, 2L, 1L),
    families = list(mgcv::ocat(R = 4), gaussian()))
  expect_error(gfam_response_means(eta, ordinal), "ordinal response-scale fitted")
  ordinal$index[] <- 2L
  expect_equal(gfam_response_means(eta, ordinal), eta)
  context$families[[1L]]$predict <- function(...) list(matrix(1, 4, 3))
  expect_error(gfam_response_means(eta, context), "scalar response mean")
})

test_that("prediction never mutates fitted family parameter or index state", {
  family <- family(gfam_extended_model)
  snapshot <- function() list(theta = family$getTheta(),
    index = attr(family$getfl(), "fi"),
    components = lapply(family$getfl(), function(f) if (is.function(f$getTheta)) f$getTheta()))
  before <- snapshot()
  training <- fitted_samples(gfam_extended_model, method = "user",
    draws = gfam_test_draws(gfam_extended_model))
  data <- gfam_extended_data[c(19, 2, 17, 6), ]
  data$y <- NULL
  expect_silent(fitted_samples(gfam_extended_model, data = data, method = "user",
    draws = gfam_test_draws(gfam_extended_model)))
  data$family_id <- NULL
  expect_silent(fitted_samples(gfam_extended_model, data = data,
    family_index = c(3, 2, 1, 2), method = "user",
    draws = gfam_test_draws(gfam_extended_model)))
  expect_identical(snapshot(), before)
  expect_identical(fitted_samples(gfam_extended_model, method = "user",
    draws = gfam_test_draws(gfam_extended_model)), training)
})

test_that("family_index has a clear model-specific boundary", {
  expect_error(fitted_samples(m_tiny_eg1, family_index = rep(1, 100), n_draws = 1L),
    "only supported for grouped-family")
})


test_that("scalar component means handle alternative and extended-family links", {
  families <- list(gaussian("log"), binomial("probit"), poisson("sqrt"),
    Gamma("inverse"), mgcv::betar(link = "cloglog"), mgcv::scat(), mgcv::cnorm(link = "log"))
  eta <- matrix(seq(0.3, 1.2, length.out = 21), nrow = 7)
  expected <- eta
  expected[1, ] <- exp(eta[1, ])
  expected[2, ] <- pnorm(eta[2, ])
  expected[3, ] <- eta[3, ]^2
  expected[4, ] <- 1 / eta[4, ]
  expected[5, ] <- 1 - exp(-exp(eta[5, ]))
  expected[7, ] <- exp(eta[7, ])
  expect_equal(gfam_response_means(eta,
    list(index = seq_along(families), families = families)), expected)
})

test_that("prediction arguments and corrected covariance are preserved", {
  data <- gfam_data[c(1, 8, 17), ]
  draws <- gfam_test_draws(gfam_model)
  X <- mgcv::predict.gam(gfam_model, newdata = data, type = "lpmatrix",
    exclude = "s(x)", block.size = 1, newdata.guaranteed = TRUE)
  expected <- gfam_test_means(X %*% t(draws) + data$off, data$family_id)
  result <- fitted_samples(gfam_model, data = data, method = "user", draws = draws,
    exclude = "s(x)", block.size = 1)
  expect_equal(gfam_test_matrix(result), expected, ignore_attr = TRUE)
  if (!is.null(gfam_model$Vc)) {
    for (backend in c("mgcv", "mvnfast")) {
      result <- fitted_samples(gfam_model, n_draws = 3, seed = 6, unconditional = TRUE,
        mvn_method = backend)
      draws <- generate_draws(gfam_model, n_draws = 3, seed = 6, unconditional = TRUE,
        mvn_method = backend)
      expect_equal(gfam_test_matrix(result), gfam_test_means(
        gfam_test_eta(gfam_model, gfam_data, draws), gfam_data$family_id),
        ignore_attr = TRUE)
    }
  }
  expect_error(fitted_samples(gfam_model, data = data, family_index = c(0, 2, 3), n_draws = 1L),
    "one integer family-list position")
  expect_error(fitted_samples(gfam_model, data = data, family_index = c(1, 1, 1), n_draws = 1L),
    "conflicts")
})


test_that("grouped ordinal sampling returns parameters and category expectations", {
  draws <- gfam_test_draws(gfam_ordinal_model)
  expectations <- fitted_samples(gfam_ordinal_model, method = "user", draws = draws,
    quantity = "expectation")
  expect_identical(unique(expectations$.parameter), "expectation")
  expect_equal(as.numeric(tapply(expectations$.fitted[expectations$.family == 1L],
    list(expectations$.row[expectations$.family == 1L],
      expectations$.draw[expectations$.family == 1L]), sum)),
    rep(1, sum(gfam_ordinal_data$family_id == 1L) * nrow(draws)))
  parameters <- fitted_samples(gfam_ordinal_model, method = "user", draws = draws)
  eta <- gfam_test_eta(gfam_ordinal_model, gfam_ordinal_data, draws)
  expected_parameters <- eta
  poisson_rows <- gfam_ordinal_data$family_id == 2L
  expected_parameters[poisson_rows, ] <- exp(eta[poisson_rows, , drop = FALSE])
  expect_equal(gfam_test_matrix(parameters), expected_parameters, ignore_attr = TRUE)
  expect_setequal(unique(parameters$.parameter), c("location", "mean"))
  data <- gfam_ordinal_data[c(1, 8, 13), ]
  expected <- gfam_test_eta(gfam_ordinal_model, data, draws)
  data$y <- NULL
  data$family_id <- NULL
  link <- fitted_samples(gfam_ordinal_model, data = data, method = "user",
    draws = draws, scale = "linear_predictor")
  expect_equal(gfam_test_matrix(link), expected, ignore_attr = TRUE)
  data <- gfam_ordinal_data[c(2, 8, 10), ]
  expect_equal(gfam_test_matrix(fitted_samples(gfam_ordinal_model, data = data,
    method = "user", draws = draws)), exp(gfam_test_eta(gfam_ordinal_model,
      data, draws)), ignore_attr = TRUE)
})


test_that("empty prediction data return an empty typed fitted-sampling table", {
  data <- gfam_data[FALSE, ]
  for (scale in c("response", "linear_predictor")) {
    result <- fitted_samples(gfam_model, data = data, scale = scale,
      method = "user", draws = gfam_test_draws(gfam_model))
    expect_s3_class(result, "fitted_samples")
    expect_identical(nrow(result), 0L)
    expect_identical(result$.row, integer())
    expect_identical(result$.draw, integer())
    expect_identical(result$.parameter, character())
    expect_identical(result$.fitted, numeric())
  }
})
