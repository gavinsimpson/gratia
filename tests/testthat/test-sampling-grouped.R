grouped_posterior_oracle <- function(model, data, draws, index, weights, seed) {
  X <- mgcv::predict.gam(model, newdata = data, type = "lpmatrix",
    newdata.guaranteed = TRUE)
  offset <- attr(X, "model.offset") %||% 0
  if (is.list(offset)) offset <- offset[[1L]]
  eta <- X %*% t(draws) + offset
  phi <- exp(model$family$getTheta()[1L])
  withr::with_seed(seed, {
    as.numeric(vapply(seq_len(NROW(draws)), function(draw) {
      out <- rep(NA_real_, length(index))
      valid <- is.finite(eta[, draw]) & !is.na(index) & !is.na(weights)
      for (i in 1:3) {
        rows <- which(valid & index == i)
        out[rows] <- switch(i,
          rbinom(length(rows), weights[rows], plogis(eta[rows, draw])) /
            (weights[rows] + (weights[rows] == 0)),
          rnorm(length(rows), eta[rows, draw], sqrt(phi / weights[rows])),
          rpois(length(rows), exp(eta[rows, draw])))
      }
      out
    }, numeric(NROW(data))))
  })
}

test_that("grouped posterior responses at evaluation data match independent draws", {
  for (model in list(gfam_model, gfam_bam)) {
    data <- gfam_data[c(24, 3, 5, 9, 1, 24, 2), ]
    draws <- gfam_test_draws(model)
    wt <- c(2, 4, 5, 2, 1, 2, 3)
    expected <- grouped_posterior_oracle(model, data, draws, data$family_id, wt, 81)
    data$y <- NULL
    actual <- posterior_samples(model, data = data, method = "user", draws = draws,
      weights = wt, seed = 81)
    expect_equal(actual$.response, expected)
    expect_identical(actual$.row, rep(seq_len(NROW(data)), NROW(draws)))
    expect_identical(actual$.draw, rep(seq_len(NROW(draws)), each = NROW(data)))
    expect_identical(actual$.parameter, rep(paste0("component", data$family_id), NROW(draws)))
    selector <- data$family_id
    data$family_id <- NULL
    explicit <- posterior_samples(model, data = data, method = "user", draws = draws,
      family_index = selector, weights = wt, seed = 81)
    expect_equal(explicit, actual)
    data$x <- seq(0.05, 0.95, length.out = NROW(data))
    data$off <- seq(-0.6, 0.6, length.out = NROW(data))
    new <- posterior_samples(model, data = data, method = "user", draws = draws,
      family_index = selector, weights = wt, seed = 81)
    expect_equal(new$.response,
      grouped_posterior_oracle(model, data, draws, selector, wt, 81))
    expect_error(posterior_samples(model, data = data, method = "user", draws = draws),
      "needs family membership")
  }
})

test_that("grouped fitting responses retain existing seeded draw order", {
  for (model in list(gfam_model, gfam_bam)) {
    for (method in c("user", "gaussian")) {
      draws <- if (method == "user") gfam_test_draws(model) else NULL
      args <- list(model = model, n = 3L, seed = 37, method = method, draws = draws)
      if (is.null(draws)) args$draws <- NULL
      old <- do.call(diagnostic_posterior_samples.default, args)
      args$n_draws <- args$n
      args$n <- NULL
      new <- do.call(posterior_samples, args)
      expect_identical(new$.response, old$.response)
      expect_identical(new$.draw, old$.draw)
      expect_identical(new$.row, old$.row)
      expect_identical(new$.parameter, old$.parameter)
    }
  }
  model <- gfam_model
  # Fitting-call offsets must be retained even though newdata prediction
  # intentionally ignores that argument in mgcv.
  call_offset <- seq(-0.3, 0.3, length.out = NROW(model$model))
  model$linear.predictors <- model$linear.predictors + call_offset
  model$offset <- model$offset + call_offset
  draws <- gfam_test_draws(model)
  old <- diagnostic_posterior_samples.default(model, 3, seed = 19,
    method = "user", draws = draws)
  new <- posterior_samples(model, n_draws = 3, seed = 19,
    method = "user", draws = draws)
  expect_equal(new$.response, old$.response)
})

test_that("grouped posterior responses preserve chains, diagnostics and joins", {
  data <- gfam_data[c(7, 2, 9, 3), ]
  data$y <- NULL
  draws <- suppressWarnings(post_draws(gfam_model, n_draws = 40, chains = 2, seed = 71,
    diagnostics = TRUE))
  attr(draws, "sampling")$draw_ids$.draw <- 1000L + seq_len(NROW(draws))
  actual <- posterior_samples(gfam_model, data = data, draws = draws,
    method = "user", seed = 52)
  index <- attr(draws, "sampling")$draw_ids
  expect_equal(actual$.chain, rep(index$.chain, each = NROW(data)))
  expect_equal(actual$.iteration, rep(index$.iteration, each = NROW(data)))
  expect_equal(actual$.draw, rep(index$.draw, each = NROW(data)))
  expect_equal(sampling_diagnostics(actual), sampling_diagnostics(draws))
  augmented <- add_posterior_samples(data, gfam_model, method = "user", draws = draws,
    seed = 52)
  expect_equal(augmented$.response,
    actual$.response[order(actual$.row, actual$.draw)])
  expect_equal(sampling_diagnostics(augmented), sampling_diagnostics(draws))
  expect_equal(posterior_samples(gfam_model, data = data, draws = draws,
    method = "user", seed = 52), actual)
  set.seed(831)
  before <- .Random.seed
  posterior_samples(gfam_model, data = data, draws = draws, method = "user", seed = 52)
  expect_identical(.Random.seed, before)
})

test_that("grouped missing evaluation rows and weights remain aligned", {
  data <- gfam_data[1:8, ]
  data$y <- NULL
  data$x[3] <- NA_real_
  data$family_id[5] <- NA_integer_
  wt <- c(2, NA, 1, 3, 2, 1, 4, 2)
  draws <- gfam_test_draws(gfam_model)
  actual <- posterior_samples(gfam_model, data = data, method = "user", draws = draws,
    seed = 64, weights = wt)
  missing <- c(2, 3, 5)
  expect_equal(NROW(actual), 24L)
  expect_true(all(is.na(actual$.response[actual$.row %in% missing])))
  keep <- setdiff(seq_len(NROW(data)), missing)
  compact <- posterior_samples(gfam_model, data = data[keep, ],
    method = "user", draws = draws, seed = 64, weights = wt[keep])
  expect_equal(actual$.response[!actual$.row %in% missing], compact$.response)
  omitted <- posterior_samples(gfam_model, data = data, method = "user", draws = draws,
    seed = 64, weights = wt, na.action = na.omit)
  expect_identical(unique(omitted$.row), c(1L, 2L, 4L, 6L, 7L, 8L))
  expect_equal(omitted$.response,
    actual$.response[!actual$.row %in% c(3, 5)])
  expect_error(posterior_samples(gfam_model, data = data, method = "user", draws = draws,
    na.action = na.fail), "missing")
  data$x[] <- NA_real_
  empty <- posterior_samples(gfam_model, data = data, method = "user", draws = draws)
  expect_equal(NROW(empty), 24L)
  expect_true(all(is.na(empty$.response)))
  model <- gfam_missing_model
  fit <- posterior_samples(model, method = "user", draws = gfam_test_draws(model), seed = 3)
  expect_equal(NROW(fit), length(model_row_map(model)) * 3L)
  expect_true(all(is.na(fit$.response[fit$.row %in% c(7, 16)])))
})

test_that("grouped response weights and membership fail informatively", {
  data <- gfam_data[1:8, ]
  draws <- gfam_test_draws(gfam_model)
  expect_error(posterior_samples(gfam_model, data = data, method = "user", draws = draws,
    weights = 1:2), "one value per prediction row")
  expect_error(posterior_samples(gfam_model, data = data, method = "user", draws = draws,
    family_index = rep(1L, 8)), "conflicts")
  expect_error(posterior_samples(gfam_model, data = data, method = "user", draws = draws,
    weights = -1), "non-negative")
  expect_error(posterior_samples(gfam_model, data = data, method = "user", draws = draws,
    weights = 1.5), "integer trial")
  a <- posterior_samples(gfam_model, data = data, method = "user", draws = draws,
    weights = 2, seed = 9)
  b <- posterior_samples(gfam_model, data = data, method = "user", draws = draws,
    weights = rep(2, 8), seed = 9)
  expect_equal(a, b)
})

test_that("grouped prediction and smooth workflows accept supplied data", {
  data <- gfam_data[c(4, 2, 6, 3), ]
  data$y <- NULL
  draws <- post_draws(gfam_model, n_draws = 4, chains = 2, seed = 3)
  fs <- fitted_samples(gfam_model, data = data, method = "user", draws = draws)
  ds <- derivative_samples(gfam_model, data = data, focal = "x", type = "central",
    eps = 1e-5, method = "user", draws = draws)
  plus <- minus <- data
  plus$x <- plus$x + 1e-5
  minus$x <- minus$x - 1e-5
  hi <- fitted_samples(gfam_model, data = plus, method = "user", draws = draws)
  lo <- fitted_samples(gfam_model, data = minus, method = "user", draws = draws)
  expected <- (hi$.fitted - lo$.fitted) / 2e-5
  expect_equal(ds$.derivative[order(ds$.draw, ds$.row)], expected, tolerance = 1e-7)
  expect_equal(sort(unique(ds$.draw)), 1:4)
  expect_equal(sort(unique(ds$.chain)), 1:2)
  smooth <- smooth_samples(gfam_model, data = data, method = "user", draws = draws)
  expect_equal(sort(unique(smooth$.draw)), 1:4)
  expect_equal(sort(unique(smooth$.chain)), 1:2)
  fixed <- predicted_samples(gfam_model, data = data, n_draws = 3, seed = 7)
  expected <- simulate(gfam_model, data = data, nsim = 3, seed = 7)
  expect_equal(fixed$.response, as.numeric(as.matrix(expected)))
  expect_equal(NROW(fs), 16L)
})


test_that("grouped matrix responses and ordinal components work at new data", {
  data <- gfam_matrix_data[c(10, 8, 10, 3, 1), ]
  label <- names(gfam_matrix_model$model)[1L]
  data[[label]][, 1L] <- NA_real_
  draws <- gfam_test_draws(gfam_matrix_model)
  actual <- posterior_samples(gfam_matrix_model, data = data, method = "user",
    draws = draws, seed = 45)
  expected <- grouped_posterior_oracle(gfam_matrix_model, data, draws,
    data[[label]][, 2L], rep(1, NROW(data)), 45)
  expect_equal(actual$.response, expected)
  data <- gfam_ordinal_data[c(1, 4, 7, 2, 5), ]
  data$y <- NULL
  draws <- gfam_test_draws(gfam_ordinal_model)
  actual <- posterior_samples(gfam_ordinal_model, data = data,
    method = "user", draws = draws, seed = 82)
  rows <- rep(data$family_id == 1L, NROW(draws))
  expect_true(all(actual$.response[rows] %in% 1:4))
  expect_true(all(actual$.response[!rows] >= 0 &
    actual$.response[!rows] == floor(actual$.response[!rows])))
  expectations <- fitted_samples(gfam_ordinal_model, data = data,
    method = "user", draws = draws, quantity = "expectation")
  expect_true(all(is.na(expectations$.category[expectations$.family == 2L])))
  expect_error(derivative_samples(gfam_model, data = gfam_data[1:4, ], focal = "x",
    n_draws = 2, family_index = 1:4), "family_index.*not supported")
})


test_that("grouped preallocated output exactly matches streaming tables", {
  data <- gfam_data[c(7, 2, 9, 3, 1, 2), ]
  data$y <- NULL
  data$x[2] <- NA_real_
  data$family_id[4] <- NA_integer_
  weights <- c(2, 3, NA_real_, 1, 4, 2)
  # Preserve the previous table-per-draw assembly as an independent output
  # reference, including its missing-row restoration and component labels.
  reference <- function(context, betas, weights, seed) {
    wt <- grouped_sampling_weights(context, weights)
    parameter <- context$family_index[context$layout$rows]
    parameter <- ifelse(is.na(parameter), NA_character_, paste0("component", parameter))
    withr::with_seed(seed, dplyr::bind_rows(lapply(seq_len(NROW(betas)), function(i) {
      response <- if (all(is.na(context$layout$map))) {
        rep(NA_real_, NROW(context$X))
      } else {
        eta <- drop(context$X %*% betas[i, ]) + context$offset
        gfam_response_draw(eta, wt, context, context$generators)
      }
      value <- diagnostic_response_table(response, i)
      value <- restore_prediction_table(value, context$layout)
      value$.parameter <- parameter
      value
    })))
  }
  for (model in list(gfam_model, gfam_bam)) {
    betas <- post_draws(model, n_draws = 8, chains = 2, seed = 4)
    attr(betas, "sampling")$draw_ids$.draw <- c(101:104, 701:704)
    for (action in list(na.pass, na.exclude, na.omit)) {
      context <- grouped_posterior_context(model, data, na.action = action)
      old <- reference(context, betas, weights, 15)
      fast <- withr::with_seed(15, grouped_response_draws(context, betas, weights))
      expect_identical(fast, old)
      sampler <- grouped_response_sampler(context, betas, weights)
      streamed <- withr::with_seed(15, dplyr::bind_rows(lapply(seq_len(NROW(betas)), sampler)))
      expect_identical(streamed, old)
      actual <- posterior_samples(model, data = data, method = "user", draws = betas,
        weights = weights, seed = 15, na.action = action)
      old <- attach_sampling(old, betas, draw_index = TRUE)
      class(old) <- c("posterior_samples", class(old))
      expect_identical(actual, old)
    }
  }
  model <- gfam_missing_model
  betas <- gfam_test_draws(model)
  weights <- rep(2, length(model_row_map(model)))
  weights[3] <- NA_real_
  context <- grouped_posterior_context(model)
  expect_identical(withr::with_seed(30, grouped_response_draws(context, betas, weights)),
    reference(context, betas, weights, 30))
  data$x[] <- NA_real_
  context <- grouped_posterior_context(model, data)
  expect_identical(withr::with_seed(30, grouped_response_draws(context, betas)),
    reference(context, betas, NULL, 30))
})
