test_that("unobserved known factor combinations inform once without changing predictions", {
  nd <- factor_support_newdata(factor_support_data)
  nd$unused <- factor(seq_len(nrow(nd)))
  for (model in factor_support_models) {
    result <- capture_factor_support(fitted_values(model, data = nd, scale = "link"))
    actual <- expect_factor_support_message(result)
    native_model <- if (inherits(model, "gamm4")) gamm4_gam(model) else
      if (inherits(model, "gamm")) model$gam else model
    expected <- predict(native_model, newdata = nd, type = "link", se.fit = TRUE)
    expect_equal(actual$.fitted, as.numeric(expected$fit))
    expect_equal(actual$.se, as.numeric(expected$se.fit))
    expect_equal(actual$.lower_ci, as.numeric(expected$fit - qnorm(.975) * expected$se.fit))
    expect_equal(actual$.upper_ci, as.numeric(expected$fit + qnorm(.975) * expected$se.fit))
    expect_identical(actual$.row, seq_len(nrow(nd)))
    expect_identical(actual$unused, nd$unused)
    expect_true(all(is.finite(actual$.fitted)))
  }
})

test_that("pointwise and simultaneous predictions retain their joint calculations", {
  nd <- factor_support_newdata(factor_support_data)
  model <- factor_support_models$gam
  for (interval in c("confidence", "simultaneous")) {
    for (scale in c("response", "link")) {
      result <- capture_factor_support(fitted_values(model, data = nd,
        scale = scale, interval = interval, seed = 164, n_sim = 300))
      actual <- expect_factor_support_message(result)
      # Independent native predictions and full-matrix coefficient simulations.
      p <- predict(model, newdata = nd, type = "link", se.fit = TRUE)
      critical <- qnorm(.975)
      if (interval == "simultaneous") {
        X <- predict(model, newdata = nd, type = "lpmatrix")
        V <- vcov(model)
        se <- sqrt(diag(X %*% V %*% t(X)))
        deviations <- withr::with_seed(164,
          mvnfast::rmvn(300, rep(0, ncol(X)), V, ncores = 1)) %*% t(X)
        maxima <- apply(abs(sweep(deviations, 2, se, `/`)), 1, max)
        critical <- unname(quantile(maxima, .95, type = 8))
      }
      expect_equal(actual$.fitted, as.numeric(p$fit))
      expect_equal(actual$.se, as.numeric(p$se.fit))
      expect_equal(actual$.lower_ci, as.numeric(p$fit - critical * p$se.fit))
      expect_equal(actual$.upper_ci, as.numeric(p$fit + critical * p$se.fit))
    }
  }
  for (selection in list(list(terms = "s(x)"), list(exclude = "s(x)"))) {
    result <- capture_factor_support(do.call(fitted_values,
      c(list(object = model, data = nd, scale = "link"), selection)))
    actual <- expect_factor_support_message(result)
    expect_equal(actual$.fitted, as.numeric(do.call(predict,
      c(list(object = model, newdata = nd, type = "link"), selection))))
  }
})

test_that("retained fitting observations determine factor support", {
  nd <- factor_support_newdata(factor_support_data)
  for (model in list(factor_support_subset, factor_support_missing)) {
    result <- capture_factor_support(fitted_values(model, data = nd))
    actual <- expect_factor_support_message(result)
    expect_equal(actual$.fitted, as.numeric(predict(model, newdata = nd, type = "response")))
    # Training predictions cannot add nonexistent combinations, even na.exclude.
    expect_silent(fitted_values(model))
  }
  # Neither a mutated fitting data object nor newly supplied responses change
  # the support recorded when fitting the model.
  model <- factor_support_subset
  model$call$data <- quote(data_that_no_longer_exists)
  nd$y <- NA_real_
  expect_factor_support_message(capture_factor_support(fitted_values(model, data = nd)))
})

test_that("numeric-derived and local factor expressions use evaluated support", {
  nd <- factor_support_newdata(factor_support_data)
  nd$j <- as.integer(nd$a) - 1L
  for (fixture in list(list(model = factor_support_transformed, envir = NULL), factor_support_local)) {
    model <- fixture$model
    actual <- expect_factor_support_message(capture_factor_support(
      fitted_values(model, data = nd, envir = fixture$envir)))
    prepared <- delete_response(with_model_envir(model, fixture$envir), nd)
    expected <- predict(model, newdata = prepared, type = "response", newdata.guaranteed = TRUE)
    expect_equal(actual$.fitted, as.numeric(expected))
    result <- capture_factor_support(fitted_values(model, data = prepared))
    expect_factor_support_message(result)
    expect_equal(result$value$.fitted, actual$.fitted)
    expression <- names(model.frame(model))[2L]
    expect_match(conditionMessage(result$messages[[1]]), expression, fixed = TRUE)
  }
})

test_that("joint three-factor support is not inferred from pairwise support", {
  model <- factor_support_three
  nd <- data.frame(a = factor(c("A", "B"), levels = c("A", "B")),
    b = factor(c("low", "high"), levels = c("low", "high")),
    c = factor(c("left", "right"), levels = c("left", "right")), x = 0.5)
  for (pair in combn(c("a", "b", "c"), 2, simplify = FALSE)) {
    expect_true(all(!is.na(vctrs::vec_match(nd[pair], unique(model.frame(model)[pair])))))
  }
  actual <- expect_factor_support_message(capture_factor_support(fitted_values(model, data = nd)), "2")
  expect_equal(actual$.fitted, as.numeric(predict(model, newdata = nd, type = "response")))
})

test_that("factor values match independent of storage, level order and names", {
  model <- factor_support_models$gam
  nd <- factor_support_newdata(factor_support_data)
  expected <- expect_factor_support_message(capture_factor_support(fitted_values(model, data = nd)))
  nd$a <- as.character(nd$a)
  nd$b <- factor(nd$b, levels = rev(levels(nd$b)))
  actual <- expect_factor_support_message(capture_factor_support(fitted_values(model, data = nd)))
  expect_equal(actual$.fitted, expected$.fitted)
  # Pasted keys would collide for (a|b, c) and (a, b|c).
  d <- data.frame(`first factor` = factor(c("a|b", "a", "a|b")),
    `second factor` = ordered(c("c", "d", "b|c")), check.names = FALSE)
  model <- list(model = d)
  attr(d, "terms") <- terms(~ `first factor` + `second factor`)
  model$terms <- attr(d, "terms")
  model$formula <- ~ `first factor` + `second factor`
  model$model <- d
  model$var.summary <- as.list(d)
  nd <- d[c(1, 2), ]
  nd[[1]][2] <- "a"
  nd[[2]][2] <- "b|c"
  layout <- list(training = FALSE, data = nd, input_rows = 1:2)
  result <- capture_factor_support(inform_unobserved_factor_combinations(model, layout))
  expect_factor_support_message(result, "2")
  expect_identical(result$value, 2L)
})

test_that("missing predictors and na.action retain input row alignment", {
  model <- factor_support_models$gam
  nd <- factor_support_newdata(factor_support_data)
  nd$x[2] <- NA_real_
  nd$y <- NA_real_ # response missingness does not prevent prediction
  for (action in list(na.pass, na.exclude, na.omit)) {
    result <- capture_factor_support(fitted_values(model, data = nd, na.action = action))
    actual <- expect_factor_support_message(result, "4")
    rows <- if (identical(action, na.omit)) c(1L, 3L, 4L, 5L) else 1:5
    expect_identical(actual$.row, rows)
    native <- predict(model, newdata = nd, type = "response", na.action = na.pass)
    expect_equal(actual$.fitted, as.numeric(native[rows]))
    if (!identical(action, na.omit)) {
      expect_true(all(is.na(as.matrix(actual[2, c(".fitted", ".se", ".lower_ci", ".upper_ci")]))))
    }
  }
  expect_error(fitted_values(model, data = nd, na.action = na.fail), "missing values")
  nd$x[] <- NA_real_
  expect_silent(all_missing <- fitted_values(model, data = nd))
  expect_true(all(is.na(all_missing$.fitted)))
  expect_silent(empty <- fitted_values(model, data = nd[FALSE, ]))
  expect_equal(nrow(empty), 0L)
})

test_that("observed support and numeric extrapolation remain silent", {
  model <- factor_support_models$gam
  nd <- factor_support_newdata(factor_support_data)[c(1, 3, 5), ]
  nd$x <- c(-100, 100, 1000)
  expect_silent(fitted_values(model, data = nd))
  expect_silent(fitted_values(model))
  # Irrelevant factors cannot cause a support message in a one-factor model.
  nd <- head(model.frame(su_m_factor_by), 3)
  nd$irrelevant <- factor(seq_len(nrow(nd)))
  expect_silent(fitted_values(su_m_factor_by, data = nd))
  expect_silent(fitted_values(m_gam, data = head(model.frame(m_gam))))
  expect_silent(fitted_values(factor_support_binary,
    data = factor_support_newdata(factor_support_data)))
  # Genuine new levels are handled by native prediction, without this message.
  nd <- factor_support_newdata(factor_support_data)
  nd$a <- as.character(nd$a)
  nd$a[c(2, 4)] <- "genuinely new"
  result <- capture_factor_support(tryCatch(fitted_values(model, data = nd), error = identity))
  native <- capture_factor_support(tryCatch(predict(model, newdata = nd), error = identity))
  expect_length(result$messages, 0L)
  expect_gt(length(result$warnings), 0L)
  # The current native predictor also errors for these truly unknown levels;
  # this feature must not reinterpret or replace that existing behavior.
  expect_identical(inherits(result$value, "error"), inherits(native$value, "error"))
  if (inherits(native$value, "error")) {
    expect_identical(conditionMessage(result$value), conditionMessage(native$value))
  }
})

test_that("random-only groups and category expansion do not alter diagnostics", {
  nd <- factor_support_newdata(factor_support_data)
  nd$group <- factor(paste0("new", seq_len(nrow(nd))))
  result <- capture_factor_support(fitted_values(factor_support_models$gamm4, data = nd))
  expected <- expect_factor_support_message(result)
  nd$group <- NULL
  expect_equal(expected$.fitted, expect_factor_support_message(
    capture_factor_support(fitted_values(factor_support_models$gamm4, data = nd)))$.fitted)
  for (scale in c("link", "response")) {
    result <- capture_factor_support(fitted_values(factor_support_response, data = nd, scale = scale,
      quantity = if (scale == "response") "expectation" else "parameters"))
    actual <- expect_factor_support_message(result)
    expect_equal(nrow(actual), nrow(nd) * if (scale == "response") 3L else 1L)
    native <- predict(factor_support_response, newdata = nd, type = scale, se.fit = TRUE)
    expect_equal(actual$.fitted, as.vector(native$fit))
  }
})
