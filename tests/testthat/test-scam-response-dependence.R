skip_if_not_installed("scam", minimum_version = "1.2-22")

scam_ar_model <- local({
  s <- mgcv::s
  d <- withr::with_seed(127, {
    d <- data.frame(x = runif(80), w = rep(c(1, 4), 40),
      start = rep(c(TRUE, rep(FALSE, 19)), 4))
    d$y <- exp(d$x) + rnorm(80, sd = 0.1)
    d
  })
  scam::scam(y ~ s(x, bs = "mpi", k = 6), data = d,
    weights = d$w, AR1.rho = 0.7, AR.start = d$start)
})

test_that("SCAM response simulation retains AR1 correlation and series starts", {
  model <- scam_ar_model
  distribution <- response_distribution(model)
  expect_false(distribution$independent)
  expect_true(distribution$joint_available)
  expect_match(distribution$dependence_reason, "AR1")
  withr::local_seed(413)
  draws <- replicate(2000, distribution$rd())
  standardized <- sweep(draws, 1L, model$fitted.values, `-`) /
    sqrt(model$sig2 / model$prior.weights)
  pairs <- which(!model$AR.start)
  restarts <- which(model$AR.start)[-1L]
  expect_equal(stats::cor(as.numeric(standardized[pairs, ]),
    as.numeric(standardized[pairs - 1L, ])), model$AR1.rho, tolerance = 0.015)
  expect_lt(abs(stats::cor(as.numeric(standardized[restarts, ]),
    as.numeric(standardized[restarts - 1L, ]))), 0.03)
  expect_equal(rowMeans(standardized^2), rep(1, nrow(standardized)), tolerance = 0.15)
  # simulate() and the response adapter consume the same innovations.
  withr::local_seed(872)
  expected <- replicate(4, distribution$rd())
  actual <- simulate(model, nsim = 4, seed = 872)
  expect_equal(as.numeric(actual), as.numeric(expected))
  # Hold coefficients fixed to isolate the response correlation in posterior draws.
  coefficients <- matrix(rep(coef(model), 4), nrow = 4, byrow = TRUE)
  samples <- posterior_samples(model, method = "user", draws = coefficients, seed = 872)
  for (i in 1:4) expect_equal(samples$.response[samples$.draw == i], expected[, i])
})

test_that("SCAM AR1 predictive checks use the correlated response generator", {
  model <- scam_ar_model
  for (scalable in c(FALSE, TRUE)) {
    result <- posterior_predictive_plot(model, n = 3, scalable = scalable, seed = 42)
    expect_s3_class(result, "ggplot")
    expect_silent(ggplot2::ggplot_build(result))
  }
  expect_error(posterior_samples(model, n_draws = 2, data = model$model),
    "require fitting observations")
  expect_error(simulate(model, nsim = 2, data = model$model),
    "require fitting observations")
})

test_that("SCAM response simulation aligns excluded rows and weights", {
  model <- scam_ar_model
  # Use a compact fitted frame with na.exclude metadata, as native fitting does.
  omitted <- c(3L, 27L)
  action <- structure(omitted, class = "exclude")
  model$na.action <- action
  attr(model$model, "na.action") <- action
  # Exercise expanded stored boundaries alongside compact model components.
  model$AR.start <- stats::naresid(action, model$AR.start)
  out <- simulate(model, nsim = 3, seed = 42)
  expect_identical(dim(out), c(82L, 3L))
  expect_true(all(is.na(out[omitted, ])))
  compact <- model_used_rows(model)
  expect_length(compact$AR.start, nrow(model$model))
  withr::local_seed(42)
  expected <- replicate(3, response_distribution(compact)$rd())
  expect_equal(as.numeric(out[-omitted, ]), as.numeric(expected))
  samples <- posterior_samples(model, n_draws = 2, seed = 2)
  expect_true(all(is.na(samples$.response[samples$.row %in% omitted])))
  expect_equal(sort(unique(samples$.row)), seq_len(82L))
})
