test_that("simulate.gamm forwards data and simulation options", {
  withr::local_seed(8008)
  nd <- su_eg1[c(3, 8, 20), c("x0", "x1", "x2", "x3")]
  weights <- c(1, 2, 4)
  expected <- simulate(m_gamm$gam, data = nd, nsim = 4, seed = 8009,
    weights = weights, exclude = "s(x0)")
  actual <- simulate(m_gamm, data = nd, nsim = 4, seed = 8009,
    weights = weights, exclude = "s(x0)")
  expect_identical(dim(actual), c(3L, 4L))
  expect_identical(actual, expected)

  expect_message(legacy <- simulate(m_gamm, newdata = nd, nsim = 4,
    seed = 8009, weights = weights, exclude = "s(x0)"),
    "Use of the `newdata` argument is deprecated.", fixed = TRUE)
  expect_identical(legacy, expected)
  # Explicit data takes precedence over the deprecated argument.
  expect_message(both <- simulate(m_gamm, data = nd,
    newdata = nd[1:2, ], nsim = 4, seed = 8009,
    weights = weights, exclude = "s(x0)"),
    "Use of the `newdata` argument is deprecated.", fixed = TRUE)
  expect_identical(both, expected)
})

test_that("smooth_samples.gamm forwards sampling and grid options", {
  withr::local_seed(8010)
  nd <- su_eg1[c(3, 8, 20), c("x0", "x1", "x2", "x3")]
  for (prediction_data in list(NULL, nd)) {
    args <- list(select = "s(x1)", n = 3, n_vals = 5, data = prediction_data,
      seed = 8011, freq = TRUE, mvn_method = "mgcv")
    actual <- do.call(smooth_samples, c(list(model = m_gamm), args))
    expected <- do.call(smooth_samples, c(list(model = m_gamm$gam), args))
    expect_identical(actual, expected)
    expect_identical(unique(actual$.smooth), "s(x1)")
    expect_length(unique(actual$.draw), 3L)
    locations <- if (is.null(prediction_data)) 5L else nrow(prediction_data)
    expect_length(unique(actual$.row), locations)
    expect_equal(nrow(actual), 3L * locations)
  }
})

test_that("overview.gamm forwards options and retains its extra class", {
  options <- list(
    list(parametric = FALSE),
    list(stars = TRUE),
    list(frequentist = TRUE, dispersion = 2, accuracy = 0.01, digits = 5,
      random_effects = FALSE, stars = TRUE)
  )
  for (args in options) {
    actual <- do.call(overview, c(list(model = m_gamm), args))
    expected <- do.call(overview, c(list(model = m_gamm$gam), args))
    class(expected) <- c("overview_gamm", class(expected))
    expect_identical(actual, expected)
    expect_s3_class(actual, "overview_gamm")
  }
  expect_false("parametric" %in% overview(m_gamm, parametric = FALSE)$type)
  expect_true("stars" %in% names(overview(m_gamm, stars = TRUE)))
})
