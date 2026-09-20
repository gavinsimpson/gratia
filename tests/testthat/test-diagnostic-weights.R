test_that("diagnostics preserve base lm weights and missing rows", {
  withr::local_seed(42)
  d <- data.frame(x = runif(100), w = rep(c(0.5, 2), 50))
  d$y <- 1 + d$x + rnorm(100, sd = 1 / sqrt(d$w))
  d_missing <- d
  d_missing$x[7] <- NA
  models <- list(
    lm(y ~ x, data = d, weights = w),
    lm(y ~ x, data = d),
    lm(y ~ x, data = d_missing, weights = w, na.action = na.exclude),
    lm(y ~ x, data = d_missing, na.action = na.exclude)
  )
  # Use the base default method as an independent reference.
  before <- lapply(models, getS3method("weights", "default"))
  for (i in seq_along(models)) {
    m <- models[[i]]
    expect_identical(weights(m), before[[i]])
    expected <- if (is.null(m$weights)) rep(1, nrow(model.frame(m))) else m$weights
    expected <- napredict(m$na.action, expected)
    expect_equal(gratia:::diagnostic_weights(m), expected)
  }
  # Check weighted and unweighted QQ and worm plot construction.
  for (m in models[1:2]) {
    for (method in c("uniform", "simulate")) {
      expect_s3_class(qq_plot(m, method = method), "ggplot")
      expect_s3_class(worm_plot(m, method = method), "ggplot")
    }
  }
  g <- glm(y ~ x, data = d, weights = w)
  expect_equal(gratia:::diagnostic_weights(g), weights(g, type = "prior"))
})
