test_that("Gaussian scale gradients stay finite at extreme predictors", {
  eta <- c(-50, 0, 350, 360, 400, 700)
  for (b in c(.01, .2, 3)) {
    model <- structure(list(family = mgcv::gaulss(b = b)), class = "gam")
    target <- ale_target(model, "scale", NULL, "response")
    result <- target$evaluate(eta)
    expected <- exp(eta)
    expect_true(all(is.finite(result$value)))
    expect_true(all(is.finite(result$slope)))
    expect_true(all(result$slope > 0))
    # Ratios detect a zero gradient even when the true derivative is tiny.
    expect_equal(
      result$slope / expected,
      rep(1, length(eta)),
      tolerance = 1e-14
    )
    expect_equal(
      result$value / (b + expected),
      rep(1, length(eta)),
      tolerance = 1e-14
    )
  }
})

test_that("Tweedie power gradients retain both logistic tails", {
  eta <- c(-700, -100, -40, -36, 0, 36, 40, 100, 700)
  for (bounds in list(c(1.01, 1.99), c(1.2, 1.8))) {
    model <- structure(
      list(family = mgcv::twlss(a = bounds[1], b = bounds[2])),
      class = "gam"
    )
    result <- ale_target(model, "power", NULL, "response")$evaluate(eta)
    tail <- exp(-abs(eta))
    expected <- diff(bounds) * tail / (1 + tail)^2
    expect_true(all(is.finite(result$value)))
    expect_true(all(is.finite(result$slope)))
    expect_true(all(result$slope > 0))
    expect_true(all(result$value >= bounds[1] & result$value <= bounds[2]))
    expect_equal(
      result$slope / expected,
      rep(1, length(eta)),
      tolerance = 1e-14
    )
    expect_equal(result$slope, rev(result$slope), tolerance = 1e-14)
  }
})

test_that("extreme Gaussian scale gradients propagate into ALE uncertainty", {
  d <- withr::with_seed(
    460,
    data.frame(x = seq(0, 1, length.out = 100), y = rnorm(100))
  )
  model <- mgcv::gam(
    list(y ~ 1, ~x),
    data = d,
    family = mgcv::gaulss(b = .2),
    method = "REML"
  )
  columns <- lss_eta_index(model)[[2]]
  # A controlled fitted object isolates prediction arithmetic from fitting at
  # these extreme scales. The tiny coefficient covariance keeps SEs finite.
  model$coefficients[columns] <- c(360, .3)
  model$Vp <- diag(length(coef(model)))
  model$Vp[columns, columns] <- diag(1e-310, length(columns))
  result <- ale(model, "x", parameter = "scale", scale = "response", n_bins = 8)
  sigma <- exp(360 + .3 * result$.value)
  derivatives <- cbind(sigma, result$.value * sigma)
  derivatives <- apply(derivatives, 2L, function(g) {
    g - mean(approx(result$.value, g, xout = d$x)$y)
  })
  expected_se <- sqrt(rowSums((derivatives * sqrt(1e-310))^2))
  expected_effect <- sigma - mean(approx(result$.value, sigma, xout = d$x)$y)
  expect_true(all(is.finite(result$.ale)))
  expect_true(all(is.finite(result$.se)))
  expect_true(all(result$.se > 0))
  expect_equal(result$.se, expected_se, tolerance = 1e-11)
  expect_equal(
    result$.ale / exp(360),
    expected_effect / exp(360),
    tolerance = 1e-12
  )
})

test_that("extreme Tweedie power gradients propagate into ALE uncertainty", {
  d <- withr::with_seed(
    461,
    data.frame(
      x = seq(0, 1, length.out = 100),
      y = mgcv::rTweedie(rep(1, 100), p = 1.5, phi = .5)
    )
  )
  model <- mgcv::gam(
    list(y ~ 1, ~x, ~1),
    data = d,
    family = mgcv::twlss(a = 1.2, b = 1.8)
  )
  columns <- lss_eta_index(model)[[2]]
  # The natural power rounds to its bound, but its mathematical derivative
  # remains representable. A controlled covariance makes its uncertainty visible.
  model$coefficients[columns] <- c(40, .2)
  model$Vp <- diag(length(coef(model)))
  model$Vp[columns, columns] <- diag(1e28, length(columns))
  result <- ale(model, "x", parameter = "power", scale = "response", n_bins = 8)
  tail <- exp(-(40 + .2 * result$.value))
  derivative <- .6 * tail / (1 + tail)^2
  gradients <- cbind(derivative, result$.value * derivative)
  gradients <- apply(gradients, 2L, function(g) {
    g - mean(approx(result$.value, g, xout = d$x)$y)
  })
  expected_se <- sqrt(rowSums((gradients * sqrt(1e28))^2))
  expect_true(all(is.finite(result$.ale)))
  expect_true(all(is.finite(result$.se)))
  expect_true(all(result$.se > 0))
  expect_equal(
    result$.se / expected_se,
    rep(1, nrow(result)),
    tolerance = 1e-12
  )
  expect_equal(
    result$.lower_ci / expected_se,
    rep(-qnorm(.975), nrow(result)),
    tolerance = 1e-12
  )
  expect_equal(
    result$.upper_ci / expected_se,
    rep(qnorm(.975), nrow(result)),
    tolerance = 1e-12
  )
})
