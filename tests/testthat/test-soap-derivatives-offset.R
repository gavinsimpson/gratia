soap_derivative_fixture <- local({
  model <- NULL
  function() {
    if (is.null(model)) {
      bnd <- list(data.frame(x = cos(seq(0, 2 * pi, length.out = 61)),
        y = sin(seq(0, 2 * pi, length.out = 61)),
        f = cos(seq(0, 2 * pi, length.out = 61))))
      d <- withr::with_seed(25, {
        d <- data.frame(x = runif(160, -1, 1), y = runif(160, -1, 1))
        d <- d[mgcv::in.out(bnd, as.matrix(d)), ]
        d$z <- d$x + d$y^2 + rnorm(nrow(d), sd = .03)
        d
      })
      knots <- expand.grid(x = seq(-.8, .8, length.out = 5),
        y = seq(-.8, .8, length.out = 5))
      knots <- knots[mgcv::in.out(bnd, as.matrix(knots)), ]
      model <<- mgcv::gam(z ~ s(x, y, bs = "so", k = 12,
        xt = list(bnd = bnd, nmax = 40)), data = d, knots = knots,
        method = "REML")
    }
    model
  }
})

# Native full-model predictions provide an independent reference. The slice
# holds y fixed, so the intercept does not contribute to either derivative.
soap_native_derivative <- function(model, data, order, type, h, prediction) {
  at <- function(delta) {
    d <- data
    d$x <- d$x + delta
    stats::predict(model, newdata = d, type = prediction)
  }
  result <- if (order == 1L) switch(type,
    forward = (at(h) - at(0)) / h,
    backward = (at(0) - at(-h)) / h,
    central = (at(h / 2) - at(-h / 2)) / h
  ) else switch(type,
    forward = (at(2 * h) - 2 * at(h) + at(0)) / h^2,
    backward = (at(0) - 2 * at(-h) + at(-2 * h)) / h^2,
    central = (at(h) - 2 * at(0) + at(-h)) / h^2
  )
  if (prediction == "link") as.vector(result) else result
}

test_that("soap partial derivatives include fixed boundary values for every stencil", {
  model <- soap_derivative_fixture()
  d <- data.frame(x = seq(-.5, .5, length.out = 8), y = .2)
  expect_true(any(abs(attr(mgcv::PredictMat(model$smooth[[1]], d), "offset")) > 0))
  for (order in 1:2) {
    for (type in c("forward", "backward", "central")) {
      for (wrt in c("smooth", "covariate")) {
        actual <- partial_derivatives(model, data = d, focal = "x", order = order,
          type = type, eps = .01, wrt = wrt)
        expected <- soap_native_derivative(model, d, order, type, .01, "link")
        expect_equal(unname(actual$.partial_deriv), unname(expected), tolerance = 1e-8)
        X <- soap_native_derivative(model, d, order, type, .01, "lpmatrix")
        se <- sqrt(rowSums((X %*% model$Vp) * X))
        expect_equal(unname(actual$.se), unname(se), tolerance = 1e-8)
        expect_equal(actual$.lower_ci, actual$.partial_deriv - actual$.crit * se)
        expect_equal(actual$.upper_ci, actual$.partial_deriv + actual$.crit * se)
      }
    }
  }
})

test_that("soap derivative simultaneous intervals and plots retain the boundary contribution", {
  model <- soap_derivative_fixture()
  d <- data.frame(x = seq(-.5, .5, length.out = 8), y = .2)
  actual <- partial_derivatives(model, data = d, focal = "x", type = "central",
    eps = .01, interval = "simultaneous", n_sim = 100, seed = 8)
  X <- soap_native_derivative(model, d, 1L, "central", .01, "lpmatrix")
  expected <- soap_native_derivative(model, d, 1L, "central", .01, "link")
  se <- sqrt(rowSums((X %*% model$Vp) * X))
  bands <- withr::with_seed(8, simultaneous_intervals(expected, se, X,
    model$Vp, level = .95, n_sim = 100, n_cores = 1))
  expect_equal(unname(actual$.partial_deriv), unname(expected), tolerance = 1e-8)
  expect_equal(unname(actual$.lower_ci), unname(bands$lower), tolerance = 1e-8)
  expect_equal(unname(actual$.upper_ci), unname(bands$upper), tolerance = 1e-8)
  p <- draw(actual)[[1]]
  expect_equal(p$data$.partial_deriv, actual$.partial_deriv)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("finite differences without a smooth offset retain native estimates and uncertainty", {
  d <- withr::with_seed(27, {
    d <- data.frame(x = runif(90), y = runif(90))
    d$z <- sin(d$x) + d$y^2 + rnorm(90, sd = .1)
    d
  })
  model <- mgcv::gam(z ~ te(x, y, k = c(4, 4)), data = d, method = "REML")
  nd <- data.frame(x = seq(.2, .8, length.out = 6), y = .5)
  for (order in 1:2) {
    actual <- partial_derivatives(model, data = nd, focal = "x", order = order,
      type = "central", eps = .01)
    expected <- soap_native_derivative(model, nd, order, "central", .01, "link")
    X <- soap_native_derivative(model, nd, order, "central", .01, "lpmatrix")
    # Full predictions and basis matrices subtract in different orders. Bound
    # absolute rounding error after division by h^order; a relative tolerance
    # becomes too strict when the fitted curvature is almost zero.
    prediction_scale <- max(1, abs(predict(model, nd, type = "link")))
    roundoff_bound <- 64 * .Machine$double.eps * prediction_scale / .01^order
    expect_lte(max(abs(actual$.partial_deriv - expected)), roundoff_bound)
    expect_equal(unname(actual$.se), unname(sqrt(rowSums((X %*% model$Vp) * X))), tolerance = 1e-8)
    expect_null(attr(smooth_finite_difference(model, model$smooth[[1]], nd,
      "x", "smooth", "central", order, .01), "offset"))
  }
})
