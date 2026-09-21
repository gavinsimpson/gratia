test_that("automatic response differences agree with analytical derivatives", {
  # Fixed coefficients give an exact reference and remove posterior RNG from
  # the test. Rescaling x checks that the step follows the covariate's units.
  for (units in c(1e-3, 1, 1e3)) {
    d <- data.frame(x = units * seq(0, 1, length.out = 40), units = units)
    d$y <- round(exp(1 + .7 * d$x / units - .2 * (d$x / units)^2))
    m <- mgcv::gam(y ~ I(x / units) + I((x / units)^2),
      family = poisson(), data = d)
    nd <- data.frame(x = units * seq(.05, .95, length.out = 12), units = units)
    b <- matrix(c(1, .7, -.2), nrow = 1)
    u <- nd$x / units
    mu <- exp(1 + .7 * u - .2 * u^2)
    for (order in 1:2) for (type in c("forward", "backward", "central")) {
      truth <- if (order == 1L) mu * (.7 - .4 * u) else
        mu * ((.7 - .4 * u)^2 - .4)
      ans <- response_derivatives(m, data = nd, focal = "x", order = order,
        type = type, method = "user", draws = b, n_sim = 1)
      # Compare in the original units so tolerances have the same meaning.
      # One-sided second differences divide cancellation error by h^2.
      # With h ~ machine epsilon^(1/3), their rounding error is larger
      # and varies across BLAS implementations. Use an absolute error bound
      # on this fixed reference, rather than a vector-relative tolerance.
      tolerance <- if (order == 2L && type != "central") 1e-4 else 5e-7
      expect_lt(max(abs(ans$.derivative * units^order - truth)), tolerance,
        label = paste("maximum error: units", units, "order", order, type))
    }
  }
})

test_that("step selection is independent of prediction batching and origin", {
  d <- data.frame(x = seq(0, 1, length.out = 40))
  d$y <- 1 + d$x + d$x^2
  m <- mgcv::gam(y ~ x + I(x^2), data = d)
  nd <- data.frame(x = c(.1, .4, .8))
  b <- matrix(c(1, 1, 1), nrow = 1)
  whole <- response_derivatives(m, data = nd, focal = "x", order = 2,
    type = "central", method = "user", draws = b, n_sim = 1)
  separate <- vapply(seq_len(nrow(nd)), function(i) {
    response_derivatives(m, data = nd[i, , drop = FALSE], focal = "x",
      order = 2, type = "central", method = "user", draws = b,
      n_sim = 1)$.derivative
  }, numeric(1))
  expect_equal(whole$.derivative, separate, tolerance = 1e-7)
  expect_equal(whole$.derivative, rep(2, 3), tolerance = 1e-6)

  # Translating the coordinate must not inflate the step to the size of its
  # origin, except for the small lower bound needed for representable changes.
  shifted <- m
  shifted$model$x <- shifted$model$x + 1e6
  expect_equal(derivative_step(shifted, "x", order = 2),
    derivative_step(m, "x", order = 2))
  expect_true(1e12 + derivative_step(m, "x", data.frame(x = 1e12)) > 1e12)
})

test_that("explicit steps retain their absolute meaning", {
  d <- data.frame(x = seq(0, 1, length.out = 40))
  d$y <- round(exp(1 + .7 * d$x - .2 * d$x^2))
  m <- mgcv::gam(y ~ x + I(x^2), data = d, family = poisson())
  nd <- data.frame(x = seq(.1, .9, length.out = 8))
  b <- matrix(c(1, .7, -.2), nrow = 1)
  f <- function(x) exp(1 + .7 * x - .2 * x^2)
  h <- .01
  ans <- derivative_samples(m, data = nd, focal = "x", order = 2,
    type = "central", eps = h, method = "user", draws = b, n_sim = 1)
  expect_equal(ans$.derivative, (f(nd$x + h) - 2 * f(nd$x) + f(nd$x - h)) / h^2,
    tolerance = 1e-9)
  for (bad in list(0, -1, NA_real_, Inf, c(.01, .02), "small")) {
    expect_error(response_derivatives(m, data = nd, focal = "x", eps = bad),
      "eps.*positive finite")
  }
})

test_that("large coordinate offsets use representable increments", {
  d <- data.frame(x = 1e12 + seq(0, 1, length.out = 40), origin = 1e12)
  d$y <- 1 + (d$x - d$origin)^2
  m <- mgcv::gam(y ~ I(x - origin) + I((x - origin)^2), data = d)
  nd <- data.frame(x = 1e12 + c(.1, .4, .8), origin = 1e12)
  b <- matrix(c(1, 0, 1), nrow = 1)
  # An ideal step can round to a different interval at this magnitude. Check
  # the computed derivative, including its denominator, against exact curvature.
  ans <- response_derivatives(m, data = nd, focal = "x", order = 2,
    type = "central", method = "user", draws = b, n_sim = 1)
  expect_equal(ans$.derivative, rep(2, nrow(nd)), tolerance = 1e-6)
})

test_that("smooth derivatives choose steps on the requested scale", {
  d <- data.frame(x = exp(seq(0, 2, length.out = 80)))
  d$y <- log(d$x)
  m <- mgcv::gam(y ~ s(log(x), k = 5), data = d)
  nd <- data.frame(x = exp(seq(.2, 1.8, length.out = 10)))
  a <- derivatives(m, data = nd, type = "central", order = 2)
  b <- derivatives(m, data = nd, type = "central", order = 2, wrt = "covariate")
  expect_equal(unname(a$.derivative), rep(0, 10), tolerance = 1e-5)
  expect_equal(unname(b$.derivative), -1 / nd$x^2, tolerance = 1e-5)
})

test_that("constant and missing training coordinates have safe fallbacks", {
  d <- data.frame(x = 1:20, y = (1:20)^2)
  m <- mgcv::gam(y ~ x, data = d)
  m$model$x[] <- 0
  expect_true(is.finite(derivative_step(m, "x")))
  expect_gt(derivative_step(m, "x"), 0)
  m$model$x[c(1, 2)] <- c(NA, Inf)
  expect_gt(derivative_step(m, "x"), 0)
  m$model$x[] <- NA_real_
  expect_error(derivative_step(m, "x"), "finite values")
})

test_that("second response derivatives tolerate small fitting differences", {
  # A different thread count may change the last few digits of the fit.
  # The default step must not amplify those changes into different curves.
  m2 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df_pois,
    family = mgcv::nb(), method = "REML", control = mgcv::gam.control(nthreads = 2))
  nd <- data_slice(m_nb, x2 = evenly(x2, n = 12))
  for (type in c("forward", "backward", "central")) {
    a <- response_derivatives(m_nb, data = nd, focal = "x2", order = 2,
      type = type, n_sim = 100, seed = 2)
    b <- response_derivatives(m2, data = nd, focal = "x2", order = 2,
      type = type, n_sim = 100, seed = 2)
    expect_equal(a[c(".derivative", ".lower_ci", ".upper_ci")],
      b[c(".derivative", ".lower_ci", ".upper_ci")], tolerance = 1e-5)
  }
})
