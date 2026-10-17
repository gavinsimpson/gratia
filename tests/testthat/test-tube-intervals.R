tube_fixture <- function(bs = "cr", by = FALSE) {
  withr::local_seed(7021)
  d <- data.frame(
    x = seq(0, 1, length.out = 90),
    group = factor(rep(c("a", "b", "c"), 30)), z = rep(1, 90)
  )
  d$y <- sin(5 * d$x) + if (by) as.integer(d$group) * d$x else 0
  d$y <- d$y + rnorm(90, sd = .3)
  model <- if (by) {
    mgcv::gam(y ~ group + s(x, by = group, bs = bs, k = 6), data = d, method = "REML")
  } else {
    mgcv::gam(y ~ s(x, bs = bs, k = 6), data = d, method = "REML")
  }
  list(model = model, data = d)
}

test_that("tube geometry matches an analytic circular arc and its bound", {
  arc <- function(x, deriv = 0L) {
    if (deriv == 0L) cbind(cos(x), sin(x)) else cbind(-sin(x), cos(x))
  }
  for (end in c(.2, 1, 3, 2 * pi)) {
    band <- tube_calibrate(arc, diag(2), c(0, end))
    expect_equal(band$length, end, tolerance = 1e-8)
    expect_lte(band$root_residual, 1e-9)
    expect_gt(band$critical, qnorm(.975))
  }
  scaled <- function(t, deriv = 0L) arc((t - 1e6) / 10, deriv) / if (deriv) 10 else 1
  a <- tube_calibrate(arc, diag(2), c(0, 1))
  b <- tube_calibrate(scaled, diag(2), c(1e6, 1e6 + 10))
  expect_equal(a$critical, b$critical, tolerance = 1e-8)
  expect_equal(tube_calibrate(arc, diag(2), c(0, 1), c(.2, .6))$critical,
    a$critical,
    tolerance = 1e-8
  )
})

test_that("tube degeneracies and covariance validation are explicit", {
  line <- function(x, deriv = 0L) cbind(if (deriv) rep(1, length(x)) else x - .5)
  a <- tube_calibrate(line, matrix(1), c(0, 1))
  expect_equal(a$critical, qnorm(.975))
  expect_identical(a$status, "rank one")
  zero <- function(x, deriv = 0L) matrix(0, length(x), 2)
  expect_equal(tube_calibrate(zero, diag(2), c(0, 1), polynomial = TRUE)$critical, 0)
  expect_equal(tube_calibrate(zero, matrix(0, 2, 2), c(0, 1))$critical, 0)
  expect_error(tube_covariance_root(diag(c(1, -.1))), "positive-semidefinite")
  expect_error(tube_covariance_root(matrix(c(1, 0, .2, 1), 2)), "symmetric")
  expect_equal(tube_covariance_root(diag(c(1, 0))) %*%
    t(tube_covariance_root(diag(c(1, 0)))), diag(c(1, 0)))
  expect_error(tube_controls(list(unknown = 1)), "named list")
  expect_error(tube_controls(list(rel_tol = 0)), "positive finite")
  expect_error(tube_domain(NULL, "x", .5), "two increasing")
  expect_error(tube_domain(c(.2, .8), "x", c(0, 1)), "inside")
  expect_error(tube_domain(list(z = c(0, 1)), "x", c(.2, .8)), "coordinate")
})

test_that("tube basis providers recover fitted matrices and derivatives", {
  x <- seq(.05, .95, length.out = 11)
  for (bs in c("cr", "cs", "ps", "tp", "ts")) {
    f <- tube_fixture(bs)
    sm <- f$model$smooth[[1]]
    nd <- data.frame(x = x)
    basis <- tube_basis(f$model, sm, nd, c(.05, .95))
    X <- smooth_predict_matrix(sm, nd, f$model)
    expect_equal(unname(basis$evaluate(x)), unname(X), tolerance = 1e-8, info = bs)
    h <- 1e-5
    ref <- (smooth_predict_matrix(sm, data.frame(x = x + h), f$model) -
      smooth_predict_matrix(sm, data.frame(x = x - h), f$model)) / (2 * h)
    expect_equal(unname(basis$evaluate(x, 1L)), unname(ref), tolerance = 1e-6, info = bs)
    for (order in 1:2) {
      for (type in c("forward", "backward", "central")) {
        h <- 1e-3
        evaluate <- tube_derivative_evaluator(basis, order, type, h)
        ref <- smooth_finite_difference(f$model, sm, nd, "x", "smooth", type, order, h)
        idx <- smooth_coef_indices(sm)
        expect_equal(unname(evaluate(x)), unname(ref[, idx, drop = FALSE]),
          tolerance = if (order == 2) 1e-6 else 1e-8, info = paste(bs, type, order)
        )
        # Differentiate the operator at a larger step as an independent check.
        ref_prime <- (evaluate(x + 1e-6) - evaluate(x - 1e-6)) / 2e-6
        expect_equal(evaluate(x, 1L), ref_prime,
          tolerance = 1e-5,
          info = paste(bs, type, order)
        )
      }
    }
  }
})

test_that("smooth tube bands retain uncertainty settings and ignore simulation controls", {
  f <- tube_fixture()
  nd <- data.frame(x = seq(.1, .9, length.out = 7))
  withr::local_seed(128)
  before <- .Random.seed
  for (overall in c(FALSE, TRUE)) {
    for (freq in c(FALSE, TRUE)) {
      for (unc in c(FALSE, TRUE)) {
        a <- smooth_estimates(f$model,
          data = nd, overall_uncertainty = overall,
          frequentist = freq, unconditional = unc, interval = "simultaneous",
          simultaneous_method = "tube", simultaneous_domain = c(.1, .9),
          n_sim = 0, n_cores = 0, seed = NA
        )
        ref <- smooth_estimates(f$model,
          data = nd, overall_uncertainty = overall,
          frequentist = freq, unconditional = unc
        )
        expect_equal(a$.estimate, ref$.estimate)
        expect_equal(a$.se, ref$.se)
        expect_equal(a$.lower_ci, a$.estimate - a$.crit * a$.se)
        expect_equal(a$.upper_ci, a$.estimate + a$.crit * a$.se)
        info <- attr(a, "interval_info")
        expect_identical(info$calibration, "tube")
        expect_equal(info$curves[[1]]$interval, c(.1, .9))
      }
    }
  }
  expect_identical(.Random.seed, before)
  a <- smooth_estimates(f$model,
    data = nd, interval = "simultaneous",
    simultaneous_method = "tube"
  )
  b <- smooth_estimates(f$model,
    data = data.frame(x = seq(.1, .9, length.out = 101)),
    interval = "simultaneous", simultaneous_method = "tube"
  )
  expect_equal(a$.crit[1], b$.crit[1])
  c <- smooth_estimates(f$model,
    data = nd, interval = "simultaneous",
    simultaneous_method = "tube", tube_control = list(rel_tol = 1e-9, abs_tol = 1e-10)
  )
  expect_equal(a$.crit, c$.crit, tolerance = 1e-6)
  expect_identical(attr(a[1:2, ], "interval_info"), attr(a, "interval_info"))
  expect_error(smooth_estimates(f$model, interval = "confidence", simultaneous_method = "tube"), "simultaneous intervals")
  expect_error(smooth_estimates(f$model, interval = "simultaneous", simultaneous_domain = c(0, 1)), "require")
  expect_error(draw(a, simultaneous_method = "tube"), "fitted model")
})

test_that("derivative and difference tube adapters preserve their quantities", {
  f <- tube_fixture()
  nd <- data.frame(x = seq(.15, .85, length.out = 9))
  for (order in 1:2) {
    for (type in c("forward", "backward", "central")) {
      a <- derivatives(f$model,
        data = nd, order = order, type = type, eps = 1e-3,
        interval = "simultaneous", simultaneous_method = "tube", n_sim = 0, ncores = 0
      )
      b <- derivatives(f$model, data = nd, order = order, type = type, eps = 1e-3)
      expect_equal(a$.derivative, b$.derivative, tolerance = 1e-6)
      expect_equal(a$.se, b$.se, tolerance = 1e-6)
      expect_identical(attr(a, "interval_info")$curves[[1]]$order, order)
    }
  }
  f <- tube_fixture(by = TRUE)
  nd <- expand.grid(x = seq(.1, .9, length.out = 8), group = levels(f$data$group))
  for (means in c(FALSE, TRUE)) {
    for (freq in c(FALSE, TRUE)) {
      a <- difference_smooths(f$model,
        select = "s(x)", data = nd,
        group_means = means, frequentist = freq, interval = "simultaneous",
        simultaneous_method = "tube", n_sim = 0, n_cores = 0, seed = NA
      )
      b <- difference_smooths(f$model,
        select = "s(x)", data = nd,
        group_means = means, frequentist = freq
      )
      expect_equal(a$.diff, b$.diff)
      expect_equal(a$.se, b$.se)
      expect_length(attr(a, "interval_info")$curves, 3L)
      expect_identical(attr(a, "interval_info")$scope, "comparison")
    }
  }
  nd <- droplevels(nd[nd$group != "c", ])
  a <- difference_smooths(f$model,
    select = "s(x)", data = nd,
    interval = "simultaneous", simultaneous_method = "tube"
  )
  nd$group <- factor(nd$group, levels = c("b", "a"))
  b <- difference_smooths(f$model,
    select = "s(x)", data = nd,
    interval = "simultaneous", simultaneous_method = "tube"
  )
  expect_equal(unname(a$.diff), unname(-b$.diff))
  expect_equal(unname(a$.lower_ci), unname(-b$.upper_ci))
})

test_that("tube wrappers forward controls and unsupported requests fail", {
  f <- tube_fixture()
  expect_s3_class(draw(f$model,
    select = "s(x)", interval = "simultaneous",
    simultaneous_method = "tube"
  ), "patchwork")
  expect_type(assemble(f$model,
    select = "s(x)", interval = "simultaneous",
    simultaneous_method = "tube"
  ), "list")
  a <- confint(f$model, parm = "s(x)", type = "simultaneous", simultaneous_method = "tube")
  expect_identical(attr(a, "interval_info")$calibration, "tube")
  withr::local_seed(3)
  d <- transform(f$data, z = runif(nrow(f$data)))
  m <- mgcv::gam(y ~ s(x, z, k = 10), data = d)
  expect_error(smooth_estimates(m, interval = "simultaneous", simultaneous_method = "tube"), "one-dimensional")
  m <- mgcv::gam(y ~ s(x, by = z, k = 6), data = d)
  expect_error(smooth_estimates(m, data = d, interval = "simultaneous", simultaneous_method = "tube"), "fixed numeric")
})
