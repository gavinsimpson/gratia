test_that("tube partitions merge rounding copies without changing the domain", {
  expect_equal(tube_partition(c(0, .5, .5 + .Machine$double.eps, 1 - .Machine$double.eps, 1)), c(0, .5, 1))
  withr::local_seed(29017)
  d <- data.frame(x = seq(0, 1, length.out = 150))
  d$y <- sin(6 * d$x) + rnorm(nrow(d), sd = .3)
  model <- mgcv::gam(y ~ s(x, bs = "cr", k = 5), data = d, method = "REML")
  a <- derivatives(model, order = 2, type = "central", interval = "simultaneous", simultaneous_method = "tube")
  expect_identical(attr(a, "interval_info")$curves[[1]]$status, "converged")
  expect_true(all(is.finite(a$.lower_ci)))
  narrow <- function(x, deriv = 0L) if (deriv) cbind(rep(1, length(x)), 0) else cbind(x, 1)
  expect_error(tube_calibrate(narrow, diag(c(1, 1e-30)), c(-1, 1)), "Near-zero")
  zero <- function(x, deriv = 0L) {
    if (deriv) cbind(1, 2 * (x - .5)) else cbind(x - .5, (x - .5)^2)
  }
  z <- tube_calibrate(zero, diag(2), c(0, 1), breakpoints = .5, polynomial = TRUE)
  expect_equal(z$components, 2L)
  expect_true(is.finite(z$critical))
})

test_that("tube model wrappers and constant numeric by terms retain predictions", {
  withr::local_seed(809)
  d <- data.frame(
    x = seq(0, 1, length.out = 120), z = 2,
    group = factor(rep(1:6, 20))
  )
  d$y <- sin(6 * d$x) + c(-1, -.6, -.2, .2, .6, 1)[d$group] + rnorm(nrow(d), sd = .3)
  ordinary <- mgcv::gam(y ~ s(x, by = z, bs = "cr", k = 6), data = d, method = "REML")
  nd <- data.frame(x = seq(.1, .9, length.out = 9), z = 2)
  a <- smooth_estimates(ordinary, data = nd, interval = "simultaneous", simultaneous_method = "tube")
  ref <- smooth_estimates(ordinary, data = nd)
  expect_equal(a$.estimate, ref$.estimate)
  expect_equal(a$.se, ref$.se)
  zero <- smooth_estimates(ordinary,
    data = transform(nd, z = 0),
    interval = "simultaneous", simultaneous_method = "tube"
  )
  expect_equal(zero$.estimate, rep(0, nrow(zero)))
  expect_true(all(zero$.se > 0)) # retain requested overall mean uncertainty
  component <- smooth_estimates(ordinary,
    data = transform(nd, z = 0),
    overall_uncertainty = FALSE, interval = "simultaneous", simultaneous_method = "tube"
  )
  expect_equal(component$.lower_ci, component$.upper_ci)
  bam <- mgcv::bam(y ~ s(x, bs = "cr", k = 6), data = d, discrete = TRUE, method = "fREML")
  a <- smooth_estimates(bam, data = nd, interval = "simultaneous", simultaneous_method = "tube")
  expect_identical(attr(a, "interval_info")$calibration, "tube")
  expect_equal(a$.estimate, smooth_estimates(bam, data = nd)$.estimate)
  g <- mgcv::gamm(y ~ s(x, bs = "cr", k = 6), random = list(group = ~1), data = d)
  a <- smooth_estimates(g, data = nd, interval = "simultaneous", simultaneous_method = "tube")
  expect_equal(a$.estimate, smooth_estimates(g$gam, data = nd)$.estimate)
  b <- derivatives(g,
    data = nd, type = "central", order = 2,
    interval = "simultaneous", simultaneous_method = "tube"
  )
  expect_identical(attr(b, "interval_info")$calibration, "tube")
  if (requireNamespace("gamm4", quietly = TRUE)) {
    g <- gamm4::gamm4(y ~ s(x, bs = "cr", k = 6), random = ~ (1 | group), data = d)
    a <- smooth_estimates(g, data = nd, interval = "simultaneous", simultaneous_method = "tube")
    expect_identical(attr(a, "interval_info")$calibration, "tube")
  }
})

test_that("tube metadata survive nested estimates and use smooth coordinates", {
  withr::local_seed(908)
  d <- data.frame(x = seq(0, 1, length.out = 100), z = seq(0, 1, length.out = 100)^2)
  d$y <- sin(6 * d$x) + rnorm(100, sd = .3)
  model <- mgcv::gam(y ~ s(x, bs = "cr", k = 6) + s(z, bs = "cr", k = 5), data = d, method = "REML")
  nd <- data.frame(x = seq(.1, .9, length.out = 9), z = seq(.1, .9, length.out = 9))
  a <- smooth_estimates(model,
    data = nd, interval = "simultaneous", simultaneous_method = "tube",
    simultaneous_domain = list(x = c(.1, .9), z = c(.1, .9)), unnest = FALSE
  )
  expect_length(attr(a, "interval_info")$curves, 2L)
  expect_length(attr(a[1, ], "interval_info")$curves, 1L)
  expect_s3_class(draw(a), "patchwork")
  transformed <- transform(d, x = exp(1 + x))
  model <- mgcv::gam(y ~ s(log(x), bs = "cr", k = 6), data = transformed, method = "REML")
  a <- smooth_estimates(model, interval = "simultaneous", simultaneous_method = "tube")
  expect_identical(attr(a, "interval_info")$curves[[1]]$coordinate, "log(x)")
  b <- derivatives(model,
    wrt = "smooth", type = "central",
    interval = "simultaneous", simultaneous_method = "tube"
  )
  expect_identical(attr(b, "interval_info")$calibration, "tube")
  expect_error(derivatives(model,
    wrt = "covariate", focal = "x", type = "central",
    interval = "simultaneous", simultaneous_method = "tube"
  ), "smooth coordinate")
  expect_error(draw(b, simultaneous_method = "tube"), "fitted model")
  expect_error(partial_derivatives(model, simultaneous_method = "tube"), "one-dimensional")
})

test_that("tube calibration is invariant to coefficient changes of basis", {
  evaluate <- function(x, deriv = 0L) {
    if (deriv) cbind(-sin(x), cos(x)) else cbind(cos(x), sin(x))
  }
  A <- matrix(c(2, .3, -.4, 1), 2)
  inverse <- solve(A)
  V <- diag(c(1, 2))
  other <- function(x, deriv = 0L) evaluate(x, deriv) %*% A
  a <- tube_calibrate(evaluate, V, c(.1, 1))
  b <- tube_calibrate(other, inverse %*% V %*% t(inverse), c(.1, 1))
  expect_equal(a$critical, b$critical, tolerance = 1e-7)
  expect_equal(a$length, b$length, tolerance = 1e-7)
})

test_that("tube calibration does not create an absent random-number state", {
  withr::local_preserve_seed()
  d <- data.frame(x = seq(0, 1, length.out = 80))
  d$y <- sin(6 * d$x)
  model <- mgcv::gam(y ~ s(x, bs = "cr", k = 6), data = d, method = "REML")
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  smooth_estimates(model, interval = "simultaneous", simultaneous_method = "tube")
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("subsampled thin plate knots do not truncate the coverage domain", {
  withr::local_seed(146)
  d <- data.frame(x = seq(0, 1, length.out = 200))
  d$y <- sin(6 * d$x) + rnorm(200, sd = .2)
  model <- mgcv::gam(y ~ s(x, k = 6, xt = list(max.knots = 12)), data = d, method = "REML")
  band <- smooth_estimates(model, interval = "simultaneous", simultaneous_method = "tube")
  expect_equal(attr(band, "interval_info")$curves[[1]]$interval, c(0, 1))
  expect_equal(band$.estimate, smooth_estimates(model)$.estimate)
})

test_that("tube contrasts reject coordinate-dependent group contributions at a single point", {
  withr::local_seed(912)
  d <- data.frame(x = rep(seq(0, 1, length.out = 70), 2), group = factor(rep(c("a", "b"), each = 70)))
  d$y <- sin(6 * d$x) + as.integer(d$group) * d$x + rnorm(140, sd = .2)
  model <- mgcv::gam(y ~ group * x + s(x, by = group, bs = "cr", k = 6), data = d, method = "REML")
  nd <- data.frame(x = .5, group = factor(c("a", "b")))
  expect_error(difference_smooths(model,
    select = "s(x)", data = nd, group_means = TRUE,
    interval = "simultaneous", simultaneous_method = "tube", simultaneous_domain = c(.1, .9)
  ), "constant along")
  band <- difference_smooths(model,
    select = "s(x)", data = nd, group_means = FALSE,
    interval = "simultaneous", simultaneous_method = "tube", simultaneous_domain = c(.1, .9)
  )
  expect_true(all(is.finite(band$.lower_ci)))
  curve <- smooth_estimates(model, select = "s(x):groupa", interval = "simultaneous", simultaneous_method = "tube")
  expect_warning(values <- curve[, ".estimate", drop = FALSE], NA)
  expect_length(attr(values, "interval_info")$curves, 0L)
})
