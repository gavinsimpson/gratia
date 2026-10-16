test_that("general pc inequalities cannot masquerade as named shapes", {
  d <- withr::with_seed(901, {
    x <- seq(0, 1, length.out = 100)
    data.frame(x = x, z = rep(seq(0, 1, length.out = 10), 10),
      y = -x + rnorm(100, sd = 0.05))
  })
  pc <- list(list(),
    list(x = matrix(0.5, 1, 1), matrix(1, 1, 1), -100))
  model <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 6, pc = pc),
    data = d)
  # mgcv retains the request in xt, but replaces its differences with one
  # loose point inequality. This is an actual decreasing fitted component.
  expect_identical(model$smooth[[1]]$xt, "m+")
  expect_equal(nrow(model$smooth[[1]]$Ain), 1L)
  expect_true(all(diff(stats::predict(model, type = "terms")[, 1]) < 0))
  expect_warning(expect_error(smooth_estimates(model, interval = "simultaneous",
    interval_method = "shape", n_sim = 50, seed = 1),
    "pc inequalities|retained named scasm"), "not retained")

  tensor_pc <- list(list(), list(x = matrix(0.5, 1, 1),
    z = matrix(0.5, 1, 1), matrix(1, 1, 1), -100))
  tensor <- mgcv::scasm(y ~ te(x, z, bs = c("sc", "bs"), k = c(5, 5),
    xt = list("m+", NULL), np = FALSE, pc = tensor_pc), data = d)
  expect_equal(nrow(tensor$smooth[[1]]$Ain), 1L)
  expect_warning(expect_error(smooth_estimates(tensor, interval = "simultaneous",
    interval_method = "shape", n_sim = 50, seed = 1),
    "pc inequalities|retained named scasm"), "not retained")
})

test_that("point and general equality constraints retain named restrictions", {
  d <- withr::with_seed(902, {
    x <- seq(0, 1, length.out = 100)
    data.frame(x = x, y = x - 0.5 + rnorm(100, sd = 0.05))
  })
  for (pc in list(list(x = 0.5),
                 list(list(x = matrix(0.5, 1, 1), matrix(1, 1, 1), 0)))) {
    model <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 6, pc = pc),
      data = d)
    expect_equal(nrow(model$smooth[[1]]$Ain), 5L)
    expect_silent(intervals <- smooth_estimates(model, n = 11,
      interval = "simultaneous", interval_method = "shape",
      n_sim = 50, seed = 1))
    expect_true(all(diff(intervals$.lower_ci) >= -1e-10))
    expect_true(all(diff(intervals$.upper_ci) >= -1e-10))
  }
})

test_that("automatic shape inference excludes derivative-valued margins", {
  d <- withr::with_seed(903, {
    x <- runif(100)
    z <- runif(100)
    data.frame(x = x, z = z, y = exp(x) + z + rnorm(100, sd = 0.1))
  })
  model <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 5), data = d)
  sm <- model$smooth[[1]]
  info <- smooth_shape_constraints(model, warn = FALSE)[[1]]
  sm$deriv <- 1
  expect_error(scasm_shape_spec(model, sm, info), "derivative bases")
  model <- mgcv::scasm(y ~ te(x, z, bs = c("sc", "bs"), k = c(5, 5),
    xt = list("m+", NULL), np = FALSE), data = d)
  sm <- model$smooth[[1]]
  info <- smooth_shape_constraints(model, warn = FALSE)[[1]]
  sm$margin[[2]]$deriv <- 1
  expect_error(scasm_shape_spec(model, sm, info), "derivative bases")

  # A custom pre-fit setup can carry a marginal replacement even though te()
  # has no argument for pc inside individual margins.
  sm <- model$smooth[[1]]
  sm$margin[[1]]$point.con <- list(list(),
    list(x = matrix(0.5, 1, 1), matrix(1, 1, 1), -100))
  expect_error(scasm_shape_spec(model, sm, info), "pc inequalities")
})

test_that("curvature eligibility permits rounding but rejects small knot changes", {
  d <- withr::with_seed(904, {
    x <- seq(0, 1, length.out = 100)
    data.frame(x = x, y = exp(x) + rnorm(100, sd = 0.1))
  })
  model <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "c+", k = 6), data = d)
  info <- smooth_shape_constraints(model, warn = FALSE)[[1]]
  sm <- model$smooth[[1]]
  expect_silent(scasm_shape_spec(model, sm, info))
  sm$knots[5] <- sm$knots[5] + 1e-9
  expect_error(scasm_shape_spec(model, sm, info), "irregular knots")
  # The rejected knot change is enough to make a coefficient vector with
  # exactly zero second differences yield negative spline curvature.
  sm$deriv <- 2
  beta <- seq_len(ncol(sm$Ain))
  expect_gte(min(sm$Ain %*% beta), 0)
  expect_lt(min(mgcv::Predict.matrix(sm, d) %*% beta), -1e-9)
})

test_that("domain eligibility is unaffected by covariate translation", {
  origin <- 1e12
  d <- data.frame(x = origin + seq(0, 1, length.out = 20))
  sm <- mgcv::smoothCon(mgcv::s(x, bs = "sc", xt = c("+", "m+"), k = 5),
    d)[[1]]
  prediction <- data.frame(x = origin - 0.5, .estimate = -0.5,
    .lower_ci = -0.6, .upper_ci = -0.4)
  expect_error(scasm_enforce_smooth_band(prediction, sm,
    list(x = c("+", "m+")), "simultaneous", 0.95),
    "fitted B-spline domain")
})
