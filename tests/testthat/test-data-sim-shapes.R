test_that("shape functions and analytic derivatives have the advertised signs", {
  x <- seq(0, 1, length.out = 501)
  shapes <- sub("^sc_", "", data_sim_shapes()[1:9])
  for (shape in shapes) {
    f <- sim_shape_function(x, shape)
    if (startsWith(shape, "increasing")) expect_true(all(f$d1 >= 0))
    if (startsWith(shape, "decreasing")) expect_true(all(f$d1 <= 0))
    if (grepl("convex$", shape)) expect_true(all(f$d2 >= 0))
    if (grepl("concave$", shape)) expect_true(all(f$d2 <= 0))
    if (shape == "positive") expect_true(all(f$f > 0))
    h <- 1e-4
    plus <- sim_shape_function(x + h, shape)$f
    minus <- sim_shape_function(x - h, shape)$f
    expect_equal((plus - minus) / (2 * h), f$d1, tolerance = 1e-5)
    expect_equal((plus - 2 * f$f + minus) / h^2, f$d2, tolerance = 1e-5)
  }
  for (model in data_sim_shapes()) {
    d <- data_sim(model, n = 40, seed = 2)
    expect_equal(nrow(d), 40)
    expect_true(all(is.finite(d$y)))
  }
  d <- data_sim("sc_tensor", n = 50, seed = 2)
  expect_true(all(d$dx > 0 & d$dz > 0))
  d <- data_sim("sc_location_scale", n = 50, seed = 2)
  expect_true(all(d$d2 <= 0 & d$d_scale > 0))
  expect_equal(attr(d, "truth")$parameters$mu, exp(d$f))
  expect_equal(attr(d, "truth")$parameters$dispersion, exp(d$f_scale))
})

test_that("scasm reference recipes fit and enforce the requested shapes", {
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  constraints <- list(increasing = "m+", decreasing = "m-", convex = "c+", concave = "c-",
    positive = "+", increasing_convex = c("m+", "c+"), increasing_concave = c("m+", "c-"),
    decreasing_convex = c("m-", "c+"), decreasing_concave = c("m-", "c-"))
  for (shape in names(constraints)) {
    d <- data_sim(paste0("sc_", shape), n = 160, seed = 21)
    xt <- constraints[[shape]]
    form <- if (shape == "positive") y ~ s(x, bs = "sc", xt = xt, k = 7) - 1 else
      y ~ s(x, bs = "sc", xt = xt, k = 7)
    m <- mgcv::scasm(form, data = d, bs = 0)
    expect_true(all(drop(m$Ain %*% stats::coef(m)) >= m$bin - 1e-7), info = shape)
    expect_true(all(is.finite(smooth_estimates(m)$.estimate)), info = shape)
    deriv <- derivatives(m, order = if (shape %in% c("convex", "concave")) 2 else 1)
    expect_true(all(is.finite(deriv$.derivative)), info = shape)
    if (shape == "positive") expect_true(all(stats::predict(m) >= -1e-7))
  }
})

test_that("additive, tensor and location-scale scasm recipes are usable", {
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  d <- data_sim("sc_additive", n = 240, family = stats::poisson(),
    predictors = function(d) d$f, seed = 2)
  m <- mgcv::scasm(y ~ s(x0, k = 6) + s(x1, bs = "sc", xt = "m+", k = 6) +
    s(x2, k = 6) + s(x3, bs = "sc", xt = "c+", k = 6), family = stats::poisson(), data = d)
  expect_true(all(drop(m$Ain %*% stats::coef(m)) >= m$bin - 1e-7))
  d <- data_sim("sc_tensor", n = 160, seed = 2)
  m <- mgcv::scasm(y ~ te(x, z, bs = c("sc", "sc"), xt = list("m+", "m+"),
    k = c(5, 5)), data = d)
  expect_true(all(drop(m$Ain %*% stats::coef(m)) >= m$bin - 1e-7))
  expect_true(all(is.finite(smooth_estimates(m, n_2d = 10)$.estimate)))
  d <- data_sim("sc_location_scale", n = 300, seed = 3)
  m <- mgcv::scasm(list(y ~ s(x, bs = "sc", xt = "c-", k = 6),
    ~s(z, bs = "sc", xt = "m+", k = 6)), family = mgcv::gammals(), data = d)
  expect_true(all(drop(m$Ain %*% stats::coef(m)) >= m$bin - 1e-7))
  expect_true(all(is.finite(smooth_estimates(m)$.estimate)))
  expect_true(all(is.finite(derivatives(m)$.derivative)))
})

test_that("native scasm bootstrap draws respect the reference constraints", {
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  d <- data_sim("sc_increasing", n = 80, seed = 9)
  m <- withr::with_seed(22, mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 6),
    data = d, bs = 4))
  expect_equal(ncol(m$bs), 4L)
  expect_true(all(m$Ain %*% m$bs >= m$bin - 1e-7))
})
