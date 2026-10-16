scasm_interval_fixture <- function(tensor = FALSE) {
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  withr::with_seed(9, {
    d <- data.frame(x = runif(100), z = runif(100), off = seq(-.2, .2, length.out = 100))
    d$y <- d$x + d$z + d$off + rnorm(100, sd = .3)
    form <- if (tensor) {
      y ~ te(x, z, bs = c("sc", "cr"), xt = list("m+", NULL), k = c(5, 4)) + offset(off)
    } else y ~ s(x, bs = "sc", xt = "m+", k = 6) + offset(off)
    mgcv::scasm(form, data = d, bs = 8)
  })
}

test_that("scasm detection and interval selection use fitted model storage", {
  m <- scasm_interval_fixture()
  expect_true(is_scasm_model(m))
  expect_identical(scasm_interval_method(m), "bootstrap")
  expect_identical(scasm_interval_method(m, "gaussian"), "gaussian")
  expect_warning(expect_identical(scasm_interval_method(m,
    interval = "simultaneous"), "gaussian"), "stored scasm bootstrap draws")
  expect_error(scasm_interval_method(m, "bootstrap", "simultaneous"),
    "Simultaneous bootstrap bands")
  expect_warning(expect_identical(scasm_interval_method(m,
    supported = FALSE), "gaussian"), "family prediction")
  expect_error(scasm_interval_method(m, "bootstrap", supported = FALSE),
    "family prediction")
  m$Ain <- matrix(numeric(), 0, length(coef(m)))
  m$bin <- numeric()
  expect_true(is_scasm_model(m))
  m$bs <- NULL
  expect_identical(scasm_interval_method(m), "gaussian")
  expect_error(scasm_interval_method(m, "bootstrap"), "valid stored")
  m$bs <- matrix(0, 2, 3)
  expect_false(valid_scasm_bootstrap(m))
  m$beta0 <- NULL
  expect_false(is_scasm_model(m))
  ordinary <- mgcv::gam(y ~ s(x, k = 5), data = m$model)
  expect_false(is_scasm_model(ordinary))
  expect_identical(scasm_interval_method(ordinary), "gaussian")
})

test_that("bootstrap projections match native mgcv limits and their correction", {
  m <- scasm_interval_fixture()
  d <- m$model[1:20, ]
  X <- predict(m, newdata = d, type = "lpmatrix")
  point <- predict(m, newdata = d, se.fit = TRUE)
  oracle <- predict(m, newdata = d, se.fit = .9)
  actual <- scasm_bootstrap_intervals(m, X, point$fit, point$se.fit, level = .9)
  expect_equal(actual$lower, oracle$ll, ignore_attr = TRUE)
  expect_equal(actual$upper, oracle$ul, ignore_attr = TRUE)
  expect_true(is.na(actual$critical))
  expect_identical(scasm_interval_info(m, "bootstrap", "confidence", .9)$correction,
    "svar.only")
  attr(m$bs, "svar.only") <- NULL
  uncorrected <- scasm_bootstrap_intervals(m, X, point$fit, point$se.fit, level = .9)
  native <- predict(m, newdata = d, se.fit = .9)
  expect_equal(uncorrected$lower, native$ll, ignore_attr = TRUE)
  expect_gt(max(abs(actual$lower - uncorrected$lower)), 0)
  # Missing rows must not affect the correction applied to represented rows.
  XX <- rbind(X[1, ], rep(NA_real_, ncol(X)))
  missing <- scasm_bootstrap_intervals(m, XX, c(point$fit[1], NA), c(point$se.fit[1], NA))
  expect_true(is.na(missing$lower[2]))
  empty <- scasm_bootstrap_intervals(m, X[FALSE, , drop = FALSE], numeric(), numeric())
  expect_length(empty$lower, 0)
})

test_that("smooth bootstrap limits follow native component predictions", {
  m <- scasm_interval_fixture()
  d <- m$model[1:20, ]
  actual <- smooth_estimates(m, data = d, interval = "confidence",
    interval_method = "bootstrap", overall_uncertainty = FALSE)
  native <- predict(m, newdata = d, type = "terms", terms = "s(x)", se.fit = .95)
  expect_equal(actual$.lower_ci, drop(native$ll), ignore_attr = TRUE)
  expect_equal(actual$.upper_ci, drop(native$ul), ignore_attr = TRUE)
  expect_true(all(is.na(actual$.crit)))
  gaussian <- smooth_estimates(m, data = d, interval = "confidence",
    interval_method = "gaussian", overall_uncertainty = FALSE)
  expect_equal(actual$.se, gaussian$.se)
})

test_that("bootstrap derivative limits project stored coefficient draws", {
  for (tensor in c(FALSE, TRUE)) {
    m <- scasm_interval_fixture(tensor)
    d <- data.frame(x = seq(.1, .9, length.out = 10), z = .5, off = 0)
    fun <- if (tensor) partial_derivatives else derivatives
    args <- list(object = m, data = d, eps = 1e-5, type = "central",
      interval_method = "bootstrap")
    if (tensor) args$focal <- "x"
    actual <- do.call(fun, args)
    plus <- minus <- d
    plus$x <- plus$x + 1e-5 / 2
    minus$x <- minus$x - 1e-5 / 2
    X <- (predict(m, newdata = plus, type = "lpmatrix") -
      predict(m, newdata = minus, type = "lpmatrix")) / 1e-5
    estimate <- if (tensor) actual$.partial_deriv else actual$.derivative
    expected <- scasm_bootstrap_intervals(m, X, estimate, actual$.se)
    expect_equal(actual$.lower_ci, expected$lower, ignore_attr = TRUE, tolerance = 1e-6)
    expect_equal(actual$.upper_ci, expected$upper, ignore_attr = TRUE, tolerance = 1e-6)
    expect_true(all(is.na(actual$.crit)))
    expect_identical(attr(actual, "interval_info")$method, "bootstrap")
    args$interval_method <- "gaussian"
    gaussian <- do.call(fun, args)
    expect_equal(actual$.se, gaussian$.se)
  }
})

test_that("Gaussian deviations retain exact constraints without adding variance", {
  V <- matrix(1, 2, 2)
  z <- withr::with_seed(9, simultaneous_coefficient_deviations(V, 10000, 1))
  expect_equal(z[, 1], z[, 2], tolerance = 1e-12)
  expect_equal(cov(z), V, tolerance = .03)
  expect_identical(simultaneous_coefficient_deviations(matrix(0, 2, 2), 5, 1),
    matrix(0, 5, 2))
  near <- diag(c(1, -.Machine$double.eps))
  expect_true(all(simultaneous_coefficient_deviations(near, 10, 1)[, 2] == 0))
  bad <- diag(c(1, -.01))
  expect_error(simultaneous_coefficient_deviations(bad, 10, 1), "positive-semidefinite")
  asym <- V
  asym[1, 2] <- asym[1, 2] + .Machine$double.eps
  expect_silent(simultaneous_coefficient_deviations(asym, 10, 1))
  asym[1, 2] <- 1.1
  expect_error(simultaneous_coefficient_deviations(asym, 10, 1), "symmetric")
  PD <- matrix(c(2, .2, .2, 1), 2)
  actual <- withr::with_seed(19, simultaneous_coefficient_deviations(PD, 10, 1))
  expected <- withr::with_seed(19, mvnfast::rmvn(10, c(0, 0), PD, ncores = 1))
  expect_identical(actual, expected)
  band <- withr::with_seed(3, simultaneous_intervals(c(1, NA), c(1, NA),
    rbind(c(1, 0), c(NA, NA)), V, n_sim = 100))
  expect_true(is.finite(band$lower[1]))
  expect_true(is.na(band$lower[2]))
})

test_that("mixed constrained tensors support Gaussian simultaneous intervals", {
  m <- scasm_interval_fixture(TRUE)
  d <- data.frame(x = seq(.1, .9, length.out = 10), z = .5, off = 0)
  expect_lt(qr(m$Vp)$rank, length(coef(m)))
  expect_warning(actual <- smooth_estimates(m, data = d,
    interval = "simultaneous", n_sim = 100, seed = 9),
    "stored scasm bootstrap draws")
  expect_true(all(is.finite(actual$.lower_ci)))
  expect_true(all(actual$.lower_ci <= actual$.estimate))
  expect_true(all(actual$.upper_ci >= actual$.estimate))
  expect_identical(attr(actual, "interval_info")$method, "gaussian")
})

test_that("empty uncertainty designs do not recycle column means", {
  m <- scasm_interval_fixture()
  sm <- m$smooth[[1]]
  attr(sm, "nCons") <- 1L
  cols <- sm$first.para:sm$last.para
  X <- matrix(numeric(), 0, length(cols))
  expect_silent(actual <- smooth_uncertainty_design(sm, X, m, m$Vp,
    overall_uncertainty = TRUE))
  expect_identical(actual$X, X)
  expect_identical(actual$cols, cols)
})
