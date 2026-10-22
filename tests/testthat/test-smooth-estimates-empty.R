# Empty smooth evaluations retain their real column types without creating data.
smooth_empty_fixture <- local({
  fits <- list()
  function(kind = c("gam", "gjrm")) {
    kind <- match.arg(kind)
    if (is.null(fits[[kind]])) {
      s <- mgcv::s
      d <- withr::with_seed(511, {
        d <- data.frame(x = runif(100), z = runif(100),
          f = factor(rep(c("a", "b"), each = 50)))
        d$y1 <- sin(5 * d$x) + .2 * d$z + rnorm(100)
        d$y2 <- cos(4 * d$x) + .2 * d$y1 + rnorm(100)
        d
      })
      if (kind == "gam") {
        fit <- mgcv::gam(y1 ~ f + s(x, by = f, k = 4) + s(z, k = 4),
          data = d, method = "REML")
      } else {
        fit <- withr::with_seed(512, do.call(GJRM::gjrm, list(
          formula = list(y1 ~ f + s(x, by = f, k = 4) + s(z, k = 4),
            y2 ~ s(x, k = 4)), data = d, margins = c("N", "N"), model = "B")))
      }
      fits[[kind]] <<- list(model = fit, data = d)
    }
    fits[[kind]]
  }
})

expect_empty_smooth <- function(model, data, select, predictors, model_label = NULL) {
  flat <- smooth_estimates(model, select = select, data = data)
  nested <- smooth_estimates(model, select = select, data = data, unnest = FALSE)
  expect_s3_class(flat, "smooth_estimates")
  expect_equal(nrow(flat), 0L)
  expect_false("data" %in% names(flat))
  expect_true(all(c(".estimate", ".se", predictors) %in% names(flat)))
  expect_type(flat$.estimate, "double")
  expect_type(flat$.se, "double")
  expect_type(flat$x, "double")
  expect_s3_class(flat$f, "factor")
  expect_identical(levels(flat$f), levels(data$f))
  expect_equal(nrow(nested), 1L)
  expect_identical(nested$.smooth, smooths(model)[select])
  expect_identical(nested$.by, "f")
  expect_equal(nrow(nested$data[[1L]]), 0L)
  expect_type(nested$data[[1L]]$.estimate, "double")
  expect_type(nested$data[[1L]]$.se, "double")
  expect_identical(nested$data[[1L]]$f, data$f[0L])
  expect_equal(tidyr::unnest(nested, "data"), flat, ignore_attr = TRUE)
  expect_identical(names(attr(flat, "plot_variables")), smooths(model)[select])
  if (!is.null(model_label)) {
    expect_type(flat$.model, "character")
    expect_identical(nested$.model, model_label)
  }
}

test_that("GAM smooths absent from factor-by prediction rows retain their schema", {
  fixture <- smooth_empty_fixture()
  d <- fixture$data[1:20, ]
  expect_empty_smooth(fixture$model, d, select = 2L, predictors = c("x", "f"))
})

test_that("zero-row GAM inputs retain every selected smooth and predictor prototype", {
  fixture <- smooth_empty_fixture()
  d <- fixture$data[0, ]
  flat <- smooth_estimates(fixture$model, data = d)
  nested <- smooth_estimates(fixture$model, data = d, unnest = FALSE)
  expect_equal(nrow(flat), 0L)
  expect_true(all(c(".estimate", ".se", "x", "z", "f") %in% names(flat)))
  expect_false("data" %in% names(flat))
  expect_type(flat$.estimate, "double")
  expect_type(flat$.se, "double")
  expect_type(flat$x, "double")
  expect_type(flat$z, "double")
  expect_identical(flat$f, d$f)
  expect_identical(nested$.smooth, smooths(fixture$model))
  expect_identical(vapply(nested$data, nrow, integer(1)), rep(0L, 3))
  expect_equal(tidyr::unnest(nested, "data"), flat, ignore_attr = TRUE)
})

test_that("empty and nonempty GAM smooths preserve their data and column types", {
  fixture <- smooth_empty_fixture()
  d <- fixture$data[1:20, ]
  flat <- smooth_estimates(fixture$model, select = c(2, 3), data = d)
  nested <- smooth_estimates(fixture$model, select = c(2, 3), data = d,
    unnest = FALSE)
  reference <- smooth_estimates(fixture$model, select = 3, data = d)
  expect_identical(vapply(nested$data, nrow, integer(1)), c(0L, 20L))
  expect_identical(nested$.smooth, smooths(fixture$model)[2:3])
  expect_equal(nrow(flat), nrow(reference))
  expect_equal(flat$.estimate, reference$.estimate)
  expect_equal(flat$.se, reference$.se)
  expect_identical(flat$z, reference$z)
  expect_true(all(is.na(flat$x)))
  expect_s3_class(flat$f, "factor")
  expect_identical(levels(flat$f), levels(d$f))
  expect_true(all(is.na(flat$f)))
  expect_equal(tidyr::unnest(nested, "data"), flat, ignore_attr = TRUE)
})

test_that("joint GJRM smooths preserve empty factor-by evaluations and equation metadata", {
  skip_if_not_installed("GJRM", minimum_version = "0.2-6")
  fixture <- smooth_empty_fixture("gjrm")
  d <- fixture$data[1:20, ]
  expect_empty_smooth(fixture$model, d, select = 2L, predictors = c("x", "f"),
    model_label = "Response 1: Mean (mu)")
  nested <- smooth_estimates(fixture$model, data = fixture$data[0, ], unnest = FALSE)
  flat <- smooth_estimates(fixture$model, data = fixture$data[0, ])
  expect_identical(nested$.smooth, smooths(fixture$model))
  expect_identical(vapply(nested$data, nrow, integer(1)), rep(0L, 4))
  expect_true(all(c(".estimate", ".se", "x", "z", "f", ".model") %in% names(flat)))
  expect_equal(nrow(flat), 0L)
  expect_type(flat$.estimate, "double")
  expect_type(flat$.se, "double")
  expect_type(flat$x, "double")
  expect_type(flat$z, "double")
  expect_identical(flat$f, fixture$data$f[0L])
  expect_equal(tidyr::unnest(nested, "data"), flat, ignore_attr = TRUE)
})

test_that("joint GJRM binds empty and evaluated equations without inventing observations", {
  skip_if_not_installed("GJRM", minimum_version = "0.2-6")
  fixture <- smooth_empty_fixture("gjrm")
  d <- fixture$data[1:20, ]
  flat <- smooth_estimates(fixture$model, select = c(2, 4), data = d)
  nested <- smooth_estimates(fixture$model, select = c(2, 4), data = d,
    unnest = FALSE)
  reference <- smooth_estimates(fixture$model, select = 4, data = d)
  expect_identical(vapply(nested$data, nrow, integer(1)), c(0L, 20L))
  expect_identical(nested$.model, c("Response 1: Mean (mu)", "Response 2: Mean (mu)"))
  expect_equal(nrow(flat), nrow(reference))
  expect_identical(unique(flat$.model), "Response 2: Mean (mu)")
  expect_equal(flat$.estimate, reference$.estimate)
  expect_equal(flat$.se, reference$.se)
  expect_identical(flat$x, reference$x)
  expect_s3_class(flat$f, "factor")
  expect_identical(levels(flat$f), levels(d$f))
  expect_true(all(is.na(flat$f)))
  expect_equal(tidyr::unnest(nested, "data"), flat, ignore_attr = TRUE)
})

test_that("empty smooth intervals retain numeric bounds without drawing simulations", {
  withr::local_seed(513)
  fixture <- smooth_empty_fixture()
  d <- fixture$data[1:20, ]
  for (interval in c("confidence", "simultaneous")) {
    before <- .Random.seed
    flat <- smooth_estimates(fixture$model, select = 2, data = d,
      interval = interval, n_sim = 20)
    nested <- smooth_estimates(fixture$model, select = 2, data = d,
      interval = interval, n_sim = 20, unnest = FALSE)
    expect_identical(.Random.seed, before)
    expect_equal(nrow(flat), 0L)
    expect_equal(nrow(nested$data[[1L]]), 0L)
    for (column in c(".crit", ".lower_ci", ".upper_ci")) {
      expect_identical(flat[[column]], double())
      expect_identical(nested$data[[1L]][[column]], double())
    }
    expect_equal(tidyr::unnest(nested, "data"), flat, ignore_attr = TRUE)
  }
})

test_that("pointwise intervals preserve empty and nonempty smooth records together", {
  fixture <- smooth_empty_fixture()
  d <- fixture$data[1:20, ]
  flat <- smooth_estimates(fixture$model, select = c(2, 3), data = d,
    interval = "confidence")
  nested <- smooth_estimates(fixture$model, select = c(2, 3), data = d,
    interval = "confidence", unnest = FALSE)
  reference <- smooth_estimates(fixture$model, select = 3, data = d,
    interval = "confidence")
  expect_identical(vapply(nested$data, nrow, integer(1)), c(0L, 20L))
  for (column in c(".estimate", ".se", ".crit", ".lower_ci", ".upper_ci")) {
    expect_equal(flat[[column]], reference[[column]])
    expect_identical(nested$data[[1L]][[column]], double())
  }
  expect_equal(tidyr::unnest(nested, "data"), flat, ignore_attr = TRUE)
  expect_error(smooth_estimates(fixture$model, select = 2, data = d,
    interval = "simultaneous", simultaneous_method = "tube"),
    "Tube calibration needs evaluation points")
})
