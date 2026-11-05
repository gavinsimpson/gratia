# These smooths were introduced in mgcv 1.9-5; older supported versions lack them.
skip_if_not_installed("mgcv", "1.9-5")

test_that("B-spline tensor estimates and uncertainty agree with mgcv", {
  models <- list(b_tensor_fixture(1L), b_tensor_fixture(), b_tensor_fixture(3L),
    b_tensor_fixture(penalty_order = 3.2), b_tensor_fixture(by = "numeric"),
    b_tensor_fixture(by = "factor"))
  for (model in models) {
    d <- model.frame(model)[1:8, ]
    reference <- predict(model, newdata = d, type = "terms", se.fit = TRUE)
    for (i in seq_along(model$smooth)) {
      sm <- model$smooth[[i]]
      expect_identical(smooth_type(sm), "B spline tensor")
      actual <- smooth_estimates(model, select = i, data = d,
        overall_uncertainty = FALSE)
      keep <- if (is_factor_by_smooth(sm)) d[[by_variable(sm)]] == by_level(sm) else
        rep(TRUE, nrow(d))
      expect_identical(unique(actual$.type), "B spline tensor")
      expect_equal(actual$.estimate, unname(reference$fit[keep, sm$label]),
        tolerance = 1e-8)
      expect_equal(actual$.se, unname(reference$se.fit[keep, sm$label]),
        tolerance = 1e-8)
    }
  }
})

test_that("B-spline tensor basis and penalties retain mgcv's matrices", {
  for (order in c(3, 3.2)) {
    model <- b_tensor_fixture(penalty_order = order)
    sm <- model$smooth[[1L]]
    d <- model.frame(model)[1:6, ]
    actual <- basis(model, data = d, unweighted = TRUE)
    X <- mgcv::PredictMat(sm, d)
    expect_equal(matrix(actual$.value, nrow = nrow(d), byrow = TRUE),
      unname(X))
    weighted <- basis(model, data = d)
    expect_equal(rowSums(matrix(weighted$.value, nrow = nrow(d), byrow = TRUE)),
      unname(predict(model, newdata = d, type = "terms")[, sm$label]))
    expect_identical(unique(actual$.type), "B spline tensor")
    for (rescale in c(FALSE, TRUE)) {
      p <- penalty(model, rescale = rescale)
      expect_identical(unique(p$.type), "B spline tensor")
      expect_length(unique(p$.penalty), length(sm$S))
      for (i in seq_along(sm$S)) {
        values <- p$.value[p$.penalty == unique(p$.penalty)[i]]
        expected <- sm$S[[i]] * if (rescale) sm$S.scale[i] else 1
        expect_equal(matrix(values, nrow = nrow(expected), byrow = TRUE), expected)
      }
    }
  }
  spec <- s(x0, x1, bs = "bt", k = 5)
  sm <- mgcv::smoothCon(spec, quick_eg1)[[1L]]
  b <- basis(spec, data = quick_eg1, at = quick_eg1[1:4, ])
  expect_equal(matrix(b$.value, nrow = 4L, byrow = TRUE),
    unname(mgcv::PredictMat(sm, quick_eg1[1:4, ])))
  expect_identical(unique(penalty(spec, data = quick_eg1)$.type), "B spline tensor")
})

test_that("B-spline tensor grids stay inside the fitted knot range", {
  model <- b_tensor_fixture(3L)
  sm <- model$smooth[[1L]]
  d <- smooth_data(model, id = 1L, n_2d = 4, n_3d = 2)
  # The upper x2 limit in this fixture rounds outwards to 1.
  expect_gt(round(max(model$model$x2), 3), max(model$model$x2))
  for (v in sm$term) {
    expect_equal(range(d[[v]]), range(model$model[[v]]))
  }
  actual <- smooth_estimates(model, n_2d = 4, n_3d = 2,
    overall_uncertainty = FALSE)
  expect_equal(nrow(actual), 32L)
  expect_equal(actual$.estimate,
    unname(predict(model, newdata = d, type = "terms")[, sm$label]))
  b <- basis(model, n_2d = 4, n_3d = 2, unweighted = TRUE)
  expect_equal(matrix(b$.value, nrow = nrow(d), byrow = TRUE),
    unname(mgcv::PredictMat(sm, d)))
  samples <- smooth_samples(model, n_grid = 3, n_draws = 2, seed = 42)
  expect_true(all(is.finite(samples$.value)))
})

test_that("B-spline tensor derivatives and coefficient draws agree with mgcv", {
  model <- b_tensor_fixture()
  d <- data.frame(x0 = seq(.2, .8, length.out = 6), x1 = .5)
  draws <- rbind(coef(model), coef(model) + .03)
  actual <- smooth_samples(model, data = d, method = "user", draws = draws)
  sm <- model$smooth[[1L]]
  j <- sm$first.para:sm$last.para
  expected <- mgcv::PredictMat(sm, d) %*% t(draws[, j])
  expect_equal(actual$.value[order(actual$.draw, actual$.row)], as.numeric(expected))
  for (order in 1:2) {
    h <- .001
    at <- function(delta, type) {
      nd <- d
      nd$x0 <- nd$x0 + delta
      predict(model, newdata = nd, type = type)
    }
    difference <- function(type) {
      if (order == 1L) (at(h / 2, type) - at(-h / 2, type)) / h else
        (at(h, type) - 2 * at(0, type) + at(-h, type)) / h^2
    }
    actual <- partial_derivatives(model, data = d, focal = "x0", order = order,
      type = "central", eps = h)
    X <- difference("lpmatrix")
    expect_equal(as.numeric(actual$.partial_deriv), as.numeric(difference("link")),
      tolerance = 1e-7)
    expect_equal(actual$.se, sqrt(rowSums((X %*% model$Vp) * X)), tolerance = 1e-7)
  }
})

test_that("B-spline tensor plots support curves, surfaces and factor-by smooths", {
  for (model in list(b_tensor_fixture(1L), b_tensor_fixture(),
      b_tensor_fixture(3L), b_tensor_fixture(by = "factor"))) {
    panels <- assemble(model, parametric = FALSE, n = 8, n_2d = 8, n_3d = 2)
    expect_length(panels, length(model$smooth))
    expect_silent(lapply(panels, ggplot2::ggplot_build))
    expect_plot_render(panels[[1L]], cran = length(model$smooth[[1L]]$term) == 1L)
    estimates <- smooth_estimates(model, n = 8, n_2d = 8, n_3d = 2)
    expect_silent(ggplot2::ggplot_build(draw(estimates)))
    b <- basis(model, n = 8, n_2d = 8, n_3d = 2)
    expect_silent(ggplot2::ggplot_build(draw(b)))
  }
})
