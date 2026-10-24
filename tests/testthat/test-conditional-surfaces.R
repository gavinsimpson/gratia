test_that("grouping selects surfaces without changing existing conditions", {
  m <- conditional_surface_fixture()
  curve <- conditional_values(m, c("x", "z"))
  expect_equal(length(unique(curve$x)), 100L)
  expect_equal(length(unique(curve$z)), 5L)
  expect_equal(attr(curve, "channels")$colour, "z")
  expect_equal(conditional_values(m, "x"),
    conditional_values(m, "x", n_vals = 100))
  cv <- conditional_values(m, list(c("x", "z")))
  expect_equal(nrow(cv), 2500L)
  expect_equal(length(unique(cv$x)), 50L)
  expect_equal(length(unique(cv$z)), 50L)
  expect_equal(cv, conditional_values(m, list(c("x", "z")), n_vals = 50))
  cv <- conditional_values(m, list(c("z", "x"), "a", "b"), n_vals = 4)
  expect_equal(nrow(cv), 4L * 4L * 3L * 2L)
  expect_identical(attr(cv, "channels"),
    list(x = c("z", "x"), colour = NULL, f_col = "a", f_row = "b"))
  expect_identical(attr(cv[1:4, ], "channels"), attr(cv, "channels"))
  moments <- conditional_moments(m, "x", moments = "mean")
  expect_equal(length(unique(moments$x)), 100L)
  expect_error(conditional_moments(m, list(c("x", "z"))),
    "supported only")
})

test_that("nested axis specifications retain explicit values and helpers", {
  m <- conditional_surface_fixture()
  cv <- conditional_values(m,
    list(list(x = c(0.1, 0.3, 0.9), z = "quartile"), a = c("A", "C")),
    n_vals = 6)
  expect_equal(sort(unique(cv$x)), c(0.1, 0.3, 0.9))
  expect_equal(sort(unique(cv$z)), unname(g_quartile(m$model$z)))
  expect_equal(nrow(cv), 18L)
  cv <- conditional_values(m, list(list("z", x = c(0.2, 0.8))), n_vals = 7)
  expect_equal(nrow(cv), 14L)
  expect_equal(attr(cv, "channels")$x, c("z", "x"))
  expect_equal(cv$.fitted, as.numeric(predict(m, cv, type = "response")))
  expect_silent(ggplot2::ggplot_build(draw(cv)))
})

test_that("surface inputs fail clearly when axes or conditions are invalid", {
  m <- conditional_surface_fixture()
  for (n in list(0, -1, NA, Inf, 1.5, c(3, 4), "5")) {
    expect_error(conditional_values(m, list(c("x", "z")), n_vals = n), "n_vals")
    expect_error(conditional_differences(m, "a", list(c("x", "z")),
      n_vals = n), "n_vals")
  }
  for (condition in list(list(list("x")), list(c("x", "z", "u")),
      list("x", c("z", "u")))) {
    expect_error(conditional_values(m, condition), "first plotting position")
  }
  expect_error(conditional_values(m, list(c("x", "x"))), "must not be repeated")
  expect_error(conditional_values(m, list(c("x", "z"), "x")), "must not be repeated")
  expect_error(conditional_values(m, list(c("x", "a"))), "Surface axes")
  expect_error(conditional_differences(m, "a", list(c("x", "a"))), "Surface axes")
  expect_error(conditional_values(m, list(list(x = 0.5, z = c(0, 1)))), "Surface axes")
  expect_error(conditional_values(m, list(list(x = c(0, Inf), z = c(0, 1)))), "Surface axes")
  expect_error(conditional_values(m, list(c("x", "z"), "a", "b", "u")), "only 4")
})

test_that("conditional surfaces use shared flat geometry, facets and ranges", {
  m <- conditional_surface_fixture()
  cv <- conditional_values(m, list(c("x", "z"), "a", "b"), n_vals = 5)
  for (surface in c("raster", "tile")) {
    p <- draw(cv, surface = surface, contour = FALSE, xlab = "X", ylab = "Z",
      zlab = "Prediction")
    expect_s3_class(p$layers[[1]]$geom,
      if (surface == "raster") "GeomRaster" else "GeomTile")
    expect_equal(p$labels$x, "X")
    expect_equal(p$labels$y, "Z")
    expect_equal(p$guides$guides$fill$params$title, "Prediction")
    expect_equal(p$data$.estimate, cv$.fitted)
    expect_null(p$labels$caption)
    expect_silent(built <- ggplot2::ggplot_build(p))
    expect_equal(nrow(built$layout$layout), 6L)
    expect_equal(built$plot$scales$get_scales("fill")$get_limits(), range(cv$.fitted))
  }
  irregular <- conditional_values(m, list(list(x = c(0, 0.2, 1), z = c(0, 1))))
  for (surface in c("raster", "tile")) {
    p <- draw(irregular, surface = surface, contour = FALSE)
    expect_s3_class(p$layers[[1]]$geom, "GeomRect")
    expect_silent(built <- ggplot2::ggplot_build(p))
    expect_equal(sort(unique(built$data[[1]]$xmin)), c(-0.1, 0.1, 0.6))
    expect_equal(sort(unique(built$data[[1]]$xmax)), c(0.1, 0.6, 1.4))
  }
  constant <- cv
  constant$.fitted <- 1
  expect_silent(ggplot2::ggplot_build(draw(constant)))
  expect_false(any(vapply(draw(constant)$layers, function(x)
    inherits(x$geom, "GeomContour"), logical(1L))))
})

test_that("surface differences retain comparisons and analytic predictions", {
  m <- conditional_surface_fixture(stats::poisson())
  condition <- list(a = c("A", "C"), c("x", "z"), "b")
  for (scale in c("link", "response")) {
    cd <- conditional_differences(m, "a", condition, n_vals = 4, scale = scale,
      exclude = "s(u)")
    ep <- conditional_difference_endpoints(cd, m)
    expect_equal(cd$.diff, as.numeric(
      predict(m, ep$a, type = scale, exclude = "s(u)") -
      predict(m, ep$b, type = scale, exclude = "s(u)")))
    expect_equal(nrow(cd), 32L)
    expect_identical(attr(cd, "channels")$x, c("x", "z"))
    expect_null(attr(cd, "channels")$colour)
    expect_identical(attr(cd[1:3, ], "channels"), attr(cd, "channels"))
    p <- draw(cd, contour = FALSE)
    expect_equal(p$labels$y, "z")
    expect_false(any(vapply(p$layers, function(x)
      inherits(x$geom, "GeomHline"), logical(1L))))
    expect_silent(built <- ggplot2::ggplot_build(p))
    expect_equal(nrow(built$layout$layout), 2L)
    expect_equal(built$plot$scales$get_scales("fill")$get_limits(),
      c(-1, 1) * max(abs(cd$.diff)))
  }
  default <- conditional_differences(m, "a", list(c("x", "z")))
  expect_equal(nrow(default), 3L * 2500L)
  expect_equal(default, conditional_differences(m, "a", list(c("x", "z")),
    n_vals = 50))
  expect_equal(conditional_differences(m, "a", "x"),
    conditional_differences(m, "a", "x", n_vals = 100))
})

test_that("surface comparison intervals use the complete two-axis grid", {
  m <- conditional_surface_fixture()
  condition <- list(c("x", "z"), "b")
  for (uncertainty in c("delta", "simulation")) {
    cd <- conditional_differences(m, "a", condition, n_vals = 3,
      uncertainty = uncertainty, interval = "simultaneous", n_sim = 30, seed = 6)
    expect_equal(nrow(cd), 54L)
    expect_true(all(is.finite(cd$.se)))
    expect_equal(cd$.lower_ci, cd$.diff - cd$.crit * cd$.se)
    expect_equal(cd$.upper_ci, cd$.diff + cd$.crit * cd$.se)
    for (i in unique(cd$.contrast)) {
      expect_equal(length(unique(cd$.crit[cd$.contrast == i])), 1L)
    }
  }
})

test_that("3D conditional surfaces retain separate meshes and interval heights", {
  skip_if_not_installed("ggcube", "0.3.0")
  m <- conditional_surface_fixture()
  cv <- conditional_values(m, list(c("x", "z"), "a"), n_vals = 4)
  cd <- conditional_differences(m, "a", list(c("x", "z"), "b"), n_vals = 4)
  for (object in list(cv, cd)) {
    p <- draw(object, surface = "3d", ci_3d = TRUE, zlab = "Value",
      contour = FALSE, surface_3d = list(yaw = 35))
    scene <- p$layers[[1]]$data
    estimate <- scene[scene$.surface_role == "estimate", ]
    lower <- scene[scene$.surface_role == "lower", ]
    upper <- scene[scene$.surface_role == "upper", ]
    expect_gt(nrow(estimate), 0)
    expect_equal(nrow(lower), nrow(estimate))
    expect_equal(lower$.surface_z, estimate$.lower_ci)
    expect_equal(upper$.surface_z, estimate$.upper_ci)
    expect_equal(p$labels$x, "x")
    expect_equal(p$labels$y, "z")
    expect_equal(p$labels$z, "Value")
    expect_silent(built <- ggplot2::ggplot_build(p))
    expect_equal(nrow(built$layout$layout), if (inherits(object,
      "conditional_differences")) 6L else 3L)
    without <- draw(object, surface = "3d", contour = FALSE)
    expect_equal(ggplot2::ggplot_build(without)$plot$scales$get_scales("fill")$get_limits(),
      built$plot$scales$get_scales("fill")$get_limits())
  }
})

test_that("multiple distribution parameters have separate surface panels", {
  m <- conditional_surface_fixture()
  m <- mgcv::gam(list(y ~ x + z, ~ x), data = m$model, family = mgcv::gaulss())
  cv <- conditional_values(m, list(c("x", "z")), n_vals = 3)
  expect_gt(dplyr::n_distinct(cv$.parameter), 1L)
  expect_silent(built <- ggplot2::ggplot_build(draw(cv)))
  expect_equal(nrow(built$layout$layout), dplyr::n_distinct(cv$.parameter))
  if (requireNamespace("ggcube", quietly = TRUE)) {
    expect_silent(built <- ggplot2::ggplot_build(draw(cv, surface = "3d")))
    expect_equal(nrow(built$layout$layout), dplyr::n_distinct(cv$.parameter))
  }
})

test_that("observed combinations and nearby numeric facets stay separate", {
  m <- conditional_surface_fixture()
  d <- m$model[!(m$model$a == "C" & m$model$b == "high"), ]
  m <- mgcv::gam(y ~ a + b + s(x, z, by = a, k = 8) + s(u, k = 4),
    data = d, method = "REML")
  cv <- conditional_values(m, list(c("x", "z"), "a", "b"), n_vals = 3,
    complete = FALSE)
  expect_equal(nrow(cv), 45L)
  cd <- conditional_differences(m, "a", list(c("x", "z"), "b"),
    n_vals = 3, complete = FALSE)
  expect_equal(nrow(cd), 36L)
  for (surface in c("raster", "tile", if (requireNamespace("ggcube",
      quietly = TRUE)) "3d")) {
    p <- draw(cd, surface = surface, contour = FALSE)
    expect_silent(built <- ggplot2::ggplot_build(p))
    expect_equal(nrow(built$layout$layout), 4L)
    nearby <- conditional_values(m,
      list(c("x", "z"), u = c(0.10001, 0.10002)), n_vals = 3)
    expect_silent(built <- ggplot2::ggplot_build(draw(nearby,
      surface = surface, contour = FALSE)))
    expect_equal(nrow(built$layout$layout), 2L)
  }
})
