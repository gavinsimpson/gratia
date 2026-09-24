test_that("curve layers preserve interval mappings and surrounding layers", {
  d <- data.frame(x = rep(1:3, 2), y = 1:6, lo = 0:5, hi = 2:7,
    f = factor(rep(c("a", "b"), each = 3)))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = f, group = f)) +
    ggplot2::geom_hline(yintercept = 0)
  p <- add_curve_interval(p, lower_var = "lo", upper_var = "hi",
    ribbon_mapping = ggplot2::aes(fill = f, colour = NULL, y = NULL),
    ribbon_alpha = 0.4) + ggplot2::geom_rug()
  expect_equal(unname(vapply(p$layers, function(l) class(l$geom)[1L], character(1))),
    c("GeomHline", "GeomRibbon", "GeomLine", "GeomRug"))
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$data[[2]]$ymin, d$lo)
  expect_equal(b$data[[2]]$ymax, d$hi)
  expect_true(all(is.na(b$data[[2]]$colour)))
  expect_equal(unique(b$data[[2]]$alpha), 0.4)
  expect_equal(length(unique(b$data[[3]]$group)), 2L)
  expect_equal(b$data[[3]]$y, d$y)
  line <- add_curve_interval(ggplot2::ggplot(d, ggplot2::aes(x, y)),
    line_colour = "red", line_alpha = 0.3)
  expect_length(line$layers, 1L)
  expect_equal(line$layers[[1]]$aes_params$colour, "red")
  expect_equal(line$layers[[1]]$aes_params$alpha, 0.3)
})

test_that("layout defaults preserve explicit dimensions and zero-panel behaviour", {
  for (n in c(1, 2, 4, 5, 9, 10)) {
    expect_equal(prepare_plot_layout(n),
      list(ncol = ceiling(sqrt(n)), nrow = ceiling(n / ceiling(sqrt(n)))))
  }
  expect_equal(prepare_plot_layout(7, ncol = 2), list(ncol = 2, nrow = NULL))
  expect_equal(prepare_plot_layout(7, nrow = 2), list(ncol = NULL, nrow = 2))
  expect_equal(prepare_plot_layout(7, ncol = 4, nrow = 2),
    list(ncol = 4, nrow = 2))
  expect_equal(prepare_plot_layout(0)$ncol, 0)
  expect_true(is.nan(prepare_plot_layout(0)$nrow))
})

test_that("derivative panels retain selection, interval limits and patchwork options", {
  d <- tibble::tibble(.smooth = rep(c("s(x)", "s(z)"), each = 5),
    .fs = NA_character_, x = rep(1:5, 2), z = rep(2:6, 2),
    .derivative = c(1:5, 11:15), .lower_ci = .derivative - 1,
    .upper_ci = .derivative + 1)
  class(d) <- c("derivatives", class(d))
  p <- draw(d, select = c(2, 1), scales = "fixed", nrow = 1,
    angle = 35, widths = c(2, 1))
  expect_equal(p[[1]]$labels$title, "s(x)")
  expect_equal(p[[2]]$labels$title, "s(z)")
  expect_equal(draw(d, select = 2)[[1]]$labels$title, "s(z)")
  expect_equal(p[[1]]$scales$get_scales("y")$limits, c(0, 16))
  expect_equal(p$patches$layout$nrow, 1)
  expect_equal(p$patches$layout$widths, c(2, 1))
  expect_equal(p[[1]]$guides$guides$x$params$angle, 35)
  expect_silent(ggplot2::ggplot_build(p[[1]]))
  pd <- d[d$.smooth == "s(x)", ]
  pd$.focal <- "x"
  pd$.partial_deriv <- pd$.derivative
  class(pd) <- c("partial_derivatives", "tbl_df", "tbl", "data.frame")
  q <- draw(pd, alpha = 0.6)[[1]]
  expect_equal(q$labels$y, "Partial derivative with respect to x")
  expect_equal(q$layers[[1]]$aes_params$alpha, 0.6)
  expect_equal(ggplot2::ggplot_build(q)$data[[2]]$y, pd$.partial_deriv)
})

test_that("derivative factor interactions retain grouping without intervals", {
  d <- tibble::as_tibble(expand.grid(x = 1:3, f = factor(c("a", "b")),
    g = factor(c("c", "d"))))
  d$.smooth <- "s(x,f,g)"
  d$.fs <- "f:g"
  d$.derivative <- seq_len(nrow(d))
  d$.lower_ci <- d$.derivative - 1
  d$.upper_ci <- d$.derivative + 1
  class(d) <- c("derivatives", class(d))
  p <- draw(d)[[1]]
  q <- draw(d, differentiate_factor_smooths = FALSE)[[1]]
  expect_length(p$layers, 1)
  expect_length(q$layers, 1)
  expect_equal(p$labels$x, "x")
  expect_equal(p$labels$colour, "f:g")
  expect_null(q$mapping$colour)
  expect_equal(length(unique(ggplot2::ggplot_build(p)$data[[1]]$group)), 4L)
  expect_equal(length(unique(ggplot2::ggplot_build(q)$data[[1]]$group)), 4L)
})

reuse_surface_data <- function() {
  d <- tibble::as_tibble(expand.grid(x = seq(0, 1, length.out = 5),
    z = seq(0, 2, length.out = 5), .draw = 1:2))
  d$.value <- sin(3 * d$x) + d$z - 1
  d$.diff <- d$.value
  d$.bf <- d$.draw
  d$.smooth <- d$.term <- "s(x,z)"
  d$.type <- "TPRS"
  d$.by <- NA_character_
  d$.level_1 <- "a"
  d$.level_2 <- "b"
  d
}

test_that("other surfaces preserve facets, guides, grouping and theme inheritance", {
  d <- reuse_surface_data()
  p <- draw_2d_posterior_smooths(d, contour = TRUE)
  q <- draw_2d_difference(d[d$.draw == 1, ], c("x", "z"), contour = TRUE)
  b <- plot_bivariate_basis(d, legend = FALSE, contour = TRUE, angle = 40)
  for (plot in list(p, q, b)) {
    expect_s3_class(plot$layers[[1]]$geom, "GeomRaster")
    expect_s3_class(plot$layers[[2]]$geom, "GeomContour")
    expect_null(plot$theme$legend.position)
    expect_silent(ggplot2::ggplot_build(plot))
  }
  expect_equal(p$guides$guides$fill$params$title, "Partial effect")
  expect_equal(q$guides$guides$fill$params$title, "Difference")
  expect_false(p$layers[[2]]$geom_params$na.rm)
  expect_false(q$layers[[2]]$geom_params$na.rm)
  expect_true(b$layers[[2]]$geom_params$na.rm)
  expect_equal(p$coordinates$ratio, 1)
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
  expect_equal(nrow(ggplot2::ggplot_build(b)$layout$layout), 2L)
  expect_equal(b$guides$guides$fill, "none")
  expect_equal(b$guides$guides$x$params$angle, 40)
  expect_true(b$guides$guides$x$params$check.overlap)
  expect_equal(b$labels$fill, "value")
  # These drawing families keep their historical range handling.
  d$.diff[1] <- NA_real_
  q <- draw_2d_difference(d[d$.draw == 1, ], c("x", "z"))
  expect_true(all(is.na(q$layers[[2]]$data$fill)))
})

test_that("basis labels and factor facets retain family-specific defaults", {
  d <- reuse_surface_data()
  attr(d, "smooth_object") <- "stored smooth call"
  labels <- prepare_basis_labels(d, "x")
  expect_equal(labels$title, "stored smooth call")
  expect_equal(labels$y, "Value")
  expect_equal(labels$colour, "Basis\nfunction")
  d$.by <- "f"
  d$f <- factor(rep(c("a", "b"), each = 25))
  p <- plot_bivariate_basis(d, legend = TRUE, contour = FALSE,
    labeller = ggplot2::label_value, title = "Custom", caption = "Caption")
  expect_equal(p$labels$title, "Custom")
  expect_equal(p$labels$caption, "Caption")
  expect_null(p$guides$guides$fill)
  expect_true("f" %in% names(ggplot2::ggplot_build(p)$layout$layout))
})

test_that("spherical rendering preserves axes, captions and ordered-by labels", {
  skip_if_not_installed("sf")
  d <- reuse_surface_data()[1:25, ]
  d$.estimate <- d$.value
  d$.se <- rep(0.2, nrow(d))
  d$.lower_ci <- d$.estimate - 0.4
  d$.upper_ci <- d$.estimate + 0.4
  class(d) <- c("mgcv_smooth", class(d))
  p <- plot_smooth.sos(d, variables = c("x", "z"), contour = FALSE,
    crs = 4326, constant = 1, fun = exp)
  q <- plot_smooth.sos(d, variables = c("x", "z"), contour = FALSE,
    crs = 4326, xlab = NULL, ylab = NULL, show = "se", ylim = c(-10, 10))
  expect_equal(p$labels$x, "z")
  expect_equal(p$labels$y, "x")
  expect_null(q$labels$x)
  expect_null(q$labels$y)
  expect_equal(p$data$.estimate, exp(d$.estimate + 1))
  expect_equal(q$layers[[2]]$data$fill, c(0.2, 0.2))
  expect_s3_class(p$coordinates, "CoordSf")
  expect_s3_class(p$layers[[1]]$geom, "GeomTile")
  expect_silent(ggplot2::ggplot_build(p))
  d$.by <- "f"
  d$f <- ordered(rep("a", nrow(d)))
  d$.smooth <- "s(x,z):fa"
  p <- plot_smooth.sos(d, variables = c("x", "z"), contour = FALSE,
    crs = 4326, caption = "ignored")
  expect_equal(p$labels$title, "s(x,z)")
  expect_equal(p$labels$subtitle, "By: f")
  expect_null(p$labels$caption)
})
