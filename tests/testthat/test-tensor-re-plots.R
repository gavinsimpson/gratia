# Numeric correctness is checked against mgcv, independently of plot assembly.
test_that("all tensor constructors display numeric axes before random factors", {
  withr::local_pdf(NULL)
  for (m in tensor_re_models) {
    panels <- assemble(m, select = 2, n_2d = 5, rug = FALSE, contour = FALSE)
    expect_length(panels, 1L)
    p <- panels[[1L]]
    expect_equal(nrow(p$data), 5L * 5L * 4L)
    nd <- p$data
    nd$u <- median(tensor_re_data$u)
    ref <- predict(m, newdata = nd, type = "terms", se.fit = TRUE)
    # overall_uncertainty adds intercept variance; disable it for term oracle.
    sm <- smooth_estimates(m, select = 2, data = p$data, overall_uncertainty = FALSE)
    expect_equal(sm$.estimate, unname(ref$fit[, 2]))
    expect_equal(sm$.se, unname(ref$se.fit[, 2]))
    b <- ggplot2::ggplot_build(p)
    expect_equal(as.character(b$layout$layout$f), levels(tensor_re_data$f))
    expect_equal(as.integer(table(b$data[[1]]$PANEL)), rep(25L, 4))
    expect_equal(sort(unique(b$data[[1]]$x)), sort(unique(p$data$x)))
    expect_equal(sort(unique(b$data[[1]]$y)), sort(unique(p$data$z)))
    expect_silent(ggplot2::ggplotGrob(p))
    precomputed <- draw(sm, contour = FALSE)[[1]]
    expect_equal(precomputed$data$.estimate, sm$.estimate)
    expect_silent(ggplot2::ggplotGrob(precomputed))
  }
})

test_that("named level selection preserves fitted order and ordered classes", {
  withr::local_pdf(NULL)
  p <- assemble(tensor_re_four, n_2d = 4, rug = TRUE, contour = FALSE,
    re_levels = list(f = c("b", "c"), g = "A"))[[1]]
  expect_equal(nrow(p$data), 32L)
  expect_equal(levels(p$data$f), levels(tensor_re_data$f))
  expect_true(is.ordered(p$data$g))
  b <- ggplot2::ggplot_build(p)
  expect_equal(as.character(b$layout$layout$f), c("c", "b"))
  expect_equal(as.character(b$layout$layout$g), c("A", "A"))
  expect_silent(ggplot2::ggplotGrob(p))
  sm <- smooth_estimates(tensor_re_four, n_2d = 4)
  subset <- sm[sm$f %in% c("c", "b"), ]
  expect_equal(attr(subset, "tensor_plot_info"), attr(sm, "tensor_plot_info"))
  saved <- unserialize(serialize(subset, NULL))
  expect_equal(draw(saved, re_levels = list(g = "A"), contour = FALSE)[[1]]$data$.estimate,
    p$data$.estimate)
})

test_that("joint limits prevent evaluation and never select arbitrary levels", {
  # The mock makes reaching any prediction matrix an immediate failure.
  testthat::local_mocked_bindings(smooth_predict_matrix = function(...) {
    stop("prediction should not be reached")
  })
  expect_message(p <- assemble(tensor_re_four, max_re_combinations = 7,
    rug = FALSE), "8 random-effect conditioning combinations")
  expect_null(p)
  expect_message(p <- assemble(tensor_re_slices, n_4d = 5,
    rug = FALSE), "20 random-effect conditioning combinations")
  expect_null(p)
})

test_that("limits have exact boundaries and explicit overrides", {
  for (limit in c(16, 17, Inf)) {
    expect_silent(p <- assemble(tensor_re_slices, n_2d = 3, n_4d = 4,
      max_re_combinations = limit, rug = FALSE, contour = FALSE))
    expect_equal(nrow(ggplot2::ggplot_build(p[[1]])$layout$layout), 16L)
  }
  expect_silent(p <- assemble(tensor_re_slices, n_2d = 3, n_4d = 5,
    max_re_combinations = 20, rug = FALSE, contour = FALSE))
  expect_equal(nrow(p[[1]]$data), 180L)
  # Direct evaluation has no plotting cap, including for later drawing.
  sm <- smooth_estimates(tensor_re_slices, n_2d = 3, n_4d = 5)
  expect_equal(nrow(sm), 300L)
  expect_message(p <- draw(sm, contour = FALSE), "20 random-effect conditioning combinations")
  expect_null(p)
  expect_silent(draw(sm, max_re_combinations = Inf, contour = FALSE))
})

test_that("supplied slices count actual conditioning combinations", {
  d <- data_slice(tensor_re_slices, x = evenly(x, n = 3),
    z = evenly(z, n = 3), u = 0.5, f = level(f, c("c", "a")))
  expect_silent(p <- assemble(tensor_re_slices, data = d,
    max_re_combinations = 2, rug = TRUE, contour = FALSE))
  expect_equal(nrow(p[[1]]$data), 18L)
  # Correlated factors contain two joint states rather than a 2 x 2 grid.
  d <- expand.grid(x = seq(0.1, 1, length.out = 3), z = seq(0, 1, length.out = 3),
    row = 1:2)
  d$f <- factor(c("c", "a")[d$row], levels = levels(tensor_re_data$f))
  d$g <- ordered(c("B", "A")[d$row], levels = levels(tensor_re_data$g))
  expect_silent(p <- assemble(tensor_re_four, data = d,
    max_re_combinations = 2, rug = FALSE, contour = FALSE))
  expect_equal(nrow(p[[1]]$data), 18L)
})

test_that("invalid level controls fail clearly", {
  m <- tensor_re_models$ti
  for (limit in list(0, -1, NA_real_, NaN, -Inf, 1.5, c(1, 2), "16")) {
    expect_error(assemble(m, max_re_combinations = limit), "positive integer or Inf")
  }
  for (levels in list(list("a"), list(f = character()), list(f = c("a", "a")),
    list(f = NA_character_))) {
    expect_error(assemble(m, re_levels = levels), "named list")
  }
  expect_error(assemble(m, re_levels = list(x = "a")), "not a factor")
  expect_error(assemble(m, re_levels = list(missing = "a")), "not a factor")
  expect_error(assemble(m, re_levels = list(f = "unknown")), "Unknown")
  d <- tensor_re_data[tensor_re_data$f == "c", ]
  expect_error(assemble(m, data = d, re_levels = list(f = "a")), "No tensor evaluation rows")
  sm <- smooth_estimates(m, select = 2, n_2d = 3)
  expect_error(draw(sm, re_levels = list(x = "a")), "not a factor")
  expect_error(draw(sm, re_levels = list(f = "unknown")), "Unknown")
})

test_that("multi-factor curves and numeric random slopes use their proper axes", {
  withr::local_pdf(NULL)
  p <- assemble(tensor_re_curves, n = 7, rug = TRUE)[[1]]
  b <- ggplot2::ggplot_build(p)
  expect_equal(as.integer(table(b$data[[1]]$group)), rep(7L, 8))
  expect_equal(nrow(b$layout$layout), 8L)
  selected <- assemble(tensor_re_curves, n = 7, rug = TRUE,
    re_levels = list(f = "c"))[[1]]
  expect_equal(nrow(ggplot2::ggplot_build(selected)$layout$layout), 2L)
  expect_silent(ggplot2::ggplotGrob(selected))
  expect_silent(ggplot2::ggplotGrob(p))
  p <- assemble(tensor_re_numeric, n_2d = 3, n_3d = 4,
    max_re_combinations = 1, rug = FALSE, contour = FALSE)[[1]]
  expect_equal(nrow(p$data), 36L)
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("tensor surfaces retain flat and raw-coordinate rendering", {
  withr::local_pdf(NULL)
  for (surface in c("raster", "tile")) {
    p <- assemble(tensor_re_raw, surface = surface, n_2d = 4,
      back_transform = TRUE, re_levels = list(f = "c"), rug = TRUE,
      contour = FALSE, envir = environment())[[1]]
    expect_true("x" %in% names(p$data))
    expect_equal(sort(unique(p$data$x)), seq(min(tensor_re_data$x),
      max(tensor_re_data$x), length.out = 4))
    expect_silent(ggplot2::ggplotGrob(p))
  }
})

test_that("random-effect surfaces use the optional three-dimensional renderer", {
  skip_if_not_installed("ggcube")
  withr::local_pdf(NULL)
  p <- assemble(tensor_re_models$ti, select = 2, n_2d = 4,
    surface = "3d", rug = FALSE, contour = FALSE)[[1]]
  expect_s3_class(p$coordinates, "Coord3D")
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 4L)
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("categorical tensors display point ranges without interpolation", {
  withr::local_pdf(NULL)
  p <- assemble(tensor_re_categories, rug = FALSE)[[1]]
  expect_s3_class(p$layers[[1]]$geom, "GeomPointrange")
  expect_equal(nrow(p$data), 12L)
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$layout$layout), 4L)
  expect_equal(b$data[[1]]$ymin, p$data$.lower_ci)
  expect_equal(b$data[[1]]$ymax, p$data$.upper_ci)
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("level selection follows expression evaluation for supplied data", {
  m <- tensor_re_four
  # mgcv's re constructor cannot itself fit factor(g) in 1.9-4. A metadata
  # copy tests our preparation/filtering boundary without constructing or
  # predicting that unsupported model.
  m$smooth[[1]]$term[4] <- "factor(g)"
  attr(m, "gratia.tensor_plot_specs") <- list(list(
    variables = c("x", "z", "f", "factor(g)"), levels = list(`factor(g)` = "A")))
  d <- tensor_re_data[c(1:10, 111:120), ]
  prepared <- process_user_data_for_eval(d, m, n = 4, n_3d = 2,
    n_4d = 2, id = 1)
  expect_equal(nrow(prepared), 10L)
  expect_true(all(as.character(prepared[["factor(g)"]]) == "A"))
  expect_equal(prepared$x, d$x[d$g == "A"])
})

test_that("seventeen levels require a deliberate selection or override", {
  expect_message(p <- assemble(tensor_re_many, n = 4, rug = FALSE),
    "17 random-effect conditioning combinations")
  expect_null(p)
  expect_silent(p <- assemble(tensor_re_many, n = 4, rug = FALSE,
    re_levels = list(f = levels(tensor_re_many_data$f)[1:16])))
  expect_equal(nrow(p[[1]]$data), 64L)
  expect_silent(p <- assemble(tensor_re_many, n = 4, rug = FALSE,
    max_re_combinations = 17))
  expect_equal(nrow(p[[1]]$data), 68L)
  sm <- smooth_estimates(tensor_re_many, n = 4)
  expect_equal(nrow(sm), 68L)
  expect_message(draw(sm), "17 random-effect conditioning combinations")
  expect_silent(p <- draw(sm, re_levels = list(f = "level17")))
  expect_equal(nrow(p[[1]]$data), 4L)
})

test_that("oversized terms are skipped before grid allocation without dropping other terms", {
  # With only this tensor selected, even constructing a grid is forbidden.
  testthat::local_mocked_bindings(smooth_data = function(...) {
    stop("grid allocation should not be reached")
  })
  expect_message(p <- assemble(tensor_re_many, n = 4, rug = FALSE),
    "17 random-effect conditioning combinations")
  expect_null(p)
})

test_that("skipping a tensor preserves other requested smooths and the RNG state", {
  withr::local_seed(1694)
  seed <- .Random.seed
  expect_message(p <- assemble(tensor_re_models$ti, n = 4,
    max_re_combinations = 3, rug = FALSE, contour = FALSE),
    "4 random-effect conditioning combinations")
  expect_length(p, 1L)
  expect_equal(unique(p[[1]]$data$.smooth), "s(u)")
  expect_identical(.Random.seed, seed)
})

test_that("numeric random-effect margins retain the two-dimensional marginal focus", {
  p <- assemble(tensor_re_numeric_surface, n_2d = 3, n_3d = 2,
    max_re_combinations = 1, rug = FALSE, contour = FALSE)[[1]]
  expect_equal(nrow(p$data), 18L)
  b <- ggplot2::ggplot_build(p)
  expect_equal(sort(unique(b$data[[1]]$x)), sort(unique(p$data$x)))
  expect_equal(sort(unique(b$data[[1]]$y)), sort(unique(p$data$z)))
  expect_equal(nrow(b$layout$layout), 2L)
})

test_that("categorical tuples remain distinct when labels contain separators", {
  withr::local_pdf(NULL)
  sm <- smooth_estimates(tensor_re_curves, n = 4)
  levels(sm$f) <- c("a:b", "a", "other1", "other2")
  levels(sm$g) <- c("c", "b:c")
  sm <- sm[(sm$f == "a:b" & sm$g == "c") | (sm$f == "a" & sm$g == "b:c"), ]
  p <- draw(sm)[[1]]
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$layout$layout), 2L)
  expect_equal(length(unique(p$data$.re_group)), 2L)
  expect_equal(as.integer(table(b$data[[1]]$group)), c(4L, 4L))
  expect_silent(ggplot2::ggplotGrob(p))

  sm <- smooth_estimates(tensor_re_categories)
  levels(sm$g) <- c("a:b", "a")
  levels(sm$h) <- c("c", "b:c")
  sm <- sm[(sm$g == "a:b" & sm$h == "c") | (sm$g == "a" & sm$h == "b:c"), ]
  p <- draw(sm)[[1]]
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$layout$layout), 2L)
  expect_equal(as.integer(table(b$data[[1]]$PANEL)), c(3L, 3L))
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("curve rugs do not create unrequested joint-factor facets", {
  withr::local_pdf(NULL)
  d <- expand.grid(x = seq(0.1, 1, length.out = 4), row = 1:2)
  d$f <- factor(c("c", "a")[d$row], levels = levels(tensor_re_data$f))
  d$g <- ordered(c("B", "A")[d$row], levels = levels(tensor_re_data$g))
  p <- assemble(tensor_re_curves, data = d, rug = TRUE,
    max_re_combinations = 2)[[1]]
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$layout$layout), 2L)
  expect_silent(ggplot2::ggplotGrob(p))
})
