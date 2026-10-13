test_that("pooled histograms draw matching opaque outlines above faint fills", {
  for (model in list(gfam_model, gfam_missing_model, gfam_gaussian_model)) {
    ordinary <- residuals_hist_plot(model, seed = 42, n_bins = 8)
    scalable <- scalable_residuals_hist_plot(model, seed = 42, n_bins = 8)
    for (p in list(ordinary, scalable)) {
      built <- ggplot_build(p)
      fill <- built$data[[1L]]
      line <- built$data[[2L]]
      expect_true(all(fill$alpha == .15))
      expect_true(all(is.na(fill$colour)))
      expect_true(all(line$alpha == 1))
      expect_equal(unique(line$colour), unique(fill$fill))
      expect_equal(nrow(built$layout$layout), 1L)
      expect_identical(p$labels$fill, p$labels$colour)
    }
    # The extra outline layer does not change bin counts or normalization.
    bars <- ggplot_build(ordinary)$data[[1L]]
    expect_equal(scalable$data$count, bars$count)
    expect_equal(scalable$data$value, bars$density)
    expect_equal(sum(scalable$data$count), diagnostic_data(model, seed = 42)$n_obs)
    line <- scalable$layers[[2L]]$data
    for (label in unique(scalable$data$.family)) {
      bins <- scalable$data[scalable$data$.family == label, ]
      path <- line[line$.family == label, ]
      expect_equal(nrow(path), 2L * nrow(bins) + 2L)
      expect_equal(path$x, c(bins$xmin[1L], as.vector(rbind(bins$xmin, bins$xmax)),
        tail(bins$xmax, 1L)))
      expect_equal(path$y, c(0, rep(bins$value, each = 2L), 0))
      # Every segment is horizontal or vertical; no diagonal interpolation.
      expect_true(all(diff(path$x) == 0 | diff(path$y) == 0))
    }
    expect_true(inherits(ordinary$layers[[2L]]$geom, "GeomStep"))
    expect_identical(ordinary$layers[[2L]]$geom_params$direction, "mid")
    outline <- ggplot_build(ordinary)$data[[2L]]
    # Padding closes the upper outline to zero at both ends.
    for (group in unique(bars$group)) {
      values <- outline[outline$group == group, ]
      expected <- bars[bars$group == group, ]
      expect_equal(values$density, c(0, expected$density, 0))
    }
  }
})

test_that("histogram outlines retain empty bins, widths and indexed components", {
  bins <- tibble::tibble(xmin = c(1, 0, 3, 0), xmax = c(3, 1, 4, 1),
    value = c(0, .4, .6, 1), .family = factor(c("a", "a", "a", "b")))
  p <- diagnostic_histogram_outline(bins)
  expect_equal(p$x[p$.family == "a"], c(0, 0, 1, 1, 3, 3, 4, 4))
  expect_equal(p$y[p$.family == "a"], c(0, .4, .4, 0, 0, .6, .6, 0))
  expect_equal(p$x[p$.family == "b"], c(0, 0, 1, 1))
  expect_equal(p$y[p$.family == "b"], c(0, 1, 1, 0))
  expect_identical(levels(p$.family), c("a", "b"))
})

test_that("faceted and single-family histograms retain their existing style", {
  for (fun in list(residuals_hist_plot, scalable_residuals_hist_plot)) {
    faceted <- fun(gfam_model, by_family = TRUE, seed = 42)
    expect_length(faceted$layers, 1L)
    expect_equal(nrow(ggplot_build(faceted)$layout$layout), 3L)
    single <- fun(m_tiny_eg1)
    expect_length(single$layers, 1L)
    expect_equal(unique(ggplot_build(single)$data[[1L]]$fill), "grey20")
  }
})
