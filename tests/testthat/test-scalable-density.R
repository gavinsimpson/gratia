test_that("KernSmooth grids give normalized conditional densities and conserve counts", {
  x <- c(0, .1, .2, .8, .9, 1)
  y <- c(-1, 0, 1, 1, 2, 3)
  bw <- c(.25, .5)
  p <- diagnostic_density_grid(x, y, c(5, 7), bw)
  expect_equal(sum(p$count), length(x))
  expect_lte(nrow(p), 35L)
  expect_true(all(is.finite(p$value) & p$value >= 0))
  mass <- tapply(p$value * (p$ymax - p$ymin), p$xmin, sum)
  expect_equal(as.numeric(mass), rep(1, length(mass)))
  # An independent call to the dependency verifies matrix orientation and
  # conditional normalization, using explicit rather than estimated bandwidths.
  raw <- KernSmooth::bkde2D(cbind(x, y), bandwidth = bw, gridsize = c(5, 7),
    range.x = list(range(x), range(y) + c(-3, 3) * bw[2L]))
  joint <- pmax(raw$fhat, 0)
  dy <- diff(raw$x2)[1L]
  normalized <- joint / (rowSums(joint) * dy)
  # Use nearest grid points to allow rounding of reconstructed cell centres.
  ix <- vapply((p$xmin + p$xmax) / 2,
    function(z) which.min(abs(raw$x1 - z)), integer(1))
  iy <- vapply((p$ymin + p$ymax) / 2,
    function(z) which.min(abs(raw$x2 - z)), integer(1))
  expect_equal(p$value, normalized[cbind(ix, iy)])
  expect_equal(length(unique(p$xmin)), 4L) # Empty middle predictor bin stays blank.
})

test_that("KDE handles constant, tied, singleton and non-finite observations", {
  for (bins in list(c(1L, 1L), c(1L, 5L), c(5L, 1L), c(5L, 6L))) {
    for (z in list(2, rep(0, 10), c(1, 1 + .Machine$double.eps))) {
      p <- diagnostic_density_grid(z, z, bins)
      expect_equal(sum(p$count), length(z))
      expect_lte(nrow(p), prod(bins))
      expect_true(all(p$xmax > p$xmin & p$ymax > p$ymin))
      expect_true(all(is.finite(p$value)))
      mass <- tapply(p$value * (p$ymax - p$ymin), p$xmin, sum)
      expect_equal(as.numeric(mass), rep(1, length(mass)))
    }
  }
  expect_warning(p <- diagnostic_density_grid(c(0, Inf, 1), c(0, 1, NA), 5),
    "Dropped 2")
  expect_equal(sum(p$count), 1)
  expect_error(diagnostic_density_grid(c(NA, Inf), c(0, 1), 5), "No finite")
  # Coarse display cells must average kernel mass rather than sample between
  # narrow peaks and silently discard observations from populated bins.
  for (bins in list(c(1, 1), c(1, 5), c(5, 1), c(5, 5))) {
    p <- diagnostic_density_grid(c(0, .2, .8, 1), c(0, 1, 0, 1), bins,
      bandwidth = c(.01, .01))
    expect_equal(sum(p$count), 4)
    expect_true(all(is.finite(p$value)))
  }
})

test_that("KDE controls reach scalable appraisals without changing count heatmaps", {
  for (model in list(m_tiny_eg1, b_pois, gfam_model, gfam_missing_model)) {
    set.seed(91)
    before <- .Random.seed
    d <- diagnostic_data(model, seed = 31)
    p <- scalable_residuals_linpred_plot(d, density_bins = c(8, 9),
      bandwidth = c(.3, .4))
    app <- appraise(model, scalable = TRUE, seed = 31, method = "normal",
      density_bins = c(8, 9), bandwidth = c(.3, .4))
    expect_equal(app[[2L]]$data, p$data)
    expect_identical(.Random.seed, before)
    expect_lte(nrow(p$data), 72L)
    expect_equal(sum(p$data$count), d$n_obs)
    expect_silent(ggplot_build(p))
    expect_identical(p$scales$get_scales("fill")$trans$name, "sqrt")
    expect_true(all(vapply(p$layers, function(l) !is.function(l$data), logical(1))))
    expect_identical(p$plot_env, environment(compact_diagnostic_plot))
    old <- scalable_residuals_linpred_plot(d, density_method = "histogram",
      density_bins = c(8, 9))
    old_app <- appraise(model, scalable = TRUE, seed = 31, method = "normal",
      density_bins = c(8, 9), density_method = "histogram")
    expect_equal(old_app[[2L]]$data, old$data)
    expect_identical(old$scales$get_scales("fill")$trans$name, "identity")
    counts <- scalable_residuals_linpred_plot(d, density = "count")
    expect_equal(counts$data,
      scalable_residuals_linpred_plot(d, density = "count", density_method = "histogram")$data)
    expect_equal(app[[4L]]$data,
      scalable_observed_fitted_plot(d, density_bins = c(8, 9), by_family = TRUE)$data)
  }
  p <- scalable_residuals_linpred_plot(gfam_model, seed = 31, by_family = TRUE,
    density_bins = c(8, 9))
  expect_equal(nrow(ggplot_build(p)$layout$layout), 3L)
  expect_lte(nrow(p$data), 3L * 72L)
  mass <- tapply(p$data$value * (p$data$ymax - p$data$ymin),
    list(p$data$.family, p$data$xmin), sum)
  expect_equal(as.numeric(mass[is.finite(mass)]), rep(1, sum(is.finite(mass))))
})

test_that("bandwidths and density estimators are validated clearly", {
  for (bad in list(0, c(0, 1), c(-1, 2), c(NA, 1), c(Inf, 1),
    c(1, 2, 3), "automatic", TRUE, numeric())) {
    expect_error(scalable_residuals_linpred_plot(m_tiny_eg1, bandwidth = bad),
      "bandwidth")
    expect_error(appraise(m_tiny_eg1, bandwidth = bad), "bandwidth")
  }
  expect_error(scalable_residuals_linpred_plot(m_tiny_eg1, density_method = "bad"),
    "arg")
  expect_error(diagnostic_density_grid(c(0, 1), c(0, 1), 50,
    bandwidth = c(1e-300, 1e-300)), "KDE grid is too large")
  expect_error(diagnostic_density_grid(c(0, 1), c(0, 1), 50,
    bandwidth = c(1, 1e308)), "non-finite KDE limits")
  for (bad in list(0, NA, 1.5, c(1, 2, 3))) {
    expect_error(diagnostic_density_grid(1, 2, bad), "density_bins")
  }
})

test_that("scalable KDE appraisal has a stable visual rendering", {
  vdiffr::expect_doppelganger("scalable KDE appraisal",
    appraise(m_tiny_eg1, scalable = TRUE, method = "normal", qq_bins = 30,
      density_bins = c(30, 30)))
})
