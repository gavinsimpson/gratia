test_that("scalable grouped diagnostics preserve new main defaults and shared residuals", {
  for (model in list(gfam_model, gfam_bam, gfam_missing_model)) {
    d <- diagnostic_data(model, seed = 23)
    expect_identical(d$type, "quantile")
    expect_equal(
      d$data$residuals,
      quantile_residuals(model_used_rows(model), type = "quantile", seed = 23)
    )
    ordinary <- appraise(model, scalable = FALSE, seed = 23)
    scaled <- appraise(model, scalable = TRUE, seed = 23, qq_bins = 15)
    expect_equal(sum(scaled[[2]]$data$count), d$n_obs)
    expect_equal(sum(scaled[[3]]$data$count), d$n_obs)
    expect_equal(sum(scaled[[4]]$data$count), d$n_obs)
    expect_equal(nrow(ggplot_build(scaled[[4]])$layout$layout), 3L)
    expect_equal(nrow(ggplot_build(scaled[[2]])$layout$layout), 1L)
    expect_lte(nrow(scaled[[1]]$data), 3L * 15L)
    expect_equal(scaled[[1]]$data, scalable_qq_plot(d, qq_bins = 15)$data)
    expect_equal(
      ordinary[[2]]$data$residuals[is.finite(ordinary[[2]]$data$residuals)],
      d$data$residuals
    )
    ranked <- diagnostic_predictor_data(d$data, d$source, FALSE)
    expect_equal(
      scaled[[2]]$data,
      diagnostic_density_grid(ranked$eta, ranked$residuals, c(50, 50))
    )
    expect_equal(
      scaled[[4]]$data$.family,
      scalable_observed_fitted_plot(d)$data$.family
    )
    auto <- appraise(model, seed = 23, scalable_threshold = 1)
    expect_true("value" %in% names(auto[[2]]$data))
  }
})

test_that("scalable grouped references retain component curves and normal reference lines", {
  cran_cases <- list(quantile = list(normal = c(FALSE, TRUE)),
    deviance = list(normal = TRUE), pearson = list(uniform = TRUE),
    response = list(simulate = c(FALSE, TRUE)))
  for (type in c("quantile", "deviance", "pearson", "response")) {
    methods <- if (type == "quantile") {
      "normal"
    } else {
      c("normal", "uniform", "simulate")
    }
    for (method in methods) {
      for (by_family in c(FALSE, TRUE)) {
        if (!full_test_run() &&
            !(by_family %in% cran_cases[[type]][[method]])) next
        old <- qq_plot(
          gfam_model,
          type = type,
          method = method,
          by_family = by_family,
          seed = 74,
          n_uniform = 2,
          n_simulate = 3
        )
        d <- diagnostic_data(gfam_model, type = type, seed = 74)
        ref <- withr::with_seed(
          74,
          diagnostic_qq_data(
            d$source,
            d,
            method = method,
            n_uniform = 2,
            n_simulate = 3,
            by_family = by_family
          )
        )
        expect_equal(ref$data, old$data, ignore_attr = TRUE)
        p <- scalable_qq_plot(
          d,
          method = method,
          by_family = by_family,
          seed = 74,
          n_uniform = 2,
          n_simulate = 3,
          qq_bins = 15
        )
        w <- scalable_worm_plot(
          d,
          method = method,
          by_family = by_family,
          seed = 74,
          n_uniform = 2,
          n_simulate = 3,
          qq_bins = 15
        )
        expect_equal(p$data, bin_diagnostic_qq(ref, 15))
        expect_equal(w$data, bin_diagnostic_qq(ref, 15, worm = TRUE))
        if (method == "normal" && type != "quantile") {
          expect_equal(
            ggplot_build(p)$data[[1L]]$slope,
            ggplot_build(old)$data[[1L]]$slope
          )
          expect_equal(
            ggplot_build(p)$data[[1L]]$intercept,
            ggplot_build(old)$data[[1L]]$intercept
          )
        }
        expect_equal(
          nrow(ggplot_build(p)$layout$layout),
          if (by_family) 3L else 1L
        )
        expect_equal(
          nrow(ggplot_build(w)$layout$layout),
          if (by_family) 3L else 1L
        )
      }
    }
  }
})

test_that("scalable grouped histograms retain component counts and densities", {
  for (by_family in c(FALSE, TRUE)) {
    old <- ggplot_build(residuals_hist_plot(
      gfam_model,
      seed = 26,
      by_family = by_family
    ))$data[[1L]]
    p <- scalable_residuals_hist_plot(
      gfam_model,
      seed = 26,
      by_family = by_family
    )
    expect_equal(p$data$count, old$count)
    expect_equal(p$data$xmin, old$xmin)
    expect_equal(p$data$xmax, old$xmax)
    expect_equal(p$data$value, if (by_family) old$count else old$density)
    expect_equal(nrow(ggplot_build(p)$layout$layout), if (by_family) 3L else 1L)
    heat <- scalable_residuals_linpred_plot(
      gfam_model,
      by_family = by_family,
      seed = 26
    )
    expect_equal(
      nrow(ggplot_build(heat)$layout$layout),
      if (by_family) 3L else 1L
    )
  }
  expect_s3_class(appraise(gfam_ordinal_model, scalable = TRUE), "patchwork")
  expect_s3_class(scalable_observed_fitted_plot(gfam_ordinal_model), "ggplot")
  expect_s3_class(
    scalable_qq_plot(gfam_ordinal_model, type = "deviance", method = "normal"),
    "ggplot"
  )
})

test_that("near-constant grids have visible bins and finite conditional densities", {
  x <- c(1, 1 + .Machine$double.eps)
  d <- diagnostic_grid(x, x, c(50, 50), "conditional")
  expect_equal(sum(d$count), 2)
  expect_true(all(d$xmax > d$xmin))
  expect_true(all(d$ymax > d$ymin))
  expect_true(all(is.finite(d$value)))
})
