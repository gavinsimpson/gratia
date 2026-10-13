test_that("MRF simultaneous intervals agree with native coefficient-draw calculations", {
  data <- data.frame(area = factor(mrf_models$areas, levels = mrf_models$areas),
    x = 0.4, z = 1, fac = factor("B", levels = c("B", "A")))
  for (name in c("full", "low", "bam")) {
    model <- mrf_models[[name]]
    label <- model$smooth[[1L]]$label
    for (overall in c(FALSE, TRUE)) {
      reference <- smooth_band_reference(model, label, data, overall = overall)
      values <- smooth_estimates(model, select = label, data = data,
        interval = "simultaneous", ci_level = 0.9, n_sim = 200, seed = 314,
        overall_uncertainty = overall)
      expect_smooth_band_reference(values, reference)
      plot <- assemble(model, select = label, data = data, rug = FALSE,
        interval = "simultaneous", ci_level = 0.9, n_sim = 200, seed = 314,
        overall_uncertainty = overall)[[1L]]
      built <- ggplot_build(plot)
      expect_equal(built$data[[1L]]$ymin, unname(values$.lower_ci))
      expect_equal(built$data[[1L]]$ymax, unname(values$.upper_ci))
      expect_match(plot$labels$caption, "simultaneous band")
    }
  }
})

test_that("MRF simultaneous plots preserve domain and keep QQ displays descriptive", {
  values <- smooth_estimates(mrf_models$unobserved, interval = "simultaneous",
    n_sim = 120, seed = 9)
  expect_equal(as.character(values$area), mrf_models$areas)
  expect_true(all(is.finite(values$.lower_ci)))
  plot <- draw(values)
  expect_equal(plot$data$.lower_ci, values$.lower_ci)
  expect_match(plot$labels$caption, "simultaneous band")
  qq <- draw(values, mrf_plot = "qq")
  expect_false(any(grepl("simultaneous band", qq$labels$caption)))
  expect_true(all(qq$data$.type == "MRF"))
})


test_that("simultaneous intervals retain distributional plot subtitles", {
  values <- smooth_estimates(m_gaulss, n = 15, interval = "simultaneous",
    n_sim = 120, seed = 314)
  expect_true(all(values$.model == "Mean (mu)"))
  saved <- draw(values, wrap = FALSE)
  direct <- assemble(m_gaulss, n = 15, interval = "simultaneous",
    n_sim = 120, seed = 314, rug = FALSE, parametric = FALSE)
  for (plot in c(saved, direct)) {
    expect_match(plot$labels$subtitle, "Mean (mu)", fixed = TRUE)
    expect_true(any(grepl("simultaneous band", plot$labels$caption)))
  }
})


test_that("MRF intervals remain ordered after decreasing transformations", {
  fun <- function(x) -exp(x)
  for (interval in c("confidence", "simultaneous")) {
    values <- smooth_estimates(mrf_models$low, interval = interval,
      n_sim = 120, seed = 314)
    plot <- draw(values, fun = fun)
    built <- ggplot_build(plot)
    expect_equal(built$data[[1L]]$ymin, unname(fun(values$.upper_ci)))
    expect_equal(built$data[[1L]]$ymax, unname(fun(values$.lower_ci)))
    expect_equal(built$data[[2L]]$y, unname(fun(values$.estimate)))
    expect_true(all(built$data[[1L]]$ymin <= built$data[[1L]]$ymax))
  }
})
