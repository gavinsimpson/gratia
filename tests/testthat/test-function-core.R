# Use the shared fits for small checks of public functions. The full suite
# repeats these paths across families, model classes and plotting options.
test_that("fitted and conditional moments retain native means and variances", {
  m <- m_1_smooth
  nd <- quick_eg1[c(2, 7, 11), ]
  actual <- fitted_moments(m, data = nd, moments = c("mean", "variance"))
  expected <- as.numeric(predict(m, nd, type = "response"))
  scale <- m$reml.scale %||% m$sig2
  expect_equal(actual$.fitted[actual$.parameter == "mean"], expected)
  expect_equal(actual$.fitted[actual$.parameter == "variance"], rep(scale, nrow(nd)))
  expect_true(all(is.finite(actual$.se)))
  expect_true(all(actual$.lower_ci <= actual$.fitted))
  expect_true(all(actual$.upper_ci >= actual$.fitted))
  beta <- rbind(coef(m), coef(m) + 0.01, coef(m) - 0.02)
  simulated <- fitted_moments(m, data = nd, moments = "mean", nuisance = "fixed",
    uncertainty = "simulation", method = "user", draws = beta)
  reference <- predict(m, nd, type = "lpmatrix") %*% t(beta)
  expect_equal(simulated$.se, unname(apply(reference, 1L, sd)))
  grid <- conditional_moments(m, condition = list(x0 = c(0.2, 0.8)),
    moments = "mean")
  expect_s3_class(grid, "conditional_moments")
  expect_equal(grid$.fitted, as.numeric(predict(m, grid, type = "response")))
  expect_silent(ggplot2::ggplot_build(draw(grid)[[1L]]))
})

test_that("location-scale moments keep separate mean and variance predictors", {
  nd <- quick_eg1[1:2, ]
  actual <- fitted_moments(m_gaulss, data = nd, moments = c("mean", "variance"))
  native <- predict(m_gaulss, nd, type = "response")
  expect_equal(actual$.fitted[actual$.parameter == "mean"], native[, 1L],
    ignore_attr = TRUE)
  expect_equal(actual$.fitted[actual$.parameter == "variance"], 1 / native[, 2L]^2,
    ignore_attr = TRUE)
  expect_true(all(is.finite(actual$.se)))
})

test_that("plot themes and palettes change appearance while retaining values", {
  withr::local_options(list(gratia.plot.theme = NULL))
  original <- draw(m_1_smooth, n = 6)[[1L]]
  override <- ggplot2::theme(
    geom.gratia.smooth = ggplot2::element_geom(colour = "purple"))
  expect_null(gratia_theme_set(override))
  expect_identical(gratia_theme_get(), override)
  gratia_theme_update(gratia.alpha.interval = 0.3)
  changed <- draw(m_1_smooth, n = 6)[[1L]]
  before <- ggplot2::ggplot_build(original)$data
  after <- ggplot2::ggplot_build(changed)$data
  expect_equal(after[[2L]]$y, before[[2L]]$y)
  expect_equal(unique(after[[2L]]$colour), "purple")
  expect_equal(unique(after[[1L]]$alpha), 0.3)
  expect_identical(gratia_palette()(3L), c("#E69F00", "#56B4E9", "#009E73"))
  expect_error(gratia_palette(reverse = NA), "TRUE or FALSE")
  gratia_theme_set(NULL)
})

test_that("label controls wrap labels without changing plotted values", {
  d <- data.frame(group = c("Boat guided", "Shore"), value = c(1, 2))
  p <- ggplot2::ggplot(d, ggplot2::aes(group, value)) + ggplot2::geom_point()
  control <- draw_label_control(axes = list(wrap = 5, n_dodge = 2))
  expect_s3_class(control, "draw_label_control")
  changed <- ggplot2::ggplot_build(apply_draw_label_control(p, control))
  expect_equal(changed$data, ggplot2::ggplot_build(p)$data)
  axis <- changed$layout$panel_params[[1L]]$guides$get_params("x")
  expect_equal(axis$key$.label, c("Boat\nguided", "Shore"))
  expect_equal(axis$n.dodge, 2L)
  expect_silent(ggplot2::ggplot_build(draw(m_1_smooth, n = 6,
    label_control = control)[[1L]]))
  expect_error(draw_label_control(axes = list(wrap = 0)), "axes\\$wrap")
})

test_that("3D confidence surfaces retain the estimated interval heights", {
  skip_if_not_installed("ggcube", "0.3.0")
  sm <- smooth_estimates(su_m_bivar_te, n_2d = 4, dist = 0)
  p <- draw(sm, surface = "3d", ci_3d = TRUE, contour = FALSE)[[1L]]
  expect_s3_class(p$coordinates, "Coord3D")
  scene <- p$layers[[1L]]$data
  for (role in c("lower", "upper")) {
    faces <- scene[scene$.surface_role == role, ]
    column <- paste0(".", role, "_ci")
    expect_gt(nrow(faces), 0L)
    expect_equal(faces$.surface_z, faces[[column]])
  }
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("smooth comparisons retain native differences on a small grid", {
  m <- su_m_bivar_te
  other <- m
  other$coefficients <- coef(m) + 0.1
  comparison <- compare_smooths(m, Other = other, n_2d = 4)
  expect_equal(nrow(comparison), 2L)
  p <- draw(comparison, show = "difference")[[1L]]
  nd <- p$data
  expected <- as.numeric(predict(m, nd, type = "terms") -
    predict(other, nd, type = "terms"))
  expect_equal(p$data$.diff, expected)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("covariate residual plots retain the observed covariates and residuals", {
  residuals <- diagnostic_data(m_1_smooth, seed = 42)$data$residuals
  for (fun in list(residuals_covariate_plot, scalable_residuals_covariate_plot)) {
    p <- fun(m_1_smooth, covariates = "x0", smooth = FALSE, seed = 42)
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
  p <- residuals_covariate_plot(m_1_smooth, covariates = "x0",
    smooth = FALSE, seed = 42)
  expect_equal(p$data$x, quick_eg1$x0)
  expect_equal(p$data$residuals, residuals)
})

test_that("public utility functions retain their basic numerical contracts", {
  f <- factor(c("b", "a", "b"), levels = c("b", "a", "c"))
  expect_identical(all_levels(f), factor(c("b", "a", "c"), levels = levels(f)))
  expect_identical(by_variable(m_1_smooth$smooth[[1L]]), "NA")
  expect_identical(by_variable(su_m_factor_by$smooth[[1L]]), "fac")
  expect_identical(is_offset(c("x", "offset(log(exposure))")), c(FALSE, TRUE))
  expect_identical(vars_from_label("te(x,z)"), c("x", "z"))
  expect_equal(gw_f3(c(0, 0.5, 1)), c(0, 0, 0))
  expect_error(evaluate_smooth(m_1_smooth), "defunct", ignore.case = TRUE)
  expect_equal(to_na(c(1, 2, 3), c(FALSE, TRUE, FALSE)), c(1, NA, 3))
  reference <- data.frame(x = c(0, 1, 0), z = c(0, 0, 1))
  input <- data.frame(x = c(0, 1), z = c(0, 1), value = c(2, 3))
  mask <- mgcv::exclude.too.far(input$x, input$z, reference$x, reference$z, dist = 0.1)
  expect_identical(too_far(input$x, input$z, reference$x, reference$z, dist = 0.1), mask)
  sm <- su_m_bivar_te$smooth[[1L]]
  actual <- too_far_to_na(sm, input, reference, cols = "value", dist = 0.1)
  expect_equal(actual$value, replace(input$value, mask, NA_real_))
})
