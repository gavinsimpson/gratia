lss_partial_residual_models <- local({
  d <- withr::with_seed(414, {
    x <- runif(240)
    data.frame(x = x, y = sin(4 * x) + rnorm(length(x)))
  })
  list(gaulss = m_gaulss, gammals = m_gammals, gumbls = m_gumbls,
    gevlss = m_gevlss,
    shash = gam(list(y ~ s(x, k = 5), ~ 1, ~ 1, ~ 1),
      family = shash(), data = d, method = "REML"),
    twlss = m_twlss, ziplss = m_ziplss)
})

test_that("all LSS families report why partial residuals are unavailable", {
  for (m in lss_partial_residual_models) {
    expect_null(m$residuals)
    expect_null(m$weights)
    msg <- paste0("Partial residuals are not available for family <",
      family_name(m), ">: working residuals and working weights are required.")
    # Default selection and terms from every fitted predictor must fail alike.
    for (select in c(list(NULL), as.list(seq_along(m$smooth)))) {
      expect_error(partial_residuals(m, select = select), msg, fixed = TRUE)
      expect_error(add_partial_residuals(model.frame(m), m, select = select),
        msg, fixed = TRUE)
    }
    expect_error(nested_partial_residuals(m), msg, fixed = TRUE)
    expect_error(nested_component_observations(m, 1L, partial = TRUE),
      msg, fixed = TRUE)
  }
})

test_that("drawing LSS smooths warns once and omits partial residuals", {
  for (m in lss_partial_residual_models) {
    warnings <- character()
    plots <- withCallingHandlers(
      draw(m, residuals = TRUE, n = 8, rug = FALSE, scales = "fixed",
        wrap = FALSE),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_identical(warnings, paste0(
      "Partial residuals are not available for family <", family_name(m),
      ">: working residuals and working weights are required. ",
      "Drawing smooths without partial residuals."))
    expect_silent(reference <- draw(m, residuals = FALSE, n = 8, rug = FALSE,
      scales = "fixed", wrap = FALSE))
    expect_identical(names(plots), names(reference))
    expect_gte(length(plots), length(m$smooth))
    for (i in seq_along(plots)) {
      expect_silent(actual <- ggplot2::ggplot_build(plots[[i]]))
      expected <- ggplot2::ggplot_build(reference[[i]])
      expect_equal(actual$data, expected$data)
      expect_equal(actual$layout$panel_params, expected$layout$panel_params)
    }
    expect_warning(assemble(m, residuals = TRUE, select = 1, n = 8,
      rug = FALSE), "Drawing smooths without partial residuals", fixed = TRUE)
  }
})

test_that("both working residuals and working weights are required", {
  for (component in c("residuals", "weights")) {
    m <- m_1_smooth
    m[[component]] <- NULL
    expect_error(partial_residuals(m),
      "working residuals and working weights are required", fixed = TRUE)
    expect_warning(draw(m, residuals = TRUE, n = 8, rug = FALSE),
      "Drawing smooths without partial residuals", fixed = TRUE)
  }
})

test_that("ordinary Gamma models retain weighted working partial residuals", {
  d <- sim_gammals(n = 120)
  m <- gam(y ~ s(x0, k = 5) + s(x2, k = 5), data = d,
    family = Gamma(link = "log"), method = "REML")
  expected <- predict(m, type = "terms") +
    as.numeric(residuals(m, type = "working")) *
    sqrt(weights(m, type = "working"))
  attr(expected, "constant") <- NULL
  rownames(expected) <- NULL
  expect_silent(actual <- partial_residuals(m))
  expect_equal(as.matrix(actual), expected)
  expect_equal(add_partial_residuals(d, m)[names(actual)], actual)
  expect_silent(plots <- draw(m, residuals = TRUE, n = 8, rug = FALSE,
    wrap = FALSE))
  for (i in seq_along(plots)) {
    expect_silent(built <- ggplot2::ggplot_build(plots[[i]]))
    points <- vapply(plots[[i]]$layers,
      function(layer) inherits(layer$geom, "GeomPoint"), logical(1))
    expect_equal(sum(points), 1L)
    expect_equal(built$data[[which(points)]]$y, expected[, i],
      ignore_attr = TRUE)
  }
})
