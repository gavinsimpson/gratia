# The full suite checks combinations across models, families and plot options.
# Small function checks remain in test-function-core.R and focused test files.
skip_on_cran()

# Indices and reference distributions come directly from the fitted mgcv object.
gfam_response_test_index <- function(model) attr(model$family$getfl(), "fi")
gfam_response_test_labels <- function(model) paste0(seq_along(model$family$getfl()),
  ": ", vapply(model$family$getfl(), `[[`, character(1), "family"))

gfam_response_test_plotters <- function() list(
  qq = list(fun = qq_plot, args = list(type = "quantile", seed = 46)),
  worm = list(fun = worm_plot, args = list(type = "quantile", seed = 46)),
  predictor = list(fun = residuals_linpred_plot,
    args = list(type = "quantile", seed = 46, rank = FALSE)),
  histogram = list(fun = residuals_hist_plot,
    args = list(type = "quantile", seed = 46, n_bins = 10)),
  observed = list(fun = observed_fitted_plot, args = list(seed = 46)),
  scalable_qq = list(fun = scalable_qq_plot,
    args = list(type = "quantile", seed = 46, qq_bins = 12)),
  scalable_worm = list(fun = scalable_worm_plot,
    args = list(type = "quantile", seed = 46, qq_bins = 12)),
  scalable_predictor = list(fun = scalable_residuals_linpred_plot,
    args = list(type = "quantile", seed = 46, density_bins = c(8, 8))),
  scalable_histogram = list(fun = scalable_residuals_hist_plot,
    args = list(type = "quantile", seed = 46, n_bins = 10)),
  scalable_observed = list(fun = scalable_observed_fitted_plot,
    args = list(seed = 46, density_bins = c(8, 8)))
)

test_that("gfam response indices select observations without renumbering family labels", {
  model <- gfam_model
  index <- gfam_response_test_index(model)
  labels <- gfam_response_test_labels(model)
  all <- diagnostic_data(model, seed = 41)
  expect_equal(diagnostic_data(model, response = NULL, seed = 41)$data, all$data)
  expect_identical(unique(all$data$.response), 1L)
  for (i in 1:3) {
    rows <- which(index == i)
    selected <- diagnostic_data(model, response = i, seed = 41)
    expect_equal(selected$data, all$data[rows, ], ignore_attr = TRUE)
    expect_identical(levels(selected$data$.family), labels)
    expect_identical(unique(as.character(selected$data$.family)), labels[i])
    expect_identical(selected$data$.row, rows)
    expect_equal(selected$n_obs, length(rows))
    expect_equal(selected$data$observed, as.vector(model$y[rows]))
    expect_equal(selected$data$fitted, as.vector(model$fitted.values[rows]))
    distribution <- response_distribution(model, response = i)
    expect_equal(distribution$mean, as.vector(model$fitted.values[rows]))
    mu <- as.vector(model$fitted.values[rows])
    expected_cdf <- switch(i,
      pbinom(model$y[rows] * model$prior.weights[rows],
        size = model$prior.weights[rows], prob = mu),
      pnorm(model$y[rows], mu,
        sqrt(exp(model$family$getTheta()[1L]) / model$prior.weights[rows])),
      ppois(model$y[rows], mu))
    expect_equal(distribution$cdf(model$y[rows]), as.vector(expected_cdf))
  }
})

test_that("repeated gfam families retain their own fitted scale and list index", {
  model <- gfam_gaussian_model
  index <- gfam_response_test_index(model)
  scales <- exp(model$family$getTheta())
  expect_gt(scales[2], scales[1])
  for (i in 1:2) {
    rows <- which(index == i)
    mu <- as.vector(model$fitted.values[rows])
    variance <- scales[i] / model$prior.weights[rows]
    expected <- pnorm(model$y[rows], mu, sqrt(variance))
    distribution <- response_distribution(model, response = i)
    expect_equal(distribution$mean, mu)
    expect_equal(distribution$qf(rep(.9, length(rows))),
      as.vector(qnorm(.9, mu, sqrt(variance))))
    expect_equal(distribution$cdf(model$y[rows]), as.vector(expected))
    expect_equal(as.vector(quantile_residuals(model, type = "pit", response = i)),
      as.vector(expected))
    selected <- diagnostic_data(model, type = "quantile", response = i)
    expect_identical(unique(as.character(selected$data$.family)),
      paste0(i, ": gaussian"))
    expect_equal(selected$data$residuals, as.vector(qnorm(expected)))
  }
})

test_that("selected gfam randomised residuals preserve the full seeded stream and RNG", {
  withr::local_seed(47)
  model <- gfam_model
  index <- gfam_response_test_index(model)
  before <- .Random.seed
  theta <- model$family$getTheta()
  families <- serialize(model$family$getfl(), NULL)
  for (type in c("pit", "quantile", "dharma")) {
    all <- diagnostic_data(model, type = type, seed = 48, n_dharma = 17)
    expect_identical(.Random.seed, before)
    for (i in 1:3) {
      rows <- which(index == i)
      selected <- diagnostic_data(model, type = type, seed = 48,
        response = i, n_dharma = 17)
      expect_equal(selected$data$residuals, all$data$residuals[rows])
      expect_equal(diagnostic_data(model, type = type, seed = 48,
        response = i, n_dharma = 17)$data, selected$data)
      expect_identical(.Random.seed, before)
      if (type == "dharma") {
        expect_equal(as.vector(dharma_residuals(model, n = 17, seed = 48,
          response = i)), all$data$residuals[rows])
      } else {
        expect_equal(as.vector(quantile_residuals(model, type = type, seed = 48,
          response = i)), all$data$residuals[rows])
      }
      expect_identical(.Random.seed, before)
    }
  }
  expect_identical(model$family$getTheta(), theta)
  expect_identical(serialize(model$family$getfl(), NULL), families)
})

test_that("gfam selected rows retain their original identities after subset and omission", {
  model <- gfam_missing_model
  index <- gfam_response_test_index(model)
  retained <- setdiff(seq_len(165L), as.integer(model$na.action))
  expect_equal(length(retained), length(index))
  all <- diagnostic_data(model, type = "pit", seed = 52)
  all_residuals <- quantile_residuals(model, type = "pit", seed = 52)
  expect_length(all_residuals, 165L)
  expect_true(all(is.na(all_residuals[as.integer(model$na.action)])))
  for (i in 1:3) {
    positions <- which(index == i)
    rows <- retained[positions]
    selected <- diagnostic_data(model, type = "pit", response = i, seed = 52)
    expect_equal(selected$data$.row, rows)
    expect_equal(selected$data$observed, gfam_missing_data$y[rows])
    expect_equal(selected$data$residuals, all$data$residuals[positions])
    expect_equal(as.vector(quantile_residuals(model, type = "pit", response = i,
      seed = 52)), as.vector(all_residuals[rows]))
    expect_length(quantile_residuals(model, response = i, seed = 52), length(rows))
    plot <- residuals_covariate_plot(model, covariates = "x", response = i,
      type = "pit", seed = 52, smooth = FALSE)
    expect_equal(plot$data$x, gfam_missing_data$x[rows])
    expect_equal(plot$data$residuals, selected$data$residuals)
  }
})

test_that("gfam response selection is independent of family facet choices", {
  model <- gfam_model
  labels <- gfam_response_test_labels(model)
  for (name in names(gfam_response_test_plotters())) {
    config <- gfam_response_test_plotters()[[name]]
    choices <- test_cases_for_cran(list(NULL, FALSE, TRUE),
      if (name %in% c("qq", "observed", "scalable_qq")) list(NULL, FALSE, TRUE)
        else list(NULL))
    for (by_family in choices) {
      arguments <- c(list(model = model, by_family = by_family), config$args)
      all <- do.call(config$fun, c(arguments, list(response = NULL)))
      selected <- do.call(config$fun, c(arguments, list(response = 2)))
      all_layout <- ggplot2::ggplot_build(all)$layout$layout
      layout <- ggplot2::ggplot_build(selected)$layout$layout
      observed <- grepl("observed", name)
      expected_facets <- if (isTRUE(by_family) || (is.null(by_family) && observed)) 3L else 1L
      expect_equal(nrow(all_layout), expected_facets, info = name)
      expect_equal(nrow(layout), 1L, info = name)
      if (".family" %in% names(layout))
        expect_identical(as.character(layout$.family), labels[2], info = name)
      if (".family" %in% names(selected$data))
        expect_identical(unique(as.character(selected$data$.family)), labels[2], info = name)
    }
  }
  # Traditional residuals retain the existing default of separate family panels.
  expect_equal(nrow(ggplot2::ggplot_build(qq_plot(model, type = "deviance",
    method = "normal", response = NULL))$layout$layout), 3L)
  for (fun in list(residuals_covariate_plot, scalable_residuals_covariate_plot)) {
    all <- fun(model, covariates = "x", smooth = FALSE, seed = 46, response = NULL)
    selected <- fun(model, covariates = "x", smooth = FALSE, seed = 46, response = 2)
    expect_equal(nrow(ggplot2::ggplot_build(all)$layout$layout), 3L)
    expect_equal(nrow(ggplot2::ggplot_build(selected)$layout$layout), 1L)
  }
  pooled <- residuals_linpred_plot(model, response = NULL, type = "quantile",
    by_family = FALSE, seed = 46)
  selected <- residuals_linpred_plot(model, response = 2, type = "quantile",
    by_family = FALSE, seed = 46)
  original_scale <- ggplot2::ggplot_build(pooled)$plot$scales$get_scales("colour")
  selected_scale <- ggplot2::ggplot_build(selected)$plot$scales$get_scales("colour")
  expect_identical(selected_scale$map(labels[2]), original_scale$map(labels[2]))
})

test_that("gfam appraise selects every panel while preserving family layout defaults", {
  withr::local_pdf(NULL)
  model <- gfam_model
  expected <- diagnostic_data(model, response = 2, seed = 54)$data$residuals
  for (scalable in c(FALSE, TRUE)) {
    for (by_family in test_cases_for_cran(list(NULL, FALSE, TRUE), list(NULL))) {
      all <- appraise(model, response = NULL, by_family = by_family,
        seed = 54, scalable = scalable, qq_bins = 12)
      selected <- appraise(model, response = 2, by_family = by_family,
        seed = 54, scalable = scalable, qq_bins = 12)
      expect_length(all, 4L)
      expect_length(selected, 4L)
      for (i in 1:4) {
        expect_equal(nrow(ggplot2::ggplot_build(all[[i]])$layout$layout),
          if (isTRUE(by_family) || i == 4L) 3L else 1L)
        expect_equal(nrow(ggplot2::ggplot_build(selected[[i]])$layout$layout), 1L)
      }
      if (!scalable) expect_equal(selected[[2]]$data$residuals, expected)
    }
  }
  check <- appraise(model, response = 2, ppcheck = TRUE, n_ppc = 3,
    seed = 55, scalable = FALSE)
  expect_length(check, 4L)
  index <- gfam_response_test_index(model)
  expect_equal(check[[3]]$data$.response[check[[3]]$data$.draw == 0L],
    as.vector(model$y[index == 2L]))
  expect_silent(patchwork::patchworkGrob(check))
})

test_that("prepared gfam diagnostics retain cached residuals through selection and reselection", {
  withr::local_seed(56)
  model <- gfam_model
  index <- gfam_response_test_index(model)
  before <- .Random.seed
  for (type in c("pit", "quantile", "dharma")) {
    all <- diagnostic_data(model, type = type, seed = 57, n_dharma = 13)
    selected <- diagnostic_data(all, response = 2)
    expect_equal(selected$data$residuals, all$data$residuals[index == 2L])
    expect_equal(diagnostic_data(selected)$data, selected$data)
    other <- diagnostic_data(selected, response = 3)
    expect_equal(other$data$residuals, all$data$residuals[index == 3L])
    expect_equal(diagnostic_data(selected, response = NULL)$data, all$data)
    source <- selected$source
    expect_equal(response_distribution(source)$mean,
      as.vector(model$fitted.values[index == 2L]))
    expect_equal(response_distribution(source, response = NULL)$mean,
      as.vector(model$fitted.values))
    reselected <- diagnostic_data(source, response = 3, type = type,
      seed = 57, n_dharma = 13)
    expect_equal(reselected$data$residuals, all$data$residuals[index == 3L])
    fresh <- diagnostic_data(model, type = type, seed = 62, n_dharma = 19)
    changed <- diagnostic_data(source, response = 3, type = type,
      seed = 62, n_dharma = 19)
    expect_equal(changed$data$residuals, fresh$data$residuals[index == 3L])
    expect_identical(.Random.seed, before)
  }
})

test_that("supported gfam responses can be selected beside unsupported families", {
  model <- gfam_model
  families <- model$family$getfl()
  families[[1]]$family <- "unsupported test component"
  model$family$getfl <- function() families
  expect_error(diagnostic_data(model, type = "quantile", seed = 58), "component.*1")
  expect_silent(selected <- diagnostic_data(model, response = 2, seed = 58))
  rows <- which(gfam_response_test_index(model) == 2L)
  scale <- exp(model$family$getTheta()[1L])
  expected <- qnorm(pnorm(model$y[rows], model$fitted.values[rows],
    sqrt(scale / model$prior.weights[rows])))
  expect_equal(selected$data$residuals, as.vector(expected))
  expect_equal(response_distribution(model, response = 2)$mean,
    as.vector(model$fitted.values[rows]))
  expect_silent(ggplot2::ggplot_build(qq_plot(model, response = 2, seed = 58)))
  simulated <- diagnostic_data(model, response = 2, type = "dharma",
    n_dharma = 11, seed = 58)
  expect_length(simulated$data$residuals, length(rows))
  expect_true(all(is.finite(simulated$data$residuals) &
    simulated$data$residuals >= 0 & simulated$data$residuals <= 1))
})

test_that("gfam response indices reject invalid values and ordinary GAM selection", {
  model <- gfam_model
  for (response in list(0, -1, 4, 1.5, Inf, NaN, NA_integer_, TRUE,
    "gaussian", numeric(), 1:2, list(2))) {
    expect_error(diagnostic_data(model, response = response), "response")
  }
  numerical <- list(diagnostic_data, quantile_residuals, response_distribution,
    dharma_residuals, appraise)
  plotters <- lapply(gfam_response_test_plotters(), `[[`, "fun")
  functions <- c(numerical, plotters,
    list(residuals_covariate_plot, scalable_residuals_covariate_plot))
  for (fun in functions) {
    expect_error(fun(model, response = 0), "response")
    expect_error(fun(m_tiny_eg1, response = 1), "gfam|grouped")
  }
  for (fun in list(diagnostic_data, observed_fitted_plot, scalable_qq_plot, appraise)) {
    expect_error(fun(m_lm, response = 1), "gfam|grouped")
  }
  expect_equal(quantile_residuals(m_tiny_eg1, response = NULL, seed = 59),
    quantile_residuals(m_tiny_eg1, seed = 59))
  expect_equal(response_distribution(m_tiny_eg1, response = NULL)$mean,
    response_distribution(m_tiny_eg1)$mean)
})

test_that("standalone gfam posterior checks select rows in ordinary and scalable plots", {
  withr::local_seed(63)
  before <- .Random.seed
  for (model in list(gfam_model, gfam_missing_model)) {
    index <- gfam_response_test_index(model)
    retained <- if (is.null(model$na.action)) seq_along(index) else
      setdiff(seq_len(length(index) + length(model$na.action)),
        as.integer(model$na.action))
    for (i in 1:3) {
      rows <- which(index == i)
      raw <- posterior_predictive_plot(model, response = i, n = 3,
        seed = 64, scalable = FALSE)
      observed <- raw$data[raw$data$.draw == 0L, ]
      expect_equal(observed$.response, as.vector(model$y[rows]))
      expect_identical(observed$.row, retained[rows])
      expect_identical(sort(unique(raw$data$.draw)), 0:3)
      for (draw in 1:3) {
        expect_identical(raw$data$.row[raw$data$.draw == draw], retained[rows])
      }
      expect_equal(nrow(raw$data), length(rows) * 4L)
      expect_equal(nrow(ggplot2::ggplot_build(raw)$layout$layout), 1L)
      scalable <- posterior_predictive_plot(model, response = i, n = 3,
        seed = 64, scalable = TRUE, batch_size = 3)
      expect_equal(lapply(scalable$layers, `[[`, "data"),
        lapply(raw$layers, `[[`, "data"))
      expect_silent(ggplot2::ggplot_build(scalable))
      expect_identical(.Random.seed, before)
    }
  }
})

test_that("prepared gfam posterior checks retain, change and reset selection", {
  model <- gfam_model
  prepared <- diagnostic_data(model, response = 2, type = "pit", seed = 65)
  for (scalable in c(FALSE, TRUE)) {
    args <- list(n = 3, seed = 66, scalable = scalable, batch_size = 3)
    selected <- do.call(posterior_predictive_plot, c(list(model = model,
      response = 2), args))
    for (input in list(prepared, prepared$source)) {
      preserved <- do.call(posterior_predictive_plot, c(list(model = input), args))
      expect_equal(preserved$data, selected$data)
      expect_equal(lapply(preserved$layers, `[[`, "data"),
        lapply(selected$layers, `[[`, "data"))
      changed <- do.call(posterior_predictive_plot, c(list(model = input,
        response = 3), args))
      reference <- do.call(posterior_predictive_plot, c(list(model = model,
        response = 3), args))
      expect_equal(changed$data, reference$data)
      expect_equal(lapply(changed$layers, `[[`, "data"),
        lapply(reference$layers, `[[`, "data"))
      reset <- do.call(posterior_predictive_plot, c(list(model = input,
        response = NULL), args))
      all <- do.call(posterior_predictive_plot, c(list(model = model), args))
      expect_equal(reset$data, all$data)
      expect_equal(lapply(reset$layers, `[[`, "data"),
        lapply(all$layers, `[[`, "data"))
    }
  }
})

test_that("posterior gfam selection skips inactive families and validates indices", {
  model <- gfam_model
  families <- model$family$getfl()
  families[[1]]$family <- "unsupported test component"
  model$family$getfl <- function() families
  rows <- which(gfam_response_test_index(model) == 2L)
  for (scalable in c(FALSE, TRUE)) {
    expect_silent(p <- posterior_predictive_plot(model, response = 2,
      n = 3, seed = 67, scalable = scalable, batch_size = 3))
    expect_silent(ggplot2::ggplot_build(p))
    if (!scalable) expect_equal(p$data$.response[p$data$.draw == 0L],
      as.vector(model$y[rows]))
  }
  for (response in list(0, 4, 1.5, NA_integer_, "gaussian", 1:2)) {
    expect_error(posterior_predictive_plot(gfam_model, response = response,
      n = 3), "response")
  }
  for (ordinary in list(m_tiny_eg1, m_lm)) {
    expect_error(posterior_predictive_plot(ordinary, response = 1, n = 3),
      "gfam|grouped")
  }
  expect_equal(posterior_predictive_plot(m_tiny_eg1, response = NULL,
    n = 3, seed = 68)$data,
    posterior_predictive_plot(m_tiny_eg1, n = 3, seed = 68)$data)
})
