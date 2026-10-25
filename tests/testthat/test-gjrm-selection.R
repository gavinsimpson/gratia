# The full suite checks combinations across models, families and plot options.
# Small function checks remain in test-function-core.R and focused test files.
skip_on_cran()

skip_if_not_installed("GJRM", minimum_version = "0.2-6")

gjrm_selection_fixture <- local({
  models <- list()
  function(repeated = FALSE) {
    key <- as.character(repeated)
    if (is.null(models[[key]])) {
      d <- withr::with_seed(82, {
        d <- data.frame(x = runif(100), z = runif(100), a = runif(100))
        d$y <- sin(6 * d$x) + .4 * d$a + rnorm(100, sd = exp(.3 * d$z))
        d
      })
      second <- if (repeated) ~ s(x, k = 4) else ~ s(z, k = 4)
      models[[key]] <<- GJRM::gamlss(list(y ~ a + s(x, k = 4), second), data = d)
    }
    models[[key]]
  }
})

test_that("GJRM smooth selection is resolved across equations before drawing", {
  model <- gjrm_selection_fixture()
  expect_identical(smooths(model), c("s(x)", "s(z)"))
  all <- draw(model, parametric = FALSE, wrap = FALSE, n = 8)
  expect_length(all, 2L)
  expect_true(all(vapply(all, inherits, logical(1), "ggplot")))
  for (select in list("s(z)", 2, c(FALSE, TRUE))) {
    selected <- draw(model, select = select, wrap = FALSE, n = 8)
    expect_length(selected, 1L)
    expect_identical(names(selected), "s(z)")
    expect_equal(selected[[1]]$data, all[[2]]$data)
    expect_identical(selected[[1]]$labels$subtitle, all[[2]]$labels$subtitle)
  }
  expect_length(draw(model, select = "s(z", partial_match = TRUE,
    wrap = FALSE, n = 8), 1L)
  expect_error(draw(model, select = "s(nope)"), "Failed to match")
  expect_error(draw(model, select = c("s(x)", "s(nope)")), "not found")
  expect_error(draw(model, select = 3), "number of smooths")
  expect_error(draw(model, select = 0), "positive integers")
  expect_error(draw(model, select = 1.5), "positive integers")
  expect_error(draw(model, select = NA_integer_), "without missing values")
  expect_error(draw(model, select = TRUE), "length\\(select\\)")
})

test_that("GJRM repeated smooth labels have discoverable distinct selection keys", {
  model <- gjrm_selection_fixture(TRUE)
  expect_identical(smooths(model), c("s(x)", "s.1(x)"))
  all <- draw(model, parametric = FALSE, wrap = FALSE, n = 8)
  expect_identical(names(all), smooths(model))
  for (select in list("s.1(x)", 2)) {
    selected <- draw(model, select = select, wrap = FALSE, n = 8)
    expect_length(selected, 1L)
    expect_equal(selected[[1]]$data, all[[2]]$data)
  }
  first <- draw(model, select = "s(x)", wrap = FALSE, n = 8)
  expect_length(first, 1L)
  expect_equal(first[[1]]$data, all[[1]]$data)
  # Reserve labels already present in another equation before adding suffixes.
  collision <- model
  collision$gam1$smooth <- c(collision$gam1$smooth, collision$gam1$smooth)
  collision$gam1$smooth[[2]]$label <- "s.1(x)"
  expect_identical(smooths(collision), c("s(x)", "s.1(x)", "s.2(x)"))
})

test_that("GJRM wrap and parametric choices preserve plots and simultaneous intervals", {
  model <- gjrm_selection_fixture()
  plain <- draw(model, wrap = FALSE, n = 8)
  expect_length(plain, 3L)
  expect_true(all(vapply(plain, inherits, logical(1), "ggplot")))
  expect_s3_class(draw(model, n = 8), "patchwork")
  wrapped <- draw(model, wrap = TRUE, n = 8, guides = "collect")
  expect_length(wrapped, length(plain))
  expect_identical(wrapped$patches$layout$guides, "collect")
  expect_silent(patchwork::patchworkGrob(wrapped))
  selected <- draw(model, select = "s(z)", parametric = TRUE, terms = "a",
    wrap = FALSE, n = 8)
  expect_length(selected, 2L)
  expect_setequal(names(selected), c("s(z)", "a"))
  expect_error(draw(model, parametric = TRUE, terms = "missing"), "No parametric terms")
  expect_length(draw(model, parametric = FALSE, terms = "missing",
    wrap = FALSE, n = 8), 2L)
  for (wrap in c(FALSE, TRUE)) {
    selected <- draw(model, select = 2, interval = "simultaneous", n_sim = 100,
      seed = 9, wrap = wrap, n = 8)
    component <- gjrm_gamlss_components(model)[[2]]
    expected <- draw(component, parametric = FALSE, interval = "simultaneous",
      n_sim = 100, seed = 9, wrap = FALSE, n = 8)
    expect_equal(selected[[1]]$data, expected[[1]]$data)
  }
})

test_that("GJRM models without smooths can still draw parametric effects", {
  d <- withr::with_seed(83, data.frame(x = runif(70), y = rnorm(70)))
  model <- GJRM::gamlss(list(y ~ x, ~ 1), data = d)
  expect_identical(smooths(model), character())
  expect_length(draw(model, wrap = FALSE, n = 8), 1L)
  expect_s3_class(draw(model, n = 8), "patchwork")
  expect_error(smooth_estimates(model), "No smooths in GJRM model")
})

test_that("direct GJRM estimates select smooths across equations", {
  for (repeated in c(FALSE, TRUE)) {
    model <- gjrm_selection_fixture(repeated)
    estimates <- smooth_estimates(model, n = 8)
    expect_s3_class(estimates, "smooth_estimates")
    expect_identical(unique(estimates$.smooth), smooths(model))
    expect_equal(nrow(estimates), 16L)
    for (select in list(smooths(model)[2], 2, c(FALSE, TRUE))) {
      selected <- smooth_estimates(model, select = select, n = 8)
      expect_equal(selected, estimates[9:16, names(selected)])
    }
    expect_equal(smooth_estimates(model, select = "s(", partial_match = TRUE,
      n = 8), estimates[seq_len(if (repeated) 8L else 16L), ])
    if (repeated) {
      expect_equal(smooth_estimates(model, select = "s.1", partial_match = TRUE,
        n = 8), estimates[9:16, ])
    }
    # Requested selection order does not reorder fitted smooths.
    expect_equal(smooth_estimates(model, select = 2:1, n = 8), estimates)
    expect_error(smooth_estimates(model, select = "missing"), "Failed to match")
    expect_error(smooth_estimates(model, select = c("s(x)", "missing")), "not found")
    expect_error(smooth_estimates(model, select = 3), "number of smooths")
    for (select in list(0, -1, 1.5, Inf, NA_integer_, c(TRUE, NA), NA_character_)) {
      expect_error(smooth_estimates(model, select = select), "positive integers")
    }
    expect_error(smooth_estimates(model, select = TRUE), "length\\(select\\)")
    expect_error(smooth_estimates(model, select = c("s(x)", "s(z)"),
      partial_match = TRUE), "single string")
    expect_error(smooth_estimates(model, select = list(1)), "not numeric")
    expect_error(smooth_estimates(model, select = c(FALSE, FALSE)), "No smooths selected")
  }
})

test_that("GJRM estimates use final coefficients and covariance at supplied rows", {
  model <- gjrm_selection_fixture()
  data <- data.frame(x = c(.8, .2, .8, .4), z = c(.3, .9, .3, .6))
  components <- gjrm_gamlss_components(model)
  for (overall in c(FALSE, TRUE)) {
    for (frequentist in c(FALSE, TRUE)) {
      estimates <- smooth_estimates(model, data = data, n = 2,
        overall_uncertainty = overall, frequentist = frequentist)
      for (i in seq_along(components)) {
        expected <- smooth_estimates(components[[i]], data = data,
          overall_uncertainty = overall, frequentist = frequentist)
        rows <- (i - 1L) * nrow(data) + seq_len(nrow(data))
        expect_equal(estimates$.estimate[rows], expected$.estimate)
        expect_equal(estimates$.se[rows], expected$.se)
        variable <- if (i == 1L) "x" else "z"
        expect_identical(estimates[[variable]][rows], data[[variable]])
        # Independently calculate the smooth-only result from the parent fit.
        if (!overall) {
          sm <- components[[i]]$smooth[[1]]
          X <- mgcv::PredictMat(sm, data)
          cols <- gjrm_gamlss_ind(model, i)[sm$first.para:sm$last.para]
          V <- if (frequentist) model$Ve else model$Vb
          expect_equal(estimates$.estimate[rows],
            drop(X %*% model$coefficients[cols]))
          expect_equal(estimates$.se[rows],
            sqrt(rowSums((X %*% V[cols, cols]) * X)))
        }
      }
    }
  }
  # Working GAM uncertainty must not leak into the prepared final components.
  altered <- model
  altered$gam1$coefficients[] <- 100
  altered$gam1$Vp <- altered$gam1$Ve <- diag(100, ncol(model$gam1$Vp))
  altered$gam1$Vc <- altered$gam1$Vp
  for (frequentist in c(FALSE, TRUE)) {
    expect_equal(smooth_estimates(altered, data = data, frequentist = frequentist),
      smooth_estimates(model, data = data, frequentist = frequentist))
  }
  expect_warning(corrected <- smooth_estimates(altered, select = 1, data = data,
    unconditional = TRUE), "Covariance corrected for smoothness uncertainty not available")
  expect_equal(corrected, smooth_estimates(model, select = 1, data = data))
  missing <- data
  missing$x[2] <- NA_real_
  estimates <- smooth_estimates(model, data = missing)
  expect_true(is.na(estimates$.estimate[2]))
  expect_true(all(is.finite(estimates$.estimate[5:8])))
  nested <- smooth_estimates(model, data = data, unnest = FALSE,
    interval = "simultaneous", n_sim = 100, seed = 19, frequentist = TRUE)
  for (i in seq_along(components)) {
    expected <- smooth_estimates(components[[i]], data = data, unnest = FALSE,
      interval = "simultaneous", n_sim = 100, seed = 19, frequentist = TRUE)
    expect_equal(nested$data[[i]], expected$data[[1]])
  }
})

test_that("saved GJRM estimates retain labels and intervals in either layout", {
  model <- gjrm_selection_fixture(TRUE)
  components <- gjrm_gamlss_components(model)
  labels <- c("Mean (mu)", "Standard deviation (sigma)")
  for (unnest in c(TRUE, FALSE)) {
    for (interval in list(NULL, "confidence", "simultaneous")) {
      estimates <- smooth_estimates(model, n = 8, unnest = unnest,
        interval = interval, ci_level = .9, n_sim = 100, seed = 17,
        interval_method = "gaussian", n_cores = 1)
      expect_identical(unique(estimates$.model), labels)
      flat <- if (unnest) estimates else tidyr::unnest(estimates, "data")
      for (i in seq_along(components)) {
        expected <- smooth_estimates(components[[i]], n = 8,
          interval = interval, ci_level = .9, n_sim = 100, seed = 17,
          interval_method = "gaussian", n_cores = 1)
        columns <- intersect(c(".estimate", ".se", ".crit", ".lower_ci", ".upper_ci"),
          names(expected))
        for (column in columns) {
          expect_equal(flat[[column]][(i - 1L) * 8L + 1:8], expected[[column]])
        }
      }
      saved <- unserialize(serialize(estimates, NULL))
      plots <- draw(saved)
      expect_length(plots, 2L)
      expect_identical(vapply(1:2, function(i) plots[[i]]$labels$subtitle,
        character(1)), labels)
      expect_identical(vapply(1:2, function(i) plots[[i]]$labels$title,
        character(1)), smooths(model))
      expect_silent(patchwork::patchworkGrob(plots))
      expect_identical(names(attr(estimates, "surface_support")), smooths(model))
      expect_identical(names(attr(attr(estimates, "coordinate_info"),
        "smooth_variables")), smooths(model))
    }
  }
  expect_error(smooth_estimates(model, interval = "confidence", ci_level = 1),
    "strictly between")
  expect_error(smooth_estimates(model, interval_method = "bootstrap"), "Bootstrap")
})

test_that("GJRM estimation skips parametric equations without losing labels", {
  d <- withr::with_seed(83, data.frame(x = runif(70), z = runif(70), y = rnorm(70)))
  for (formula in list(list(y ~ x, ~ s(z, k = 4)), list(y ~ s(x, k = 4), ~ 1))) {
    model <- GJRM::gamlss(formula, data = d)
    estimates <- smooth_estimates(model, n = 8)
    expect_equal(nrow(estimates), 8L)
    expect_identical(unique(estimates$.smooth), smooths(model))
    expected <- if (length(model$gam1$smooth)) "Mean (mu)" else "Standard deviation (sigma)"
    expect_identical(unique(estimates$.model), expected)
    expect_identical(draw(estimates)[[1]]$labels$subtitle, expected)
    expect_equal(smooth_estimates(model, select = 1, n = 8), estimates)
  }
  single <- GJRM::gamlss(list(y ~ s(x, k = 4)), data = d)
  expect_false(".model" %in% names(smooth_estimates(single)))
  expect_null(draw(smooth_estimates(single))[[1]]$labels$subtitle)
  multiple <- GJRM::gamlss(list(y ~ s(x, k = 4) + s(z, k = 4),
    ~ s(x, k = 4)), data = d)
  estimates <- smooth_estimates(multiple, n = 8)
  expect_identical(unique(estimates$.smooth), c("s(x)", "s(z)", "s.1(x)"))
  selected <- smooth_estimates(multiple, select = c(FALSE, TRUE, TRUE), n = 8)
  expect_identical(unique(selected$.smooth), smooths(multiple)[2:3])
  for (column in names(selected)) {
    expect_equal(selected[[column]], estimates[[column]][9:24])
  }
  expect_equal(smooth_estimates(multiple, select = "s.1(x)", n = 8),
    estimates[17:24, "z" != names(estimates)])
})

test_that("GJRM estimation forwards surface grids and coordinate controls", {
  d <- withr::with_seed(84, {
    d <- data.frame(x = runif(100, .1, 1), z = runif(100), a = runif(100))
    d$y <- sin(6 * d$x) + rnorm(100, sd = exp(.3 * d$z))
    d
  })
  fun <- function(x) log(x)
  model <- GJRM::gamlss(list(y ~ s(fun(x), k = 4), ~ s(z, a, k = 8)), data = d)
  env <- environment()
  expected <- gjrm_gamlss_components(model)
  data <- d[c(5, 2, 5, 9), ]
  rm(fun)
  envir <- new.env(parent = env)
  envir$fun <- function(x) log(x)
  estimates <- smooth_estimates(model, data = data, envir = envir,
    back_transform = TRUE, dist = 0, clip = TRUE)
  expect_equal(estimates[["fun(x)"]][1:4], log(data$x))
  expect_equal(estimates$a[5:8], data$a)
  expect_equal(estimates$z[5:8], data$z)
  plots <- draw(estimates, back_transform = TRUE)
  expect_equal(plots[[1]]$data$x, data$x)
  grid <- smooth_estimates(model, n = 7, n_2d = 4, n_3d = 3, n_4d = 2,
    envir = envir, dist = 0, back_transform = TRUE)
  expect_identical(as.integer(table(factor(grid$.smooth,
    levels = smooths(model)))), c(7L, 16L))
  for (i in seq_along(expected)) {
    component <- smooth_estimates(expected[[i]], n = 7, n_2d = 4,
      envir = envir, dist = 0, back_transform = TRUE)
    expect_equal(grid$.estimate[grid$.smooth == smooths(model)[i]], component$.estimate)
  }
  expect_identical(names(attr(grid, "surface_support")), smooths(model))
  expect_error(smooth_estimates(model, envir = list()), "environment")
})
