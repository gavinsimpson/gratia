# Independent native prediction oracles for area effects (not coefficients).
mrf_native_check <- function(model, data = NULL, select = 1L,
  overall = FALSE, unconditional = FALSE, frequentist = FALSE) {
  if (is.null(data)) {
    data <- data.frame(area = factor(mrf_models$areas,
      levels = mrf_models$areas), x = 0.4, z = 1,
      fac = factor("B", levels = c("B", "A")))
  }
  actual <- smooth_estimates(model, select = select, data = data,
    overall_uncertainty = overall, unconditional = unconditional,
    frequentist = frequentist)
  X <- predict(model, newdata = data, type = "lpmatrix", na.action = na.pass)
  sm <- model$smooth[[select]]
  idx <- seq.int(sm$first.para, sm$last.para)
  expected <- drop(X[, idx, drop = FALSE] %*% coef(model)[idx])
  expect_equal(unname(actual$.estimate), unname(expected), tolerance = 1e-10)
  if (!overall) {
    V <- vcov(model, unconditional = unconditional, freq = frequentist)
    Xi <- X[, idx, drop = FALSE]
    expected_se <- sqrt(pmax(0, rowSums((Xi %*% V[idx, idx, drop = FALSE]) * Xi)))
  } else {
    native <- predict(model, newdata = data, type = "iterms", se.fit = TRUE,
      unconditional = unconditional, na.action = na.pass)
    expected_se <- unname(native$se.fit[, sm$label])
  }
  expect_equal(actual$.se, expected_se, tolerance = 1e-10)
  expect_true(all(actual$.type == "MRF"))
  actual
}

test_that("MRF area estimates match native predictions across parameterizations", {
  for (name in c("full", "low", "penalty_model", "disconnected", "polygon", "bam")) {
    m <- mrf_models[[name]]
    for (overall in c(FALSE, TRUE)) mrf_native_check(m, overall = overall)
    mrf_native_check(m, unconditional = TRUE)
    mrf_native_check(m, frequentist = TRUE)
  }
  expect_false(is.null(mrf_models$low$smooth[[1]]$P))
  expect_gt(length(mrf_models$areas), length(smooth_coefs(mrf_models$low$smooth[[1]],
    model = mrf_models$low)))
})

test_that("MRF automatic grids cover the fitted area domain in factor order", {
  for (name in c("full", "low", "unobserved", "unobserved_full")) {
    m <- mrf_models[[name]]
    d <- smooth_data(m, id = 1, n = 2)
    expect_equal(as.character(d$area), mrf_models$areas)
    expect_identical(levels(d$area), mrf_models$areas)
    sm <- smooth_estimates(m, select = 1, n = 2)
    expect_equal(as.character(sm$area), mrf_models$areas)
    expect_true(all(is.finite(sm$.estimate)))
    expect_true(all(is.finite(sm$.se)))
    nested <- smooth_estimates(m, select = 1, unnest = FALSE)
    expect_equal(nested$data[[1]]$.estimate, sm$.estimate)
    bf <- basis(m, select = 1, n = 2)
    expect_setequal(as.character(bf$area), mrf_models$areas)
    X <- mgcv::PredictMat(m$smooth[[1]], d)
    for (j in seq_len(ncol(X))) {
      expect_equal(bf$.value[bf$.bf == j],
        X[, j] * coef(m)[m$smooth[[1]]$first.para + j - 1L], ignore_attr = TRUE)
    }
  }
  # This region exists only in the smooth's knots, not the fitting factor.
  expect_false(mrf_models$areas[8] %in% levels(mrf_models$unobserved$model$area))
})

test_that("MRF supplied rows preserve labels, order, repetition and missingness", {
  d <- data.frame(area = c("r7", NA, "r1", "r7"), x = 0.4)
  actual <- mrf_native_check(mrf_models$full, d)
  expect_identical(as.character(actual$area), d$area)
  expect_identical(unname(is.na(actual$.estimate)), is.na(d$area))
  expect_identical(is.na(actual$.se), is.na(d$area))
  for (areas in list(c(NA_character_, NA_character_))) {
    sm <- smooth_estimates(mrf_models$full, select = 1,
      data = data.frame(area = areas))
    expect_length(sm$.estimate, length(areas))
    expect_true(all(is.na(sm$.estimate)))
  }
  expect_error(smooth_estimates(mrf_models$full, select = 1,
    data = data.frame(area = character())), "at least one row")
  expect_error(smooth_estimates(mrf_models$full, select = 1,
    data = data.frame(area = c("r1", "elsewhere"))), class = "gratia_mrf_area_error")
  # Character and differently ordered factor inputs identify the same regions.
  expect_equal(smooth_estimates(mrf_models$full, select = 1,
    data = data.frame(area = factor(d$area, levels = c("r7", "r1"))))$.estimate,
    actual$.estimate)
})

test_that("MRF transformed factors retain evaluated and raw area coordinates", {
  m <- mrf_models$transformed
  sm <- mrf_native_check(m)
  expect_equal(as.character(sm[["factor(area)"]]), mrf_models$areas)
  # mgcv resets formula environments, so recover local raw data explicitly.
  sm <- smooth_estimates(m, back_transform = TRUE,
    envir = list2env(list(d = mrf_models$data), parent = environment(formula(m))))
  expect_equal(as.character(sm[["factor(area)"]]), mrf_models$areas)
  expect_s3_class(draw(sm, back_transform = TRUE), "patchwork")
})

test_that("MRF by variables preserve their native multiplier and grouping", {
  for (level in c("B", "A")) {
    d <- data.frame(area = mrf_models$areas, fac = factor(level, levels = c("B", "A")))
    which <- if (level == "B") 1L else 2L
    mrf_native_check(mrf_models$by, data = d, select = which)
  }
  d <- data.frame(area = mrf_models$areas, z = seq(0.5, 1.5, length.out = 8))
  actual <- mrf_native_check(mrf_models$numeric_by, data = d)
  unit <- smooth_estimates(mrf_models$numeric_by,
    data = transform(d, z = 1), overall_uncertainty = FALSE)
  expect_equal(actual$.estimate, unit$.estimate * d$z)
  expect_equal(actual$.se, unit$.se * d$z)
  expect_true(all(smooth_data(mrf_models$numeric_by, id = 1)$z == 1))
})

test_that("MRF plots show labelled area effects and existing interval semantics", {
  m <- mrf_models$full
  sm <- smooth_estimates(m, select = 1) |> add_confint(coverage = 0.8)
  p <- assemble(m, select = 1, ci_level = 0.8, rug = FALSE)[[1]]
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$.estimate, sm$.estimate)
  expect_equal(p$data$.lower_ci, sm$.lower_ci)
  expect_equal(p$data$.upper_ci, sm$.upper_ci)
  expect_identical(levels(p$data$area), mrf_models$areas)
  expect_true(all(p$data$.type == "MRF"))
  expect_s3_class(p$data, "mrf_smooth")
  expect_false(inherits(p$data, "random_effect"))
  expect_identical(unname(vapply(p$layers, function(l) class(l$geom)[!grepl("^GeomGratia", class(l$geom))][1], character(1))),
    c("GeomLinerange", "GeomPoint"))
  built <- ggplot_build(p)
  expect_equal(built$data[[1]]$ymin, unname(sm$.lower_ci))
  expect_equal(built$data[[1]]$ymax, unname(sm$.upper_ci))
  expect_identical(built$layout$panel_scales_x[[1]]$get_limits(), mrf_models$areas)
  p2 <- assemble(m, select = 1, ci_level = 0.8, rug = FALSE,
    constant = 2, fun = exp)[[1]]
  expect_equal(p2$data$.estimate, exp(sm$.estimate + 2))
  expect_equal(p2$data$.lower_ci, exp(sm$.lower_ci + 2))
  expect_equal(p2$data$.upper_ci, exp(sm$.upper_ci + 2))
  expect_s3_class(draw(sm), "patchwork")
  expect_s3_class(draw(smooth_estimates(m, select = 1, unnest = FALSE)), "patchwork")
  expect_s3_class(draw(m, select = 1), "patchwork")
  expect_s3_class(draw(m, select = 1, wrap = FALSE)[[1]], "ggplot")
  expect_length(assemble(m, parametric = FALSE, rug = FALSE), 2)
  expect_s3_class(draw(structure(list(gam = m, lme = NULL), class = "gamm"),
    select = 1), "patchwork")
})

test_that("MRF descriptive QQ plots retain identity and expected quantiles", {
  p <- assemble(mrf_models$low, rug = FALSE, mrf_plot = "qq")[[1]]
  b <- ggplot_build(p)
  expect_true(all(p$data$.type == "MRF"))
  expect_equal(b$data[[1]]$x, qnorm(ppoints(8)))
  expect_equal(b$data[[1]]$y, unname(sort(p$data$.estimate)))
  expect_true(all(is.finite(b$data[[2]]$slope)))
  expect_s3_class(draw(smooth_estimates(mrf_models$low), mrf_plot = "qq"), "patchwork")
  missing <- smooth_estimates(mrf_models$low,
    data = data.frame(area = c("r1", NA, "r2")))
  expect_silent(ggplot_build(draw_smooth_estimates(add_confint(missing), mrf_plot = "qq")))
  empty <- smooth_estimates(mrf_models$low,
    data = data.frame(area = NA_character_))
  expect_error(draw_smooth_estimates(add_confint(empty), mrf_plot = "qq"),
    "No finite MRF effects")
  constant <- smooth_estimates(mrf_models$low)
  constant$.estimate <- rep(0, nrow(constant))
  expect_silent(ggplot_build(draw_smooth_estimates(add_confint(constant), mrf_plot = "qq")))
  p0 <- assemble(mrf_models$low, rug = FALSE, mrf_plot = "qq", qq_line = FALSE)[[1]]
  expect_length(p0$layers, 1)
  expect_error(assemble(mrf_models$low, mrf_plot = "invalid"), "arg")
  # This option does not alter unrelated ordinary smooths.
  a <- assemble(mrf_models$full, select = 2, rug = FALSE, mrf_plot = "qq")[[1]]
  z <- assemble(mrf_models$full, select = 2, rug = FALSE)[[1]]
  expect_equal(a$data, z$data)
  expect_identical(vapply(a$layers, function(l) class(l$geom)[!grepl("^GeomGratia", class(l$geom))][1], character(1)),
    vapply(z$layers, function(l) class(l$geom)[!grepl("^GeomGratia", class(l$geom))][1], character(1)))
})

test_that("grouped MRF panels keep area effects and QQ groups separate", {
  for (type in c("effects", "qq")) {
    p <- assemble(mrf_models$by, parametric = FALSE, grouped_by = TRUE,
      rug = FALSE, mrf_plot = type)[[1]]
    b <- ggplot_build(p)
    expect_equal(nrow(b$layout$layout), 2)
    expect_identical(p$labels$title, "s(area)")
    expect_identical(p$labels$subtitle, "By: fac")
    for (panel in 1:2) {
      actual <- b$data[[1]][as.integer(b$data[[1]]$PANEL) == panel, ]
      expected <- p$data$.estimate[p$data$fac == levels(p$data$fac)[panel]]
      expect_length(actual$y, 8)
      expect_equal(actual$y, unname(if (type == "qq") sort(expected) else expected))
    }
    expect_s3_class(draw(smooth_estimates(mrf_models$by), grouped_by = TRUE,
      mrf_plot = type), "patchwork")
  }
})

test_that("basic MRF plots have visual regression coverage", {
  skip_on_cran()
  vdiffr::expect_doppelganger("MRF area effects",
    draw(mrf_models$low, rug = FALSE))
  vdiffr::expect_doppelganger("MRF descriptive QQ",
    draw(mrf_models$low, rug = FALSE, mrf_plot = "qq"))
  vdiffr::expect_doppelganger("MRF grouped area effects",
    draw(mrf_models$by, parametric = FALSE, grouped_by = TRUE, rug = FALSE))
})
