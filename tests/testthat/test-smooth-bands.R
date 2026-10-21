test_that("smooth intervals are opt-in and credible aliases pointwise confidence", {
  model <- smooth_band_model
  plain <- smooth_estimates(model, n = 9)
  expect_false(any(c(".crit", ".lower_ci", ".upper_ci") %in% names(plain)))
  pointwise <- smooth_estimates(model, n = 9, interval = "confidence", ci_level = 0.9)
  credible <- smooth_estimates(model, n = 9, interval = "credible", ci_level = 0.9)
  expect_identical(credible, pointwise)
  expect_equal(pointwise$.crit, rep(qnorm(0.95), nrow(pointwise)))
  expect_equal(pointwise$.lower_ci, pointwise$.estimate - qnorm(0.95) * pointwise$.se)
  expect_identical(confint(model, parm = NULL, type = "credible", n = 9),
    confint(model, parm = NULL, type = "confidence", n = 9))
  aliases <- assemble(model, interval = "credible", n = 9, rug = FALSE)
  default <- assemble(model, interval = "confidence", n = 9, rug = FALSE)
  expect_equal(lapply(aliases, `[[`, "data"), lapply(default, `[[`, "data"))
})

test_that("smooth simultaneous bands match independent covariance and uncertainty oracles", {
  model <- smooth_band_model
  nd <- data.frame(x = seq(0.1, 0.9, length.out = 11), z = 0.4, f = model$model$f[1])
  expect_false(is.null(model$Vc))
  expect_gt(max(abs(model$Vp - model$Ve)), 1e-6)
  expect_gt(max(abs(model$Vp - model$Vc)), 1e-6)
  for (overall in c(FALSE, TRUE)) for (frequentist in c(FALSE, TRUE)) {
    for (unconditional in c(FALSE, TRUE)) {
      reference <- smooth_band_reference(model, "s(x)", nd, overall,
        frequentist, unconditional)
      actual <- smooth_estimates(model, select = "s(x)", data = nd,
        interval = "simultaneous", ci_level = 0.9, seed = 314, n_sim = 200,
        overall_uncertainty = overall, frequentist = frequentist,
        unconditional = unconditional)
      expect_smooth_band_reference(actual, reference)
      ci <- confint(model, parm = "s(x)", data = nd, type = "simultaneous",
        level = 0.9, seed = 314, nsim = 200, overall_uncertainty = overall,
        frequentist = frequentist, unconditional = unconditional)
      expect_smooth_band_reference(ci, reference)
    }
  }
})

test_that("coverage is per fitted term and independent of selection order or grouping", {
  for (model in list(smooth_band_model, su_m_factor_by_x2)) {
    labels <- smooths(model)
    all <- smooth_estimates(model, interval = "simultaneous", n = 12,
      n_sim = 120, seed = 5)
    reverse <- smooth_estimates(model, select = rev(labels), interval = "simultaneous",
      n = 12, n_sim = 120, seed = 5)
    expect_equal(reverse, all)
    for (label in labels) {
      one <- smooth_estimates(model, select = label, interval = "simultaneous",
        n = 12, n_sim = 120, seed = 5)
      rows <- all[all$.smooth == label, ]
      for (column in c(".estimate", ".se", ".crit", ".lower_ci", ".upper_ci")) {
        expect_identical(rows[[column]], one[[column]])
      }
    }
  }
  panels <- assemble(su_m_factor_by_x2, grouped = c("factor-by", "sz", "fs"), rug = FALSE,
    interval = "simultaneous", n = 12, n_sim = 120, seed = 5)
  values <- smooth_estimates(su_m_factor_by_x2, n = 12, interval = "simultaneous",
    n_sim = 120, seed = 5)
  expect_equal(panels[[1]]$data$.lower_ci, values$.lower_ci)
  expect_equal(panels[[1]]$data$.upper_ci, values$.upper_ci)
})

test_that("model adapters and multiple linear predictors retain smooth-only band semantics", {
  for (model in list(m_gam, m_bam, m_gamm, m_gamm4, m_gaulss, m_ziplss)) {
    gam_model <- if (inherits(model, "gamm4")) model$gam else if (inherits(model, "gamm")) {
      model$gam
    } else model
    label <- utils::tail(smooths(gam_model), 1)
    nd <- gam_model$model[1:8, , drop = FALSE]
    for (overall in c(FALSE, TRUE)) {
      actual <- smooth_estimates(model, select = label, data = nd,
        interval = "simultaneous", n_sim = 200, seed = 314, ci_level = 0.9,
        overall_uncertainty = overall)
      reference <- smooth_band_reference(gam_model, label, nd, overall)
      expect_smooth_band_reference(actual, reference)
    }
    expect_length(draw(model, select = label, n = 8, rug = FALSE, wrap = FALSE,
      interval = "simultaneous", n_sim = 120, seed = 5), 1)
  }
})

test_that("tensor, RE and factor smooths retain all slices in one coverage family", {
  for (model in list(su_m_trivar_te, su_m_trivar_t2, su_m_bivar_ti,
      m_fs, m_sz, rm1, rm2, tensor_re_four, tensor_re_slices, tensor_re_categories)) {
    actual <- smooth_estimates(model, interval = "simultaneous", n = 6,
      n_2d = 4, n_3d = 3, n_sim = 120, seed = 7)
    for (label in unique(actual$.smooth)) {
      rows <- actual[actual$.smooth == label, ]
      expect_length(unique(rows$.crit), 1)
      expect_true(all(is.finite(rows$.lower_ci)))
    }
  }
  ci <- confint(su_m_trivar_te, parm = NULL, type = "simultaneous",
    n_2d = 4, n_3d = 3, nsim = 120, seed = 7)
  expect_equal(nrow(ci), 4 * 4 * 3)
  expect_length(unique(ci$.crit), 1)
})

test_that("transformed coordinates and supplied missing rows are preserved", {
  model <- smooth_band_transformed
  nd <- smooth_band_data[1:9, ]
  nd$x[3] <- NA_real_
  for (label in smooths(model)) {
    actual <- smooth_estimates(model, select = label, data = nd,
      interval = "simultaneous", n_sim = 200, seed = 314, ci_level = 0.9)
    reference <- smooth_band_reference(model, label, nd, overall = TRUE)
    expect_smooth_band_reference(actual, reference)
    expect_true(is.na(actual$.lower_ci[3]))
    expect_equal(nrow(actual), nrow(nd))
  }
  values <- smooth_estimates(model, select = 1, back_transform = TRUE,
    interval = "simultaneous", n = 10, n_sim = 120, seed = 7)
  expect_false(is.null(attr(values, "coordinate_info")))
  p <- draw(values, back_transform = TRUE)
  expect_equal(p$data$.lower_ci, values$.lower_ci)
  expect_equal(p$labels$x, "x")
  # Missing rows unrelated to the selected smooth do not remove its band.
  nd$x <- NA_real_
  a <- smooth_estimates(smooth_band_model, select = "s(z)", data = nd,
    interval = "simultaneous", n_sim = 120, seed = 7)
  expect_true(all(is.finite(a$.lower_ci)))
})

test_that("precomputed bounds survive drawing and model-free requests are honest", {
  values <- smooth_estimates(smooth_band_model, select = "s(x)", n = 10,
    interval = "simultaneous", n_sim = 120, seed = 7)
  p <- draw(values, interval = "simultaneous")
  expect_identical(p$data$.lower_ci, values$.lower_ci)
  expect_identical(p$data$.upper_ci, values$.upper_ci)
  expect_match(p$labels$caption, "simultaneous band", all = FALSE)
  nested <- smooth_estimates(smooth_band_model, select = "s(x)", n = 10,
    interval = "simultaneous", n_sim = 120, seed = 7, unnest = FALSE)
  expect_identical(draw(nested)$data$.lower_ci, values$.lower_ci)
  plain <- smooth_estimates(smooth_band_model, select = "s(x)", n = 10)
  expect_error(draw(plain, interval = "simultaneous"), "require the fitted model")
  expect_error(draw(values, interval = "confidence"), "differs")
  missing_bounds <- values
  missing_bounds$.lower_ci <- NULL
  expect_error(draw(missing_bounds), "interval-bound columns")
  expect_error(draw(missing_bounds, interval = "simultaneous"), "interval-bound columns")
  nested_missing <- nested
  nested_missing$data[[1]]$.upper_ci <- NULL
  expect_error(draw(nested_missing), "interval-bound columns")
  # User-supplied bounds without model metadata remain drawable unchanged.
  plain$.lower_ci <- rep(-100, nrow(plain))
  plain$.upper_ci <- rep(100, nrow(plain))
  expect_identical(draw(plain)$data$.lower_ci, plain$.lower_ci)
})

test_that("model drawing preserves bands, transformations and pointwise parametric panels", {
  model <- smooth_band_model
  a <- smooth_estimates(model, select = "s(x)", n = 12,
    interval = "simultaneous", n_sim = 120, seed = 7)
  p <- assemble(model, select = "s(x)", n = 12, rug = FALSE,
    interval = "simultaneous", n_sim = 120, seed = 7)[[1]]
  expect_equal(p$data$.lower_ci, a$.lower_ci)
  expect_equal(draw(model, select = "s(x)", n = 12, rug = FALSE, wrap = FALSE,
    interval = "simultaneous", n_sim = 120, seed = 7)[[1]]$data$.upper_ci,
    a$.upper_ci)
  for (direction in c(1, -1)) {
    fun <- function(x) direction * exp(x)
    p <- assemble(model, select = "s(x)", n = 12, rug = FALSE,
      interval = "simultaneous", n_sim = 120, seed = 7,
      constant = 1, fun = fun, scales = "fixed")[[1]]
    expect_equal(p$data$.estimate, fun(a$.estimate + 1))
    expect_equal(p$data$.lower_ci, pmin(fun(a$.lower_ci + 1), fun(a$.upper_ci + 1)))
    expect_equal(p$data$.upper_ci, pmax(fun(a$.lower_ci + 1), fun(a$.upper_ci + 1)))
    limits <- ggplot_build(p)$layout$panel_params[[1]]$y.range
    expect_lte(limits[1], min(p$data$.lower_ci))
    expect_gte(limits[2], max(p$data$.upper_ci))
  }
  panels <- assemble(model, n = 10, rug = FALSE, interval = "simultaneous",
    n_sim = 120, seed = 7)
  expect_match(panels[[length(panels)]]$labels$caption, "pointwise.*parametric")
  expect_match(panels[[1]]$labels$caption, "simultaneous")
})

test_that("smooth band calibration preserves RNG and validates unsupported requests", {
  model <- smooth_band_model
  withr::local_seed(11)
  before <- .Random.seed
  a <- smooth_estimates(model, interval = "simultaneous", n = 10,
    n_sim = 120, seed = 7)
  expect_identical(.Random.seed, before)
  expect_identical(a, smooth_estimates(model, interval = "simultaneous", n = 10,
    n_sim = 120, seed = 7))
  for (value in list(0, -1, NA_real_, c(0.5, 0.9), 1)) {
    expect_error(smooth_estimates(model, interval = "confidence", ci_level = value), "ci_level")
  }
  for (argument in c("n_sim", "n_cores", "seed")) {
    for (value in list(NA_real_, c(1, 2), 1.2)) {
      args <- list(model, interval = "simultaneous", n = 10)
      args[[argument]] <- value
      expect_error(do.call(smooth_estimates, args), argument)
    }
  }
  expect_error(smooth_estimates(model, interval = "incorrect"), "arg")
  expect_s3_class(draw(m_scam, interval = "simultaneous", n = 10,
    n_sim = 120, seed = 7), "patchwork")
  # Entirely missing supplied grids retain all positions with NA bounds.
  nd <- data.frame(x = rep(NA_real_, 3))
  a <- smooth_estimates(model, select = "s(x)", data = nd,
    interval = "simultaneous", n_sim = 120, seed = 7)
  expect_true(all(is.na(a$.lower_ci)))
  expect_equal(nrow(a), 3)
})

test_that("simultaneous smooth visual regressions cover curves, factor grouping and slices", {
  expect_doppelganger("simultaneous smooth curve", draw(smooth_band_model,
    select = "s(x)", interval = "simultaneous", n = 35, n_sim = 300, seed = 314,
    rug = FALSE))
  expect_doppelganger("simultaneous factor by smooths", draw(su_m_factor_by_x2,
    interval = "simultaneous", grouped = c("factor-by", "sz", "fs"), n = 35, n_sim = 300,
    seed = 314, rug = FALSE, parametric = FALSE))
  expect_doppelganger("simultaneous tensor slices", draw(su_m_trivar_te,
    interval = "simultaneous", n_2d = 8, n_3d = 3, n_sim = 300,
    seed = 314, rug = FALSE))
})

test_that("parallel evaluation leaves seeded smooth calibration unchanged", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  baseline <- smooth_estimates(smooth_band_model, interval = "simultaneous",
    n = 10, n_sim = 120, seed = 7)
  withr::defer(mirai::daemons(0))
  # Let exit handlers finish writing worker coverage before the process exits.
  mirai::daemons(1, dispatcher = FALSE, autoexit = NA)
  expect_identical(smooth_estimates(smooth_band_model, interval = "simultaneous",
    n = 10, n_sim = 120, seed = 7), baseline)
})

test_that("zero uncertainty and absent RNG state are handled without fabricated bands", {
  model <- smooth_band_transformed
  nd <- data.frame(x = rep(0, 3), z = seq(0.1, 0.9, length.out = 3))
  withr::local_preserve_seed()
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  actual <- smooth_estimates(model, select = "s(z):x", data = nd,
    interval = "simultaneous", n_sim = 120, seed = 7)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_equal(actual$.se, rep(0, 3))
  expect_equal(actual$.crit, rep(0, 3))
  expect_equal(actual$.lower_ci, actual$.estimate)
  expect_equal(actual$.upper_ci, actual$.estimate)
})

test_that("surface masking and soap boundary rows are excluded from calibration", {
  for (model in list(su_m_bivar, m_soap, m_soap_sep_known)) {
    estimates <- smooth_estimates(model, interval = "simultaneous", n_2d = 6,
      n_sim = 120, seed = 7, dist = 0.1, clip = FALSE)
    for (rows in split_smooth_components(estimates)) {
      valid <- is.finite(rows$.estimate) & is.finite(rows$.se)
      expect_true(any(valid))
      expect_true(all(is.finite(rows$.lower_ci[valid])))
      expect_true(all(is.na(rows$.lower_ci[!valid])))
      expect_length(unique(rows$.crit), 1)
    }
  }
})

test_that("categorical and four-dimensional tensor bands agree with native predictions", {
  for (model in list(tensor_re_four, tensor_re_slices, tensor_re_categories)) {
    nd <- model$model[1:9, , drop = FALSE]
    label <- smooths(model)[1]
    actual <- smooth_estimates(model, select = label, data = nd,
      interval = "simultaneous", ci_level = 0.9, n_sim = 200, seed = 314,
      overall_uncertainty = FALSE)
    reference <- smooth_band_reference(model, label, nd)
    expect_smooth_band_reference(actual, reference)
  }
  expect_doppelganger("simultaneous categorical tensor intervals", draw(tensor_re_categories,
    interval = "simultaneous", n_sim = 300, seed = 314, rug = FALSE))
})

test_that("known soap offsets stay in centres and out of coefficient deviations", {
  model <- m_soap_sep_known
  sm <- model$smooth[[1]]
  d <- model$model[1:8, ]
  X <- mgcv::PredictMat(sm, d)
  cols <- sm$first.para:sm$last.para
  expected <- drop(X %*% coef(model)[cols]) + attr(X, "offset")
  expect_true(any(abs(attr(X, "offset")) > 0))
  actual <- smooth_estimates(model, select = 1, data = d, interval = "simultaneous",
    overall_uncertainty = FALSE, ci_level = 0.9, n_sim = 200, seed = 314)
  actual <- actual[!actual$.bndry, ]
  expect_equal(actual$.estimate, expected, tolerance = 1e-7)
  V <- model$Vp
  draws <- withr::with_seed(314, mvnfast::rmvn(200, rep(0, ncol(V)), V, ncores = 1))
  se <- sqrt(rowSums((X %*% V[cols, cols]) * X))
  errors <- draws[, cols] %*% t(X)
  maxima <- apply(abs(sweep(errors, 2, se, `/`)), 1, max)
  critical <- unname(quantile(maxima, 0.9, type = 8))
  expect_equal(actual$.crit, rep(critical, nrow(d)))
  expect_equal(actual$.lower_ci, expected - critical * se, tolerance = 1e-7)
})
