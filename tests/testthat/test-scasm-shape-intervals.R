scasm_shape_fixture <- function(tensor = FALSE) {
  withr::with_seed(801, {
    d <- data.frame(x = runif(150), z = runif(150))
    d$y <- exp(d$x) + d$z + rnorm(150, sd = .2)
    form <- if (tensor) {
      y ~ te(x, z, bs = c("sc", "bs"), k = c(5, 5),
        xt = list(c("m+", "c+"), NULL), np = FALSE)
    } else y ~ s(x, bs = "sc", xt = c("m+", "c+"), k = 6)
    mgcv::scasm(form, data = d, bs = 20)
  })
}

test_that("shape intervals transform an explicitly chosen initial source", {
  m <- scasm_shape_fixture()
  for (source in c("gaussian", "bootstrap")) {
    initial <- smooth_estimates(m, n = 21, interval = "confidence",
      interval_method = source)
    expected <- shape_intervals(initial$.estimate, initial$.lower_ci,
      initial$.upper_ci, data.frame(x = initial$x), list(x = c("m+", "c+")),
      interval = "pointwise")
    expect_warning(a <- smooth_estimates(m, n = 21, interval = "confidence",
      interval_method = "shape", shape_source = source), "pointwise calibration")
    expect_equal(a$.lower_ci, expected$.lower_ci)
    expect_equal(a$.upper_ci, expected$.upper_ci)
    expect_equal(a$.estimate, initial$.estimate, tolerance = 1e-10)
    expect_equal(a$.se, initial$.se)
    expect_true(all(is.na(a$.crit)))
    info <- attr(a, "interval_info")
    expect_identical(info$method, "shape")
    expect_identical(info$source, source)
    expect_identical(info$pointwise_calibration, "not guaranteed")
    expect_identical(info$domain, "evaluation grid")
    expect_identical(info$shape, list(`1` = list(x = c("m+", "c+"))))
    expect_identical(info$correction, if (source == "bootstrap") "svar.only" else "none")
  }
})

test_that("simultaneous shape intervals retain reproducibility and source options", {
  m <- scasm_shape_fixture()
  for (frequentist in c(FALSE, TRUE)) {
    for (unconditional in c(FALSE, TRUE)) {
      initial <- smooth_estimates(m, n = 15, interval = "simultaneous",
        interval_method = "gaussian", n_sim = 100, seed = 41,
        frequentist = frequentist, unconditional = unconditional)
      before <- .Random.seed
      expect_silent(a <- smooth_estimates(m, n = 15,
        interval = "simultaneous", interval_method = "shape", n_sim = 100,
        seed = 41, frequentist = frequentist, unconditional = unconditional))
      expect_identical(.Random.seed, before)
      expected <- shape_intervals(initial$.estimate, initial$.lower_ci,
        initial$.upper_ci, data.frame(x = initial$x), list(x = c("m+", "c+")))
      expect_equal(a$.lower_ci, expected$.lower_ci)
      expect_equal(a$.upper_ci, expected$.upper_ci)
      expect_lte(max(a$.upper_ci - a$.lower_ci),
        max(initial$.upper_ci - initial$.lower_ci) + 1e-10)
    }
  }
  expect_error(smooth_estimates(m, interval = "simultaneous",
    interval_method = "shape", shape_source = "bootstrap"), "Simultaneous bootstrap")
  expect_error(smooth_estimates(m, interval = "confidence",
    interval_method = "shape", shape_source = "bootstrap", frequentist = TRUE),
    "require frequentist = FALSE")
  expect_error(smooth_estimates(m, interval = "confidence",
    interval_method = "shape", shape_source = "bootstrap", unconditional = TRUE),
    "require frequentist = FALSE")
})

test_that("shape captions omit interval details and retain their metadata", {
  object <- data.frame(.smooth = "s(x)", .type = "B spline", .by = NA_character_)
  attr(object, "shape_constraints") <- list(list(label = "s(x)", tensor = FALSE,
    margins = list(list(variables = "x", basis = "B spline", codes = "m+",
      requested = TRUE, retained = TRUE))))
  basis_caption <- prepare_smooth_labels(object, x_var = "x")$caption
  for (source in c("gaussian", "bootstrap")) {
    for (interval in c("confidence", "simultaneous")) {
      info <- list(method = "shape", source = source, interval = interval,
        level = 0.95, domain = "evaluation grid")
      attr(object, "interval_info") <- info
      expect_identical(prepare_smooth_labels(object, x_var = "x")$caption,
        basis_caption)
      expect_identical(attr(object, "interval_info"), info)
    }
  }
})

test_that("safe tensor shape intervals use the complete rectangular grid", {
  m <- scasm_shape_fixture(TRUE)
  a <- smooth_estimates(m, n_2d = 7, interval = "simultaneous",
    interval_method = "shape", n_sim = 50, seed = 2, dist = 0)
  x <- sort(unique(a$x))
  for (nm in c(".estimate", ".lower_ci", ".upper_ci")) {
    for (z in unique(a$z)) {
      v <- a[[nm]][a$z == z]
      expect_true(all(diff(v) >= -1e-10))
      expect_true(all(diff(diff(v) / diff(x)) >= -1e-9))
    }
  }
  expect_silent(p <- draw(m, n_2d = 6, interval = "simultaneous",
    interval_method = "shape", n_sim = 50, seed = 2, dist = 0, rug = FALSE))
  expect_s3_class(p, "ggplot")
  expect_identical(p[[1]]$labels$caption,
    scasm_shape_plot_caption(a, "Basis: Tensor product"))
  expect_error(smooth_estimates(m, data = m$model[1:20, ],
    interval = "simultaneous", interval_method = "shape", n_sim = 20),
    "complete Cartesian grid")
  expect_error(smooth_estimates(m, n_2d = 9, dist = .0001,
    interval = "simultaneous", interval_method = "shape", n_sim = 20),
    "complete finite evaluation grid")
})

test_that("saved nested and subset shape intervals are drawn without reconstruction", {
  m <- scasm_shape_fixture()
  a <- smooth_estimates(m, n = 15, interval = "simultaneous",
    interval_method = "shape", n_sim = 50, seed = 7)
  nested <- smooth_estimates(m, n = 15, interval = "simultaneous",
    interval_method = "shape", n_sim = 50, seed = 7, unnest = FALSE)
  expect_identical(nested$data[[1]]$.lower_ci, a$.lower_ci)
  expect_silent(draw(nested))
  subset <- a[c(1, 3, 5, 9, 15), ]
  expect_identical(attr(subset, "interval_info"), attr(a, "interval_info"))
  expect_silent(draw(subset))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(a, path)
  expect_identical(readRDS(path), a)
  ci <- confint(m, parm = "s(x)", n = 15, type = "simultaneous",
    interval_method = "shape", nsim = 50, seed = 7)
  expect_equal(ci$.lower_ci, a$.lower_ci)
  expect_equal(ci$.upper_ci, a$.upper_ci)
  expect_identical(attr(ci, "interval_info"), attr(a, "interval_info"))
  plots <- assemble(m, n = 15, interval = "simultaneous",
    interval_method = "shape", n_sim = 50, seed = 7, rug = FALSE)
  expect_length(plots, 1)
  expect_identical(plots[[1]]$labels$caption,
    scasm_shape_plot_caption(a, "Basis: B spline"))
  replaced <- add_confint(a)
  expect_identical(attr(replaced, "interval_info")$method, "gaussian")
  expect_null(attr(replaced, "interval_info")$shape)
  expect_null(attr(replaced, "interval_info")$coverage)
  expect_false(grepl("Shape-enforced", paste(deparse(draw(replaced)[[1]]$labels$caption), collapse = " ")))
  expect_error(draw(a, fun = exp), "effect shifts and transformations")
  expect_error(add_constant(a, 1), "effect shifts and transformations")
  expect_error(transform_fun(a, exp), "effect shifts and transformations")
  expect_error(confint(m, parm = "s(x)", type = "simultaneous",
    interval_method = "shape", nsim = 20, shift = TRUE), "shift and transform")
})

test_that("automatic shape enforcement rejects unjustified restrictions", {
  m <- scasm_shape_fixture()
  expect_error(smooth_estimates(m, interval_method = "shape"), "Request an interval")
  expect_error(smooth_estimates(m, interval_method = "shape", interval = "confidence",
    overall_uncertainty = TRUE), "component-only")
  ordinary <- mgcv::gam(y ~ s(x, k = 6), data = m$model)
  expect_error(smooth_estimates(ordinary, interval = "confidence",
    interval_method = "shape"), "retained named scasm")
  sm <- m$smooth[[1]]
  info <- smooth_shape_constraints(m, warn = FALSE)[[1]]
  sm$knots[5] <- sm$knots[5] + .01
  expect_error(scasm_shape_spec(m, sm, info), "irregular knots")
  sm <- m$smooth[[1]]
  sm$by <- "z"
  expect_error(scasm_shape_spec(m, sm, info), "numeric by")
  expect_warning(expect_error(smooth_estimates(m,
    data = data.frame(x = c(-1, .5)), interval = "confidence",
    interval_method = "shape"), "fitted B-spline domain"), "pointwise calibration")
  tensor <- scasm_shape_fixture(TRUE)
  tensor$smooth[[1]]$XP <- list(diag(5), diag(5))
  expect_error(smooth_estimates(tensor, interval = "confidence",
    interval_method = "shape"), "nonnegative B-spline")
  info$margins[[1]]$codes <- character()
  expect_error(scasm_shape_spec(m, m$smooth[[1]], info), "named restrictions")
  expect_silent(empty <- smooth_estimates(m, data = m$model[FALSE, ],
    interval = "simultaneous", interval_method = "shape", n_sim = 20))
  expect_equal(nrow(empty), 0)
})

test_that("factor by smooths and multiple predictors retain component identity", {
  d <- withr::with_seed(43, {
    x <- rep(seq(.01, .99, length.out = 60), 2)
    data.frame(x = x, z = runif(120), fac = factor(rep(c("a", "b"), each = 60)),
      y = x + rnorm(120, sd = .3))
  })
  by <- mgcv::scasm(y ~ fac + s(x, by = fac, bs = "sc", xt = "m+", k = 5), data = d)
  a <- smooth_estimates(by, n = 12, interval = "simultaneous",
    interval_method = "shape", n_sim = 50, seed = 8)
  expect_length(unique(a$.smooth), 2)
  for (label in unique(a$.smooth)) {
    expect_true(all(diff(a$.lower_ci[a$.smooth == label]) >= -1e-10))
  }
  expect_silent(draw(a, grouped_by = TRUE))
  multi <- mgcv::scasm(list(y ~ s(x, bs = "sc", xt = "m+", k = 5),
    ~s(z, bs = "sc", xt = "m+", k = 5)), data = d, family = mgcv::gaulss())
  a <- smooth_estimates(multi, n = 12, interval = "simultaneous",
    interval_method = "shape", n_sim = 50, seed = 8)
  expect_length(unique(a$.model), 2)
  expect_length(attr(a, "interval_info")$shape, 2)
  expect_silent(draw(a))
})

test_that("transformed covariates enforce shape in evaluated model coordinates", {
  d <- data.frame(x = exp(seq(.01, 1, length.out = 100)))
  d$y <- withr::with_seed(92, log(d$x) + rnorm(100, sd = .1))
  m <- mgcv::scasm(y ~ s(log(x), bs = "sc", xt = "m+", k = 5), data = d,
    control = list(keepData = TRUE))
  a <- smooth_estimates(m, data = data.frame(x = exp(seq(.01, 1, length.out = 15))),
    back_transform = TRUE,
    interval = "simultaneous", interval_method = "shape", n_sim = 50, seed = 3)
  expect_true(all(diff(a$.lower_ci) >= -1e-10))
  expect_identical(attr(a, "interval_info")$coordinate_scale, "model")
  expect_silent(draw(a, back_transform = TRUE))
})
