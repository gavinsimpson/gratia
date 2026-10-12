coordinate_fixture <- function() {
  withr::with_seed(412, {
    d <- expand.grid(x = seq(1, 5, length.out = 12),
      z = seq(1, 3, length.out = 10), group = factor(c("A", "B")))
    d$w <- seq(1, 4, length.out = nrow(d))
    d$y <- 2 - 2 * log(d$x) + sqrt(d$z) + rnorm(nrow(d), sd = 0.1)
    d
  })
}

coordinate_gam <- function(formula, data, ...) {
  model <- eval(substitute(gam(formula, data = data, ...)), parent.frame())
  with_model_envir(model, parent.frame())
}

test_that("issue 412 uses predictor coordinates rather than partial effects", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ log(x), data = d)
  pe <- parametric_effects(m)
  x <- unique(d$x)
  expect_equal(pe$.value, log(x))
  expect_equal(pe$.partial, unname(coef(m)["log(x)"]) * log(x))
  expect_lt(coef(m)["log(x)"], 0)
  p <- draw(m, wrap = FALSE)[[1L]]
  expect_identical(p$labels$x, "log(x)")
  expect_identical(p$labels$title, "log(x)")
  expect_equal(p$data$.value, log(x))
  expect_equal(coef(lm(.partial ~ .value, data = p$data))[[2L]],
    unname(coef(m)["log(x)"]))

  # A zero coefficient must not collapse the coordinate to one observation.
  m$coefficients["log(x)"] <- 0
  zero <- parametric_effects(m)
  expect_equal(zero$.value, log(x))
  expect_equal(zero$.partial, rep(0, length(x)))
})

test_that("scalar axes share model and direct drawing, nesting and selection", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ log(x) + sqrt(z), data = d)
  for (nested in c(FALSE, TRUE)) {
    for (prepared_raw in c(FALSE, TRUE)) {
      pe <- parametric_effects(m, unnest = !nested, back_transform = prepared_raw)
      selected <- pe[pe$.term == "log(x)", ]
      expect_identical(names(attr(selected, "axis_info")), "log(x)")
      for (bt in c(FALSE, TRUE)) {
        direct <- draw(selected, back_transform = bt)[[1L]]
        model <- draw(m, terms = "log(x)", back_transform = bt,
          wrap = FALSE)[[1L]]
        expect_equal(direct$data$.value, model$data$.value)
        expect_equal(direct$data$.partial, model$data$.partial)
        expect_equal(direct$data$.lower_ci, model$data$.lower_ci)
        expect_equal(direct$data$.upper_ci, model$data$.upper_ci)
        expect_identical(direct$labels$x, if (bt) "x" else "log(x)")
        built <- ggplot_build(direct)
        expect_equal(sort(built$data[[3L]]$x),
          sort(if (bt) unique(d$x) else log(unique(d$x))))
      }
    }
  }
})

test_that("coordinate transforms preserve fitted parameters and effect transforms", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ scale(x), data = d)
  nd <- d[1:4, ]
  pe <- parametric_effects(m, data = nd)
  expected <- as.numeric(model.frame(delete.response(m$pterms), nd)[[1L]])
  expect_equal(pe$.value, expected)
  expect_false(isTRUE(all.equal(pe$.value, as.numeric(scale(nd$x)))))
  for (bt in c(FALSE, TRUE)) {
    p <- draw(pe, back_transform = bt, constant = 1, fun = exp)[[1L]]
    expect_equal(p$data$.value, if (bt) nd$x else expected)
    expect_equal(p$data$.partial, exp(pe$.partial + 1))
    expect_equal(p$data$.lower_ci, exp(pe$.partial - qnorm(0.975) * pe$.se + 1))
    expect_equal(p$data$.upper_ci, exp(pe$.partial + qnorm(0.975) * pe$.se + 1))
  }
})

test_that("transform is deprecated with an explicit inverse mapping", {
  withr::local_options(lifecycle_verbosity = "warning")
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ log(x), data = d)
  expect_warning(model <- parametric_effects(m, transform = TRUE),
    class = "lifecycle_warning_deprecated")
  expect_equal(model$.value, log(unique(d$x)))
  expect_warning(raw <- parametric_effects(m, transform = FALSE),
    class = "lifecycle_warning_deprecated")
  expect_equal(raw$.value, unique(d$x))
  expect_error(suppressWarnings(parametric_effects(m, transform = TRUE,
    back_transform = TRUE)), "different covariate scales")
  expect_warning(parametric_effects(m, transform = TRUE, back_transform = FALSE),
    class = "lifecycle_warning_deprecated")
  for (value in list(NULL, NA, 1, c(TRUE, FALSE))) {
    expect_error(draw(m, back_transform = value), "must be TRUE or FALSE")
    expect_error(parametric_effects(m, back_transform = value), "must be TRUE or FALSE")
  }
})

test_that("interaction surfaces use model or original coordinates per axis", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ log(x):sqrt(z):log(w), data = d)
  for (bt in c(FALSE, TRUE)) {
    pe <- parametric_effects(m, n_2d = 6, n_3d = 3, dist = 0)
    direct <- draw(pe, back_transform = bt, contour = FALSE, rug = FALSE)[[1L]]
    model <- draw(m, back_transform = bt, n_2d = 6, n_3d = 3,
      contour = FALSE, rug = FALSE, dist = 0, wrap = FALSE)[[1L]]
    expect_equal(direct$data$x, model$data$x)
    expect_equal(direct$data$z, model$data$z)
    expect_equal(direct$data$w, model$data$w)
    expect_equal(direct$data$.partial, model$data$.partial)
    expect_equal(direct$data$.lower_ci, model$data$.lower_ci)
    expect_identical(model$labels$x, if (bt) "x" else "log(x)")
    expect_identical(model$labels$y, if (bt) "z" else "sqrt(z)")
    expect_equal(sort(unique(model$data$x)),
      if (bt) seq(1, 5, length.out = 6) else log(seq(1, 5, length.out = 6)))
    labeller <- model$facet$params$labeller
    labels <- labeller(data.frame(w = unique(model$data$w)))
    expect_match(labels[[1L]][[1L]], if (bt) "w:" else "log\\(w\\):")
    expect_warning(ggplot_build(model), NA)
    expect_s3_class(ggplot_build(model), "ggplot_built")
  }
})

test_that("interaction rugs and grouped curves use the selected scale", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ log(x):group, data = d)
  pe <- parametric_effects(m, n = 9)
  for (bt in c(FALSE, TRUE)) {
    p <- draw(pe, back_transform = bt)[[1L]]
    expect_identical(p$labels$x, if (bt) "x" else "log(x)")
    expect_equal(p$layers[[3L]]$data$x, if (bt) d$x else log(d$x))
    expect_s3_class(ggplot_build(p), "ggplot_built")
  }
  # Surface observation coordinates are independent of the prediction grid.
  m <- coordinate_gam(y ~ log(x):sqrt(z), data = d)
  pe <- parametric_effects(m, n_2d = 7, dist = 0)
  p <- draw(pe, contour = FALSE)[[1L]]
  rug <- Filter(function(layer) inherits(layer$geom, "GeomPoint"), p$layers)[[1L]]
  expect_equal(rug$data$x, log(d$x))
  expect_equal(rug$data$z, sqrt(d$z))
})

test_that("polynomials and mixed expressions have explicit raw-axis semantics", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ poly(x, 2):z + I(x * z) + x:I(x^2), data = d)
  pe <- parametric_effects(m, data = d)
  for (term in unique(pe$.term)) {
    selected <- pe[pe$.term == term, ]
    p <- draw(selected, contour = FALSE)[[1L]]
    expect_identical(p$labels$x, "x")
    column <- if (unique(p$data$.type) == "interaction") "x" else ".value"
    expect_equal(sort(unique(p$data[[column]])), sort(unique(d$x)))
  }
})

test_that("multiple predictors retain transformed-axis metadata", {
  d <- coordinate_fixture()
  m <- coordinate_gam(list(y ~ log(x), ~ log(x):sqrt(z)), data = d, family = gaulss())
  pe <- parametric_effects(m, data = d)
  for (bt in c(FALSE, TRUE)) {
    direct <- draw(pe[pe$.term == "log(x):sqrt(z).1", ], back_transform = bt,
      contour = FALSE, rug = FALSE)[[1L]]
    model <- draw(m, data = d, terms = "log(x):sqrt(z).1", back_transform = bt,
      contour = FALSE, rug = FALSE, wrap = FALSE)[[1L]]
    expect_equal(direct$data$.partial, model$data$.partial)
    expect_equal(direct$data$x, if (bt) unique(d[c("x", "z")])$x else
      log(unique(d[c("x", "z")])$x))
    expect_identical(direct$labels$x, if (bt) "x" else "log(x)")
  }
})

test_that("smooth curves transform coordinates, rugs and residual locations", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ s(log(x), k = 6), data = d)
  sm <- smooth_estimates(m, n = 12, back_transform = TRUE)
  model <- draw(m, n = 12, back_transform = TRUE, residuals = TRUE,
    wrap = FALSE)[[1L]]
  direct <- draw(sm, back_transform = TRUE)[[1L]]
  expect_identical(model$labels$x, "x")
  expect_identical(direct$labels$x, "x")
  expect_equal(model$data$x, exp(sm$`log(x)`))
  expect_equal(direct$data$x, model$data$x)
  expect_equal(direct$data$.estimate, model$data$.estimate)
  rug <- Filter(function(layer) inherits(layer$geom, "GeomRug"), model$layers)[[1L]]
  residual <- Filter(function(layer) inherits(layer$geom, "GeomPoint"), model$layers)[[1L]]
  expect_equal(sort(rug$data$x), sort(unique(d$x)))
  expect_equal(residual$data$x, d$x)
  expect_s3_class(ggplot_build(model), "ggplot_built")
  expect_s3_class(draw(sm[1:6, ], back_transform = TRUE), "patchwork")
  expect_s3_class(draw(smooth_estimates(m, unnest = FALSE, back_transform = TRUE),
    back_transform = TRUE), "patchwork")
})

test_that("tensor products transform all coordinates and render irregular cells", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ te(log(x), sqrt(z), log(w), k = c(4, 4, 4)), data = d)
  sm <- smooth_estimates(m, n_2d = 6, n_3d = 3, dist = 0, back_transform = TRUE)
  for (geom in c("raster", "tile")) {
    direct <- draw(sm, back_transform = TRUE, surface = geom, contour = FALSE)[[1L]]
    model <- draw(m, back_transform = TRUE, surface = geom, contour = FALSE,
      n_2d = 6, n_3d = 3, dist = 0, wrap = FALSE)[[1L]]
    expect_equal(direct$data$x, model$data$x)
    expect_equal(direct$data$z, model$data$z)
    expect_equal(direct$data$w, model$data$w)
    expect_equal(direct$data$.estimate, model$data$.estimate)
    expect_equal(direct$data$x, exp(sm$`log(x)`))
    expect_equal(direct$data$z, sm$`sqrt(z)`^2)
    expect_equal(direct$data$w, exp(sm$`log(w)`))
    expect_identical(model$labels$x, "x")
    expect_identical(model$labels$y, "z")
    expect_s3_class(model$layers[[1L]]$geom, if (geom == "raster") "GeomRaster" else "GeomTile")
    expect_warning(built <- ggplot_build(model), NA)
    cells <- built$data[[1L]]
    expect_true(all(cells$xmin < cells$xmax))
    expect_true(all(cells$ymin < cells$ymax))
    expect_equal(sort(unique(cells$xmax))[-length(unique(cells$xmax))],
      sort(unique(cells$xmin))[-1L])
  }
})

test_that("default smooth grids are preserved and raw grids evaluate forwards", {
  d <- coordinate_fixture()
  foo <- sqrt
  m <- coordinate_gam(y ~ s(foo(x), k = 6), data = d)
  default <- smooth_estimates(m, n = 12)
  expect_equal(default$`foo(x)`, seq(min(foo(d$x)), max(foo(d$x)), length.out = 12))
  expect_error(draw(default, back_transform = TRUE), "smooth_estimates.*back_transform = TRUE", fixed = FALSE)
  raw <- smooth_estimates(m, n = 12, back_transform = TRUE)
  p <- draw(raw, back_transform = TRUE)[[1L]]
  expect_equal(p$data$x, seq(min(d$x), max(d$x), length.out = 12))
  expect_equal(raw$`foo(x)`, foo(p$data$x))
  expect_equal(draw(raw)[[1L]]$data$`foo(x)`, raw$`foo(x)`)
  expected <- predict(m, data.frame(`foo(x)` = foo(p$data$x), check.names = FALSE),
    type = "terms", newdata.guaranteed = TRUE)
  expect_equal(raw$.estimate, unname(expected[, 1L]))
  # Row coordinates survive reordering and subsetting, without dictionary lookup.
  rows <- c(8, 2, 8, 5)
  expect_equal(draw(raw[rows, ], back_transform = TRUE)[[1L]]$data$x, p$data$x[rows])
})

test_that("raw-grid recovery validates fitting rows and unavailable data", {
  d <- coordinate_fixture()
  shift <- 0.5
  m <- coordinate_gam(y ~ s(log(x + shift), k = 6), data = d)
  sm <- smooth_estimates(m, n = 12, back_transform = TRUE)
  shift <- 10
  expect_equal(range(draw(sm, back_transform = TRUE)[[1L]]$data$x), range(d$x))
  expect_s3_class(draw(m, back_transform = FALSE), "patchwork")
  expect_error(draw(m, back_transform = TRUE), "Cannot recover raw")
  shift <- 0.5
  unavailable <- new.env(parent = baseenv())
  unavailable$shift <- shift
  expect_s3_class(draw(m, envir = unavailable), "patchwork")
  expect_error(draw(m, envir = unavailable, back_transform = TRUE), "Cannot recover raw")
  # var.summary can include rows excluded by the fitting subset.
  m <- coordinate_gam(y ~ s(log(x), k = 6), data = d, subset = x < 4)
  p <- draw(m, n = 12, back_transform = TRUE, wrap = FALSE)[[1L]]
  expect_equal(range(p$data$x), range(d$x[d$x < 4]))
})

test_that("retained parametric coordinates support many-to-one functions", {
  d <- coordinate_fixture()
  d$x <- rep(c(-2, -1, 1, 2), length.out = nrow(d))
  m <- coordinate_gam(y ~ I(x^2), data = d)
  for (prepared_raw in c(FALSE, TRUE)) {
    pe <- parametric_effects(m, back_transform = prepared_raw)
    for (bt in c(FALSE, TRUE)) {
      direct <- draw(pe, back_transform = bt)[[1L]]
      model <- draw(m, back_transform = bt, wrap = FALSE)[[1L]]
      expect_equal(direct$data$.value, model$data$.value)
      expect_equal(direct$data$.partial, model$data$.partial)
      expect_equal(direct$data$.value, if (bt) unique(d$x) else unique(d$x)^2)
    }
  }
  m <- coordinate_gam(y ~ I(x^2):z, data = d)
  pe <- parametric_effects(m, data = d, dist = 0)
  p <- draw(pe, back_transform = TRUE, contour = FALSE, rug = FALSE)[[1L]]
  expect_equal(p$data$x, unique(d[c("x", "z")])$x)
})

test_that("original-scale irregular surfaces share rendering across drawing paths", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ log(x):z, data = d)
  nd <- expand.grid(x = c(1, 1.5, 2, 3, 4, 5), z = seq(1, 3, length.out = 6))
  for (geom in c("raster", "tile")) {
    for (prepared_raw in c(FALSE, TRUE)) {
      pe <- parametric_effects(m, data = nd, back_transform = prepared_raw, dist = 0)
      direct <- draw(pe, back_transform = TRUE, surface = geom,
        contour = FALSE, rug = FALSE)[[1L]]
      model <- draw(m, data = nd, back_transform = TRUE, surface = geom,
        dist = 0, contour = FALSE, rug = FALSE, wrap = FALSE)[[1L]]
      expect_s3_class(direct$layers[[1L]]$geom, "GeomRect")
      expect_s3_class(model$layers[[1L]]$geom, "GeomRect")
      expect_warning(direct_built <- ggplot_build(direct), NA)
      expect_warning(model_built <- ggplot_build(model), NA)
      expect_equal(direct_built$data[[1L]][c("xmin", "xmax", "ymin", "ymax", "fill")],
        model_built$data[[1L]][c("xmin", "xmax", "ymin", "ymax", "fill")])
    }
  }
})

test_that("confidence intervals retain coordinate metadata for every smooth", {
  d <- coordinate_fixture()
  shift <- 0.5
  m <- coordinate_gam(y ~ s(log(x), k = 6) + s(scale(z), k = 6) +
    s(log(w + shift), k = 6), data = d)
  sm <- smooth_estimates(m, n = 12, back_transform = TRUE)
  for (type in c("confidence", "simultaneous")) {
    ci <- withr::with_seed(412, confint(m, parm = smooths(m), type = type,
      n = 12, nsim = 100, back_transform = TRUE))
    expect_equal(attr(ci, "coordinate_info"), attr(sm, "coordinate_info"))
    plots <- draw(ci, back_transform = TRUE)
    expected <- draw(sm, back_transform = TRUE)
    for (i in seq_along(smooths(m))) {
      variable <- c("x", "z", "w")[[i]]
      expect_identical(plots[[i]]$labels$x, variable)
      expect_equal(plots[[i]]$data[[variable]], expected[[i]]$data[[variable]])
      selected <- ci[ci$.smooth == smooths(m)[[i]], ]
      expect_identical(draw(selected, back_transform = TRUE)[[1L]]$labels$x,
        variable)
    }
  }
})

test_that("custom and non-injective smooth functions retain raw coordinates", {
  d <- coordinate_fixture()
  foo <- function(x) x^2
  d$x <- seq(-2, 2, length.out = nrow(d))
  m <- coordinate_gam(y ~ s(foo(x), k = 6), data = d)
  sm <- smooth_estimates(m, n = 13, back_transform = TRUE)
  p <- draw(sm, back_transform = TRUE)[[1L]]
  expect_equal(p$data$x, seq(-2, 2, length.out = 13))
  expect_equal(sm$`foo(x)`, p$data$x^2)
  expect_equal(p$data$.estimate, sm$.estimate)
  nd <- data.frame(x = c(-1.1, 1.1, -1.3, 1.3))
  sm <- smooth_estimates(m, data = nd)
  expect_equal(draw(sm, back_transform = TRUE)[[1L]]$data$x, nd$x)
  expect_s3_class(draw(m, data = nd, back_transform = TRUE), "patchwork")
  evaluated <- data.frame(`foo(x)` = foo(nd$x), check.names = FALSE)
  expect_error(smooth_estimates(m, data = evaluated, back_transform = TRUE), "Cannot recover an original")
  m <- coordinate_gam(y ~ s(I(x * z), k = 6), data = d)
  expect_error(smooth_estimates(m, back_transform = TRUE), "one original")
})

test_that("forward smooth evaluation uses fitted scale parameters", {
  d <- coordinate_fixture()
  expressions <- c("scale(x)", "exp(scale(x))", "log10(x)", "log(x + 0.5)")
  for (expression in expressions) {
    m <- coordinate_gam(as.formula(paste0("y ~ s(", expression, ", k = 6)")), data = d)
    nd <- data.frame(x = c(1.1, 1.8, 3.2))
    sm <- smooth_estimates(m, data = nd, back_transform = TRUE)
    reference <- model.frame(delete.response(m$terms), nd)
    expected <- predict(m, newdata = reference, type = "terms", newdata.guaranteed = TRUE)
    expect_equal(sm$.estimate, unname(expected[, 1L]), info = expression)
    expect_equal(draw(sm, back_transform = TRUE)[[1L]]$data$x, nd$x)
    sm <- smooth_estimates(m, n = 12, back_transform = TRUE)
    x <- draw(sm, back_transform = TRUE)[[1L]]$data$x
    reference <- model.frame(delete.response(m$terms), data.frame(x = x))
    expect_equal(sm$.estimate, unname(predict(m, reference, type = "terms",
      newdata.guaranteed = TRUE)[, 1L]))
    if (expression == "scale(x)") {
      fitted <- m$model$`scale(x)`
      expect_equal(as.numeric(sm$`scale(x)`),
        (x - attr(fitted, "scaled:center")) / attr(fitted, "scaled:scale"))
    }
  }
})

test_that("factor by smooths share original coordinates in grouped displays", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ s(log(x), by = group, k = 6), data = d)
  sm <- smooth_estimates(m, n = 12, back_transform = TRUE)
  for (grouped in c(FALSE, TRUE)) {
    direct <- draw(sm, back_transform = TRUE, grouped_by = grouped)
    model <- draw(m, n = 12, back_transform = TRUE, grouped_by = grouped,
      residuals = TRUE, wrap = FALSE)
    for (i in seq_along(model)) {
      expect_identical(model[[i]]$labels$x, "x")
      expect_equal(direct[[i]]$data$x, model[[i]]$data$x)
      expect_s3_class(ggplot_build(model[[i]]), "ggplot_built")
    }
  }
})


test_that("raw grids work across smooth classes and fitters", {
  d <- coordinate_fixture()
  foo <- sqrt
  formulas <- list(y ~ te(foo(x), sqrt(z), k = c(4, 4)),
    y ~ ti(foo(x), sqrt(z), k = c(4, 4)),
    y ~ t2(foo(x), sqrt(z), k = c(4, 4)),
    y ~ s(foo(x), by = log(w), k = 5),
    y ~ s(foo(x), group, bs = "fs", k = 5),
    y ~ s(foo(x), group, bs = "sz", k = 5))
  for (fit in c("gam", "bam", "discrete")) for (f in formulas) {
    if (fit == "discrete" && grepl("t2(", deparse(f), fixed = TRUE)) next
    m <- if (fit == "gam") mgcv::gam(f, data = d) else
      mgcv::bam(f, data = d, discrete = fit == "discrete")
    sm <- smooth_estimates(m, n = 8, n_2d = 5, back_transform = TRUE,
      envir = environment())
    p <- draw(sm, back_transform = TRUE, contour = FALSE)[[1L]]
    expect_equal(sm$`foo(x)`, foo(p$data$x))
    if ("log(w)" %in% names(sm)) expect_equal(sm$`log(w)`, rep(1, nrow(sm)))
    expect_warning(ggplot_build(p), NA)
    # Drawing retained raw grids on the model scale also handles irregular cells.
    expect_warning(ggplot_build(draw(sm, contour = FALSE)[[1L]]), NA)
  }
})


test_that("stored raw covariates still validate fitted transformations", {
  d <- coordinate_fixture()
  shift <- 0.5
  m <- coordinate_gam(y ~ x + s(log(x + shift), k = 6), data = d)
  shift <- 10
  expect_error(smooth_estimates(m, back_transform = TRUE), "Cannot reproduce fitted coordinate")
  # Explicit prediction data deliberately evaluate the current expression.
  expect_s3_class(smooth_estimates(m, data = d[1:8, ], back_transform = TRUE), "smooth_estimates")
  foo <- sqrt
  m <- coordinate_gam(y ~ x + s(foo(x), k = 6), data = d)
  foo <- log
  expect_error(smooth_estimates(m, back_transform = TRUE), "Cannot reproduce fitted coordinate")
  rm(d)
  # An untransformed coordinate needs only the raw column already in the fit.
  plain <- coordinate_gam(y ~ s(x, k = 6), data = coordinate_fixture())
  expect_s3_class(draw(plain, back_transform = TRUE), "patchwork")
})


test_that("raw higher-dimensional grids preserve small positive inputs", {
  d <- coordinate_fixture()
  d$w <- d$w / 10000
  m <- coordinate_gam(y ~ te(log(x), sqrt(z), log(w), k = c(4, 4, 4)), data = d)
  sm <- smooth_estimates(m, n_2d = 5, n_3d = 4, back_transform = TRUE)
  p <- draw(sm, back_transform = TRUE, contour = FALSE)[[1L]]
  expect_equal(sort(unique(p$data$w)), seq(min(d$w), max(d$w), length.out = 4))
  expect_true(all(is.finite(sm$`log(w)`)))
  expect_equal(sm$`log(w)`, log(p$data$w))
})

test_that("typical raw values in evaluated grids are not raw axis coordinates", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ s(log(x), k = 6), data = d)
  nd <- smooth_data(m, id = 1, n = 5, include_all = TRUE)
  expect_length(unique(nd$x), 1L)
  sm <- smooth_estimates(m, data = nd)
  expect_false(".gratia_raw_x" %in% names(sm))
  expect_equal(draw(sm)[[1L]]$data$`log(x)`, nd$`log(x)`)
  expect_error(draw(sm, back_transform = TRUE), "Original covariate coordinates")
  expect_error(smooth_estimates(m, data = nd, back_transform = TRUE),
    "Cannot recover an original covariate")
  # Model frames with paired raw and evaluated inputs must agree by row too.
  paired <- model.frame(m)
  paired$x <- rep(median(d$x), nrow(paired))
  expect_false(".gratia_raw_x" %in% names(smooth_estimates(m, data = paired)))
  paired$x <- d$x
  expect_equal(draw(smooth_estimates(m, data = paired), back_transform = TRUE)[[1L]]$data$x,
    d$x)
  raw <- smooth_data(m, id = 1, n = 5, include_all = TRUE, back_transform = TRUE)
  expect_equal(draw(smooth_estimates(m, data = raw), back_transform = TRUE)[[1L]]$data$x,
    seq(min(d$x), max(d$x), length.out = 5))
})

test_that("drawing uses envir for recovered smooth observations", {
  fit <- local({
    dat <- coordinate_fixture()
    foo <- sqrt
    list(model = mgcv::gam(y ~ s(foo(x), k = 6), data = dat),
      parametric = mgcv::gam(y ~ foo(x), data = dat),
      envir = environment(), x = dat$x)
  })
  for (model in list(fit$model, fit$parametric)) {
    p <- draw(model, back_transform = TRUE, envir = fit$envir,
      residuals = TRUE, distinct_rug = FALSE, wrap = FALSE)[[1L]]
    expect_identical(p$labels$x, "x")
    rug <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomRug"), logical(1)))
    expect_length(rug, 1L)
    expected_rug <- if (identical(model, fit$model)) fit$x else unique(fit$x)
    expect_equal(sort(ggplot_build(p)$data[[rug]]$x), sort(expected_rug))
    if (identical(model, fit$model)) {
      points <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
      expect_length(points, 1L)
      expect_equal(ggplot_build(p)$data[[points]]$x, fit$x)
    }
  }
})

test_that("a numeric by multiplier can share an original axis name", {
  d <- coordinate_fixture()
  for (formula in list(y ~ s(log(x), by = x, k = 6),
      y ~ te(log(x), sqrt(z), by = x, k = c(4, 4)))) {
    m <- coordinate_gam(formula, data = d)
    grid <- smooth_data(m, id = 1, n = 6, n_2d = 5, back_transform = TRUE)
    expect_equal(grid$x, rep(1L, nrow(grid)))
    expect_equal(grid$`log(x)`, log(grid$.gratia_raw_x))
    for (nd in list(NULL, d[1:12, ])) {
      sm <- smooth_estimates(m, data = nd, n = 6, n_2d = 5, dist = 0, back_transform = TRUE)
      direct <- draw(sm, back_transform = TRUE, contour = FALSE)[[1L]]
      model <- draw(m, data = nd, n = 6, n_2d = 5, back_transform = TRUE,
        residuals = TRUE, contour = FALSE, dist = 0, wrap = FALSE)[[1L]]
      expect_equal(direct$data$x, sm$.gratia_raw_x)
      expect_equal(model$data$x, direct$data$x)
      expect_equal(model$data$.estimate, sm$.estimate)
      expect_equal(sm$`log(x)`, log(direct$data$x))
      reference <- if (is.null(nd)) grid else nd
      expected <- predict_model(m, reference, type = "terms")
      expect_equal(sm$.estimate, unname(expected[, 1L]))
      expect_warning(ggplot_build(model), NA)
      expect_warning(ggplot_build(draw(sm, contour = FALSE)[[1L]]), NA)
    }
  }
})

test_that("an untransformed numeric by axis requires explicit prediction rows", {
  d <- coordinate_fixture()
  plain <- coordinate_gam(y ~ s(x, by = x, k = 6), data = d)
  expect_error(smooth_estimates(plain, back_transform = TRUE),
    "untransformed automatic grid axis")
  expect_equal(draw(smooth_estimates(plain, data = d[1:8, ]),
    back_transform = TRUE)[[1L]]$data$x, d$x[1:8])
})

test_that("supplied raw grids do not require unavailable observation decorations", {
  fit <- local({
    dat <- coordinate_fixture()
    smooth <- mgcv::gam(y ~ s(log(x), k = 6), data = dat)
    parametric <- mgcv::gam(y ~ log(x), data = dat)
    nd <- dat[1:8, ]
    rm(dat)
    list(smooth = smooth, parametric = parametric, nd = nd)
  })
  for (model in list(fit$smooth, fit$parametric)) {
    for (residuals in c(FALSE, TRUE)) {
      p <- draw(model, data = fit$nd, back_transform = TRUE,
        residuals = residuals, wrap = FALSE)[[1L]]
      expect_false(any(vapply(p$layers, function(layer)
        inherits(layer$geom, "GeomRug") || inherits(layer$geom, "GeomPoint"), logical(1))))
      expect_equal(if (identical(model, fit$smooth)) p$data$x else p$data$.value, fit$nd$x)
      expect_warning(ggplot_build(p), NA)
    }
    expect_error(draw(model, back_transform = TRUE), "Cannot recover raw")
  }
})

test_that("supplied parametric rows retain observed rugs independently", {
  d <- coordinate_fixture()
  m <- coordinate_gam(y ~ log(x), data = d)
  nd <- data.frame(x = c(1.2, 2.5, 4.8))
  pe <- parametric_effects(m, data = nd)
  for (bt in c(FALSE, TRUE)) {
    direct <- draw(pe, back_transform = bt)[[1L]]
    model <- draw(m, data = nd, back_transform = bt, wrap = FALSE)[[1L]]
    expected <- if (bt) unique(d$x) else log(unique(d$x))
    for (p in list(direct, model)) {
      rug <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomRug"), logical(1)))
      expect_equal(sort(ggplot_build(p)$data[[rug]]$x), sort(expected))
    }
    expect_equal(direct$data$.value, if (bt) nd$x else log(nd$x))
    expect_equal(model$data$.value, direct$data$.value)
  }
})

test_that("transformed soap boundaries cannot silently lose their coordinates", {
  withr::local_seed(412)
  bnd <- list(mgcv::fs.boundary())
  names(bnd[[1L]]) <- c("v", "w")
  v <- runif(600) * 5 - 1
  w <- runif(600) * 2 - 1
  inside <- mgcv::inSide(bnd, x = v, y = w)
  dat <- data.frame(y = mgcv::fs.test(v, w, b = 1) + rnorm(600) * 0.3,
    x = exp(v), w = w)[inside, ]
  knots <- data.frame(v = rep(seq(-0.5, 3, by = 0.5), 4),
    w = rep(c(-0.6, -0.3, 0.3, 0.6), each = 8))
  names(knots)[1L] <- names(bnd[[1L]])[1L] <- "log(x)"
  m <- mgcv::gam(y ~ s(log(x), w, k = 30, bs = "so",
    xt = list(bnd = bnd, nmax = 60)), data = dat, knots = knots)
  m <- with_model_envir(m, environment())
  sm <- smooth_estimates(m, n_2d = 10, back_transform = TRUE, clip = TRUE)
  expect_true(any(sm$.bndry))
  expect_true(all(is.na(sm$.gratia_raw_x[sm$.bndry])))
  expect_error(draw(sm, back_transform = TRUE, contour = FALSE),
    "Original boundary coordinates")
  expect_error(draw(m, n_2d = 10, back_transform = TRUE, contour = FALSE),
    "Original boundary coordinates")
  for (bt in c(FALSE, TRUE)) {
    estimates <- smooth_estimates(m, n_2d = 10, back_transform = bt, clip = TRUE)
    p <- draw(estimates, contour = FALSE)[[1L]]
    paths <- which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomPath"), logical(1)))
    expect_length(paths, 1L)
    expect_true(all(is.finite(ggplot_build(p)$data[[paths]]$x)))
  }
  # An untransformed soap boundary remains available on either display scale.
  dat$v <- log(dat$x)
  names(knots)[1L] <- names(bnd[[1L]])[1L] <- "v"
  plain <- mgcv::gam(y ~ s(v, w, k = 30, bs = "so",
    xt = list(bnd = bnd, nmax = 60)), data = dat, knots = knots)
  expect_warning(ggplot_build(draw(plain, n_2d = 10, back_transform = TRUE,
    contour = FALSE, wrap = FALSE)[[1L]]), NA)
})
