scasm_constraint_fixture <- function() {
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  withr::with_seed(720, {
    d <- data.frame(x = runif(120), z = runif(120))
    d$y <- 2 * d$x + d$z + rnorm(120, sd = 0.2)
    d
  })
}

test_that("shape metadata records codes without changing smooth types", {
  d <- scasm_constraint_fixture()
  for (code in c("m+", "m-", "c+", "c-", "+")) {
    sm <- mgcv::smoothCon(mgcv::s(x, bs = "sc", xt = code, k = 6), d)[[1L]]
    sm$first.para <- 1L
    sm$last.para <- ncol(sm$Ain)
    model <- list(smooth = list(sm), Ain = sm$Ain,
      coefficients = numeric(ncol(sm$Ain)))
    info <- smooth_shape_constraints(model)
    expect_identical(info[[1L]]$margins[[1L]]$codes, code)
    expect_true(info[[1L]]$margins[[1L]]$retained)
    expect_identical(smooth_type(sm), "B spline")
  }
  m <- mgcv::scasm(y ~ s(x, bs = "sc", xt = c("m+", "c+"), k = 6), data = d)
  info <- smooth_shape_constraints(m)
  expect_identical(info[[1L]]$margins[[1L]]$codes, c("m+", "c+"))
  expect_identical(scasm_shape_caption(info[[1L]], plotmath = FALSE),
    "B spline (increasing, convex)")
  expect_true(is.expression(scasm_shape_caption(info[[1L]])))
})

test_that("retention is checked for each tensor rather than the whole model", {
  d <- scasm_constraint_fixture()
  m <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 6) +
    ti(x, z, bs = c("sc", "sc"), xt = list("m+", "m+"), k = c(5, 5)),
    data = d)
  expect_warning(info <- smooth_shape_constraints(m),
    "not retained.*ti\\(x,z\\)")
  expect_true(info[[1L]]$margins[[1L]]$retained)
  expect_false(info[[2L]]$margins[[1L]]$retained)
  expect_false(info[[2L]]$margins[[2L]]$retained)
  expect_match(scasm_shape_caption(info[[2L]], FALSE), "requested, not retained")
  expect_silent(smooth_shape_constraints(m, warn = FALSE))

  m <- mgcv::scasm(y ~ t2(x, z, bs = c("sc", "sc"),
    xt = list("m+", "m+"), k = c(5, 5)), data = d)
  expect_warning(info <- smooth_shape_constraints(m), "not retained")
  expect_false(info[[1L]]$margins[[1L]]$retained)

  m <- mgcv::scasm(y ~ te(x, z, bs = c("sc", "cr"),
    xt = list("m+", NULL), k = c(5, 5)), data = d)
  expect_silent(info <- smooth_shape_constraints(m))
  expect_true(info[[1L]]$margins[[1L]]$retained)
  expect_false(info[[1L]]$margins[[2L]]$requested)
  expect_identical(scasm_shape_caption(info[[1L]], FALSE),
    "Tensor product; x: B spline (increasing); z: CRS")
})

test_that("ordinary gam requests are not labelled as retained constraints", {
  d <- scasm_constraint_fixture()
  m <- mgcv::gam(y ~ s(x, bs = "sc", xt = "m+", k = 6), data = d)
  expect_warning(info <- smooth_shape_constraints(m), "not retained")
  expect_false(info[[1L]]$margins[[1L]]$retained)
  m <- mgcv::gam(y ~ s(x, bs = "cr", k = 6), data = d)
  expect_identical(smooth_shape_constraints(m), list())
})

test_that("captions preserve literal text and use plotmath arrows", {
  d <- scasm_constraint_fixture()
  m <- mgcv::scasm(y ~ s(x, bs = "sc", xt = c("m+", "c+"), k = 6), data = d)
  info <- smooth_shape_constraints(m)
  obj <- data.frame(.smooth = m$smooth[[1L]]$label, .smooth_id = 1L,
    .type = "B spline", .by = NA_character_)
  attr(obj, "shape_constraints") <- info
  caption <- scasm_shape_plot_caption(obj, "Facets: z; Basis: B spline")
  expect_true(is.expression(caption))
  expect_match(paste(deparse(caption), collapse = ""), "%up%", fixed = TRUE)
  expect_match(paste(deparse(caption), collapse = ""), "Facets: z", fixed = TRUE)
  expect_identical(scasm_shape_subset(info, transform(obj, .smooth_id = 2L)), list())
  attr(obj, "interval_info") <- list(interval = "simultaneous", level = 0.95)
  labels <- prepare_smooth_labels(obj, x_var = "x")
  expect_true(is.expression(labels$caption))
  expect_match(paste(deparse(labels$caption), collapse = ""), "95% simultaneous")
  expect_null(prepare_smooth_labels(obj, x_var = "x", caption = FALSE)$caption)

  # Each supported graphics device must accept the expression without a font
  # package or a Unicode arrow in the source text.
  paths <- file.path(tempdir(), paste0("gratia-shape-caption.", c("pdf", "svg", "png")))
  on.exit(unlink(paths), add = TRUE)
  for (i in seq_along(paths)) {
    device <- list(grDevices::pdf, grDevices::svg, grDevices::png)[[i]]
    device(paths[i])
    tryCatch({
      graphics::plot.new()
      expect_warning(graphics::text(0.5, 0.5, caption), NA)
    }, finally = grDevices::dev.off())
    expect_gt(file.info(paths[i])$size, 0)
  }
})

test_that("general tensor constraints are described separately from margins", {
  d <- scasm_constraint_fixture()
  m <- mgcv::scasm(y ~ te(x, z, bs = c("sc", "cr"),
    xt = list("m+", NULL), k = c(5, 5)), data = d)
  # General constraints can be imposed on the tensor coefficients without
  # representing a named marginal shape.
  m$smooth[[1L]]$margin[[1L]]$xt <- NULL
  m$smooth[[1L]]$margin[[1L]]$Ain <- NULL
  info <- smooth_shape_constraints(m)
  expect_true(info[[1L]]$linear_constraints)
  expect_true(info[[1L]]$retained)
  expect_match(scasm_shape_caption(info[[1L]], FALSE),
    "Tensor product (linear constraints)", fixed = TRUE)
  expect_false(info[[1L]]$margins[[1L]]$requested)

  obj <- data.frame(.smooth = m$smooth[[1L]]$label,
    .smooth_id = 1L, .type = smooth_type(m$smooth[[1L]]), .by = NA_character_)
  attr(obj, "shape_constraints") <- info
  caption <- scasm_shape_plot_caption(obj,
    paste0("Basis: ", obj$.type, "; facet information"), plotmath = FALSE)
  expect_type(caption, "character")
  expect_match(caption, "; facet information", fixed = TRUE)
  attr(obj, "interval_info") <- list(interval = "confidence", level = 0.95,
    method = "bootstrap")
  labs <- prepare_smooth_labels(obj, x_var = "x")
  expect_match(paste(deparse(labs$caption), collapse = ""),
    "95% bootstrap pointwise interval", fixed = TRUE)
})

test_that("constraint captions cover combinations and decreasing arrows", {
  info <- list(tensor = FALSE, margins = list(list(variables = "x",
    codes = c("m-", "c-", "+"), requested = TRUE, retained = TRUE,
    basis = "B spline")))
  expect_identical(scasm_shape_caption(info, FALSE),
    "B spline (decreasing, concave, positive)")
  expect_match(paste(deparse(scasm_shape_caption(info)), collapse = ""),
    "%down%", fixed = TRUE)
  info$margins[[1L]]$codes <- character()
  expect_identical(scasm_shape_caption(info, FALSE), "B spline (linear constraints)")
})

test_that("ordinary multidimensional tensor margins need no shape metadata", {
  d <- withr::with_seed(19, data.frame(x = runif(100), z = runif(100),
    w = runif(100), y = rnorm(100)))
  m <- mgcv::gam(y ~ te(x, z, w, bs = c("cr", "ds"), d = c(1, 2),
    k = c(4, 8)), data = d)
  original <- m$smooth
  expect_identical(smooth_shape_constraints(m), list())
  expect_silent(est <- smooth_estimates(m, n = 8, n_3d = 3))
  expect_true(all(is.finite(est$.estimate)))
  expect_identical(m$smooth, original)
})

test_that("shape captions describe multidimensional tensor margins locally", {
  d <- scasm_constraint_fixture()
  d$w <- withr::with_seed(811, runif(nrow(d)))
  m <- mgcv::scasm(y ~ te(x, z, w, bs = c("sc", "ds"), d = c(1, 2),
    xt = list("m+", NULL), k = c(5, 8)), data = d)
  original <- m$smooth
  expect_silent(info <- smooth_shape_constraints(m))
  expect_match(scasm_shape_caption(info[[1]], FALSE), "Duchon spline (2d)",
    fixed = TRUE)
  expect_identical(m$smooth, original)
})
