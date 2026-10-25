# The full suite checks combinations across models, families and plot options.
# Small function checks remain in test-function-core.R and focused test files.
skip_on_cran()

skip_if_not_installed("GJRM", minimum_version = "0.2-6")

gjrm_plot_review_fixture <- local({
  model <- NULL
  function() {
    if (is.null(model)) {
      s <- mgcv::s
      d <- withr::with_seed(2111, {
        n <- 180L
        d <- data.frame(x = runif(n), z = runif(n),
          f = factor(rep(c("a", "b"), n / 2)))
        d$y1 <- sin(6 * d$x) + as.numeric(d$f) + rnorm(n)
        d$y2 <- cos(6 * d$x) * as.numeric(d$f) + sin(2 * pi * d$z) + rnorm(n)
        d
      })
      formulas <- list(y1 ~ f + s(x, by = f, k = 4),
        y2 ~ f + s(x, by = f, k = 4) + s(z, bs = "cc", k = 4))
      model <<- withr::with_seed(2112, do.call(GJRM::gjrm,
        list(formula = formulas, data = d, margins = c("N", "N"), model = "B",
          knots = list(z = c(0, 1)))))
    }
    model
  }
})

test_that("saved factor-by smooth groups keep ordinary GAM terms separate", {
  withr::local_pdf(NULL)
  d <- withr::with_seed(2113, {
    d <- data.frame(x = runif(160), z = runif(160),
      f = factor(rep(c("a", "b"), 80)))
    d$y <- sin(5 * d$x) * as.numeric(d$f) + cos(4 * d$z) + rnorm(160)
    d
  })
  m <- gam(y ~ f + s(x, by = f, k = 4) + s(z, by = f, k = 4), data = d)
  direct <- draw(m, grouped = c("factor-by", "sz", "fs"), parametric = FALSE, n = 8, rug = FALSE)
  saved <- draw(smooth_estimates(m, n = 8), grouped = c("factor-by", "sz", "fs"))
  expect_length(direct, 2L)
  expect_length(saved, 2L)
  for (i in 1:2) {
    expect_setequal(saved[[i]]$data$.smooth, direct[[i]]$data$.smooth)
    expect_equal(saved[[i]]$data$.estimate, direct[[i]]$data$.estimate)
  }
  expect_silent(patchwork::patchworkGrob(saved))
  # A joint response equation can contain more than one factor-by term too.
  d$y2 <- withr::with_seed(2116, .3 * d$y + rnorm(nrow(d)))
  joint <- withr::with_seed(2117, do.call(GJRM::gjrm,
    list(formula = list(y ~ f + s(x, by = f, k = 4) +
      s(z, by = f, k = 4), y2 ~ 1), data = d,
      margins = c("N", "N"), model = "B")))
  direct <- draw(joint, grouped = c("factor-by", "sz", "fs"), parametric = FALSE, n = 8, rug = FALSE)
  saved <- draw(smooth_estimates(joint, n = 8), grouped = c("factor-by", "sz", "fs"))
  expect_length(direct, 2L)
  expect_length(saved, 2L)
  for (i in 1:2) {
    expect_setequal(saved[[i]]$data$.smooth, direct[[i]]$data$.smooth)
    expect_equal(saved[[i]]$data$.estimate, direct[[i]]$data$.estimate)
  }
  expect_silent(patchwork::patchworkGrob(saved))
})

test_that("saved joint factor-by groups preserve response identity and order", {
  withr::local_pdf(NULL)
  m <- gjrm_plot_review_fixture()
  expected <- c("Response 1: Mean (mu); By: f",
    "Response 2: Mean (mu); By: f", "Response 2: Mean (mu)")
  direct <- draw(m, grouped = c("factor-by", "sz", "fs"), parametric = FALSE, n = 8, rug = FALSE)
  expect_length(direct, 3L)
  for (unnest in c(TRUE, FALSE)) {
    saved <- draw(smooth_estimates(m, n = 8, unnest = unnest), grouped = c("factor-by", "sz", "fs"))
    expect_length(saved, 3L)
    expect_identical(vapply(seq_len(length(saved)), function(i)
      saved[[i]]$labels$subtitle, character(1)), expected)
    for (i in 1:3) {
      expect_length(unique(saved[[i]]$data$.model), 1L)
      expect_equal(saved[[i]]$data$.estimate, direct[[i]]$data$.estimate)
    }
    expect_silent(patchwork::patchworkGrob(saved))
  }
})

test_that("joint factor-by plot list names use discoverable model-wide labels", {
  m <- gjrm_plot_review_fixture()
  plots <- assemble(m, parametric = FALSE, n = 8, rug = FALSE)
  expect_identical(names(plots), smooths(m))
  grouped <- assemble(m, grouped = c("factor-by", "sz", "fs"), parametric = FALSE, n = 8, rug = FALSE)
  expect_identical(names(grouped), c("s(x)", "s.1(x)", "s(z)"))
  selected <- assemble(m, select = "s.1(x):fb", n = 8, rug = FALSE)
  expect_identical(names(selected), "s.1(x):fb")
})

test_that("joint cyclic smooths and saved metadata use the existing polar renderer", {
  withr::local_pdf(NULL)
  m <- gjrm_plot_review_fixture()
  direct <- draw(m, select = "s(z)", cyclic = "polar", n = 12,
    rug = FALSE, wrap = FALSE)[[1]]
  expect_s3_class(direct$coordinates, "CoordRadial")
  expect_identical(direct$labels$subtitle, "Response 2: Mean (mu)")
  for (unnest in c(TRUE, FALSE)) {
    values <- smooth_estimates(m, select = "s(z)", n = 12, unnest = unnest)
    expect_true(length(attr(values, "cyclic_info")) > 0L)
    saved <- draw(values, cyclic = "polar")[[1]]
    expect_s3_class(saved$coordinates, "CoordRadial")
    expect_identical(saved$labels$subtitle, direct$labels$subtitle)
    expect_silent(ggplot2::ggplotGrob(saved))
  }
})

test_that("joint tube interval metadata retain every selected equation's calibration", {
  withr::local_pdf(NULL)
  m <- gjrm_joint_fixture()
  for (unnest in c(TRUE, FALSE)) {
    values <- smooth_estimates(m, select = c(1, 3), n = 8, unnest = unnest,
      interval = "simultaneous", simultaneous_method = "tube",
      overall_uncertainty = FALSE)
    info <- attr(values, "interval_info")
    expect_identical(info$calibration, "tube")
    expect_length(info$curves, 2L)
    expect_identical(vapply(info$curves, `[[`, character(1), "identity"),
      smooths(m)[c(1, 3)])
    expect_true(all(vapply(info$curves, function(x)
      is.finite(x$critical) && x$critical > 0, logical(1))))
    expect_silent(patchwork::patchworkGrob(draw(values)))
  }
})

test_that("repeated joint parametric effects identify their fitted equations", {
  m <- gjrm_plot_review_fixture()
  plots <- assemble(m, select = rep(FALSE, n_smooths(m)),
    parametric = TRUE, terms = "f", n = 8)
  expect_identical(names(plots), c("f (equation 1)", "f (equation 2)"))
  expect_identical(vapply(plots, function(p) p$labels$subtitle, character(1)),
    c(`f (equation 1)` = "Response 1: Mean (mu)",
      `f (equation 2)` = "Response 2: Mean (mu)"))
  for (i in 1:2) {
    local <- match("fb", names(m[[paste0("gam", i)]]$coefficients))
    j <- local + if (i == 2L) m$X1.d2 else 0L
    expect_equal(plots[[i]]$data$.partial[plots[[i]]$data$.level == "b"],
      unname(m$coefficients[j]))
  }
  for (subtitle in list("My subtitle", "", expression(mu))) {
    custom <- assemble(m, parametric = TRUE, terms = "f", subtitle = subtitle, n = 8)
    expect_true(all(vapply(custom, function(p)
      identical(p$labels$subtitle, subtitle), logical(1))))
  }
  default <- assemble(m, select = rep(FALSE, n_smooths(m)),
    parametric = TRUE, terms = "f", subtitle = NULL, n = 8)
  expect_identical(vapply(default, function(p) p$labels$subtitle, character(1)),
    vapply(plots, function(p) p$labels$subtitle, character(1)))
})

gjrm_surface_review_fixture <- local({
  model <- NULL
  function() {
    if (is.null(model)) {
      s <- mgcv::s
      d <- withr::with_seed(2114, {
        n <- 150L
        d <- data.frame(x = runif(n), z = runif(n))
        d$y1 <- 10 * sin(5 * d$x) * cos(3 * d$z) + rnorm(n)
        d$y2 <- sin(5 * d$x) + rnorm(n)
        d
      })
      model <<- withr::with_seed(2115, do.call(GJRM::gjrm,
        list(formula = list(y1 ~ s(x, z, k = 8), y2 ~ s(x, k = 4)),
          data = d, margins = c("N", "N"), model = "B")))
    }
    model
  }
})

test_that("joint fixed scales share effect ranges without changing surface coordinates", {
  withr::local_pdf(NULL)
  m <- gjrm_surface_review_fixture()
  free <- assemble(m, n_2d = 6, n = 8, rug = FALSE, contour = FALSE,
    dist = 0, scales = "free")
  fixed <- assemble(m, n_2d = 6, n = 8, rug = FALSE, contour = FALSE,
    dist = 0, scales = "fixed")
  free_surface <- ggplot2::ggplot_build(free[[1]])
  surface <- ggplot2::ggplot_build(fixed[[1]])
  curve <- ggplot2::ggplot_build(fixed[[2]])
  expect_equal(surface$layout$panel_params[[1]]$y.range,
    free_surface$layout$panel_params[[1]]$y.range)
  fill_limits <- surface$plot$scales$get_scales("fill")$get_limits()
  curve_limits <- curve$layout$panel_scales_y[[1]]$get_limits()
  expect_equal(fill_limits, curve_limits)
  expect_true(all(fixed[[1]]$data$.estimate >= fill_limits[1]))
  expect_true(all(fixed[[1]]$data$.estimate <= fill_limits[2]))
  expect_silent(patchwork::patchworkGrob(patchwork::wrap_plots(fixed)))
})

test_that("joint smooth plotting warns once when partial residuals are unavailable", {
  m <- gjrm_plot_review_fixture()
  messages <- character()
  actual <- withCallingHandlers(assemble(m, n = 8, rug = FALSE,
    parametric = FALSE, residuals = TRUE), warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(messages, 1L)
  expect_match(messages, "working residuals and working weights")
  expected <- assemble(m, n = 8, rug = FALSE, parametric = FALSE,
    residuals = FALSE)
  expect_identical(names(actual), names(expected))
  expect_equal(lapply(actual, `[[`, "data"), lapply(expected, `[[`, "data"))
})

test_that("joint fixed surface scales preserve custom fill palettes and breaks", {
  m <- gjrm_surface_review_fixture()
  scale <- ggplot2::scale_fill_viridis_c(option = "C", breaks = c(-10, 0, 10))
  plots <- assemble(m, n_2d = 6, n = 8, rug = FALSE, contour = FALSE,
    dist = 0, scales = "fixed", continuous_fill = scale)
  actual <- plots[[1]]$scales$get_scales("fill")
  expect_identical(actual$breaks, scale$breaks)
  expect_equal(actual$palette(c(0, .5, 1)), scale$palette(c(0, .5, 1)))
  surface <- ggplot2::ggplot_build(plots[[1]])
  curve <- ggplot2::ggplot_build(plots[[2]])
  expect_equal(surface$plot$scales$get_scales("fill")$get_limits(),
    curve$layout$panel_scales_y[[1]]$get_limits())
})

test_that("joint fixed 3D surfaces share vertical and fill effect ranges", {
  skip_if_not_installed("ggcube")
  withr::local_pdf(NULL)
  m <- gjrm_surface_review_fixture()
  plots <- assemble(m, n_2d = 5, n = 8, rug = FALSE, contour = FALSE,
    dist = 0, scales = "fixed", surface = "3d", ci_3d = TRUE)
  expect_s3_class(plots[[1]]$coordinates, "Coord3D")
  surface <- ggplot2::ggplot_build(plots[[1]])
  curve <- ggplot2::ggplot_build(plots[[2]])
  fill_limits <- surface$plot$scales$get_scales("fill")$get_limits()
  expect_equal(surface$plot$scales$get_scales("z")$get_limits(), fill_limits)
  expect_equal(curve$layout$panel_scales_y[[1]]$get_limits(), fill_limits)
  expect_silent(patchwork::patchworkGrob(patchwork::wrap_plots(plots)))
})

test_that("joint selection models recover original covariates for transformed smooths", {
  withr::local_pdf(NULL)
  s <- mgcv::s
  d <- withr::with_seed(2118, {
    n <- 220L
    d <- data.frame(x = runif(n, .2, 2))
    d$y1 <- as.numeric(2 * log(d$x) + rnorm(n) > 0)
    d$y2 <- sin(4 * log(d$x)) + rnorm(n)
    d$y2[d$y1 == 0] <- NA_real_
    d
  })
  keep <- seq_len(nrow(d)) %% 5 != 0
  m <- withr::with_seed(2119, do.call(GJRM::gjrm,
    list(formula = list(y1 ~ s(log(x), k = 4), y2 ~ s(log(x), k = 4)),
      data = d, subset = keep, margins = c("probit", "N"), model = "BSS")))
  expect_equal(nrow(m$gam1$model), sum(keep))
  expect_lt(nrow(m$gam2$model), nrow(m$gam1$model))
  components <- gjrm_gamlss_components(m)
  for (i in 1:2) {
    rows <- match(rownames(components[[i]]$model), rownames(d))
    expect_false(anyNA(rows))
    recovered <- recover_raw_data(components[[i]], vars = "x")
    expect_equal(recovered$x, d$x[rows])
    automatic <- smooth_estimates(m, select = i, n = 8, back_transform = TRUE)
    raw_column <- attr(automatic, "coordinate_info")[["log(x)"]]$column
    expect_equal(range(automatic[[raw_column]]), range(d$x[rows]))
    data <- data.frame(x = automatic[[raw_column]])
    explicit <- smooth_estimates(m, select = i, data = data, back_transform = TRUE)
    expect_equal(automatic$.estimate, explicit$.estimate)
    expect_equal(automatic$.se, explicit$.se)
    automatic_plot <- draw(m, select = i, n = 8, back_transform = TRUE,
      rug = FALSE, wrap = FALSE)[[1]]
    explicit_plot <- draw(m, select = i, data = data, back_transform = TRUE,
      rug = FALSE, wrap = FALSE)[[1]]
    expect_equal(automatic_plot$data, explicit_plot$data)
    expect_identical(automatic_plot$labels$x, "x")
    expect_silent(ggplot2::ggplotGrob(automatic_plot))
  }
})

test_that("joint grouped factor-by names reserve plain smooth selection labels", {
  withr::local_pdf(NULL)
  s <- mgcv::s
  d <- gjrm_plot_review_fixture()$mcd
  m <- withr::with_seed(2121, do.call(GJRM::gjrm,
    list(formula = list(y1 ~ f + s(x, by = f, k = 4), y2 ~ s(x, k = 4)),
      data = d, margins = c("N", "N"), model = "B")))
  expect_identical(smooths(m), c("s(x):fa", "s(x):fb", "s(x)"))
  plots <- assemble(m, grouped = c("factor-by", "sz", "fs"), parametric = FALSE, n = 8, rug = FALSE)
  expect_identical(names(plots), c("s(x) (equation 1)", "s(x)"))
  first <- assemble(m, select = 1:2, grouped = c("factor-by", "sz", "fs"), n = 8, rug = FALSE)
  expect_identical(names(first), "s(x) (equation 1)")
  second <- assemble(m, select = "s(x)", grouped = c("factor-by", "sz", "fs"), n = 8, rug = FALSE)
  expect_identical(names(second), "s(x)")
  expect_equal(second[[1]]$data, plots[[2]]$data)
  saved <- draw(smooth_estimates(m, n = 8), grouped = c("factor-by", "sz", "fs"))
  expect_length(saved, 2L)
  expect_identical(vapply(seq_len(length(saved)), function(i)
    saved[[i]]$labels$subtitle, character(1)),
    c("Response 1: Mean (mu); By: f", "Response 2: Mean (mu)"))
  expect_silent(patchwork::patchworkGrob(saved))
})

test_that("joint plots skip empty equations without reporting that all terms are absent", {
  for (fun in list(assemble, draw)) {
    expect_silent(smooths <- fun(gjrm_joint_fixture("count_cont"), n = 8))
    expect_length(smooths, 4L)
    expect_silent(parametric <- fun(gjrm_joint_fixture("parametric"), n = 8))
    expect_length(parametric, 1L)
    expect_silent(empty <- withVisible(fun(gjrm_joint_fixture("empty"))))
    expect_null(empty$value)
    expect_false(empty$visible)
  }
})

test_that("separate joint factor smooth curves retain their level names", {
  withr::local_pdf(NULL)
  s <- mgcv::s
  d <- withr::with_seed(2122, {
    d <- data.frame(x = runif(150), f = factor(rep(letters[1:3], 50)))
    d$y1 <- sin(5 * d$x) + as.numeric(d$f) + rnorm(150)
    d$y2 <- cos(5 * d$x) + as.numeric(d$f) + rnorm(150)
    d
  })
  model <- withr::with_seed(2123, do.call(GJRM::gjrm,
    list(formula = list(y1 ~ s(x, f, bs = "fs", k = 4),
      y2 ~ s(x, f, bs = "fs", k = 4)), data = d,
      margins = c("N", "N"), model = "B")))
  separate <- assemble(model, grouped = FALSE, show_ci = "fs",
    parametric = FALSE, n = 8, rug = FALSE)
  expect_length(separate, 6L)
  expect_equal(anyDuplicated(names(separate)), 0L)
  expect_true(all(grepl('f = "[abc]"$', names(separate))))
  expect_true(all(startsWith(names(separate)[1:3], smooths(model)[1L])))
  expect_true(all(startsWith(names(separate)[4:6], smooths(model)[2L])))
  saved <- draw(smooth_estimates(model, n = 8), grouped = FALSE, show_ci = "fs")
  expect_length(saved, 6L)
  for (i in seq_len(6L)) {
    expect_equal(separate[[i]]$data$.estimate, saved[[i]]$data$.estimate)
    expect_true(any(vapply(separate[[i]]$layers, function(layer)
      inherits(layer$geom, "GeomRibbon"), logical(1))))
  }
  expect_silent(patchwork::patchworkGrob(saved))
  grouped <- assemble(model, grouped = "fs", show_ci = FALSE,
    parametric = FALSE, n = 8, rug = FALSE)
  expect_length(grouped, 2L)
  expect_identical(names(grouped), smooths(model))
  expect_false(any(vapply(grouped[[1L]]$layers, function(layer)
    inherits(layer$geom, "GeomRibbon"), logical(1))))
})

test_that("grouped joint factor-by names preserve colons in covariates", {
  s <- mgcv::s
  d <- withr::with_seed(2124, {
    d <- data.frame(x = runif(120), f = factor(rep(c("a", "b"), 60)))
    d$y1 <- sin(5 * d$x) + as.numeric(d$f) + rnorm(120)
    d$y2 <- cos(5 * d$x) + rnorm(120)
    d
  })
  model <- withr::with_seed(2125, do.call(GJRM::gjrm,
    list(formula = list(y1 ~ f + s(I(x + nchar(":f")), by = f, k = 4),
      y2 ~ s(x, k = 4)), data = d, margins = c("N", "N"), model = "B")))
  plots <- assemble(model, grouped = "factor-by", n = 8, rug = FALSE,
    parametric = FALSE)
  expect_identical(names(plots), c('s(I(x + nchar(":f")))', "s(x)"))
  expect_identical(plots[[1L]]$labels$title, 's(I(x + nchar(":f")))')
})
