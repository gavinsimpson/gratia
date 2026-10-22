# Check fitted cycle limits as well as coordinates: observed ranges need not
# cover a complete cycle.
cyclic_polar_data <- function() {
  d <- data.frame(x = seq(0.1, 0.9, length.out = 80))
  d$z <- rep(seq(0, 1, length.out = 20), 4)
  d$f <- factor(rep(c("a", "b"), each = 40))
  d$y <- sin(2 * pi * d$x) + 0.2 * cos(35 * d$x) + d$z
  d
}

cyclic_polar_model <- function(bs = "cc") {
  mgcv::gam(y ~ s(x, bs = bs, k = 7), data = cyclic_polar_data(),
    knots = list(x = c(0, 1)), method = "REML")
}

expect_polar_build <- function(plot) {
  expect_s3_class(plot$coordinates, "CoordRadial")
  expect_silent(built <- ggplot2::ggplot_build(plot))
  expect_gt(length(built$data), 0L)
  invisible(built)
}

test_that("cyclic boundary metadata survives nested and saved estimates", {
  for (bs in c("cc", "cp")) {
    m <- cyclic_polar_model(bs)
    estimates <- smooth_estimates(m, n = 11)
    nested <- smooth_estimates(m, n = 11, unnest = FALSE)
    info <- attr(estimates, "cyclic_info")
    expect_length(info, 1L)
    expect_equal(info[[1L]]$variable, "x")
    expect_equal(info[[1L]]$boundaries, c(0, 1))
    expect_equal(attr(nested, "cyclic_info"), info)
    expect_polar_build(draw(estimates, cyclic = "polar")[[1L]])
    expect_polar_build(draw(nested, cyclic = "polar")[[1L]])
  }
})

test_that("model polar plots evaluate the entire fitted cycle", {
  m <- cyclic_polar_model()
  cartesian <- assemble(m, n = 11, rug = FALSE)[[1L]]
  polar <- assemble(m, n = 11, cyclic = "polar", rug = FALSE)[[1L]]
  expect_s3_class(cartesian$coordinates, "CoordCartesian")
  expect_equal(range(cartesian$data$x), c(0.1, 0.9))
  expect_equal(polar$data$x, seq(0, 1, length.out = 11))
  expect_equal(polar$data$.estimate[c(1, 11)],
    rep(polar$data$.estimate[1], 2), tolerance = 1e-7)
  built <- expect_polar_build(polar)
  expect_equal(built$layout$panel_params[[1L]]$theta.range, c(0, 1))
  expect_equal(polar$coordinates$arc[1L], 0)
  expect_polar_build(draw(m, n = 11, cyclic = "polar", rug = FALSE)[[1L]])
})

test_that("saved and supplied grids are retained without closing gaps", {
  m <- cyclic_polar_model()
  d <- data.frame(x = seq(0.2, 0.6, length.out = 7))
  saved <- smooth_estimates(m, data = d)
  p <- draw(saved, cyclic = "polar")[[1L]]
  expect_equal(p$data$x, d$x)
  expect_equal(p$data$.estimate, saved$.estimate)
  expect_polar_build(p)
  supplied <- assemble(m, data = d, cyclic = "polar", rug = FALSE)[[1L]]
  expect_equal(supplied$data$x, d$x)
  automatic <- smooth_estimates(m, n = 7)
  expect_equal(draw(automatic, cyclic = "polar")[[1L]]$data$x, automatic$x)
})

test_that("polar controls are validated for fitted and saved smooths", {
  m <- cyclic_polar_model()
  saved <- smooth_estimates(m, n = 7)
  for (object in list(m, saved)) {
    expect_error(draw(object, cyclic = "invalid"), "cyclic|arg")
    expect_error(draw(object, polar_args = list(start = 1)), "polar")
    expect_error(draw(object, cyclic = "polar", polar_args = list(theta = "y")),
      "theta|polar_args")
    expect_error(draw(object, cyclic = "polar", polar_args = list(end = pi)),
      "end|polar_args")
    expect_error(draw(object, cyclic = "polar", polar_args = list(unknown = 1)),
      "unknown|polar_args")
    p <- draw(object, cyclic = "polar", polar_args = list(start = -pi / 2,
      reverse = "theta", inner.radius = 0.1, r.axis.inside = TRUE,
      rotate.angle = TRUE, clip = "off"))[[1L]]
    expect_polar_build(p)
    expect_equal(p$coordinates$arc, ggplot2::coord_radial(
      start = -pi / 2, reverse = "theta")$arc)
    expect_equal(p$coordinates$clip, "off")
  }
  attr(saved, "cyclic_info") <- NULL
  expect_error(draw(saved, cyclic = "polar"), "boundar|metadata|cyclic_info")
  expect_s3_class(draw(saved)[[1L]]$coordinates, "CoordCartesian")
})

test_that("only eligible one dimensional smooths become polar", {
  d <- cyclic_polar_data()
  m <- mgcv::gam(y ~ s(x, bs = "cc", k = 6) + s(z, k = 5), data = d,
    knots = list(x = c(0, 1)), method = "REML")
  p <- assemble(m, n = 11, cyclic = "polar", rug = FALSE)
  expect_polar_build(p[[1L]])
  expect_s3_class(p[[2L]]$coordinates, "CoordCartesian")
  p_cartesian <- assemble(m, n = 11, rug = FALSE)
  expect_equal(p[[2L]]$data, p_cartesian[[2L]]$data)
  surface <- mgcv::gam(y ~ te(x, z, bs = c("cc", "cr"), k = c(4, 4)),
    data = d, knots = list(x = c(0, 1)), method = "REML")
  p_surface <- assemble(surface, n_2d = 5, cyclic = "polar", rug = FALSE)[[1L]]
  expect_false(inherits(p_surface$coordinates, "CoordRadial"))
  expect_silent(ggplot2::ggplot_build(p_surface))
})

test_that("one dimensional tensors and positive cyclic SCAM smooths draw", {
  d <- cyclic_polar_data()
  tensor <- mgcv::gam(y ~ t2(x, bs = "cc", k = 7), data = d,
    knots = list(x = c(0, 1)), method = "REML")
  expect_polar_build(assemble(tensor, cyclic = "polar", n = 11,
    rug = FALSE)[[1L]])
  skip_if_not_installed("scam")
  d$y <- d$y + 3
  positive <- scam::scam(y ~ s(x, bs = "cpop", k = 7), data = d,
    knots = list(x = c(0, 1)))
  estimates <- smooth_estimates(positive, n = 11)
  expect_equal(attr(estimates, "cyclic_info")[[1L]]$boundaries, c(0, 1))
  expect_polar_build(draw(positive, cyclic = "polar", n = 11,
    rug = FALSE)[[1L]])
})

test_that("grouped cyclic curves require identical boundaries", {
  d <- cyclic_polar_data()
  m <- mgcv::gam(y ~ f + s(x, by = f, bs = "cc", k = 6), data = d,
    knots = list(x = c(0, 1)), method = "REML")
  saved <- smooth_estimates(m, n = 11)
  grouped <- draw(saved, cyclic = "polar", grouped = c("factor-by", "sz", "fs"))
  expect_length(grouped, 1L)
  expect_polar_build(grouped[[1L]])
  expect_equal(nrow(grouped[[1L]]$data), 22L)
  expect_polar_build(assemble(m, cyclic = "polar", grouped = c("factor-by", "sz", "fs"),
    n = 11, rug = FALSE)[[1L]])
  info <- attr(saved, "cyclic_info")
  info[[2L]]$boundaries <- c(0, 2)
  attr(saved, "cyclic_info") <- info
  expect_error(draw(saved, cyclic = "polar", grouped = c("factor-by", "sz", "fs")),
    "boundar|cycle")
  separate <- draw(saved, cyclic = "polar")
  expect_length(separate, 2L)
})

test_that("polar plotting retains intervals signed effects and overlays", {
  m <- cyclic_polar_model()
  plain <- assemble(m, cyclic = "polar", n = 11, rug = FALSE)[[1L]]
  expect_lt(min(plain$data$.estimate), 0)
  expect_gt(max(plain$data$.estimate), 0)
  expect_true(all(plain$data$.lower_ci <= plain$data$.estimate))
  expect_true(all(plain$data$.upper_ci >= plain$data$.estimate))
  reference <- smooth_estimates(m, data = data.frame(x = plain$data$x))
  expect_equal(plain$data$.estimate, reference$.estimate)
  transformed <- assemble(m, cyclic = "polar", n = 11, rug = FALSE,
    constant = 2, fun = exp)[[1L]]
  expect_equal(transformed$data$.estimate, exp(reference$.estimate + 2))
  withr::local_options(lifecycle_verbosity = "quiet")
  overlays <- assemble(m, cyclic = "polar", n = 11,
    residuals = TRUE, rug = TRUE)[[1L]]
  expect_gt(length(overlays$layers), length(plain$layers))
  expect_polar_build(overlays)
  expect_silent(ggplot2::ggplotGrob(overlays))
  styled <- overlays + ggplot2::theme(
    geom.gratia.rug = ggplot2::element_geom(colour = "purple"))
  expect_silent(ggplot2::ggplotGrob(styled))
})

test_that("transformed cyclic coordinates reject raw polar axes", {
  d <- cyclic_polar_data()
  d$u <- exp(d$x)
  m <- mgcv::gam(y ~ s(log(u), bs = "cc", k = 7), data = d,
    knots = list("log(u)" = c(0, 1)), method = "REML")
  expect_polar_build(assemble(m, cyclic = "polar", n = 11, rug = FALSE)[[1L]])
  expect_error(draw(m, cyclic = "polar", back_transform = TRUE),
    "transform|back_transform")
  saved <- smooth_estimates(m, n = 11)
  expect_error(draw(saved, cyclic = "polar", back_transform = TRUE),
    "transform|back_transform")
  expect_polar_build(draw(cyclic_polar_model(), cyclic = "polar",
    back_transform = TRUE, n = 11, rug = FALSE)[[1L]])
})

test_that("fixed scales share the radial effect range", {
  d <- cyclic_polar_data()
  m <- mgcv::gam(y ~ s(x, bs = "cc", k = 6) + s(z, bs = "cc", k = 5),
    data = d, knots = list(x = c(0, 1), z = c(0, 1)), method = "REML")
  p <- assemble(m, cyclic = "polar", scales = "fixed", n = 11, rug = FALSE)
  first <- expect_polar_build(p[[1L]])
  second <- expect_polar_build(p[[2L]])
  expect_equal(first$layout$panel_params[[1L]]$r.range,
    second$layout$panel_params[[1L]]$r.range)
})

test_that("gamm and gamm4 wrappers forward cyclic controls", {
  d <- cyclic_polar_data()
  model <- mgcv::gamm(y ~ s(x, bs = "cc", k = 6), data = d,
    knots = list(x = c(0, 1)))
  expect_polar_build(draw(model, cyclic = "polar", n = 11,
    rug = FALSE)[[1L]])
  skip_if_not_installed("gamm4")
  model4 <- gamm4::gamm4(y ~ s(x, bs = "cc", k = 6), data = d,
    knots = list(x = c(0, 1)))
  expect_polar_build(draw(model4, cyclic = "polar", n = 11,
    rug = FALSE)[[1L]])
})

test_that("glmmTMB forwards cyclic metadata and plot controls", {
  skip_if_not_installed("glmmTMB", "1.1.15.2")
  d <- cyclic_polar_data()
  m <- glmmTMB::glmmTMB(y ~ s(x, bs = "cc", k = 6), data = d, REML = TRUE)
  estimates <- smooth_estimates(m, n = 11)
  expect_length(attr(estimates, "cyclic_info"), 1L)
  p <- draw(m, cyclic = "polar", n = 11, rug = FALSE)[[1L]]
  expect_polar_build(p)
  expect_equal(range(p$data$x), attr(estimates, "cyclic_info")[[1L]]$boundaries)
  expect_polar_build(draw(estimates, cyclic = "polar")[[1L]])
})

test_that("sdmTMB forwards cyclic metadata and plot controls", {
  skip_if_not_installed("sdmTMB", "1.2.0")
  d <- cyclic_polar_data()
  m <- sdmTMB::sdmTMB(y ~ s(x, bs = "cc", k = 6), data = d,
    spatial = "off", control = sdmTMB::sdmTMBcontrol(backend = "rtmb"))
  estimates <- smooth_estimates(m, n = 11)
  expect_length(attr(estimates, "cyclic_info"), 1L)
  p <- draw(m, cyclic = "polar", n = 11, rug = FALSE)[[1L]]
  expect_polar_build(p)
  expect_equal(range(p$data$x), attr(estimates, "cyclic_info")[[1L]]$boundaries)
  expect_polar_build(draw(estimates, cyclic = "polar")[[1L]])
})

test_that("GJRM equation metadata distinguishes repeated cyclic smooths", {
  skip_if_not_installed("GJRM", "0.2-6")
  skip_on_os("win")
  withr::local_seed(1009)
  d <- cyclic_polar_data()
  d$y <- d$y + rnorm(nrow(d), sd = exp(0.2 * sin(2 * pi * d$x)))
  m <- GJRM::gamlss(list(y ~ s(x, bs = "cc", k = 5),
    ~ s(x, bs = "cc", k = 5)), data = d)
  estimates <- smooth_estimates(m, n = 11)
  info <- attr(estimates, "cyclic_info")
  expect_length(info, 2L)
  expect_setequal(names(info), smooths(m))
  expect_false(anyDuplicated(names(info)) > 0L)
  plots <- draw(m, cyclic = "polar", parametric = FALSE,
    n = 11, rug = FALSE, wrap = FALSE)
  expect_length(plots, 2L)
  for (p in plots) expect_polar_build(p)
  saved <- draw(estimates, cyclic = "polar")
  expect_length(saved, 2L)
  for (i in seq_len(length(saved))) expect_polar_build(saved[[i]])
})

test_that("polar controls and boundary metadata reject malformed values", {
  saved <- smooth_estimates(cyclic_polar_model(), n = 7)
  for (args in list(1, list(1), stats::setNames(list(1, 2), c("start", "start")))) {
    expect_error(draw(saved, cyclic = "polar", polar_args = args),
      "polar_args.*list|unique argument names")
  }
  original <- attr(saved, "cyclic_info")
  for (bounds in list(c(NA_real_, 1), c(1, 0), c(0, 0), c(0, Inf), 1)) {
    info <- original
    info[[1L]]$boundaries <- bounds
    attr(saved, "cyclic_info") <- info
    expect_error(draw(saved, cyclic = "polar"), "Invalid.*cyclic boundary")
  }
})

test_that("polar automatic simultaneous bands use the full fitted grid", {
  m <- cyclic_polar_model()
  p <- assemble(m, cyclic = "polar", n = 13, rug = FALSE,
    interval = "simultaneous", n_sim = 100, seed = 921)[[1L]]
  reference <- smooth_estimates(m,
    data = data.frame(x = seq(0, 1, length.out = 13)),
    interval = "simultaneous", n_sim = 100, seed = 921)
  expect_equal(p$data$x, reference$x)
  expect_equal(p$data$.estimate, reference$.estimate)
  expect_equal(p$data$.lower_ci, reference$.lower_ci)
  expect_equal(p$data$.upper_ci, reference$.upper_ci)
  expect_polar_build(p)
})

test_that("supplied transformed coordinates cannot request raw polar axes", {
  d <- cyclic_polar_data()
  d$u <- exp(d$x)
  m <- mgcv::gam(y ~ s(log(u), bs = "cc", k = 7), data = d,
    knots = list("log(u)" = c(0, 1)), method = "REML")
  newdata <- data.frame(u = exp(seq(0.2, 0.6, length.out = 7)))
  expect_error(draw(m, cyclic = "polar", data = newdata,
    back_transform = TRUE, rug = FALSE), "transform|back_transform")
  saved <- smooth_estimates(m, data = newdata)
  expect_error(draw(saved, cyclic = "polar", back_transform = TRUE),
    "transform|back_transform")
})

test_that("cyclic metadata distinguishes components sharing a label", {
  d <- cyclic_polar_data()
  for (constructor in c("s", "te")) {
    formula <- stats::as.formula(sprintf(
      'y ~ %s(x, bs = "cc", k = 7) + %s(x, bs = "tp", k = 8)',
      constructor, constructor))
    m <- suppressWarnings(mgcv::gam(formula, data = d))
    saved <- smooth_estimates(m, n = 9)
    info <- attr(saved, "cyclic_info")
    expect_length(info, 1L)
    expect_equal(info[[1L]]$type, smooth_type(m$smooth[[1L]]))
    expect_equal(info[[1L]]$id, 1L)
    plots <- assemble(m, cyclic = "polar", n = 9, rug = FALSE)
    expect_polar_build(plots[[1L]])
    expect_s3_class(plots[[2L]]$coordinates, "CoordCartesian")
    saved_plots <- draw(saved, cyclic = "polar")
    expect_polar_build(saved_plots[[1L]])
    expect_s3_class(saved_plots[[2L]]$coordinates, "CoordCartesian")
  }
})

test_that("duplicate cyclic tensor labels retain separate cycle boundaries", {
  d <- cyclic_polar_data()
  m <- suppressWarnings(mgcv::gam(y ~ te(x, bs = "cc", k = 7) +
    te(x, bs = "cp", k = 8), data = d))
  saved <- smooth_estimates(m, n = 9)
  info <- attr(saved, "cyclic_info")
  expect_length(info, 2L)
  expect_equal(names(info), rep("te(x)", 2L))
  expect_equal(vapply(info, `[[`, integer(1), "id"), c(`te(x)` = 1L, `te(x)` = 2L))
  info[[1L]]$boundaries <- c(0, 1)
  info[[2L]]$boundaries <- c(0, 2)
  attr(saved, "cyclic_info") <- info
  plots <- draw(saved, cyclic = "polar")
  expect_equal(plots[[1L]]$coordinates$limits$theta, c(0, 1))
  expect_equal(plots[[2L]]$coordinates$limits$theta, c(0, 2))
})

test_that("one dimensional cyclic P spline tensor constructors draw", {
  d <- cyclic_polar_data()
  for (constructor in c("te", "ti", "t2")) {
    formula <- stats::as.formula(sprintf('y ~ %s(x, bs = "cp", k = 7)',
      constructor))
    m <- mgcv::gam(formula, data = d, knots = list(x = c(0, 1)))
    expect_polar_build(assemble(m, cyclic = "polar", n = 9, rug = FALSE)[[1L]])
  }
})

test_that("radial expansion separates interval bands from the outer rug", {
  m <- cyclic_polar_model()
  for (reverse in c("none", "r", "thetar")) {
    p <- assemble(m, cyclic = "polar", n = 31,
      polar_args = list(r.axis.inside = TRUE, reverse = reverse))[[1L]]
    built <- ggplot2::ggplot_build(p)
    panel <- built$layout$panel_params[[1L]]
    expect_equal(panel$theta.range, c(0, 1))
    expect_true(p$coordinates$r_axis_inside)
    limits <- panel$r.range
    rug_length <- 0.03 * diff(limits)
    if (reverse == "none") {
      expect_gt(limits[2L] - rug_length, max(p$data$.upper_ci))
    } else {
      expect_lt(limits[1L] + rug_length, min(p$data$.lower_ci))
    }
    expect_silent(ggplot2::ggplotGrob(p))
  }
})

test_that("radial axes default inside and respect explicit overrides", {
  m <- cyclic_polar_model()
  for (object in list(m, smooth_estimates(m, n = 11),
      smooth_estimates(m, n = 11, unnest = FALSE))) {
    p <- draw(object, cyclic = "polar")[[1L]]
    expect_true(p$coordinates$r_axis_inside)
    expect_silent(ggplot2::ggplotGrob(p))

    outside <- draw(object, cyclic = "polar",
      polar_args = list(r.axis.inside = FALSE))[[1L]]
    expect_false(outside$coordinates$r_axis_inside)
    expect_silent(ggplot2::ggplotGrob(outside))

    positioned <- draw(object, cyclic = "polar",
      polar_args = list(r.axis.inside = 0.25))[[1L]]
    expect_equal(positioned$coordinates$r_axis_inside, 0.25)
    expect_silent(ggplot2::ggplotGrob(positioned))

    automatic <- draw(object, cyclic = "polar",
      polar_args = list(r.axis.inside = NULL))[[1L]]
    expect_false(automatic$coordinates$r_axis_inside)
    expect_silent(ggplot2::ggplotGrob(automatic))
  }
  expect_true(assemble(m, cyclic = "polar")[[1L]]$coordinates$r_axis_inside)
})
