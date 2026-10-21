skip_if_not_installed("ggcube", "0.3.0")

ci3d_grid <- function() {
  d <- expand.grid(x = seq(-1, 1, length.out = 5),
    z = seq(-1, 1, length.out = 5))
  d$.estimate <- d$x^2 - d$z^2
  d$.lower_ci <- d$.estimate - 0.5 - d$x^2
  d$.upper_ci <- d$.estimate + 1 + d$z^2
  d
}

ci3d_plot <- function(d = ci3d_grid(), ...) {
  prepare_surface_plot(d, "x", "z", ".estimate", surface = "3d",
    labels = ggplot2::labs(caption = "Test surface"), ...)
}

ci3d_scene <- function(p) p$layers[[1]]$data

ci3d_data <- function() {
  withr::local_seed(548)
  d <- data.frame(x = runif(180), z = runif(180), w = runif(180),
    v = runif(180), g = factor(rep(1:18, 10)),
    f = factor(rep(c("a", "b"), 90)))
  d$y <- sin(5 * d$x) * cos(4 * d$z) + d$w + rnorm(180, sd = 0.3)
  d
}

expect_ci3d <- function(p, alpha = 0.15, colour = "grey60") {
  scene <- ci3d_scene(p)
  estimate <- scene[scene$.surface_role %in% "estimate", ]
  lower <- scene[scene$.surface_role %in% "lower", ]
  upper <- scene[scene$.surface_role %in% "upper", ]
  expect_gt(nrow(estimate), 0L)
  expect_equal(nrow(lower), nrow(estimate))
  expect_equal(nrow(upper), nrow(estimate))
  expect_equal(lower$.surface_x, estimate$.surface_x)
  expect_equal(lower$.surface_y, estimate$.surface_y)
  expect_equal(lower$.surface_z, estimate$.lower_ci)
  expect_equal(upper$.surface_z, estimate$.upper_ci)
  expect_true(all(estimate$.surface_alpha == 1))
  expect_true(all(lower$.surface_alpha == alpha))
  expect_true(all(upper$.surface_alpha == alpha))
  expect_true(all(lower$.surface_fixed_fill == colour))
  expect_true(all(upper$.surface_fixed_fill == colour))
  expect_false(any(lower$.surface_face %in% estimate$.surface_face))
  expect_false(any(upper$.surface_face %in% lower$.surface_face))
  expect_true(all(is.na(lower$.surface_fill)))
  expect_plot_render(p, cran = TRUE)
}

test_that("3D interval controls are validated even for flat plots", {
  withr::local_options(lifecycle_verbosity = "quiet")
  m <- mgcv::gam(y ~ s(x, z, k = 10), data = ci3d_data())
  for (flag in list(NA, NULL, 1, c(TRUE, FALSE))) {
    expect_error(draw(m, ci_3d = flag), "'ci_3d'")
  }
  for (alpha in list(NA, Inf, -0.1, 1.1, c(0.1, 0.2), "0.2")) {
    expect_error(assemble(m, ci_3d_alpha = alpha), "'ci_3d_alpha'")
  }
  for (colour in list(NA, NULL, 1, c("red", "blue"), "no-such-colour")) {
    expect_error(draw(m, ci_3d_col = colour), "'ci_3d_col'")
  }
  for (alpha in c(0, 1)) {
    expect_ci3d(ci3d_plot(ci_3d = TRUE, ci_3d_alpha = alpha), alpha = alpha)
  }
})

test_that("neutral bounds share geometry without changing the fill palette", {
  a <- ci3d_plot(contour = FALSE)
  b <- ci3d_plot(ci_3d = TRUE, ci_3d_alpha = 0.12, ci_3d_col = "grey50",
    contour = FALSE)
  expect_ci3d(b, alpha = 0.12, colour = "grey50")
  built_a <- ggplot2::ggplot_build(a)
  built_b <- ggplot2::ggplot_build(b)
  expect_equal(built_a$plot$scales$get_scales("fill")$get_limits(),
    built_b$plot$scales$get_scales("fill")$get_limits())
  expect_equal(built_b$plot$scales$get_scales("z")$get_limits(),
    range(ci3d_grid()[c(".estimate", ".lower_ci", ".upper_ci")]))
  expect_identical(b$labels$caption, a$labels$caption)
  expect_identical(b$labels$caption, "Test surface")
  expect_equal(ci3d_scene(a), ci3d_scene(ci3d_plot(ci_3d = FALSE, contour = FALSE)))
})

test_that("interval faces retain holes and omit only missing bound faces", {
  d <- ci3d_grid()
  d$.estimate[d$x == 0 & d$z == 0] <- NA_real_
  p <- ci3d_plot(d, ci_3d = TRUE, contour = FALSE)
  expect_ci3d(p)
  scene <- ci3d_scene(p)
  expect_false(any(scene$.surface_x == 0 & scene$.surface_y == 0))
  d$.lower_ci[1] <- NA_real_
  p <- ci3d_plot(d, ci_3d = TRUE, contour = FALSE)
  scene <- ci3d_scene(p)
  expect_lt(sum(scene$.surface_role == "lower"), sum(scene$.surface_role == "estimate"))
  expect_equal(sum(scene$.surface_role == "upper"), sum(scene$.surface_role == "estimate"))
  expect_plot_render(p, cran = TRUE)
  duplicate <- d[2, ]
  duplicate$.upper_ci <- duplicate$.upper_ci + 1
  expect_error(ci3d_plot(rbind(d, duplicate), ci_3d = TRUE), "Conflicting heights")
  expect_silent(ci3d_plot(rbind(d, duplicate), ci_3d = FALSE))
  d$.lower_ci <- NA_real_
  expect_plot_render(ci3d_plot(d, ci_3d = TRUE), cran = TRUE)
  d$.estimate <- NA_real_
  expect_plot_render(ci3d_plot(d, ci_3d = TRUE), cran = TRUE)
})

test_that("scatter topology and empty conditioning facets are retained", {
  skip_if_not_installed("geometry")
  d <- ci3d_grid()[-c(1, 5), ]
  expect_ci3d(ci3d_plot(d, ci_3d = TRUE))
  d <- rbind(transform(d, slice = "a"), transform(d, slice = "b", .estimate = NA_real_))
  p <- ci3d_plot(d, ci_3d = TRUE,
    facet = ggplot2::facet_wrap(~slice), facet_vars = "slice")
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
  expect_ci3d(p)
})

test_that("GAM entry points preserve transformed and stored interval heights", {
  d <- ci3d_data()
  m <- mgcv::gam(y ~ te(x, z, k = c(4, 4)) + s(w, k = 4) + x:v,
    data = d, method = "REML")
  for (interval in c("confidence", "simultaneous")) {
    options <- list(surface = "3d", ci_3d = TRUE, n_2d = 5, n = 8,
      interval = interval, n_sim = 100, seed = 6, rug = FALSE,
      constant = 1, fun = function(x) -exp(x))
    a <- do.call(assemble, c(list(object = m), options))
    b <- do.call(draw, c(list(object = m, wrap = FALSE), options))
    expect_ci3d(a[[1]])
    expect_ci3d(a[[3]])
    expect_equal(ci3d_scene(a[[1]]), ci3d_scene(b[[1]]))
    expect_equal(ci3d_scene(a[[3]]), ci3d_scene(b[[3]]))
    expect_s3_class(a[[2]]$layers[[1]]$geom, "GeomRibbon")
    baseline <- do.call(assemble, modifyList(c(list(object = m), options),
      list(ci_3d = FALSE)))
    for (i in seq_along(a)) {
      expect_identical(a[[i]]$labels$caption, baseline[[i]]$labels$caption)
    }
    sm <- smooth_estimates(m, select = 1, n_2d = 5, interval = interval,
      n_sim = 100, seed = 6)
    nested <- smooth_estimates(m, select = 1, n_2d = 5, interval = interval,
      n_sim = 100, seed = 6, unnest = FALSE)
    for (saved in list(sm, nested)) {
      p <- draw(saved, surface = "3d", ci_3d = TRUE)[[1]]
      expect_ci3d(p)
    }
    p <- draw(sm, surface = "3d", ci_3d = TRUE)[[1]]
    baseline <- draw(sm, surface = "3d", ci_3d = FALSE)[[1]]
    expect_identical(p$labels$caption, baseline$labels$caption)
    expect_equal(p$data$.lower_ci, sm$.lower_ci)
    expect_equal(p$data$.upper_ci, sm$.upper_ci)
  }
  expect_s3_class(draw(m, surface = "3d", ci_3d = TRUE, n_2d = 5), "patchwork")
  for (show in c("raster", "tile")) {
    a <- assemble(m, surface = show, ci_3d = TRUE, n_2d = 5)
    b <- assemble(m, surface = show, n_2d = 5)
    expect_equal(ggplot2::ggplot_build(a[[1]])$data, ggplot2::ggplot_build(b[[1]])$data)
  }
  p <- draw(m, surface = "3d", ci_3d = TRUE, show = "se", n_2d = 5,
    parametric = FALSE, caption = FALSE)[[1]]
  expect_false(any(ci3d_scene(p)$.surface_role %in% c("lower", "upper")))
  expect_null(p$labels$caption)
  p <- draw(m, surface = "3d", ci_3d = TRUE, caption = FALSE, n_2d = 5)[[3]]
  expect_null(p$labels$caption)
})

test_that("tensor types and higher dimensional TPRS retain bounds and facets", {
  d <- ci3d_data()
  for (term in c("te(x,z,k=c(3,3))", "ti(x,z,k=c(3,3))", "t2(x,z,k=c(3,3))",
    "s(x,z,k=10)", "s(x,z,w,k=12)", "s(x,z,w,v,k=16)",
    "te(x,z,w,k=c(3,3,3))", "te(x,z,w,v,k=c(3,3,3,3))",
    "te(x,z,by=f,k=c(3,3))")) {
    m <- mgcv::gam(stats::as.formula(paste("y ~", term)), data = d)
    plots <- draw(m, surface = "3d", ci_3d = TRUE, n_2d = 4, n_3d = 2,
      n_4d = 2, rug = FALSE, wrap = FALSE)
    for (p in plots) expect_ci3d(p)
    if (grepl("x,z,w", term, fixed = TRUE)) {
      expect_equal(nrow(ggplot2::ggplot_build(plots[[1]])$layout$layout),
        if (grepl("x,z,w,v", term, fixed = TRUE)) 4L else 2L)
    }
  }
})

test_that("parametric-only models and saved effects use the same interval renderer", {
  d <- ci3d_data()
  m <- mgcv::gam(y ~ x:z + x:w:f, data = d)
  a <- assemble(m, surface = "3d", ci_3d = TRUE, n_2d = 5,
    scales = "fixed", constant = 2, fun = exp)
  for (p in a) expect_ci3d(p)
  limits <- lapply(a, function(p) ggplot2::ggplot_build(p)$plot$scales$get_scales("z")$get_limits())
  expect_equal(limits[[1]], limits[[2]])
  for (unnest in c(TRUE, FALSE)) {
    pe <- parametric_effects(m, n_2d = 5, unnest = unnest)
    p <- draw(pe, surface = "3d", ci_3d = TRUE, constant = 2, fun = exp)
    for (i in seq_along(p)) expect_ci3d(p[[i]])
  }
})

test_that("3D interval transparency has visual coverage from several views", {
  p <- ci3d_plot(ci_3d = TRUE, rug = data.frame(x = c(-1, 1), z = c(-1, 1)))
  expect_doppelganger("3D translucent interval surfaces", p)
  expect_doppelganger("3D interval bounds below",
    ci3d_plot(ci_3d = TRUE, surface_3d = list(pitch = -30, yaw = 110, persp = FALSE)))
})

test_that("GAM fitting wrappers forward surface bounds", {
  d <- ci3d_data()
  models <- list(
    mgcv::bam(y ~ s(x, z, k = 10) + x:w, data = d),
    mgcv::gamm(y ~ s(x, z, k = 10) + x:w, data = d)
  )
  if (requireNamespace("gamm4", quietly = TRUE)) {
    models <- c(models, list(gamm4::gamm4(y ~ s(x, z, k = 10) + x:w,
      random = ~(1 | g), data = d)))
  }
  for (m in models) {
    p <- draw(m, surface = "3d", ci_3d = TRUE, n_2d = 4, wrap = FALSE)
    expect_length(p, 2)
    for (plot in p) expect_ci3d(plot)
  }
})

test_that("SCAM and scasm surface bounds retain their fitted intervals", {
  skip_if_not_installed("scam")
  d <- ci3d_data()
  d$y <- 2 * d$x + d$z + d$w + d$y / 10
  m <- scam::scam(y ~ s(x, z, k = 10) + x:w, data = d)
  p <- draw(m, surface = "3d", ci_3d = TRUE, n_2d = 4, wrap = FALSE)
  for (plot in p) expect_ci3d(plot)
  skip_if_not(exists("scasm", asNamespace("mgcv")))
  m <- mgcv::scasm(y ~ te(x, z, bs = c("sc", "cr"),
    xt = list("m+", NULL), k = c(5, 5)) + x:w, data = d, bs = 8)
  p <- draw(m, surface = "3d", ci_3d = TRUE, n_2d = 4, wrap = FALSE,
    interval_method = "gaussian")
  for (plot in p) expect_ci3d(plot)
  expect_true(is.expression(p[[1]]$labels$caption))
  baseline <- draw(m, surface = "3d", ci_3d = FALSE, n_2d = 4,
    wrap = FALSE, interval_method = "gaussian")
  for (i in seq_along(p)) {
    expect_identical(p[[i]]$labels$caption, baseline[[i]]$labels$caption)
  }
})

test_that("GJRM shares vertical ranges across equations without changing covariates", {
  skip_if_not_installed("GJRM", "0.2-6")
  d <- ci3d_data()
  m <- GJRM::gamlss(list(y ~ s(x, z, k = 10) + x:w,
    ~ s(x, z, k = 10) + x:w + s(v, k = 4)), data = d)
  options <- list(object = m, surface = "3d", ci_3d = TRUE, n_2d = 4,
    n = 8, wrap = FALSE, rug = FALSE)
  free <- do.call(draw, options)
  fixed <- do.call(assemble, c(options, list(scales = "fixed")))
  expect_length(fixed, 5)
  expect_identical(names(fixed), names(free))
  vertical <- list()
  for (i in seq_along(fixed)) {
    a <- ggplot2::ggplot_build(free[[i]])
    b <- ggplot2::ggplot_build(fixed[[i]])
    if (inherits(fixed[[i]]$coordinates, "Coord3D")) {
      expect_ci3d(fixed[[i]])
      vertical[[i]] <- b$plot$scales$get_scales("z")$get_limits()
      expect_equal(a$layout$panel_scales_x[[1]]$get_limits(), b$layout$panel_scales_x[[1]]$get_limits())
      expect_equal(a$layout$panel_scales_y[[1]]$get_limits(), b$layout$panel_scales_y[[1]]$get_limits())
    } else vertical[[i]] <- b$layout$panel_scales_y[[1]]$get_limits()
  }
  for (limits in vertical) expect_equal(limits, vertical[[1]])
  selected <- draw(m, select = 2, parametric = FALSE, surface = "3d",
    ci_3d = TRUE, n_2d = 4, wrap = FALSE)
  expect_length(selected, 1)
  expect_ci3d(selected[[1]])
  expect_match(selected[[1]]$labels$subtitle, "sigma")
  expect_s3_class(do.call(draw, modifyList(options, list(wrap = TRUE))), "patchwork")
})

test_that("glmmTMB forwards bounds for smooth and parametric distributional effects", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_not_installed("glmmTMB", "1.1.15.2")
  d <- ci3d_data()
  m <- glmmTMB::glmmTMB(y ~ s(x, z, k = 10) + x:w,
    dispformula = ~ s(x, z, k = 8) + x:w, data = d, REML = TRUE)
  p <- draw(m, surface = "3d", ci_3d = TRUE, ci_3d_alpha = 0.1,
    ci_3d_col = "grey40", n_2d = 4, wrap = FALSE)
  expect_length(p, 4)
  for (plot in p) expect_ci3d(plot, alpha = 0.1, colour = "grey40")
  sm <- smooth_estimates(m, n_2d = 4)
  saved <- draw(sm, surface = "3d", ci_3d = TRUE)
  for (i in seq_along(saved)) expect_ci3d(saved[[i]])
  only_parametric <- glmmTMB::glmmTMB(y ~ x:z, data = d)
  p <- draw(only_parametric, surface = "3d", ci_3d = TRUE, n_2d = 4)[[1]]
  expect_ci3d(p)
})

test_that("sdmTMB drawing and assembly forward bounds for both delta components", {
  skip_if_not_installed("sdmTMB", minimum_version = "1.2.0")
  d <- ci3d_data()
  d$positive <- withr::with_seed(114,
    ifelse(runif(nrow(d)) < 0.6, exp(d$y), 0))
  m <- sdmTMB::sdmTMB(positive ~ s(x, z, k = 8) + x:w,
    data = d, spatial = "off",
    family = sdmTMB::delta_gamma())
  a <- assemble(m, surface = "3d", ci_3d = TRUE, n_2d = 4)
  b <- draw(m, surface = "3d", ci_3d = TRUE, n_2d = 4, wrap = FALSE)
  expect_length(a, 4)
  for (i in seq_along(a)) {
    expect_ci3d(a[[i]])
    expect_equal(ci3d_scene(a[[i]]), ci3d_scene(b[[i]]))
  }
  only_parametric <- sdmTMB::sdmTMB(y ~ x:z, data = d, spatial = "off")
  expect_ci3d(draw(only_parametric, surface = "3d", ci_3d = TRUE, n_2d = 4)[[1]])
})

test_that("soap and original-scale surfaces retain bound support", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geometry")
  p <- draw(m_soap, surface = "3d", ci_3d = TRUE, n_2d = 12, rug = FALSE)[[1]]
  expect_ci3d(p)
  d <- ci3d_data()
  d$x <- exp(d$x)
  m <- mgcv::gam(y ~ te(log(x), z, k = c(3, 3)), data = d)
  p <- draw(m, surface = "3d", ci_3d = TRUE, back_transform = TRUE,
    n_2d = 6, dist = 0.2, envir = environment())[[1]]
  expect_ci3d(p)
  expect_gte(min(ci3d_scene(p)$.surface_x, na.rm = TRUE), 1)
})
