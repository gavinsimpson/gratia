surface_plot_data <- function(dimension = 2L) {
  grid <- list(x = seq(2, 8, by = 2), y = seq(10, 25, by = 5))
  if (dimension > 2L) grid$z <- c(0, 1)
  if (dimension > 3L) grid$w <- c(2, 4)
  d <- tibble::as_tibble(expand.grid(grid))
  d$.estimate <- sin(d$x) + d$y / 10
  d$.se <- 0.1 + d$x / 100
  d$.lower_ci <- d$.estimate - d$.se
  d$.upper_ci <- d$.estimate + d$.se
  d$.smooth <- "s(x,y)"
  d$.type <- "TPRS (2d)"
  d$.by <- NA_character_
  class(d) <- c("mgcv_smooth", class(d))
  d
}

surface_plot_geoms <- function(p) {
  unname(vapply(p$layers, function(l) class(l$geom)[1L], character(1)))
}

test_that("surface methods preserve facets, fill values and observations", {
  methods <- list(plot_smooth.bivariate_smooth, plot_smooth.trivariate_smooth,
    plot_smooth.quadvariate_smooth)
  for (i in seq_along(methods)) {
    d <- surface_plot_data(i + 1L)
    variables <- c("x", "y", "z", "w")[seq_len(i + 1L)]
    p <- methods[[i]](d, variables = variables, rug = d,
      contour = FALSE, constant = 1, fun = exp, angle = 45)
    expect_equal(surface_plot_geoms(p), c("GeomRaster", "GeomBlank",
      if (i == 1L) "GeomPoint"))
    expect_equal(p$data$.estimate, exp(d$.estimate + 1))
    expect_equal(p$data$.se, d$.se)
    expect_equal(p$labels$x, "x")
    expect_equal(p$labels$y, "y")
    expect_equal(p$guides$guides$x$params$angle, 45)
    expect_equal(p$guides$guides$fill$params$title, "Partial\neffect")
    expect_equal(p$theme$legend.position, "right")
    expect_silent(b <- ggplot2::ggplot_build(p))
    expect_equal(nrow(b$layout$layout), c(1L, 2L, 4L)[i])
    if (i == 3L) expect_false(p$facet$params$as.table)
    p_se <- methods[[i]](d, variables = variables, show = "se",
      contour = FALSE, ylim = c(-100, 100), caption = FALSE)
    expect_equal(p_se$guides$guides$fill$params$title, "Std. err.")
    expect_equal(p_se$layers[[2]]$data$fill, range(d$.se))
    expect_null(p_se$labels$caption)
    expect_silent(ggplot2::ggplot_build(p_se))
  }
})

test_that("surface contours, labels and explicit range expansion are preserved", {
  d <- surface_plot_data()
  d$.by <- "f"
  d$f <- ordered(rep("a", nrow(d)))
  d$.smooth <- "s(x,y):fa"
  p <- plot_smooth.bivariate_smooth(d, variables = c("x", "y"),
    contour_col = "red", n_contour = 4, ylim = c(-10, 10),
    continuous_fill = ggplot2::scale_fill_viridis_c(),
    xlab = "X", ylab = "Y", title = "Custom: suffix")
  expect_equal(surface_plot_geoms(p), c("GeomRaster", "GeomContour", "GeomBlank"))
  expect_equal(p$labels$title, "Custom")
  expect_equal(p$labels$subtitle, "By: f; a")
  expect_equal(p$labels$x, "X")
  expect_equal(p$labels$y, "Y")
  expect_equal(p$layers[[2]]$stat_params$bins, 4)
  expect_equal(p$layers[[2]]$aes_params$colour, "red")
  expect_equal(p$layers[[3]]$data$fill, c(-10, 10))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("soap surfaces keep boundary loops last and isotropic coordinates", {
  d <- surface_plot_data()
  d$.bndry <- FALSE
  d$.loop <- NA_integer_
  boundary <- d[c(1, 4, 16, 13, 1, 6, 7, 11, 10, 6), ]
  boundary$.bndry <- TRUE
  boundary$.loop <- rep(1:2, each = 5)
  boundary$.estimate <- boundary$.se <- NA_real_
  object <- dplyr::bind_rows(d, boundary)
  p <- plot_smooth.soap_film(object, variables = c("x", "y"), rug = d,
    contour = FALSE)
  expect_equal(surface_plot_geoms(p),
    c("GeomRaster", "GeomBlank", "GeomPoint", "GeomPath"))
  expect_equal(nrow(p$data), nrow(d))
  expect_equal(p$layers[[4]]$data$.loop, boundary$.loop)
  expect_equal(p$coordinates$ratio, 1)
  expect_silent(b <- ggplot2::ggplot_build(p))
  expect_equal(length(unique(b$data[[4]]$group)), 2L)
  class(d) <- c("isotropic_smooth", class(d))
  p <- plot_smooth.isotropic_smooth(d, variables = c("x", "y"), contour = FALSE)
  expect_equal(p$coordinates$ratio, 1)
})

test_that("raster and tile surfaces share coordinates, facets and contours", {
  methods <- list(plot_smooth.bivariate_smooth, plot_smooth.trivariate_smooth,
    plot_smooth.quadvariate_smooth)
  for (i in seq_along(methods)) {
    d <- surface_plot_data(i + 1L)
    variables <- c("x", "y", "z", "w")[seq_len(i + 1L)]
    for (show in c("estimate", "se")) {
      default <- methods[[i]](d, variables = variables, show = show)
      raster <- methods[[i]](d, variables = variables, show = show, geom = "raster")
      tile <- methods[[i]](d, variables = variables, show = show, geom = "tile")
      expect_s3_class(default$layers[[1]]$geom, "GeomRaster")
      expect_s3_class(tile$layers[[1]]$geom, "GeomTile")
      expect_equal(tile$labels, raster$labels)
      expect_silent(br <- ggplot2::ggplot_build(raster))
      expect_silent(bt <- ggplot2::ggplot_build(tile))
      expect_equal(ggplot2::ggplot_build(default)$data, br$data)
      cols <- c("x", "y", "xmin", "xmax", "ymin", "ymax", "fill", "PANEL")
      expect_equal(bt$data[[1]][cols], br$data[[1]][cols])
      expect_equal(bt$data[[2]], br$data[[2]])
      expect_equal(bt$layout$layout, br$layout$layout)
      expect_equal(unique(bt$data[[1]]$xmax - bt$data[[1]]$xmin), 2)
      expect_equal(unique(bt$data[[1]]$ymax - bt$data[[1]]$ymin), 5)
      expect_true(all(is.na(bt$data[[1]]$colour)))
    }
    expect_error(methods[[i]](d, variables = variables, geom = "invalid"),
      "'arg' should be one of")
  }
  expect_error(prepare_surface_plot(surface_plot_data(), "x", "y", ".estimate",
    geom = "invalid"), "'arg' should be one of")
  expect_error(plot_smooth.isotropic_smooth(surface_plot_data(), geom = "invalid"),
    "'arg' should be one of")
  expect_error(plot_smooth.soap_film(surface_plot_data(), geom = "invalid"),
    "'arg' should be one of")
})

test_that("surface ranges ignore non-finite values without changing plot data", {
  d <- surface_plot_data()
  d$.estimate[1:4] <- c(NA, NaN, Inf, -Inf)
  d$.se[1:4] <- c(NA, NaN, Inf, -Inf)
  for (show in c("estimate", "se")) {
    s <- prepare_surface_data(d, show = show)
    values <- d[[s$fill_var]]
    finite <- values[is.finite(values)]
    expected <- if (show == "estimate") c(-1, 1) * max(abs(finite)) else range(finite)
    expect_equal(s$fill_limits, expected)
    expect_identical(s$data, d)
    no_finite <- d
    no_finite[[s$fill_var]] <- rep(c(NA, NaN, Inf, -Inf), length.out = nrow(d))
    expect_silent(s_empty <- prepare_surface_data(no_finite, show = show))
    expect_null(s_empty$fill_limits)
    p <- plot_smooth.bivariate_smooth(no_finite, variables = c("x", "y"),
      show = show, contour = FALSE)
    expect_equal(surface_plot_geoms(p), "GeomRaster")
  }
  expect_equal(prepare_surface_data(d, ylim = c(-20, 20))$fill_limits, c(-20, 20))
  d <- surface_plot_data()
  expect_equal(prepare_surface_data(d, constant = 1, fun = exp)$fill_limits,
    c(-1, 1) * max(exp(d$.estimate + 1)))
  expect_equal(prepare_surface_data(d, show = "se", constant = 1, fun = exp,
    ylim = c(-20, 20))$fill_limits, range(d$.se))
})

test_that("surface faceting keeps captions, aspect ratios and tensor order", {
  for (dimension in 3:4) {
    d <- surface_plot_data(dimension)
    class(d) <- c("isotropic_smooth", class(d))
    method <- if (dimension == 3L) plot_smooth.trivariate_smooth else
      plot_smooth.quadvariate_smooth
    variables <- c("x", "y", "z", "w")[seq_len(dimension)]
    for (geom in c("raster", "tile")) {
      p <- method(d, variables = variables, geom = geom, contour = FALSE, rug = d)
      expect_equal(p$coordinates$ratio, 1)
      expect_false("GeomPoint" %in% surface_plot_geoms(p))
      expected <- if (dimension == 3L) "Facets: z ; Basis: TPRS (2d)" else
        "Facet rows: z ; columns: w ; Basis: TPRS (2d)"
      expect_equal(p$labels$caption, rep(expected, nrow(d)))
    }
  }
  d <- surface_plot_data(3)
  attr(d, "tensor_term_order") <- c("y", "x", "z")
  p <- plot_smooth.trivariate_smooth(d, geom = "tile", contour = FALSE)
  expect_equal(p$labels$x, "y")
  expect_equal(p$labels$y, "x")
  names(d)[1] <- "log(x)"
  p <- plot_smooth.bivariate_smooth(d, variables = c("log(x)", "y"),
    geom = "tile", contour = FALSE)
  expect_equal(p$labels$x, "log(x)")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("tile selection propagates through model drawing and assembly", {
  withr::local_seed(308)
  d <- data.frame(x = runif(90), z = runif(90), u = runif(90),
    f = factor(rep(1:3, each = 30)))
  d$y <- sin(d$x * 4) + d$z + d$u + rnorm(90, sd = 0.2)
  m <- mgcv::gam(y ~ u + s(x, z, k = 8) + s(u, k = 4) + s(f, bs = "re"), data = d)
  sm <- smooth_estimates(m, n = 8, n_2d = 5)
  for (geom in c("raster", "tile")) {
    args <- list(object = m, geom = geom, n = 8, n_2d = 5, rug = FALSE)
    plots <- list(do.call(draw, args), do.call(assemble, args),
      do.call(draw, c(args, list(wrap = FALSE))), draw(sm, geom = geom))
    for (p in plots) {
      expect_s3_class(p[[1]]$layers[[1]]$geom,
        if (geom == "tile") "GeomTile" else "GeomRaster")
      expect_s3_class(p[[2]]$layers[[1]]$geom, "GeomRibbon")
      expect_s3_class(p[[3]]$layers[[1]]$geom, "GeomPoint")
      expect_equal(nrow(p[[1]]$data), 25L)
      expect_silent(ggplot2::ggplot_build(p[[1]]))
    }
    with_parametric <- draw(m, parametric = TRUE, geom = geom,
      n = 8, n_2d = 5, rug = FALSE, wrap = FALSE)
    expect_gt(length(with_parametric), 3L)
    expect_false(inherits(with_parametric[[4]]$layers[[1]]$geom, "GeomTile"))
  }
  expect_error(draw(m, geom = "invalid"), "'arg' should be one of")
  expect_error(assemble(m, geom = "invalid"), "'arg' should be one of")
  expect_error(draw(sm, geom = "invalid"), "'arg' should be one of")
})

test_that("soap tile rendering keeps holes, observations and SE fill ranges", {
  sm <- smooth_estimates(m_soap, n_2d = 8, clip = TRUE)
  for (geom in c("raster", "tile")) {
    p <- draw(sm, geom = geom, show = "se", contour = FALSE)[[1]]
    expect_s3_class(p$layers[[1]]$geom, if (geom == "tile") "GeomTile" else "GeomRaster")
    expect_equal(tail(surface_plot_geoms(p), 1L), "GeomPath")
    expect_false(any(p$data$.bndry))
    expect_true(all(is.finite(p$layers[[2]]$data$fill)))
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("surface tiles have stable faceted and soap boundary appearances", {
  d <- surface_plot_data(3)
  faceted <- plot_smooth.trivariate_smooth(d, variables = c("x", "y", "z"),
    geom = "tile", n_contour = 4)
  d <- surface_plot_data()
  d$.bndry <- FALSE
  d$.loop <- NA_integer_
  boundary <- d[c(1, 4, 16, 13, 1, 6, 7, 11, 10, 6), ]
  boundary$.bndry <- TRUE
  boundary$.loop <- rep(1:2, each = 5)
  boundary$.estimate <- boundary$.se <- NA_real_
  soap <- plot_smooth.soap_film(dplyr::bind_rows(d, boundary),
    variables = c("x", "y"), geom = "tile", contour = FALSE)
  expect_doppelganger("faceted surface tiles", faceted)
  expect_doppelganger("soap surface tiles with boundary loops", soap)
})

test_that("tensor surfaces forward geometry and retain marginal ordering", {
  withr::local_seed(309)
  d <- data.frame(x = runif(120), z = runif(120), u = runif(120), v = runif(120))
  d$y <- sin(4 * d$x) + d$z + d$u + d$v + rnorm(120, sd = 0.2)
  specs <- c("x, z, k = c(3, 3)",
    "x, z, u, d = c(1, 2), k = c(3, 6)",
    "x, z, u, v, d = c(2, 2), k = c(5, 5)")
  for (constructor in c("te", "ti", "t2")) {
    for (i in seq_along(specs)) {
      m <- mgcv::gam(as.formula(paste0("y ~ ", constructor, "(", specs[i], ")")), data = d)
      sm <- smooth_estimates(m, n_2d = 4, n_3d = 2, n_4d = 2)
      variables <- attr(sm, "tensor_term_order")[[1]]
      for (geom in c("raster", "tile")) {
        for (p in list(draw(m, geom = geom, n_2d = 4, n_3d = 2, n_4d = 2,
          rug = FALSE, contour = FALSE), draw(sm, geom = geom, contour = FALSE))) {
          panel <- p[[1]]
          expect_s3_class(panel$layers[[1]]$geom,
            if (geom == "tile") "GeomTile" else "GeomRaster")
          expect_equal(panel$labels$x, variables[1])
          expect_equal(panel$labels$y, variables[2])
          expect_silent(b <- ggplot2::ggplot_build(panel))
          expect_equal(nrow(b$layout$layout), 2L ^ (i - 1L))
          expect_equal(nrow(panel$data), 16L * 2L ^ (i - 1L))
        }
      }
    }
  }
})

test_that("GAMM and BAM drawing forward surface geometry", {
  withr::local_seed(310)
  d <- data.frame(x = runif(80), z = runif(80))
  d$y <- sin(4 * d$x) + d$z + rnorm(80, sd = 0.2)
  models <- list(mgcv::gamm(y ~ s(x, z, k = 8), data = d),
    mgcv::bam(y ~ s(x, z, k = 8), data = d))
  for (m in models) {
    p <- draw(m, geom = "tile", n_2d = 5, rug = FALSE)[[1]]
    expect_s3_class(p$layers[[1]]$geom, "GeomTile")
    expect_silent(ggplot2::ggplot_build(p))
    expect_error(draw(m, geom = "invalid"), "'arg' should be one of")
  }
})

test_that("gamm4 drawing forwards surface geometry", {
  skip_if_not_installed("gamm4")
  withr::local_seed(311)
  d <- data.frame(x = runif(80), z = runif(80))
  d$y <- sin(4 * d$x) + d$z + rnorm(80, sd = 0.2)
  m <- gamm4::gamm4(y ~ s(x, z, k = 8), data = d)
  p <- draw(m, geom = "tile", n_2d = 5, rug = FALSE)[[1]]
  expect_s3_class(p$layers[[1]]$geom, "GeomTile")
  expect_silent(ggplot2::ggplot_build(p))
  expect_error(draw(m, geom = "invalid"), "'arg' should be one of")
})

test_that("SCAM accepts surface geometry without changing its univariate panels", {
  skip_if_not_installed("scam")
  p <- draw(m_scam, geom = "tile", n = 40, rug = FALSE)
  expect_s3_class(p[[1]]$layers[[1]]$geom, "GeomRibbon")
  expect_silent(ggplot2::ggplot_build(p[[1]]))
  expect_error(draw(m_scam, geom = "invalid"), "'arg' should be one of")
})

test_that("spherical surfaces retain their tile geometry and projection", {
  skip_if_not_installed("sf")
  sm <- smooth_estimates(m_sos, n_2d = 5)
  a <- draw(sm, geom = "raster", contour = FALSE)[[1]]
  b <- draw(sm, geom = "tile", contour = FALSE)[[1]]
  expect_s3_class(a$layers[[1]]$geom, "GeomTile")
  expect_s3_class(b$layers[[1]]$geom, "GeomTile")
  expect_s3_class(a$coordinates, "CoordSf")
  expect_equal(a$data, b$data)
  expect_equal(a$labels, b$labels)
})
