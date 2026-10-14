skip_if_not_installed("ggcube", "0.3.0")

mesh_data <- function(n = 5L) {
  d <- expand.grid(
    x = seq(-1, 1, length.out = n),
    y = seq(-1, 1, length.out = n)
  )
  d$effect <- d$x^2 - d$y^2
  d
}

mesh_plot <- function(d, ...) {
  prepare_surface_plot(d, "x", "y", "effect",
    surface = "3d",
    fill_limits = surface_fill_limits(d$effect), fill_title = "Partial effect",
    labels = ggplot2::labs(x = "x", y = "y"), ...
  )
}

test_that("grid meshes preserve holes and reject conflicting vertices", {
  d <- mesh_data()
  mesh <- surface_mesh(d, "x", "y", "effect")
  expect_equal(nrow(mesh), 16L * 6L)
  d$effect[d$x == 0 & d$y == 0] <- NA_real_
  masked <- surface_mesh(d, "x", "y", "effect")
  expect_equal(nrow(masked), 12L * 6L)
  expect_true(all(is.finite(masked$.surface_z)))
  expect_equal(surface_mesh(rbind(d, d[1L, ]), "x", "y", "effect"), masked)
  conflict <- d[1L, ]
  conflict$effect <- 9
  expect_error(
    surface_mesh(rbind(d, conflict), "x", "y", "effect"),
    "Conflicting heights"
  )
  d$effect <- NA_real_
  expect_equal(nrow(surface_mesh(d, "x", "y", "effect")), 0L)
  expect_plot_render(mesh_plot(d), cran = TRUE)
  expect_error(surface_mesh(
    data.frame(x = 1:4, y = 1:4, effect = 1:4),
    "x", "y", "effect"
  ), "non-collinear")
})

test_that("3D faces retain diverging fills and train the vertex range", {
  d <- mesh_data()
  p <- mesh_plot(d, contour = FALSE)
  expect_s3_class(p$coordinates, "Coord3D")
  expect_s3_class(p$layers[[1]]$geom, "GeomPolygon3D")
  expect_true(is.na(p$layers[[1]]$aes_params$colour))
  expect_equal(p$layers[[1]]$aes_params$alpha, 1)
  b <- ggplot2::ggplot_build(p)
  scale <- b$plot$scales$get_scales("fill")
  default <- ggplot2::scale_fill_distiller(palette = "RdBu", type = "div")
  expect_equal(scale$palette(c(0, 0.5, 1)), default$palette(c(0, 0.5, 1)))
  expect_equal(scale$get_limits(), c(-1, 1))
  expect_equal(b$plot$scales$get_scales("z")$get_limits(), c(-1, 1))
  expect_equal(scale$map(0), default$palette(0.5))
  expect_plot_render(p, cran = TRUE)
  p <- mesh_plot(d, continuous_fill = ggplot2::scale_fill_viridis_c())
  expect_equal(
    ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$palette(0.5),
    ggplot2::scale_fill_viridis_c()$palette(0.5)
  )
})

test_that("contours follow retained mesh faces at their true heights", {
  d <- mesh_data()
  d$effect[d$x == 0 & d$y == 0] <- NA_real_
  mesh <- surface_mesh(d, "x", "y", "effect")
  contours <- surface_contours(mesh, c(-0.5, 0.5))
  expect_gt(nrow(contours), 0L)
  expect_equal(contours$.surface_z, contours$.surface_zend)
  expect_true(all(contours$.surface_z %in% c(-0.5, 0.5)))
  expect_true(all(contours$.surface_face %in% mesh$.surface_face))
  expect_equal(nrow(surface_contours(mesh, numeric())), 0L)
  d$effect <- 0
  p <- mesh_plot(d)
  expect_false(any(p$layers[[1]]$data$.surface_type == "segment"))
  expect_plot_render(p)
  p <- mesh_plot(mesh_data(), n_contour = 4, contour_col = "red")
  scene <- p$layers[[1]]$data
  segments <- scene[scene$.surface_type == "segment", ]
  expect_gt(nrow(segments), 0L)
  expect_equal(unique(segments$.surface_colour), "red")
  expect_plot_render(p, cran = TRUE)
})

test_that("observation floor projections do not train the effect axis", {
  d <- mesh_data()
  p <- mesh_plot(d, rug = data.frame(x = 0, y = 0), contour = FALSE)
  scene <- p$layers[[1]]$data
  expect_true(all(scene$.surface_floor[scene$.surface_type != "polygon"]))
  a <- ggplot2::ggplot_build(p)$plot$scales$get_scales("z")$get_limits()
  b <- ggplot2::ggplot_build(mesh_plot(d, contour = FALSE))$plot$scales$get_scales("z")$get_limits()
  expect_equal(a, b)
  expect_plot_render(p)
})

test_that("facets keep separate topology, a common palette, and empty slices", {
  d <- mesh_data()
  d <- rbind(
    transform(d, slice = "a"), transform(d, slice = "b", effect = effect * 4),
    transform(d, slice = "empty", effect = NA_real_)
  )
  p <- mesh_plot(d, facet = ggplot2::facet_wrap(~slice), facet_vars = "slice")
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$layout$layout), 3L)
  expect_length(unique(b$data[[1]]$PANEL), 2L)
  expect_equal(b$plot$scales$get_scales("fill")$get_limits(), c(-4, 4))
  expect_plot_render(p)
  if (full_test_run()) {
    expect_silent(patchwork::patchworkGrob(
      patchwork::wrap_plots(p, mesh_plot(mesh_data()))))
  }
})

test_that("scattered meshes triangulate masked points before discarding faces", {
  skip_if_not_installed("geometry")
  d <- data.frame(
    x = c(-1, 1, 1, -1, 0, 0.2),
    y = c(-1, -1, 1, 1, 0, 0.6), effect = c(0, 1, 2, 3, NA, 1)
  )
  mesh <- surface_mesh(d, "x", "y", "effect")
  expect_gt(nrow(mesh), 0L)
  expect_true(all(is.finite(mesh$.surface_z)))
  expect_false(any(mesh$.surface_x == 0 & mesh$.surface_y == 0))
  expect_plot_render(mesh_plot(d), cran = TRUE)
})

test_that("scattered surfaces cannot bridge separated support regions", {
  skip_if_not_installed("geometry")
  skip_if_not_installed("sf")
  left <- data.frame(x = c(0, 0.1, 0, 0.05), y = c(0, 0, 0.1, 0.03))
  right <- transform(left, x = x + 1)
  d <- rbind(left, right)
  d$effect <- d$x + d$y
  support <- surface_support(d, "x", "y", d, 0.16)
  mesh <- surface_mesh(d, "x", "y", "effect", support = support)
  expect_gt(nrow(mesh), 0L)
  faces <- split(mesh$.surface_x, mesh$.surface_face)
  expect_true(all(vapply(faces, function(x) diff(range(x)) < 0.2, logical(1))))
})

test_that("soap domains preserve holes and floor boundary loops", {
  skip_if_not_installed("sf")
  d <- mesh_data(7)
  outer <- data.frame(x = c(-1, -1, 1, 1, -1), y = c(-1, 1, 1, -1, -1), loop = 1)
  hole <- data.frame(
    x = c(-0.4, -0.4, 0.4, 0.4, -0.4),
    y = c(-0.4, 0.4, 0.4, -0.4, -0.4), loop = 2
  )
  boundary <- rbind(outer, hole)
  p <- mesh_plot(d, boundary = boundary, boundary_group = "loop")
  mesh <- p$layers[[1]]$data
  mesh <- mesh[mesh$.surface_type == "polygon", ]
  expect_gt(nrow(mesh), 0L)
  expect_false(any(abs(mesh$.surface_x) < 0.4 & abs(mesh$.surface_y) < 0.4))
  scene <- p$layers[[1]]$data
  edges <- scene[grepl("^boundary:", scene$.surface_face), ]
  expect_gt(nrow(edges), 0L)
  expect_true(all(edges$.surface_floor))
  expect_plot_render(p, cran = TRUE)
})

surface_model <- function() {
  withr::local_seed(714)
  d <- data.frame(
    x = runif(120), z = runif(120), w = runif(120), v = runif(120),
    f = factor(rep(c("a", "b"), 60))
  )
  d$y <- sin(4 * d$x) * cos(3 * d$z) + d$w + rnorm(120, sd = 0.2)
  d
}

test_that("all model and estimate entry points forward 3D surfaces", {
  d <- surface_model()
  m <- mgcv::gam(y ~ te(x, z, k = c(4, 4)) + s(w, k = 4) + x:z:f, data = d)
  for (p in list(
    assemble(m, surface = "3d", n_2d = 5, rug = FALSE),
    draw(m, surface = "3d", n_2d = 5, rug = FALSE, wrap = FALSE)
  )) {
    expect_s3_class(p[[1]]$coordinates, "Coord3D")
    expect_s3_class(p[[2]]$layers[[1]]$geom, "GeomRibbon")
    expect_s3_class(p[[3]]$coordinates, "Coord3D")
    expect_plot_render(p[[1]])
  }
  sm <- smooth_estimates(m, n_2d = 5, n = 8, dist = 0.1)
  expect_named(attr(sm, "surface_support"), smooths(m))
  selected <- sm[sm$.smooth == "te(x,z)", ]
  expect_named(attr(selected, "surface_support"), "te(x,z)")
  p <- draw(selected, surface = "3d")[[1]]
  expect_s3_class(p$coordinates, "Coord3D")
  pe <- parametric_effects(m, n_2d = 5, unnest = FALSE)
  p <- draw(pe, surface = "3d")[[1]]
  expect_s3_class(p$coordinates, "Coord3D")
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
})

test_that("tensor types and higher dimensional slices render in 3D", {
  d <- surface_model()
  for (term in c(
    "te(x,z,k=c(3,3))", "ti(x,z,k=c(3,3))", "t2(x,z,k=c(3,3))",
    "te(x,z,w,k=c(3,3,3))", "te(x,z,w,v,k=c(3,3,3,3))", "s(x,z,k=10)"
  )) {
    m <- mgcv::gam(stats::as.formula(paste("y ~", term)), data = d)
    p <- draw(m, surface = "3d", n_2d = 5, n_3d = 2, n_4d = 2, rug = FALSE)[[1]]
    expect_s3_class(p$coordinates, "Coord3D")
    expect_plot_render(p)
    if (grepl("x,z,w", term, fixed = TRUE)) {
      expect_equal(
        nrow(ggplot2::ggplot_build(p)$layout$layout),
        if (grepl("x,z,w,v", term, fixed = TRUE)) 4L else 2L
      )
    }
  }
})

test_that("3D height transformations, SEs and isotropic proportions are preserved", {
  d <- mesh_data()
  names(d)[3] <- ".estimate"
  d$.lower_ci <- d$.estimate - 0.4
  d$.upper_ci <- d$.estimate + 0.4
  d$.se <- 0.2
  d$.smooth <- "s(x,y)"
  d$.type <- "TPRS (2d)"
  d$.by <- NA_character_
  class(d) <- c("mgcv_smooth", class(d))
  d$y <- 2 * d$y
  p <- plot_smooth.isotropic_smooth(d,
    variables = c("x", "y"), surface = "3d",
    constant = 2, fun = exp, contour = FALSE
  )
  expect_equal(p$data$.estimate, exp(d$.estimate + 2))
  expect_equal(p$coordinates$ratio, c(0.5, 1, 1))
  p <- plot_smooth.bivariate_smooth(d,
    variables = c("x", "y"), surface = "3d",
    show = "se", constant = 2, fun = exp, ylim = c(-99, 99)
  )
  expect_equal(unique(p$layers[[1]]$data$.surface_z), 0.2)
  expect_equal(p$labels$z, "Std. err.")
  expect_equal(ggplot2::ggplot_build(p)$plot$scales$get_scales("z")$get_limits(), c(0.2, 0.2))
})

test_that("numeric interactions keep shared fixed ranges and transformed heights", {
  d <- surface_model()
  m <- mgcv::gam(y ~ x:z + x:w, data = d)
  pe <- parametric_effects(m, n_2d = 5, dist = 0)
  p <- draw(pe, surface = "3d", scales = "fixed", constant = 1, fun = exp)
  limits <- lapply(seq_along(p), function(i) ggplot2::ggplot_build(p[[i]])$plot$scales$get_scales("z")$get_limits())
  expect_equal(limits[[1]], limits[[2]])
  expect_gt(limits[[1]][1], 0)
  expect_equal(
    p[[1]]$data$.partial,
    exp(pe$.partial[pe$.term == unique(p[[1]]$data$.term)] + 1)
  )
})

test_that("representative 3D surfaces have visual coverage", {
  d <- mesh_data(8)
  expect_doppelganger(
    "3D diverging saddle with contours and floor observations",
    mesh_plot(d, rug = d[c(1, 8, 57, 64), ])
  )
  d$effect[d$x > -0.5 & d$x < 0.5 & d$y > -0.5 & d$y < 0.5] <- NA_real_
  expect_doppelganger("3D surface with masked hole", mesh_plot(d))
  f <- rbind(
    transform(mesh_data(6), slice = "a"),
    transform(mesh_data(6), slice = "b", effect = effect * 2)
  )
  expect_doppelganger(
    "3D conditioned surfaces share diverging palette",
    mesh_plot(f, facet = ggplot2::facet_wrap(~slice), facet_vars = "slice")
  )
})


test_that("named view lists are validated without changing themes or other panels", {
  d <- mesh_data()
  view <- list(yaw = 45, persp = FALSE)
  a <- mesh_plot(d, surface_3d = view)
  b <- mesh_plot(d, surface_3d = view)
  expect_equal(a$coordinates$yaw, 45)
  expect_false(a$coordinates$persp)
  expect_null(a$theme$plot.margin)
  expect_equal(a$theme, mesh_plot(d)$theme)
  expect_plot_render(a)
  expect_equal(b$coordinates$yaw, 45)
  expect_null(view$plot_bounds)
  a$coordinates$yaw <- -15
  expect_equal(b$coordinates$yaw, 45)
  expect_equal(view$yaw, 45)
  expect_error(mesh_plot(d, surface_3d = list(45)), "uniquely named")
  expect_error(mesh_plot(d, surface_3d = list(yaw = 45, yaw = 2)), "uniquely named")
  expect_error(mesh_plot(d, surface_3d = list(unknown = TRUE)), "Unknown")
  expect_error(mesh_plot(d, surface_3d = ggcube::coord_3d()), "uniquely named")
  d <- surface_model()
  m <- mgcv::gam(y ~ te(x, z, k = c(3, 3)), data = d)
  p <- draw(m, surface = "3d", surface_3d = view, n_2d = 5)[[1]]
  expect_equal(p$coordinates$yaw, 45)
  expect_error(draw(m, geom = "tile"), "replaced by 'surface'")
  expect_error(assemble(m, geom = "tile"), "replaced by 'surface'")
  expect_error(draw(smooth_estimates(m, n_2d = 5), geom = "tile"), "replaced by 'surface'")
})

test_that("soap model drawing and spherical fallback preserve their domains", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geometry")
  p <- draw(m_soap, surface = "3d", n_2d = 12, rug = FALSE)[[1]]
  expect_s3_class(p$coordinates, "Coord3D")
  expect_plot_render(p)
  sm <- smooth_estimates(m_soap, n_2d = 12, clip = FALSE)
  p <- draw(sm, surface = "3d")[[1]]
  expect_plot_render(p)
  sm <- smooth_estimates(m_sos, n_2d = 5)
  expect_message(
    p <- draw(sm, surface = "3d", contour = FALSE)[[1]],
    "Spherical smooths retain"
  )
  expect_s3_class(p$coordinates, "CoordSf")
})

test_that("scattered direct estimates and model assembly share support masking", {
  skip_if_not_installed("geometry")
  skip_if_not_installed("sf")
  d <- surface_model()
  m <- mgcv::gam(y ~ te(x, z, k = c(3, 3)), data = d)
  sm <- smooth_estimates(m, data = d, dist = 0.1)
  a <- assemble(m, data = d, surface = "3d", rug = FALSE, contour = FALSE)[[1]]
  b <- draw(sm, surface = "3d", contour = FALSE)[[1]]
  expect_equal(a$layers[[1]]$data$.surface_x, b$layers[[1]]$data$.surface_x)
  expect_equal(a$layers[[1]]$data$.surface_y, b$layers[[1]]$data$.surface_y)
  expect_equal(a$layers[[1]]$data$.surface_z, b$layers[[1]]$data$.surface_z)
  expect_plot_render(a)
  p <- draw(mgcv::bam(y ~ te(x, z, k = c(3, 3)), data = d), surface = "3d", n_2d = 5)[[1]]
  expect_s3_class(p$coordinates, "Coord3D")
})

test_that("fixed model ranges span smooth and parametric surfaces after transformation", {
  d <- surface_model()
  m <- mgcv::gam(y ~ te(x, z, k = c(3, 3)) + x:w, data = d)
  p <- assemble(m,
    surface = "3d", surface_3d = list(yaw = 20), scales = "fixed",
    constant = 1, fun = exp, n_2d = 5, rug = FALSE
  )
  limits <- lapply(p, function(x) ggplot2::ggplot_build(x)$plot$scales$get_scales("z")$get_limits())
  expect_equal(limits[[1]], limits[[2]])
  expect_gt(limits[[1]][1], 0)
  fill <- ggplot2::ggplot_build(p[[1]])$plot$scales$get_scales("fill")$get_limits()
  expect_equal(fill[1], -fill[2])
  expect_equal(p[[1]]$coordinates$yaw, 20)
  expect_equal(p[[2]]$coordinates$yaw, 20)
})


test_that("support metadata handles matrix-valued reference covariates", {
  d <- mesh_data()
  observed <- data.frame(x = I(matrix(1:6, 3)), y = I(matrix(7:12, 3)))
  support <- surface_support(d, "x", "y", observed, 0.1)
  expect_equal(support$observed, data.frame(x = 1:6, y = 7:12))
})


test_that("grid faces cannot cross known gaps between valid corners", {
  skip_if_not_installed("sf")
  d <- mesh_data(2)
  support <- surface_support(d, "x", "y", d, 0.4)
  expect_equal(nrow(surface_mesh(d, "x", "y", "effect")), 6L)
  expect_equal(nrow(surface_mesh(d, "x", "y", "effect", support = support)), 0L)
})


test_that("default back faces stay lit and explicit lights take precedence", {
  d <- mesh_data()
  a <- mesh_plot(d)
  b <- mesh_plot(d, surface_3d = list(yaw = 45))
  expect_equal(a$coordinates$light, ggcube::light(backface_scale = 1))
  expect_equal(b$coordinates$light$backface_scale, 1)
  a$coordinates$light$backface_scale <- -1
  expect_equal(b$coordinates$light$backface_scale, 1)
  custom <- ggcube::light(backface_scale = -1, contrast = 0.5)
  p <- mesh_plot(d, surface_3d = list(light = custom))
  expect_equal(p$coordinates$light, custom)
  expect_plot_render(p, cran = TRUE)
  p <- mesh_plot(d, surface_3d = list(light = ggcube::light(method = "none")))
  expect_equal(p$coordinates$light$shade, "neither")
  expect_plot_render(p)
  p <- mesh_plot(d, surface_3d = list(light = NULL))
  expect_null(p$coordinates$light)
  expect_plot_render(p)
})

test_that("coplanar contours are painted with their owning faces", {
  projected <- data.frame(
    group = c(1, 1, 2, 2, 2, 3, 4, 4, 4),
    .prim = c("segment", "segment", rep("polygon", 3), "point",
      rep("polygon", 3)),
    surface_owner = c("far", "far", rep("far", 3), NA, rep("near", 3))
  )
  ordered <- surface_scene_order(projected)
  expect_equal(ordered$group, c(2, 2, 2, 1, 1, 3, 4, 4, 4))
  expect_equal(ordered$.prim[6], "point")
})

test_that("opaque surfaces hide floor observations in the rendered scene", {
  d <- mesh_data(8)
  d$effect <- 0.5
  p <- mesh_plot(d, rug = d, contour = FALSE,
    surface_3d = list(roll = 0, persp = FALSE)) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank())
  # In a top-down view every floor observation lies behind the opaque plane.
  projection <- new.env(parent = emptyenv())
  transform <- p$coordinates$transform
  p$coordinates$transform <- function(data, panel_params) {
    projected <- transform(data, panel_params)
    if ("surface_type" %in% names(projected)) projection$data <- projected
    projected
  }
  expect_plot_render(p, cran = TRUE)
  projected <- projection$data
  expect_lt(max(which(projected$.prim == "point")),
    min(which(projected$.prim == "polygon")))
  scene <- p$layers[[1]]$data
  expect_true(all(scene$.surface_alpha[scene$.surface_type == "polygon"] == 1))
  expect_true(any(scene$.surface_type == "point"))
  expect_doppelganger("3D opaque surface hides floor observations", p)
})

test_that("original-scale 3D smooths retain support masking and nested metadata", {
  skip_if_not_installed("sf")
  d <- surface_model()
  d$x <- exp(d$x)
  m <- mgcv::gam(y ~ te(log(x), z, k = c(3, 3)), data = d)
  sm <- smooth_estimates(m, n_2d = 8, back_transform = TRUE, dist = 0.2,
    envir = environment())
  a <- draw(sm, surface = "3d", contour = FALSE)[[1]]
  b <- draw(sm, surface = "3d", back_transform = TRUE, contour = FALSE)[[1]]
  model <- assemble(m, surface = "3d", back_transform = TRUE,
    n_2d = 8, dist = 0.2, rug = FALSE, contour = FALSE,
    envir = environment())[[1]]
  mesh_a <- a$layers[[1]]$data
  mesh_b <- b$layers[[1]]$data
  expect_gt(nrow(mesh_b), 0L)
  expect_equal(mesh_b$.surface_x, exp(mesh_a$.surface_x))
  expect_equal(mesh_b$.surface_y, mesh_a$.surface_y)
  expect_equal(mesh_b$.surface_z, mesh_a$.surface_z)
  expect_equal(model$layers[[1]]$data$.surface_z, mesh_b$.surface_z)
  nested <- smooth_estimates(m, n_2d = 8, back_transform = TRUE,
    dist = 0.2, unnest = FALSE, envir = environment())
  p <- draw(nested, surface = "3d", back_transform = TRUE,
    contour = FALSE)[[1]]
  expect_equal(p$layers[[1]]$data$.surface_z, mesh_b$.surface_z)
  expect_plot_render(b)
})
