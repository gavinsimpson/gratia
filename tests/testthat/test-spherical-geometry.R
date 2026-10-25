# The full suite checks combinations across models, families and plot options.
# Small function checks remain in test-function-core.R and focused test files.
skip_on_cran()

test_that("automatic SOS evaluation grids keep named observed ranges", {
  d <- data.frame(north = c(-89.9, 89.9), east = c(-179.9, 179.9))
  g <- sos_data(d, n = 5, vars = c("north", "east"))
  expect_equal(g$north, seq(-89.9, 89.9, length.out = 5))
  expect_equal(g$east, seq(-179.9, 179.9, length.out = 5))
  sm <- smooth_data(m_sos_named, id = 1, n_2d = 6)
  expect_equal(nrow(sm), 36L)
  expect_equal(range(sm$north), range(sos_named_df$north))
  expect_equal(range(sm$east), range(sos_named_df$east))
})

test_that("spherical cells cover poles and split longitude seams", {
  skip_if_not_installed("sf", "1.0-0")
  d <- spherical_test_grid()
  cells <- sos_cells(d, "east", "north")
  expect_equal(unname(sf::st_bbox(cells)), c(-180, -90, 180, 90),
    ignore_attr = TRUE)
  expect_valid_spherical_geometry(cells)
  for (i in seq_len(nrow(cells))) {
    xy <- sf::st_coordinates(cells[i, ])
    expect_lte(diff(range(xy[, 1L])), 90 + 1e-10)
    expect_true(all(abs(xy[, 2L]) <= 90))
  }
  expect_equal(cells$.estimate, d$.estimate[match(paste(cells$north, cells$east),
    paste(d$north, d$east))])
  seam <- sos_cells(spherical_test_grid(c(-10, 10), c(175, 185)), "east", "north")
  expect_equal(nrow(seam), 4L)
  expect_valid_spherical_geometry(seam)
  expect_equal(range(sf::st_coordinates(seam)[, 1L]), c(-180, 180))
})

test_that("irregular spherical cells use midpoint boundaries and stable row values", {
  skip_if_not_installed("sf", "1.0-0")
  d <- spherical_test_grid(c(-50, -10, 20), c(10, 15, 40))
  d <- d[c(9, 1, 5, 2, 6, 3, 7, 4, 8), ]
  cells <- sos_cells(d, "east", "north")
  expect_equal(cells$.estimate, d$.estimate)
  expect_equal(sos_axis_edges(c(10, 15, 40)), c(7.5, 12.5, 27.5, 52.5))
  expect_equal(sos_axis_edges(c(-89, 89), latitude = TRUE), c(-90, 0, 90))
  expect_valid_spherical_geometry(cells)
  # Very coarse global grids must not produce hemispherical/degenerate loops.
  coarse <- sos_cells(spherical_test_grid(c(-90, 90), c(-180, 180)), "east", "north")
  expect_valid_spherical_geometry(coarse)
})

test_that("orthographic clipping keeps partially visible cells and restores sf state", {
  skip_if_not_installed("sf", "1.0-0")
  d <- spherical_test_grid(c(-10, 10), c(70, 100, 130))
  cells <- sos_cells(d, "east", "north")
  crs <- "+proj=ortho +lat_0=0 +lon_0=0 +R=6371000"
  p <- sos_project(cells, crs)
  expect_true(any(p$east == 100)) # Its centre is hidden, but its west edge is visible.
  expect_false(any(p$east == 130))
  expect_valid_spherical_geometry(p)
  expect_true(all(p$.estimate %in% d$.estimate))
  old <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))
  expect_silent(q <- sos_project(cells, sf::st_crs(crs)$wkt))
  expect_false(sf::sf_use_s2())
  expect_equal(sf::st_as_binary(sf::st_geometry(p)), sf::st_as_binary(sf::st_geometry(q)))
})

test_that("global orthographic geometry covers the projected disc at different views", {
  skip_if_not_installed("sf", "1.0-0")
  cells <- sos_cells(spherical_test_grid(), "east", "north")
  for (view in c("+lat_0=0 +lon_0=0", "+lat_0=20 +lon_0=170", "+lat_0=-90 +lon_0=0")) {
    p <- sos_project(cells, paste("+proj=ortho +R=6371000", view))
    expect_valid_spherical_geometry(p)
    fraction <- sum(as.numeric(sf::st_area(p))) / (pi * 6371000^2)
    expect_gt(fraction, 0.999)
    expect_lt(fraction, 1.001)
  }
  p <- sos_project(cells, "+proj=wintri")
  expect_valid_spherical_geometry(p)
  expect_equal(sort(unique(p$.estimate)), sort(unique(cells$.estimate)))
})

test_that("supplied SOS coordinates are unchanged and small/scattered grids draw as points", {
  skip_if_not_installed("sf", "1.0-0")
  for (d in list(spherical_test_grid(10, c(20, 30)), spherical_test_grid()[c(1, 15, 30), ])) {
    expect_message(p <- spherical_plot(d, contour = FALSE, crs = 4326), "as points")
    expect_equal(p$data, d)
    expect_true(all(sf::st_geometry_type(p$layers[[1L]]$data) == "POINT"))
    expect_silent(ggplot2::ggplot_build(p))
  }
  d <- spherical_test_grid(c(-40, -10, 30), c(10, 20, 60))
  p <- spherical_plot(d, contour = FALSE, crs = 4326)
  expect_equal(p$data, d)
  expect_valid_spherical_geometry(p$layers[[1L]]$data)
  same <- rbind(d, d[1L, ])
  expect_equal(nrow(sos_plot_data(same, "east", "north")$data), nrow(d))
  same$.estimate[nrow(same)] <- 100
  expect_error(spherical_plot(same), "conflicting")
  d$north[1L] <- 100
  expect_error(spherical_plot(d), "latitude.*longitude")
})

test_that("SOS plots preserve transformations, scales, contours, rugs and CRS semantics", {
  skip_if_not_installed("sf", "1.0-0")
  d <- spherical_test_grid()
  rug <- as.data.frame(d[c(1, 20, 50), c("north", "east")])
  p <- spherical_plot(d, constant = 1, fun = exp, rug = rug,
    crs = "+proj=ortho +lat_0=20 +lon_0=170", default_crs = 3857,
    xlab = "Longitude", ylab = "Latitude", contour = TRUE)
  expect_equal(p$data$.estimate, exp(d$.estimate + 1))
  expect_equal(p$labels$x, "Longitude")
  expect_equal(p$labels$y, "Latitude")
  expect_equal(p$coordinates$default_crs, 3857)
  expect_s3_class(p$layers[[1L]]$geom, "GeomSf")
  expect_true(any(vapply(p$layers, function(z) inherits(z$data, "sf") &&
    any(sf::st_geometry_type(z$data) %in% c("LINESTRING", "MULTILINESTRING")), logical(1))))
  expect_silent(ggplot2::ggplot_build(p))
  expect_doppelganger("spherical dateline and horizon geometry", p)
  q <- spherical_plot(d, show = "se", contour = FALSE, crs = "+proj=ortho +lat_0=90 +lon_0=0")
  expect_equal(unique(q$layers[[1L]]$data$.se), 0.2)
  expect_doppelganger("spherical polar geometry", q)
})

test_that("named SOS models preserve direct numerical estimates and render supplied data", {
  skip_if_not_installed("sf", "1.0-0")
  d <- spherical_test_grid(c(-80, 0, 80), c(-175, 0, 175))
  nd <- as.data.frame(d[c("north", "east")])
  sm <- smooth_estimates(m_sos_named, data = nd, dist = 0, overall_uncertainty = FALSE)
  smooth <- m_sos_named$smooth[[1L]]
  X <- mgcv::PredictMat(smooth, nd)
  j <- seq.int(smooth$first.para, smooth$last.para)
  expect_equal(sm$.estimate, drop(X %*% coef(m_sos_named)[j]))
  expect_equal(sm$.se, sqrt(rowSums((X %*% vcov(m_sos_named)[j, j]) * X)))
  expect_equal(sm$north, nd$north)
  expect_equal(sm$east, nd$east)
  p <- draw(sm, contour = FALSE)[[1L]]
  expect_equal(p$labels$x, "east")
  expect_equal(p$labels$y, "north")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("fully hidden spherical cells, contours and rugs yield empty valid layers", {
  skip_if_not_installed("sf", "1.0-0")
  d <- spherical_test_grid(c(-10, 10), c(150, 170))
  crs <- "+proj=ortho +lat_0=0 +lon_0=0"
  cells <- sos_project(sos_cells(d, "east", "north"), crs)
  expect_equal(nrow(cells), 0L)
  paths <- sos_contours(d, "east", "north", ".estimate")
  expect_equal(nrow(sos_project(paths, crs)), 0L)
  points <- sos_project(sos_points(d, "east", "north"), crs)
  expect_equal(nrow(points), 0L)
  expect_equal(sf::st_crs(cells), sf::st_crs(crs))
  expect_silent(p <- spherical_plot(d, rug = d, crs = crs))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("spherical observation rugs validate columns and omit incomplete rows", {
  skip_if_not_installed("sf", "1.0-0")
  rug <- data.frame(north = c(0, NA, 20), east = c(0, 10, Inf))
  expect_equal(nrow(sos_points(rug, "east", "north")), 1L)
  expect_error(sos_points(rug["north"], "east", "north"), "Missing.*east")
  expect_error(sos_points(data.frame(north = 100, east = 0), "east", "north"), "latitudes")
  expect_error(sos_points(data.frame(north = "north", east = 0), "east", "north"), "numeric")
  expect_silent(p <- spherical_plot(rug = rug, contour = FALSE))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("SOS factor-by panels keep distinct estimates, contours and observation rugs", {
  skip_if_not_installed("sf", "1.0-0")
  separate <- assemble(m_sos_by, grouped = c("sz", "fs"), parametric = FALSE,
    n_2d = 5, contour = TRUE, crs = 4326)
  expect_length(separate, 2L)
  for (p in separate) {
    expect_equal(length(unique(p$data$region)), 1L)
    expect_silent(ggplot2::ggplot_build(p))
  }
  grouped <- assemble(m_sos_by, grouped = c("factor-by", "sz", "fs"), parametric = FALSE,
    n_2d = 5, contour = TRUE, crs = 4326)
  expect_length(grouped, 1L)
  p <- grouped[[1L]]
  built <- ggplot2::ggplot_build(p)
  expect_equal(as.character(built$layout$layout$region), c("a", "b"))
  expect_equal(nrow(p$data), 50L)
  layers <- p$layers[vapply(p$layers, function(x) inherits(x$data, "sf"), logical(1))]
  polygon <- layers[vapply(layers, function(x) all(sf::st_geometry_type(x$data) %in%
    c("POLYGON", "MULTIPOLYGON")), logical(1))]
  expect_length(polygon, 2L)
  for (layer in polygon) {
    expect_equal(length(unique(layer$data$region)), 1L)
    source <- p$data[p$data$region == layer$data$region[[1L]], ]
    expect_equal(layer$data$.estimate, source$.estimate[match(
      paste(layer$data$north, layer$data$east), paste(source$north, source$east))])
  }
  contours <- layers[vapply(layers, function(x) any(sf::st_geometry_type(x$data) %in%
    c("LINESTRING", "MULTILINESTRING")), logical(1))]
  expect_length(contours, 2L)
  expect_true(all(vapply(contours, function(x) length(unique(x$data$region)) == 1L, logical(1))))
  rugs <- layers[vapply(layers, function(x) all(sf::st_geometry_type(x$data) == "POINT"), logical(1))]
  expect_length(rugs, 1L)
  expect_equal(table(rugs[[1L]]$data$region), table(sos_by_df$region))
  sf_layers <- which(vapply(p$layers, function(x) inherits(x$data, "sf"), logical(1)))
  for (i in sf_layers) {
    source_level <- as.character(p$layers[[i]]$data$region)
    expected_panel <- match(source_level, as.character(built$layout$layout$region))
    expect_equal(as.integer(built$data[[i]]$PANEL), expected_panel)
  }

  sm <- smooth_estimates(m_sos_by, n_2d = 5)
  expect_silent(q <- draw(sm, grouped = c("factor-by", "sz", "fs"), crs = 4326, contour = FALSE)[[1L]])
  expect_equal(as.character(ggplot2::ggplot_build(q)$layout$layout$region), c("a", "b"))
  expect_silent(draw(m_sos_by, grouped = c("sz", "fs"), parametric = FALSE,
    n_2d = 5, crs = 4326, contour = FALSE))
})


test_that("spherical rugs have their own theme and hidden bounds stay invisible", {
  skip_if_not_installed("sf")
  p <- spherical_plot(contour = FALSE, rug = spherical_test_grid(), crs = 4326) +
    ggplot2::theme(geom.gratia.surface = ggplot2::element_geom(colour = "red"),
      geom.gratia.surface.rug = ggplot2::element_geom(colour = "blue"),
      gratia.alpha.surface.rug = .6)
  b <- ggplot2::ggplot_build(p)$data
  expect_equal(unique(b[[1]]$colour), "red")
  expect_equal(unique(b[[length(b)]]$colour), "blue")
  expect_equal(unique(b[[length(b)]]$alpha), .6)
  d <- spherical_test_grid(c(-10, 10), c(150, 170))
  p <- spherical_plot(d, contour = FALSE, crs = "+proj=ortho +lat_0=0 +lon_0=0") +
    ggplot2::theme(geom.gratia.surface = ggplot2::element_geom(colour = "red", fill = "red"))
  b <- ggplot2::ggplot_build(p)$data
  nonempty <- b[vapply(b, function(x) nrow(x) > 0L && "geometry" %in% names(x), logical(1))]
  expect_true(length(nonempty) > 0L)
  expect_true(all(vapply(nonempty, function(x) {
    all(is.na(x$colour)) && all(is.na(x$fill))
  }, logical(1))))
  expect_silent(ggplot2::ggplotGrob(p))
})
