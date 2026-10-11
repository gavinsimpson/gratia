# Orthographic's spherical and ellipsoidal forms have the same visibility
# condition: the dot product of geodetic unit normals is non-negative.
# PROJ: src/projections/ortho.cpp, ortho_s_forward / ortho_e_forward.
sos_ortho_view <- function(crs) {
  proj <- sf::st_crs(crs)$proj4string
  if (is.na(proj) || !grepl("(^| )\\+proj=ortho( |$)", proj)) return(NULL)
  fields <- strsplit(proj, " +")[[1L]]
  angle <- function(name) {
    value <- fields[startsWith(fields, paste0("+", name, "="))]
    if (!length(value)) return(0)
    value <- sub("^[^=]+=", "", value[[1L]])
    if (endsWith(value, "r")) {
      as.numeric(sub("r$", "", value))
    } else as.numeric(value) * pi / 180
  }
  c(lon = angle("lon_0"), lat = angle("lat_0"))
}

sos_horizon <- function(view) {
  normal <- c(cos(view[[2L]]) * cos(view[[1L]]),
    cos(view[[2L]]) * sin(view[[1L]]), sin(view[[2L]]))
  east <- c(-sin(view[[1L]]), cos(view[[1L]]), 0)
  north <- c(-sin(view[[2L]]) * cos(view[[1L]]),
    -sin(view[[2L]]) * sin(view[[1L]]), cos(view[[2L]]))
  theta <- seq(0, 2 * pi, length.out = 721L)
  # About 0.6 m inside the horizon: prevents rounding across PROJ's boundary,
  # without the visible shrinkage of a fixed-distance spherical buffer.
  inset <- 1e-7
  xyz <- sqrt(1 - inset^2) * (outer(cos(theta), east) +
    outer(sin(theta), north)) + rep(inset * normal, each = length(theta))
  xy <- cbind(atan2(xyz[, 2L], xyz[, 1L]),
    asin(pmax(-1, pmin(1, xyz[, 3L])))) * 180 / pi
  xy[nrow(xy), ] <- xy[1L, ]
  sf::st_sfc(sf::st_polygon(list(xy)), crs = "OGC:CRS84")
}

sos_axis_edges <- function(x, latitude = FALSE) {
  x <- sort(unique(x))
  middle <- x[-length(x)] + diff(x) / 2
  edge <- c(x[[1L]] - diff(x)[[1L]] / 2, middle,
    utils::tail(x, 1L) + utils::tail(diff(x), 1L) / 2)
  if (latitude) {
    edge <- pmax(-90, pmin(90, edge))
  } else {
    # A global min/max grid covers at most one turn, rather than overlapping
    # cells created by extrapolating its first and last half-widths.
    if (diff(range(edge)) > 360) {
      centre <- mean(range(x))
      edge <- pmax(centre - 180, pmin(centre + 180, edge))
    }
    if (diff(range(x)) >= 180 && min(x) >= -180 && max(x) <= 180) {
      edge <- pmax(-180, pmin(180, edge))
    }
  }
  edge
}

sos_polygon <- function(xmin, xmax, ymin, ymax) {
  along <- function(a, b) seq(a, b, length.out = max(2L, ceiling(abs(b - a) / 2) + 1L))
  x <- along(xmin, xmax)
  y <- along(ymin, ymax)
  xy <- rbind(cbind(x, ymin), cbind(xmax, y[-1L]),
    cbind(rev(x)[-1L], ymax), cbind(xmin, rev(y)[-1L]))
  # All longitudes at a pole describe the same point. Keep a single vertex.
  xyz <- cbind(cos(xy[, 2L] * pi / 180) * cos(xy[, 1L] * pi / 180),
    cos(xy[, 2L] * pi / 180) * sin(xy[, 1L] * pi / 180),
    sin(xy[, 2L] * pi / 180))
  same <- rowSums(abs(xyz[-1L, , drop = FALSE] -
    xyz[-nrow(xyz), , drop = FALSE])) < 1e-12
  xy <- xy[c(TRUE, !same), , drop = FALSE]
  if (!isTRUE(all.equal(xy[1L, ], xy[nrow(xy), ]))) xy <- rbind(xy, xy[1L, ])
  sf::st_polygon(list(xy))
}

sos_cells <- function(object, longitude, latitude) {
  x <- sort(unique(object[[longitude]]))
  y <- sort(unique(object[[latitude]]))
  xe <- sos_axis_edges(x)
  ye <- sos_axis_edges(y, latitude = TRUE)
  polygons <- list()
  ids <- list()
  for (i in seq_len(nrow(object))) {
    ix <- match(object[[longitude]][[i]], x)
    iy <- match(object[[latitude]][[i]], y)
    lo <- xe[ix]
    hi <- xe[ix + 1L]
    # Break at the antimeridian and at quarter turns. No spherical polygon is
    # as large as a hemisphere, including grids with only two coordinates.
    cuts <- seq(floor(lo / 90) * 90, ceiling(hi / 90) * 90, by = 90)
    cuts <- sort(unique(c(lo, cuts[cuts > lo + 1e-10 & cuts < hi - 1e-10], hi)))
    for (j in seq_len(length(cuts) - 1L)) {
      a <- cuts[[j]]
      b <- cuts[[j + 1L]]
      shift <- 360 * floor(((a + b) / 2 + 180) / 360)
      polygons[[length(polygons) + 1L]] <- sos_polygon(a - shift, b - shift,
        ye[iy], ye[iy + 1L])
      ids[[length(polygons)]] <- i
    }
  }
  sf::st_sf(as.data.frame(object)[unlist(ids), , drop = FALSE],
    geometry = sf::st_sfc(polygons, crs = "OGC:CRS84"))
}

sos_project <- function(object, crs) {
  view <- sos_ortho_view(crs)
  if (!is.null(view)) {
    old <- sf::sf_use_s2()
    if (!old) {
      suppressMessages(sf::sf_use_s2(TRUE))
      on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
    }
    geometry_type <- sf::st_geometry_type(object)
    polygon <- all(geometry_type %in% c("POLYGON", "MULTIPOLYGON"))
    line <- all(geometry_type %in% c("LINESTRING", "MULTILINESTRING"))
    object <- sf::st_set_agr(object, "constant")
    object <- sf::st_intersection(object, sos_horizon(view))
    # Tangential contacts can include lower-dimensional pieces. These are not
    # surface cells or contour paths, and must not become polygon rings.
    if (nrow(object) && any(sf::st_geometry_type(object) == "GEOMETRYCOLLECTION")) {
      object <- sf::st_collection_extract(object, if (polygon) "POLYGON" else if (line) "LINESTRING" else "POINT")
      object <- object[!sf::st_is_empty(object), , drop = FALSE]
    }
  }
  object <- sf::st_transform(object, crs)
  if (!nrow(object)) return(object)
  if (any(sf::st_is_empty(object)) || any(!is.finite(unlist(sf::st_geometry(object))))) {
    cli::cli_abort("The supplied CRS cannot project all of this spherical surface. Choose a global or orthographic CRS.")
  }
  if (any(sf::st_geometry_type(object) %in% c("POLYGON", "MULTIPOLYGON"))) {
    invalid <- !sf::st_is_valid(object)
    if (any(invalid)) {
      # Dense geographic edges can cross after projection by roundoff near
      # the limb. Repair only those planar rings, without new dependencies.
      sf::st_geometry(object)[invalid] <- sf::st_buffer(sf::st_geometry(object)[invalid], 0)
      sf::st_geometry(object) <- sf::st_cast(sf::st_geometry(object), "MULTIPOLYGON")
    }
  }
  if (any(sf::st_is_empty(object)) ||
      any(!is.finite(unlist(sf::st_geometry(object)))) ||
      any(!sf::st_is_valid(object))) {
    cli::cli_abort("The supplied CRS produced invalid spherical geometry. Choose a global or orthographic CRS.")
  }
  object
}

sos_plot_data <- function(object, longitude, latitude) {
  xy <- object[c(longitude, latitude)]
  if (any(!vapply(xy, is.numeric, logical(1))) || any(!is.finite(as.matrix(xy)))) {
    cli::cli_abort("Spherical smooth coordinates must be finite numeric latitude and longitude values.")
  }
  if (any(abs(object[[latitude]]) > 90)) {
    cli::cli_abort(c("Spherical smooth latitudes must be between -90 and 90 degrees.",
      "i" = "Specify the smooth as `s(latitude, longitude, bs = \"sos\")`."))
  }
  duplicate <- duplicated(xy) | duplicated(xy, fromLast = TRUE)
  if (any(duplicate)) {
    values <- intersect(c(".estimate", ".se", ".lower_ci", ".upper_ci"), names(object))
    if (nrow(unique(as.data.frame(object)[duplicate, c(longitude, latitude, values), drop = FALSE])) >
        nrow(unique(as.data.frame(xy)[duplicate, , drop = FALSE]))) {
      cli::cli_abort("Duplicate spherical coordinates have conflicting estimates or standard errors.")
    }
    object <- object[!duplicated(xy), , drop = FALSE]
  }
  n_x <- length(unique(object[[longitude]]))
  n_y <- length(unique(object[[latitude]]))
  rectangular <- n_x > 1L && n_y > 1L && nrow(object) == as.double(n_x) * n_y
  list(data = object, rectangular = rectangular)
}

sos_points <- function(object, longitude, latitude) {
  missing <- setdiff(c(longitude, latitude), names(object))
  if (length(missing)) cli::cli_abort("Missing spherical point coordinate columns: {.val {missing}}.")
  xy <- object[c(longitude, latitude)]
  if (any(!vapply(xy, is.numeric, logical(1)))) {
    cli::cli_abort("Spherical point coordinates must be numeric.")
  }
  # Observation rugs can include incomplete rows recovered from original data.
  keep <- is.finite(object[[longitude]]) & is.finite(object[[latitude]])
  object <- object[keep, , drop = FALSE]
  if (any(abs(object[[latitude]]) > 90)) {
    cli::cli_abort("Spherical point latitudes must be between -90 and 90 degrees.")
  }
  sf::st_as_sf(as.data.frame(object), coords = c(longitude, latitude),
    crs = "OGC:CRS84", remove = FALSE)
}

sos_contours <- function(object, longitude, latitude, value, n = NULL) {
  x <- sort(unique(object[[longitude]]))
  y <- sort(unique(object[[latitude]]))
  z <- matrix(NA_real_, nrow = length(x), ncol = length(y))
  z[cbind(match(object[[longitude]], x), match(object[[latitude]], y))] <- object[[value]]
  if (!any(is.finite(z)) || diff(range(z, na.rm = TRUE)) == 0) return(NULL)
  paths <- grDevices::contourLines(x, y, z, nlevels = if (is.null(n)) 10 else n)
  if (!length(paths)) return(NULL)
  lines <- lapply(paths, function(p) {
    xy <- cbind(p$x, p$y)
    # Contour interpolation is in angular grid coordinates, not a shortcut
    # along a great circle across the date line on very coarse grids.
    dense <- lapply(seq_len(nrow(xy) - 1L), function(i) {
      a <- xy[i, ]
      b <- xy[i + 1L, ]
      n <- max(1L, ceiling(max(abs(b - a)) / 2))
      outer(seq(0, 1, length.out = n + 1L)[-1L], b - a) +
        rep(a, each = n)
    })
    sf::st_linestring(rbind(xy[1L, ], do.call(rbind, dense)))
  })
  sf::st_sf(level = vapply(paths, `[[`, numeric(1L), "level"),
    geometry = sf::st_wrap_dateline(sf::st_sfc(lines, crs = "OGC:CRS84")))
}
