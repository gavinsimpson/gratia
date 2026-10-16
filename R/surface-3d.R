# Helpers for the optional ggcube renderer. Keep all mesh coordinates in the
# displayed covariate space: predictions and effects are transformed upstream.
validate_surface_3d <- function(view) {
  named <- !length(view) || (
    !is.null(names(view)) && !anyNA(names(view)) &&
      all(nzchar(names(view))) && !anyDuplicated(names(view))
  )
  if (!is.list(view) || is.data.frame(view) || !named) {
    stop("'surface_3d' must be a list of uniquely named coord_3d() arguments.",
      call. = FALSE
    )
  }
  allowed <- c(
    "pitch", "roll", "yaw", "persp", "dist", "expand", "clip",
    "panels", "xlabels", "ylabels", "zlabels", "title_position",
    "rotate_labels", "scales", "scale_depth", "ratio", "zoom", "light"
  )
  unknown <- setdiff(names(view), allowed)
  if (length(unknown)) {
    stop("Unknown 'surface_3d' option(s): ",
      paste(unknown, collapse = ", "), ". See ggcube::coord_3d().",
      call. = FALSE
    )
  }
  invisible(NULL)
}

validate_ci_3d <- function(ci_3d, ci_3d_alpha, ci_3d_col) {
  if (!is.logical(ci_3d) || length(ci_3d) != 1L || is.na(ci_3d)) {
    stop("'ci_3d' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(ci_3d_alpha) || length(ci_3d_alpha) != 1L ||
      !is.finite(ci_3d_alpha) || ci_3d_alpha < 0 || ci_3d_alpha > 1) {
    stop("'ci_3d_alpha' must be a finite number between 0 and 1.", call. = FALSE)
  }
  valid_colour <- is.character(ci_3d_col) && length(ci_3d_col) == 1L &&
    !is.na(ci_3d_col) && tryCatch({
      grDevices::col2rgb(ci_3d_col)
      TRUE
    }, error = function(e) FALSE)
  if (!valid_colour) {
    stop("'ci_3d_col' must be a single valid R colour.", call. = FALSE)
  }
  invisible(NULL)
}

check_surface_dots <- function(call) {
  if ("geom" %in% names(call[["..."]])) {
    stop("'geom' has been replaced by 'surface'; ",
      "use surface = 'raster', 'tile', or '3d'.",
      call. = FALSE
    )
  }
}

surface_dependency <- function(package, reason) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(reason, " requires the optional package '", package,
      "'. Install it with install.packages('", package, "').",
      call. = FALSE
    )
  }
}

surface_view <- function(view, object, x_var, y_var, isotropic) {
  if (!"light" %in% names(view)) {
    view$light <- ggcube::light(backface_scale = 1)
  }
  if (isTRUE(isotropic) && is.null(view$ratio) &&
        (is.null(view$scales) || view$scales == "free")) {
    spans <- vapply(object[c(x_var, y_var)], function(x) {
      x <- x[is.finite(x)]
      if (length(x)) diff(range(x)) else 1
    }, numeric(1))
    if (all(spans > 0)) view$ratio <- c(unname(spans / max(spans)), 1)
  }
  # coord_3d() returns a coordinate AND a theme. Take only its fresh coordinate
  # so gratia keeps the user's theme, margins and facet styling intact.
  components <- do.call(ggcube::coord_3d, view)
  components[[which(vapply(components, inherits, logical(1), "Coord3D"))]]
}

# Preserve the reference and normalization used by exclude.too.far(), even
# when a caller asks for no rug or draws an estimates object later.
smooth_surface_support <- function(model, smooth, estimates, dist) {
  if (is.null(dist) || dist <= 0 || smooth_dim(smooth) != 2L ||
        is_soap_film(smooth)) {
    return(NULL)
  }
  variables <- smooth_variable(smooth)
  if (length(variables) < 2L) {
    return(NULL)
  }
  variables <- variables[1:2]
  reference <- model$model[variables]
  data <- if ("data" %in% names(estimates)) estimates$data[[1L]] else estimates
  if (!all(vapply(reference, is.numeric, logical(1))) ||
        !all(variables %in% names(data))) {
    return(NULL)
  }
  surface_support(data, variables[1], variables[2], reference, dist)
}

surface_support <- function(data, x_var, y_var, observed, dist) {
  if (is.null(observed) || is.null(dist) || dist <= 0) {
    return(NULL)
  }
  ranges <- lapply(data[c(x_var, y_var)], function(x) range(x[is.finite(x)]))
  if (any(!vapply(
    ranges, function(x) all(is.finite(x)) && diff(x) > 0,
    logical(1)
  ))) {
    return(NULL)
  }
  # Matrix-valued covariates (e.g. distributed lags) use their aligned entries,
  # as exclude.too.far() does when forming its reference locations.
  reference <- data.frame(
    x = as.vector(observed[[x_var]]), y = as.vector(observed[[y_var]])
  )
  reference <- reference[is.finite(reference$x) & is.finite(reference$y), ]
  list(observed = unique(reference), ranges = unname(ranges), dist = dist)
}

# Closed polygon loops use even/odd membership, matching soap-film domains.
surface_domain <- function(boundary, x_var, y_var, group) {
  if (is.null(boundary) || !nrow(boundary)) {
    return(NULL)
  }
  surface_dependency("sf", "3D soap-film domain checks")
  loops <- split(boundary, boundary[[group]])
  polygons <- lapply(loops, function(d) {
    xy <- as.matrix(d[c(x_var, y_var)])
    if (!identical(xy[1, ], xy[nrow(xy), ])) xy <- rbind(xy, xy[1, ])
    sf::st_polygon(list(unname(xy)))
  })
  domain <- sf::st_sfc(polygons[[1L]])
  if (length(polygons) > 1L) {
    for (i in seq.int(2L, length(polygons))) {
      domain <- sf::st_sym_difference(domain, sf::st_sfc(polygons[[i]]))
    }
  }
  sf::st_make_valid(domain)
}

surface_faces_inside <- function(vertices, faces, support = NULL,
                                 domain = NULL) {
  if (is.null(support) && is.null(domain)) {
    return(rep(TRUE, nrow(faces)))
  }
  surface_dependency("sf", "3D surface support and domain checks")
  xy <- vertices[, 1:2, drop = FALSE]
  if (!is.null(support)) {
    for (i in 1:2) {
      xy[, i] <-
        (xy[, i] - support$ranges[[i]][1]) / diff(support$ranges[[i]])
    }
    ref <- as.matrix(support$observed)
    for (i in 1:2) {
      ref[, i] <-
        (ref[, i] - support$ranges[[i]][1]) / diff(support$ranges[[i]])
    }
    points <- sf::st_sfc(lapply(seq_len(nrow(ref)), function(i) {
      sf::st_point(ref[i, ])
    }))
    # GEOS approximates circles with inscribed polygons: acceptance is
    # conservative, so a retained face cannot bridge an excluded support gap.
    region <- sf::st_union(sf::st_buffer(points, support$dist, nQuadSegs = 30))
  } else {
    region <- domain
  }
  polygons <- sf::st_sfc(lapply(seq_len(nrow(faces)), function(i) {
    coords <- xy[faces[i, ], , drop = FALSE]
    sf::st_polygon(list(rbind(coords, coords[1, ])))
  }))
  keep <- lengths(sf::st_covered_by(polygons, region)) > 0L
  if (!is.null(support) && !is.null(domain)) {
    keep <- keep & surface_faces_inside(vertices, faces, domain = domain)
  }
  keep
}

# Return triangles, retaining masked vertices until AFTER topology is built.
surface_mesh <- function(data, x_var, y_var, z_var, support = NULL,
                         domain = NULL, height_vars = z_var) {
  empty <- data[FALSE, , drop = FALSE]
  empty$.surface_x <- empty$.surface_y <- empty$.surface_z <- numeric()
  empty$.surface_fill <- numeric()
  empty$.surface_face <- character()
  if (!nrow(data)) {
    return(empty)
  }
  xy_ok <- is.finite(data[[x_var]]) & is.finite(data[[y_var]])
  data <- data[xy_ok, , drop = FALSE]
  xyz <- data.frame(x = data[[x_var]], y = data[[y_var]], z = data[[z_var]])
  key <- paste(match(xyz$x, unique(xyz$x)), match(xyz$y, unique(xyz$y)),
    sep = ":"
  )
  repeated <- duplicated(key)
  if (any(repeated)) {
    first <- match(key, key)
    same <- vapply(height_vars, function(v) {
      z <- data[[v]]
      all((is.na(z) & is.na(z[first])) |
        (!is.na(z) & !is.na(z[first]) & z == z[first]))
    }, logical(1))
    if (!all(same)) {
      stop("Conflicting heights at the same 3D surface location.",
        call. = FALSE
      )
    }
    data <- data[!repeated, , drop = FALSE]
    xyz <- xyz[!repeated, , drop = FALSE]
  }
  if (!any(is.finite(xyz$z))) {
    return(empty)
  }
  if (nrow(xyz) < 3L || length(unique(xyz$x)) < 2L ||
        length(unique(xyz$y)) < 2L ||
        qr(cbind(1, as.matrix(xyz[1:2])))$rank < 3L) {
    stop("A 3D surface needs at least three distinct, non-collinear locations.",
      call. = FALSE
    )
  }
  xs <- sort(unique(xyz$x))
  ys <- sort(unique(xyz$y))
  regular <- nrow(xyz) == length(xs) * length(ys)
  if (regular) {
    index <- integer(nrow(xyz))
    offset <- match(xyz$x, xs) + (match(xyz$y, ys) - 1L) * length(xs)
    index[offset] <- seq_len(nrow(xyz))
    cells <- expand.grid(
      x = seq_len(length(xs) - 1L), y = seq_len(length(ys) - 1L)
    )
    a <- cells$x + (cells$y - 1L) * length(xs)
    corners <- cbind(
      index[a], index[a + 1L], index[a + 1L + length(xs)],
      index[a + length(xs)]
    )
    complete <- apply(corners, 1L, function(i) all(is.finite(xyz$z[i])))
    corners <- corners[complete, , drop = FALSE]
    faces <- rbind(
      corners[, c(1, 2, 3), drop = FALSE],
      corners[, c(1, 3, 4), drop = FALSE]
    )
  } else {
    surface_dependency("geometry", "Scattered 3D surfaces")
    norm <- as.matrix(xyz[1:2])
    for (i in 1:2) {
      norm[, i] <- (norm[, i] - min(norm[, i])) / diff(range(norm[, i]))
    }
    faces <- geometry::delaunayn(norm, options = "Qt Qc Qz Qbb")
    complete <- apply(faces, 1L, function(i) all(is.finite(xyz$z[i])))
    faces <- faces[complete, , drop = FALSE]
  }
  if (!nrow(faces)) {
    return(empty)
  }
  support_xy <- if (is.null(support$columns)) {
    xyz[1:2]
  } else {
    data[support$columns]
  }
  keep <- surface_faces_inside(as.matrix(support_xy), faces,
    support = support, domain = domain
  )
  faces <- faces[keep, , drop = FALSE]
  if (!nrow(faces)) {
    return(empty)
  }
  ids <- as.vector(t(faces))
  mesh <- data[ids, , drop = FALSE]
  mesh$.surface_x <- xyz$x[ids]
  mesh$.surface_y <- xyz$y[ids]
  mesh$.surface_z <- xyz$z[ids]
  heights <- matrix(xyz$z[ids], ncol = 3L, byrow = TRUE)
  mesh$.surface_fill <- rep(rowMeans(heights), each = 3L)
  mesh$.surface_face <- rep(seq_len(nrow(faces)), each = 3L)
  mesh
}

surface_contour_breaks <- function(values, bins) {
  values <- values[is.finite(values)]
  if (!length(values)) {
    return(numeric())
  }
  limits <- range(values)
  if (!all(is.finite(limits)) || diff(limits) == 0) {
    return(numeric())
  }
  if (is.null(bins)) {
    return(pretty(limits, 10))
  }
  if (!is.numeric(bins) || length(bins) != 1L || !is.finite(bins) ||
        bins < 1 || bins != floor(bins)) {
    stop("'n_contour' must be a positive integer.", call. = FALSE)
  }
  accuracy <- signif(diff(limits), 1) / 10
  limits <- c(floor(limits[1] / accuracy), ceiling(limits[2] / accuracy)) *
    accuracy
  if (bins == 1) {
    return(limits)
  }
  breaks <- scales::fullseq(limits, diff(limits) / (bins - 1))
  if (length(breaks) < bins + 1) {
    breaks <- scales::fullseq(limits, diff(limits) / bins)
  }
  breaks
}

# Intersect each triangle with horizontal planes. Segments inherit the panel
# columns from their face, so no contours can connect across facets or holes.
surface_contours <- function(mesh, breaks) {
  result <- mesh[FALSE, , drop = FALSE]
  result$.surface_xend <- result$.surface_yend <- numeric()
  result$.surface_zend <- numeric()
  if (!nrow(mesh) || !length(breaks)) {
    return(result)
  }
  pieces <- list()
  for (i in seq.int(1L, nrow(mesh), by = 3L)) {
    face <- mesh[i + 0:2, , drop = FALSE]
    heights <- face$.surface_z
    if (diff(range(heights)) == 0) next
    for (level in breaks[breaks >= min(heights) & breaks <= max(heights)]) {
      crossings <- list()
      for (edge in list(c(1L, 2L), c(2L, 3L), c(3L, 1L))) {
        z <- heights[edge]
        if (diff(z) == 0 || level < min(z) || level > max(z)) next
        weight <- (level - z[1]) / diff(z)
        xy <- as.matrix(face[edge, c(".surface_x", ".surface_y")])
        crossings[[length(crossings) + 1L]] <-
          xy[1, ] + weight * (xy[2, ] - xy[1, ])
      }
      if (length(crossings) < 2L) next
      xy <- unique(do.call(rbind, crossings))
      if (nrow(xy) != 2L) next
      segment <- face[1L, , drop = FALSE]
      segment$.surface_x <- xy[1, 1]
      segment$.surface_y <- xy[1, 2]
      segment$.surface_xend <- xy[2, 1]
      segment$.surface_yend <- xy[2, 2]
      segment$.surface_z <- segment$.surface_zend <- level
      pieces[[length(pieces) + 1L]] <- segment
    }
  }
  if (length(pieces)) dplyr::bind_rows(pieces) else result
}

# Sort contours with their owning triangle. Coplanar contour segments must be
# painted just after that face, then hidden by any nearer faces in the scene.
surface_scene_order <- function(projected) {
  owned <- projected$.prim == "segment" & !is.na(projected$surface_owner)
  if (!any(owned)) {
    return(projected)
  }
  contours <- split(which(owned), projected$surface_owner[owned])
  other <- which(!owned)
  groups <- projected$group[other]
  blocks <- split(
    other,
    cumsum(c(TRUE, groups[-1L] != groups[-length(groups)]))
  )
  order <- unlist(lapply(blocks, function(i) {
    last <- utils::tail(i, 1L)
    if (projected$.prim[last] == "polygon") {
      c(i, contours[[as.character(projected$surface_owner[last])]])
    } else {
      i
    }
  }), use.names = FALSE)
  projected[order, , drop = FALSE]
}

# Extend ggcube's public polygon layer so all primitives share its 3D sorter.
# Floor heights are assigned after scale training, so projected observations
# and soap boundaries cannot expand the effect range.
surface_scene_layer <- function(scene) {
  prototype <- ggcube::geom_polygon_3d()
  parent <- prototype$geom
  geom <- ggplot2::ggproto("GeomGratiaSurface3D", parent,
    optional_aes = c(
      parent$optional_aes, "surface_owner", "surface_type",
      "surface_floor", "surface_colour", "surface_alpha", "surface_linewidth",
      "surface_fixed_fill"
    ),
    draw_panel = function(data, panel_params, coord, sort_method = "auto",
                          scale_depth = TRUE, force_convex = FALSE,
                          rule = "evenodd") {
      data$.prim <- as.character(data$surface_type)
      data$z[data$surface_floor] <- min(panel_params$scale_info$z$limits)
      data$colour <- data$surface_colour
      fixed <- !is.na(data$surface_fixed_fill)
      data$fill[fixed] <- data$surface_fixed_fill[fixed]
      data$alpha <- data$surface_alpha
      data$linewidth <- data$surface_linewidth
      data$size <- 1.5
      data$shape <- 19
      data$stroke <- 0.5
      scene_coord <- ggplot2::ggproto(NULL, coord,
        transform = function(data, panel_params) {
          surface_scene_order(coord$transform(data, panel_params))
        }
      )
      parent$draw_panel(data, panel_params, scene_coord,
        sort_method = sort_method, scale_depth = scale_depth,
        force_convex = force_convex, rule = rule
      )
    }
  )
  ggplot2::layer(
    data = scene, geom = geom, stat = prototype$stat,
    position = "identity", inherit.aes = FALSE,
    params = list(colour = NA, alpha = 1),
    mapping = ggplot2::aes(
      x = .data$.surface_x, y = .data$.surface_y, z = .data$.surface_z,
      fill = .data$.surface_fill, group = .data$.surface_face,
      surface_owner = I(.data$.surface_owner),
      surface_type = I(.data$.surface_type),
      surface_floor = I(.data$.surface_floor),
      surface_colour = I(.data$.surface_colour),
      surface_fixed_fill = I(.data$.surface_fixed_fill),
      surface_alpha = I(.data$.surface_alpha),
      surface_linewidth = I(.data$.surface_linewidth)
    )
  )
}

surface_scene_data <- function(
  mesh, segments, rug, boundary, boundary_group,
  x_var, y_var, floor_height, contour_col
) {
  scene <- mesh
  scene$.surface_owner <- as.character(scene$.surface_face)
  scene$.surface_type <- rep("polygon", nrow(scene))
  scene$.surface_floor <- rep(FALSE, nrow(scene))
  scene$.surface_colour <- rep(NA_character_, nrow(scene))
  scene$.surface_alpha <- rep(1, nrow(scene))
  scene$.surface_linewidth <- rep(0.1, nrow(scene))
  scene$.surface_fixed_fill <- rep(NA_character_, nrow(scene))
  scene$.surface_role <- rep("estimate", nrow(scene))
  if (!is.null(segments) && nrow(segments)) {
    a <- segments[names(mesh)]
    b <- a
    b$.surface_x <- segments$.surface_xend
    b$.surface_y <- segments$.surface_yend
    b$.surface_z <- segments$.surface_zend
    a$.surface_face <- b$.surface_face <-
      paste0("contour:", seq_len(nrow(segments)))
    contours <- rbind(a, b)
    contours$.surface_owner <- rep(as.character(segments$.surface_face), 2L)
    contours$.surface_fill <- NA_real_
    contours$.surface_type <- "segment"
    contours$.surface_floor <- FALSE
    contours$.surface_colour <- contour_col
    contours$.surface_alpha <- 1
    contours$.surface_linewidth <- 0.5
    scene <- dplyr::bind_rows(scene, contours)
  }
  if (!is.null(rug) && nrow(rug)) {
    points <- mesh[rep(NA_integer_, nrow(rug)), , drop = FALSE]
    points$.surface_x <- rug[[x_var]]
    points$.surface_y <- rug[[y_var]]
    points$.surface_z <- floor_height
    points$.surface_fill <- NA_real_
    points$.surface_face <- paste0("rug:", seq_len(nrow(rug)))
    points$.surface_owner <- NA_character_
    points$.surface_type <- "point"
    points$.surface_floor <- TRUE
    points$.surface_colour <- "black"
    points$.surface_alpha <- 0.1
    points$.surface_linewidth <- 0.5
    scene <- dplyr::bind_rows(scene, points)
  }
  if (!is.null(boundary) && nrow(boundary)) {
    loops <- split(boundary, boundary[[boundary_group]])
    for (j in seq_along(loops)) {
      loop <- loops[[j]]
      if (nrow(loop) < 2L) next
      n <- nrow(loop) - 1L
      ends <- rep(seq_len(n), each = 2L) + rep(0:1, n)
      edges <- mesh[rep(NA_integer_, length(ends)), , drop = FALSE]
      edges$.surface_x <- loop[[x_var]][ends]
      edges$.surface_y <- loop[[y_var]][ends]
      edges$.surface_z <- floor_height
      edges$.surface_fill <- NA_real_
      edges$.surface_face <- paste("boundary", j,
        rep(seq_len(n), each = 2L),
        sep = ":"
      )
      edges$.surface_owner <- NA_character_
      edges$.surface_type <- "segment"
      edges$.surface_floor <- TRUE
      edges$.surface_colour <- "black"
      edges$.surface_alpha <- 1
      edges$.surface_linewidth <- 2
      scene <- dplyr::bind_rows(scene, edges)
    }
  }
  scene
}

# Bounds use the estimate's retained faces, so they cannot bridge a masked gap
# or choose a different diagonal. Missing bound vertices remove only that face.
surface_interval_faces <- function(mesh, alpha, colour) {
  if (!nrow(mesh)) return(mesh)
  dplyr::bind_rows(lapply(c(lower = ".lower_ci", upper = ".upper_ci"),
    function(column) {
      bound <- mesh
      z <- bound[[column]]
      complete <- rep(colSums(matrix(is.finite(z), nrow = 3L)) == 3L,
        each = 3L)
      bound <- bound[complete, , drop = FALSE]
      bound$.surface_z <- z[complete]
      role <- if (column == ".lower_ci") "lower" else "upper"
      bound$.surface_face <- if (nrow(bound)) {
        paste(role, bound$.surface_face, sep = ":")
      } else character()
      bound$.surface_owner <- as.character(bound$.surface_face)
      bound$.surface_role <- rep(role, nrow(bound))
      bound$.surface_fill <- rep(NA_real_, nrow(bound))
      bound$.surface_alpha <- rep(alpha, nrow(bound))
      bound$.surface_fixed_fill <- rep(colour, nrow(bound))
      bound
    }))
}

prepare_surface_3d <- function(
  object, x_var, y_var, fill_var, fill_limits,
  fill_title, labels, continuous_fill, contour, contour_col, n_contour,
  facet, facet_vars, rug, boundary, boundary_group, surface_3d, isotropic,
  support, z_limits, ci_3d = FALSE, ci_3d_alpha = 0.15, ci_3d_col = "grey60"
) {
  surface_dependency("ggcube", "3D surface plotting")
  if (utils::packageVersion("ggcube") < "0.3.0") {
    stop("3D surfaces require ggcube >= 0.3.0.", call. = FALSE)
  }
  domain <- surface_domain(boundary, x_var, y_var, boundary_group)
  groups <- if (length(facet_vars)) {
    dplyr::group_split(dplyr::group_by(
      object,
      dplyr::across(tidyselect::all_of(facet_vars))
    ))
  } else {
    list(object)
  }
  meshes <- lapply(groups, surface_mesh,
    x_var = x_var, y_var = y_var,
    z_var = fill_var, support = support, domain = domain,
    height_vars = if (ci_3d) c(fill_var, ".lower_ci", ".upper_ci") else fill_var
  )
  mesh <- dplyr::bind_rows(lapply(seq_along(meshes), function(i) {
    if (nrow(meshes[[i]])) {
      meshes[[i]]$.surface_face <-
        paste(i, meshes[[i]]$.surface_face, sep = ":")
    }
    meshes[[i]]
  }))
  if (is.null(continuous_fill)) {
    continuous_fill <- ggplot2::scale_fill_distiller(
      palette = "RdBu", type = "div"
    )
  }
  segments <- if (isTRUE(contour)) {
    breaks <- surface_contour_breaks(object[[fill_var]], n_contour)
    surface_contours(mesh, breaks)
  } else {
    NULL
  }
  heights <- object[[fill_var]][is.finite(object[[fill_var]])]
  scene <- surface_scene_data(mesh, segments, rug, boundary, boundary_group,
    x_var, y_var,
    floor_height = if (length(heights)) heights[1L] else 0,
    contour_col = contour_col
  )
  if (ci_3d) {
    bounds <- surface_interval_faces(
      scene[scene$.surface_type == "polygon", , drop = FALSE],
      ci_3d_alpha, ci_3d_col
    )
    scene <- dplyr::bind_rows(scene, bounds)
  }
  plt <- ggplot2::ggplot(object) +
    surface_scene_layer(scene) +
    facet +
    surface_view(surface_3d, object, x_var, y_var, isotropic) +
    labels +
    ggplot2::labs(z = fill_title, fill = fill_title) +
    continuous_fill +
    ggcube::scale_z_continuous() +
    ggplot2::geom_blank(data = object, mapping = ggplot2::aes(
      x = .data[[x_var]], y = .data[[y_var]]
    ), inherit.aes = FALSE)
  # Train ranges with the original vertices, not only face-averaged fills.
  finite <- object[is.finite(object[[fill_var]]), , drop = FALSE]
  if (nrow(finite)) {
    plt <- plt + ggplot2::geom_blank(
      data = finite,
      mapping = ggplot2::aes(
        x = .data[[x_var]], y = .data[[y_var]],
        z = .data[[fill_var]], fill = .data[[fill_var]]
      ), inherit.aes = FALSE
    )
  }
  if (!nrow(finite)) plt <- plt + ggplot2::expand_limits(z = c(0, 1))
  if (!is.null(fill_limits)) {
    plt <- plt + ggplot2::expand_limits(fill = fill_limits)
  }
  if (!is.null(z_limits)) {
    plt <- plt + ggplot2::expand_limits(z = z_limits)
  }
  plt + ggplot2::guides(fill = ggplot2::guide_colourbar(title = fill_title)) +
    ggplot2::theme(legend.position = "right")
}
