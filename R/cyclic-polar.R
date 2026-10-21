# Coordinate limits always come from the fitted basis, never the plotted grid.
validate_cyclic_polar <- function(cyclic, polar_args) {
  allowed <- c("start", "reverse", "inner.radius", "r.axis.inside",
    "rotate.angle", "clip")
  if (!is.list(polar_args) || (length(polar_args) &&
      (is.null(names(polar_args)) || anyNA(names(polar_args)) ||
       any(!nzchar(names(polar_args))) || anyDuplicated(names(polar_args))))) {
    cli::cli_abort("`polar_args` must be a list with unique argument names.")
  }
  unknown <- setdiff(names(polar_args), allowed)
  if (length(unknown)) {
    cli::cli_abort("Unsupported `polar_args`: {paste(unknown, collapse = ', ')}. Gratia determines the angular variable and fitted cycle limits.")
  }
  if (length(polar_args) && cyclic != "polar") {
    cli::cli_abort("Nonempty `polar_args` requires `cyclic = \"polar\"`.")
  }
  # Validate options even when the selection contains no cyclic curves.
  if (length(polar_args)) do.call(ggplot2::coord_radial, polar_args)
  invisible(NULL)
}

resolve_cyclic_plot <- function(object, variables, cyclic, cyclic_info,
                                back_transform, coordinate_info) {
  if (cyclic != "polar" || length(variables) != 1L) return(NULL)
  identities <- unique(object[, intersect(c(".smooth", ".type", ".smooth_id"),
    names(object)), drop = FALSE])
  labels <- as.character(identities$.smooth)
  records <- lapply(seq_len(nrow(identities)), function(i) {
    candidates <- which(names(cyclic_info) == labels[[i]])
    candidates <- candidates[vapply(cyclic_info[candidates], function(info) {
      if (!is.list(info)) return(FALSE)
      type_matches <- is.null(info$type) || identical(info$type, identities$.type[[i]])
      id_matches <- !".smooth_id" %in% names(identities) || is.null(info$id) ||
        identical(as.integer(info$id), as.integer(identities$.smooth_id[[i]]))
      type_matches && id_matches
    }, logical(1))]
    if (length(candidates) > 1L) {
      cli::cli_abort("Ambiguous fitted cyclic boundary metadata for {.val {labels[[i]]}}; recompute smooth estimates.")
    }
    if (length(candidates)) cyclic_info[[candidates]] else NULL
  })
  known_cyclic <- any(grepl("cyclic", object$.type, ignore.case = TRUE))
  # Old tensor estimates do not say which marginal basis they used.
  unknown_tensor <- is.null(cyclic_info) &&
    any(grepl("1d Tensor product", object$.type, fixed = TRUE))
  if (!known_cyclic && !unknown_tensor && all(vapply(records, is.null, logical(1)))) {
    return(NULL)
  }
  if (!length(records) || any(vapply(records, is.null, logical(1)))) {
    cli::cli_abort(c("Fitted cyclic boundary metadata is missing for {paste(labels, collapse = ', ')}.",
      "i" = "Recompute with smooth_estimates(model) before drawing in polar coordinates."))
  }
  valid <- vapply(records, function(info) {
    is.list(info) && identical(info$variable, variables[[1L]]) &&
      is.numeric(info$boundaries) && length(info$boundaries) == 2L &&
      all(is.finite(info$boundaries)) && diff(info$boundaries) > 0
  }, logical(1))
  if (!all(valid)) cli::cli_abort("Invalid fitted cyclic boundary metadata for {paste(labels, collapse = ', ')}.")
  bounds <- records[[1L]]$boundaries
  same <- vapply(records, function(info) {
    isTRUE(all.equal(info$boundaries, bounds, check.attributes = FALSE))
  }, logical(1))
  if (!all(same)) {
    cli::cli_abort("Grouped cyclic curves must have matching cycle boundaries; draw them in separate panels.")
  }
  axis <- coordinate_info[[variables[[1L]]]]
  transformed <- if (!is.null(axis)) {
    !identical(axis$original, variables[[1L]])
  } else !identical(make.names(variables[[1L]]), variables[[1L]])
  if (isTRUE(back_transform) && transformed) {
    cli::cli_abort("Polar plots of transformed cyclic covariates require `back_transform = FALSE`.")
  }
  records[[1L]]
}

add_cyclic_polar <- function(plt, info, polar_args, angle = NULL) {
  bounds <- info$boundaries
  plt$layers <- lapply(plt$layers, function(layer) {
    if (inherits(layer$geom, "GeomRug")) {
      original <- layer$geom
      layer$geom <- ggplot2::ggproto(NULL, original, draw_panel = draw_cyclic_rug)
    }
    layer
  })
  breaks <- function(limits) {
    ticks <- scales::breaks_pretty()(bounds)
    ticks[ticks >= bounds[1L] & ticks < bounds[2L]]
  }
  if (!"r.axis.inside" %in% names(polar_args)) {
    polar_args$r.axis.inside <- TRUE
  }
  coord <- do.call(ggplot2::coord_radial,
    # Keep the cycle closed while allowing radial padding between the band
    # and the observation rug at the outer edge.
    c(list(theta = "x", thetalim = bounds,
      expand = c(top = TRUE, right = FALSE, bottom = TRUE, left = FALSE)),
      polar_args))
  plt + ggplot2::scale_x_continuous(breaks = breaks) + coord +
    ggplot2::guides(theta = ggplot2::guide_axis_theta(angle = angle))
}

# Cartesian rugs assume a rectangular panel. Transform both ends of radial
# marks so observations remain visible around the outer edge of the cycle.
draw_cyclic_rug <- function(data, panel_params, coord, lineend = "butt",
                            sides = "b", outside = FALSE,
                            length = grid::unit(0.03, "npc")) {
  limits <- panel_params$r.range
  reversed <- coord$reverse %in% c("r", "thetar")
  edge <- if (reversed) limits[1L] else limits[2L]
  delta <- diff(limits) * 0.03 * if (reversed) 1 else -1
  data$y <- edge
  from <- coord$transform(data, panel_params)
  data$y <- edge + delta
  to <- coord$transform(data, panel_params)
  grid::segmentsGrob(from$x, from$y, to$x, to$y, default.units = "native",
    gp = grid::gpar(col = scales::alpha(data$colour, data$alpha),
      lwd = data$linewidth * ggplot2::.pt, lty = data$linetype,
      lineend = lineend))
}
