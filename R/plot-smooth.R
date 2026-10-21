# plot_smooth() generic and methods - lower level functions that do the actual
# ggplot plotting calls

`plot_smooth` <- function(object, ...) {
  UseMethod("plot_smooth")
}

#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_point geom_rug geom_abline
#'   expand_limits labs geom_line geom_ribbon aes guides guide_axis
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
`plot_smooth.mgcv_smooth` <- function(
  object,
  variables = NULL,
  rug = NULL,
  ci_level = 0.95,
  constant = NULL,
  fun = NULL,
  ci_alpha = 0.2,
  ci_col = "black",
  smooth_col = "black",
  resid_col = "steelblue3",
  decrease_col = "#56B4E9",
  increase_col = "#E69F00",
  change_lwd = 1.75,
  angle = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  partial_residuals = NULL,
  ylim = NULL,
  grouped_by = FALSE,
  ...
) {
  # do we have a grouped factor by?
  by_var <- unique(object$.by)
  # grouped_by can be set & TRUE even if this isn't a factor by smooth or
  # varying coef term - this catches those cases
  if (all(is.na(by_var)) || data_class(object)[[by_var]] == "numeric") {
    grouped_by <- FALSE
  }
  if (is.null(variables)) {
    if (isTRUE(grouped_by)) {
      variables <- vars_from_label(unique(object[[".term"]]))
    } else {
      variables <- vars_from_label(unique(object[[".smooth"]]))
    }
  }

  scales <- if (grouped_by) {
    prepare_smooth_scales(nlevels(object[[by_var]]))
  } else {
    list(colour = NULL, fill = NULL)
  }

  if (is.null(title)) {
    title <- ifelse(grouped_by, unique(object$.term),
      as.character(unique(object$.smooth))
    )
  }
  labels <- prepare_smooth_labels(object, x_var = variables,
    xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
    caption = caption, grouped_by = grouped_by
  )

  prepare_smooth_plot(object,
    x_var = variables, colour_var = if (grouped_by) by_var else NULL,
    group_var = if (grouped_by) by_var else NULL,
    rug = rug, constant = constant, fun = fun,
    ci_alpha = ci_alpha, ci_col = ci_col, smooth_col = smooth_col,
    partial_residuals = partial_residuals, resid_col = resid_col,
    sizer = TRUE, decrease_col = decrease_col, increase_col = increase_col,
    change_lwd = change_lwd, angle = angle, ylim = ylim,
    discrete_colour = scales$colour, discrete_fill = scales$fill,
    labels = labels
  )
}

#' Prepare common smooth plot labels
#'
#' Continuous-x plots use the same axis defaults for omitted and explicitly
#' NULL labels. Legend labels can be added to the result.
#'
#' @param object Prepared smooth estimates with `.smooth`, `.type` and `.by`
#'   metadata, and the by-variable column when applicable.
#' @param x_var Character string naming the x-axis covariate, used as the
#'   default x-axis label.
#' @param xlab,ylab Axis labels. `NULL` uses `x_var` for x and `y_var` for y
#'   (or `"Partial effect"` when `y_var` is `NULL`). Empty strings give blank labels.
#' @param y_var Optional y-axis covariate name for surface plots.
#' @param default_caption Caption used when `caption` is `NULL` or `TRUE`.
#'   Defaults to the basis caption; surface callers can include facet variables.
#' @param title Plot title, or `NULL` to use the unique `.smooth` value. Callers
#'   plotting grouped by smooths supply the `.term` title instead.
#' @param subtitle Plot subtitle, or `NULL` to derive it from the model part
#'   and by variable.
#' @param caption `NULL` or `TRUE` adds the basis caption; other values suppress
#'   it, retaining the existing smooth plotting convention.
#' @param grouped_by Logical; label a grouped factor by plot? Grouped plots keep
#'   the full title and name only the by variable in the default subtitle.
#'   Separate plots strip the title at the first colon and include the factor
#'   level in the subtitle. Continuous by variables have no level suffix.
#' @param by_level Data classes for which an ungrouped subtitle includes the
#'   by-variable level. Spherical plots include only unordered factors.
#' @return A `ggplot2::labs()` object.
#' @keywords internal
#' @noRd
`prepare_smooth_labels` <- function(
  object, x_var, xlab = NULL, ylab = NULL, title = NULL, subtitle = NULL,
  caption = NULL, grouped_by = FALSE, y_var = NULL,
  default_caption = paste("Basis:", object[[".type"]]),
  by_level = c("factor", "ordered")
) {
  default_subtitle <- is.null(subtitle)
  if (is.null(xlab)) {
    xlab <- x_var
  }
  if (is.null(ylab)) {
    ylab <- if (is.null(y_var)) "Partial effect" else y_var
  }
  if (is.null(title)) {
    title <- unique(object[[".smooth"]])
  }
  default_caption <- scasm_shape_plot_caption(object, default_caption)
  default_caption <- scam_shape_plot_caption(object, default_caption)
  interval_info <- attr(object, "interval_info")
  if (!identical(interval_info$method, "shape") &&
      !is.null(interval_info) && interval_info$interval == "simultaneous") {
    source <- if (identical(interval_info$method, "gaussian") &&
      length(attr(object, "shape_constraints"))) "Gaussian " else ""
    default_caption <- scasm_caption_append(default_caption,
      paste0(format(100 * interval_info$level), "% ", source,
        "simultaneous band (per smooth)"), sep = "; ")
  } else if (identical(interval_info$method, "bootstrap")) {
    default_caption <- scasm_caption_append(default_caption,
      paste0(format(100 * interval_info$level), "% bootstrap pointwise interval"))
  }
  caption <- if (is.null(caption) || isTRUE(caption)) {
    default_caption
  } else {
    NULL
  }

  if (all(!is.na(object[[".by"]]))) {
    by_var <- as.character(unique(object[[".by"]]))
    if (!grouped_by) {
      title <- strsplit(title, split = ":")[[1L]][[1L]]
    }
    if (is.null(subtitle)) {
      subtitle <- paste0("By: ", by_var)
      if (!grouped_by && data_class(object)[[by_var]] %in% by_level) {
        subtitle <- paste0(subtitle, "; ", unique(object[[by_var]]))
      }
    }
  }

  if (default_subtitle) subtitle <- smooth_plot_subtitle(object, subtitle)

  labs(x = xlab, y = ylab, title = title, subtitle = subtitle, caption = caption)
}

#' Choose paired discrete smooth plot scales
#'
#' @param n_levels Number of factor levels, including unused levels. Up to nine
#'   levels use Okabe-Ito scales; larger factors use hue scales.
#' @param discrete_colour,discrete_fill Optional user-supplied ggplot2 scales.
#'   Each `NULL` scale is replaced independently by its default.
#' @return A list with `colour` and `fill` scale objects. The ordinary ggplot2
#'   discrete colour default used for `fs` curves is handled by its caller.
#' @keywords internal
#' @noRd
`prepare_smooth_scales` <- function(
  n_levels, discrete_colour = NULL, discrete_fill = NULL
) {
  palette <- if (n_levels > 9L) {
    list(colour = scale_colour_hue, fill = scale_fill_hue)
  } else {
    list(colour = scale_colour_okabe_ito, fill = scale_fill_okabe_ito)
  }
  list(
    colour = if (is.null(discrete_colour)) gratia_default_scale(palette$colour()) else discrete_colour,
    fill = if (is.null(discrete_fill)) gratia_default_scale(palette$fill()) else discrete_fill
  )
}

#' Construct a continuous-x partial-effect plot
#'
#' Internal renderer: callers resolve variable names, labels, palettes and
#' smooth-specific data preparation. No smooth classes or labels are inspected.
#' Column arguments are strings; labels is a resolved ggplot2::labs() object.
#' Intervals use .lower_ci and .upper_ci; curves use .estimate. Transformations
#' apply only through the existing add_constant() and transform_fun() methods.
#' Residuals and SiZer columns retain their existing, untransformed values.
#' Set sizer only for callers that already support change overlays. A NULL
#' ribbon_colour removes the inherited colour mapping (as used by sz smooths).
#'
#' @param object Data frame of prepared smooth estimates containing `.estimate`
#'   and the columns named by the variable arguments. Intervals require
#'   `.lower_ci` and `.upper_ci`. Retain the classes needed by `add_constant()`
#'   and `transform_fun()`.
#' @param x_var Character string naming the continuous x-axis column in `object`
#'   and, when supplied, `rug` and `partial_residuals`.
#' @param colour_var Character string naming the column mapped to curve colour
#'   and interval fill, or `NULL` to use fixed colours.
#' @param group_var Character string naming the column mapped to the `group`
#'   aesthetic, or `NULL` to let ggplot2 infer groups from the other aesthetics.
#' @param interval Logical; draw a ribbon using `.lower_ci` and `.upper_ci`?
#' @param rug Optional data frame containing `x_var` for a bottom-axis rug.
#'   `NULL` omits the rug.
#' @param rug_colour_var Character string naming a column in `rug` to map to
#'   rug colour, or `NULL` for an uncoloured rug. Rug layers do not inherit
#'   the smooth's aesthetics.
#' @param labels Resolved labels supplied as a `ggplot2::labs()` object. The
#'   caller supplies axis, plot and legend labels as appropriate.
#' @param discrete_colour Optional ggplot2 colour scale, added when `colour_var`
#'   is supplied. `NULL` leaves scale selection to ggplot2.
#' @param discrete_fill Optional ggplot2 fill scale, added when `colour_var`
#'   is supplied. `NULL` leaves scale selection to ggplot2.
#' @param legend Logical; `FALSE` hides the legend, while `TRUE` retains the
#'   legend behaviour determined by the layers, scales and theme.
#' @param ribbon_colour Fixed ribbon outline colour when `colour_var` is
#'   supplied. The default, `NA`, suppresses the outline. `NULL` instead removes
#'   the inherited colour mapping, preserving the behaviour of `sz` plots.
#' @param constant Optional numeric constant added to the estimates and interval
#'   bounds via `add_constant()`, before applying `fun`. `NULL` adds nothing.
#' @param fun Optional function, or function name accepted by `match.fun()`,
#'   applied to estimates and interval bounds via `transform_fun()`. `NULL`
#'   leaves them untransformed. Neither `constant` nor `fun` modifies residuals
#'   or SiZer columns.
#' @param ci_alpha Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Numeric opacity for interval ribbons, between 0 and 1.
#' @param ci_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Fixed interval fill colour when `colour_var` is `NULL`.
#' @param smooth_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Fixed curve colour when `colour_var` is `NULL`. Also used
#'   for `.change` overlays in that case.
#' @param partial_residuals Optional data frame containing `x_var` and
#'   `partial_residual`, drawn as points beneath the ribbons and curves.
#'   `NULL` omits the points.
#' @param resid_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Fixed colour for partial residual points.
#' @param sizer Logical; add change overlays when their columns are present?
#'   `.change` takes precedence; otherwise both `.increase` and `.decrease`
#'   are used. With none of these columns present, no overlays are added.
#' @param decrease_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Fixed colour for `.decrease` overlays when `colour_var`
#'   is `NULL`; otherwise overlays use the mapped curve colours.
#' @param increase_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Fixed colour for `.increase` overlays when `colour_var`
#'   is `NULL`; otherwise overlays use the mapped curve colours.
#' @param change_lwd Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Numeric line width for SiZer overlays, passed to the
#'   `linewidth` argument of `ggplot2::geom_line()`.
#' @param angle Optional x-axis tick-label angle, passed to
#'   `ggplot2::guide_axis()`. `NULL` uses the theme's setting.
#' @param ylim Optional numeric values to include in the y-axis range via
#'   `ggplot2::expand_limits()`. These expand the range rather than clip it.
#'   `NULL` leaves the range determined by the plotted data.
#' @keywords internal
#' @noRd
`prepare_smooth_plot` <- function(
  object, x_var, colour_var = NULL, group_var = NULL,
  interval = TRUE, rug = NULL, rug_colour_var = NULL,
  labels = labs(), discrete_colour = NULL, discrete_fill = NULL,
  legend = TRUE, ribbon_colour = NA,
  constant = NULL, fun = NULL, ci_alpha = 0.2, ci_col = "black",
  smooth_col = "black", partial_residuals = NULL, resid_col = "steelblue3",
  sizer = FALSE, decrease_col = "#56B4E9", increase_col = "#E69F00",
  change_lwd = 1.75, angle = NULL, ylim = NULL
) {
  # If constant supplied apply it to `.estimate`
  object <- add_constant(object, constant = constant)

  # If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object, fun = fun)
  object <- order_interval_bounds(object)

  # String column names also support transformed covariates such as log2(x).
  plt <- gratia_plot(object, aes(x = .data[[x_var]], y = .data$.estimate)) +
    guides(x = guide_axis(angle = angle))
  if (!is.null(colour_var)) {
    plt <- plt + aes(colour = .data[[colour_var]])
  }

  if (!is.null(group_var)) {
    plt <- plt + aes(group = .data[[group_var]])
  }

  # do we want partial residuals? Only for univariate smooths without by vars
  if (!is.null(partial_residuals)) {
    plt <- plt + geom_gratia_partial_residual_point(
      data = partial_residuals,
      aes(
        x = .data[[x_var]],
        y = .data[["partial_residual"]]
      ),
      inherit.aes = FALSE,
      colour = resid_col, alpha = 0.5
    )
  }

  # plot the confidence interval and smooth line
  sizer_cols <- c(".change", ".increase", ".decrease")
  do_sizer <- sizer & sizer_cols %in% names(object)
  ribbon_mapping <- if (is.null(colour_var)) aes() else {
    aes(fill = .data[[colour_var]])
  }
  if (!is.null(colour_var) && is.null(ribbon_colour)) {
    # Keep an explicit NULL mapping to suppress inherited ribbon outlines.
    ribbon_mapping["colour"] <- list(NULL)
  }
  plt <- add_curve_interval(plt, interval_role = "smooth.interval",
    lower_var = if (interval) ".lower_ci" else NULL, upper_var = ".upper_ci",
    ribbon_mapping = ribbon_mapping, ribbon_alpha = ci_alpha,
    ribbon_colour = if (is.null(colour_var)) NA else ribbon_colour,
    ribbon_fill = if (is.null(colour_var)) ci_col else NULL,
    line_colour = if (is.null(colour_var)) smooth_col else NULL)
  if (!is.null(colour_var)) {
    plt <- plt + discrete_colour + discrete_fill
  }

  if (any(do_sizer)) {
    change_vars <- if (do_sizer[[1]]) ".change" else c(".increase", ".decrease")
    change_cols <- if (do_sizer[[1]]) list(smooth_col) else list(increase_col, decrease_col)
    for (i in seq_along(change_vars)) {
      change_var <- change_vars[[i]]
      if (is.null(colour_var)) {
        plt <- plt + gratia_change_geom(change_var, aes(y = .data[[change_var]]),
          colour = change_cols[[i]], linewidth = change_lwd,
          na.rm = TRUE, show.legend = FALSE)
      } else {
        plt <- plt + gratia_change_geom(change_var,
          aes(y = .data[[change_var]], colour = .data[[colour_var]]),
          linewidth = change_lwd, na.rm = TRUE,
          show.legend = if (do_sizer[[1]]) NA else FALSE)
      }
    }
  }

  plt <- plt + labels
  if (!legend) {
    plt <- plt + theme(legend.position = "none")
  }
  if (!is.null(rug)) {
    rug_mapping <- aes(x = .data[[x_var]])
    if (!is.null(rug_colour_var)) {
      rug_mapping <- aes(x = .data[[x_var]], colour = .data[[rug_colour_var]])
    }
    plt <- plt + geom_gratia_rug(data = rug, mapping = rug_mapping,
      inherit.aes = FALSE, sides = "b", alpha = 0.5)
  }
  if (!is.null(ylim)) {
    plt <- plt + expand_limits(y = ylim)
  }
  plt
}

#' Prepare values and fill ranges for a smooth surface
#'
#' @param object Prepared smooth estimates, retaining classes for
#'   `add_constant()` and `transform_fun()` dispatch.
#' @param show Character; display the estimate or its standard error.
#' @param constant Optional numeric shift applied before `fun`.
#' @param fun Optional transformation of estimates and interval bounds.
#'   Standard errors are not transformed.
#' @param ylim Optional fill-range expansion for estimates; ignored for SEs.
#' @return A list containing `data`, `fill_var`, `fill_title` and `fill_limits`.
#'   Automatic ranges use finite values only; with none, `fill_limits` is `NULL`.
#' @keywords internal
#' @noRd
`prepare_surface_data` <- function(
  object, show = c("estimate", "se"), constant = NULL, fun = NULL, ylim = NULL
) {
  show <- match.arg(show)
  object <- add_constant(object, constant = constant)
  object <- transform_fun(object, fun = fun)
  object <- order_interval_bounds(object)
  fill_var <- if (show == "estimate") ".estimate" else ".se"
  fill_title <- if (show == "estimate") "Partial\neffect" else "Std. err."
  fill_limits <- surface_fill_limits(object[[fill_var]],
    symmetric = show == "estimate", ylim = if (show == "estimate") ylim)
  list(data = object, fill_var = fill_var, fill_title = fill_title,
    fill_limits = fill_limits)
}

# Shared range policy for already transformed surface values. Explicit limits
# retain the smooth plotting API's range-expansion semantics.
surface_fill_limits <- function(values, symmetric = TRUE, ylim = NULL) {
  if (!is.null(ylim)) return(ylim)
  values <- values[is.finite(values)]
  if (!length(values)) return(NULL)
  if (isTRUE(symmetric)) c(-1, 1) * max(abs(values)) else range(values)
}

#' Construct a smooth surface plot
#'
#' Callers resolve smooth-specific variables, labels, facets and coordinates.
#' The input is already transformed; this function only assembles the plot.
#' Projected spherical callers supply their coordinates and always use tiles.
#'
#' @param object Data frame containing the surface coordinates and fill values.
#' @param x_var,y_var Character strings naming the coordinate columns in
#'   `object` and, if supplied, `rug` and `boundary`.
#' @param fill_var Character string naming the column mapped to fill and contours.
#' @param fill_limits Optional values to include in the fill range via
#'   `ggplot2::expand_limits()`. `NULL` adds no expansion layer.
#' @param fill_title Title for the vertical fill colour bar.
#' @param labels Resolved labels as a `ggplot2::labs()` object.
#' @param surface Character; `"raster"` (default) uses a raster image, while
#'   `"tile"` uses borderless rectangles sized from the coordinate resolution,
#'   and `"3d"` uses a polygon mesh.
#'   Changed covariate scales with irregular spacing use rectangles bounded by
#'   adjacent coordinate midpoints for flat surfaces.
#' @param surface_3d Named options for the 3D coordinate system.
#' @param ci_3d Enable 3D interval surfaces.
#' @param ci_3d_alpha,ci_3d_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Style 3D interval surfaces.
#' @param facet_vars Column names that identify separate mesh panels.
#' @param surface_support Recorded support and normalization for masking.
#' @param isotropic Preserve equal horizontal units by default.
#' @param z_limits Optional values to include in the vertical 3D range.
#' @param continuous_fill Optional ggplot2 fill scale. `NULL` uses the diverging
#'   RdBu distiller scale.
#' @param contour Logical; add contour lines for `fill_var`?
#' @param contour_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Fixed colour for contour lines.
#' @param n_contour Optional number of contour bins passed to `geom_contour()`.
#' @param angle Optional x-axis tick-label angle passed to `guide_axis()`.
#' @param facet Optional ggplot2 facet specification supplied by the caller.
#' @param coord Optional ggplot2 coordinate system supplied by the caller.
#' @param rug Optional observation data drawn as points with alpha 0.1.
#'   Faceted surface callers omit these data to avoid creating extra panels.
#' @param boundary Optional boundary coordinates, drawn last as black paths
#'   with linewidth 2.
#' @param boundary_group Character string naming the boundary loop column.
#'   Required when `boundary` is supplied.
#' @param mapping Optional plot-level mappings, replacing the default x/y
#'   mappings. Used by basis plots to retain inherited fill and grouping.
#' @param contour_mapping Optional contour mappings; defaults to `fill_var`
#'   mapped to z. Can explicitly remove inherited fill or set a contour group.
#' @param contour_data Optional data for contour layers only, for example to
#'   omit constant interaction panels. `NULL` inherits the surface data.
#' @param contour_na_rm Logical; remove missing contour values silently?
#' @param axis_guide X-axis guide; defaults to `guide_axis(angle = angle)`.
#' @param fill_guide Fill guide; `NULL` leaves ggplot2's guide unchanged.
#' @param legend_position Legend position; `NULL` preserves the theme setting.
#' @param tile_colour Fixed tile outline colour; `NULL` retains ggplot2's default.
#' @return A ggplot object.
#' @importFrom ggplot2 geom_tile
#' @keywords internal
#' @noRd
`prepare_surface_plot` <- function(
  object, x_var, y_var, fill_var, fill_limits = NULL, fill_title = NULL,
  labels = labs(), surface = c("raster", "tile", "3d"),
  surface_3d = list(),
  ci_3d = FALSE,
  ci_3d_alpha = 0.15, ci_3d_col = "grey60", continuous_fill = NULL,
  contour = TRUE, contour_col = "black", n_contour = NULL, angle = NULL,
  facet = NULL, coord = NULL, rug = NULL, boundary = NULL, boundary_group = NULL,
  mapping = NULL, contour_mapping = NULL, contour_na_rm = TRUE,
  contour_data = NULL,
  axis_guide = guide_axis(angle = angle),
  fill_guide = guide_colourbar(title = fill_title, direction = "vertical"),
  legend_position = "right", tile_colour = NA,
  facet_vars = character(), surface_support = NULL, isotropic = FALSE,
  z_limits = fill_limits, contour_stat = "contour"
) {
  surface <- match.arg(surface)
  validate_surface_3d(surface_3d)
  validate_ci_3d(ci_3d, ci_3d_alpha, ci_3d_col)
  if (surface == "3d") {
    if (is.numeric(object[[x_var]]) && is.numeric(object[[y_var]])) {
      ci_3d <- ci_3d && fill_var %in% c(".estimate", ".partial")
      if (ci_3d && !all(c(".lower_ci", ".upper_ci") %in% names(object))) {
        stop("3D interval surfaces require both interval-bound columns.", call. = FALSE)
      }
      return(prepare_surface_3d(object, x_var, y_var, fill_var, fill_limits,
        fill_title, labels, continuous_fill, contour, contour_col, n_contour,
        facet, facet_vars, rug, boundary, boundary_group, surface_3d,
        isotropic || inherits(coord, "CoordFixed"), surface_support, z_limits,
        ci_3d, ci_3d_alpha, ci_3d_col))
    }
    message("3D surfaces require two numeric axes; retaining the flat display.")
    surface <- "raster"
  }
  if (is.null(continuous_fill)) {
    continuous_fill <- gratia_default_scale(scale_fill_distiller(palette = "RdBu", type = "div"))
  }
  if (is.null(mapping)) {
    mapping <- aes(x = .data[[x_var]], y = .data[[y_var]])
  }
  irregular <- function(x) {
    x <- sort(unique(x[is.finite(x)]))
    gaps <- diff(x)
    length(gaps) > 1L && !isTRUE(all.equal(gaps, rep(gaps[[1L]], length(gaps)),
      tolerance = 1e-7))
  }
  changed <- isTRUE(attr(object, "gratia.coordinate_display"))
  if (changed && (irregular(object[[x_var]]) || irregular(object[[y_var]]))) {
    bounds <- function(x) {
      values <- sort(unique(x[is.finite(x)]))
      if (length(values) < 2L) return(cbind(x - 0.5, x + 0.5))
      mid <- (values[-1L] + values[-length(values)]) / 2
      edges <- c(values[[1L]] - (mid[[1L]] - values[[1L]]), mid,
        utils::tail(values, 1L) + (utils::tail(values, 1L) - utils::tail(mid, 1L)))
      i <- match(x, values)
      cbind(edges[i], edges[i + 1L])
    }
    cells <- object
    xb <- bounds(object[[x_var]])
    yb <- bounds(object[[y_var]])
    cells$.cell_xmin <- xb[, 1L]
    cells$.cell_xmax <- xb[, 2L]
    cells$.cell_ymin <- yb[, 1L]
    cells$.cell_ymax <- yb[, 2L]
    surface_geom <- geom_gratia_surface_rect(data = cells, mapping = aes(
      xmin = .data$.cell_xmin, xmax = .data$.cell_xmax,
      ymin = .data$.cell_ymin, ymax = .data$.cell_ymax,
      fill = .data[[fill_var]]), colour = tile_colour)
  } else {
    surface_geom <- switch(surface,
      raster = geom_gratia_surface_raster(aes(fill = .data[[fill_var]]), interpolate = FALSE),
      tile = if (is.null(tile_colour)) {
        geom_gratia_surface_tile(aes(fill = .data[[fill_var]]))
      } else {
        geom_gratia_surface_tile(aes(fill = .data[[fill_var]]), colour = tile_colour)
      })
  }
  plt <- gratia_plot(object, mapping) +
    surface_geom + facet + coord

  if (isTRUE(contour)) {
    if (is.null(contour_mapping)) {
      contour_mapping <- aes(z = .data[[fill_var]])
    }
    plt <- plt + geom_gratia_contour(contour_mapping, data = contour_data,
      colour = contour_col, bins = n_contour, na.rm = contour_na_rm,
      stat = contour_stat)
  }
  plt <- plt + labels + continuous_fill
  if (!is.null(fill_limits)) {
    plt <- plt + expand_limits(fill = fill_limits)
  }
  plt <- plt + guides(x = axis_guide)
  if (!is.null(fill_guide)) {
    plt <- plt + guides(fill = fill_guide)
  }
  if (!is.null(legend_position)) {
    plt <- gratia_plot_theme_defaults(plt, legend.position = legend_position)
  }

  if (!is.null(rug)) {
    plt <- plt + geom_gratia_surface_rug_point(data = rug,
      mapping = aes(x = .data[[x_var]], y = .data[[y_var]]),
      inherit.aes = FALSE, alpha = 0.1)
  }
  if (!is.null(boundary)) {
    plt <- plt + geom_gratia_boundary_path(data = boundary,
      mapping = aes(x = .data[[x_var]], y = .data[[y_var]],
        group = .data[[boundary_group]]),
      linewidth = 2, colour = "black")
  }
  plt
}

#' @importFrom ggplot2 ggplot geom_point geom_raster geom_contour
#'   expand_limits labs guides guide_colourbar theme guide_axis
#' @importFrom grid unit
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
`plot_smooth.bivariate_smooth` <- function(
  object,
  variables = NULL,
  rug = NULL,
  show = c("estimate", "se"),
  contour = TRUE,
  contour_col = "black",
  n_contour = NULL,
  constant = NULL,
  fun = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  continuous_fill = NULL,
  angle = NULL,
  surface = c("raster", "tile", "3d"),
  surface_3d = list(),
  ci_3d = FALSE,
  ci_3d_alpha = 0.15, ci_3d_col = "grey60",
  surface_support = NULL,
  ...
) {
  surface <- match.arg(surface)
  validate_surface_3d(surface_3d)
  validate_ci_3d(ci_3d, ci_3d_alpha, ci_3d_col)
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  prepared <- prepare_surface_data(object, show = show,
    constant = constant, fun = fun, ylim = ylim)
  object <- prepared$data
  labels <- prepare_smooth_labels(object,
    x_var = variables[1], y_var = variables[2], xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption
  )
  prepare_surface_plot(object,
    x_var = variables[1], y_var = variables[2], surface = surface, surface_3d = surface_3d,
    ci_3d = ci_3d, ci_3d_alpha = ci_3d_alpha, ci_3d_col = ci_3d_col,
    fill_var = prepared$fill_var, fill_limits = prepared$fill_limits,
    fill_title = prepared$fill_title, labels = labels,
    continuous_fill = continuous_fill, contour = contour,
    contour_col = contour_col, n_contour = n_contour, angle = angle, rug = rug,
    surface_support = surface_support, isotropic = inherits(object, "isotropic_smooth")
  )
}

#' @importFrom ggplot2 ggplot geom_point geom_raster geom_contour aes
#'   expand_limits labs guides guide_colourbar theme facet_wrap
#' @importFrom grid unit
#' @keywords internal
#' @noRd
`plot_smooth.trivariate_smooth` <- function(
  object,
  variables = NULL,
  rug = NULL,
  show = c("estimate", "se"),
  contour = TRUE,
  contour_col = "black",
  n_contour = NULL,
  constant = NULL,
  fun = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  continuous_fill = NULL,
  angle = NULL,
  surface = c("raster", "tile", "3d"),
  surface_3d = list(),
  ci_3d = FALSE,
  ci_3d_alpha = 0.15, ci_3d_col = "grey60",
  surface_support = NULL,
  ...
) {
  surface <- match.arg(surface)
  validate_surface_3d(surface_3d)
  validate_ci_3d(ci_3d, ci_3d_alpha, ci_3d_col)
  if (is.null(variables)) {
    variables <- attr(object, "tensor_term_order")
    if (is.null(variables)) {
      variables <- vars_from_label(unique(object[[".smooth"]]))
    }
  }

  prepared <- prepare_surface_data(object, show = show,
    constant = constant, fun = fun, ylim = ylim)
  object <- prepared$data
  labels <- prepare_smooth_labels(object,
    x_var = variables[1], y_var = variables[2], xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption,
    default_caption = paste("Facets:", variables[3], "; Basis:", object[[".type"]])
  )
  # Observations are not assigned to slices: adding them would create facets.
  coord <- if (inherits(object, "isotropic_smooth")) coord_equal() else NULL
  prepare_surface_plot(object,
    x_var = variables[1], y_var = variables[2], surface = surface, surface_3d = surface_3d,
    ci_3d = ci_3d, ci_3d_alpha = ci_3d_alpha, ci_3d_col = ci_3d_col,
    fill_var = prepared$fill_var, fill_limits = prepared$fill_limits,
    fill_title = prepared$fill_title, labels = labels,
    continuous_fill = continuous_fill, contour = contour,
    contour_col = contour_col, n_contour = n_contour, angle = angle,
    facet = facet_wrap(vars(.data[[variables[3]]])), coord = coord,
    facet_vars = variables[3], surface_support = surface_support
  )
}

#' @importFrom ggplot2 ggplot geom_point geom_raster geom_contour
#'   expand_limits labs guides guide_colourbar theme facet_grid
#' @importFrom dplyr vars
#' @importFrom grid unit
#' @keywords internal
#' @noRd
`plot_smooth.quadvariate_smooth` <- function(
  object,
  variables = NULL,
  rug = NULL,
  show = c("estimate", "se"),
  contour = TRUE,
  contour_col = "black",
  n_contour = NULL,
  constant = NULL,
  fun = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  continuous_fill = NULL,
  angle = NULL,
  surface = c("raster", "tile", "3d"),
  surface_3d = list(),
  ci_3d = FALSE,
  ci_3d_alpha = 0.15, ci_3d_col = "grey60",
  surface_support = NULL,
  ...
) {
  surface <- match.arg(surface)
  validate_surface_3d(surface_3d)
  validate_ci_3d(ci_3d, ci_3d_alpha, ci_3d_col)
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  prepared <- prepare_surface_data(object, show = show,
    constant = constant, fun = fun, ylim = ylim)
  object <- prepared$data
  labels <- prepare_smooth_labels(object,
    x_var = variables[1], y_var = variables[2], xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption,
    default_caption = paste("Facet rows:", variables[3],
      "; columns:", variables[4], "; Basis:", object[[".type"]])
  )
  # Observations are not assigned to slices: adding them would create facets.
  coord <- if (inherits(object, "isotropic_smooth")) coord_equal() else NULL
  prepare_surface_plot(object,
    x_var = variables[1], y_var = variables[2], surface = surface, surface_3d = surface_3d,
    ci_3d = ci_3d, ci_3d_alpha = ci_3d_alpha, ci_3d_col = ci_3d_col,
    fill_var = prepared$fill_var, fill_limits = prepared$fill_limits,
    fill_title = prepared$fill_title, labels = labels,
    continuous_fill = continuous_fill, contour = contour,
    contour_col = contour_col, n_contour = n_contour, angle = angle,
    facet = facet_grid(rows = vars(.data[[variables[3]]]),
      cols = vars(.data[[variables[4]]]),
      as.table = is.factor(object[[variables[3]]])), coord = coord,
    facet_vars = variables[3:4], surface_support = surface_support
  )
}

#' @importFrom ggplot2 coord_equal
`plot_smooth.isotropic_smooth` <- function(
  object,
  surface = c("raster", "tile", "3d"),
  surface_3d = list(),
  ci_3d = FALSE,
  ci_3d_alpha = 0.15, ci_3d_col = "grey60",
  surface_support = NULL,
  ...
) {
  surface <- match.arg(surface)
  validate_surface_3d(surface_3d)
  validate_ci_3d(ci_3d, ci_3d_alpha, ci_3d_col)
  # Preserve equal horizontal units even when this method is called directly.
  class(object) <- unique(c("isotropic_smooth", class(object)))
  # plot as per a bivariate smooth
  plt <- plot_smooth.bivariate_smooth(object, surface = surface,
    surface_3d = surface_3d,
    ci_3d = ci_3d, ci_3d_alpha = ci_3d_alpha, ci_3d_col = ci_3d_col,
    surface_support = surface_support, ...)

  # but set the x/y coordinates to have aspect ratio = 1
  if (surface != "3d") plt <- plt + coord_equal(ratio = 1)

  plt # return
}

#' @importFrom ggplot2 ggplot geom_point geom_abline expand_limits
#'   labs
#' @keywords internal
#' @noRd
`plot_smooth.random_effect` <- function(
  object,
  variables = NULL,
  qq_line = TRUE,
  constant = NULL,
  fun = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  angle = NULL,
  ...
) {
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  ## If constant supplied apply it to `est`
  object <- add_constant(object, constant = constant)

  ## If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object, fun = fun)
  # QQ panels show estimated effects only and need no interval columns.
  attr(object, "interval_info") <- NULL

  ## base plot with computed QQs
  plt <- gratia_plot(object, aes(sample = .data[[".estimate"]])) +
    geom_gratia_smooth_point(stat = "qq") +
    guides(x = guide_axis(angle = angle))

  ## add a QQ reference line
  if (isTRUE(qq_line)) {
    sampq <- quantile(object[[".estimate"]], c(0.25, 0.75))
    gaussq <- qnorm(c(0.25, 0.75))
    slope <- diff(sampq) / diff(gaussq)
    intercept <- sampq[1L] - slope * gaussq[1L]

    plt <- plt + geom_gratia_reference_abline(slope = slope, intercept = intercept)
  }

  plt <- plt + prepare_smooth_labels(object,
    x_var = "Gaussian quantiles", y_var = "Partial effects",
    xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
    caption = caption)

  ## fixing the y axis limits?
  if (!is.null(ylim)) {
    plt <- plt + expand_limits(y = ylim)
  }

  plt
}

#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_point geom_line expand_limits theme aes
#'   labs
#' @keywords internal
#' @noRd
`plot_smooth.factor_smooth` <- function(
  object,
  variables = NULL,
  rug = NULL,
  constant = NULL,
  fun = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  discrete_colour = NULL,
  angle = NULL,
  ...
) {
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  numeric_vars <- variables[vapply(object[variables], is.numeric, logical(1))]
  if (length(numeric_vars) >= 2L) {
    variables <- tensor_plot_variables(variables, object)
    renderer <- switch(as.character(length(variables)),
      `2` = plot_smooth.bivariate_smooth,
      `3` = plot_smooth.trivariate_smooth,
      `4` = plot_smooth.quadvariate_smooth)
    if (is.null(renderer)) {
      cli_alert_info("Can't yet plot tensor terms with more than four covariates: {unique(object$.smooth)}.")
      return(NULL)
    }
    return(renderer(object, variables = variables, rug = rug,
      constant = constant, fun = fun, xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle, caption = caption,
      ylim = ylim, angle = angle, ...))
  }

  # Combine categorical margins only for grouping; fitted factor columns remain.
  curve_facet <- NULL
  if (length(variables) > 2L) {
    factors <- setdiff(variables, numeric_vars)
    group_vars <- if (length(numeric_vars)) factors else factors[-1L]
    group_column <- utils::tail(make.unique(c(names(object), ".re_group")), 1L)
    groups <- tensor_plot_groups(object, rug, factors, group_vars)
    object[[group_column]] <- groups$group
    rug <- groups$rug
    if (!is.null(rug)) rug[[group_column]] <- groups$rug_group
    if (length(numeric_vars)) {
      curve_facet <- facet_wrap(vars(.data[[group_column]]))
    }
    variables <- c(if (length(numeric_vars)) numeric_vars else factors[1L], group_column)
  }

  # A tensor can have two factor margins, or put its factor margin first.
  # Only continuous covariates should be connected with lines.
  factor_vars <- vapply(object[variables], is.factor, logical(1))
  all_factors <- all(factor_vars)
  if (!all_factors) {
    variables <- c(variables[!factor_vars], variables[factor_vars])
  }

  if (is.null(discrete_colour)) {
    discrete_colour <- gratia_default_scale(scale_colour_discrete())
  }

  labels <- prepare_smooth_labels(object, x_var = variables[1],
    xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

  if (!all_factors) {
    return(prepare_smooth_plot(object,
      x_var = variables[1], colour_var = variables[2], interval = FALSE,
      rug = rug, constant = constant, fun = fun,
      discrete_colour = discrete_colour, legend = FALSE,
      angle = angle, ylim = ylim,
      labels = labels
    ) + curve_facet)
  }

  # Factor-only point ranges retain their explicit-NULL label suppression.
  if (!missing(xlab)) {
    labels["x"] <- list(xlab)
  }
  if (!missing(ylab)) {
    labels["y"] <- list(ylab)
  }

  ## If constant supplied apply it to `est`
  object <- add_constant(object, constant = constant)

  ## If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object, fun = fun)
  object <- order_interval_bounds(object)

  plt <- gratia_plot(object, aes(
    x = .data[[variables[1]]],
    y = .data[[".estimate"]],
    ymin = .data[[".lower_ci"]],
    ymax = .data[[".upper_ci"]]
  )) +
    geom_gratia_smooth_pointrange() +
    facet_wrap(vars(.data[[variables[2]]]), labeller = ggplot2::label_both)
  plt <- plt + guides(x = guide_axis(angle = angle))

  ## add labelling to plot
  plt <- plt + labels

  ## add rug?
  if (!is.null(rug)) {
    plt <- plt + geom_gratia_rug(
      data = rug,
      mapping = aes(x = .data[[variables[1]]]),
      inherit.aes = FALSE,
      sides = "b", alpha = 0.5
    )
  }

  ## fixing the y axis limits?
  if (!is.null(ylim)) {
    plt <- plt + expand_limits(y = ylim)
  }

  plt
}

#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_point geom_line expand_limits theme aes
#'   labs scale_fill_hue scale_colour_hue
#' @importFrom ggokabeito scale_colour_okabe_ito scale_fill_okabe_ito
#' @keywords internal
#' @noRd
`plot_smooth.sz_factor_smooth` <- function(
  object,
  variables = NULL,
  rug = NULL,
  constant = NULL,
  fun = NULL,
  ci_alpha = 0.2,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  discrete_colour = NULL,
  discrete_fill = NULL,
  angle = NULL,
  ...
) {
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  fs <- vapply(object[variables], is.factor, logical(1L))
  # Are we plotting a >1D base smoother?
  plt <- if (sum(!fs) > 1L) {
    plot_multivariate_sz_smooth(object,
      variables = variables, rug = rug,
      constant = constant, fun = fun, ci_alpha = ci_alpha,
      xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
      caption = caption, ylim = ylim, discrete_colour = discrete_colour,
      discrete_fill = discrete_fill, angle = angle,
      ...
    )
  } else {
    plot_univariate_sz_smooth(object,
      variables = variables, rug = rug,
      constant = constant, fun = fun, ci_alpha = ci_alpha,
      xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
      caption = caption, ylim = ylim, discrete_colour = discrete_colour,
      discrete_fill = discrete_fill, angle = angle,
      ...
    )
  }
  plt
}

`plot_multivariate_sz_smooth` <- function(
  object,
  variables = NULL,
  rug = NULL,
  constant = NULL,
  fun = NULL,
  ci_alpha = 0.2,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  discrete_colour = NULL,
  discrete_fill = NULL,
  angle = NULL,
  ...
) {
  message("Can't currently plot multivariate 'sz' smooths.")
  message("Skipping: ", unique(object[[".smooth"]]))
  NULL
}

`plot_univariate_sz_smooth` <- function(
  object,
  variables = NULL,
  rug = NULL,
  constant = NULL,
  fun = NULL,
  ci_alpha = 0.2,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  discrete_colour = NULL,
  discrete_fill = NULL,
  angle = NULL,
  ...
) {
  # variables will likely be length two, but it could be >2 if there are
  # multivariate factors **or** if the base smooth is nD isotropic smooth
  # such as a TPRS or Duchon spline
  fs <- vapply(object[variables], is.factor, logical(1L))
  if (length(variables) > 2L) {
    object <- mutate(object,
      ".sz_var" = interaction(object[variables[fs]],
        sep = ":",
        lex.order = TRUE
      )
    )
    fac_var <- ".sz_var"
    fac_var_lab <- paste(variables[fs], sep = ":")
    x_var <- variables[!fs]

    # need to repeat for the rug
    if (!is.null(rug)) {
      rug <- mutate(rug,
        ".sz_var" = interaction(rug[variables[fs]],
          sep = ":",
          lex.order = TRUE
        )
      )
    }

    if (length(x_var) > 1L) {
      # this is a bivariate sz factor smooth, which we can't handle yet
      return(NULL)
    }
  } else {
    # which is the factor?
    if (fs[1L]) {
      x_var <- variables[2]
      fac_var <- fac_var_lab <- variables[1]
    } else {
      x_var <- variables[1]
      fac_var <- fac_var_lab <- variables[2]
    }
  }

  scales <- prepare_smooth_scales(nlevels(object[[fac_var]]),
    discrete_colour = discrete_colour, discrete_fill = discrete_fill
  )

  labels <- prepare_smooth_labels(object, x_var = x_var,
    xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
    caption = caption
  )
  labels$colour <- labels$fill <- fac_var_lab

  prepare_smooth_plot(object,
    x_var = x_var, colour_var = fac_var, rug = rug, rug_colour_var = fac_var,
    constant = constant, fun = fun, ci_alpha = ci_alpha,
    discrete_colour = scales$colour, discrete_fill = scales$fill,
    ribbon_colour = NULL, angle = angle, ylim = ylim,
    labels = labels
  )
}

#' @importFrom ggplot2 coord_sf geom_tile guide_colourbar geom_contour aes
#'   expand_limits guides guide_axis geom_point theme labs
#' @importFrom grid unit
`plot_smooth.sos` <- function(
  object,
  variables = NULL,
  rug = NULL,
  show = c("estimate", "se"),
  contour = TRUE,
  contour_col = "black",
  n_contour = NULL,
  constant = NULL,
  fun = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  continuous_fill = NULL,
  crs = NULL,
  default_crs = NULL,
  lims_method = "geometry_bbox",
  angle = NULL,
  surface = "raster", surface_3d = list(),
  ci_3d = FALSE,
  ci_3d_alpha = 0.15, ci_3d_col = "grey60",
  grouped_by = FALSE,
  ...
) {
  if (surface == "3d") message("Spherical smooths retain their geographic rendering.")
  # handle splines on the sphere
  # this needs the sf pkg for coord_sf()
  if (!requireNamespace("sf", quietly = TRUE)) {
    message(
      "\nPlotting SOS smooths uses `ggplot2::coord_sf()`.\n",
      "This requires that the {sf} package be installed.\n",
      "Run: `install.packages(\"sf\")`\n"
    )
    stop("Package {sf} is not available.")
  }
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }


  # If constant supplied apply it to `est`
  object <- add_constant(object, constant = constant)

  # If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object, fun = fun)
  object <- order_interval_bounds(object)

  show <- match.arg(show)
  if (isTRUE(identical(show, "estimate"))) {
    guide_title <- "Partial\neffect"
    plot_var <- ".estimate"
    guide_limits <- if (is.null(ylim)) {
      c(-1, 1) * max(abs(object[[plot_var]]), na.rm = TRUE)
    } else {
      ylim
    }
  } else {
    guide_title <- "Std. err."
    plot_var <- ".se"
    guide_limits <- range(object[[".se"]])
  }

  # if crs is not specified, use orthographic, rotated to centre of data
  # longitude
  if (is.null(crs)) {
    crs <- paste0(
      "+proj=ortho +lat_0=20 +lon_0=",
      mean(range(object[[variables[2]]]))
    )
  }
  if (is.null(default_crs)) {
    default_crs <- 4326
  }

  # default axis labels if none supplied
  if (missing(xlab)) {
    xlab <- variables[2] ## yes, the smooth is s(lat, lon) !
  }

  if (missing(ylab)) {
    ylab <- variables[1] ## yes, the smooth is s(lat, lon) !
  }

  panel <- NULL
  if (isTRUE(grouped_by) && all(!is.na(object$.by))) {
    by_var <- unique(object$.by)
    if (length(by_var) == 1L && is.factor(object[[by_var]])) {
      panel <- by_var
      if (is.null(title)) title <- unique(sub(":.*$", "", object$.smooth))
    }
  }
  labels <- prepare_smooth_labels(object,
    x_var = variables[2], y_var = variables[1], xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption, grouped_by = grouped_by,
    by_level = "factor")
  # Preserve the spherical method's omitted-versus-NULL axis convention.
  labels[c("x", "y")] <- list(xlab, ylab)

  groups <- if (is.null(panel)) list(object) else split(object, object[[panel]], drop = TRUE)
  prepared <- lapply(groups, sos_plot_data,
    longitude = variables[2], latitude = variables[1])
  object <- if (length(prepared) == 1L) prepared[[1L]]$data else {
    dplyr::bind_rows(lapply(prepared, `[[`, "data"))
  }
  geometry <- lapply(prepared, function(group) {
    if (group$rectangular) {
      cells <- sos_cells(group$data, longitude = variables[2], latitude = variables[1])
    } else {
      message("Spherical surfaces require a rectilinear grid; drawing supplied coordinates as points.")
      cells <- sos_points(group$data, longitude = variables[2], latitude = variables[1])
    }
    sos_project(cells, crs = crs)
  })
  if (is.null(continuous_fill)) {
    continuous_fill <- gratia_default_scale(scale_fill_distiller(palette = "RdBu", type = "div"))
  }
  plt <- gratia_plot(object)
  for (i in seq_along(geometry)) {
    plt <- plt + geom_gratia_surface_sf(data = geometry[[i]],
      mapping = aes(fill = .data[[plot_var]]), inherit.aes = FALSE,
      colour = if (prepared[[i]]$rectangular) NA else "black", shape = 21)
  }
  plt <- plt + labels + continuous_fill + expand_limits(fill = guide_limits) +
    guides(x = guide_axis(angle = angle),
      fill = guide_colourbar(title = guide_title, direction = "vertical"))
  plt <- gratia_plot_theme_defaults(plt, legend.position = "right")
  if (!is.null(panel)) plt <- plt + facet_wrap(vars(!!rlang::sym(panel)))
  if (!any(vapply(geometry, nrow, integer(1))) && !is.null(sos_ortho_view(crs))) {
    # An entirely hidden supplied grid still needs finite panel bounds. An
    # invisible horizon outline trains the panel without inventing estimates.
    outline <- sf::st_sf(geometry = sos_horizon(sos_ortho_view(crs)))
    outline <- sos_project(outline, crs)
    plt <- plt + ggplot2::geom_sf(data = outline, inherit.aes = FALSE,
      fill = NA, colour = NA, show.legend = FALSE)
  }
  if (isTRUE(contour)) {
    for (group in prepared) {
      if (!group$rectangular) next
      paths <- sos_contours(group$data, variables[2], variables[1], plot_var, n_contour)
      if (!is.null(paths)) {
        if (!is.null(panel)) paths[[panel]] <- group$data[[panel]][rep(1L, nrow(paths))]
        paths <- sos_project(paths, crs = crs)
        plt <- plt + geom_gratia_contour_sf(data = paths, inherit.aes = FALSE,
          colour = contour_col, fill = NA)
      }
    }
  }
  if (!is.null(rug)) {
    points <- sos_points(rug, longitude = variables[2], latitude = variables[1])
    points <- sos_project(points, crs = crs)
    plt <- plt + geom_gratia_surface_rug_sf(data = points, inherit.aes = FALSE, alpha = 0.1)
  }
  plt + coord_sf(crs = crs, default_crs = default_crs, lims_method = lims_method)

}

#' @importFrom ggplot2 ggplot geom_point geom_raster geom_contour
#'   expand_limits labs guides guide_colourbar theme guide_axis geom_line
#'   geom_path scale_fill_distiller coord_fixed
#' @importFrom grid unit
#' @importFrom rlang .data
#' @importFrom vctrs vec_slice
#' @keywords internal
#' @noRd
`plot_smooth.soap_film` <- function(
  object,
  variables = NULL,
  rug = NULL,
  show = c("estimate", "se"),
  contour = TRUE,
  contour_col = "black",
  n_contour = NULL,
  constant = NULL,
  fun = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylim = NULL,
  continuous_fill = NULL,
  angle = NULL,
  surface = c("raster", "tile", "3d"),
  surface_3d = list(),
  ci_3d = FALSE,
  ci_3d_alpha = 0.15, ci_3d_col = "grey60",
  surface_support = NULL,
  ...
) {
  surface <- match.arg(surface)
  validate_surface_3d(surface_3d)
  validate_ci_3d(ci_3d, ci_3d_alpha, ci_3d_col)
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  prepared <- prepare_surface_data(object, show = show,
    constant = constant, fun = fun, ylim = ylim)
  object <- prepared$data
  boundary <- vec_slice(object, object[[".bndry"]])
  object <- vec_slice(object, !object[[".bndry"]])
  labels <- prepare_smooth_labels(object,
    x_var = variables[1], y_var = variables[2], xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption
  )
  prepare_surface_plot(object,
    x_var = variables[1], y_var = variables[2], surface = surface, surface_3d = surface_3d,
    ci_3d = ci_3d, ci_3d_alpha = ci_3d_alpha, ci_3d_col = ci_3d_col,
    fill_var = prepared$fill_var, fill_limits = prepared$fill_limits,
    fill_title = prepared$fill_title, labels = labels,
    continuous_fill = continuous_fill, contour = contour,
    contour_col = contour_col, n_contour = n_contour, angle = angle,
    rug = rug, coord = coord_fixed(ratio = 1), boundary = boundary,
    boundary_group = ".loop"
  )
}
