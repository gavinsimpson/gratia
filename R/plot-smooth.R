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
#' @param subtitle Plot subtitle, or `NULL` to derive it from the by variable.
#' @param caption `NULL` or `TRUE` adds the basis caption; other values suppress
#'   it, retaining the existing smooth plotting convention.
#' @param grouped_by Logical; label a grouped factor by plot? Grouped plots keep
#'   the full title and name only the by variable in the default subtitle.
#'   Separate plots strip the title at the first colon and include the factor
#'   level in the subtitle. Continuous by variables have no level suffix.
#' @return A `ggplot2::labs()` object.
#' @keywords internal
#' @noRd
`prepare_smooth_labels` <- function(
  object, x_var, xlab = NULL, ylab = NULL, title = NULL, subtitle = NULL,
  caption = NULL, grouped_by = FALSE, y_var = NULL,
  default_caption = paste("Basis:", object[[".type"]])
) {
  if (is.null(xlab)) {
    xlab <- x_var
  }
  if (is.null(ylab)) {
    ylab <- if (is.null(y_var)) "Partial effect" else y_var
  }
  if (is.null(title)) {
    title <- unique(object[[".smooth"]])
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
      if (!grouped_by && data_class(object)[[by_var]] %in% c("factor", "ordered")) {
        subtitle <- paste0(subtitle, "; ", unique(object[[by_var]]))
      }
    }
  }

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
    colour = if (is.null(discrete_colour)) palette$colour() else discrete_colour,
    fill = if (is.null(discrete_fill)) palette$fill() else discrete_fill
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
#' @param ci_alpha Numeric opacity for interval ribbons, between 0 and 1.
#' @param ci_col Fixed interval fill colour when `colour_var` is `NULL`.
#' @param smooth_col Fixed curve colour when `colour_var` is `NULL`. Also used
#'   for `.change` overlays in that case.
#' @param partial_residuals Optional data frame containing `x_var` and
#'   `partial_residual`, drawn as points beneath the ribbons and curves.
#'   `NULL` omits the points.
#' @param resid_col Fixed colour for partial residual points.
#' @param sizer Logical; add change overlays when their columns are present?
#'   `.change` takes precedence; otherwise both `.increase` and `.decrease`
#'   are used. With none of these columns present, no overlays are added.
#' @param decrease_col Fixed colour for `.decrease` overlays when `colour_var`
#'   is `NULL`; otherwise overlays use the mapped curve colours.
#' @param increase_col Fixed colour for `.increase` overlays when `colour_var`
#'   is `NULL`; otherwise overlays use the mapped curve colours.
#' @param change_lwd Numeric line width for SiZer overlays, passed to the
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

  # String column names also support transformed covariates such as log2(x).
  plt <- ggplot(object, aes(x = .data[[x_var]], y = .data$.estimate)) +
    guides(x = guide_axis(angle = angle))
  if (!is.null(colour_var)) {
    plt <- plt + aes(colour = .data[[colour_var]])
  }

  if (!is.null(group_var)) {
    plt <- plt + aes(group = .data[[group_var]])
  }

  # do we want partial residuals? Only for univariate smooths without by vars
  if (!is.null(partial_residuals)) {
    plt <- plt + geom_point(
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
  if (!is.null(colour_var)) {
    if (interval) {
      ribbon_mapping <- aes(
        ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]],
        fill = .data[[colour_var]]
      )
      if (is.null(ribbon_colour)) {
        # An explicit NULL mapping removes the inherited colour for sz curves.
        ribbon_mapping <- aes(
          ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]],
          fill = .data[[colour_var]], colour = NULL
        )
        plt <- plt + geom_ribbon(ribbon_mapping, alpha = ci_alpha)
      } else {
        plt <- plt + geom_ribbon(ribbon_mapping,
          alpha = ci_alpha, colour = ribbon_colour)
      }
    }
    plt <- plt + geom_line()

    plt <- plt + discrete_colour + discrete_fill
  } else {
    if (interval) {
      plt <- plt + geom_ribbon(
        aes(ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]]),
        alpha = ci_alpha, colour = NA, fill = ci_col
      )
    }
    plt <- plt + geom_line(colour = smooth_col)
  }

  if (any(do_sizer)) {
    change_vars <- if (do_sizer[[1]]) ".change" else c(".increase", ".decrease")
    change_cols <- if (do_sizer[[1]]) smooth_col else c(increase_col, decrease_col)
    for (i in seq_along(change_vars)) {
      change_var <- change_vars[[i]]
      if (is.null(colour_var)) {
        plt <- plt + geom_line(aes(y = .data[[change_var]]),
          colour = change_cols[[i]], linewidth = change_lwd,
          na.rm = TRUE, show.legend = FALSE)
      } else {
        plt <- plt + geom_line(
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
    plt <- plt + geom_rug(data = rug, mapping = rug_mapping,
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
  fill_var <- if (show == "estimate") ".estimate" else ".se"
  fill_title <- if (show == "estimate") "Partial\neffect" else "Std. err."
  values <- object[[fill_var]]
  values <- values[is.finite(values)]
  fill_limits <- if (show == "estimate" && !is.null(ylim)) {
    ylim
  } else if (length(values) == 0L) {
    NULL
  } else if (show == "estimate") {
    c(-1, 1) * max(abs(values))
  } else {
    range(values)
  }
  list(data = object, fill_var = fill_var, fill_title = fill_title,
    fill_limits = fill_limits)
}

#' Construct a flat smooth surface plot
#'
#' Callers resolve smooth-specific variables, labels, facets and coordinates.
#' The input is already transformed; this function only assembles the plot.
#'
#' @param object Data frame containing the surface coordinates and fill values.
#' @param x_var,y_var Character strings naming the coordinate columns in
#'   `object` and, if supplied, `rug` and `boundary`.
#' @param fill_var Character string naming the column mapped to fill and contours.
#' @param fill_limits Optional values to include in the fill range via
#'   `ggplot2::expand_limits()`. `NULL` adds no expansion layer.
#' @param fill_title Title for the vertical fill colour bar.
#' @param labels Resolved labels as a `ggplot2::labs()` object.
#' @param geom Character; `"raster"` (default) uses a raster image, while
#'   `"tile"` uses borderless rectangles sized from the coordinate resolution.
#' @param continuous_fill Optional ggplot2 fill scale. `NULL` uses the diverging
#'   RdBu distiller scale.
#' @param contour Logical; add contour lines for `fill_var`?
#' @param contour_col Fixed colour for contour lines.
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
#' @return A ggplot object.
#' @importFrom ggplot2 geom_tile
#' @keywords internal
#' @noRd
`prepare_surface_plot` <- function(
  object, x_var, y_var, fill_var, fill_limits = NULL, fill_title = NULL,
  labels = labs(), geom = c("raster", "tile"), continuous_fill = NULL,
  contour = TRUE, contour_col = "black", n_contour = NULL, angle = NULL,
  facet = NULL, coord = NULL, rug = NULL, boundary = NULL, boundary_group = NULL
) {
  geom <- match.arg(geom)
  if (is.null(continuous_fill)) {
    continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
  }
  plt <- ggplot(object, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    switch(geom,
      raster = geom_raster(aes(fill = .data[[fill_var]]), interpolate = FALSE),
      tile = geom_tile(aes(fill = .data[[fill_var]]), colour = NA)
    ) + facet + coord

  if (isTRUE(contour)) {
    plt <- plt + geom_contour(aes(z = .data[[fill_var]]),
      colour = contour_col, bins = n_contour, na.rm = TRUE)
  }
  plt <- plt + labels + continuous_fill
  if (!is.null(fill_limits)) {
    plt <- plt + expand_limits(fill = fill_limits)
  }
  plt <- plt + guides(
    fill = guide_colourbar(title = fill_title, direction = "vertical"),
    x = guide_axis(angle = angle)
  ) + theme(legend.position = "right")

  if (!is.null(rug)) {
    plt <- plt + geom_point(data = rug,
      mapping = aes(x = .data[[x_var]], y = .data[[y_var]]),
      inherit.aes = FALSE, alpha = 0.1)
  }
  if (!is.null(boundary)) {
    plt <- plt + geom_path(data = boundary,
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
  geom = c("raster", "tile"),
  ...
) {
  geom <- match.arg(geom)
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  surface <- prepare_surface_data(object, show = show,
    constant = constant, fun = fun, ylim = ylim)
  object <- surface$data
  labels <- prepare_smooth_labels(object,
    x_var = variables[1], y_var = variables[2], xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption
  )
  prepare_surface_plot(object,
    x_var = variables[1], y_var = variables[2], geom = geom,
    fill_var = surface$fill_var, fill_limits = surface$fill_limits,
    fill_title = surface$fill_title, labels = labels,
    continuous_fill = continuous_fill, contour = contour,
    contour_col = contour_col, n_contour = n_contour, angle = angle, rug = rug
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
  geom = c("raster", "tile"),
  ...
) {
  geom <- match.arg(geom)
  if (is.null(variables)) {
    variables <- attr(object, "tensor_term_order")
    if (is.null(variables)) {
      variables <- vars_from_label(unique(object[[".smooth"]]))
    }
  }

  surface <- prepare_surface_data(object, show = show,
    constant = constant, fun = fun, ylim = ylim)
  object <- surface$data
  labels <- prepare_smooth_labels(object,
    x_var = variables[1], y_var = variables[2], xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption,
    default_caption = paste("Facets:", variables[3], "; Basis:", object[[".type"]])
  )
  # Observations are not assigned to slices: adding them would create facets.
  coord <- if (inherits(object, "isotropic_smooth")) coord_equal() else NULL
  prepare_surface_plot(object,
    x_var = variables[1], y_var = variables[2], geom = geom,
    fill_var = surface$fill_var, fill_limits = surface$fill_limits,
    fill_title = surface$fill_title, labels = labels,
    continuous_fill = continuous_fill, contour = contour,
    contour_col = contour_col, n_contour = n_contour, angle = angle,
    facet = facet_wrap(vars(.data[[variables[3]]])), coord = coord
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
  geom = c("raster", "tile"),
  ...
) {
  geom <- match.arg(geom)
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  surface <- prepare_surface_data(object, show = show,
    constant = constant, fun = fun, ylim = ylim)
  object <- surface$data
  labels <- prepare_smooth_labels(object,
    x_var = variables[1], y_var = variables[2], xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption,
    default_caption = paste("Facet rows:", variables[3],
      "; columns:", variables[4], "; Basis:", object[[".type"]])
  )
  # Observations are not assigned to slices: adding them would create facets.
  coord <- if (inherits(object, "isotropic_smooth")) coord_equal() else NULL
  prepare_surface_plot(object,
    x_var = variables[1], y_var = variables[2], geom = geom,
    fill_var = surface$fill_var, fill_limits = surface$fill_limits,
    fill_title = surface$fill_title, labels = labels,
    continuous_fill = continuous_fill, contour = contour,
    contour_col = contour_col, n_contour = n_contour, angle = angle,
    facet = facet_grid(rows = vars(.data[[variables[3]]]),
      cols = vars(.data[[variables[4]]]), as.table = FALSE), coord = coord
  )
}

#' @importFrom ggplot2 coord_equal
`plot_smooth.isotropic_smooth` <- function(
  object,
  geom = c("raster", "tile"),
  ...
) {
  geom <- match.arg(geom)
  # plot as per a bivariate smooth
  plt <- plot_smooth.bivariate_smooth(object, geom = geom, ...)

  # but set the x/y coordinates to have aspect ratio = 1
  plt <- plt + coord_equal(ratio = 1)

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

  ## base plot with computed QQs
  plt <- ggplot(object, aes(sample = .data[[".estimate"]])) +
    geom_point(stat = "qq") +
    guides(x = guide_axis(angle = angle))

  ## add a QQ reference line
  if (isTRUE(qq_line)) {
    sampq <- quantile(object[[".estimate"]], c(0.25, 0.75))
    gaussq <- qnorm(c(0.25, 0.75))
    slope <- diff(sampq) / diff(gaussq)
    intercept <- sampq[1L] - slope * gaussq[1L]

    plt <- plt + geom_abline(slope = slope, intercept = intercept)
  }

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- "Gaussian quantiles"
  }
  if (is.null(ylab)) {
    ylab <- "Partial effects"
  }
  if (is.null(title)) {
    title <- unique(object$.smooth) # variables
  }
  # add the basis via caption if caption is TRUE or NULL
  if ((is.logical(caption) && isTRUE(caption)) || is.null(caption)) {
    caption <- paste("Basis:", object[[".type"]])
  } else {
    caption <- NULL
  }

  if (all(!is.na(object[[".by"]]))) {
    # is the by variable a factor or a numeric
    by_class <- data_class(object)[[object[[".by"]][[1L]]]]
    by_var <- as.character(unique(object[[".by"]]))
    spl <- strsplit(title, split = ":")
    title <- spl[[1L]][[1L]]
    if (is.null(subtitle)) {
      subtitle <- if (by_class %in% c("factor", "ordered")) {
        paste0("By: ", by_var, "; ", unique(object[[by_var]]))
      } else {
        paste0("By: ", by_var) # continuous by
      }
    }
  }

  ## add labelling to plot
  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

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

  # throw a warning and return NULL if trying to plot a >=2d base smoother
  # like a 2D TPRS or Duchon spline
  if ((l <- length(variables)) > 2L) {
    # warning("Can't plot ", l - 1, "D random factor smooths. Not plotting.")
    msg <- "Can't yet plot multivariate smooths with a 're' marginal: {unique(object[['.smooth']])}."
    #message("Can't currently plot multivariate smooths with a 're' marginal.")
    #message("Skipping: ", unique(object[[".smooth"]]))
    cli_alert_info(msg, wrap = TRUE)
    return(NULL) # returns early!
  }

  # A tensor can have two factor margins, or put its factor margin first.
  # Only continuous covariates should be connected with lines.
  factor_vars <- vapply(object[variables], is.factor, logical(1))
  all_factors <- all(factor_vars)
  if (!all_factors) {
    variables <- c(variables[!factor_vars], variables[factor_vars])
  }

  if (is.null(discrete_colour)) {
    discrete_colour <- scale_colour_discrete()
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
    ))
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

  plt <- ggplot(object, aes(
    x = .data[[variables[1]]],
    y = .data[[".estimate"]],
    ymin = .data[[".lower_ci"]],
    ymax = .data[[".upper_ci"]]
  )) +
    ggplot2::geom_pointrange() +
    facet_wrap(vars(.data[[variables[2]]]), labeller = ggplot2::label_both)
  plt <- plt + guides(x = guide_axis(angle = angle))

  ## add labelling to plot
  plt <- plt + labels

  ## add rug?
  if (!is.null(rug)) {
    plt <- plt + geom_rug(
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
  lims_method = "cross",
  angle = NULL,
  ...
) {
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

  if (is.null(continuous_fill)) {
    continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
  }

  # If constant supplied apply it to `est`
  object <- add_constant(object, constant = constant)

  # If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object, fun = fun)

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

  # base plot
  # Simon parameterises the SOS with first argument latitude and second
  #  argument longitude, so we need to reverse that here
  plt <- ggplot(object, aes(
    x = .data[[variables[2]]],
    y = .data[[variables[1]]]
  )) +
    geom_tile(mapping = aes(fill = .data[[plot_var]])) +
    coord_sf(
      crs = crs, default_crs = default_crs,
      lims_method = lims_method
    )
  # foo <- object |> select(latitude, longitude, .estimate)
  # foo_df <- as.data.frame(foo)
  # sf <- stars::st_as_stars(foo_df) |>
  #   sf::st_set_crs("OGC:CRS84") |>
  #   sf::st_as_sf(points = FALSE) |>
  #   sf::st_set_agr("constant")
  # st_ortho_cut <- function(x, lon_0, lat_0, radius = 9800000) {
  #   stopifnot(st_is_longlat(x))
  #   pt <- sf::st_sfc(st_point(c(lon_0, lat_0)), crs = "OGC:CRS84")
  #   buf <- sf::st_buffer(pt, units::set_units(radius, "m"))
  #   ortho <- paste0("+proj=ortho +lat_0=", lat_0, " +lon_0=", lon_0)
  #   sf::st_transform(sf::st_intersection(x, buf), sf::st_crs(ortho))
  # }
  # sf_o <- st_ortho_cut(sf, lat_0 = 20, lon_0 = mean(range(object[[variables[2]]])))
  
  # ggplot() + geom_sf(data = sf, aes(fill = .estimate))

  if (isTRUE(contour)) {
    plt <- plt + geom_contour(
      mapping = aes(z = .data[[plot_var]]),
      colour = contour_col,
      bins = n_contour,
      na.rm = TRUE
    )
  }

  # default axis labels if none supplied
  if (missing(xlab)) {
    xlab <- variables[2] ## yes, the smooth is s(lat, lon) !
  }

  if (missing(ylab)) {
    ylab <- variables[1] ## yes, the smooth is s(lat, lon) !
  }

  if (is.null(title)) {
    title <- unique(object[[".smooth"]])
  }
  # add the basis via caption if caption is TRUE or NULL
  if ((is.logical(caption) && isTRUE(caption)) || is.null(caption)) {
    caption <- paste("Basis:", object[[".type"]])
  } else {
    caption <- NULL
  }

  if (all(!is.na(object[[".by"]]))) {
    # is the by variable a factor or a numeric
    by_class <- data_class(object)[[object[[".by"]][[1L]]]]
    by_var <- as.character(unique(object[[".by"]]))
    spl <- strsplit(title, split = ":")
    title <- spl[[1L]][[1L]]
    if (is.null(subtitle)) {
      subtitle <- if (by_class != "factor") {
        paste0("By: ", by_var) # continuous by
      } else {
        paste0("By: ", by_var, "; ", unique(object[[by_var]]))
      }
    }
  }

  # add labelling to plot
  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

  # Set the palette
  plt <- plt + continuous_fill

  # Set the limits for the fill
  plt <- plt + expand_limits(fill = guide_limits)

  # add guide
  plt <- plt +
    guides(
      fill = guide_colourbar(
        title = guide_title, direction = "vertical"#,
        #barheight = grid::unit(5, "lines") #grid::unit(0.25, "npc")
      ),
      x = guide_axis(angle = angle)
    )

  # position legend at the
  plt <- plt + theme(legend.position = "right")

  # add rug?
  if (!is.null(rug)) {
    plt <- plt +
      geom_point(
        data = rug, ## yes, the smooth is s(lat, lon) !
        mapping = aes(
          x = .data[[variables[2]]],
          y = .data[[variables[1]]]
        ),
        inherit.aes = FALSE, alpha = 0.1
      )
  }

  plt
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
  geom = c("raster", "tile"),
  ...
) {
  geom <- match.arg(geom)
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  surface <- prepare_surface_data(object, show = show,
    constant = constant, fun = fun, ylim = ylim)
  object <- surface$data
  boundary <- vec_slice(object, object[[".bndry"]])
  object <- vec_slice(object, !object[[".bndry"]])
  labels <- prepare_smooth_labels(object,
    x_var = variables[1], y_var = variables[2], xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption
  )
  prepare_surface_plot(object,
    x_var = variables[1], y_var = variables[2], geom = geom,
    fill_var = surface$fill_var, fill_limits = surface$fill_limits,
    fill_title = surface$fill_title, labels = labels,
    continuous_fill = continuous_fill, contour = contour,
    contour_col = contour_col, n_contour = n_contour, angle = angle,
    rug = rug, coord = coord_fixed(ratio = 1), boundary = boundary,
    boundary_group = ".loop"
  )
}
