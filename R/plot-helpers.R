#' Add an interval ribbon and curve to an existing plot
#'
#' This renderer does no data preparation, transformation or labelling. The
#' caller supplies the plot's x/y/group mappings and any layers below the curve.
#' Layers are appended in ribbon-then-line order, inheriting plot aesthetics.
#'
#' @param plot A ggplot object with data and coordinate mappings.
#' @param lower_var,upper_var Names of interval columns. A `NULL` `lower_var`
#'   omits the ribbon; otherwise both column names must be supplied.
#' @param ribbon_mapping,line_mapping Additional layer mappings. Ribbon mappings
#'   can override inherited aesthetics, including explicitly setting `y = NULL`.
#' @param ribbon_alpha Ribbon opacity.
#' @param ribbon_colour,ribbon_fill Fixed ribbon colour and fill. `NULL` leaves
#'   the aesthetic inherited or at ggplot2's default; `NA` suppresses outlines.
#' @param line_colour,line_alpha Fixed line colour and opacity. `NULL` leaves
#'   the aesthetic inherited or at ggplot2's default.
#' @return The plot with its new layers.
#' @keywords internal
#' @noRd
add_curve_interval <- function(plot, lower_var = NULL, upper_var = NULL,
  ribbon_mapping = aes(), line_mapping = aes(), ribbon_alpha = 0.2,
  ribbon_colour = NULL, ribbon_fill = NULL, line_colour = NULL,
  line_alpha = NULL) {
  if (!is.null(lower_var)) {
    mapping <- aes(ymin = .data[[lower_var]], ymax = .data[[upper_var]])
    mapping[names(ribbon_mapping)] <- ribbon_mapping
    params <- list(mapping = mapping, alpha = ribbon_alpha)
    if (!is.null(ribbon_colour)) params$colour <- ribbon_colour
    if (!is.null(ribbon_fill)) params$fill <- ribbon_fill
    plot <- plot + do.call(geom_ribbon, params)
  }
  params <- list(mapping = line_mapping)
  if (!is.null(line_colour)) params$colour <- line_colour
  if (!is.null(line_alpha)) params$alpha <- line_alpha
  plot + do.call(geom_line, params)
}

#' Resolve the default arrangement of plot panels
#'
#' @param n_plots Number of panels, using the caller's existing counting rule.
#' @param ncol,nrow Requested layout dimensions. Defaults are calculated only
#'   when both are `NULL`; supplying either leaves both arguments unchanged.
#' @return A list containing `ncol` and `nrow`.
#' @keywords internal
#' @noRd
prepare_plot_layout <- function(n_plots, ncol = NULL, nrow = NULL) {
  if (is.null(ncol) && is.null(nrow)) {
    ncol <- ceiling(sqrt(n_plots))
    nrow <- ceiling(n_plots / ncol)
  }
  list(ncol = ncol, nrow = nrow)
}

#' Assemble plots that optionally share interval limits
#'
#' @param plots List of ggplot objects.
#' @param object Data containing `.lower_ci` and `.upper_ci` columns. Fixed
#'   limits use the entire object, including unselected smooths, as before.
#' @param scales Either `"fixed"` or `"free"`. Fixed limits use `lims()`, retaining
#'   its clipping behaviour and existing handling of non-finite bounds.
#' @param ncol,nrow Requested panel layout dimensions.
#' @param guides Guide collection policy passed to `patchwork::wrap_plots()`.
#' @param ... Additional patchwork arguments supplied by the public method.
#' @return A patchwork object.
#' @keywords internal
#' @noRd
wrap_interval_plots <- function(plots, object, scales, ncol = NULL,
  nrow = NULL, guides = "keep", ...) {
  if (identical(scales, "fixed")) {
    limits <- range(object[[".lower_ci"]], object[[".upper_ci"]])
    plots <- lapply(plots, function(p) p + lims(y = limits))
  }
  layout <- prepare_plot_layout(length(plots), ncol = ncol, nrow = nrow)
  wrap_plots(plots, byrow = TRUE, ncol = layout$ncol, nrow = layout$nrow,
    guides = guides, ...)
}

#' Construct a derivative curve panel
#'
#' @param object Prepared derivative data.
#' @param x_var,y_var Column names for the curve coordinates.
#' @param group_var,colour_var Optional columns for grouping and colouring curves.
#' @param labels Resolved ggplot labels, supplied by the derivative method.
#' @param interval Logical; include the `.lower_ci`/`.upper_ci` ribbon?
#' @param alpha Ribbon opacity.
#' @param angle X-axis tick-label angle.
#' @return A ggplot object, before any change overlays.
#' @keywords internal
#' @noRd
prepare_derivative_plot <- function(object, x_var, y_var, labels,
  group_var = NULL, colour_var = NULL, interval = TRUE, alpha = 0.2,
  angle = NULL) {
  mapping <- aes(x = .data[[x_var]], y = .data[[y_var]])
  if (!is.null(group_var)) mapping$group <- aes(group = .data[[group_var]])$group
  if (!is.null(colour_var)) {
    mapping$colour <- aes(colour = .data[[colour_var]])$colour
  }
  add_curve_interval(ggplot(object, mapping),
    lower_var = if (interval) ".lower_ci" else NULL, upper_var = ".upper_ci",
    ribbon_mapping = aes(y = NULL), ribbon_alpha = alpha) +
    guides(x = guide_axis(angle = angle)) + labels
}

#' Resolve labels for plots of basis functions
#'
#' @param object Basis evaluations with smooth metadata.
#' @param variables One or two covariate names, in plotting order.
#' @param xlab,ylab Axis labels; `NULL` selects basis-specific defaults.
#' @param title Plot title; `NULL` uses the stored smooth call, falling back to
#'   the smooth label for basis evaluations computed from a model.
#' @param subtitle,caption Labels passed through unchanged.
#' @return A ggplot labels object, including the basis legend title.
#' @keywords internal
#' @noRd
prepare_basis_labels <- function(object, variables, xlab = NULL, ylab = NULL,
  title = NULL, subtitle = NULL, caption = NULL) {
  surface <- length(variables) == 2L
  if (is.null(xlab)) xlab <- variables[1L]
  if (is.null(ylab)) ylab <- if (surface) variables[2L] else "Value"
  if (is.null(title)) {
    title <- attr(object, "smooth_object")
    if (is.null(title)) title <- unique(object[[".smooth"]])
  }
  labels <- labs(x = xlab, y = ylab, title = title,
    subtitle = subtitle, caption = caption)
  if (surface) labels$fill <- "value" else labels$colour <- "Basis\nfunction"
  labels
}

#' Resolve the existing basis-plot facet convention
#'
#' @param object Basis evaluations containing `.by` and `.bf` metadata.
#' @param variables One or two covariate names.
#' @param labeller Facet labeller; `NULL` uses `prefix_label_both`.
#' @return A facet specification or `NULL`. Factor/character by variables take
#'   precedence over basis-function facets, preserving the existing convention.
#' @keywords internal
#' @noRd
prepare_basis_facet <- function(object, variables, labeller = NULL) {
  if (is.null(labeller)) labeller <- prefix_label_both
  if (all(!is.na(object[[".by"]]))) {
    by_var <- unique(object[[".by"]])
    if (is.character(object[[by_var]]) || is.factor(object[[by_var]])) {
      return(facet_wrap(by_var, labeller = labeller))
    }
  } else if (length(variables) == 2L) {
    return(facet_wrap(vars(.data[[".bf"]]), labeller = labeller))
  }
  NULL
}
