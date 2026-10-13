# The MRF coefficient parameterization is not its area domain, particularly
# for reduced-rank bases. Match by labels, retaining fitted factor order.
mrf_area_levels <- function(smooth, model) {
  domain <- levels(smooth$knots)
  fitted <- prepare_smooth_data(model, smooth)[[smooth$term]]
  order <- if (is.factor(fitted)) levels(fitted) else unique(as.character(fitted))
  c(intersect(order, domain), setdiff(domain, order))
}

prepare_mrf_data <- function(smooth, data, model) {
  if (!nrow(data)) cli::cli_abort("MRF evaluation data must contain at least one row.")
  term <- smooth$term
  area <- mrf_area_levels(smooth, model)
  values <- as.character(data[[term]])
  unknown <- unique(values[!is.na(values) & !values %in% area])
  if (length(unknown)) {
    cli::cli_abort("Unknown MRF area label{?s}: {.val {unknown}}.",
      class = "gratia_mrf_area_error")
  }
  data[[term]] <- factor(values, levels = area)
  data
}

#' @keywords internal
#' @noRd
plot_smooth.mrf_smooth <- function(object, variables = NULL,
  mrf_plot = c("effects", "qq"), qq_line = TRUE,
  constant = NULL, fun = NULL, ci_col = "black", smooth_col = "black",
  xlab = NULL, ylab = NULL, title = NULL, subtitle = NULL, caption = NULL,
  angle = NULL, ylim = NULL, grouped_by = FALSE, ...
) {
  mrf_plot <- match.arg(mrf_plot)
  if (is.null(variables)) variables <- vars_from_label(unique(object$.smooth))
  by_var <- unique(object$.by)
  grouped <- isTRUE(grouped_by) && length(by_var) == 1L &&
    !is.na(by_var) && is.factor(object[[by_var]])
  if (grouped) {
    if (is.null(title)) title <- sub(":.*$", "", unique(object$.smooth)[1L])
    if (is.null(subtitle)) subtitle <- paste0("By: ", by_var)
  }
  if (mrf_plot == "qq") {
    # Reuse the existing descriptive QQ display without changing MRF identity.
    object <- object[is.finite(object$.estimate), , drop = FALSE]
    if (!nrow(object)) cli::cli_abort("No finite MRF effects are available for a QQ plot.")
    qq_line <- isTRUE(qq_line) && nrow(object) > 1L
    plt <- plot_smooth.random_effect(object, variables = variables,
      qq_line = qq_line && !grouped, constant = constant, fun = fun,
      xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
      caption = caption, angle = angle, ylim = ylim)
    if (grouped) {
      plt <- plt + ggplot2::aes(group = .data[[by_var]])
      if (isTRUE(qq_line)) plt <- plt + ggplot2::geom_qq_line()
    }
  } else {
    object <- add_constant(object, constant = constant)
    object <- transform_fun(object, fun = fun)
    object <- order_interval_bounds(object)
    plt <- ggplot2::ggplot(object,
      ggplot2::aes(x = .data[[variables]], y = .data$.estimate)) +
      ggplot2::geom_linerange(ggplot2::aes(ymin = .data$.lower_ci,
        ymax = .data$.upper_ci), colour = ci_col) +
      ggplot2::geom_point(colour = smooth_col) +
      ggplot2::guides(x = ggplot2::guide_axis(angle = angle)) +
      prepare_smooth_labels(object, x_var = variables, xlab = xlab,
        ylab = ylab, title = title, subtitle = subtitle, caption = caption,
        grouped_by = grouped)
    if (!is.null(ylim)) plt <- plt + ggplot2::expand_limits(y = ylim)
  }
  if (grouped) plt <- plt + ggplot2::facet_wrap(ggplot2::vars(.data[[by_var]]))
  plt
}
