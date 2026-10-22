# Draw coefficient estimates at their stored time steps, without adding an
# evaluation grid or treating prediction rows as new observations.
draw_time_varying_coefficient <- function(
  object, info = NULL, style = c("ribbon", "pointrange"),
  ci_level = 0.95, ci_col = "black", ci_alpha = 0.2, line_col = "black",
  constant = NULL, fun = NULL, xlab = NULL, ylab = NULL,
  title = NULL, subtitle = NULL, caption = NULL, ylim = NULL, angle = NULL
) {
  style <- match.arg(style)
  if (!all(c(".lower_ci", ".upper_ci") %in% names(object))) {
    critical <- coverage_normal(ci_level)
    object$.lower_ci <- object$.partial - critical * object$.se
    object$.upper_ci <- object$.partial + critical * object$.se
  }
  object <- add_constant(object, constant = constant,
    column = c(".partial", ".lower_ci", ".upper_ci"))
  object <- transform_fun(object, fun = fun,
    column = c(".partial", ".lower_ci", ".upper_ci"))
  object <- object[order(object$.value), , drop = FALSE]
  plt <- gratia_plot(object,
    ggplot2::aes(x = .data$.value, y = .data$.partial, group = 1))
  if (style == "ribbon" && nrow(object) > 1L) {
    plt <- plt + geom_gratia_interval_ribbon(ggplot2::aes(
      ymin = .data$.lower_ci, ymax = .data$.upper_ci),
      alpha = ci_alpha, fill = ci_col, colour = NA) +
      geom_gratia_effect_line(colour = line_col) +
      gratia_geom("effect", ggplot2::geom_point(size = 1.25, colour = line_col))
  } else {
    if (nrow(object) > 1L) {
      plt <- plt + geom_gratia_effect_line(colour = line_col,
        alpha = 0.5, linewidth = 0.35)
    }
    plt <- plt + gratia_geom("interval", ggplot2::geom_linerange(ggplot2::aes(
      ymin = .data$.lower_ci, ymax = .data$.upper_ci),
      colour = ci_col, linewidth = 0.4)) +
      gratia_geom("effect", ggplot2::geom_point(size = 1.8, colour = line_col))
  }
  if (is.null(xlab)) xlab <- if (!is.null(info$time)) info$time else "Time"
  if (is.null(ylab)) ylab <- "Coefficient"
  if (is.null(title)) title <- unique(object$.term)
  if (is.null(subtitle)) subtitle <- info$model_label
  if (identical(caption, FALSE)) caption <- NULL
  plt <- plt + ggplot2::labs(x = xlab, y = ylab, title = title,
    subtitle = subtitle, caption = caption) +
    ggplot2::guides(x = ggplot2::guide_axis(angle = angle))
  if (!is.null(ylim)) plt <- plt + ggplot2::expand_limits(y = ylim)
  plt
}
