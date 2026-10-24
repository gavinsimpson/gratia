#' Generic plotting via `ggplot2`
#'
#' Generic function for plotting of R objects that uses the `ggplot2` package.
#'
#' @title Generic plotting via `ggplot2`
#' @param object and R object to plot.
#' @param ... arguments passed to other methods.
#' @param label_control Optional layout settings created by
#'   [draw_label_control()]. These apply across drawing methods without
#'   changing their default appearance.
#'
#' @section Label layout:
#' Pass `label_control = draw_label_control(...)` to `draw()` to wrap discrete
#' axis labels and legend text, stagger discrete tick labels, or adjust legend
#' and colour-bar layout. Settings apply to each panel, including unwrapped
#' plot lists. See [draw_label_control()] for the available settings.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @author Gavin L. Simpson
#'
#' @export
`draw` <- function(object, ..., label_control = NULL) {
  if (!is.null(label_control)) {
    control <- validate_draw_label_control(label_control)
    value <- withVisible(draw(object, ...))
    result <- apply_draw_label_control(value$value, control)
    if (!value$visible) return(invisible(result))
    return(result)
  }
  UseMethod("draw")
}

#' Plot derivatives of smooths
#'
#' @param alpha Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @param add_change logical; should the periods of significant change be
#'   highlighted on the plot?
#' @param change_type character; the type of change to indicate. If `"change"`,
#'   no differentiation is made between periods of significant increase or
#'   decrease. If `"sizer"`, the periods of increase and decrease are
#'   differentiated in the resulting plot.
#' @param change_col,decrease_col,increase_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Colour specifications to use for
#'   indicating periods of change. `col_change` is used when
#'   `change_type = "change"`, while `col_decrease` and `col_increase` are used
#'   when `change_type = "sizer"``.
#' @param lwd_change Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Numeric; the `linewidth` to use for the change indicators.
#' @param differentiate_factor_smooths logical; should colour be used to
#'   differentiate the levels of the factors involved in  random factor smooths
#'   (`bs = "fs"`) or constrained factor smooths (`bs = "sz"`)? If `TRUE`, the
#'   factor(s) are used to define groups and mapped to the colour aesthetic.
#'
#' @inheritParams draw.gam
#'
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line labs
#' @importFrom stringr str_split
#' @importFrom patchwork wrap_plots
#' @export
#'
#' @examples
#' load_mgcv()
#' # Simulate an additive Gaussian response and fit a GAM
#' dat <- data_sim("eg1", n = 200, scale = 1, seed = 42)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' # Estimate and plot the rate of change of one smooth
#' df <- derivatives(mod, select = "s(x2)", type = "central")
#' draw(df)
#' @inheritSection draw Label layout
`draw.derivatives` <- function(
  object,
  select = NULL,
  scales = c("free", "fixed"),
  add_change = FALSE,
  change_type = c("change", "sizer"),
  alpha = 0.2,
  change_col = "black",
  decrease_col = "#56B4E9",
  increase_col = "#E69F00",
  lwd_change = 1.5,
  ncol = NULL, nrow = NULL,
  guides = "keep",
  angle = NULL,
  differentiate_factor_smooths = TRUE,
  ...
) {
  tube_check_saved_controls(names(match.call(expand.dots = FALSE)$...))
  gratia_plot_arguments(c("change_col", "decrease_col", "increase_col", "lwd_change", "alpha"), environment())
  scales <- match.arg(scales)
  change_type <- match.arg(change_type)

  if (isTRUE(add_change)) {
    object <- object |>
      add_sizer(type = change_type)
  }

  ## how many smooths
  sm <- unique(object[[".smooth"]])
  ## select smooths
  select <- check_user_select_smooths(smooths = sm, select = select)
  sm <- sm[select]

  plotlist <- vector("list", length = length(sm))

  for (i in seq_along(sm)) {
    take <- object[[".smooth"]] == sm[i]
    df <- object[take, ]
    xvar <- vars_from_label(unique(df[[".smooth"]]))
    group_var <- colour_var <- NULL
    if (!all(is.na(df$.fs))) {
      fs_col_lab <- unique(df$.fs)
      fs_var <- fs_col_lab |> stringr::str_split(":") |> unlist()
      xvar <- xvar[!xvar %in% fs_var]
      if (length(fs_var) > 1L) {
        df <- df |> mutate(
          ..fs_interact.. = do.call("interaction", args = df[fs_var]))
        fs_var <- "..fs_interact.."
      }
      group_var <- fs_var
      if (differentiate_factor_smooths) {
        colour_var <- fs_var
      }
    }
    labels <- labs(title = sm[i], x = xvar, y = "Derivative")
    if (!is.null(colour_var)) labels$colour <- fs_col_lab
    plt <- prepare_derivative_plot(df, x_var = xvar, y_var = ".derivative",
      group_var = group_var, colour_var = colour_var, labels = labels,
      interval = all(is.na(df$.fs)), alpha = alpha, angle = angle)
    if (isTRUE(add_change)) {
      plt <- if (identical(change_type, "change")) {
        plt +
          geom_gratia_change_line(aes(x = .data[[xvar]], y = .data$.change),
            linewidth = lwd_change, na.rm = TRUE,
            colour = change_col
          )
      } else {
        plt +
          geom_gratia_increase_line(aes(x = .data[[xvar]], y = .data$.increase),
            colour = increase_col, linewidth = lwd_change,
            na.rm = TRUE
          ) +
          geom_gratia_decrease_line(aes(x = .data[[xvar]], y = .data$.decrease),
            colour = decrease_col, linewidth = lwd_change,
            na.rm = TRUE
          )
      }
    }
    plotlist[[i]] <- plt
  }

  wrap_interval_plots(plotlist, object, scales = scales,
    ncol = ncol, nrow = nrow, guides = guides, ...)
}

#' @export
#' @rdname draw.derivatives
`draw.partial_derivatives` <- function(object,
                                       select = NULL,
                                       scales = c("free", "fixed"), alpha = 0.2,
                                       ncol = NULL, nrow = NULL,
                                       guides = "keep",
                                       angle = NULL,
                                       ...) {
  gratia_plot_arguments(c("alpha"), environment())
  scales <- match.arg(scales)

  ## how many smooths
  sm <- unique(object[[".smooth"]])
  ## select smooths
  select <- check_user_select_smooths(smooths = sm, select = select)
  sm <- sm[select]

  plotlist <- vector("list", length = length(sm))

  for (i in seq_along(sm)) {
    take <- object[[".smooth"]] == sm[i]
    df <- object[take, ]
    xvar <- unique(df[[".focal"]])
    plotlist[[i]] <- prepare_derivative_plot(df,
      x_var = xvar, y_var = ".partial_deriv",
      group_var = if (!all(is.na(df$.fs))) ".fs" else NULL,
      interval = all(is.na(df$.fs)), alpha = alpha, angle = angle,
      labels = labs(title = sm[i], x = xvar,
        y = paste("Partial derivative with respect to", xvar)))
  }

  wrap_interval_plots(plotlist, object, scales = scales,
    ncol = ncol, nrow = nrow, guides = guides, ...)
}

#' Plot basis functions
#'
#' Plots basis functions using ggplot2
#'
#' @param object an object, the result of a call to [basis()].
#' @param legend logical; should a legend by drawn to indicate basis functions?
#' @param use_facets logical; for factor by smooths, use facets to show the
#'   basis functions for each level of the factor? If `FALSE`, a separate ggplot
#'   object will be created for each level and combined using
#'   [patchwork::wrap_plots()]. **Currently ignored**.
#' @param labeller a labeller function with which to label facets. The default
#'   is to use [ggplot2::label_both()].
#' @param xlab character or expression; the label for the x axis. If not
#'   supplied, a suitable label will be generated from `object`.
#' @param ylab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated from `object`.
#' @param title character or expression; the title for the plot. See
#'   [ggplot2::labs()].
#' @param subtitle character or expression; the subtitle for the plot. See
#'   [ggplot2::labs()].
#' @param caption character or expression; the plot caption. See
#'   [ggplot2::labs()].
#' @param angle numeric; the angle at which the x axis tick labels are to be
#'   drawn passed to the `angle` argument of [ggplot2::guide_axis()].
#' @param ... arguments passed to other methods. Not used by this method.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom ggplot2 ggplot aes labs geom_line guides facet_wrap label_both
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' df <- data_sim("eg4", n = 400, seed = 42)
#'
#' bf <- basis(s(x0), data = df)
#' draw(bf)
#'
#' bf <- basis(s(x2, by = fac, bs = "bs"), data = df)
#' draw(bf)
#' @inheritSection draw Label layout
`draw.mgcv_smooth` <- function(object,
                               legend = FALSE,
                               use_facets = TRUE,
                               labeller = NULL,
                               xlab, ylab,
                               title = NULL, subtitle = NULL,
                               caption = NULL,
                               angle = NULL,
                               ...) {
  ## capture the univariate smooth variable
  smooth_var <- names(object)[5L]

  ## default labeller
  if (is.null(labeller)) {
    labeller <- label_both
  }

  ## basis plot
  plt <- gratia_plot(object, aes(
    x = .data[[smooth_var]],
    y = .data[["value"]],
    colour = .data[["bf"]]
  )) +
    geom_gratia_basis_line() +
    guides(x = guide_axis(angle = angle))

  ## default labels if none supplied
  if (missing(xlab)) {
    xlab <- smooth_var
  }
  if (missing(ylab)) {
    ylab <- "Value"
  }
  if (is.null(title)) {
    title <- attr(object, "smooth_object")
    # if still null then this came from a model & we don't have the call
    if (is.null(title)) {
      title <- unique(object[["smooth"]])
    }
  }

  ## fixup for by variable smooths, facet for factor by smooths
  if (all(!is.na(object[["by_variable"]]))) {
    by_var_name <- unique(object[["by_variable"]])
    by_var <- object[[by_var_name]]
    if (is.character(by_var) || is.factor(by_var)) {
      plt <- plt + facet_wrap(by_var_name, labeller = labeller)
    }
  }

  ## add labelling to plot
  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption, colour = "Basis\nfunction"
  )

  ## draw a guide?
  if (!legend) {
    plt <- plt + guides(colour = "none")
  }

  plt
}

#' Plot basis functions
#'
#' Plots basis functions using ggplot2
#'
#' @param object an object, the result of a call to [basis()].
#' @param legend logical; should a legend by drawn to indicate basis functions?
#' @param labeller a labeller function with which to label facets. The default
#'   is to use [ggplot2::label_both()].
#' @param ylab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated from `object`.
#' @param title character or expression; the title for the plot. See
#'   [ggplot2::labs()].
#' @param subtitle character or expression; the subtitle for the plot. See
#'   [ggplot2::labs()].
#' @param caption character or expression; the plot caption. See
#'   [ggplot2::labs()].
#' @param angle numeric; the angle at which the x axis tick labels are to be
#'   drawn passed to the `angle` argument of [ggplot2::guide_axis()].
#' @param ... arguments passed to other methods. Not used by this method.
#'
#' @inheritParams draw.gam
#' @inheritParams basis
#'
#' @details
#' For smooths of three or four covariates, each basis function has its own
#' plot. The first two covariates define the surface axes. Three-covariate
#' smooths have wrapped facets for the third covariate; four-covariate smooths
#' have the third covariate in rows and the fourth in columns. `ncol` and `nrow`
#' control the outer arrangement of plots. Fill scales share a range within
#' each smooth. Use `variables` in [basis()] to choose the axes before grid
#' generation, or here to rearrange existing evaluations without resampling.
#' Surface plots include contour lines by default, matching [draw.gam()].
#' Use `contour = FALSE` to draw surfaces without contour lines.
#'
#' Factor-by bases from smooth specifications are drawn once for the whole
#' factor-by set, using evaluations from all available levels. For fitted
#' models, `basis(model)` returns coefficient-weighted contributions, which
#' remain separate for each level. Use `basis(model, unweighted = TRUE)` to
#' draw the shared basis once. Subsetting the basis before drawing restricts
#' the plot to the retained evaluations and basis functions.
#'
#' @return A `patchwork` object.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom purrr map map_lgl
#' @importFrom patchwork wrap_plots
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' # Simulate an additive Gaussian response and fit a GAM
#' df <- data_sim("eg1", n = 200, seed = 42)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' # Plot the coefficient-weighted basis functions for one smooth
#' bf <- basis(m, select = "s(x2)")
#' draw(bf)
#' @inheritSection draw Label layout
`draw.basis` <- function(object,
                         legend = FALSE,
                         labeller = NULL,
                         ylab = NULL,
                         title = NULL, subtitle = NULL,
                         caption = NULL,
                         ncol = NULL, nrow = NULL,
                         angle = NULL,
                         guides = "keep",
                         contour = TRUE,
                         n_contour = 10,
                         contour_col = "black",
                         ..., variables = NULL) {
  gratia_plot_arguments(c("contour_col"), environment())
  sm_l <- basis_plot_groups(object)
  sm_plts <- map(sm_l,
    plot_basis,
    legend = legend,
    labeller = labeller,
    ylab = ylab,
    title = title,
    subtitle = subtitle,
    caption = caption,
    angle = angle,
    contour = contour,
    n_contour = n_contour,
    contour_col = contour_col,
    ..., variables = variables
  )

  # filter out NULLs as those are types of smooths we can't plot (yet)
  no_plot <- map_lgl(sm_plts, is.null)
  sm_plts <- sm_plts[!no_plot]

  if (all(no_plot)) {
    message("Unable to draw any of the bases.")
    return(invisible())
  }

  # Higher-dimensional bases return one faceted plot per basis function.
  sm_plts <- do.call(c, lapply(sm_plts, function(x) {
    if (inherits(x, "ggplot")) list(x) else x
  }))

  # return
  n_plots <- length(sm_plts)
  layout <- prepare_plot_layout(n_plots, ncol = ncol, nrow = nrow)
  ncol <- layout$ncol
  nrow <- layout$nrow
  wrap_plots(sm_plts,
    byrow = TRUE, ncol = ncol, nrow = nrow,
    guides = guides, ...
  )
}

# Combine only bases marked as shared when they were constructed. Older
# objects and fitted coefficient-weighted bases keep their separate smooths.
basis_plot_groups <- function(object) {
  labels <- unique(object$.smooth)
  groups <- stats::setNames(labels, labels)
  shared <- attr(object, "factor_by_groups")
  take <- intersect(labels, names(shared))
  groups[take] <- shared[take]
  lapply(unique(unname(groups)), function(group) {
    members <- names(groups)[groups == group]
    data <- object[object$.smooth %in% members, ]
    if (any(members %in% take)) {
      order <- basis_plot_variables(object[object$.smooth == members[1L], ])
      by_var <- unique(data$.by)
      data[by_var] <- NULL
      data$.by <- NA_character_
      data$.smooth <- group
      data <- unique(data)
      attr(data, "plot_variables") <- stats::setNames(list(order), group)
      attr(data, "factor_by_groups") <- NULL
    }
    data
  })
}

#' @importFrom ggplot2 ggplot aes labs geom_line guides facet_wrap label_both
`plot_basis` <- function(object,
                         legend = FALSE,
                         labeller = NULL,
                         xlab = NULL, ylab = NULL,
                         title = NULL, subtitle = NULL,
                         caption = NULL,
                         angle = NULL,
                         contour = TRUE,
                         n_contour = 10,
                         contour_col = "black",
                         ..., variables = NULL) {
  smooth_var <- basis_plot_variables(object, variables)
  if (is.null(title)) title <- unique(object$.smooth)
  n_sms <- length(smooth_var)

  plt <- switch(n_sms,
    plot_univariate_basis(object,
      legend = legend,
      labeller = labeller, xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle, caption = caption,
      angle = angle, ..., variables = smooth_var
    ),
    plot_bivariate_basis(object,
      legend = legend,
      labeller = labeller, xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle, caption = caption,
      angle = angle, contour = contour, n_contour = n_contour,
      contour_col = contour_col, ..., variables = smooth_var
    ),
    plot_multivariate_basis(object, variables = smooth_var,
      legend = legend, labeller = labeller, xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle, caption = caption,
      angle = angle, contour = contour, n_contour = n_contour,
      contour_col = contour_col, ...),
    plot_multivariate_basis(object, variables = smooth_var,
      legend = legend, labeller = labeller, xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle, caption = caption,
      angle = angle, contour = contour, n_contour = n_contour,
      contour_col = contour_col, ...)
  )

  plt
}

`plot_univariate_basis` <- function(object,
                                    legend = FALSE,
                                    labeller = NULL,
                                    xlab = NULL, ylab = NULL,
                                    title = NULL, subtitle = NULL,
                                    caption = NULL,
                                    angle = NULL,
                                    ..., variables = NULL) {
  smooth_var <- basis_plot_variables(object, variables)
  plt <- add_curve_interval(gratia_plot(object,
    aes(x = .data[[smooth_var]], y = .data[[".value"]],
      colour = .data[[".bf"]]))) +
    guides(x = guide_axis(angle = angle)) +
    prepare_basis_facet(object, smooth_var, labeller = labeller) +
    prepare_basis_labels(object, smooth_var, xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle, caption = caption)
  if (!legend) {
    plt <- plt + guides(colour = "none")
  }
  plt

}

#' @importFrom ggplot2 scale_fill_distiller guides guide_axis facet_wrap vars labs geom_contour coord_equal
`plot_bivariate_basis` <- function(object,
                                   legend = FALSE,
                                   labeller = NULL,
                                   xlab = NULL, ylab = NULL,
                                   title = NULL, subtitle = NULL,
                                   caption = NULL,
                                   angle = NULL,
                                   contour = TRUE,
                                   n_contour = 10,
                                   contour_col = "black",
                                   ..., variables = NULL, fill_limits = NULL) {
  smooth_var <- basis_plot_variables(object, variables)
  coord <- if (str_detect(object[[".type"]][1L], "TPRS")) coord_equal() else NULL
  plt <- prepare_surface_plot(object,
    x_var = smooth_var[1], y_var = smooth_var[2], fill_var = ".value",
    fill_limits = fill_limits,
    mapping = aes(x = .data[[smooth_var[1]]], y = .data[[smooth_var[2]]],
      fill = .data[[".value"]], group = .data[[".bf"]]),
    labels = prepare_basis_labels(object, smooth_var, xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle, caption = caption),
    facet = prepare_basis_facet(object, smooth_var, labeller = labeller),
    coord = coord, contour = contour, contour_col = contour_col,
    n_contour = n_contour,
    contour_stat = StatBasisContour,
    contour_mapping = aes(z = .data[[".value"]],
      group = .data[[".bf"]], fill = NULL),
    axis_guide = guide_axis(angle = angle, check.overlap = TRUE),
    fill_guide = NULL, legend_position = NULL)
  if (!legend) {
    plt <- plt + guides(fill = "none")
  }
  plt

}

# A basis slice may be constant or contain none of the shared contour levels.
# Keep ggplot2's contour calculation and levels, but allow an empty result.
StatBasisContour <- ggplot2::ggproto("StatBasisContour", ggplot2::StatContour,
  compute_group = function(data, scales, z.range, bins = NULL,
    binwidth = NULL, breaks = NULL, na.rm = FALSE) {
    values <- data$z[is.finite(data$z)]
    if (length(values) < 2L ||
        diff(range(values)) <= sqrt(.Machine$double.eps) * max(abs(values))) {
      return(data.frame())
    }
    empty <- FALSE
    withCallingHandlers(
      ggplot2::StatContour$compute_group(data, scales, z.range = z.range,
        bins = bins, binwidth = binwidth, breaks = breaks, na.rm = na.rm),
      warning = function(cnd) {
        msg <- conditionMessage(cnd)
        if (grepl("Zero contours were generated", msg, fixed = TRUE)) {
          empty <<- TRUE
          invokeRestart("muffleWarning")
        }
        if (empty && msg %in% c(
          "no non-missing arguments to min; returning Inf",
          "no non-missing arguments to max; returning -Inf")) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
)

# Resolve saved orders before falling back to labels for older basis objects.
basis_plot_variables <- function(object, variables = NULL) {
  label <- unique(object$.smooth)
  order <- attr(object, "plot_variables")[[label]]
  if (is.null(order)) order <- vars_from_label(label)
  resolved <- validate_plot_variables(variables, order)
  if (!is.null(variables)) validate_plot_variable_types(resolved, object)
  if (identical(unique(object$.type), "SOS") &&
      !identical(unname(resolved), unname(order))) {
    cli::cli_abort("`variables` cannot reorder latitude and longitude in a spline on the sphere.")
  }
  resolved
}

# Each basis function has its own conditioning facets and a shared fill range.
plot_multivariate_basis <- function(object, variables, subtitle = NULL, ...) {
  finite <- object$.value[is.finite(object$.value)]
  limits <- if (length(finite)) range(finite) else NULL
  bfs <- levels(object$.bf)
  bfs <- bfs[bfs %in% unique(object$.bf)]
  lapply(bfs, function(bf) {
    data <- object[object$.bf == bf, ]
    plot_bivariate_basis(data, variables = variables, fill_limits = limits,
      subtitle = if (is.null(subtitle)) paste("Basis function", bf) else subtitle,
      ...)
  })
}

#' Plot posterior smooths
#'
#' @param n_samples numeric; if not `NULL`, sample `n_samples` from the
#'   posterior draws for plotting.
#' @param seed numeric; random seed to be used to if sampling draws.
#' @param alpha Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @param colour Deprecated; use [gratia_theme_get()] theme
#'   settings instead. The colour to use to draw the posterior smooths. Passed to
#'   [ggplot2::geom_line()] as argument `colour`.
#' @param contour logical; should contour lines be added to smooth surfaces?
#' @param xlab character or expression; the label for the x axis. If not
#'   supplied, a suitable label will be generated from `object`.
#' @param ylab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated from `object`.
#' @param title character or expression; the title for the plot. See
#'   [ggplot2::labs()].
#' @param subtitle character or expression; the subtitle for the plot. See
#'   [ggplot2::labs()].
#' @param caption character or expression; the plot caption. See
#'   [ggplot2::labs()].
#' @param angle numeric; the angle at which the x axis tick labels are to be
#'   drawn passed to the `angle` argument of [ggplot2::guide_axis()].
#' @param ... arguments to be passed to [patchwork::wrap_plots()].
#'
#' @export
#'
#' @inheritParams draw.gam
#'
#' @author Gavin L. Simpson
#'
#' @importFrom dplyr filter
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom patchwork wrap_plots
#' @importFrom vctrs vec_slice
#'
#' @examples
#' load_mgcv()
#' # Simulate an additive Gaussian response and fit a GAM
#' dat <- data_sim("eg1", n = 200, scale = 1, seed = 1)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' # Draw 15 possible curves for the smooth of x2 on a 100-point grid
#' sm <- smooth_samples(m, select = "s(x2)", n_grid = 100,
#'   n_draws = 15, seed = 23478)
#' draw(sm)
#' @inheritSection draw Label layout
`draw.smooth_samples` <- function(object,
                                  select = NULL,
                                  n_samples = NULL, seed = NULL,
                                  xlab = NULL, ylab = NULL, title = NULL,
                                  subtitle = NULL, caption = NULL,
                                  alpha = 1, colour = "black",
                                  contour = FALSE,
                                  contour_col = "black",
                                  n_contour = NULL,
                                  scales = c("free", "fixed"),
                                  rug = TRUE,
                                  partial_match = FALSE,
                                  angle = NULL,
                                  ncol = NULL, nrow = NULL,
                                  guides = "keep", ...) {
  gratia_plot_arguments(c("contour_col", "alpha", "colour"), environment())
  scales <- match.arg(scales)

  ## select smooths
  S <- unique(object[[".term"]])
  select <- check_user_select_smooths(
    smooths = S, select = select,
    partial_match = partial_match
  )
  S <- S[select]
  object <- vec_slice(object, object[[".term"]] %in% S)

  ## can only plot 1d smooths - currently - prune S but how?
  ## FIXME

  do_plot_smooths <- function(i, object, ...) {
    object <- vec_slice(object, object[[".term"]] == i)
    draw_posterior_smooths(object, ...)
  }

  plts <- map(S, do_plot_smooths,
    object = object, n_samples = n_samples, seed = seed,
    xlab = xlab, ylab = ylab,
    title = title, subtitle = subtitle, caption = caption,
    rug = rug, alpha = alpha, colour = colour,
    contour = contour, n_contour = n_contour,
    contour_col = contour_col, angle = angle
  )

  if (isTRUE(identical(scales, "fixed"))) {
    ylims <- range(object[[".value"]])

    p <- seq_along(plts)
    for (i in p) {
      plts[[i]] <- plts[[i]] + lims(y = ylims)
    }
  }

  ## return
  n_plots <- length(plts)
  layout <- prepare_plot_layout(n_plots, ncol = ncol, nrow = nrow)
  ncol <- layout$ncol
  nrow <- layout$nrow
  wrap_plots(plts,
    byrow = TRUE, ncol = ncol, nrow = nrow, guides = guides,
    ...
  )
}

#' @importFrom vctrs vec_slice
`draw_posterior_smooths` <- function(object, n_samples = NULL, seed = NULL,
                                     xlab = NULL, ylab = NULL,
                                     title = NULL, subtitle = NULL,
                                     caption = NULL, rug = TRUE, alpha = 1,
                                     colour = "black",
                                     contour = FALSE,
                                     contour_col = "black",
                                     n_contour = NULL, angle = NULL, ...) {
  # handle the seed nicely
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  xvars <- unique(object[[".term"]])
  xvars <- vars_from_label(xvars)
  n_xvars <- length(xvars)

  ## randomly sample n_samples from the posterior draws
  n_draws <- max(object[[".draw"]]) # how many draws?
  max_plts <- 16L

  # if n_samples null, user didn't specify how many, set to maximum
  if (is.null(n_samples)) {
    n_samples <- if (n_xvars > 1L) {
      max_plts
    } else {
      n_draws
    }
  }
  # if n_samples exceeds the number of draws, set it to be the maximum
  # this is really only if a user set it too large. Also handle case of user
  # being silly and asking for < 1 draw
  if (n_draws < n_samples || n_samples < 1L) {
    n_samples <- n_draws
  }

  # if we're plotting fewer than n_draws samples, randomly sample the draws
  # to plot
  if (n_samples < n_draws) {
    draws <- unique(object[[".draw"]])
    draws <- sample(draws, n_samples)
    object <- vec_slice(object, object[[".draw"]] %in% draws)
  }

  plt <- if (identical(n_xvars, 1L)) {
    draw_1d_posterior_smooths(object,
      rug = rug,
      alpha = alpha, colour = colour,
      xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle,
      caption = caption, angle = angle, ...
    )
  } else if (identical(n_xvars, 2L)) {
    draw_2d_posterior_smooths(object,
      contour = contour,
      contour_col = contour_col,
      n_contour = n_contour,
      xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle,
      caption = caption, angle = angle, ...
    )
  } else if (identical(n_xvars, 3L)) {
    draw_3d_posterior_smooths(object,
      contour = contour,
      contour_col = contour_col,
      n_contour = n_contour,
      xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle,
      caption = caption, angle = angle, ...
    )
  } else {
    message("Can't plot samples of smooths of more than 3 variables.")
    NULL
  }

  plt # return
}

#' @importFrom rlang .data
#' @importFrom dplyr distinct
`draw_1d_posterior_smooths` <- function(object, xlab = NULL, ylab = NULL,
                                        title = NULL, subtitle = NULL,
                                        caption = NULL, rug = TRUE, alpha = 1,
                                        colour = "black", angle = NULL) {
  smooth_var <- vars_from_label(object[1L, ".smooth"])

  plt <- add_curve_interval(
    gratia_plot(object, aes(x = .data[[smooth_var]], y = .data[[".value"]],
      group = .data[[".draw"]])),
    line_alpha = alpha, line_colour = colour, curve_role = "sample") +
    guides(x = guide_axis(angle = angle))

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- smooth_var
  }
  if (is.null(ylab)) {
    ylab <- "Partial effect"
  }
  if (is.null(title)) {
    title <- unique(object[[".term"]])
  }
  if (all(!is.na(object[[".by"]]))) {
    spl <- strsplit(title, split = ":")
    title <- spl[[1L]][[1L]]
    if (is.null(subtitle)) {
      by_var <- as.character(unique(object[[".by"]]))
      subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
    }
  }

  ## add labelling to plot
  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

  ## add rug?
  if (isTRUE(rug)) {
    plt <- plt + geom_gratia_rug(
      data = distinct(object, .data[[smooth_var]]),
      mapping = aes(x = .data[[smooth_var]]),
      inherit.aes = FALSE, sides = "b", alpha = 0.5
    )
  }

  plt
}


#' @importFrom ggplot2 ggplot guides aes geom_raster geom_contour labs
#'   scale_fill_distiller guide_colourbar
#' @importFrom grid unit
`draw_2d_posterior_smooths` <- function(object,
                                        contour = FALSE,
                                        contour_col = "black",
                                        n_contour = NULL,
                                        xlab = NULL,
                                        ylab = NULL,
                                        title = NULL,
                                        subtitle = NULL,
                                        caption = NULL,
                                        angle = NULL) {
  xvars <- unique(object[[".term"]])
  xvars <- vars_from_label(xvars)

  if (is.null(xlab)) {
    xlab <- xvars[1]
  }
  if (is.null(ylab)) {
    ylab <- xvars[2]
  }
  if (is.null(title)) {
    sm_label <- unique(object$.smooth)
    by_var <- unique(object$.by)
    ## fix this so it knows about the level
    title <- if (is.na(by_var)) {
      sm_label
    } else {
      mgcv_by_smooth_labels(sm_label, by_var, level = "")
    }
  }

  coord <- if (any(str_detect(unique(object$.type),
    c("^TPRS", "^Duchon", "^GP", "^SOS")))) coord_equal() else NULL
  prepare_surface_plot(object,
    x_var = xvars[1], y_var = xvars[2], fill_var = ".value",
    fill_limits = c(-1, 1) * max(abs(object[[".value"]])),
    fill_title = "Partial effect",
    labels = labs(title = title, x = xlab, y = ylab, subtitle = subtitle,
      caption = caption),
    contour = contour, contour_col = contour_col, n_contour = n_contour,
    contour_na_rm = FALSE, angle = angle, facet = facet_wrap(~.draw),
    coord = coord, legend_position = NULL)

}

`draw_3d_posterior_smooths` <- function(object, xvars,
                                        contour = FALSE,
                                        contour_col = "black",
                                        n_contour = NULL,
                                        xlab = NULL,
                                        ylab = NULL,
                                        title = NULL,
                                        subtitle = NULL,
                                        caption = NULL,
                                        angle = NULL) {
  warning("Plotting samples of 3D smooths is not yet implemented")
  return(NULL)
}

#' Plot differences of smooths
#'
#' @param rug logical;
#' @param ref_line logical;
#' @param contour logical; should contour lines be added to smooth surfaces?
#' @param ci_alpha Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @param ci_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Colour specification for the confidence/credible intervals
#'   band. Affects the fill of the interval.
#' @param smooth_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Colour specification for the the smooth or difference line.
#' @param line_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Colour specification for drawing reference lines
#' @param ncol,nrow numeric; the numbers of rows and columns over which to
#'   spread the plots
#' @param xlab,ylab,title,subtitle,caption character; labels with which to
#'   annotate plots
#' @param guides character; one of `"keep"` (the default), `"collect"`, or
#'   `"auto"`. Passed to [patchwork::plot_layout()]
#' @param variables character vector; every covariate of the selected smooth,
#'   in plotting order. The first two are the x and y axes; the third defines
#'   facets for three covariates, or facet rows for four covariates; the fourth
#'   defines facet columns. `NULL` uses the order saved with the estimates.
#'   Drawing rearranges evaluated points without generating a new grid.
#' @inheritParams draw.gam
#'
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line labs lims
#' @importFrom patchwork wrap_plots
#' @importFrom purrr map
#' @export
#'
#' @examples
#'
#' load_mgcv()
#' # simulate some data; a factor smooth example
#' df <- data_sim("eg4", seed = 42)
#' # fit GAM
#' m <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")
#'
#' # calculate the differences between pairs of smooths the f_j(x2) term
#' diffs <- difference_smooths(m, select = "s(x2)")
#' draw(diffs)
#' @inheritSection draw Label layout
`draw.difference_smooth` <- function(object,
                                     select = NULL,
                                     rug = FALSE,
                                     ref_line = FALSE,
                                     contour = FALSE,
                                     contour_col = "black",
                                     n_contour = NULL,
                                     ci_alpha = 0.2,
                                     ci_col = "black",
                                     smooth_col = "black",
                                     line_col = "red",
                                     scales = c("free", "fixed"),
                                     ncol = NULL, nrow = NULL,
                                     guides = "keep",
                                     xlab = NULL,
                                     ylab = NULL,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL,
                                     angle = NULL, ...,
                                     variables = NULL) {
  tube_check_saved_controls(names(match.call(expand.dots = FALSE)$...))
  gratia_plot_arguments(c("contour_col", "ci_alpha", "ci_col", "smooth_col", "line_col"), environment())
  scales <- match.arg(scales)

  ## how many smooths
  sm <- unique(object[[".smooth"]])
  ## select smooths
  select <- check_user_select_smooths(smooths = sm, select = select)
  sm <- sm[select]

  orders <- attr(object, "plot_variables")
  tensor_orders <- attr(object, "tensor_term_order")
  object <- object[object$.smooth %in% sm, , drop = FALSE]
  attr(object, "plot_variables") <- orders
  attr(object, "tensor_term_order") <- tensor_orders
  if (!is.null(variables)) {
    for (label in sm) {
      validate_plot_variables(variables, difference_plot_variables(object, label))
    }
  }
  # Match comparison tuples without joining factor labels into strings.
  comparison <- vctrs::vec_group_id(object[c(".smooth", ".level_1", ".level_2")])
  df_list <- split(object, f = factor(comparison, levels = unique(comparison)))
  df_list <- lapply(df_list, function(x) {
    attr(x, "plot_variables") <- orders
    attr(x, "tensor_term_order") <- tensor_orders
    x
  })
  dimensions <- vapply(df_list, function(x) {
    label <- unique(x$.smooth)
    length(difference_plot_variables(x, label))
  }, integer(1L))
  surface_limits <- if (scales == "fixed" && any(dimensions > 1L)) {
    values <- unlist(lapply(df_list[dimensions > 1L], `[[`, ".diff"))
    c(-1, 1) * max(c(0, abs(values[is.finite(values)])))
  } else {
    NULL
  }

  plotlist <- map(df_list, draw_difference,
    ci_alpha = ci_alpha,
    smooth_col = smooth_col,
    line_col = line_col,
    rug = rug,
    ref_line = ref_line,
    ci_col = ci_col,
    contour = contour,
    contour_col = contour_col,
    n_contour = n_contour,
    xlab = xlab, ylab = ylab, title = title,
    subtitle = subtitle, caption = caption, angle = angle,
    variables = variables, fill_limits = surface_limits
  )

  if (scales == "fixed" && any(dimensions == 1L)) {
    curves <- bind_rows(df_list[dimensions == 1L])
    limits <- range(curves$.lower_ci, curves$.upper_ci)
    plotlist[dimensions == 1L] <- lapply(plotlist[dimensions == 1L],
      function(p) p + lims(y = limits))
  }
  wrap_interval_plots(plotlist, object, scales = "free",
    ncol = ncol, nrow = nrow, guides = guides, ...)
}

difference_plot_variables <- function(object, label) {
  variables <- attr(object, "plot_variables")[[label]]
  if (is.null(variables)) {
    variables <- attr(object, "tensor_term_order")
    if (is.list(variables)) variables <- variables[[label]]
  }
  if (is.null(variables)) variables <- vars_from_label(label)
  variables
}

difference_value_label <- function(object) {
  info <- attr(object, "derivative_info", exact = TRUE)
  if (is.null(info)) return("Difference")
  paste0(if (info$order == 1L) "First" else "Second",
    " derivative of difference\nwith respect to ", info$focal)
}

`draw_difference` <- function(object,
                              rug = NULL,
                              ref_line = NULL,
                              contour = NULL,
                              ci_alpha = NULL,
                              ci_col = NULL,
                              smooth_col = NULL,
                              line_col = NULL,
                              contour_col = "black",
                              n_contour = NULL,
                              xlab = NULL, ylab = NULL,
                              title = NULL, subtitle = NULL, caption = NULL,
                              angle = NULL, variables = NULL,
                              fill_limits = NULL) {
  label <- unique(object[[".smooth"]])
  xvars <- validate_plot_variables(variables,
    difference_plot_variables(object, label))
  if (!is.null(variables)) validate_plot_variable_types(xvars, object)
  n_xvars <- length(xvars)
  plt <- if (identical(n_xvars, 1L)) {
    draw_1d_difference(object, xvars,
      rug = rug, ref_line = ref_line,
      ci_alpha = ci_alpha, smooth_col = smooth_col,
      line_col = line_col,
      ci_col = ci_col, xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle,
      caption = caption, angle = angle
    )
  } else if (identical(n_xvars, 2L)) {
    draw_2d_difference(object, xvars,
      contour = contour,
      contour_col = contour_col, n_contour = n_contour,
      xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle,
      caption = caption, angle = angle, fill_limits = fill_limits
    )
  } else if (identical(n_xvars, 3L)) {
    draw_3d_difference(object, xvars,
      contour = contour,
      contour_col = contour_col, n_contour = n_contour,
      xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle,
      caption = caption, angle = angle, fill_limits = fill_limits
    )
  } else if (identical(n_xvars, 4L)) {
    draw_4d_difference(object, xvars,
      contour = contour,
      contour_col = contour_col, n_contour = n_contour,
      xlab = xlab, ylab = ylab,
      title = title, subtitle = subtitle,
      caption = caption, angle = angle, fill_limits = fill_limits
    )
  } else {
    message("Can't plot differences for smooths of more than 4 variables.")
    NULL
  }

  plt # return
}

#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs geom_hline
#'   geom_rug
`draw_1d_difference` <- function(object, xvars,
                                 rug = FALSE,
                                 ref_line = FALSE,
                                 ci_alpha = 0.2,
                                 ci_col = "black",
                                 smooth_col = "black",
                                 line_col = "red",
                                 xlab = NULL,
                                 ylab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL, angle = NULL) {
  sm_label <- unique(object$.smooth)
  by_var <- unique(object$.by)
  f1 <- unique(object$.level_1)
  f2 <- unique(object$.level_2)
  plt_subtitle <- if (is.null(subtitle)) {
    bquote("Comparison:" ~ .(f1) - .(f2))
  } else {
    subtitle
  }
  y_label <- if (is.null(ylab)) {
    difference_value_label(object)
  } else {
    ylab
  }
  plt_title <- if (is.null(title)) {
    paste(sm_label, "by", by_var)
  } else {
    title
  }

  plt <- gratia_plot(object, aes(
    x = .data[[xvars[1L]]],
    y = .data$.diff
  )) +
    guides(x = guide_axis(angle = angle))

  if (isTRUE(ref_line)) {
    plt <- plt + geom_gratia_reference_hline(yintercept = 0, colour = line_col)
  }
  plt <- add_curve_interval(plt,
    lower_var = ".lower_ci", upper_var = ".upper_ci",
    ribbon_mapping = aes(y = NULL), ribbon_alpha = ci_alpha,
    ribbon_fill = ci_col, ribbon_colour = NA, line_colour = smooth_col) +
    labs(
      title = plt_title, x = if (is.null(xlab)) xvars else xlab, y = y_label,
      subtitle = plt_subtitle, caption = caption
    )

  if (isTRUE(rug)) {
    plt <- plt + geom_gratia_rug(sides = "b", alpha = 0.5)
  }
  plt
}

#' @importFrom ggplot2 ggplot guides aes geom_raster geom_contour labs
#'   scale_fill_distiller guide_colourbar
#' @importFrom grid unit
`draw_2d_difference` <- function(object, xvars,
                                 contour = FALSE,
                                 contour_col = "black",
                                 n_contour = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL, angle = NULL,
                                 fill_limits = NULL) {
  if (is.null(xlab)) {
    xlab <- xvars[1]
  }
  if (is.null(ylab)) {
    ylab <- xvars[2]
  }
  sm_label <- unique(object$.smooth)
  by_var <- unique(object$.by)
  f1 <- unique(object$.level_1)
  f2 <- unique(object$.level_2)
  plt_title <- if (is.null(title)) {
    paste(sm_label, "by", by_var)
  } else {
    title
  }
  plt_subtitle <- if (is.null(subtitle)) {
    bquote("Comparison:" ~ .(f1) - .(f2))
  } else {
    subtitle
  }
  if (is.null(fill_limits)) {
    fill_limits <- c(-1, 1) * max(abs(object[[".diff"]]))
  }

  prepare_surface_plot(object,
    x_var = xvars[1], y_var = xvars[2], fill_var = ".diff",
    fill_limits = fill_limits,
    fill_title = difference_value_label(object),
    labels = labs(title = plt_title, x = xlab, y = ylab,
      subtitle = plt_subtitle, caption = caption),
    contour = contour, contour_col = contour_col, n_contour = n_contour,
    contour_na_rm = FALSE, angle = angle, legend_position = NULL)

}

#' @importFrom ggplot2 facet_wrap facet_grid label_both
#' @importFrom dplyr vars
`draw_3d_difference` <- function(object, xvars, contour = FALSE,
                                 contour_col = "black", n_contour = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL, angle = NULL,
                                 fill_limits = NULL) {
  if (is.null(fill_limits)) fill_limits <- surface_fill_limits(object$.diff)
  draw_2d_difference(object, xvars, contour = contour,
    contour_col = contour_col, n_contour = n_contour,
    xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
    caption = caption, angle = angle, fill_limits = fill_limits) +
    facet_wrap(vars(.data[[xvars[3L]]]), labeller = label_both)
}

`draw_4d_difference` <- function(object, xvars, contour = FALSE,
                                 contour_col = "black", n_contour = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL, angle = NULL,
                                 fill_limits = NULL) {
  if (is.null(fill_limits)) fill_limits <- surface_fill_limits(object$.diff)
  draw_2d_difference(object, xvars, contour = contour,
    contour_col = contour_col, n_contour = n_contour,
    xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
    caption = caption, angle = angle, fill_limits = fill_limits) +
    facet_grid(rows = vars(.data[[xvars[3L]]]),
      cols = vars(.data[[xvars[4L]]]), labeller = label_both)
}

#' Display penalty matrices of smooths using `ggplot`
#'
#' Displays the penalty matrices of smooths as a heatmap using `ggplot`
#'
#' @param geom character; either `"raster"` (the default) or `"tile"`.
#'   Raster rendering uses an image for the matrix, which can keep large PDF
#'   plots compact. Tile rendering uses individual rectangles, providing vector
#'   cells in PDF and SVG output, but can increase file size for large matrices.
#' @param normalize logical; normalize the penalty to the range -1, 1?
#' @param as_matrix logical; how should the plotted penalty matrix be oriented?
#'   If `TRUE` row 1, column 1 of the penalty matrix is draw in the upper left,
#'   whereas, if `FALSE` it is drawn in the lower left of the plot.
#' @param ncol,nrow numeric; the numbers of rows and columns over which to
#'   spread the plots.
#' @param xlab character or expression; the label for the x axis. If not
#'   supplied, no axis label will be drawn. May be a vector, one per penalty.
#' @param ylab character or expression; the label for the y axis.  If not
#'   supplied, no axis label will be drawn. May be a vector, one per penalty.
#' @param title character or expression; the title for the plot. See
#'   [ggplot2::labs()]. May be a vector, one per penalty.
#' @param subtitle character or expression; the subtitle for the plot. See
#'   [ggplot2::labs()]. May be a vector, one per penalty.
#' @param caption character or expression; the plot caption. See
#'   [ggplot2::labs()]. May be a vector, one per penalty.
#' @param guides character; one of `"keep"` (the default), `"collect"`, or
#'   `"auto"`. Passed to [patchwork::plot_layout()]
#'
#' @inheritParams draw.gam
#'
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom patchwork wrap_plots
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' # Fit different spline bases to simulated Gaussian responses
#' dat <- data_sim("eg1", n = 200, seed = 42)
#' m <- gam(y ~ s(x0) + s(x1, bs = "cr") + s(x2, bs = "bs"),
#'   data = dat, method = "REML")
#'
#' # Display the penalty matrices for the fitted smooths
#' draw(penalty(m))
#' @inheritSection draw Label layout
`draw.penalty_df` <- function(object,
                              normalize = FALSE,
                              as_matrix = TRUE,
                              continuous_fill = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              ncol = NULL, nrow = NULL,
                              guides = "keep",
                              geom = c("raster", "tile"),
                              ...) {
  geom <- match.arg(geom)

  ## if non-specified fill set our default
  if (is.null(continuous_fill)) {
    continuous_fill <- gratia_default_scale(scale_fill_gradient2(
      low = "#2166AC",
      high = "#B2182B",
      mid = "white",
      midpoint = 0
    ))
  }

  plt_list <- split(object, f = object[[".penalty"]])
  n_plots <- length(plt_list)
  for (i in seq_along(plt_list)) {
    plt_list[[i]] <- plot_penalty(plt_list[[i]],
      normalize = normalize,
      as_matrix = as_matrix,
      geom = geom,
      continuous_fill = continuous_fill,
      xlab = rep(xlab, n_plots),
      ylab = rep(ylab, n_plots),
      title = rep(title, n_plots),
      subtitle = rep(subtitle, n_plots),
      caption = rep(caption, n_plots)
    )
  }

  ## return
  layout <- prepare_plot_layout(n_plots, ncol = ncol, nrow = nrow)
  ncol <- layout$ncol
  nrow <- layout$nrow
  wrap_plots(plt_list,
    byrow = TRUE, ncol = ncol, nrow = nrow,
    guides = guides,
    ...
  )
}

#' @importFrom ggplot2 ggplot geom_raster
#' @importFrom dplyr mutate
#' @importFrom rlang .data
`plot_penalty` <- function(object,
                           normalize = FALSE,
                           as_matrix = TRUE,
                           continuous_fill = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           geom = c("raster", "tile")) {
  geom <- match.arg(geom)

  ## fix ordering of levels so the heatmap matches a matrix
  ## Don't reverse the cols!!
  row_levs <- if (isTRUE(as_matrix)) {
    rev(sort(unique(object$.row)))
  } else {
    sort(unique(object$.row))
  }
  object <- mutate(object,
    .row = factor(.data$.row, levels = row_levs),
    .col = factor(.data$.col, levels = sort(unique(.data$.col)))
  )

  ## rescale to -1 -- 1
  if (as.logical(normalize)) {
    object <- mutate(object, .value = norm_minus_one_to_one(.data$.value))
  }

  ## base plot
  plt <- gratia_plot(
    object,
    aes(
      x = .data$.col,
      y = .data$.row,
      fill = .data$.value
    )
  ) +
    switch(geom,
      raster = geom_gratia_surface_raster(interpolate = FALSE),
      tile = geom_gratia_surface_tile(width = 1, height = 1, colour = NA)
    )

  ## add the scale
  plt <- plt + continuous_fill

  ## labelling
  if (is.null(title)) {
    title <- unique(object[[".penalty"]])
  }
  if (is.null(caption)) {
    caption <- object[[".type"]]
  }
  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption, fill = "Penalty"
  )

  plt
}
