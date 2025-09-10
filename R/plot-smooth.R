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
  ...
) {
  # do we have a grouped factor by?
  grouped_by <- FALSE
  if (".term" %in% names(object) && !all(is.na(object[[".by"]]))) {
    if (is.null(variables)) {
      variables <- vars_from_label(unique(object[[".term"]]))
    }
    grouped_by <- TRUE
  } else {
    if (is.null(variables)) {
      variables <- vars_from_label(unique(object[[".smooth"]]))
    }
  }

  # If constant supplied apply it to `.estimate`
  object <- add_constant(object, constant = constant)

  # If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object, fun = fun)

  # base plot - need as.name to handle none standard names, like log2(x)
  by_var <- unique(object$.by)
  plt <- if (grouped_by) {
    ggplot(object, aes(
      x = .data[[variables]], y = .data$.estimate,
      colour = .data[[by_var]], group = .data[[by_var]]
    )) +
      guides(x = guide_axis(angle = angle))
  } else {
    ggplot(object, aes(x = .data[[variables]], y = .data$.estimate)) +
      guides(x = guide_axis(angle = angle))
  }

  # do we want partial residuals? Only for univariate smooths without by vars
  if (!is.null(partial_residuals)) {
    plt <- plt + geom_point(
      data = partial_residuals,
      aes(
        x = .data[[variables]],
        y = .data[["partial_residual"]]
      ),
      inherit.aes = FALSE,
      colour = resid_col, alpha = 0.5
    )
  }

  # plot the confidence interval and smooth line
  sizer_cols <- c(".change", ".increase", ".decrease")
  do_sizer <- sizer_cols %in% names(object)
  if (grouped_by) {
    plt <- plt +
      geom_ribbon(
        mapping = aes(
          ymin = .data[[".lower_ci"]],
          ymax = .data[[".upper_ci"]],
          fill = .data[[by_var]]
        ),
        alpha = ci_alpha, colour = NA
      ) +
      geom_line(aes(colour = .data[[by_var]]))

    plt <- if (nlevels(object[[by_var]]) > 9) {
      plt + scale_colour_hue() +
        scale_fill_hue()
    } else {
      plt + scale_colour_okabe_ito() +
        scale_fill_okabe_ito()
    }

    if (any(do_sizer)) {
      plt <- if (do_sizer[[1]]) {
        plt + geom_line(
          aes(
            y = .data[[".change"]],
            colour = .data[[by_var]]
          ),
          linewidth = change_lwd,
          na.rm = TRUE
        )
      } else {
        plt + geom_line(
          aes(
            y = .data[[".increase"]],
            colour = .data[[by_var]]
          ),
          linewidth = change_lwd,
          na.rm = TRUE,
          show.legend = FALSE
        ) +
          geom_line(
            aes(
              y = .data[[".decrease"]],
              colour = .data[[by_var]]
            ),
            linewidth = change_lwd,
            na.rm = TRUE,
            show.legend = FALSE
          )
      }
    }
  } else {
    plt <- plt +
      geom_ribbon(
        mapping = aes(
          ymin = .data[[".lower_ci"]],
          ymax = .data[[".upper_ci"]]
        ),
        alpha = ci_alpha, colour = NA, fill = ci_col
      ) +
      geom_line(colour = smooth_col)
    if (any(do_sizer)) {
      plt <- if (do_sizer[[1]]) {
        plt + geom_line(aes(y = .data[[".change"]]),
          colour = smooth_col, linewidth = change_lwd, na.rm = TRUE,
          show.legend = FALSE
        )
      } else {
        plt + geom_line(aes(y = .data[[".increase"]]),
          colour = increase_col, linewidth = change_lwd,
          na.rm = TRUE, show.legend = FALSE
        ) +
          geom_line(aes(y = .data[[".decrease"]]),
            colour = decrease_col, linewidth = change_lwd,
            na.rm = TRUE, show.legend = FALSE
          )
      }
    }
  }

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- variables
  }
  if (is.null(ylab)) {
    ylab <- "Partial effect"
  }
  if (is.null(title)) {
    title <- ifelse(grouped_by, unique(object$.term),
      as.character(unique(object$.smooth))
    )
  }
  # add the basis via caption if caption is TRUE or NULL
  if ((is.logical(caption) && isTRUE(caption)) || is.null(caption)) {
    caption <- paste("Basis:", object[[".type"]])
  } else {
    caption <- NULL
  }
  if (all(!is.na(object[[".by"]]))) {
    if (grouped_by) {
      if (is.null(subtitle)) {
        subtitle <- paste0("By: ", by_var)
      }
    } else {
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
  }

  ## add labelling to plot
  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

  ## add rug?
  if (!is.null(rug)) {
    plt <- plt +
      geom_rug(
        data = rug,
        mapping = aes(x = .data[[variables]]),
        inherit.aes = FALSE, sides = "b", alpha = 0.5
      )
  }

  # fix the yaxis limits?
  if (!is.null(ylim)) {
    plt <- plt + expand_limits(y = ylim)
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
  ...
) {
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  if (is.null(continuous_fill)) {
    continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
  }

  ## If constant supplied apply it to `.estimate`
  object <- add_constant(object, constant = constant)

  ## If fun supplied, use it to transform est and the upper and lower interval
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

  plt <- ggplot(object, aes(
    x = .data[[variables[1]]],
    y = .data[[variables[2]]]
  )) +
    geom_raster(mapping = aes(fill = .data[[plot_var]]))

  if (isTRUE(contour)) {
    plt <- plt + geom_contour(
      mapping = aes(z = .data[[plot_var]]),
      colour = contour_col,
      bins = n_contour,
      na.rm = TRUE
    )
  }

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- variables[1L]
  }
  if (is.null(ylab)) {
    ylab <- variables[2L]
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

  ## Set the palette
  plt <- plt + continuous_fill

  ## Set the limits for the fill
  plt <- plt + expand_limits(fill = guide_limits)

  ## add guide
  plt <- plt +
    guides(
      fill = guide_colourbar(
        title = guide_title,
        direction = "vertical"#,
        #barheight = grid::unit(5, "lines") #grid::unit(0.25, "npc")
      ),
      x = guide_axis(angle = angle)
    )

  ## position legend at the
  plt <- plt + theme(legend.position = "right")

  ## add rug?
  if (!is.null(rug)) {
    plt <- plt +
      geom_point(
        data = rug,
        mapping = aes(
          x = .data[[variables[1]]],
          y = .data[[variables[2]]]
        ),
        inherit.aes = FALSE, alpha = 0.1
      )
  }

  plt
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
  ...
) {
  if (is.null(variables)) {
    variables <- attr(object, "tensor_term_order")
    if (is.null(variables)) {
      variables <- vars_from_label(unique(object[[".smooth"]]))
    }
  }

  if (is.null(continuous_fill)) {
    continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
  }

  ## If constant supplied apply it to `estimate`
  object <- add_constant(object, constant = constant)

  ## If fun supplied, use it to transform est and the upper and lower interval
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

  plt <- ggplot(object, aes(
    x = .data[[variables[1]]],
    y = .data[[variables[2]]]
  )) +
    geom_raster(mapping = aes(fill = .data[[plot_var]])) +
    facet_wrap(vars(.data[[variables[3]]]))

  if (isTRUE(contour)) {
    plt <- plt + geom_contour(
      mapping = aes(z = .data[[plot_var]]),
      colour = contour_col,
      bins = n_contour,
      na.rm = TRUE
    )
  }

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- variables[1L]
  }
  if (is.null(ylab)) {
    ylab <- variables[2L]
  }
  if (is.null(title)) {
    title <- unique(object[[".smooth"]])
  }
  # add the basis via caption if caption is TRUE or NULL
  if ((is.logical(caption) && isTRUE(caption)) || is.null(caption)) {
    caption <- paste("Facets:", variables[3], "; Basis:", object[[".type"]])
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

  ## Set the palette
  plt <- plt + continuous_fill

  ## Set the limits for the fill
  plt <- plt + expand_limits(fill = guide_limits)

  ## add guide
  plt <- plt +
    guides(
      fill = guide_colourbar(
        title = guide_title,
        direction = "vertical"#,
        #barheight = grid::unit(5, "lines") #grid::unit(0.25, "npc")
      ),
      x = guide_axis(angle = angle)
    )

  ## position legend at the
  plt <- plt + theme(legend.position = "right")

  ## add rug? -- not yet. Need a better way to select smooth_data for 3 and 4D
  ## smooths. At the moment, we are taking a few values over the range of the
  ## 3 or 4 d variables (only, 1 and 2 dim still get n values). But we don't
  ## have data at those 3/4d coordinates. When we plot with a rug, we end up
  ## introducing nrow(orig_data) new values into the object that gets plotted
  ## and this messes up the facets at draw time.
  ##
  ## What we want here perhaps is to bin the data into the groups formed by
  ## the cut points of the data that we're plotting at and only modify the
  ## rug data so that we group the data by the cuts we're faceting by and
  ## modify the 3/4d variable(s) to be these unique values that we're
  ## plotting as facets.
  # if (!is.null(rug)) {
  #     plt <- plt +
  #       geom_point(data = rug,
  #                  mapping = aes(x = .data[[variables[1]]],
  #                                y = .data[[variables[2]]]),
  #                  inherit.aes = FALSE, alpha = 0.1)
  # }

  if (inherits(object, "isotropic_smooth")) {
    plt <- plt + coord_equal()
  }

  plt
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
  ...
) {
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  if (is.null(continuous_fill)) {
    continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
  }

  ## If constant supplied apply it to `estimate`
  object <- add_constant(object, constant = constant)

  ## If fun supplied, use it to transform est and the upper and lower interval
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

  plt <- ggplot(object, aes(
    x = .data[[variables[1]]],
    y = .data[[variables[2]]]
  )) +
    geom_raster(mapping = aes(fill = .data[[plot_var]])) +
    facet_grid(
      rows = vars(.data[[variables[3]]]),
      cols = vars(.data[[variables[4]]]),
      as.table = FALSE
    )

  if (isTRUE(contour)) {
    plt <- plt + geom_contour(
      mapping = aes(z = .data[[plot_var]]),
      colour = contour_col,
      bins = n_contour,
      na.rm = TRUE
    )
  }

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- variables[1L]
  }
  if (is.null(ylab)) {
    ylab <- variables[2L]
  }
  if (is.null(title)) {
    title <- unique(object[[".smooth"]])
  }
  # add the basis via caption if caption is TRUE or NULL
  if ((is.logical(caption) && isTRUE(caption)) || is.null(caption)) {
    caption <- paste(
      "Facet rows:", variables[3],
      "; columns:", variables[4],
      "; Basis:", object[[".type"]]
    )
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

  ## Set the palette
  plt <- plt + continuous_fill

  ## Set the limits for the fill
  plt <- plt + expand_limits(fill = guide_limits)

  ## add guide
  plt <- plt +
    guides(
      fill = guide_colourbar(
        title = guide_title,
        direction = "vertical"#,
        #barheight = grid::unit(5, "lines") #grid::unit(0.25, "npc")
      ),
      x = guide_axis(angle = angle)
    )

  ## position legend at the
  plt <- plt + theme(legend.position = "right")

  ## add rug? -- not yet. Need a better way to select smooth_data for 3 and 4D
  ## smooths. At the moment, we are taking a few values over the range of the
  ## 3 or 4 d variables (only, 1 and 2 dim still get n values). But we don't
  ## have data at those 3/4d coordinates. When we plot with a rug, we end up
  ## introducing nrow(orig_data) new values into the object that gets plotted
  ## and this messes up the facets at draw time.
  ##
  ## What we want here perhaps is to bin the data into the groups formed by
  ## the cut points of the data that we're plotting at and only modify the
  ## rug data so that we group the data by the cuts we're faceting by and
  ## modify the 3/4d variable(s) to be these unique values that we're
  ## plotting as facets.
  # if (!is.null(rug)) {
  #     plt <- plt +
  #       geom_point(data = rug,
  #                  mapping = aes(x = .data[[variables[1]]],
  #                                y = .data[[variables[2]]]),
  #                  inherit.aes = FALSE, alpha = 0.1)
  # }

  if (inherits(object, "isotropic_smooth")) {
    plt <- plt + coord_equal()
  }

  plt
}

#' @importFrom ggplot2 coord_equal
`plot_smooth.isotropic_smooth` <- function(
  object,
  ...
) {
  # plot as per a bivariate smooth
  plt <- plot_smooth.bivariate_smooth(object, ...)

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
    message("Can't currently plot multivariate 'fs' smooths.")
    message("Skipping: ", unique(object[[".smooth"]]))
    return(NULL) # returns early!
  }

  if (is.null(discrete_colour)) {
    discrete_colour <- scale_colour_discrete()
  }

  ## If constant supplied apply it to `est`
  object <- add_constant(object, constant = constant)

  ## If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object, fun = fun)

  plt <- ggplot(object, aes(
    x = .data[[variables[1]]],
    y = .data[[".estimate"]],
    colour = .data[[variables[2]]]
  )) +
    geom_line() +
    discrete_colour +
    theme(legend.position = "none") +
    guides(x = guide_axis(angle = angle))

  ## default axis labels if none supplied
  if (missing(xlab)) {
    xlab <- variables[1]
  }
  if (missing(ylab)) {
    ylab <- "Partial effect"
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

  # how many levels? can't have more than 9 for okabeito
  n_levs <- nlevels(object[[fac_var]])
  if (is.null(discrete_colour)) {
    discrete_colour <- if (n_levs > 9L) {
      scale_colour_hue()
    } else {
      scale_colour_okabe_ito()
    }
  }

  if (is.null(discrete_fill)) {
    discrete_fill <- if (n_levs > 9L) {
      scale_fill_hue()
    } else {
      scale_fill_okabe_ito()
    }
  }

  ## If constant supplied apply it to `est`
  object <- add_constant(object, constant = constant)

  ## If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object, fun = fun)

  # plot
  plt <- ggplot(object, aes(
    x = .data[[x_var]],
    y = .data[[".estimate"]],
    colour = .data[[fac_var]]
  )) +
    geom_ribbon(
      mapping = aes(
        ymin = .data[[".lower_ci"]],
        ymax = .data[[".upper_ci"]],
        fill = .data[[fac_var]],
        colour = NULL
      ),
      alpha = ci_alpha
    ) +
    geom_line() +
    discrete_colour +
    discrete_fill +
    guides(x = guide_axis(angle = angle))

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- x_var
  }
  if (is.null(ylab)) {
    ylab <- "Partial effect"
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
    caption = caption, colour = fac_var_lab, fill = fac_var_lab
  )

  ## add rug?
  if (!is.null(rug)) {
    plt <- plt + geom_rug(
      data = rug,
      mapping = aes(
        x = .data[[x_var]],
        colour = .data[[fac_var]]
      ),
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
  ...
) {
  if (is.null(variables)) {
    variables <- vars_from_label(unique(object[[".smooth"]]))
  }

  if (is.null(continuous_fill)) {
    continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
  }

  ## If constant supplied apply it to `.estimate`
  object <- add_constant(object, constant = constant)

  ## If fun supplied, use it to transform est and the upper and lower interval
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

  # extract the boundary data
  bndry <- vec_slice(object, object[[".bndry"]])
  object <- vec_slice(object, !object[[".bndry"]])

  plt <- ggplot(
    object,
    aes(
      x = .data[[variables[1]]],
      y = .data[[variables[2]]]
    )
  ) +
    geom_raster(mapping = aes(fill = .data[[plot_var]])) +
    coord_fixed(ratio = 1)

  if (isTRUE(contour)) {
    plt <- plt + geom_contour(
      mapping = aes(z = .data[[plot_var]]),
      colour = contour_col,
      bins = n_contour,
      na.rm = TRUE
    )
  }

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- variables[1L]
  }
  if (is.null(ylab)) {
    ylab <- variables[2L]
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

  ## Set the palette
  plt <- plt + continuous_fill

  ## Set the limits for the fill
  plt <- plt + expand_limits(fill = guide_limits)

  ## add guide
  plt <- plt +
    guides(
      fill = guide_colourbar(
        title = guide_title,
        direction = "vertical"#,
        #barheight = grid::unit(5, "lines") #grid::unit(0.25, "npc")
      ),
      x = guide_axis(angle = angle)
    )

  ## position legend at the
  plt <- plt + theme(legend.position = "right")

  ## add rug?
  if (!is.null(rug)) {
    plt <- plt +
      geom_point(
        data = rug,
        mapping = aes(
          x = .data[[variables[1]]],
          y = .data[[variables[2]]]
        ),
        inherit.aes = FALSE, alpha = 0.1
      )
  }

  ## add the boundary
  plt <- plt +
    geom_path(
      data = bndry,
      mapping = aes(
        x = .data[[variables[1]]],
        y = .data[[variables[2]]],
        group = .data[[".loop"]] # need to group by loops in case >1 loop
      ),
      linewidth = 2, colour = "black"
    )

  plt
}
