#' Plot estimated smooths
#'
#' Plots estimated univariate and bivariate smooths using ggplot2.
#'
#' @param object an object, the result of a call to [evaluate_smooth()].
#' @param rug For `evaluate_smooth()`, a numeric vector of values for the
#'   location of data on the x axis. The default of `NULL` results in no
#'   rug plot being drawn. For `evaluate_parametric_terms()`, a logical to
#'   indicate if a rug plot should be drawn.
#' @param ci_level numeric between 0 and 1; the coverage of credible interval.
#' @param constant numeric; a constant to add to the estimated values of the
#'   smooth. `constant`, if supplied, will be added to the estimated value
#'   before the confidence band is computed.
#' @param fun function; a function that will be applied to the estimated values
#'   and confidence interval before plotting. Can be a function or the name of a
#'   function. Function `fun` will be applied after adding any `constant`, if
#'   provided.
#' @param partial_residuals data frame; partial residuals and data values if
#'   partial residuals are drawn. Should have names `..p_resid` and `..orig_x`
#'   if supplied.
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
#' @param response_range numeric; a vector of two values giving the range of
#'   response data for the guide. Used to fix plots to a common scale/range.
#'   Ignored if `show` is set to `"se"`.
#' @param ... arguments passed to other methods.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom ggplot2 ggplot aes labs geom_line geom_ribbon
#'   expand_limits
#' @importFrom rlang .data
#' @importFrom grid unit
#'
#' @export
#' @name draw.evaluated_smooth
#' @aliases draw.evaluated_1d_smooth draw.evaluated_2d_smooth geom_rug
#'
#' @examples
#' load_mgcv()
#'
#' dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' sm <- evaluate_smooth(m1, "s(x2)")
#' draw(sm)
#'
#' ## supply constant to shift y axis scale
#' draw(sm, constant = coef(m1)[1])
#'
#' dat <- data_sim("eg2", n = 1000, dist = "normal", scale = 1, seed = 2)
#' m2 <- gam(y ~ s(x, z, k = 40), data = dat, method = "REML")
#'
#' sm <- evaluate_smooth(m2, "s(x,z)", n = 100)
#' draw(sm)
`draw.evaluated_1d_smooth` <- function(object,
                                       rug = NULL,
                                       ci_level = 0.95,
                                       constant = NULL,
                                       fun = NULL,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       partial_residuals = NULL,
                                       response_range = NULL,
                                       ...) {
    smooth_var <- names(object)[3L]

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## Add confidence interval
    crit <- qnorm((1 - ci_level) / 2, lower.tail = FALSE)
    object[["upper"]] <- object[["est"]] + (crit * object[["se"]])
    object[["lower"]] <- object[["est"]] - (crit * object[["se"]])

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    plt <- ggplot(object, aes(x = .data[[smooth_var]],
                              y = .data[["est"]],
                              group = .data[["smooth"]]))

    ## do we want partial residuals? Only for univariate smooths without by vars
    if (!is.null(partial_residuals)) {
        plt <- plt + geom_point(data = partial_residuals,
                                aes(x = .data[["..orig_x"]],
                                    y = .data[["..p_resid"]]),
                                inherit.aes = FALSE,
                                colour = "steelblue3", alpha = 0.5)
    }

    ## plot the confidence interval
    plt <- plt + geom_ribbon(mapping = aes(ymin = .data$lower,
                                           ymax = .data$upper),
                             alpha = 0.3) +
        geom_line()

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_var
    }
    if (missing(ylab)) {
        ylab <- "Partial effect"
    }
    if (is.null(title)) {
        title <- unique(object[["smooth"]])
    }
    if (all(!is.na(object[["by_variable"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[["by_variable"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## add rug?
    if (!is.null(rug)) {
        plt <- plt +
            geom_rug(data = data.frame(x = rug),
                     mapping = aes(x = .data$x),
                     inherit.aes = FALSE, sides = "b", alpha = 0.5)
    }

    ## fixing the y axis limits?
    if (!is.null(response_range)) {
        plt <- plt + expand_limits(y = response_range)
    }

    plt
}

#' @param show character; plot the estimated smooth (`"estimate"`) or its
#'   standard error (`"se"`).
#' @param contour logical; should contours be draw on the plot using
#'   [ggplot2::geom_contour()].
#' @param contour_col colour specification for contour lines.
#' @param n_contour numeric; the number of contour bins. Will result in
#'   `n_contour - 1` contour lines being drawn. See [ggplot2::geom_contour()].
#' @param response_range numeric; a vector of two values giving the range of
#'   response data for the guide. Used to fix plots to a common scale/range.
#'   Ignored if `show` is set to `"se"`.
#' @param continuous_fill suitable scale used for the filled surface. If `NULL`,
#'   the default used is `scale_fill_distiller(palette = "RdBu", type = "div")`.
#'
#' @importFrom ggplot2 ggplot aes geom_raster geom_contour labs guides
#'   guide_colourbar scale_fill_distiller theme
#' @importFrom grid unit
#'
#' @export
#' @rdname draw.evaluated_smooth
`draw.evaluated_2d_smooth` <- function(object, show = c("estimate","se"),
                                       contour = TRUE,
                                       contour_col = "black",
                                       n_contour = NULL,
                                       constant = NULL,
                                       fun = NULL,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       response_range = NULL,
                                       continuous_fill = NULL,
                                       ...) {
    if (is.null(continuous_fill)) {
        continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
    }

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    smooth_vars <- names(object)[3:4]
    show <- match.arg(show)
    if (isTRUE(identical(show, "estimate"))) {
        guide_title <- "Partial\neffect"
        plot_var <- "est"
        guide_limits <- if (is.null(response_range)) {
            c(-1, 1) * max(abs(object[[plot_var]]))
        } else {
            response_range
        }
    } else {
        guide_title <- "Std. err."
        plot_var <- "se"
        guide_limits <- range(object[["se"]])
    }

    plt <- ggplot(object, aes(x = .data[[smooth_vars[1]]],
                              y = .data[[smooth_vars[2]]])) +
        geom_raster(mapping = aes(fill = .data[[plot_var]]))

    if (isTRUE(contour)) {
        plt <- plt + geom_contour(mapping = aes(z = .data[[plot_var]]),
                                  colour = contour_col,
                                  bins = n_contour)
    }

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_vars[1L]
    }
    if (missing(ylab)) {
        ylab <- smooth_vars[2L]
    }
    if (is.null(title)) {
        title <- unique(object[["smooth"]])
    }
    if (all(!is.na(object[["by_variable"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[["by_variable"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## Set the palette
    plt <- plt + continuous_fill

    ## Set the limits for the fill
    plt <- plt + expand_limits(fill = guide_limits)

    ## add guide
    plt <- plt + guides(fill = guide_colourbar(title = guide_title,
                                               direction = "vertical",
                                               barheight = grid::unit(0.25,
                                                                      "npc")))

    ## position legend at the
    plt <- plt + theme(legend.position = "right")

    plt
}


#' @param qq_line logical; draw a reference line through the lower and upper
#'   theoretical quartiles.
#'
#' @importFrom ggplot2 geom_abline geom_point labs expand_limits
#' @importFrom stats quantile qnorm
#'
#' @export
#' @rdname draw.evaluated_smooth
`draw.evaluated_re_smooth` <- function(object, qq_line = TRUE,
                                       constant = NULL, fun = NULL,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       response_range = NULL, ...) {
    smooth_var <- unique(object[["smooth"]]) ## names(object)[3L]

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    ## base plot with computed QQs
    plt <- ggplot(object, aes(sample = .data$est)) +
        geom_point(stat = "qq")

    ## add a QQ reference line
    if (isTRUE(qq_line)) {
        sampq <- quantile(object[["est"]], c(0.25, 0.75))
        gaussq <- qnorm(c(0.25, 0.75))
        slope <- diff(sampq) / diff(gaussq)
        intercept <- sampq[1L] - slope * gaussq[1L]

        plt <- plt + geom_abline(slope = slope, intercept = intercept)
    }

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- "Gaussian quantiles"
    }
    if (missing(ylab)) {
        ylab <- "Partial effects"
    }
    if(is.null(title)) {
        title <- smooth_var
    }
    if (all(!is.na(object[["by_variable"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[["by_variable"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## fixing the y axis limits?
    if (!is.null(response_range)) {
        plt <- plt + expand_limits(y = response_range)
    }

    plt
}

#' @param discrete_colour an appropriate discrete colour scale from `ggplot2`.
#'   The scale will need to be able to provide as many colours as there are
#'   levels in the factor variable involved in the smooth. Suitable alternatives
#'   include [ggplot2::scale_colour_viridis_d()].
#'
#' @importFrom ggplot2 geom_line theme scale_colour_discrete geom_rug
#'   expand_limits
#' @export
#' @rdname draw.evaluated_smooth
`draw.evaluated_fs_smooth` <- function(object,
                                       rug = NULL,
                                       constant = NULL,
                                       fun = NULL,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       response_range = NULL,
                                       discrete_colour = NULL,
                                       ...) {
    if (is.null(discrete_colour)) {
        discrete_colour <- scale_colour_discrete()
    }

    smooth_var <- names(object)[3L]
    smooth_fac <- names(object)[4L]

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    plt <- ggplot(object, aes(x = .data[[smooth_var]],
                              y = .data$est,
                              colour = .data[[smooth_fac]])) +
        geom_line() +
        discrete_colour +
        theme(legend.position = "none")

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_var
    }
    if (missing(ylab)) {
        ylab <- "Partial effect"
    }
    if (is.null(title)) {
        title <- unique(object[["smooth"]])
    }
    if (all(!is.na(object[["by_variable"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[["by_variable"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## add rug?
    if (!is.null(rug)) {
        plt <- plt + geom_rug(data = data.frame(x = rug),
                              mapping = aes(x = .data$x),
                              inherit.aes = FALSE,
                              sides = "b", alpha = 0.5)
    }

    ## fixing the y axis limits?
    if (!is.null(response_range)) {
        plt <- plt + expand_limits(y = response_range)
    }

    plt
}

#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#'
#' @importFrom ggplot2 ggplot geom_pointrange geom_rug geom_ribbon geom_line
#'   aes expand_limits
#' @export
#' @rdname draw.evaluated_smooth
`draw.evaluated_parametric_term` <- function(object,
                                             ci_level = 0.95,
                                             constant = NULL,
                                             fun = NULL,
                                             xlab, ylab,
                                             title = NULL, subtitle = NULL,
                                             caption = NULL,
                                             rug = TRUE,
                                             position = "identity",
                                             response_range = NULL,
                                             ...) {
    is_fac <- object[["type"]][1L] == "factor"
    term_label <- object[["term"]][1L]

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## add a CI
    crit <- qnorm((1 - ci_level) / 2, lower.tail = FALSE)
    object <- mutate(object,
                     lower = .data$partial - (crit * .data$se),
                     upper = .data$partial + (crit * .data$se))

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    plt <- ggplot(object, aes(x = .data$value, y = .data$partial))

    if (is_fac) {
        plt <- plt + geom_pointrange(aes(ymin = .data$lower,
                                         ymax = .data$upper))
    } else {
        if (isTRUE(rug)) {
            plt <- plt + geom_rug(sides = "b", position = position, alpha = 0.5)
        }
        plt <- plt + geom_ribbon(aes(ymin = .data$lower,
                                     ymax = .data$upper),
                                 alpha = 0.3) +
            geom_line()
    }

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- term_label
    }
    if (missing(ylab)) {
        ylab <- sprintf("Partial effect of %s", term_label)
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## fixing the y axis limits?
    if (!is.null(response_range)) {
        plt <- plt + expand_limits(y = response_range)
    }

    plt
}
