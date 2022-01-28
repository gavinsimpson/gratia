#' Generic plotting via `ggplot2`
#'
#' Generic function for plotting of R objects that uses the `ggplot2` package.
#'
#' @title Generic plotting via `ggplot2`
#' @param object and R object to plot.
#' @param ... arguments passed to other methods.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @author Gavin L. Simpson
#'
#' @export
`draw` <- function(object, ...) {
    UseMethod("draw")
}

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
#'   partial residuals are drawn. Should have names `..p_resid` and `..orig_x` if
#'   supplied.
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
#' @importFrom ggplot2 ggplot aes_ aes_string labs geom_line geom_ribbon expand_limits
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

    plt <- ggplot(object, aes_(x = as.name(smooth_var), y = ~ est,
                               group = ~ smooth))

    ## do we want partial residuals? Only for univariate smooths without by vars
    if (!is.null(partial_residuals)) {
        plt <- plt + geom_point(data = partial_residuals,
                                aes_string(x = "..orig_x", y = "..p_resid"),
                                inherit.aes = FALSE,
                                colour = "steelblue3", alpha = 0.5)
    }

    ## plot the confidence interval
    plt <- plt + geom_ribbon(mapping = aes_string(ymin = "lower",
                                                  ymax = "upper"),
                             alpha = 0.3) +
        geom_line()

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_var
    }
    if (missing(ylab)) {
        ylab <- "Effect"
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
            geom_rug(data = data.frame(x = rug), mapping = aes_string(x = 'x'),
                     inherit.aes = FALSE, sides = 'b', alpha = 0.5)
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
#' @importFrom ggplot2 ggplot aes_string geom_raster geom_contour labs guides guide_colourbar scale_fill_distiller theme
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
        guide_title <- "Effect" # unique(object[["smooth"]])
        plot_var <- "est"
        guide_limits <- if (is.null(response_range)) {
            c(-1, 1) * max(abs(object[[plot_var]]))
        } else {
            response_range
        }
    } else {
        guide_title <- "Std. err." # bquote(SE * (.(unique(object[["smooth"]]))))
        plot_var <- "se"
        guide_limits <- range(object[["se"]])
    }

    plt <- ggplot(object, aes_string(x = smooth_vars[1], y = smooth_vars[2])) +
        geom_raster(mapping = aes_string(fill = plot_var))

    if (isTRUE(contour)) {
        plt <- plt + geom_contour(mapping = aes_string(z = plot_var),
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
                                               barheight = grid::unit(0.25, "npc")))

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
    plt <- ggplot(object, aes_string(sample = "est")) +
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
        ylab <- "Effects"
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
#' @importFrom ggplot2 geom_line theme scale_colour_discrete geom_rug expand_limits
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

    plt <- ggplot(object, aes_(x = as.name(smooth_var), y = ~ est,
                               colour = as.name(smooth_fac))) +
        geom_line() +
        discrete_colour +
        theme(legend.position = "none")

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_var
    }
    if (missing(ylab)) {
        ylab <- "Effect"
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
                              mapping = aes_string(x = 'x'),
                              inherit.aes = FALSE,
                              sides = 'b', alpha = 0.5)
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
#' @importFrom ggplot2 ggplot geom_pointrange geom_rug geom_ribbon geom_line aes_string expand_limits
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

    plt <- ggplot(object, aes_string(x = "value", y = "partial"))

    if (is_fac) {
        plt <- plt + geom_pointrange(aes_string(ymin = "lower", ymax = "upper"))
    } else {
        if (isTRUE(rug)) {
            plt <- plt + geom_rug(sides = "b", position = position, alpha = 0.5)
        }
        plt <- plt + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"),
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

#' Plot derivatives of smooths
#'
#' @param alpha numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @inheritParams draw.gam
#'
#' @importFrom ggplot2 ggplot geom_ribbon aes_string geom_line labs
#' @importFrom patchwork wrap_plots
#' @export
#'
#' @examples
#'
#' load_mgcv()
#' dat <- data_sim("eg1", n = 800, dist = "normal", scale = 2, seed = 42)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' ## first derivative of all smooths
#' df <- derivatives(mod, type = "central")
#' draw(df)
#' ## fixed axis scales
#' draw(df, scales = "fixed")
`draw.derivatives` <- function(object,
                               select = NULL,
                               scales = c("free", "fixed"), alpha = 0.2,
                               ncol = NULL, nrow = NULL,
                               guides = "keep",
                               ...) {
    scales <- match.arg(scales)

    ## how many smooths
    sm <- unique(object[["smooth"]])
    ## select smooths
    select <- check_user_select_smooths(smooths = sm, select = select)
    sm <- sm[select]
    
    plotlist <- vector("list", length = length(sm))

    for (i in seq_along(sm)) {
        take <- object[["smooth"]] == sm[i]
        df <- object[take, ]
        xvar <- unique(df[['var']])
        plotlist[[i]] <- ggplot(df, aes_string(x = "data", y = "derivative")) +
            geom_ribbon(aes_string(ymin = "lower", ymax = "upper", y = NULL),
                        alpha = alpha) +
            geom_line() +
            labs(title = sm[i], x = xvar, y = "Derivative")
    }

    if (isTRUE(identical(scales, "fixed"))) {
        ylims <- range(object[["lower"]], object[["upper"]])

        for (i in seq_along(plotlist)) {
            plotlist[[i]] <- plotlist[[i]] + lims(y = ylims)
        }
    }
    ## return
    n_plots <- length(plotlist)
    if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_plots))
        nrow <- ceiling(n_plots / ncol)
    }
    wrap_plots(plotlist, byrow = TRUE, ncol = ncol, nrow = nrow, guides = guides,
               ...)
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
#' @param ... arguments passed to other methods. Not used by this method.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom ggplot2 ggplot aes_ labs geom_line guides facet_wrap label_both
#' 
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{set.seed(42)}
#' df <- gamSim(4, n = 400, verbose = FALSE)
#'
#' bf <- basis(s(x0), data = df)
#' draw(bf)
#'
#' bf <- basis(s(x2, by = fac, bs = 'bs'), data = df)
#' draw(bf)
`draw.mgcv_smooth` <- function(object,
                               legend = FALSE,
                               use_facets = TRUE,
                               labeller = NULL,
                               xlab, ylab,
                               title = NULL, subtitle = NULL,
                               caption = NULL,
                               ...) {
    ## capture the univariate smooth variable
    smooth_var <- names(object)[5L]

    ## default labeller
    if (is.null(labeller)) {
        labeller  <- label_both
    }

    ## basis plot
    plt <- ggplot(object, aes_(x = as.name(smooth_var), y = ~ value,
                               colour = ~ bf)) +
        geom_line()

    ## default labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_var
    }
    if (missing(ylab)) {
        ylab <- "Value"
    }
    if (is.null(title)) {
        title <- attr(object, "smooth_object")
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
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## draw a guide?
    if (!legend) {
        plt <- plt + guides(colour = "none")
    }

    plt
}

#' Plot posterior smooths
#'
#' @param n_samples numeric; if not `NULL`, sample `n_samples` from the set
#'   of posterior samples for plotting.
#' @param alpha numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @param colour The colour to use to draw the posterior smooths. Passed to
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
#'
#' @examples
#' load_mgcv()
#' \dontshow{set.seed(1)}
#' dat1 <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
#' ## a single smooth GAM
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat1, method = "REML")
#' ## posterior smooths from m1
#' sm1 <- smooth_samples(m1, n = 15, seed = 23478)
#' ## plot
#' draw(sm1, alpha = 0.7)
#' 
#' \dontshow{set.seed(1)}
#' dat2 <- gamSim(4, verbose = FALSE)
#' ## a multi-smooth GAM with a factor-by smooth
#' m2 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat2, method = "REML")
#' ## posterior smooths from m1
#' sm2 <- smooth_samples(m2, n = 15, seed = 23478)
#' ## plot, this time selecting only the factor-by smooth
#' draw(sm2, select = "s(x2)", partial_match = TRUE, alpha = 0.7)
`draw.smooth_samples` <- function(object,
                                  select = NULL,
                                  n_samples = NULL,
                                  xlab = NULL, ylab = NULL, title = NULL,
                                  subtitle = NULL, caption = NULL,
                                  alpha = 1, colour = "black",
                                  contour = FALSE,
                                  contour_col = "black",
                                  n_contour = NULL,
                                  scales = c("free", "fixed"),
                                  rug = TRUE,
                                  partial_match = FALSE,
                                  ncol = NULL, nrow = NULL,
                                  guides = "keep", ...) {
    scales <- match.arg(scales)

    ## select smooths
    S <- unique(object[["term"]])
    select <- check_user_select_smooths(smooths = S, select = select,
                                        partial_match = partial_match)
    S <- S[select]
    object <- filter(object, .data$term %in% S)

    ## can only plot 1d smooths - currently - prune S but how?
    ## FIXME

    do_plot_smooths <- function(i, object, ...) {
        object <- filter(object, .data$term == i)
        draw_posterior_smooths(object, ...)
    }

    plts <- map(S, do_plot_smooths,
                object = object, n_samples = n_samples,
                xlab = xlab, ylab = ylab,
                title = title, subtitle = subtitle, caption = caption,
                rug = rug, alpha = alpha, colour = colour,
                contour = contour, n_contour = n_contour,
                contour_col = contour_col)

    if (isTRUE(identical(scales, "fixed"))) {
        ylims <- range(object[["value"]])

        p <- seq_along(plts)
        for (i in p) {
            plts[[i]] <- plts[[i]] + lims(y = ylims)
        }
    }

    ## return
    n_plots <- length(plts)
    if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_plots))
        nrow <- ceiling(n_plots / ncol)
    }
    wrap_plots(plts, byrow = TRUE, ncol = ncol, nrow = nrow, guides = guides,
               ...)
}

`draw_posterior_smooths` <- function(object, n_samples = NULL,
                                     xlab = NULL, ylab = NULL,
                                     title = NULL, subtitle = NULL,
                                     caption = NULL, rug = TRUE, alpha = 1,
                                     colour = "black",
                                     contour = FALSE,
                                     contour_col = "black",
                                     n_contour = NULL, ...) {
    #data_names <- attr(tbl, "data_names")
    #smooth_var <- data_names[[unique(tbl[["term"]])]]

    xvars <- unique(object[["term"]])
    xvars <- vars_from_label(xvars)
    n_xvars <- length(xvars)

    ## randomly sample n_samples from the posterior draws
    if (!is.null(n_samples)) {
        if ((m <- max(object[["draw"]])) < n_samples) {
            n_samples <- m
        }
        draws <- unique(object[["draw"]])
        draws <- sample(draws, n_samples)
        object <- filter(object, .data$draw %in% draws)
    }

    plt <- if (identical(n_xvars, 1L)) {
        draw_1d_posterior_smooths(object, rug = rug,
                                  alpha = alpha, colour = colour,
                                  xlab = xlab, ylab = ylab,
                                  title = title, subtitle = subtitle,
                                  caption = caption, ...)
    } else if (identical(n_xvars, 2L)) {
        draw_2d_posterior_smooths(object, contour = contour,
                                  contour_col = contour_col,
                                  n_contour = n_contour,
                                  xlab = xlab, ylab = ylab,
                                  title = title, subtitle = subtitle,
                                  caption = caption, ...)
    } else if (identical(n_xvars, 3L)) {
        draw_3d_posterior_smooths(object, contour = contour,
                                  contour_col = contour_col,
                                  n_contour = n_contour,
                                  xlab = xlab, ylab = ylab,
                                  title = title, subtitle = subtitle,
                                  caption = caption, ...)
    } else {
        message("Can't plot samples of smooths of more than 3 variables.")
        NULL
    }

    plt #return
}

`draw_1d_posterior_smooths` <- function(object, xlab = NULL, ylab = NULL,
                                        title = NULL, subtitle = NULL,
                                        caption = NULL, rug = TRUE, alpha = 1,
                                        colour = "black") {
    data_names <- attr(object, "data_names")
    smooth_var <- data_names[[unique(object[["term"]])]]

    plt <- ggplot(object, aes_(x = ~ .x1, y = ~ value, group = ~ draw)) +
        geom_line(alpha = alpha, colour = colour)

    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- smooth_var
    }
    if (is.null(ylab)) {
        ylab <- "Effect"
    }
    if (is.null(title)) {
        title <- unique(object[["term"]])
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
        plt <- plt + geom_rug(mapping = aes_string(x = '.x1'),
                              inherit.aes = FALSE, sides = 'b', alpha = 0.5)
    }

    plt
}


#' @importFrom ggplot2 ggplot guides aes_ geom_raster geom_contour labs scale_fill_distiller guide_colourbar
#' @importFrom grid unit
`draw_2d_posterior_smooths` <- function(object,
                                        contour = FALSE,
                                        contour_col = "black",
                                        n_contour = NULL,
                                        xlab = NULL,
                                        ylab = NULL,
                                        title = NULL,
                                        subtitle = NULL,
                                        caption = NULL) {
    xvars <- unique(object[["term"]])
    xvars <- vars_from_label(xvars)

    if (is.null(xlab)) {
        xlab <- xvars[1]
    }
    if (is.null(ylab)) {
        ylab <- xvars[2]
    }
    if (is.null(title)) {
        sm_label <- unique(object$smooth)
        by_var <- unique(object$by_variable)
        ## fix this so it knows about the level
        title <- if (is.na(by_var)) {
            sm_label
        } else {
            mgcv_by_smooth_labels(sm_label, by_var, level = "")
        }
    }

    ## this is how it should be done but smooth_samples doesn't put
    ##   the data into the object under their own names..., just .x1, .x2, etc
    ## plt <- ggplot(object, aes_(x = as.name(xvars[1L]),
    ##                           y = as.name(xvars[2L]))) +
    plt <- ggplot(object, aes_string(x = ".x1", y = ".x2")) +
        geom_raster(aes_(fill = ~ value))

    if (contour) {
        plt <- plt + geom_contour(aes_(z = ~ value), bins = n_contour,
                                  colour = contour_col)
    }

    plt <- plt +
        labs(title = title, x = xlab, y = ylab, subtitle = subtitle,
             caption = caption)

    plt <- plt + scale_fill_distiller(palette = "RdBu", type = "div")

    # facet by the draw column
    plt <- plt + facet_wrap(~ draw)

    ## Set the limits for the fill
    guide_limits <- c(-1, 1) * max(abs(object[["value"]]))
    plt <- plt + expand_limits(fill = guide_limits)

    # add guide
    plt <- plt +
        guides(fill = guide_colourbar(title = "Effect", 
                                      direction = "vertical",
                                      barheight = grid::unit(0.25, "npc")))

    plt
}

`draw_3d_posterior_smooths` <- function(object, xvars, contour = FALSE,
                                        contour_col = "black", n_contour = NULL,
                                        xlab = NULL,
                                        ylab = NULL,
                                        title = NULL,
                                        subtitle = NULL,
                                        caption = NULL) {
    warning("Plotting samples of 3D smooths is not yet implemented")
    return(NULL)
}

#' Plot differences of smooths
#'
#' @param rug logical;
#' @param ref_line logical;
#' @param contour logical; should contour lines be added to smooth surfaces?
#' @param ci_alpha numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @param ci_colour colour specification for the confidence/credible intervals
#'   band.
#' @param line_col colour specicification for drawing lines
#' @param ncol,nrow numeric; the numbers of rows and columns over which to
#'   spread the plots
#' @param xlab,ylab,title,subtitle,caption character; labels with which to
#'   annotate plots
#' @param guides character; one of `"keep"` (the default), `"collect"`, or
#'   `"auto"`. Passed to [patchwork::plot_layout()]
#' @inheritParams draw.gam
#'
#' @importFrom ggplot2 ggplot geom_ribbon aes_string geom_line labs lims
#' @importFrom patchwork wrap_plots
#' @importFrom purrr map
#' @export
#'
#' @examples
#'
#' load_mgcv()
#' \dontshow{set.seed(42)}
#' df <- data_sim("eg4", seed = 42)
#' m <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")
#'
#' diffs <- difference_smooths(m, smooth = "s(x2)")
#' draw(diffs)
`draw.difference_smooth` <- function(object,
                                     select = NULL,
                                     rug = FALSE,
                                     ref_line = FALSE,
                                     contour = FALSE,
                                     contour_col = "black",
                                     n_contour = NULL,
                                     ci_alpha = 0.2,
                                     ci_colour = "black",
                                     line_col = "steelblue",
                                     scales = c("free", "fixed"),
                                     ncol = NULL, nrow = NULL,
                                     guides = "keep",
                                     xlab = NULL,
                                     ylab = NULL,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL, ...) {
    scales <- match.arg(scales)

    ## how many smooths
    sm <- unique(object[["smooth"]])
    ## select smooths
    select <- check_user_select_smooths(smooths = sm, select = select)
    sm <- sm[select]

    plotlist <- vector("list", length = length(sm))

    df_list <- split(object, f = paste(object$level_1, object$level_2,
                     sep = "-"))

    plotlist <- map(df_list, draw_difference, ci_alpha = ci_alpha,
                    line_col = line_col, rug = rug, ref_line = ref_line,
                    ci_colour = ci_colour, contour = contour,
                    contour_col = contour_col, n_contour = n_contour,
                    xlab = xlab, ylab = ylab, title = title,
                    subtitle = subtitle, caption = caption)

    if (isTRUE(identical(scales, "fixed"))) {
        ylims <- range(object[["lower"]], object[["upper"]])

        for (i in seq_along(plotlist)) {
            plotlist[[i]] <- plotlist[[i]] + lims(y = ylims)
        }
    }

    ## plot_grid(plotlist = plotlist, align = align, axis = axis, ...)
    n_plots <- length(plotlist)
    if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_plots))
        nrow <- ceiling(n_plots / ncol)
    }
    wrap_plots(plotlist, byrow = TRUE, ncol = ncol, nrow = nrow,
               guides = guides, ...)
}

`draw_difference` <- function(object,
                              rug = NULL,
                              ref_line = NULL,
                              contour = NULL,
                              ci_alpha = NULL,
                              ci_colour = NULL,
                              line_col = NULL,
                              contour_col = "black",
                              n_contour = NULL,
                              xlab = NULL, ylab = NULL,
                              title = NULL, subtitle = NULL, caption = NULL) {
    xvars <- unique(object[["smooth"]])
    xvars <- vars_from_label(xvars)
    n_xvars <- length(xvars)
    plt <- if (identical(n_xvars, 1L)) {
      draw_1d_difference(object, xvars, rug = rug, ref_line = ref_line,
                         ci_alpha = ci_alpha, line_col = line_col,
                         ci_colour = ci_colour, xlab = xlab, ylab = ylab,
                         title = title, subtitle = subtitle,
                         caption = caption)
    } else if (identical(n_xvars, 2L)) {
        draw_2d_difference(object, xvars, contour = contour,
                           contour_col = contour_col, n_contour = n_contour,
                           xlab = xlab, ylab = ylab,
                           title = title, subtitle = subtitle,
                           caption = caption)
    } else if (identical(n_xvars, 3L)) {
        draw_3d_difference(object, xvars, contour = contour,
                           contour_col = contour_col, n_contour = n_contour,
                           xlab = xlab, ylab = ylab,
                           title = title, subtitle = subtitle,
                           caption = caption)
    } else if (identical(n_xvars, 4L)) {
        draw_4d_difference(object, xvars, contour = contour,
                           contour_col = contour_col, n_contour = n_contour,
                           xlab = xlab, ylab = ylab,
                           title = title, subtitle = subtitle,
                           caption = caption)
    } else {
        message("Can't plot differences for smooths of more than 4 variables.")
        NULL
    }

    plt #return
}

#' @importFrom ggplot2 ggplot aes_string geom_ribbon geom_line labs geom_hline geom_rug
`draw_1d_difference` <- function(object, xvars,
                                 rug = FALSE,
                                 ref_line = FALSE,
                                 ci_alpha = 0.2,
                                 ci_colour = "black",
                                 line_col = "red",
                                 xlab = NULL,
                                 ylab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL) {
    sm_label <- unique(object$smooth)
    by_var <- unique(object$by)
    f1 <- unique(object$level_1)
    f2 <- unique(object$level_2)
    plt_title1 <- mgcv_by_smooth_labels(sm_label, by_var, f1)
    plt_title2 <- mgcv_by_smooth_labels(sm_label, by_var, f2)
    plt_title <- paste(plt_title1, plt_title2, sep = " - ")
    y_label <- "Difference"

    plt <- ggplot(object, aes_(x = as.name(xvars[1L]), y = ~ diff))

    if (isTRUE(ref_line)) {
        plt <- plt + geom_hline(yintercept = 0, colour = line_col)
    }
    plt <- plt +
        geom_ribbon(aes_string(ymin = "lower", ymax = "upper", y = NULL),
                    alpha = ci_alpha) +
        geom_line() +
        labs(title = plt_title, x = xvars, y = y_label)

    if(isTRUE(rug)) {
        plt <- plt + geom_rug(sides = "b", alpha = 0.5)
    }
    plt
}

#' @importFrom ggplot2 ggplot guides aes_ geom_raster geom_contour labs scale_fill_distiller guide_colourbar
#' @importFrom grid unit
`draw_2d_difference` <- function(object, xvars,
                                 contour = FALSE,
                                 contour_col = "black", ## "#3366FF",
                                 n_contour = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL) {
    if (is.null(xlab)) {
        xlab <- xvars[1]
    }
    if (is.null(ylab)) {
        ylab <- xvars[2]
    }
    if (is.null(title)) {
        sm_label <- unique(object$smooth)
        by_var <- unique(object$by)
        f1 <- unique(object$level_1)
        f2 <- unique(object$level_2)
        plt_title1 <- mgcv_by_smooth_labels(sm_label, by_var, f1)
        plt_title2 <- mgcv_by_smooth_labels(sm_label, by_var, f2)
        title <- paste(plt_title1, plt_title2, sep = " - ")
    }

    plt <- ggplot(object, aes_(x = as.name(xvars[1L]),
                               y = as.name(xvars[2L]))) +
        geom_raster(aes_(fill = ~ diff))

    if (contour) {
        plt <- plt + geom_contour(aes_(z = ~ diff), bins = n_contour,
                                  colour = contour_col)
    }

    plt <- plt +
        labs(title =title, x = xlab, y = ylab, subtitle = subtitle,
             caption = caption)

    plt <- plt + scale_fill_distiller(palette = "RdBu", type = "div")

    ## Set the limits for the fill
    guide_limits <- c(-1, 1) * max(abs(object[["diff"]]))
    plt <- plt + expand_limits(fill = guide_limits)

    plt <- plt +
        guides(fill = guide_colourbar(title = "Difference", 
                                      direction = "vertical",
                                      barheight = grid::unit(0.25, "npc")))

    plt
}

`draw_3d_difference` <- function(object, xvars, contour = FALSE,
                                 contour_col = "black", n_contour = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL) {
    warning("Plotting differences of 3D smooths is not yet implemented")
    return(NULL)
}

`draw_4d_difference` <- function(object, xvars, contour = FALSE,
                                 contour_col = "black", n_contour = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL) {
    warning("Plotting differences of 4D smooths is not yet implemented")
    return(NULL)
}

#' Display penalty matrices of smooths using `ggplot`
#'
#' Displays the penalty matrices of smooths as a heatmap using `ggplot`
#'
#' @param normalize logical; normalize the penalty to the range -1, 1?
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
#' @inheritParams draw.evaluated_2d_smooth
#'
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom patchwork wrap_plots
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' dat <- data_sim("eg4", n = 400, seed = 42)
#' m <- gam(y ~ s(x0) + s(x1, bs = 'cr') + s(x2, bs = 'bs', by = fac),
#'          data = dat, method = "REML")
#'
#' ## produce a multi-panel plot of all penalties
#' draw(penalty(m))
#'
#' # for a specific smooth
#' draw(penalty(m, smooth = "s(x2):fac1"))
`draw.penalty_df` <- function(object,
                              normalize = FALSE,
                              continuous_fill = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              ncol = NULL, nrow = NULL,
                              guides = "keep",
                              ...) {
    ## if non-specified fill set our default
    if (is.null(continuous_fill)) {
        continuous_fill <- scale_fill_gradient2(low = "#2166AC",
                                                high = "#B2182B",
                                                mid = "white",
                                                midpoint = 0)
    }

    plt_list <- split(object, f = object[["penalty"]])
    n_plots <- length(plt_list)
    for (i in seq_along(plt_list)) {
        plt_list[[i]] <- plot_penalty(plt_list[[i]],
                                      normalize = normalize,
                                      continuous_fill = continuous_fill,
                                      xlab = rep(xlab, n_plots),
                                      ylab = rep(ylab, n_plots),
                                      title = rep(title, n_plots),
                                      subtitle = rep(subtitle, n_plots),
                                      caption = rep(caption, n_plots))
    }

    ## return
    if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_plots))
        nrow <- ceiling(n_plots / ncol)
    }
    wrap_plots(plt_list, byrow = TRUE, ncol = ncol, nrow = nrow, guides = guides,
               ...)
}

#' @importFrom ggplot2 ggplot geom_raster
#' @importFrom dplyr mutate
#' @importFrom rlang .data
`plot_penalty` <- function(object,
                           normalize = FALSE,
                           continuous_fill = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL) {

    ## fix ordering of levels so the heatmap matches a matrix
    ## Don't reverse the cols!!
    object <- mutate(object,
                     row = factor(.data$row, levels = rev(sort(unique(.data$row)))),
                     col = factor(.data$col, levels = sort(unique(.data$col))))
    
    ## rescale to -1 -- 1
    if (as.logical(normalize)) {
        object <- mutate(object, value = norm_minus_one_to_one(.data$value))
    }

    ## base plot
    plt <- ggplot(object,
                  aes_string(x = "col", y = "row", fill = "value")) +
        geom_raster()

    ## add the scale
    plt <- plt + continuous_fill

    ## labelling
    if (is.null(title)) {
        title <- unique(object[["penalty"]])
    }
    if (is.null(caption)) {
        caption <- object[["type"]]
    }
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption, fill = "Penalty")

    plt
}
