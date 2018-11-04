##' Generic plotting via `ggplot2`
##'
##' Generic function for plotting of R objects that uses the `ggplot2` package.
##'
##' @title Generic plotting via `ggplot2`
##' @param object and R object to plot.
##' @param ... arguments passed to other methods.
##'
##' @return A [ggplot2::ggplot()] object.
##'
##' @author Gavin L. Simpson
##'
##' @export
`draw` <- function(object, ...) {
    UseMethod("draw")
}

##' Plot estimated smooths
##'
##' Plots estimated univariate and bivariate smooths using ggplot2.
##'
##' @param object an object, the result of a call to [evaluate_smooth()].
##' @param xlab character or expression; the label for the x axis. If not
##'   supplied, a suitable label will be generated from `object`.
##' @param ylab character or expression; the label for the y axis. If not
##'   supplied, a suitable label will be generated from `object`.
##' @param title character or expression; the title for the plot. See
##'   [ggplot2::labs()].
##' @param subtitle character or expression; the subtitle for the plot. See
##'   [ggplot2::labs()].
##' @param caption character or expression; the plot caption. See
##'   [ggplot2::labs()].
##' @param ... arguments passed to other methods.
##'
##' @return A [ggplot2::ggplot()] object.
##'
##' @author Gavin L. Simpson
##'
##' @importFrom ggplot2 ggplot aes_ aes_string labs geom_line geom_ribbon
##' @importFrom grid unit
##'
##' @export
##' @name draw.evaluated_smooth
##' @aliases draw.evaluated_1d_smooth draw.evaluated_2d_smooth
##'
##' @examples
##' library("mgcv")
##'
##' \dontshow{set.seed(2)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' sm <- evaluate_smooth(m1, "s(x2)")
##' draw(sm)
##'
##' \dontshow{set.seed(2)}
##' dat <- gamSim(2, n = 4000, dist = "normal", scale = 1)
##' m2 <- gam(y ~ s(x, z, k = 40), data = dat$data, method = "REML")
##'
##' sm <- evaluate_smooth(m2, "s(x,z)", n = 100)
##' draw(sm)
`draw.evaluated_1d_smooth` <- function(object,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       ...) {
    smooth_var <- names(object)[3L]

    ## Add confidence interval
    object[["upper"]] <- object[["est"]] + (2 * object[["se"]])
    object[["lower"]] <- object[["est"]] - (2 * object[["se"]])

    plt <- ggplot(object, aes_(x = as.name(smooth_var), y = ~ est)) +
        geom_ribbon(mapping = aes_string(ymin = "lower",
                                         ymax = "upper"),
                    alpha = 0.2) +
        geom_line()

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_var
    }
    if (missing(ylab)) {
        ylab <- unique(object[["smooth"]])
    }
    if (is.null(title)) {
        title <- unique(object[["smooth"]])
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    plt
}

##' @param show character; plot the estimated smooth (`"estimate"`) or its
##'   standard error (`"se"`).
##' @param contour logical; should contours be draw on the plot using
##'   [ggplot2::geom_contour()].
##'
##' @importFrom ggplot2 ggplot aes_string geom_raster geom_contour labs guides guide_colourbar scale_fill_distiller theme
##' @importFrom grid unit
##'
##' @export
##' @rdname draw.evaluated_smooth
`draw.evaluated_2d_smooth` <- function(object, show = c("estimate","se"),
                                       contour = TRUE,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       ...) {
    smooth_vars <- names(object)[3:4]
    show <- match.arg(show)
    if (isTRUE(identical(show, "estimate"))) {
        guide_title <- unique(object[["smooth"]])
        plot_var <- "est"
        guide_limits <- c(-1, 1) * max(abs(object[[plot_var]]))
    } else {
        guide_title <- bquote(SE * (.(unique(object[["smooth"]]))))
        plot_var <- "se"
        guide_limits <- range(object[["se"]])
    }

    plt <- ggplot(object, aes_string(x = smooth_vars[1], y = smooth_vars[2])) +
        geom_raster(mapping = aes_string(fill = plot_var))

    if (isTRUE(contour)) {
        plt <- plt + geom_contour(mapping = aes_string(z = plot_var))
    }

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_vars[1L]
    }
    if (missing(ylab)) {
        ylab <- smooth_vars[2L]
    }
    if (is.null(title)) {
        title <- guide_title
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## Set the palette
    plt <- plt + scale_fill_distiller(palette = "RdBu", type = "div",
                                      limits = guide_limits)

    ## add guide
    plt <- plt + guides(fill = guide_colourbar(title = guide_title,
                                               direction = "vertical",
                                               barheight = grid::unit(0.25, "npc")))

    ## position legend at the
    plt <- plt + theme(legend.position = "right")

    plt
}

##' Plot estimated smooths from a fitted GAM
##'
##' Plots estimated smooths from a fitted GAM model in a similar way to
##' `mgcv::plot.gam()` but instead of using base graphics, [ggplot2::ggplot()]
##' is used instead.
##'
##' @param object a fitted GAM, the result of a call to [mgcv::gam()].
##' @param parametric logical; plot parametric terms also? Default is `TRUE`.
##' @param select character;
##' @param scales character; should all univariate smooths be plotted with the
##'   same y-axis scale? The default, `scales = "fixed"`, ensures this is done.
##'   If `scales = "free"` each univariate smooth has its own y-axis scale.
##'   Currently does not affect the y-axis scale of plots of the parametric
##'   terms.
##' @param align characer; see argument `align` in `cowplot::plot_grid()`.
##'   Defaults to `"hv"` so that plots are nicely aligned.
##' @param axis characer; see argument `axis` in `cowplot::plot_grid()`.
##'   Defaults to `"lrtb"` so that plots are nicely aligned.
##' @param ... arguments passed to `cowplot::plot_grid()`. Any arguments to
##'   `plot_grid()` may be supplied, except for: `plotlist` and `align`.
##'
##' @inheritParams evaluate_smooth
##'
##' @return A [ggplot2::ggplot()] object.
##'
##' @author Gavin L. Simpson
##'
##' @importFrom ggplot2 lims
##' @importFrom cowplot plot_grid
##' @export
##'
##' @examples
##' library("mgcv")
##'
##' \dontshow{set.seed(2)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' draw(m1)
`draw.gam` <- function(object,
                       parametric = TRUE,
                       select, # ignored for now; but used for subsetting which smooths
                       scales = c("free", "fixed"),
                       align = "hv", axis = "lrtb",
                       n = 100, unconditional = FALSE, inc_mean = FALSE,
                       dist = 0.1, ...) {
    scales <- match.arg(scales)
    S <- smooths(object)                # vector of smooth labels - "s(x)"

    ## can only plot 1 or 2d smooths - get smooth dimensions & prune list `s`
    d <- smooth_dim(object)
    S <- S[d <= 2L]
    d <- d[d <= 2L]

    ## FIXME: Exclude "re" smooths from "fixed" scales?
    is_re <- vapply(object[["smooth"]], is_re_smooth, logical(1L))

    is_by <- vapply(object[["smooth"]], is_by_smooth, logical(1L))
    if (any(is_by)) {
        S <- vapply(strsplit(S, ":"), `[[`, character(1L), 1L)
    }

    npara <- 0
    nsmooth <- length(S)

    ## are we doing parametric terms?
    if (isTRUE(parametric)) {
        terms <- parametric_terms(object)
        npara <- length(terms)
        p <- vector("list", length = npara)
    }

    l <- vector("list", length = nsmooth)
    g <- vector("list", length = nsmooth + npara)


    for (i in unique(S)) {
        eS <- evaluate_smooth(object, smooth = i, n = n,
                                  unconditional = unconditional,
                              inc_mean = inc_mean, dist = dist)
        l[S == i] <- split(eS, eS[["smooth"]])
    }

    for (i in seq_along(l)) {
        ##l[[i]][["smooth"]] <- droplevels(l[[i]][["smooth"]])
        g[[i]] <- draw(l[[i]])
    }

    if (isTRUE(parametric)) {
        for (i in seq_along(terms)) {
            p[[i]] <- evaluate_parametric_term(object, term = terms[i])
            g[[i + nsmooth]] <- draw(p[[i]])
        }
    }

    if (isTRUE(identical(scales, "fixed"))) {
        wrapper <- function(x) {
            range(x[["est"]] + (2 * x[["se"]]),
                  x[["est"]] - (2 * x[["se"]]))
        }
        ylims <- range(unlist(lapply(l, wrapper)))

        for (i in seq_along(S)[d == 1L]) { # only the univariate smooths; FIXME: "re" smooths too?
            g[[i]] <- g[[i]] + lims(y = ylims)
        }
    }

    plot_grid(plotlist = g, align = align, axis = axis, ...)
}

##' @param qq_line logical; draw a reference line through the lower and upper
##'   theoretical quartiles.
##'
##' @importFrom ggplot2 geom_abline geom_point labs
##' @importFrom stats quantile qnorm
##'
##' @export
##' @rdname draw.evaluated_smooth
`draw.evaluated_re_smooth` <- function(object, qq_line = TRUE, xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL, ...) {
    smooth_var <- names(object)[3L]

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
        ylab <- paste("Effects:", smooth_var)
    }
    if(is.null(title)) {
        title <- smooth_var
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    plt
}

##' @param colour_scale function; an appropriate discrete colour scale from `ggplot2`.
##'
##' @importFrom ggplot2 geom_line theme scale_colour_discrete
##' @export
##' @rdname draw.evaluated_smooth
`draw.evaluated_fs_smooth` <- function(object,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       colour_scale = scale_colour_discrete,
                                       ...) {
    smooth_var <- names(object)[3L]
    smooth_fac <- names(object)[4L]

    plt <- ggplot(object, aes_(x = as.name(smooth_var), y = ~ est,
                               colour = as.name(smooth_fac))) +
        geom_line() +
        scale_colour_discrete() +
        theme(legend.position = "none")

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_var
    }
    if (missing(ylab)) {
        ylab <- unique(object[["smooth"]])
    }
    if (is.null(title)) {
        title <- unique(object[["smooth"]])
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    plt
}

##' @param rug logical; draw a rug plot of the data
##' @param position Position adjustment, either as a string, or the result of a
##'   call to a position adjustment function.
##'
##' @importFrom ggplot2 ggplot geom_pointrange geom_rug geom_ribbon geom_line aes_string
##' @export
##' @rdname draw.evaluated_smooth
`draw.evaluated_parametric_term` <- function(object,
                                             xlab, ylab,
                                             title = NULL, subtitle = NULL,
                                             caption = NULL,
                                             rug = TRUE,
                                             position = "identity",
                                             ...) {
    is_fac <- object[["type"]][1L] == "factor"
    term_label <- object[["term"]][1L]

    plt <- ggplot(object, aes_string(x = "value", y = "partial"))

    if (is_fac) {
        plt <- plt + geom_pointrange(aes_string(ymin = "lower", ymax = "upper"))
    } else {
        if (isTRUE(rug)) {
            plt <- plt + geom_rug(sides = "b", position = position)
        }
        plt <- plt + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"),
                                 alpha = 0.2) +
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

    plt
}

##' Plot derivatives of smooths
##'
##' @param alpha numeric; alpha transparency for confidence or simultaneous
##'   interval.
##' @inheritParams draw.gam
##'
##' @importFrom ggplot2 ggplot geom_ribbon aes_string geom_line labs
##' @importFrom cowplot plot_grid
##' @export
##'
##' @examples
##'
##' library("mgcv")
##' \dontshow{set.seed(42)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
##' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' ## first derivative of all smooths
##' df <- derivatives(mod)
##' draw(df)
`draw.derivatives` <- function(object,
                               select, # ignored for now; but used for subsetting which smooths
                               scales = c("free", "fixed"), alpha = 0.2,
                               align = "hv", axis = "lrtb", ...) {
    scales <- match.arg(scales)

    ## how many smooths
    sm <- unique(object[["smooth"]])
    xvar <- unique(object[["var"]])
    plotlist <- vector("list", length = length(sm))

    for (i in seq_along(sm)) {
        take <- object[["smooth"]] == sm[i]
        df <- object[take, ]
        plotlist[[i]] <- ggplot(df, aes_string(x = "data", y = "derivative")) +
            geom_ribbon(aes_string(ymin = "lower", ymax = "upper", y = NULL),
                        alpha = alpha) +
            geom_line() +
            labs(title = sm[i], x = xvar[i], y = "Derivative")
    }

    if (isTRUE(identical(scales, "fixed"))) {
        ylims <- range(object[["lower"]], object[["upper"]])

        for (i in seq_along(plotlist)) {
            plotlist[[i]] <- plotlist[[i]] + lims(y = ylims)
        }
    }

    plot_grid(plotlist = plotlist, align = align, axis = axis, ...)
}
