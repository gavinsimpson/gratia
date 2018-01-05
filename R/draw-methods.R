##' Generic plotting via `ggplot2`
##'
##' Generic function for plotting of R objects that uses the `ggplot2` package.
##'
##' @title Generic plotting via `ggplot2`
##' @param object and R object to plot.
##' @param ... arguments passed to other methods.
##'
##' @return A `ggplot2` object
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
##' @param object an object, the result of a call to [tsgam::evaluate_smooth()].
##' @param xlab character or expression; the label for the x axis. If not
##'   supplied, a suitable label will be generated from `object`.
##' @param ylab character or expression; the label for the y axis. If not supplied, a suitable label will be generated from `object`.
##' @param title character or expression; the title for the plot. See
##'   `ggplot2::labs()`.
##' @param subtitle character or expression; the subtitle for the plot. See
##'   `ggplot2::labs()`.
##' @param caption character or expression; the plot caption. See
##'    `ggplot2::labs()`.
##' @param ... arguments passed to other methods.
##'
##' @return A `ggplot2` object
##'
##' @author Gavin L. Simpson
##'
##' @importFrom ggplot2 ggplot aes_string labs geom_line geom_ribbon
##' @importFrom grid unit
##'
##' @export
##' @name draw.evaluated_smooth
##' @alias draw.evaluated_1d_smooth
##'
##' @examples
##' library("mgcv")
##'
##' set.seed(2)
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' sm <- evaluate_smooth(m1, "s(x2)")
##' draw(sm)
##'
##' set.seed(2)
##' dat <- gamSim(2, n = 4000, dist = "normal", scale = 1)
##' m2 <- gam(y ~ s(x, z, k = 40), data = dat$data, method = "REML")
##'
##' sm <- evaluate_smooth(m2, "s(x,z)", n = 100)
##' draw(sm)
##'
##' ## now the standard error the smooth instead
##' draw(sm, what = "se")
`draw.evaluated_1d_smooth` <- function(object,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       ...) {
    smooth_var <- names(object)[2L]

    ## Add confidence interval
    object[["upper"]] <- object[["est"]] + (2 * object[["se"]])
    object[["lower"]] <- object[["est"]] - (2 * object[["se"]])

    plt <- ggplot(object, aes_string(x = smooth_var, y = "est")) +
        geom_ribbon(mapping = aes_string(ymin = "lower",
                                         ymax = "upper"),
                    alpha = 0.2) +
        geom_line()

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- smooth_var
    }
    if (missing(ylab)) {
        ylab <- levels(object[["smooth"]])
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    plt
}

##' @param what character; plot the estimated smooth (`"estimate"`) or its
##'   standard error (`"se"`).
##' @param contour logical; should contours be draw on the plot using
##'   [ggplot2::geom_contour()].
##'
##' @importFrom ggplot2 ggplot aes_string geom_raster geom_contour labs guides guide_colourbar scale_fill_distiller theme
##' @importFrom grid unit
##'
##' @export
##' @rdname draw.evaluated_smooth
`draw.evaluated_2d_smooth` <- function(object, what = c("estimate","se"),
                                       contour = TRUE,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       ...) {
    smooth_vars <- names(object)[2:3]
    what <- match.arg(what)
    if (isTRUE(identical(what, "estimate"))) {
        guide_title <- levels(object[["smooth"]])
        plot_var <- "est"
        guide_limits <- c(-1, 1) * max(abs(object[[plot_var]]))
    } else {
        guide_title <- bquote(SE * (.(levels(object[["smooth"]]))))
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

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## Set the palette
    plt <- plt + scale_fill_distiller(palette = "RdBu", type = "div",
                                      limits = guide_limits)

    ## add guide
    plt <- plt + guides(fill = guide_colourbar(title = guide_title,
                                               direction = "horizontal",
                                               barwidth = grid::unit(0.5, "npc")))

    ## position legend at the
    plt <- plt + theme(legend.position = "top")

    plt
}
