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
##' @param rug For `evaluate_smooth()`, a numeric vector of values for the
##'   location of data on the x axis. The default of `NULL` results in no
##'   rug plot being drawn. For `evaluate_parametric_terms()`, a logical to
##'   indicate if a rug plot should be drawn.
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
##' @aliases draw.evaluated_1d_smooth draw.evaluated_2d_smooth geom_rug
##'
##' @examples
##' suppressPackageStartupMessages(library("mgcv"))
##'
##' \dontshow{set.seed(2)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' sm <- evaluate_smooth(m1, "s(x2)")
##' draw(sm)
##'
##' \dontshow{set.seed(2)}
##' dat <- gamSim(2, n = 1000, dist = "normal", scale = 1)
##' m2 <- gam(y ~ s(x, z, k = 40), data = dat$data, method = "REML")
##'
##' sm <- evaluate_smooth(m2, "s(x,z)", n = 100)
##' draw(sm)
`draw.evaluated_1d_smooth` <- function(object,
                                       rug = NULL,
                                       xlab, ylab,
                                       title = NULL, subtitle = NULL,
                                       caption = NULL,
                                       ...) {
    smooth_var <- names(object)[3L]

    ## Add confidence interval
    object[["upper"]] <- object[["est"]] + (2 * object[["se"]])
    object[["lower"]] <- object[["est"]] - (2 * object[["se"]])

    plt <- ggplot(object, aes_(x = as.name(smooth_var), y = ~ est, group = ~ smooth)) +
        geom_ribbon(mapping = aes_string(ymin = "lower",
                                         ymax = "upper"),
                    alpha = 0.2) +
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
        plt <- plt + geom_rug(data = data.frame(x = rug),
                              mapping = aes_string(x = 'x'),
                              inherit.aes = FALSE,
                              sides = 'b')
    }

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
        guide_title <- "Effect" # unique(object[["smooth"]])
        plot_var <- "est"
        guide_limits <- c(-1, 1) * max(abs(object[[plot_var]]))
    } else {
        guide_title <- "Std. err." # bquote(SE * (.(unique(object[["smooth"]]))))
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
##' @param parametric logical; plot parametric terms also? Default is `TRUE`,
##'   only if `select` is `NULL`. If `select` is used, `parametric` is set to
##'   `FALSE` unless the user specifically sets `parametric = TRUE`.
##' @param select character, logical, or numeric; which smooths to plot. If
##'   `NULL`, the default, then all model smooths are drawn. Numeric `select`
##'   indexes the smooths in the order they are specified in the formula and
##'   stored in `object`. Character `select` matches the labels for smooths
##'   as shown for example in the output from `summary(object)`. Logical
##'   `select` operates as per numeric `select` in the order that smooths are
##'   stored.
##' @param scales character; should all univariate smooths be plotted with the
##'   same y-axis scale? The default, `scales = "fixed"`, ensures this is done.
##'   If `scales = "free"` each univariate smooth has its own y-axis scale.
##'   Currently does not affect the y-axis scale of plots of the parametric
##'   terms.
##' @param align characer; see argument `align` in `cowplot::plot_grid()`.
##'   Defaults to `"hv"` so that plots are nicely aligned.
##' @param axis characer; see argument `axis` in `cowplot::plot_grid()`.
##'   Defaults to `"lrtb"` so that plots are nicely aligned.
##' @param rug logical; draw a rug plot at the botom of each plot?
##' @param partial_match logical; should smooths be selected by partial matches
##'   with `select`? If `TRUE`, `select` can only be a single string to match
##'   against.
##' @param ... arguments passed to `cowplot::plot_grid()`. Any arguments to
##'   `plot_grid()` may be supplied, except for: `plotlist` and `align`.
##'
##' @inheritParams evaluate_smooth
##'
##' @note Internally, plots of each smooth are created using [ggplot2::ggplot()]
##'   and composed into a single plot using [cowplot::plot_grid()]. As a result,
##'   it is not possible to use `+` to add to the plots in the way one might
##'   typically work with `ggplot()` plots.
##'
##' @return The object returned is created by [cowplot::plot_grid()].
##'
##' @author Gavin L. Simpson
##'
##' @importFrom ggplot2 lims
##' @importFrom cowplot plot_grid
##' @export
##'
##' @examples
##' suppressPackageStartupMessages(library("mgcv"))
##'
##' \dontshow{set.seed(2)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' draw(m1)
`draw.gam` <- function(object,
                       parametric = NULL,
                       select = NULL,
                       scales = c("free", "fixed"),
                       align = "hv", axis = "lrtb",
                       n = 100, unconditional = FALSE,
                       overall_uncertainty = TRUE,
                       dist = 0.1, rug = TRUE,
                       partial_match = FALSE, ...) {
    scales <- match.arg(scales)
    S <- smooths(object)                # vector of smooth labels - "s(x)"

    ## if not using select, set parametric TRUE if not set to FALSE
    if (!is.null(select)) {
        if (is.null(parametric)) {
            parametric <- FALSE
        }
    } else {
        if (is.null(parametric)) {
            parametric <- TRUE
        }
    }

    ## select smooths
    select <- check_user_select_smooths(smooths = S, select = select,
                                        partial_match = partial_match)

    ## can only plot 1 or 2d smooths - get smooth dimensions & prune list `s`
    ## d <- smooth_dim(object)[select]
    d <- smooth_dim(object)
    take <- d <= 2L
    select <- select[take]
    S <- S[take]
    d <- d[take]

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
        p <- vector("list", length = npara)    }

    g <- l <- vector("list", length = nsmooth)
    ## g <- vector("list", length = nsmooth + npara)

    for (i in unique(S)) {
        eS <- evaluate_smooth(object, smooth = i, n = n,
                              unconditional = unconditional,
                              overall_uncertainty = overall_uncertainty,
                              dist = dist)
        l[S == i] <- split(eS, eS[["smooth"]])
    }

    ## filter out smooths here using select
    l <- l[select]
    d <- d[select]
    g <- g[select]

    ## If we can't handle any of the terms in the model, bail
    if (length(g) == 0L) {
        message("Unable to draw any of the model terms.")
        return(invisible(g))
    }

    ## model frame may be needed for rugs
    mf  <- model.frame(object)

    for (i in seq_along(l)) {
        if (isTRUE(rug)) {
            sname <- unique(l[[i]][["smooth"]])
            ## could be a by smooth, strip off the by variable bit
            sname <- strsplit(sname, ":")[[1L]][[1L]]
            sm <- get_smooth(object, term = sname)
            if (!is_mgcv_smooth(sm)) {  # could be list (factor by)
                sm <- sm[[1L]]
            }
            svar <- smooth_variable(sm)
            if (is_fs_smooth(sm)) {
                svar <- svar[[1L]]
            }
            g[[i]] <- draw(l[[i]], rug = mf[[svar]])
        } else {
            g[[i]] <- draw(l[[i]])
        }
    }

    if (isTRUE(parametric)) {
        leng <- length(g)
        for (i in seq_along(terms)) {
            p[[i]] <- evaluate_parametric_term(object, term = terms[i])
            g[[i + leng]] <- draw(p[[i]])
        }
    }

    if (isTRUE(identical(scales, "fixed"))) {
        wrapper <- function(x) {
            range(x[["est"]] + (2 * x[["se"]]),
                  x[["est"]] - (2 * x[["se"]]))
        }
        ylims <- range(unlist(lapply(l, wrapper)))
        if (isTRUE(parametric)) {
            ylims <- range(ylims,
                           unlist(lapply(p, function(x) range(x[["upper"]],
                                                              x[["lower"]]))))
        }

        gg <- seq_along(g)[c(d==1L, rep(TRUE, npara))]
        for (i in gg) { # only the univariate smooths; FIXME: "re" smooths too?
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

    plt
}

##' @param colour_scale function; an appropriate discrete colour scale from `ggplot2`.
##'
##' @importFrom ggplot2 geom_line theme scale_colour_discrete geom_rug
##' @export
##' @rdname draw.evaluated_smooth
`draw.evaluated_fs_smooth` <- function(object,
                                       rug = NULL,
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
                              sides = 'b')
    }

    plt
}

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
##' suppressPackageStartupMessages(library("mgcv"))
##' \dontshow{set.seed(42)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
##' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' ## first derivative of all smooths
##' df <- derivatives(mod)
##' draw(df)
`draw.derivatives` <- function(object,
                               select = NULL,
                               scales = c("free", "fixed"), alpha = 0.2,
                               align = "hv", axis = "lrtb", ...) {
    scales <- match.arg(scales)

    ## how many smooths
    sm <- unique(object[["smooth"]])
    ## select smooths
    select <- check_user_select_smooths(smooths = sm, select = select)
    sm <- sm[select]
    xvar <- unique(object[["var"]])[select]
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

##' Plot basis functions
##'
##' Plots basis functions using ggplot2
##' 
##' @param object an object, the result of a call to [basis()].
##' @param legend logical; should a legend by drawn to indicate basis functions?
##' @param use_facets logical; for factor by smooths, use facets to show the
##'   basis functions for each level of the factor? If `FALSE`, a separate ggplot
##'   object will be created for each level and combined using
##'   [cowplot::plot_grid()]. **Currently ignored**.
##' @param labeller a labeller function with which to label facets. The default
##'   is to use [ggplot2::label_both()].
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
##' @param ... arguments passed to other methods. Not used by this method.
##'
##' @return A [ggplot2::ggplot()] object.
##'
##' @author Gavin L. Simpson
##'
##' @importFrom ggplot2 ggplot aes_ labs geom_line guides facet_wrap label_both
##' 
##' @export
##'
##' @examples
##' suppressPackageStartupMessages(library("mgcv"))
##' \dontshow{set.seed(42)}
##' df <- gamSim(4, n = 400, verbose = FALSE)
##'
##' bf <- basis(s(x0), data = df)
##' draw(bf)
##'
##' bf <- basis(s(x2, by = fac, bs = 'bs'), data = df)
##' draw(bf)
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

##' @param alpha numeric; alpha transparency for confidence or simultaneous
##'   interval.
##' @param colour The colour to use to draw the posterior smooths. Passed to
##'   [ggplot2::geom_line()] as argument `colour`. 
##'
##' @export
##'
##' @inheritParams draw.gam
##'
##' @author Gavin L. Simpson
##' 
##' @importFrom dplyr filter
##' @importFrom purrr map
##' @importFrom rlang .data
##'
##' @examples
##' suppressPackageStartupMessages('mgcv')
##' \dontshow{set.seed(1)}
##' dat1 <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
##' ## a single smooth GAM
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat1, method = "REML")
##' ## posterior smooths from m1
##' sm1 <- smooth_samples(m1, n = 15, seed = 23478)
##' ## plot
##' draw(sm1, alpha = 0.7)
##' 
##' \dontshow{set.seed(1)}
##' dat2 <- gamSim(2, n = 4000, dist = "normal", scale = 1, verbose = FALSE)
##' ## a multi-smooth GAM
##' m2 <- gam(y ~ s(x, z, k = 40), data = dat2$data, method = "REML")
##' ## posterior smooths from m1
##' sm2 <- smooth_samples(m2, n = 15, seed = 23478)
##' ## plot
##' draw(sm2, alpha = 0.7)
##' 
##' \dontshow{set.seed(1)}
##' dat3 <- gamSim(4, verbose = FALSE)
##' ## a multi-smooth GAM with a factor-by smooth
##' m3 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat3)
##' ## posterior smooths from m1
##' sm3 <- smooth_samples(m3, n = 15, seed = 23478)
##' ## plot
##' draw(sm3, alpha = 0.7)
##' ## this time selecting only one smooth
##' draw(sm3, select = "s(x0)")
##' ## or selecting the factor-by smooth
##' draw(sm3, select = "s(x2)", partial_match = TRUE, alpha = 0.7)
`draw.smooth_samples` <- function(object,
                                  select = NULL,
                                  xlab = NULL, ylab = NULL, title = NULL,
                                  subtitle = NULL, caption = NULL,
                                  alpha = 1, colour = "black",
                                  scales = c("free", "fixed"),
                                  align = "hv", axis = "lrtb",
                                  rug = TRUE,
                                  partial_match = FALSE, ...) {
    
    scales <- match.arg(scales)

    ## select smooths
    S <- unique(object[["term"]])
    select <- check_user_select_smooths(smooths = S, select = select,
                                        partial_match = partial_match)
    S <- S[select]
    object <- filter(object, .data$term %in% S)

    ## can only plot 1d smooths - currently - prune S but how?
    ## FIXME
    
    do_plot_smooths <- function(i, tbl, ...) {
        tbl <- filter(tbl, .data$term == i)
        plot_posterior_smooths(tbl, ...)
    }

    plts <- map(S, do_plot_smooths, tbl = object, xlab = xlab, ylab = ylab,
                title = title, subtitle = subtitle, caption = caption,
                rug = rug, alpha = alpha, colour = colour)

    if (isTRUE(identical(scales, "fixed"))) {
        ylims <- range(object[["value"]])

        p <- seq_along(plts)
        for (i in p) {
            plts[[i]] <- plts[[i]] + lims(y = ylims)
        }
    }

    plot_grid(plotlist = plts, align = align, axis = axis, ...)
}

`plot_posterior_smooths` <- function(tbl, xlab = NULL, ylab = NULL, title = NULL,
                                     subtitle = NULL, caption = NULL,
                                     rug = TRUE, alpha = 1, colour = "black", ...) {
    data_names <- attr(tbl, "data_names")
    smooth_var <- data_names[[unique(tbl[["term"]])]]
    
    plt <- ggplot(tbl, aes_(x = ~ .x1, y = ~ value, group = ~ draw)) +
        geom_line(alpha = alpha, colour = colour)
    
    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- smooth_var
    }
    if (is.null(ylab)) {
        ylab <- "Effect"
    }
    if (is.null(title)) {
        title <- unique(tbl[["term"]])
    }
    if (all(!is.na(tbl[["by_variable"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(tbl[["by_variable"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(tbl[[by_var]]))
        }
    }
    
    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)
    
    ## add rug?
    if (!is.null(rug)) {
        plt <- plt + geom_rug(mapping = aes_string(x = '.x1'),
                              inherit.aes = FALSE, sides = 'b')
    }
    
    plt
}
