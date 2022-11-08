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

#' Plot derivatives of smooths
#'
#' @param alpha numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @inheritParams draw.gam
#'
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line labs
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
                               angle = NULL,
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
        plt <- if (!all(is.na(df$fs_var))) {
            ggplot(df, aes(x = .data$data,
                           y = .data$derivative,
                           group = .data$fs_var))
        } else {
            ggplot(df, aes(x = .data$data,
                           y = .data$derivative)) +
              geom_ribbon(aes(ymin = .data$lower,
                              ymax = .data$upper,
                              y = NULL), alpha = alpha) 
        }
        plotlist[[i]] <- plt +
            geom_line() +
            labs(title = sm[i], x = xvar, y = "Derivative") +
            guides(x = guide_axis(angle = angle))
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
    wrap_plots(plotlist, byrow = TRUE, ncol = ncol, nrow = nrow,
               guides = guides, ...)
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
        plt <- if (!all(is.na(df$fs_var))) {
            ggplot(df, aes(x = .data$data,
                           y = .data$partial_deriv,
                           group = .data$fs_var))
        } else {
            ggplot(df, aes(x = .data$data,
                           y = .data$partial_deriv)) +
              geom_ribbon(aes(ymin = .data$lower,
                              ymax = .data$upper,
                              y = NULL), alpha = alpha)
        }
        plotlist[[i]] <- plt +
            geom_line() +
            labs(title = sm[i], x = xvar,
            y = paste("Partial derivative with respect to", xvar)) +
            guides(x = guide_axis(angle = angle))
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
    wrap_plots(plotlist, byrow = TRUE, ncol = ncol, nrow = nrow,
               guides = guides, ...)
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
        labeller  <- label_both
    }

    ## basis plot
    plt <- ggplot(object, aes(x = .data[[smooth_var]],
        y = .data[["value"]],
        colour = .data[["bf"]])) +
        geom_line() +
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
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
        caption = caption, colour = "Basis\nfunction")

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
#'
#' @return A `patchwork` object.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom purrr map map_lgl
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr group_split
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' df <- data_sim("eg1", n = 400, seed = 42)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' bf <- basis(m)
#' draw(bf)
#'
#' bf <- basis(m, "s(x2)")
#' draw(bf)
`draw.basis` <- function(object,
                         legend = FALSE,
                         labeller = NULL,
                         ylab = NULL,
                         title = NULL, subtitle = NULL,
                         caption = NULL,
                         ncol = NULL, nrow = NULL,
                         angle = NULL,
                         guides = "keep",
                         contour = FALSE,
                         n_contour = 10,
                         contour_col = "black",
                         ...) {
    sm <- unique(object[["smooth"]])
    sm_l <- group_split(object, factor(.data$smooth, levels = sm))
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
        ...)

    # filter out NULLs as those are types of smooths we can't plot (yet)
    no_plot <- map_lgl(sm_plts, is.null)
    sm_plts <- sm_plts[!no_plot]

    if (all(no_plot)) {
        message("Unable to draw any of the bases.")
        return(invisible())
    }

    # return
    n_plots <- length(sm_plts)
    if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_plots))
        nrow <- ceiling(n_plots / ncol)
    }
    wrap_plots(sm_plts, byrow = TRUE, ncol = ncol, nrow = nrow,
               guides = guides, ...)
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
                         ...) {
    ## capture the univariate smooth variable
    smooth_var <- vars_from_label(unique(object[["smooth"]]))
    n_sms <- length(smooth_var)

    plt <- switch(n_sms,
        plot_univariate_basis(object, legend = legend,
            labeller = labeller, xlab = xlab, ylab = ylab,
            title = title, subtitle = subtitle, caption = caption,
            angle = angle, ...),
        plot_bivariate_basis(object, legend = legend,
            labeller = labeller, xlab = xlab, ylab = ylab,
            title = title, subtitle = subtitle, caption = caption,
            angle = angle, contour = contour, n_contour = n_contour,
            contour_col = contour_col, ...))

    plt
}

`plot_univariate_basis` <- function(object,
                                    legend = FALSE,
                                    labeller = NULL,
                                    xlab = NULL, ylab = NULL,
                                    title = NULL, subtitle = NULL,
                                    caption = NULL,
                                    angle = NULL,
                                    ...) {
    smooth_var <- vars_from_label(unique(object[["smooth"]]))

    ## default labeller
    if (is.null(labeller)) {
        labeller  <- label_both
    }

    ## basis plot
    plt <- ggplot(object, aes(x = .data[[smooth_var]],
        y = .data[["value"]],
        colour = .data[["bf"]])) +
        geom_line() +
        guides(x = guide_axis(angle = angle))

    ## default labels if none supplied
    if (is.null(xlab)) {
        xlab <- smooth_var
    }
    if (is.null(ylab)) {
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
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
        caption = caption, colour = "Basis\nfunction")

    ## draw a guide?
    if (!legend) {
        plt <- plt + guides(colour = "none")
    }

    plt
}

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
                                   ...) {
    smooth_var <- vars_from_label(unique(object[["smooth"]]))

    ## default labeller
    if (is.null(labeller)) {
        labeller  <- label_both
    }

    ## basis plot
    plt <- ggplot(object, aes(x = .data[[smooth_var[1]]],
        y = .data[[smooth_var[2]]],
        fill = .data[["value"]],
        group = .data[["bf"]])) +
        geom_raster() +
        guides(x = guide_axis(angle = angle)) +
        scale_fill_distiller(palette = "RdBu", type = "div")

    ## default labels if none supplied
    if (is.null(xlab)) {
        xlab <- smooth_var[2]
    }
    if (is.null(ylab)) {
        ylab <- smooth_var[2]
    }
    flab <- "value"
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
    } else {
        plt <- plt + facet_wrap(vars(.data[["bf"]]), labeller = labeller)
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
        caption = caption, fill = flab)

    ## draw a guide?
    if (!legend) {
        plt <- plt + guides(fill = "none")
    }

    if (isTRUE(contour)) {
        plt <- plt + geom_contour(mapping = aes(z = .data[["value"]]),
            colour = contour_col,
            bins = n_contour,
            na.rm = TRUE)
    }

    if (str_detect(object[["type"]][1L], "TPRS")) {
        plt <- plt + coord_equal()
    }

    plt
}

#' Plot posterior smooths
#'
#' @param n_samples numeric; if not `NULL`, sample `n_samples` from the
#'   posterior draws for plotting.
#' @param seed numeric; random seed to be used to if sampling draws.
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
#'
#' @examples
#' load_mgcv()
#' dat1 <- data_sim("eg1", n = 400, dist = "normal", scale = 1, seed = 1)
#' ## a single smooth GAM
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat1, method = "REML")
#' ## posterior smooths from m1
#' sm1 <- smooth_samples(m1, n = 15, seed = 23478)
#' ## plot
#' draw(sm1, alpha = 0.7)
#' ## plot only 5 randomly smapled draws
#' draw(sm1, n_samples = 5, alpha = 0.7)
#'
#' ## A factor-by smooth example
#' dat2 <- data_sim("eg4", n = 400, dist = "normal", scale = 1, seed = 1)
#' ## a multi-smooth GAM with a factor-by smooth
#' m2 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat2, method = "REML")
#' ## posterior smooths from m1
#' sm2 <- smooth_samples(m2, n = 15, seed = 23478)
#' ## plot, this time selecting only the factor-by smooth
#' draw(sm2, select = "s(x2)", partial_match = TRUE, alpha = 0.7)
#'
#' \donttest{
#' ## A 2D smooth example
#' dat3 <- data_sim("eg2", n = 400, dist = "normal", scale = 1, seed = 1)
#' ## fit a 2D smooth
#' m3 <- gam(y ~ te(x, z), data = dat3, method = "REML")
#' ## get samples
#' sm3 <- smooth_samples(m3, n = 10)
#' ## plot just 6 of the draws, with contour line overlays
#' draw(sm3, n_samples = 6, contour = TRUE, seed = 42)
#' }
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
                object = object, n_samples = n_samples, seed = seed,
                xlab = xlab, ylab = ylab,
                title = title, subtitle = subtitle, caption = caption,
                rug = rug, alpha = alpha, colour = colour,
                contour = contour, n_contour = n_contour,
                contour_col = contour_col, angle = angle)

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

    xvars <- unique(object[["term"]])
    xvars <- vars_from_label(xvars)
    n_xvars <- length(xvars)

    ## randomly sample n_samples from the posterior draws
    n_draws <- max(object[["draw"]]) # how many draws?
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
        draws <- unique(object[["draw"]])
        draws <- sample(draws, n_samples)
        object <- filter(object, .data$draw %in% draws)
    }

    plt <- if (identical(n_xvars, 1L)) {
        draw_1d_posterior_smooths(object, rug = rug,
                                  alpha = alpha, colour = colour,
                                  xlab = xlab, ylab = ylab,
                                  title = title, subtitle = subtitle,
                                  caption = caption, angle = angle, ...)
    } else if (identical(n_xvars, 2L)) {
        draw_2d_posterior_smooths(object, contour = contour,
                                  contour_col = contour_col,
                                  n_contour = n_contour,
                                  xlab = xlab, ylab = ylab,
                                  title = title, subtitle = subtitle,
                                  caption = caption, angle = angle, ...)
    } else if (identical(n_xvars, 3L)) {
        draw_3d_posterior_smooths(object, contour = contour,
                                  contour_col = contour_col,
                                  n_contour = n_contour,
                                  xlab = xlab, ylab = ylab,
                                  title = title, subtitle = subtitle,
                                  caption = caption, angle = angle, ...)
    } else {
        message("Can't plot samples of smooths of more than 3 variables.")
        NULL
    }

    plt #return
}

#' @importFrom rlang .data
#' @importFrom dplyr distinct
`draw_1d_posterior_smooths` <- function(object, xlab = NULL, ylab = NULL,
                                        title = NULL, subtitle = NULL,
                                        caption = NULL, rug = TRUE, alpha = 1,
                                        colour = "black", angle = NULL) {
    data_names <- attr(object, "data_names")
    smooth_var <- data_names[[unique(object[["term"]])]]

    plt <- ggplot(object, aes(x = .data[[".x1"]],
                              y = .data[["value"]],
                              group = .data[["draw"]])) +
        geom_line(alpha = alpha, colour = colour) +
        guides(x = guide_axis(angle = angle))

    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- smooth_var
    }
    if (is.null(ylab)) {
        ylab <- "Partial effect"
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
        plt <- plt + geom_rug(data = distinct(object, .data$.x1),
                              mapping = aes(x = .data[[".x1"]]),
                              inherit.aes = FALSE, sides = "b", alpha = 0.5)
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
    plt <- ggplot(object, aes(x = .data[[".x1"]],
                              y = .data[[".x2"]])) +
        geom_raster(aes(fill = .data[["value"]])) +
        guides(x = guide_axis(angle = angle))

    if (contour) {
        plt <- plt + geom_contour(aes(z = .data[["value"]]),
                                      bins = n_contour,
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
        guides(fill = guide_colourbar(title = "Partial effect",
                                      direction = "vertical",
                                      barheight = grid::unit(0.25, "npc")))

    # if isotropic smooth, fix aspect ratio
    if (any(str_detect(unique(object$type),
                       c("^TPRS", "^Duchon", "^GP", "^SOS")))) {
        plt <- plt + coord_equal()
    }

    plt
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
#' @param ci_alpha numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @param ci_col colour specification for the confidence/credible intervals
#'   band. Affects the fill of the interval.
#' @param smooth_col colour specification for the the smooth or difference line.
#' @param line_col colour specicification for drawing reference lines
#' @param ncol,nrow numeric; the numbers of rows and columns over which to
#'   spread the plots
#' @param xlab,ylab,title,subtitle,caption character; labels with which to
#'   annotate plots
#' @param guides character; one of `"keep"` (the default), `"collect"`, or
#'   `"auto"`. Passed to [patchwork::plot_layout()]
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
                                     ci_col= "black",
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
                                     angle = NULL, ...) {
    scales <- match.arg(scales)

    ## how many smooths
    sm <- unique(object[["smooth"]])
    ## select smooths
    select <- check_user_select_smooths(smooths = sm, select = select)
    sm <- sm[select]

    plotlist <- vector("list", length = length(sm))

    df_list <- split(object, f = paste(object$level_1, object$level_2,
                     sep = "-"))

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
                    subtitle = subtitle, caption = caption, angle = angle)

    if (isTRUE(identical(scales, "fixed"))) {
        ylims <- range(object[["lower"]], object[["upper"]])

        for (i in seq_along(plotlist)) {
            plotlist[[i]] <- plotlist[[i]] + lims(y = ylims)
        }
    }

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
                              ci_col = NULL,
                              smooth_col = NULL,
                              line_col = NULL,
                              contour_col = "black",
                              n_contour = NULL,
                              xlab = NULL, ylab = NULL,
                              title = NULL, subtitle = NULL, caption = NULL,
                              angle = NULL) {
    xvars <- unique(object[["smooth"]])
    xvars <- vars_from_label(xvars)
    n_xvars <- length(xvars)
    plt <- if (identical(n_xvars, 1L)) {
      draw_1d_difference(object, xvars, rug = rug, ref_line = ref_line,
                         ci_alpha = ci_alpha, smooth_col = smooth_col,
                         line_col = line_col,
                         ci_col = ci_col, xlab = xlab, ylab = ylab,
                         title = title, subtitle = subtitle,
                         caption = caption, angle = angle)
    } else if (identical(n_xvars, 2L)) {
        draw_2d_difference(object, xvars, contour = contour,
                           contour_col = contour_col, n_contour = n_contour,
                           xlab = xlab, ylab = ylab,
                           title = title, subtitle = subtitle,
                           caption = caption, angle = angle)
    } else if (identical(n_xvars, 3L)) {
        draw_3d_difference(object, xvars, contour = contour,
                           contour_col = contour_col, n_contour = n_contour,
                           xlab = xlab, ylab = ylab,
                           title = title, subtitle = subtitle,
                           caption = caption, angle = angle)
    } else if (identical(n_xvars, 4L)) {
        draw_4d_difference(object, xvars, contour = contour,
                           contour_col = contour_col, n_contour = n_contour,
                           xlab = xlab, ylab = ylab,
                           title = title, subtitle = subtitle,
                           caption = caption, angle = angle)
    } else {
        message("Can't plot differences for smooths of more than 4 variables.")
        NULL
    }

    plt #return
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
    sm_label <- unique(object$smooth)
    by_var <- unique(object$by)
    f1 <- unique(object$level_1)
    f2 <- unique(object$level_2)
    plt_subtitle <- if (is.null(subtitle)) {
        bquote("Comparison:" ~ .(f1) - .(f2))
    } else {
        subtitle
    }
    y_label <- if (is.null(ylab)) {
        "Difference"
    } else {
        ylab
    }
    plt_title <- if (is.null(title)) {
        paste(sm_label, "by", by_var)
    } else {
        title
    }

    plt <- ggplot(object, aes(x = .data[[xvars[1L]]],
                              y = .data$diff)) +
        guides(x = guide_axis(angle = angle))

    if (isTRUE(ref_line)) {
        plt <- plt + geom_hline(yintercept = 0, colour = line_col)
    }
    plt <- plt +
        geom_ribbon(aes(ymin = .data$lower,
                        ymax = .data$upper,
                        y = NULL),
                    alpha = ci_alpha, fill = ci_col, colour = NA) +
        geom_line(colour = smooth_col) +
            labs(title = plt_title, x = xvars, y = y_label,
                 subtitle = plt_subtitle)

    if (isTRUE(rug)) {
        plt <- plt + geom_rug(sides = "b", alpha = 0.5)
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
                                 caption = NULL, angle = NULL) {
    if (is.null(xlab)) {
        xlab <- xvars[1]
    }
    if (is.null(ylab)) {
        ylab <- xvars[2]
    }
    sm_label <- unique(object$smooth)
    by_var <- unique(object$by)
    f1 <- unique(object$level_1)
    f2 <- unique(object$level_2)
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

    plt <- ggplot(object, aes(x = .data[[xvars[1L]]],
                              y = .data[[xvars[2L]]])) +
        geom_raster(aes(fill = .data$diff)) +
        guides(x = guide_axis(angle = angle))

    if (contour) {
        plt <- plt + geom_contour(aes(z = .data$diff),
                                  bins = n_contour,
                                  colour = contour_col)
    }

    plt <- plt +
        labs(title = plt_title, x = xlab, y = ylab, subtitle = plt_subtitle,
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
                                 caption = NULL, angle = NULL) {
    warning("Plotting differences of 3D smooths is not yet implemented")
    return(NULL)
}

`draw_4d_difference` <- function(object, xvars, contour = FALSE,
                                 contour_col = "black", n_contour = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL, angle = NULL) {
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
    wrap_plots(plt_list, byrow = TRUE, ncol = ncol, nrow = nrow,
               guides = guides,
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
                     row = factor(.data$row,
                                  levels = rev(sort(unique(.data$row)))),
                     col = factor(.data$col, levels = sort(unique(.data$col))))

    ## rescale to -1 -- 1
    if (as.logical(normalize)) {
        object <- mutate(object, value = norm_minus_one_to_one(.data$value))
    }

    ## base plot
    plt <- ggplot(object,
                  aes(x = .data$col,
                      y = .data$row,
                      fill = .data$value)) +
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
