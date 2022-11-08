#' Plot estimated smooths from a fitted GAM
#'
#' Plots estimated smooths from a fitted GAM model in a similar way to
#' `mgcv::plot.gam()` but instead of using base graphics, [ggplot2::ggplot()]
#' is used instead.
#'
#' @param object a fitted GAM, the result of a call to [mgcv::gam()].
#' @param data a optional data frame that may or may not be used? FIXME!
#' @param select character, logical, or numeric; which smooths to plot. If
#'   `NULL`, the default, then all model smooths are drawn. Numeric `select`
#'   indexes the smooths in the order they are specified in the formula and
#'   stored in `object`. Character `select` matches the labels for smooths
#'   as shown for example in the output from `summary(object)`. Logical
#'   `select` operates as per numeric `select` in the order that smooths are
#'   stored.
#' @param parametric logical; plot parametric terms also? Note that `select` is
#'   used for selecting which smooths to plot. The `terms` argument is used to
#'   select which parametric effects are plotted. The default, as with
#'   [mgcv::plot.gam()], is to not draw parametyric effects.
#' @param terms character; which model parametric terms should be drawn? The
#'   Default of `NULL` will plot all parametric terms that can be drawn.
#' @param residuals logical; should partial residuals for a smooth be drawn?
#'   Ignored for anything but a simple univariate smooth.
#' @param scales character; should all univariate smooths be plotted with the
#'   same y-axis scale? The default, `scales = "fixed"`, ensures this is done.
#'   If `scales = "free"` each univariate smooth has its own y-axis scale.
#'   Currently does not affect the y-axis scale of plots of the parametric
#'   terms.
#' @param constant numeric; a constant to add to the estimated values of the
#'   smooth. `constant`, if supplied, will be added to the estimated value
#'   before the confidence band is computed.
#' @param fun function; a function that will be applied to the estimated values
#'   and confidence interval before plotting. Can be a function or the name of a
#'   function. Function `fun` will be applied after adding any `constant`, if
#'   provided.
#' @param ci_level numeric between 0 and 1; the coverage of credible interval.
#' @param n numeric; the number of points over the range of the covariate at
#'   which to evaluate the smooth.
#' @param n_3d numeric; the number of new observations to generate for the third
#'   dimension of a 3D smooth.
#' @param n_4d numeric; the number of new observations to generate for the
#'   dimensions higher than 2 (!) of a *k*D smooth (*k* >= 4). For example, if
#'   the smooth is a 4D smooth, each of dimensions 3 and 4 will get `n_4d`
#'   new observations.
#' @param unconditional logical; should confidence intervals include the
#'   uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian
#'   covariance matrix will be used.
#' @param overall_uncertainty logical; should the uncertainty in the model
#'  constant term be included in the standard error of the evaluate values of
#'  the smooth?
#' @param dist numeric; if greater than 0, this is used to determine when
#'   a location is too far from data to be plotted when plotting 2-D smooths.
#'   The data are scaled into the unit square before deciding what to exclude,
#'   and `dist` is a distance within the unit square. See
#'   [mgcv::exclude.too.far()] for further details.
#' @param rug logical; draw a rug plot at the botom of each plot for 1-D
#'   smooths or plot locations of data for higher dimensions.
#' @param contour logical; should contours be draw on the plot using
#'   [ggplot2::geom_contour()].
#' @param ci_alpha numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @param ci_col colour specification for the confidence/credible intervals
#'   band. Affects the fill of the interval.
#' @param smooth_col colour specification for the smooth line.
#' @param resid_col colour specification for the partial residuals.
#' @param contour_col colour specification for contour lines.
#' @param n_contour numeric; the number of contour bins. Will result in
#'   `n_contour - 1` contour lines being drawn. See [ggplot2::geom_contour()].
#' @param partial_match logical; should smooths be selected by partial matches
#'   with `select`? If `TRUE`, `select` can only be a single string to match
#'   against.
#' @param discrete_colour,continuous_colour,continuous_fill suitable scales
#'   for the types of data.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param angle numeric; the angle at which the x axis tick labels are to be
#'   drawn passed to the `angle` argument of [ggplot2::guide_axis()].
#' @param ncol,nrow numeric; the numbers of rows and columns over which to
#'   spread the plots
#' @param guides character; one of `"keep"` (the default), `"collect"`, or
#'   `"auto"`. Passed to [patchwork::plot_layout()]
#' @param widths,heights The relative widths and heights of each column and
#'   row in the grid. Will get repeated to match the dimensions of the grid. If
#'   there is more than 1 plot and `widths = NULL`, the value of `widths` will
#'   be set internally to `widths = 1` to accomodate plots of smooths that
#'   use a fixed aspect ratio.
#' @param projection character; projection to use, see [ggplot2::coord_map()]
#'   for details.
#' @param orientation an optional vector `c(latitude, longitude, rotation)`
#'   which describes where the "North Pole" should be when computing the
#'   projection. The third value is a clockwise rotation (in degrees), which
#'   defaults to the midrange of the longitude coordinates in the data. The
#'   default values for `orientation` therefore are
#'   `c(20, 0, mean(range(longitude))))`` if this is not specified by the user.
#'   See links in [ggplot2::coord_map()] for more information.
#' @param ... additional arguments passed to [patchwork::wrap_plots()].
#'
#' @note Internally, plots of each smooth are created using [ggplot2::ggplot()]
#'   and composed into a single plot using [patchwork::wrap_plots()]. As a
#'   result, it is not possible to use `+` to add to the plots in the way one
#'   might typically work with `ggplot()` plots. Instead, use the `&` operator;
#'   see the examples.
#'
#' @return The object returned is created by [patchwork::wrap_plots()].
#'
#' @author Gavin L. Simpson
#'
#' @importFrom ggplot2 scale_colour_discrete scale_colour_continuous
#'   scale_fill_distiller
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr mutate rowwise %>% ungroup left_join summarise group_split
#' @importFrom purrr pluck map_lgl
#' @importFrom rlang expr_label
#' @export
#'
#' @examples
#' load_mgcv()
#'
#' # simulate some data
#' df1 <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' # fit GAM
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df1, method = "REML")
#'
#' # plot all smooths
#' draw(m1)
#'
#' # can add partial residuals
#' draw(m1, residuals = TRUE)
#'
#' df2 <- data_sim(2, n = 1000, dist = "normal", scale = 1, seed = 2)
#' m2 <- gam(y ~ s(x, z, k = 40), data = df2, method = "REML")
#' draw(m2, contour = FALSE, n = 50)
#'
#' # change the number of contours drawn and the fill scale used for
#' # the surface
#' library("ggplot2")
#' draw(m2, n_contour = 5, n = 50,
#'      continuous_fill = scale_fill_distiller(palette = "Spectral",
#'                                             type = "div"))
#'
#' # See https://gavinsimpson.github.io/gratia/articles/custom-plotting.html
#' # for more examples and for details on how to modify the theme of all the
#' # plots produced by draw()
#' # to modify all panels, for example to change the theme, use the & operator
`draw.gam` <- function(object,
                       data = NULL,
                       select = NULL,
                       parametric = FALSE,
                       terms = NULL,
                       residuals = FALSE,
                       scales = c("free", "fixed"),
                       ci_level = 0.95,
                       n = 100,
                       n_3d = 16,
                       n_4d = 4,
                       unconditional = FALSE,
                       overall_uncertainty = TRUE,
                       constant = NULL,
                       fun = NULL,
                       dist = 0.1,
                       rug = TRUE,
                       contour = TRUE,
                       ci_alpha = 0.2,
                       ci_col = "black",
                       smooth_col = "black",
                       resid_col = "steelblue3",
                       contour_col = "black",
                       n_contour = NULL,
                       partial_match = FALSE,
                       discrete_colour = NULL,
                       continuous_colour = NULL,
                       continuous_fill = NULL,
                       position = "identity",
                       angle = NULL,
                       ncol = NULL, nrow = NULL,
                       guides = "keep", widths = NULL, heights = NULL,
                       projection = "orthographic",
                       orientation = NULL,
                       ...) {
    model_name <- expr_label(substitute(object))
    # fixed or free?
    scales <- match.arg(scales)

    # fix up default scales
    if (is.null(discrete_colour)) {
        discrete_colour <- scale_colour_discrete()
    }
    if (is.null(continuous_colour)) {
        continuous_colour <- scale_colour_continuous()
    }
    if (is.null(continuous_fill)) {
        continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
    }

    # if not using select, set parametric TRUE if not set to FALSE
    if (!is.null(select)) {
        if (is.null(parametric)) {
            parametric <- FALSE
        }
    } else {
        if (is.null(parametric)) {
            parametric <- TRUE
        }
    }

    # sort out n_3d and n_4d. If these are `NULL` then do sensible thing at set
    # them small. Default is 3 for n_4d and for 12 for n_3d, but if we have a kD
    # smooth (k >= 4) we want to

    S <- smooths(object) # vector of smooth labels - "s(x)"

    # select smooths
    select <-
        check_user_select_smooths(smooths = S, select = select,
                                  partial_match = partial_match,
                                  model_name = expr_label(substitute(object)))

    # this is needed for the parametric terms below
    sm_plts <- NULL
    ylims <- NULL

    # do we have any smooths to plot?
    if (length(select) > 0L) {
        # evaluate all requested smooths
        sm_eval <- smooth_estimates(object,
                                    smooth = S[select],
                                    n = n,
                                    n_3d = n_3d,
                                    n_4d = n_4d,
                                    data = data,
                                    unconditional = unconditional,
                                    overall_uncertainty = overall_uncertainty,
                                    dist = dist,
                                    unnest = FALSE)

        # add confidence interval
        sm_eval <- sm_eval %>%
          rowwise() %>%
          mutate(data = list(add_confint(.data$data, coverage = ci_level))) %>%
          ungroup()

        # Take the range of the smooths & their confidence intervals now
        # before we put rug and residuals on
        sm_rng <- sm_eval %>%
            rowwise() %>%
            summarise(rng = range(c(data$est, data$lower_ci,
                                    data$upper_ci))) %>%
            pluck("rng")

        # Add partial residuals if requested - by default they are
        # At the end of this, sm_eval will have a new list column containing the
        # partial residuals, `partial_residual`
        p_resids_rng <- NULL
        if (isTRUE(residuals)) {
            if (is.null(residuals(object)) || is.null(weights(object))) {
                residuals <- FALSE
            } else {
                # get residuals in a suitable format
                p_resids <- nested_partial_residuals(object, terms = S[select])

                # compute the range of residuals for each smooth
                p_resids_rng <- p_resids %>%
                    rowwise() %>%
                    summarise(rng =
                        range(.data$partial_residual$partial_residual)) %>%
                    pluck("rng")

                # merge with the evaluated smooth
                sm_eval <- left_join(sm_eval, p_resids, by = "smooth")
            }
        }

        # add rug data?
        if (isTRUE(rug)) {
            # get rug data in a suitable format
            rug_data <- nested_rug_values(object, terms = S[select])

            # merge with the evaluated smooth
            sm_eval <- left_join(sm_eval, rug_data, by = "smooth")
        }

        # need to figure out scales if "fixed"
        if (isTRUE(identical(scales, "fixed"))) {
            ylims <- range(sm_rng, p_resids_rng)
        }

        # draw smooths
        # the factor is to reorder to way the smooths entered the model
        sm_l <- group_split(sm_eval, factor(.data$smooth, levels = S[select]))
        sm_plts <- map(sm_l,
                       draw_smooth_estimates,
                       constant = constant,
                       fun = fun,
                       contour = contour,
                       contour_col = contour_col,
                       n_contour = n_contour,
                       ci_alpha = ci_alpha,
                       ci_col = ci_col,
                       smooth_col = smooth_col,
                       resid_col = resid_col,
                       partial_match = partial_match,
                       discrete_colour = discrete_colour,
                       continuous_colour = continuous_colour,
                       continuous_fill = continuous_fill,
                       angle = angle,
                       ylim = ylims,
                       projection = projection,
                       orientation = orientation)

        #sm_plts <- sm_plts[S] # reorder to way the smooths entered the model

    } # end stuff for smooths...

    # Are we plotting parametric effects too?
    if (isTRUE(parametric)) {
        para <- parametric_effects(object, term = terms, #data = data,
                                   unconditional = unconditional,
                                   unnest = TRUE, ci_level = ci_level)
        # Add CI
        crit <- coverage_normal(ci_level)
        object <- mutate(para,
                         lower = .data$partial - (crit * .data$se),
                         upper = .data$partial + (crit * .data$se))
        # need to alter the ylim if scales are fixed
        if (isTRUE(identical(scales, "fixed"))) {
            ylims <- range(ylims, object$partial, object$upper, object$lower)
        }

        para_plts <- para %>%
          group_by(.data$term) %>%
          group_map(.keep = TRUE,
                    .f = ~ draw_parametric_effect(.x,
                                                  ci_level = ci_level,
                                                  ci_col = ci_col,
                                                  ci_alpha = ci_alpha,
                                                  line_col = smooth_col,
                                                  constant = constant,
                                                  fun = fun,
                                                  rug = rug,
                                                  position = position,
                                                  angle = angle,
                                                  ylim = ylims))
    } # parametric done

    if (isTRUE(parametric)) {
        sm_plts <- append(sm_plts, para_plts)
    }

    # filter out NULLs as those are types of smooths we can't plot (yet)
    no_plot <- map_lgl(sm_plts, is.null)
    sm_plts <- sm_plts[!no_plot]

    if (all(no_plot)) {
        message("Unable to draw any of the model terms.")
        return(invisible())
    }

    # return
    n_plots <- length(sm_plts)
    if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_plots))
        nrow <- ceiling(n_plots / ncol)
    }
    if (n_plots > 1L && is.null(widths)) {
        # it doesn't matter about the widths if only one plot, but if we have
        # more than one plot and the user didn't change `widths`, then we will
        # force a value of 1 to give all plots the same relative width
        widths <- 1
    }
    wrap_plots(sm_plts, byrow = TRUE, ncol = ncol, nrow = nrow,
               guides = guides, widths = widths, heights = heights, ...)
}

#' @export
`draw.list` <- function(object, ...) {
    if (!is_gamm4(object)) {
        stop("Don't know how to draw a list")
    }
    draw(object$gam, ...)
}

#' @export
`draw.gamm` <- function(object, ...) {
    draw(object$gam, ...)
}
