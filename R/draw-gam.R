#' Plot estimated smooths from a fitted GAM
#'
#' Plots estimated smooths from a fitted GAM model in a similar way to
#' `mgcv::plot.gam()` but instead of using base graphics, [ggplot2::ggplot()]
#' is used instead.
#'
#' @param object a fitted GAM, the result of a call to [mgcv::gam()].
#' @param data an optional data frame that is used to supply the data at which
#'   the smooths will be evaluated and plotted. This is usually not needed, but
#'   is an option if you need fine control over exactly what data are used for
#'   plotting.
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
#'   [mgcv::plot.gam()], is to not draw parametric effects.
#' @param terms character; which model parametric terms should be drawn? The
#'   Default of `NULL` will plot all parametric terms that can be drawn.
#' @param residuals logical; should partial residuals for a smooth be drawn?
#'   Ignored for anything but a simple univariate smooth.
#' @param scales character; should all univariate smooths be plotted with the
#'   same y-axis scale? If `scales = "free"`, the default, each univariate
#'   smooth has its own y-axis scale. If `scales = "fixed"`, a common y axis
#'   scale is used for all univariate smooths.
#'
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
#' @param rug logical; draw a rug plot at the bottom of each plot for 1-D
#'   smooths or plot locations of data for higher dimensions.
#' @param distinct_rug logical; should the data for the rug plots be reduced to
#'   the distinct values?
#' @param contour logical; should contours be draw on the plot using
#'   [ggplot2::geom_contour()].
#' @param grouped_by logical; should factor by smooths be drawn as one panel
#'   per level of the factor (`FALSE`, the default), or should the individual
#'   smooths be combined into a single panel containing all levels (`TRUE`)?
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
#' @param discrete_colour a suitable colour scale to be used when plotting
#'   discrete variables.
#' @param discrete_fill a suitable fill scale to be used when plotting
#'   discrete variables.
#' @param continuous_colour a suitable colour scale to be used when plotting
#'   continuous variables.
#' @param continuous_fill a suitable fill scale to be used when plotting
#'   continuous variables.
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
#'   be set internally to `widths = 1` to accommodate plots of smooths that
#'   use a fixed aspect ratio.
#' @param crs the coordinate reference system (CRS) to use for the plot. All
#'   data will be projected into this CRS. See [ggplot2::coord_sf()] for
#'   details.
#' @param default_crs the coordinate reference system (CRS) to use for the
#'   non-sf layers in the plot. If left at the default `NULL`, the CRS used is
#'   4326 (WGS84), which is appropriate for spline-on-the-sphere smooths, which
#'   are parameterized in terms of latitude and longitude as coordinates. See
#'   [ggplot2::coord_sf()] for more details.
#' @param lims_method character; affects how the axis limits are determined. See
#'   [ggplot2::coord_sf()]. Be careful; in testing of some examples, changing
#'   this to `"orthogonal"` for example with the chlorophyll-a example from
#'   Simon Wood's GAM book quickly used up all the RAM in my test system and the
#'   OS killed R. This could be incorrect usage on my part; right now the grid
#'   of points at which SOS smooths are evaluated (if not supplied by the user)
#'   can produce invalid coordinates for the corners of tiles as the grid is
#'   generated for tile centres without respect to the spacing of those tiles.
#' @param wrap logical; wrap plots as a patchwork? If \code{FALSE}, a list of
#'   ggplot objects is returned, 1 per term plotted.
#' @param caption logical; show the smooth type in the caption of each plot?
#' @param envir an environment to look up the data within.
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
#' @importFrom dplyr mutate rowwise ungroup left_join group_split summarise
#' @importFrom purrr pluck map_lgl in_parallel map
#' @importFrom rlang expr_label
#' @importFrom utils packageVersion getFromNamespace
#' @importFrom stringr str_split_fixed
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
#' df2 <- data_sim("eg2", n = 1000, dist = "normal", scale = 1, seed = 2)
#' m2 <- gam(y ~ s(x, z, k = 40), data = df2, method = "REML")
#' draw(m2, contour = FALSE, n = 50)
#'
#' # See https://gavinsimpson.github.io/gratia/articles/custom-plotting.html
#' # for more examples and for details on how to modify the theme of all the
#' # plots produced by draw(). To modify all panels, for example to change the
#' # theme, use the & operator
`draw.gam` <- function(
  object,
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
  distinct_rug = TRUE,
  contour = TRUE,
  grouped_by = FALSE,
  ci_alpha = 0.2,
  ci_col = "black",
  smooth_col = "black",
  resid_col = "steelblue3",
  contour_col = "black",
  n_contour = NULL,
  partial_match = FALSE,
  discrete_colour = NULL,
  discrete_fill = NULL,
  continuous_colour = NULL,
  continuous_fill = NULL,
  position = "identity",
  angle = NULL,
  ncol = NULL, nrow = NULL,
  guides = "keep", widths = NULL, heights = NULL,
  crs = NULL,
  default_crs = NULL,
  lims_method = "cross",
  wrap = TRUE,
  caption = TRUE,
  envir = environment(formula(object)),
  ...
) {

  # run assemble() without calling it all directly
  #.call <- match.call()
  .call <- match.call(expand.dots = FALSE)
  .call[[1L]] <- quote(gratia::assemble)
  sm_plts <- eval(.call, parent.frame())

  if (is.null(sm_plts)) {
    return(invisible())
  }

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

  if (wrap) {
    sm_plts <- wrap_plots(
      sm_plts,
      byrow = TRUE, ncol = ncol, nrow = nrow,
      guides = guides, widths = widths, heights = heights, ...
    )
  }

  # return
  sm_plts
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

#' @export
`draw.scam` <- function(object, ..., overall_uncertainty = FALSE) {
  class(object) <- append(class(object), "gam", after = 1)
  draw.gam(object, overall_uncertainty = FALSE, ...)
}

# TODO: Remove after dplyr 1.1.0 is released and use `multiple = "all"` instead
suppress_matches_multiple_warning <- function(expr) {
  handler_matches_multiple <- function(cnd) {
    if (inherits(cnd, "dplyr_warning_join_matches_multiple")) {
      restart <- findRestart("muffleWarning")
      if (!is.null(restart)) {
        invokeRestart(restart)
      }
    }
  }
  withCallingHandlers(expr, warning = handler_matches_multiple)
}
