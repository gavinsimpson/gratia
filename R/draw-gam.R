#' Plot smooth and parametric terms from a fitted GAM
#'
#' Plots smooth and parametric contributions from a fitted GAM model using
#' [ggplot2::ggplot()].
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
#' @param parametric logical or `NULL`; plot parametric terms also? The default,
#'   `NULL`, includes parametric terms when `select = NULL`, but excludes them
#'   when smooths are selected explicitly. Use `TRUE` to include parametric terms
#'   regardless of `select`, or `FALSE` to draw only smooths (the previous
#'   default). The `terms` argument selects which parametric effects are plotted;
#'   `select` selects only smooths.
#' @param terms character; which model parametric terms should be drawn? The
#'   Default of `NULL` will plot all parametric terms that can be drawn.
#'   Interactions show their individual contributions to the linear predictor;
#'   main effects and intercepts are not added. Multivariate parametric terms
#'   use the grid and surface controls below, as described in
#'   [parametric_effects()].
#' @param back_transform logical; plot covariates on their original scale?
#'   Defaults to `FALSE`, using model coordinates such as `log(x)` for a term
#'   containing `log(x)`. With `TRUE`, recover the raw covariates, generate a
#'   grid on their original scale and evaluate the model expressions forwards.
#'   Applies to axes, rugs, partial-residual locations and conditioning facets,
#'   including tensor products and parametric interactions. Each smooth axis
#'   must depend on one distinct original model covariate. Functions such as
#'   `foo(x)` are supported when their raw inputs and functions can be recovered;
#'   no inverse function is needed. Missing raw data or unavailable functions
#'   produce an error; supply `data` or `envir` as appropriate.
#'   With supplied prediction `data`, unavailable raw fitting observations
#'   omit rugs and partial residuals; prediction rows are never used as observations.
#'   For precomputed smooth estimates, prepare raw grids with
#'   `smooth_estimates(model, back_transform = TRUE)` before drawing with
#'   `back_transform = TRUE`. Raw inputs supplied through `data` also retain
#'   their coordinates. Transformed-only estimates cannot be relabelled on the
#'   raw scale, including grids carrying typical values from `include_all = TRUE`.
#'   Transformed soap-film boundaries lack retained raw coordinates and must
#'   be drawn with `back_transform = FALSE`.
#'   Parametric estimates retain both scales; multi-column basis
#'   expansions retain raw axes on either setting. Does not transform estimated
#'   effects or their intervals; use `fun` for that.
#' @param re_levels named list of factor level labels to retain in tensor products
#'   with random-effect margins, for example `list(species = c("a", "b"))`.
#'   Fitted factor order and ordered status are retained.
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
#'   which to evaluate a univariate smooth.
#' @param n_2d numeric; the number of points along each of the first two axes
#'   of a smooth surface, including surface panels of higher-dimensional smooths.
#'   The default is 50 in plotting and plot-preparation functions. If `NULL`,
#'   use `n` instead. Ignored when evaluation `data` are supplied. Factor levels
#'   are retained, and curves with only one continuous covariate use `n`.
#' @param n_3d numeric; the number of new observations to generate for the third
#'   dimension of a 3D smooth. Factor conditioning dimensions retain at most
#'   this many levels, taking the first levels in fitted order. Explicit
#'   `re_levels` selections take precedence; supplied `data` are unchanged.
#' @param n_4d numeric; the number of new observations to generate for the
#'   dimensions higher than 2 (!) of a *k*D smooth (*k* >= 4). For example, if
#'   the smooth is a 4D smooth, each of dimensions 3 and 4 will get `n_4d`
#'   new observations. For factor conditioning dimensions, this is the maximum
#'   number of levels retained per dimension, taking the first in fitted order.
#'   Explicit `re_levels` selections take precedence; supplied `data` are unchanged.
#' @param unconditional logical; should confidence intervals include the
#'   uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian
#'   covariance matrix will be used.
#' @param overall_uncertainty logical; should the uncertainty in the model
#'   constant term be included in the standard errors of smooth and parametric
#'   effects? For parametric effects this includes the intercept variance and
#'   its covariance with the term, without changing the partial estimates.
#'   Each term uses the intercept of its own linear predictor.
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
#' @param surface character; one of `"raster"` (the default), `"tile"`, or
#'   `"3d"`. Raster surfaces keep large PDF plots compact; tiles are borderless
#'   vector rectangles. Three-dimensional surfaces use the optional `ggcube`
#'   package, with the displayed partial effect as height and fill. Numeric
#'   smooth and parametric surfaces, including conditioning facets and soap
#'   films, are supported. Curves and factor estimates keep their usual display;
#'   spherical smooths retain their geographic rendering.
#'   Covariate scale changes that produce irregular spacing use rectangles
#'   bounded by adjacent coordinate midpoints for flat surfaces.
#' @param surface_3d named list of view options passed to [ggcube::coord_3d()],
#'   for example `list(yaw = 45, persp = FALSE)`. The default `list()` uses the
#'   default view with `ggcube::light(backface_scale = 1)`, preserving equal
#'   horizontal units for isotropic smooths. Supply `light = ggcube::light(...)`
#'   in this list to override lighting; `ggcube::light(method = "none")` disables
#'   it. An explicitly supplied `light` replaces the entire default light.
#'   Only the coordinate system is used; ggcube's theme settings are not applied.
#'   These options affect all 3D panels in a call. For different views per term,
#'   modify individual plots returned by [assemble()]. The `dist` entry here
#'   controls perspective; the top-level `dist` argument controls masking.
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
#' @param default_crs the coordinate reference system (CRS) to use for non-sf
#'   layers added to spherical plots. The default, `NULL`, uses 4326 (WGS84).
#'   Internally generated surface cells, contours and observation points have
#'   their own geographic CRS, with longitude on the horizontal axis and
#'   latitude on the vertical axis. See [ggplot2::coord_sf()] for more details.
#' @param lims_method character; affects how spherical plot axis limits are
#'   determined. The default, `"geometry_bbox"`, uses the rendered geometry.
#'   Passed to [ggplot2::coord_sf()]. Spherical surfaces use explicit
#'   geographic cells with valid polar and date-line boundaries. Orthographic
#'   plots clip cells, contours and observation points to the visible hemisphere
#'   before projection. Explicit prediction data on a rectilinear grid retain
#'   their actual spacing; scattered or singleton coordinates are drawn as
#'   points, with a message, because they do not define surface cells. Incomplete
#'   observation rug coordinates are omitted. Grouped factor-by spherical
#'   surfaces use separate facets for each factor level.
#' @param wrap logical; wrap plots as a patchwork? If \code{FALSE}, a list of
#'   ggplot objects is returned, 1 per term plotted.
#' @param caption logical; show the smooth type in the caption of each plot?
#' @param envir an optional environment for looking up data, functions and
#'   constants. Defaults to the model's evaluation environment.
#' @param ... additional arguments passed to [patchwork::wrap_plots()].
#'
#' @details
#' For models with multiple linear predictors, smooth plot subtitles identify
#' the model part for glmmTMB models or the distributional parameter for mgcv
#' models. Single-predictor models omit these automatic labels; by-variable
#' information and custom subtitles are still shown. Constant glmmTMB dispersion
#' alone does not count as an additional predictor.
#' For example, `gaulss()` uses "Mean (mu)" and "Standard deviation (sigma)",
#' while `twlss()` uses "Mean (mu)", "Power (p)" and "Dispersion (phi)".
#' Shared smooths name each predictor to which they contribute. Families without
#' a known parameter mapping use numbered linear predictors. The existing smooth
#' titles and `select` values are unchanged, and all smooths are drawn by default.
#' By-variable information is appended to the default subtitle. Supply `subtitle`
#' through `...` to replace it, or `subtitle = ""` to suppress it; `NULL` uses the
#' default. Custom `title`, `xlab` and `ylab` can also be supplied through `...`.
#' These labels apply to smooth panels.
#'
#' Random-effect tensor plots put continuous covariates before factors. Two
#' continuous covariates form a surface, with remaining factor levels and numeric
#' slices forming conditioning panels, consistently for `te()`, `ti()` and `t2()`.
#' One continuous covariate gives curves for categorical combinations;
#' categorical-only terms give point ranges. Up to four covariates are supported.
#' Numeric conditioning slices use `n_3d` or `n_4d`. For factor conditioning
#' dimensions, these controls retain at most that many levels, taking the first
#' levels in fitted order. Explicit `re_levels` selections take precedence.
#' Supplied `data` and precomputed estimates retain their rows; select specific
#' factor levels with `re_levels`, or use [data_slice()] to fix conditioning
#' covariates. Observation layers do not introduce additional surface panels.
#'
#' Three-dimensional faces retain the current diverging RdBu fill scale and
#' effect-range rules, shared across conditioning facets. `continuous_fill`
#' overrides the fill scale. Faces have no outlines; `contour_col` controls
#' contour lines. Surface faces are fully opaque. Faces, contours, and floor
#' observations are depth-sorted together, so nearer faces hide the parts behind
#' them. The existing theme and facet styling are retained.
#'
#' The first two displayed covariates form the horizontal axes, and the effect
#' (or standard error when requested) forms the vertical axis. Additional
#' covariates retain their existing conditioning facets: `n_2d` controls surface
#' resolution, while `n_3d` and `n_4d` control covariate slices. Fixed effect
#' ranges are also used for the vertical axis. Confidence surfaces are not added.
#'
#' Contours follow the displayed mesh, and available observations are projected
#' onto its floor. Observations are not assigned to conditioning slices. Soap
#' boundary loops are also projected onto the floor.
#'
#' Supplied scattered evaluation data use piecewise-linear Delaunay triangles
#' and require the optional `geometry` package. Support masking and
#' soap-domain checks require `sf`. Faces crossing known excluded regions
#' are discarded conservatively. Fully masked slices remain empty; insufficient
#' non-collinear locations or conflicting heights at one location are errors.
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
#' draw(m2, contour = FALSE, n_2d = 50)
#' draw(m2, surface = "tile", n_2d = 30)
#'
#' if (requireNamespace("ggcube", quietly = TRUE)) {
#'   draw(m2, surface = "3d", n_2d = 20,
#'     surface_3d = list(yaw = 45, persp = FALSE))
#'   panels <- assemble(m2, surface = "3d", n_2d = 20)
#'   # Replace coordinates to customize a single term; retain the current theme.
#'   panels[[1]] <- panels[[1]] +
#'     ggcube::coord_3d(yaw = -20,
#'       light = ggcube::light(backface_scale = 1))[[1]]
#'   patchwork::wrap_plots(panels)
#' }
#'
#' # See https://gavinsimpson.github.io/gratia/articles/custom-plotting.html
#' # for more examples and for details on how to modify the theme of all the
#' # plots produced by draw(). To modify all panels, for example to change the
#' # theme, use the & operator
`draw.gam` <- function(
  object,
  data = NULL,
  select = NULL,
  parametric = NULL,
  terms = NULL,
  residuals = FALSE,
  scales = c("free", "fixed"),
  ci_level = 0.95,
  n = 100,
  n_2d = 50,
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
  lims_method = "geometry_bbox",
  wrap = TRUE,
  caption = TRUE,
  envir = NULL,
  surface = c("raster", "tile", "3d"),
  surface_3d = list(),
  back_transform = FALSE,
  re_levels = NULL,
  ...
) {
  surface <- match.arg(surface)
  validate_surface_3d(surface_3d)
  check_surface_dots(match.call(expand.dots = FALSE))


  # run assemble() without calling it all directly
  .call <- match.call()
  .call[[1L]] <- quote(gratia::assemble)
  sm_plts <- eval(.call, parent.frame())

  if (is.null(sm_plts)) {
    return(invisible())
  }

  n_plots <- length(sm_plts)
  layout <- prepare_plot_layout(n_plots, ncol = ncol, nrow = nrow)
  ncol <- layout$ncol
  nrow <- layout$nrow
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
`draw.gamm4` <- function(object, ...) {
  draw(gamm4_gam(object), ...)
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
