#' @title Model diagnostic plots
#'
#' @param model a fitted model. Currently models inheriting from class `"gam"`,
#'   as well as classes `"glm"` and `"lm"` from calls to [stats::glm] or
#'   [stats::lm] are supported.
#' @param method character or `NULL`; reference-quantile method. For quantile
#'   residuals, `NULL` and `"normal"` use a fixed standard-normal reference;
#'   `"uniform"` and `"simulate"` require a traditional residual type instead.
#'   For grouped-family traditional residuals, `NULL` selects `"simulate"` when
#'   every component has a generator, or `"uniform"` with a message when every
#'   component has an inverse CDF. An unavailable reference distribution is an
#'   error; `"normal"` is available only as an explicit approximation.
#'   For ordinary models, `NULL` retains the default `"simulate"` for Tweedie
#'   families and `"uniform"` otherwise, with existing availability fallbacks.
#'   `"uniform"` averages residual reference quantiles obtained by inverse-CDF
#'   sampling over `n_uniform` draws. `"simulate"` uses `n_simulate` simulated
#'   response data sets at the observed covariates and permits reference bands.
#'   `"normal"` uses a location/scale-fitted normal approximation for traditional
#'   residuals. `"direct"` is deprecated in favour of `"uniform"`.
#'
#' @details With `type = NULL`, ordinary models retain deviance residuals.
#'   Models fitted with [mgcv::gfam()] use randomised quantile (Dunn--Smyth)
#'   residuals when every active component has validated CDF and response-bound
#'   semantics. Otherwise, a message identifies unsupported components and all
#'   components use deviance residuals; requesting `type = "quantile"` explicitly
#'   is an error when any component is unsupported. Components never mix
#'   residual scales. Invalid encodings or numerical CDF failures are errors.
#'
#'   Quantile residuals use each component's fitted parameters, dispersion and
#'   weights, including binomial trial counts and Gaussian precision weights.
#'   Gamma components with unit weights, plus negative binomial, beta, scaled-t
#'   and Tweedie components are supported. Non-unit Gamma weights, ordinal,
#'   zero-inflated Poisson,
#'   inverse Gaussian and unrecognised custom-family probability bounds are not
#'   yet supported for quantile diagnostics. A custom CDF alone is insufficient
#'   without validated response-bound and discrete-left-limit semantics.
#'   CDF overrides for recognised families must retain their argument and
#'   probability-bound conventions. Scalar uncensored `cnorm()`, `clog()` and
#'   `cpois()` components are supported; genuine censoring requires recoverable,
#'   aligned censoring bounds. Matrix censored responses rejected by `gfam()`
#'   and upstream fitting failures are not repaired.
#'
#'   `by_family = NULL` pools quantile residuals with colours identifying indexed
#'   family components, including repeated uses of the same family. Traditional
#'   residuals use component facets with free axes by default. The pooled
#'   residual-versus-predictor plot uses within-component predictor ranks:
#'   `(rank(eta, ties.method = "average") - 0.5) / n`, where `n` counts the finite
#'   observations included for that component; constants map to `0.5`.
#'   Facets retain original linear predictors. Use `rank` to override this choice
#'   independently of residual type or layout. The links and original predictor
#'   values can have different meanings across components; ranks compare
#'   positions within components, not effects on a common response scale.
#'   Pooled histograms use common bin edges and component-specific densities.
#'
#'   Quantile QQ and worm plots compare to fixed standard-normal quantiles,
#'   with an identity line or zero line. Their reference bands are exact
#'   pointwise order-statistic bands for an ideal independent standard-normal
#'   sample, conditional on fitted distribution parameters. They do not include
#'   parameter-estimation uncertainty or provide joint coverage. Worm plots
#'   subtract the same theoretical quantiles from residuals and reference bands.
#'   `appraise()` computes one seeded residual vector shared by its residual
#'   panels and always separates observed-versus-fitted values by component,
#'   because response units differ. Standalone [observed_fitted_plot()] allows
#'   explicit pooling. Grouped ordinal models support traditional deviance
#'   QQ/worm plots, but not the observed-versus-fitted panel or full appraisal.
#'
#' @param by_family logical or `NULL`; separate indexed grouped-family components
#'   into facets. The default pools quantile residuals with component colours
#'   and facets traditional residuals. `TRUE` and `FALSE` explicitly select the
#'   layout. In `appraise()`, observed-versus-fitted values always use component
#'   facets. Standalone `observed_fitted_plot()` facets by default and permits
#'   explicit pooling. Ignored for ordinary models.
#' @param rank logical or `NULL`; use within-component ranks on the horizontal
#'   axis of residual-versus-predictor plots. The default ranks pooled grouped
#'   plots and retains original predictors in facets. `TRUE` or `FALSE` overrides
#'   this independently of the residual type and layout. Only finite observations
#'   included in each component determine its ranks; average ties are used.
#' @param use_worm logical; should a worm plot be drawn in place of the QQ plot?
#' @param n_uniform numeric; number of times to randomize uniform quantiles
#'   in the direct computation method (`method = "direct"`) for QQ plots.
#' @param n_simulate numeric; number of data sets to simulate from the estimated
#'   model when using the simulation method (`method = "simulate"`) for QQ
#'   plots.
#' @param seed numeric; the random number seed to use for `method = "simulate"`
#'   and `method = "uniform"`, and for randomised quantile residuals.
#' @param type character or `NULL`; residual type: `"deviance"`, `"pearson"`,
#'   `"response"`, or `"quantile"`. The default selects quantile residuals for
#'   supported grouped-family models and deviance residuals otherwise.
#' @param n_bins character or numeric; either the number of bins or a string
#'   indicating how to calculate the number of bins.
#' @param ncol,nrow numeric; the numbers of rows and columns over which to
#'   spread the plots.
#' @param guides character; one of `"keep"` (the default), `"collect"`, or
#'   `"auto"`. Passed to [patchwork::plot_layout()]
#' @param level numeric; the coverage level for QQ plot reference intervals.
#'   Must be strictly `0 < level < 1`. Used for quantile reference bands and
#'   traditional `method = "simulate"` or `"normal"` bands.
#' @param ci_alpha,ci_col colour and transparency of QQ reference intervals,
#'   including calibrated quantile-residual bands.
#' @param point_col,point_alpha colour and transparency used to draw points in
#'   the plots. See [graphics::par()] section **Color Specification**. This is
#'   passed to the individual plotting functions, and therefore affects the
#'   points of all plots. Pooled grouped residual panels map component identity
#'   to colour instead of using the constant `point_col`.
#' @param line_col colour specification for the 1:1 line in the QQ plot and the
#'   reference line in the residuals vs linear predictor plot.
#' @param ... arguments passed to [patchwork::wrap_plots()].
#'
#' @importFrom patchwork wrap_plots
#'
#' @note The wording used in [mgcv::qq.gam()] uses *direct* in reference to the
#'   simulated residuals method (`method = "simulated"`). To avoid confusion,
#'   `method = "direct"` is deprecated in favour of `method = "uniform"`.
#'
#' @seealso The plots are produced by functions [gratia::qq_plot()],
#'   [gratia::residuals_linpred_plot()], [gratia::residuals_hist_plot()],
#'   and [gratia::observed_fitted_plot()].
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' ## simulate some data...
#' dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
#' ## run some basic model checks
#' appraise(mod, point_col = "steelblue", point_alpha = 0.4)
#'
#' ## To change the theme for all panels use the & operator, for example to
#' ## change the ggplot theme for all panels
#' library("ggplot2")
#' if (packageVersion("ggplot2") <= "3.5.2") {
#'   # Throws warning with ggplot rc 4.0.0 and patchwork 1.3.1 - will be fixed
#'   # in patchwork 1.3.2 - so temporarily skipping during ggplot release
#'   # process
#'   appraise(mod, seed = 42,
#'     point_col = "steelblue", point_alpha = 0.4,
#'     line_col = "black"
#'   ) & theme_minimal()
#' }
`appraise` <- function(model, ...) {
  UseMethod("appraise")
}

#' @rdname appraise
#' @export
`appraise.gam` <- function(
  model,
  method = NULL,
  use_worm = FALSE,
  n_uniform = 10,
  n_simulate = 50,
  seed = NULL,
  type = NULL,
  n_bins = c("sturges", "scott", "fd"),
  ncol = NULL,
  nrow = NULL,
  guides = "keep",
  level = 0.9,
  ci_col = "black", 
  ci_alpha = 0.2,
  point_col = "grey20",
  point_alpha = 1,
  line_col = "red",
  by_family = NULL,
  rank = NULL,
  ...
) {
  check_gfam_appraise(model)
  ## process args
  type <- diagnostic_type(model, type)
  by_family <- diagnostic_by_family(model, type, by_family)
  if (type == "quantile") model <- cache_diagnostic_residuals(model, type, seed)
  if (is.character(n_bins)) {
    n_bins <- match.arg(n_bins)
  }

  if (!is.character(n_bins) && !is.numeric(n_bins)) {
    stop(
      "'n_bins' should be a number or one of: ",
      paste(dQuote(c("sturges", "scott", "fd")), collapse = ", ")
    )
  }

  qq_plot_fun <- if (isTRUE(use_worm)) {
    worm_plot
  } else {
    qq_plot
  }
  plt1 <- qq_plot_fun(
    model,
    method = method,
    type = type,
    n_uniform = n_uniform,
    n_simulate = n_simulate,
    seed = seed,
    level = level,
    ci_col = ci_col,
    ci_alpha = ci_alpha,
    point_col = point_col,
    point_alpha = point_alpha,
    line_col = line_col, by_family = by_family
  )
  plt2 <- residuals_linpred_plot(model,
    type = type, point_col = point_col,
    point_alpha = point_alpha,
    line_col = line_col, by_family = by_family, rank = rank
  )
  plt3 <- residuals_hist_plot(model,
    type = type, n_bins = n_bins,
    subtitle = NULL, by_family = by_family
  )
  plt4 <- observed_fitted_plot(model,
    subtitle = NULL, point_col = point_col,
    point_alpha = point_alpha, by_family = is_gfam_family(family(model))
  )
  ## return
  n_plots <- 4
  layout <- prepare_plot_layout(n_plots, ncol = ncol, nrow = nrow)
  ncol <- layout$ncol
  nrow <- layout$nrow
  wrap_plots(plt1, plt2, plt3, plt4,
    byrow = TRUE, ncol = ncol, nrow = nrow, guides = guides,
    ...
  )
}

#' @rdname appraise
#' @importFrom stats model.frame model.response df.residual
#' @export
`appraise.lm` <- function(model, ...) {
  model <- model_used_rows(model)
  r <- residuals(model)
  r.df <- df.residual(model)
  model[["sig2"]] <- sum((r - mean(r))^2) / r.df
  if (is.null(weights(model))) {
    model$prior.weights <- rep(1, nrow(model.frame(model)))
  }
  if (is.null(model[["linear.predictors"]])) {
    model[["linear.predictors"]] <- model[["fitted.values"]]
  }
  if (is.null(model[["y"]])) {
    model[["y"]] <- model.response(model.frame(model))
  }
  appraise.gam(model, ...)
}
