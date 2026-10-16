#' @title Model diagnostic plots
#'
#' @param model a fitted model supported by [diagnostic_support], including
#'   lm, glm, gam/bam, gamm/gamm4, glmmTMB, univariate GJRM and sdmTMB.
#' @param method character or `NULL`; how to calculate reference quantiles.
#'   Probability integral transform (PIT) and quantile residuals use fixed
#'   uniform and standard-normal references, respectively. For quantile residuals, `"normal"` also selects that fixed
#'   reference; other named methods require traditional residuals.
#'   For deviance, response and Pearson residuals:
#'
#'   * `"uniform"` applies the fitted inverse cumulative distribution function
#'     (CDF) to randomised uniform quantiles and averages the resulting ordered residuals over `n_uniform`
#'     draws.
#'   * `"simulate"` generates `n_simulate` response data sets at the fitting
#'     covariates. Responses are generated together when needed to preserve
#'     dependence, and their ordered residuals supply reference values and bands.
#'   * `"marginal"` approximates the pooled distribution of individual residuals,
#'     without bands for dependence. This is selected for BAM GEE and PQL
#'     correlations that do not define a joint response distribution. Explicit
#'     joint simulation is unavailable in those cases.
#'   * `"normal"` uses a normal approximation fitted to the residual location
#'     and scale. For dependent responses it omits bands based on independence.
#'   * `"direct"` is deprecated in favour of `"uniform"`.
#'
#'   For ordinary GAMs, `NULL` selects `"simulate"` for Tweedie and `"uniform"`
#'   otherwise. Missing inverse CDFs cause a switch to simulation; missing response
#'   generators can cause a switch to a normal approximation for independent
#'   responses. Dependent responses use joint simulation when available, or
#'   marginal checks otherwise. For grouped families, `NULL` selects simulation
#'   if every component has a generator, or inverse-CDF sampling with a message
#'   if every component has an inverse CDF. If neither is available, it gives an
#'   error; a normal approximation must be requested explicitly.
#'
#' @details Use `type = "dharma"` for simulation-based PIT residuals calculated
#'   internally using gratia's response generators; no DHARMa or mgcViz
#'   dependency is needed. These residuals lie in `[0, 1]`, including exact
#'   endpoints, and do not require a response CDF. `n_dharma` controls their
#'   simulation count. All residual panels share one calculated vector.
#'   QQ and worm plots require `method = NULL` and use uniform reference
#'   quantiles, with approximate pointwise bands for independent responses.
#'   Bands are omitted for dependent responses. Censored and multivariate
#'   outcomes are unsupported. See [dharma_residuals()] for the calculation.
#'
#'   With `type = NULL`, ordinary lm/glm/GAM fits use deviance residuals.
#'   Models fitted with [mgcv::gfam()] use randomised quantile (Dunn--Smyth)
#'   residuals when every active component supplies a supported CDF and
#'   probability limits for discrete or censored observations. Otherwise, a
#'   message identifies unsupported components and all
#'   components use deviance residuals; requesting `type = "quantile"` explicitly
#'   is an error when any component is unsupported. Components never mix
#'   residual scales. Invalid encodings or numerical CDF failures are errors.
#'
#'   Quantile residuals use each component's fitted parameters, dispersion and
#'   weights, including binomial trial counts and Gaussian precision weights.
#'   Gamma (including precision weights), negative binomial, beta, scaled-t,
#'   Tweedie, inverse Gaussian, ordinal and zero-inflated Poisson components are
#'   supported. Unrecognised custom-family bounds remain unsupported.
#'   A custom CDF must also support the probabilities just below discrete
#'   values and at censoring limits. Replacements for recognised family CDFs
#'   must use the same arguments and probability conventions. Scalar uncensored
#'   `cnorm()`, `clog()` and
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
#'   with an identity line or zero line. Independent responses have exact
#'   pointwise reference bands for ordered residuals. For dependent responses,
#'   bands use simulations that preserve dependence when a joint generator is
#'   available; otherwise they are omitted with an explanation. Bands condition
#'   on fitted parameters, exclude estimation uncertainty and do not provide
#'   simultaneous coverage. References calculated from simulation ranks also
#'   use simulated bands; see [diagnostic_support]. Worm plots
#'   subtract the same theoretical quantiles from residuals and reference bands.
#'   `appraise()` computes one seeded residual vector shared by its residual
#'   panels and always separates observed-versus-fitted values by component,
#'   because response units differ. Standalone [observed_fitted_plot()] allows
#'   explicit pooling. Ordinal panels use expected category codes.
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
#' @param n_uniform numeric; number of inverse-CDF draws to average when
#'   `method = "uniform"`.
#' @param n_simulate numeric; number of data sets to simulate from the estimated
#'   model when using the simulation method (`method = "simulate"`) for QQ
#'   plots.
#' @param seed numeric; the random number seed to use for `method = "simulate"`
#'   and `method = "uniform"`, and for randomised quantile residuals and
#'   posterior predictive draws.
#' @param type character or `NULL`; residual type: `"deviance"`, `"response"`,
#'   `"pearson"`, `"pit"`, `"quantile"`, or `"dharma"`. `NULL` uses the model-specific default:
#'   deviance for ordinary lm/glm/GAM fits, and quantile for supported grouped
#'   families, GJRM, sdmTMB and full mixed-model checks. glmmTMB uses Pearson
#'   residuals conditionally and response residuals marginally. Extracted GAM
#'   components follow the ordinary GAM defaults. See [diagnostic_support].
#' @param n_dharma positive integer; number of response simulations for
#'   `type = "dharma"`. This is separate from `n_simulate`, which controls
#'   traditional QQ reference calculations. Defaults to 250.
#' @param n_bins character or numeric; either the number of bins or a string
#'   indicating how to calculate the number of bins.
#' @param ncol,nrow numeric; the numbers of rows and columns over which to
#'   spread the plots.
#' @param guides character; one of `"keep"` (the default), `"collect"`, or
#'   `"auto"`. Passed to [patchwork::plot_layout()]
#' @param level numeric; the coverage level for QQ plot reference intervals.
#'   Must be strictly `0 < level < 1`. Used for quantile reference bands and
#'   traditional `method = "simulate"` or `"normal"` bands. Also controls
#'   posterior predictive intervals for discrete responses with `ppcheck = TRUE`.
#' @param ci_alpha,ci_col colour and transparency of QQ reference intervals,
#'   including bands for quantile residuals.
#' @param point_col,point_alpha colour and transparency used to draw points in
#'   the plots. See [graphics::par()] section **Color Specification**. This is
#'   passed to the individual plotting functions, and therefore affects the
#'   points of all plots. Pooled grouped residual panels map component identity
#'   to colour instead of using the constant `point_col`.
#' @param line_col colour specification for the 1:1 line in the QQ plot and the
#'   reference line in the residuals vs linear predictor plot. Also used for
#'   the observed distribution when `ppcheck = TRUE`.
#' @param ppcheck logical; replace the residual histogram with a
#'   [posterior_predictive_plot()]? Defaults to `FALSE`. Requires a model and
#'   family supported by that function. Replicated distributions use `ci_col`.
#'   Discrete posterior points and interval bars default to opacity `0.8`, while
#'   continuous density curves use `0.2`. Explicitly supplying `ci_alpha`
#'   overrides these defaults as well as the QQ reference interval opacity.
#' @param n_ppc positive integer; number of posterior predictive data sets
#'   when `ppcheck = TRUE`. Independent of the QQ plot's `n_simulate`.
#' @param scalable Logical or `NULL`; `NULL` automatically uses scalable plots
#'   at or above `scalable_threshold` retained fitting observations. `TRUE` or `FALSE`
#'   forces scalable or original rendering.
#' @param scalable_threshold Positive integer; automatic switching threshold.
#' @param qq_bins Maximum number of QQ/worm points per family component,
#'   including endpoints.
#' @param density_bins Positive integer or pair giving heatmap grid dimensions.
#' @inheritParams scalable_qq_plot
#' @param conditioning character or `NULL`; `"conditional"` holds fitted random
#'   effects fixed, `"marginal"` averages over new effects, and `"gam_component"`
#'   uses the extracted GAM. Availability and defaults depend on the model; see
#'   [diagnostic_support].
#' @param component character; for delta sdmTMB models, select the complete
#'   `"response"`, occurrence (`"binomial"`), or `"positive"` component.
#'   Positive-component checks use only observations with positive responses.
#' @param field_method how to treat fitted sdmTMB fields: hold one posterior
#'   draw fixed (`"mle-mvn"`), hold estimates fixed (`"mle-eb"`), or average
#'   over fitted-field uncertainty (`"mle-mvn-integrated"`). See
#'   [quantile_residuals()] and [diagnostic_support].
#' @param ppc_batch_size positive integer; number of posterior predictive
#'   draws to process together for scalable plots. Changing the batch size does
#'   not change results for a fixed seed.
#' @param ... arguments passed to [patchwork::wrap_plots()].
#'
#' @importFrom patchwork wrap_plots
#'
#' @note The wording used in [mgcv::qq.gam()] uses *direct* in reference to the
#'   simulated residuals method (`method = "simulated"`). To avoid confusion,
#'   `method = "direct"` is deprecated in favour of `method = "uniform"`.
#'
#' @details Scalable plots aggregate all observations before rendering. Residual
#'   heatmaps show conditional density; observed-versus-fitted heatmaps show
#'   counts. Point styling applies to QQ/worm points, and heatmap fill scales can
#'   be changed with ggplot scales. QQ references average draws as they are
#'   generated or compute exact pooled quantiles using temporary files above
#'   the configured memory threshold; see [diagnostic_support]. Supported univariate GJRM::gamlss models use quantile
#'   residuals by default.
#'
#'   If the response expectation does not exist, the observed-versus-fitted
#'   panel explains why it is unavailable. Other supported panels remain usable.
#'   Prepared diagnostic objects can be given a new conditioning or field choice;
#'   the model is prepared again so all panels use the requested choice.
#'
#' @seealso The plots are produced by functions [gratia::qq_plot()],
#'   [gratia::residuals_linpred_plot()], [gratia::residuals_hist_plot()],
#'   [gratia::observed_fitted_plot()], and optionally
#'   [gratia::posterior_predictive_plot()].
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
#' ## replace the residual histogram with a posterior predictive check
#' appraise(mod, ppcheck = TRUE, n_ppc = 20, seed = 42)
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
  scalable = NULL,
  scalable_threshold = 10000L,
  qq_bins = 1000L,
  density_bins = c(50L, 50L),
  ppcheck = FALSE,
  n_ppc = 50,
  density_method = c("kde", "histogram"),
  bandwidth = NULL,
  conditioning = NULL,
  ppc_batch_size = 10L,
  ..., n_dharma = 250L
) {
  if (!is.null(type)) type <- diagnostic_type(model, type)
  dharma_count <- if (missing(n_dharma)) NULL else n_dharma
  arguments <- list(...)
  adapter_arguments <- diagnostic_adapter_arguments(model, arguments)
  adapter_names <- names(adapter_arguments)
  if (identical(type, "dharma") ||
      (inherits(model, "gratia_diagnostics") && identical(model$type, "dharma"))) {
    if (!is.null(method)) stop("DHARMa-style residuals require method = NULL.", call. = FALSE)
    model <- do.call(prepare_diagnostic_data, c(list(model = model,
      type = type, seed = seed, conditioning = conditioning,
      n_dharma = dharma_count), adapter_arguments))
    conditioning <- NULL
    adapter_arguments <- list()
  }
  argument_names <- names(arguments) %||% rep("", length(arguments))
  plot_arguments <- arguments[!argument_names %in% adapter_names]
  if (inherits(model, "gratia_diagnostics")) {
    if (length(adapter_arguments) || !is.null(conditioning))
      model <- do.call(prepare_diagnostic_data, c(list(model = model,
        type = type, seed = seed, conditioning = conditioning, n_dharma = dharma_count), adapter_arguments))
  } else {
    model <- do.call(diagnostic_plot_source, c(list(model = model,
      type = type, seed = seed, conditioning = conditioning, n_dharma = dharma_count), adapter_arguments))
  }
  if (!is.logical(ppcheck) || length(ppcheck) != 1L || is.na(ppcheck)) {
    stop("'ppcheck' must be TRUE or FALSE.", call. = FALSE)
  }
  # Prepared diagnostics retain conditioning on their source model.
  ppc_conditioning <- conditioning %||% if (inherits(model, "gratia_diagnostics"))
    model$source$conditioning else model$conditioning
  # Check posterior sampling support before preparing other diagnostic panels.
  plt3 <- if (ppcheck) {
    posterior_predictive_plot(diagnostic_posterior_source(model), n = n_ppc, seed = seed,
      conditioning = ppc_conditioning, scalable = scalable,
      scalable_threshold = scalable_threshold, batch_size = ppc_batch_size,
      observed_col = line_col, replicated_col = ci_col,
      replicated_alpha = if (missing(ci_alpha)) NULL else ci_alpha,
      level = level)
  } else NULL
  prepared <- inherits(model, "gratia_diagnostics")
  if (prepared) {
    if (is.null(type)) {
      type <- model$type
    }
    type <- match.arg(type, model$type)
    n_obs <- model$n_obs
    by_family <- diagnostic_data_by_family(model, by_family)
  } else {
    check_gfam_appraise(model)
    type <- diagnostic_type(model, type)
    by_family <- diagnostic_by_family(model, type, by_family)
    if (type %in% c("pit", "quantile")) {
      model <- cache_diagnostic_residuals(model, type, seed)
    }
    n_obs <- NROW(stats::model.frame(model))
  }
  mean_panel <- diagnostic_mean_panel(response_distribution(
    if (inherits(model, "gratia_diagnostics")) model$source else model))
  use_scalable <- diagnostic_scalable(scalable, scalable_threshold, n_obs)
  validate_diagnostic_bins(qq_bins, "qq_bins", minimum = 3L)
  validate_diagnostic_bins(density_bins, "density_bins", length = c(1L, 2L))
  density_method <- match.arg(density_method)
  validate_diagnostic_bandwidth(bandwidth)
  if (is.character(n_bins)) {
    n_bins <- match.arg(n_bins)
  }

  if (!is.character(n_bins) && !is.numeric(n_bins)) {
    stop(
      "'n_bins' should be a number or one of: ",
      paste(dQuote(c("sturges", "scott", "fd")), collapse = ", ")
    )
  }

  if (use_scalable || prepared) {
    d <- prepare_diagnostic_data(model, type, seed, conditioning = conditioning)
    plt1 <- scalable_qq(
      d,
      method = method,
      type = type,
      n_uniform = n_uniform,
      n_simulate = n_simulate,
      seed = seed,
      level = level,
      qq_bins = qq_bins,
      worm = isTRUE(use_worm),
      ci_col = ci_col,
      ci_alpha = ci_alpha,
      point_col = point_col,
      point_alpha = point_alpha,
      line_col = line_col,
      aggregate = use_scalable,
      by_family = by_family
    )
    if (use_scalable) {
      plt2 <- scalable_residuals_linpred_plot(
        d,
        density_bins = density_bins,
        line_col = line_col,
        by_family = by_family,
        rank = rank,
        density_method = density_method,
        bandwidth = bandwidth
      )
      if (!ppcheck) {
        plt3 <- scalable_residuals_hist_plot(
          d,
          n_bins = n_bins,
          by_family = by_family
        )
      }
      plt4 <- mean_panel %||% scalable_observed_fitted_plot(
        d,
        density_bins = density_bins,
        by_family = TRUE
      )
    } else {
      plt2 <- render_diagnostic_points(
        d$data,
        "eta",
        "residuals",
        d$eta_label,
        diagnostic_residual_label(type),
        "Residuals vs linear predictor",
        point_col,
        point_alpha,
        line_col,
        line_intercept = if (type %in% c("pit", "dharma")) 0.5 else 0
      ) + ggplot2::labs(
        subtitle = paste("Family:", stats::family(d$source)[["family"]]),
        caption = diagnostic_predictor_caption(d$source)
      )
      if (!ppcheck) {
        plt3 <- scalable_residuals_hist_plot(
          d,
          n_bins = n_bins,
          by_family = by_family
        )
      }
      plt4 <- mean_panel %||% render_diagnostic_points(
        d$data,
        "fitted",
        "observed",
        "Fitted values",
        "Response",
        "Observed vs fitted values",
        point_col,
        point_alpha
      )
    }
    layout <- prepare_plot_layout(4L, ncol = ncol, nrow = nrow)
    return(wrap_diagnostic_appraisal(
      plt1,
      plt2,
      plt3,
      plt4,
      byrow = TRUE,
      ncol = layout$ncol,
      nrow = layout$nrow,
      guides = guides,
      plot_args = plot_arguments
    ))
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
    line_col = line_col,
    by_family = by_family
  )
  plt2 <- residuals_linpred_plot(
    model,
    type = type,
    point_col = point_col,
    point_alpha = point_alpha,
    line_col = line_col,
    by_family = by_family,
    rank = rank
  )
  if (!ppcheck) {
    plt3 <- residuals_hist_plot(
      model,
      type = type,
      n_bins = n_bins,
      subtitle = NULL,
      by_family = by_family
    )
  }
  plt4 <- mean_panel %||% observed_fitted_plot(
    model,
    subtitle = NULL,
    point_col = point_col,
    point_alpha = point_alpha,
    by_family = is_gfam_family(family(model))
  )
  ## return
  n_plots <- 4
  layout <- prepare_plot_layout(n_plots, ncol = ncol, nrow = nrow)
  ncol <- layout$ncol
  nrow <- layout$nrow
  wrap_diagnostic_appraisal(
    plt1,
    plt2,
    plt3,
    plt4,
    byrow = TRUE,
    ncol = ncol,
    nrow = nrow,
    guides = guides,
    plot_args = plot_arguments
  )
}

#' @rdname appraise
#' @importFrom stats model.frame model.response df.residual
#' @export
appraise.lm <- function(model, ...) {
  appraise.gam(prepare_lm_diagnostics(model), ...)
}
#' @rdname appraise
#' @export
appraise.gamlss <- function(model, ..., type = "quantile", seed = NULL,
                            conditioning = NULL) {
  appraise.default(model, ..., type = type, seed = seed, conditioning = conditioning)
}

#' @rdname appraise
#' @export
appraise.default <- function(model, ..., type = NULL, seed = NULL, conditioning = NULL, n_dharma = 250L) {
  d <- prepare_diagnostic_data(model, type, seed, conditioning = conditioning, n_dharma = if (missing(n_dharma)) NULL else n_dharma)
  appraise.gam(d, ..., type = d$type, seed = seed, conditioning = conditioning)
}
