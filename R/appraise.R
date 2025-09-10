#' @title Model diagnostic plots
#'
#' @param model a fitted model. Currently models inheriting from class `"gam"`,
#'   as well as classes `"glm"` and `"lm"` from calls to [stats::glm] or
#'   [stats::lm] are supported.
#' @param method character; method used to generate theoretical quantiles.
#'   The default is `"uniform"`, which generates reference quantiles using
#'   random draws from a uniform distribution and the inverse cumulative
#'   distribution function (CDF) of the fitted values. The reference quantiles
#'   are averaged over `n_uniform` draws. `"simulate"` generates reference
#'   quantiles by simulating new response data from the model at the observed
#'   values of the covariates, which are then residualised to generate reference
#'   quantiles, using `n_simulate` simulated data sets. `"normal"` generates
#'   reference quantiles using the standard normal distribution. `"uniform"` is
#'   more computationally efficient, but `"simulate"` allows reference bands to
#'   be drawn on the QQ-plot. `"normal"` should be avoided but is used as a fall
#'   back if a random number generator (`"simulate"`) or the inverse of the CDF
#'   (`"uniform"``) are not available from the `family` used during model
#'   fitting.
#'
#'   Note that `method = "direct"` is deprecated in favour of
#'   `method = "uniform"`.
#' @param use_worm logical; should a worm plot be drawn in place of the QQ plot?
#' @param n_uniform numeric; number of times to randomize uniform quantiles
#'   in the direct computation method (`method = "direct"`) for QQ plots.
#' @param n_simulate numeric; number of data sets to simulate from the estimated
#'   model when using the simulation method (`method = "simulate"`) for QQ
#'   plots.
#' @param seed numeric; the random number seed to use for `method = "simulate"`
#'   and `method = "uniform"`.
#' @param type character; type of residuals to use. Only `"deviance"`,
#'   `"response"`, and `"pearson"` residuals are allowed.
#' @param n_bins character or numeric; either the number of bins or a string
#'   indicating how to calculate the number of bins.
#' @param ncol,nrow numeric; the numbers of rows and columns over which to
#'   spread the plots.
#' @param guides character; one of `"keep"` (the default), `"collect"`, or
#'   `"auto"`. Passed to [patchwork::plot_layout()]
#' @param level numeric; the coverage level for QQ plot reference intervals.
#'   Must be strictly `0 < level < 1`. Only used with `method = "simulate"`.
#' @param ci_alpha,ci_col colour and transparency used to draw the QQ plot
#'   reference interval when `method = "simulate"`.
#' @param point_col,point_alpha colour and transparency used to draw points in
#'   the plots. See [graphics::par()] section **Color Specification**. This is
#'   passed to the individual plotting functions, and therefore affects the
#'   points of all plots.
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
  method = c("uniform", "simulate", "normal", "direct"),
  use_worm = FALSE,
  n_uniform = 10, n_simulate = 50,
  seed = NULL,
  type = c("deviance", "pearson", "response"),
  n_bins = c("sturges", "scott", "fd"),
  ncol = NULL, nrow = NULL,
  guides = "keep",
  level = 0.9,
  ci_col = "black", ci_alpha = 0.2,
  point_col = "black", point_alpha = 1,
  line_col = "red",
  ...
) {
  ## process args
  method <- match.arg(method)
  if (identical(method, "direct")) {
    message("`method = \"direct\"` is deprecated, use `\"uniform\"`")
    method <- "uniform"
  }
  type <- match.arg(type)
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
    line_col = line_col
  )
  plt2 <- residuals_linpred_plot(model,
    type = type, point_col = point_col,
    point_alpha = point_alpha,
    line_col = line_col
  )
  plt3 <- residuals_hist_plot(model,
    type = type, n_bins = n_bins,
    subtitle = NULL
  )
  plt4 <- observed_fitted_plot(model,
    subtitle = NULL, point_col = point_col,
    point_alpha = point_alpha
  )
  ## return
  n_plots <- 4
  if (is.null(ncol) && is.null(nrow)) {
    ncol <- ceiling(sqrt(n_plots))
    nrow <- ceiling(n_plots / ncol)
  }
  wrap_plots(plt1, plt2, plt3, plt4,
    byrow = TRUE, ncol = ncol, nrow = nrow, guides = guides,
    ...
  )
}

#' @rdname appraise
#' @importFrom stats model.frame model.response df.residual
#' @export
`appraise.lm` <- function(model, ...) {
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
