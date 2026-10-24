#' Estimate moments of the conditional response distribution
#'
#' Calculate population moments at observed or supplied covariate values, with
#' uncertainty propagated jointly through the model's distribution parameters.
#'
#' @param object A fitted [mgcv::gam()] or [mgcv::bam()] model.
#' @param data Optional prediction data. With `NULL`, evaluate fitting rows,
#'   retaining fitting offsets and the model's missing-value convention.
#' @param moments Character vector selecting `"mean"`, `"variance"`, `"sd"`,
#'   `"cv"`, `"skewness"`, `"kurtosis"`, or `"excess_kurtosis"`. `"all"`
#'   requests all seven. CV is SD divided by a strictly positive mean;
#'   kurtosis is the fourth central moment divided by squared variance.
#' @param uncertainty One of `"delta"`, `"simulation"`, or `"none"`.
#' @param ci_level Coverage of pointwise uncertainty intervals, between 0 and 1.
#' @param unconditional Include smoothing parameter uncertainty when available.
#' @param nuisance One of `"estimated"` or `"fixed"`. The default propagates
#'   uncertainty in estimated family parameters outside the coefficient vector,
#'   such as Gamma dispersion, negative-binomial shape, or Tweedie power.
#'   `"fixed"` explicitly conditions on their fitted values.
#' @param n_draws Number of joint parameter draws for simulation, at least two.
#'   User-supplied coefficient draws determine their own count when omitted.
#' @param seed Optional integer random seed. An explicit seed preserves the
#'   caller's random-number state; `NULL` uses and advances it when generating
#'   random draws. Supplied draws do not consume random numbers.
#' @param scores Optional numeric category scores, in category order, for
#'   ordinal or multinomial models. Otherwise return moments of each category's
#'   zero/one membership indicator. Binomial moments describe a Bernoulli
#'   response, independently of fitting trial counts or likelihood weights.
#' @param method Coefficient sampler for simulation; see [post_draws()].
#'   Joint sampling of estimated global family parameters uses `"gaussian"`.
#' @param draws Optional coefficient matrix for `method = "user"`, in fitted
#'   coefficient order. Requires `nuisance = "fixed"` if global parameters
#'   were estimated outside the coefficient vector.
#' @param envir Environment used to recover model covariates.
#' @param family_index Optional integer component-family positions for grouped
#'   predictions, one per supplied row. Membership is otherwise recovered as in
#'   [fitted_samples()]. It must agree with membership recoverable from `data`.
#' @param ... Prediction arguments passed to [mgcv::predict.gam()], including
#'   `exclude` and `na.action`. `type`, `newdata`, `se.fit`, and `terms` are
#'   controlled internally and cannot be supplied.
#'
#' @details
#' Estimates describe one response at each covariate combination, with unit
#' observation weight. Fitting weights are not transferred to new responses.
#' For ML/REML fits with an estimated dispersion, all uncertainty modes use
#' the likelihood estimate stored in `reml.scale`. The conditional coefficient
#' covariance is rescaled to this value. This can differ from mgcv's separate
#' Pearson/Fletcher estimate in `sig2`; the saved outer Hessian describes the
#' likelihood estimate, so combining it with `sig2` would mix estimators.
#' Response variance and uncertainty in its estimate are different quantities:
#' `.fitted` contains the variance when `.parameter` is `"variance"`, and `.se`
#' describes uncertainty in that estimate. These are not moments of a posterior
#' predictive mixture or prediction intervals for future observations.
#'
#' Standard Gaussian, Poisson, binomial, Gamma and inverse-Gaussian families,
#' negative binomial, beta, scaled t, Tweedie, all seven location/scale/shape
#' families, ordinal, multinomial and multivariate-normal models are supported.
#' Censored normal, logistic and Poisson families return moments of the latent
#' uncensored response. Beta moments describe the beta likelihood, without the
#' epsilon clipping used by some response generators. `bcg()` supports the
#' original lognormal response only when its Box-Cox parameter is fixed at zero:
#' other powers require a distribution outside the inverse transformation's
#' domain that the fitted family does not specify. Quasi and Cox families are
#' unsupported. Grouped (`gfam`) predictions use each row's component family
#' and the full joint model covariance. Grouped ordinal components require
#' explicit category scores; grouped category-indicator output is unsupported.
#'
#' The delta method differentiates the complete parameter-to-moment
#' transformation and uses its full joint covariance. Gaussian, Poisson, Gamma,
#' inverse-Gaussian and Tweedie derivatives (including their models with several predictors) are
#' analytic; other families use central numerical differences. Positive variance, SD, CV
#' and kurtosis intervals are calculated on a log scale; `.se` always remains
#' on the reported quantity's scale. Other delta intervals use the identity
#' scale. Simulation uses joint parameter draws and equal-tailed quantiles.
#' Both methods retain the moment evaluated at fitted parameters as `.fitted`.
#'
#' With estimated global family parameters, a first-order joint Gaussian
#' approximation combines the conditional Bayesian coefficient covariance with
#' second derivatives of the ML/REML fitting criterion and derivatives of the
#' fitted coefficients with respect to the family and smoothing parameters. Without
#' `unconditional`, outer curvature is conditioned on smoothing parameters.
#' With `unconditional`, the full saved outer Hessian is inverted, including
#' its smoothing uncertainty. The curvature and coefficient sensitivities are
#' evaluated at the fitted parameters, with mgcv's mapping for linked or fixed
#' smoothing parameters. A positive-definite Hessian is required. This does not
#' also add the corrected coefficient
#' covariance, which would count that uncertainty twice. The fitted object must
#' store a matching Hessian and sensitivities; unsupported fitting methods,
#' including fast REML BAM fits, give an error. This is a local approximation
#' about the fitted parameters, not an exact posterior sampler for dispersion
#' or family shape. With fixed global parameters, the existing coefficient
#' covariance or [post_draws()] machinery is used. Delta intervals and Gaussian
#' simulation with `unconditional = TRUE` and fixed estimated global parameters use
#' the first-order smoothing correction conditional on those fixed parameters:
#' native corrected covariance can also include their uncertainty. This requires
#' the same saved outer Hessian and sensitivities when smoothing parameters are
#' present. Legacy `negbin()` fits outside ML/REML require explicit
#' `nuisance = "fixed"`; use `nb()` to estimate and propagate shape uncertainty.
#' The covariance calculation uses a first-order expansion of the coefficient
#' means and does not include mgcv's additional correction for changes in the
#' conditional coefficient covariance. The `moment_info` attribute
#' records which approximation was used.
#'
#' Nonexistent moments are retained as `NA` with a status and one warning per
#' call. In simulation, `.p_defined` records the fraction of draws with a finite
#' value. Intervals and standard errors are withheld if any draw is undefined;
#' draws are never silently dropped. CV requires a positive mean, and skewness
#' and kurtosis require positive variance. GEV moments of order r require
#' shape < 1/r; t moments require degrees of freedom greater than their order.
#' Status values distinguish `"moment_does_not_exist"`, `"nonpositive_mean"`,
#' `"zero_variance"`, `"undefined_in_draws"`, and `"missing_predictors"`.
#' `"numerical_failure"` identifies a value that could not be calculated with
#' finite precision or invalid distribution parameters; `"derivative_unavailable"`
#' identifies a finite estimate whose local derivative could not be calculated.
#'
#' @return A tibble of class `"fitted_moments"`, containing `.row`, covariates,
#'   `.parameter`, `.fitted`, `.se`, `.lower_ci`, `.upper_ci` and `.status`.
#'   Category indicators include `.category`; multivariate responses include
#'   `.y`; grouped responses include `.family`. Simulation also includes `.p_defined`. All estimates, standard errors
#'   and interval bounds are on the requested quantity's scale.
#' @seealso [conditional_moments()], [fitted_values()], [fitted_samples()]
#' @export
#' @examples
#' load_mgcv()
#' m <- gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
#' fitted_moments(m, data = mtcars[1:3, ], moments = c("mean", "variance"))
#' fitted_moments(m, data = mtcars[1:3, ], moments = "variance",
#'   uncertainty = "simulation", n_draws = 500, seed = 42)
fitted_moments <- function(object, ...) {
  UseMethod("fitted_moments")
}

#' @export
fitted_moments.default <- function(object, ...) {
  stop("Response moments require a supported mgcv gam or bam model.", call. = FALSE)
}

#' @rdname fitted_moments
#' @export
fitted_moments.gam <- function(object, data = NULL,
    moments = c("mean", "variance", "cv"),
    uncertainty = c("delta", "simulation", "none"), ci_level = 0.95,
    unconditional = FALSE, nuisance = c("estimated", "fixed"),
    n_draws = 10000L, seed = NULL, scores = NULL,
    method = c("gaussian", "mh", "inla", "user"), draws = NULL,
    envir = NULL, family_index = NULL, ...) {
  uncertainty <- match.arg(uncertainty)
  nuisance <- match.arg(nuisance)
  method <- match.arg(method)
  if (!is.null(draws) && (uncertainty != "simulation" || method != "user"))
    stop("'draws' requires uncertainty = 'simulation' and method = 'user'.", call. = FALSE)
  if (uncertainty == "simulation" && method != "gaussian" && unconditional)
    stop("'unconditional' requires Gaussian simulation; other samplers use their own parameter draws.", call. = FALSE)
  validate_smooth_interval("confidence", ci_level, 2L, 1L, seed)
  validate_shape_flag(unconditional, "unconditional")
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L ||
      !is.finite(seed) || abs(seed) > .Machine$integer.max || seed != trunc(seed)))
    stop("'seed' must be NULL or a single integer.", call. = FALSE)
  if (!is.character(moments) || !length(moments) || anyNA(moments) ||
      anyDuplicated(moments) ||
      !(identical(moments, "all") || all(moments %in% moment_names())))
    stop("'moments' must select distinct moment names, or 'all'.", call. = FALSE)
  if (identical(moments, "all")) moments <- moment_names()
  marginal_not_supported(list(...), "fitted_moments.gam()")
  dots <- list(...)
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots)))))
    stop("Arguments in '...' must be named.", call. = FALSE)
  if (any(names(dots) %in% c("type", "newdata", "se.fit", "terms")))
    stop("Use 'data' for prediction rows and 'exclude' to omit terms; 'type', 'newdata', 'se.fit' and 'terms' cannot be supplied.", call. = FALSE)
  object <- with_model_envir(object, envir)
  if (inherits(object, "scam") || isTRUE(attr(object, "gratia.gamm4")))
    stop("Response moments currently require an ordinary mgcv gam or bam fit.", call. = FALSE)
  f <- stats::family(object)
  ft <- family_type(f)
  grouped <- is_gfam_family(f)
  if (startsWith(ft, "quasi") || ft == "cox_ph")
    stop("Response moments do not support quasi or Cox families.", call. = FALSE)
  if (!grouped && !is.null(family_index))
    stop("'family_index' is only used for grouped models.", call. = FALSE)
  if (!is.null(scores) && !grouped && ft != "multinom" && !startsWith(ft, "ordered_categorical"))
    stop("'scores' is only used for ordinal or multinomial responses.", call. = FALSE)
  if (!is.null(object$AR1.rho) && object$AR1.rho != 0 && uncertainty != "none")
    stop("Moment uncertainty is unavailable for residual-correlated BAM fits.", call. = FALSE)
  if (uncertainty != "none" && has_inequality_constraints(object))
    stop("Moment uncertainty does not enforce this model's inequality constraints.", call. = FALSE)
  # Use the existing layout and offset handling for every linear predictor.
  context <- if (grouped) moment_grouped_context(object, data, family_index, ...) else
    lss_prediction_context(object, data, ...)
  context$family <- unserialize(serialize(context$family, NULL))
  model <- context$model <- moment_scale_model(context$model)
  p <- length(stats::coef(model))
  theta <- if (is.function(f$getTheta) && inherits(f, "extended.family"))
    f$getTheta() else numeric()
  parameters <- if (uncertainty == "none") {
    list(centre = as.numeric(stats::coef(model)), nth = 0L, scale = FALSE,
      raw_theta = theta, source = "fitted parameters", fixed = TRUE)
  } else moment_parameter_context(model, nuisance, unconditional)
  evaluate <- function(par, details = FALSE) {
    beta <- par[seq_len(p)]
    family <- context$family
    if (parameters$nth) {
      th <- parameters$raw_theta
      th[seq_len(parameters$nth)] <- par[p + seq_len(parameters$nth)]
      family$putTheta(th)
    }
    scale <- if (parameters$scale) exp(par[length(par)]) else model$sig2
    eta <- lss_predictors(context, beta)
    result <- if (grouped) moment_grouped_values(context, family, eta, beta, scores) else
      moment_family_values(family, eta, scale, beta, scores)
    result$values <- result$values[, moments, drop = FALSE]
    result$status <- result$status[, moments, drop = FALSE]
    if (details) result else as.vector(result$values)
  }
  point <- evaluate(parameters$centre, details = TRUE)
  value <- as.vector(point$values)
  nc <- nrow(point$values)
  parameter <- rep(moments, each = nc)
  status <- as.vector(point$status)
  se <- lower <- upper <- rep(NA_real_, length(value))
  defined <- NULL
  if (uncertainty == "delta") {
    J <- moment_analytic_gradient(context, parameters, point, moments)
    if (is.null(J)) J <- moment_gradient(evaluate, parameters$centre, value)
    interval <- moment_delta(value, J, parameters$covariance, parameter, ci_level)
    se <- interval$se
    lower <- interval$lower
    upper <- interval$upper
    status[is.finite(value) & !is.finite(se)] <- "derivative_unavailable"
  } else if (uncertainty == "simulation") {
    if (method == "user" && missing(n_draws)) n_draws <- NROW(draws)
    if (!is.numeric(n_draws) || length(n_draws) != 1L || !is.finite(n_draws) ||
        n_draws < 2 || n_draws > .Machine$integer.max || n_draws != trunc(n_draws))
      stop("'n_draws' must be an integer of at least two.", call. = FALSE)
    simulate <- function() {
      if (parameters$nth || parameters$scale) {
        if (method != "gaussian" || !is.null(draws)) stop(
          "Estimated global parameters require joint Gaussian simulation; other coefficient samplers require nuisance = 'fixed'.", call. = FALSE)
        sweep(simultaneous_coefficient_deviations(parameters$covariance, n_draws, 1L),
          2L, parameters$centre, "+")
      } else {
        draw_seed <- if (is.null(seed) && method != "user")
          sample.int(.Machine$integer.max, 1L) else seed
        if (method == "gaussian" && isTRUE(parameters$joint_sampling))
          return(sweep(simultaneous_coefficient_deviations(parameters$covariance,
            n_draws, 1L), 2L, parameters$centre, "+"))
        args <- list(model = model, n_draws = n_draws, method = method, seed = draw_seed)
        if (method == "user") args$draws <- draws
        if (method == "gaussian") args$unconditional <- unconditional &&
          !(nuisance == "fixed" && (isTRUE(model$scale.estimated) ||
            (f$n.theta %||% 0L) > 0L) && !length(model$sp))
        do.call(post_draws, args)
      }
    }
    joint <- if (is.null(seed)) simulate() else withr::with_seed(seed, simulate())
    interval <- moment_simulation(evaluate, joint, ci_level)
    se <- interval$se
    lower <- interval$lower
    upper <- interval$upper
    defined <- interval$defined
    status[is.finite(value) & defined < 1] <- "undefined_in_draws"
  }
  invalid <- !is.finite(value)
  value[invalid] <- se[invalid] <- lower[invalid] <- upper[invalid] <- NA_real_
  rows <- rep(point$row, times = length(moments))
  out <- tibble::tibble(.row = rows, .parameter = parameter, .fitted = value,
    .se = se, .lower_ci = lower, .upper_ci = upper, .status = status)
  if (!is.null(defined)) out$.p_defined <- defined
  if (!is.null(point$category)) out$.category <- rep(point$category, length(moments))
  if (!is.null(point$response)) out$.y <- rep(point$response, length(moments))
  out <- restore_prediction_table(out, context$layout)
  if (grouped) out$.family <- context$grouped$family_index[match(out$.row,
    seq_along(context$grouped$family_index))]
  covariates <- tibble::as_tibble(context$layout$output_data)
  if (context$layout$training) {
    keep <- intersect(names(covariates), model_vars(object))
    covariates <- covariates[, keep, drop = FALSE]
  }
  covariates <- covariates[match(out$.row, context$layout$rows), , drop = FALSE]
  covariates <- covariates[, !names(covariates) %in% names(out), drop = FALSE]
  out <- tibble::as_tibble(cbind(out, covariates))
  out$.status[is.na(out$.status)] <- "missing_predictors"
  out <- out[order(out$.row, match(out$.parameter, moments)), , drop = FALSE]
  if (any(!out$.status %in% c("ok", "missing_predictors"))) warning(
    "Some requested moments or their uncertainty are undefined; inspect '.status'",
    if (uncertainty == "simulation") " and '.p_defined'" else "", ".", call. = FALSE)
  attr(out, "moment_info") <- list(uncertainty = uncertainty, ci_level = ci_level,
    nuisance = nuisance, unconditional = unconditional, covariance = parameters$source,
    approximation = if (uncertainty == "none") "fitted parameters" else
      if (uncertainty == "simulation" && method != "gaussian")
        paste(method, "coefficient draws") else "first-order joint Gaussian approximation",
    observation_weight = 1, scores = scores, method = if (uncertainty == "simulation") method,
    interval_scale = "log for positive variance, SD, CV and kurtosis; identity otherwise")
  class(out) <- c("fitted_moments", class(out))
  out
}

moment_grouped_context <- function(model, data, family_index, ...) {
  grouped <- gfam_prediction_context(model, data, family_index,
    na.action = list(...)$na.action %||% stats::na.pass)
  model <- model_used_rows(model)
  X <- gfam_predict_eta(model, grouped$layout$data, type = "lpmatrix", ...)
  offsets <- prediction_offsets(model, X)
  if (grouped$layout$training) offsets[[1L]] <- as.vector(
    predict_model(model, type = "link", ...) + fitting_link_correction(model)) -
      drop(X %*% stats::coef(model))
  list(model = model, family = stats::family(model), X = X,
    index = list(seq_len(ncol(X))), offsets = matrix(offsets[[1L]], ncol = 1L),
    layout = grouped$layout, grouped = grouped)
}

#' Conditional moments of a GAM response distribution
#'
#' Construct covariate combinations using [conditional_values()] conventions,
#' then estimate response moments with [fitted_moments()]. Other covariates are
#' held at typical values; observations are not averaged over those covariates.
#'
#' @inheritParams conditional_values
#' @param condition Character vector or list specifying covariates and optional
#'   values. The first covariate supplies the plotting axis, followed by colour,
#'   facet columns and facet rows. Grouped surface conditions are not supported.
#' @param n_vals Positive integer; number of values to generate along a numeric
#'   plotting axis. Defaults to 100. Explicit condition values or helpers override
#'   generated values; other numeric conditions use five-number summaries.
#' @inheritParams fitted_moments
#' @param ... Arguments to [fitted_moments()], including `uncertainty`,
#'   `nuisance`, `n_draws`, `seed`, `scores`, and `exclude`.
#' @return A tibble of class `"conditional_moments"`, with the columns returned
#'   by [fitted_moments()] and the plotting channels of [conditional_values()].
#' @export
#' @examples
#' load_mgcv()
#' m <- gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
#' conditional_moments(m, condition = "wt", moments = c("mean", "variance")) |>
#'   draw()
conditional_moments <- function(model, condition = NULL, data = NULL,
    moments = c("mean", "variance", "cv"), n_vals = 100, complete = TRUE, ...) {
  UseMethod("conditional_moments")
}

#' @export
conditional_moments.default <- function(model, ...) {
  stop("Conditional moments require a supported mgcv gam or bam model.", call. = FALSE)
}

#' @rdname conditional_moments
#' @export
conditional_moments.gam <- function(model, condition = NULL, data = NULL,
    moments = c("mean", "variance", "cv"), n_vals = 100, complete = TRUE, ...) {
  grid <- conditional_prediction_grid(model, condition, data, n_vals, complete)
  out <- fitted_moments(model, data = grid$data, moments = moments, ...)
  channels <- stats::setNames(rep(list(NULL), 4L), c("x", "colour", "f_col", "f_row"))
  channels[seq_along(grid$condition)] <- grid$condition
  attr(out, "channels") <- channels
  class(out) <- c("conditional_moments", class(out))
  out
}

#' Draw conditional response moments
#'
#' @inheritParams draw.conditional_values
#' @param ylab Character; label for the response moment axis.
#' @param object A tibble returned by [conditional_moments()].
#' @param ... Additional arguments passed to [patchwork::wrap_plots()].
#' @return A patchwork containing one plot per moment and response/category.
#' @export
#' @inheritSection draw Label layout
draw.conditional_moments <- function(object, facet_scales = "fixed",
    discrete_colour = NULL, discrete_fill = NULL, xlab = NULL, ylab = NULL, ...) {
  keys <- intersect(c(".parameter", ".category", ".y"), names(object))
  groups <- unique(as.data.frame(object[keys]))
  plots <- lapply(seq_len(nrow(groups)), function(i) {
    keep <- rep(TRUE, nrow(object))
    for (key in keys) keep <- keep & object[[key]] == groups[[key]][i]
    part <- object[keep, , drop = FALSE]
    label <- paste(unlist(groups[i, , drop = FALSE]), collapse = ": ")
    draw.conditional_values(part, facet_scales = facet_scales,
      discrete_colour = discrete_colour, discrete_fill = discrete_fill,
      xlab = xlab, ylab = ylab %||% label)
  })
  patchwork::wrap_plots(plots, ...)
}

#' @export
`[.fitted_moments` <- function(x, i, j, drop = FALSE) {
  info <- attr(x, "moment_info")
  channels <- attr(x, "channels")
  out <- NextMethod()
  if (is.data.frame(out)) {
    attr(out, "moment_info") <- info
    attr(out, "channels") <- channels
  }
  out
}
