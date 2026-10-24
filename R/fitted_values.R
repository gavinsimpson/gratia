#' Generate fitted values from a estimated GAM
#'
#' @param object a fitted model. Currently only models fitted by [mgcv::gam()]
#'   and [mgcv::bam()] are supported.
#' @param data optional data frame of covariate values for which fitted values
#'   are to be returned.
#' @param scale character; what scale should the fitted values be returned on?
#'   `"linear predictor"` is a synonym for `"link"` if you prefer that
#'   terminology. `"linear_predictor"` is also accepted. Expectations require
#'   `"response"`; parameter predictions use natural values or their predictors.
#' @inheritParams fitted_samples
#' @inheritParams smooth_estimates
#' @param ci_level numeric; a value between 0 and 1 indicating the coverage of
#'   the credible interval.
#' @param interval character; `"confidence"` (the default) gives pointwise
#'   intervals. `"simultaneous"` gives simultaneous intervals for predictions
#'   at the supplied covariate combinations.
#' @param n_sim positive integer; number of coefficient draws used for
#'   simultaneous intervals. Ignored for pointwise intervals.
#' @param n_cores positive integer; number of cores used by [mvnfast::rmvn()]
#'   for simultaneous intervals. Parallel execution requires OpenMP support.
#' @param seed integer or `NULL`; optional random seed for simultaneous
#'   intervals. An explicit seed preserves the caller's random number state.
#'   With `NULL`, the current random number state is used and advanced.
#' @param unconditional logical; include smoothing parameter uncertainty in
#'   the Bayesian covariance matrix, if available. Otherwise a warning is
#'   issued and the uncorrected covariance is used.
#' @param ... arguments passed to [mgcv::predict.gam()]. Note that `type`,
#'   `newdata`, and `se.fit` are already used and passed on to
#'   [mgcv::predict.gam()].
#' @param interval_method character; `"auto"` uses stored scasm bootstrap draws
#'   for pointwise intervals when available and Gaussian intervals otherwise.
#'   `"gaussian"` requests covariance-based limits and `"bootstrap"` requires
#'   stored draws. Simultaneous bootstrap bands are unsupported. Shape-enforced
#'   intervals are available for smooth components through [smooth_estimates()].
#'
#' @details
#' With `marginalise`, predictions are response means averaged over the selected
#' random-effect distributions, using the methods described in [fitted_samples()].
#' The point estimate uses fitted coefficients; pointwise intervals use the
#' derivative of the integrated mean and the coefficient covariance. `.se` is
#' on the response scale. These normal intervals are not clipped to the response
#' range. Simultaneous and bootstrap marginal intervals are not yet supported.
#' The `integration_info` attribute records numerical accuracy and fixed
#' variance estimates.
#'
#' `quantity = "parameters"` returns all distribution parameters represented
#' by predictors, including intercept-only predictors. See [family_parameters()]
#' for their definitions. These are natural distribution parameters, which
#' can differ from mgcv's native response predictions: Gaussian LSS returns
#' standard deviation, GEV returns location rather than expectation, and
#' sinh-arcsinh returns shape parameters rather than response moments.
#'
#' `quantity = "expectation"` returns the expected response on the response
#' scale, labelled `"expectation"`. Categorical expectations are probabilities
#' of category membership; multivariate models return a mean per response.
#' `.category` and `.y` identify the category or response separately from
#' `.parameter`. Global parameters are described but held fixed when calculating
#' coefficient uncertainty. Coupled expectations use the full coefficient
#' covariance and a delta-method Jacobian. Their intervals are pointwise.
#' Ordinal and multinomial probability intervals use a normal approximation to
#' log odds, transformed back to probabilities. Log odds and their uncertainty
#' are computed before rounding probabilities, so valid probabilities rounded
#' to zero or one retain defined intervals.
#'
#' The `prediction_info` attribute records the quantity, scale, standard-error
#' scale and parameter descriptions. Parameter standard errors retain their
#' link-scale meaning; category-probability and expectation standard errors are
#' on the response scale. Unsupported simultaneous or bootstrap transformations
#' are rejected rather than combining marginal parameter intervals.
#'
#' Native scasm fits with stored bootstrap draws use mgcv's pointwise limits
#' by default, including its `svar.only` correction for smoothing bias.
#' `interval_method = "gaussian"` restores covariance-based limits. `.se`
#' retains its existing covariance-based meaning. No replicates are generated.
#' Bootstrap limits use existing scalar and location-scale parameter
#' transformations. Unsupported nonlinear family outputs give an error for
#' an explicit bootstrap request; automatic selection warns and uses Gaussian
#' limits. Simultaneous bootstrap bands are not supported. Automatic
#' simultaneous intervals warn when stored bootstrap draws are left unused.
#' The `interval_info` attribute records the method and correction.
#'
#' Formula offsets are included in predictions. Offsets supplied only through
#' the fitting function's `offset` argument are omitted, including when
#' `data = NULL`: this function evaluates explicit prediction rows internally.
#' In contrast, [fitted_samples()] retains fitting-argument offsets when its
#' `data` argument is `NULL`. Put offsets in the formula to evaluate them at
#' new observations.
#'
#' With `data = NULL`, results follow the model's `na.action`: `na.exclude`
#' restores excluded observations as `NA` fitted values, standard errors and
#' interval bounds; `na.omit` returns only retained observations. Covariates
#' unavailable in the stored model frame are restored as typed `NA` values.
#' `.row` indexes the fitting data after any `subset` was applied.
#'
#' Supplying `data` requests predictions at those rows, independently of training
#' exclusions. Missing responses do not prevent prediction. Missing required
#' predictors produce `NA` results by default. An explicit `na.action` passed
#' through `...` is honoured; with `na.omit`, `.row` retains the positions in the
#' supplied data, rather than numbering the remaining rows consecutively.
#'
#' For supplied prediction data, an informative message identifies combinations
#' of categorical predictor levels absent from the retained fitting observations.
#' Predictions are unchanged: an unobserved combination is not necessarily
#' non-estimable, particularly in additive models. Numeric-by-factor range
#' extrapolation is not checked. Missing predictors and genuinely new factor
#' levels retain their usual prediction behavior.
#'
#' Simultaneous intervals jointly cover the model's underlying expected
#' responses, or linear predictors on the link scale, at all valid covariate
#' combinations evaluated in this call, with approximate posterior probability
#' `ci_level`. The fitted values are estimates of these quantities. The
#' combinations may include factor levels and need not form an ordered sequence,
#' curve, or grid. This interpretation also applies to two predictions.
#' With `data = NULL`, the simultaneous set comprises the retained fitting
#' covariate combinations. Missing predictions do not enter this set. Coverage
#' is not asserted outside this set or between its points. Separate calls
#' define separate simultaneous sets. These are uncertainty intervals for model
#' predictions, not prediction intervals for future observations.
#'
#' Simultaneous intervals use joint Gaussian coefficient draws and a critical
#' value based on the maximum absolute standardized deviation over the supplied
#' rows. Their limits are computed on the link scale and transformed if
#' `scale = "response"`; `.se` remains on the link scale. The simulation
#' covariance may be positive semidefinite. Supported families are Gaussian,
#' Poisson, binomial, Gamma, inverse Gaussian, quasi families, negative binomial,
#' Tweedie, beta regression, and scaled t, fitted by `gam()` or `bam()` (including
#' the `gam` component of a `gamm()` fit), and by `scam()`. For SCAM models,
#' the Gaussian approximation uses transformed coefficients conditional on
#' the fitted smoothing parameters. Other families support pointwise intervals
#' only.
#'
#' Response-scale simultaneous intervals support the identity, log, logit,
#' probit, cloglog, cauchit, square-root, inverse, and inverse-squared links.
#' Intervals crossing an inverse-link domain boundary are rejected; use
#' `scale = "link"` for these intervals or for other links.
#'
#' `terms` and `exclude` in `...` retain [mgcv::predict.gam()] semantics and
#' apply to both prediction and interval calculation. If terms are selected or
#' excluded, intervals concern that selected quantity, not necessarily the full
#' expected response. Use the link scale for sums of smooth contributions, and
#' consider the intercept and offset treatment when selecting terms.
#'
#' @note For parameter output, the `.se` component is on the *link*
#'   (*linear predictor*) scale. Multinomial probabilities on the response scale
#'   and all expected responses have response-scale standard errors.
#'
#' @return A tibble (data frame) whose first *m* columns contain either the data
#'   used to fit the model (if `data` was `NULL`), or the variables supplied to
#'  `data`. The result also includes:
#'
#' * `.parameter`: parameter name, or `"expectation"`,
#' * `.fitted`: the requested quantity on the specified scale,
#' * `.se`: standard error; see `prediction_info` for its scale,
#' * `.lower_ci`, `.upper_ci`: the limits of the credible interval on the fitted values,
#'     on the specified scale.
#'
#' Models fitted with certain families will include additional variables
#'
#' * `mgcv::ocat()` models: when `quantity = "expectation"`, the returned object will
#'   contain a `.row` column and a `.category` column, which indicate to which row
#'   of the `data` each row of the returned object belongs. Additionally, there
#'   will be `nrow(data) * n_categories` rows in the returned object; each row
#'   is the predicted probability for a single category of the response.
#'
#' @inheritParams smooth_estimates
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 6)
#' }
#' sim_df <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = sim_df, method = "REML")
#' fv <- fitted_values(m)
#' fv
#'
#' # Simultaneous intervals for two arbitrary covariate combinations
#' fitted_values(m, data = sim_df[c(1, 20), ], interval = "simultaneous",
#'   n_sim = 1000, seed = 42)
#'
#' # A selected sum of smooth contributions on the link scale
#' fitted_values(m, data = sim_df[c(1, 20), ], scale = "link",
#'   terms = c("s(x2)", "s(x3)"), interval = "simultaneous",
#'   n_sim = 1000, seed = 42)
#' \dontshow{
#' options(op)
#' }
`fitted_values` <- function(object, ...) {
  UseMethod("fitted_values")
}

#' @export
#' @rdname fitted_values
`fitted_values.gam` <- function(object,
                                data = NULL,
                                scale = c(
                                  "response",
                                  "link",
                                  "linear predictor"
                                ),
                                ci_level = 0.95, envir = NULL, ...,
                                interval = c("confidence", "simultaneous"),
                                n_sim = 10000, n_cores = 1, seed = NULL,
                                unconditional = FALSE,
                                interval_method = c("auto", "gaussian", "bootstrap"),
                                quantity = c("parameters", "expectation"),
                                family_index = NULL, marginalise = NULL,
                                integration = c("quadrature", "monte_carlo"),
                                n_quad = 40L, n_integrate = 5000L) {
  interval <- match.arg(interval)
  object <- with_model_envir(object, envir)
  choice <- prediction_choice(quantity[1L], scale[1L])
  quantity <- choice$quantity
  scale <- choice$scale
  if (!is.null(marginalise)) {
    if (!is.null(family_index)) stop("'family_index' is unsupported with marginalisation.", call. = FALSE)
    return(marginal_fitted_values(object, data, marginalise, quantity, scale,
      ci_level, interval, interval_method, unconditional, seed, integration,
      n_quad, n_integrate, ...))
  }
  spec <- distribution_spec(object)

  grouped <- is_gfam_family(stats::family(object))
  if (!grouped && !is.null(family_index)) stop("'family_index' is only supported for grouped-family models.", call. = FALSE)
  group_context <- if (grouped) gfam_prediction_context(object, data, family_index,
    na.action = list(...)$na.action %||% stats::na.pass) else NULL
  layout <- if (grouped) group_context$layout else
    prediction_layout(object, data, na.action = list(...)$na.action %||% stats::na.pass)
  inform_unobserved_factor_combinations(object, layout)
  object <- model_used_rows(object)
  data <- as_tibble(layout$data)

  resolved_method <- scasm_interval_method(object, interval_method, interval,
    supported = distribution_bootstrap_supported(spec, quantity))
  # compute fitted values
  if (identical(interval, "simultaneous")) {
    fit <- fit_vals_simultaneous(object,
      data = data, ci_level = ci_level, scale = scale,
      n_sim = n_sim, n_cores = n_cores, seed = seed,
      unconditional = unconditional, empty = !any(!is.na(layout$map)), ...
    )
    fit$.parameter <- if (quantity == "expectation") "expectation" else
      spec$parameters$parameter[spec$parameters$returned][1L]
    if (quantity == "expectation") {
      eta <- predict_model(object, newdata = data, type = "link", ...)
      # Check finite-mean conditions even when the expectation is a location
      # parameter, as in a scaled t model with fixed degrees of freedom.
      if (any(!is.na(layout$map)))
        distribution_output(spec, matrix(eta, ncol = 1L), quantity, scale)
      fit$.se <- fit$.se * abs(stats::family(object)$mu.eta(eta))
    }
  } else if (grouped) {
    fit <- fit_vals_grouped(object, data, group_context, quantity, scale,
      ci_level, unconditional, ...)
  } else {
    fit <- fit_vals_distribution(object, data = data, spec = spec,
      quantity = quantity, ci_level = ci_level, scale = scale,
      unconditional = unconditional, empty = !any(!is.na(layout$map)), ...
    )
  }
  if (resolved_method == "bootstrap") {
    fit <- distribution_bootstrap_limits(object, data, fit, spec, quantity,
      scale, ci_level, unconditional, ...)
  }
  if (identical(scale, "response")) {
    fit <- order_interval_bounds(fit)
  }
  # One retained observation cannot reveal the original parameter ordering.
  attr(fit, "prediction_order") <- "parameter"
  fit <- restore_prediction_table(fit, layout, include_data = TRUE)
  attr(fit, "prediction_order") <- NULL
  if (grouped) {
    input_row <- if (".row" %in% names(layout$output_data))
      layout$rows[match(fit$.row, layout$output_data$.row)] else fit$.row
    fit$.family <- group_context$family_index[input_row]
    if (quantity == "expectation" && gfam_category_expectations(group_context)) {
      fit <- gfam_expectation_table(fit, group_context, ci_level)
    }
    if (quantity == "parameters") {
      labels <- vapply(spec$components, function(s) s$parameters$parameter[1L], character(1L))
      fit$.parameter <- labels[fit$.family]
    }
  }
  if (layout$training) {
    f <- formula(object)
    if (!is.list(f)) f <- list(f)
    response <- vapply(f, function(x) if (length(x) == 3L) {
      if (is.symbol(x[[2L]])) as.character(x[[2L]]) else paste(deparse(x[[2L]]), collapse = "")
    } else "", character(1L))
    covariates <- as.character(attr(attr(object$model, "terms"), "variables"))[-1L]
    remove <- c(response, setdiff(names(layout$data), covariates), "(weights)", "(offset)")
    fit <- fit[, !names(fit) %in% remove, drop = FALSE]
  }
  attr(fit, "terms") <- NULL
  fit <- dplyr::relocate(fit, ".parameter", .before = ".fitted")
  attr(fit, "interval_info") <- scasm_interval_info(object, resolved_method,
    interval, ci_level)
  attach_prediction_spec(fit, spec, quantity, scale,
    se_scale = if (quantity == "expectation" || (spec$categorical && scale == "response"))
      "response" else "link")
}

#' @export
#' @rdname fitted_values
`fitted_values.gamm` <- function(object, ...) {
  marginal_not_supported(list(...), "fitted_values.gamm()")
  fitted_values(object$gam, ...)
}

#' @export
#' @rdname fitted_values
`fitted_values.scam` <- function(object, ..., unconditional = FALSE) {
  check_scam_unconditional(object, unconditional)
  fitted_values.gam(object, ..., unconditional = unconditional)
}

# Single-predictor adapter: the centre includes offsets, but coefficient
# deviations do not. Family-specific nonlinear predictions need other adapters.
fit_vals_simultaneous <- function(
  object, data, ci_level, scale, n_sim, n_cores, seed, unconditional,
  empty = FALSE, ...
) {
  fam <- family(object)
  supported <- c(
    "gaussian", "poisson", "binomial", "gamma", "inverse_gaussian",
    "quasi", "quasipoisson", "quasibinomial", "negative_binomial",
    "tweedie", "beta_regression", "scaled_t"
  )
  if (inherits(fam, "general.family") ||
      !family_type(object) %in% supported) {
    stop("Simultaneous intervals are not supported for this model family or class.",
      call. = FALSE)
  }
  if (!is.logical(unconditional) || length(unconditional) != 1L ||
      is.na(unconditional)) {
    stop("`unconditional` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L ||
      !is.finite(seed) || abs(seed) > .Machine$integer.max || seed != trunc(seed))) {
    stop("`seed` must be NULL or a single integer.", call. = FALSE)
  }
  response <- identical(scale, "response")
  if (response && !fam$link %in% c(
    "identity", "log", "logit", "probit", "cloglog", "cauchit",
    "sqrt", "inverse", "1/mu^2"
  )) {
    stop("This inverse link is not supported for simultaneous intervals; use `scale = 'link'`.",
      call. = FALSE)
  }
  # Resolve the corrected-covariance fallback once, including its warning.
  V <- get_vcov(object, unconditional = unconditional)
  corrected <- unconditional && !is.null(object$Vc)
  if (empty) {
    estimate <- se <- rep(NA_real_, nrow(data))
    X <- matrix(NA_real_, nrow(data), ncol(V))
  } else {
    prediction <- predict_model(object, newdata = data, ...,
      type = "link", se.fit = TRUE, unconditional = corrected)
    estimate <- as.vector(prediction$fit)
    se <- as.vector(prediction$se.fit)
    X <- predict_model(object, newdata = data, ..., type = "lpmatrix")
  }
  calculate <- function() simultaneous_intervals(
    estimate, se, X, V, level = ci_level, n_sim = n_sim, n_cores = n_cores
  )
  intervals <- if (is.null(seed)) calculate() else withr::with_seed(seed, calculate())
  fit <- tibble(
    .row = seq_len(nrow(data)), .fitted = estimate, .se = se,
    .lower_ci = intervals$lower, .upper_ci = intervals$upper
  )
  if (response) {
    lower <- intervals$lower
    upper <- intervals$upper
    invalid <- switch(fam$link,
      "inverse" = lower <= 0 & upper >= 0,
      "1/mu^2" = lower <= 0,
      "sqrt" = lower < 0,
      FALSE
    )
    if (any(invalid, na.rm = TRUE)) {
      stop("Simultaneous intervals cross the inverse-link domain boundary; use `scale = 'link'`.",
        call. = FALSE)
    }
    fit <- mutate(fit, across(all_of(c(".fitted", ".lower_ci", ".upper_ci")),
      .fns = inv_link(object)))
  }
  # prediction_layout() supplies compact data without an existing .row column.
  bind_cols(data, fit) |> relocate(".row", .before = 1L)
}
