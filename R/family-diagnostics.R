#' Extend diagnostics for an mgcv-compatible family
#'
#' A family can supply a `gratia_diagnostics` function to describe its response
#' distribution without adding family-name checks to gratia. The function is
#' called with named arguments `mu`, `wt`, `scale`, `y` and `eta`. These contain
#' response parameters, weights, fitted scale, the fitting response and linear
#' predictors, respectively. Diagnostics use fitted parameters and sampling
#' weights; [log_lik()] uses draw-specific parameters and likelihood weights.
#' Missing fitting rows have been removed;
#' grouped families receive only their own rows. `eta` can be `NULL` when a
#' distribution is constructed without a fitted model.
#'
#' @details
#' Return a named list. Omitted fields retain gratia's existing family defaults;
#' an explicit `NULL` removes an optional function or moment. Existing `$rd`,
#' `$cdf` and `$qf` functions are preserved and supplied with their usual mgcv
#' arguments. Replacement functions in the returned list instead capture fitted
#' parameters and use the following interface:
#'
#' * `mean`, `variance`: numeric response moments, one per observation. A matrix
#'   is allowed for multiple response columns. Use the full response expectation,
#'   rather than a location parameter or a component mean.
#' * `cdf`, `cdf_left`: functions `(q, log_p = FALSE, lower_tail = TRUE)` for
#'   `P(Y <= q)` and `P(Y < q)`, respectively. They must support both tails and
#'   logarithms. For an upper tail, `cdf_left` returns `P(Y >= q)`.
#' * `qf`: a function `(p, log_p = FALSE, lower_tail = TRUE)` for quantiles.
#' * `rd`: a function with no arguments that draws a complete response vector
#'   (or response matrix), retaining any dependence between observations.
#' * `log_lik`: a function `(y)` returning one normalised log density or log
#'   probability mass per observation, including fitting weights. A response
#'   vector contributes one value; a multivariate response contributes one joint
#'   value per row. [log_lik()] calls the factory at each coefficient draw with
#'   likelihood weights, which can differ from diagnostic sampling weights.
#'   Parameters represented by `mu` and `eta` vary with that draw; the hook must
#'   exclude smoothing penalties and priors. Finite positive log densities and
#'   `-Inf` for impossible observations are allowed; missing values and `Inf`
#'   are invalid.
#' * `bounds`: a function `(y)` returning a list with numeric `lower` and `upper`
#'   response bounds. Censoring intervals use their endpoints. Exact count
#'   observations can use `y - 1` and `y`. Alternatively, supply `cdf_left`:
#'   gratia then randomises between `P(Y < y)` and `P(Y <= y)` automatically.
#'   A bounds list may also supply `cdf_lower` and `cdf_upper`, functions
#'   `(log_p = FALSE, lower_tail = TRUE)` that evaluate its endpoint probabilities,
#'   and `left_limit`, a logical vector marking rows to randomise at equal bounds.
#' * `continuous`: set to `TRUE` to declare that responses have no probability
#'   masses. An unfamiliar grouped-family component must supply `bounds`,
#'   `cdf_left`, or this declaration before using PIT/quantile residuals.
#' * `diagnostic_residuals`: a function `(y, type)` for observed and reference
#'   deviance, response and Pearson residuals. Otherwise the family's existing
#'   residual method is used.
#' * `observe`: a function `(y)` applying an observation process, such as known
#'   censoring thresholds, to generated responses before traditional QQ/worm
#'   reference residuals are calculated. Return the response encoding expected
#'   by the family's residual method. This function is not applied to fitting
#'   observations or to the public response and posterior samplers.
#' * `reference_available`, `reference_reason`: a single logical value and an
#'   explanation. Set availability to `FALSE` when traditional distribution-based
#'   references cannot be generated, for example when censoring thresholds are
#'   unknown. Analytic PIT/quantile plots remain available.
#' * `mean_available`, `variance_available`, `moment_reason`: single logical
#'   values specifying whether the population moments exist, and an explanation
#'   used when a requested residual requires an undefined moment.
#' * `independent`, `joint_available`, `dependence_reason`: single logical values
#'   describing independence and availability of joint response draws, and an
#'   explanation of dependence. `joint_available` requires an `rd` function.
#' * `approximation`: an optional explanation shown in diagnostic subtitles.
#'
#' Functions must return one value per response in fitting order. Probabilities
#' must be valid and lower bounds must not exceed upper bounds. gratia validates
#' supplied values and reports errors instead of silently using another method.
#' The factory is called again for each prepared distribution; avoid changing
#' the family or drawing random numbers in the factory itself.
#'
#' Adding this function does not extend a family's supported fitting methods.
#' Posterior predictive sampling still uses the fitting package's parameter and
#' response sampling methods. The likelihood hook also operates at supplied
#' coefficient draws.
#'
#' @name family_diagnostics
#' @seealso [diagnostic_support], [quantile_residuals()], [appraise()]
#' @examples
#' load_mgcv()
#' f <- poisson()
#' f$gratia_diagnostics <- function(mu, wt, scale, y, eta) {
#'   list(mean = mu, variance = mu,
#'     cdf_left = function(q, log_p = FALSE, lower_tail = TRUE) {
#'       ppois(ceiling(q) - 1, lambda = mu,
#'         log.p = log_p, lower.tail = lower_tail)
#'     })
#' }
#' dat <- data.frame(x = seq(0, 1, length.out = 40))
#' dat$y <- rep(c(0, 1, 2, 4), 10)
#' f$rd <- function(mu, wt, scale) rpois(length(mu), mu)
#' f$qf <- function(p, mu, wt, scale) qpois(p, mu)
#' f$cdf <- function(q, mu, wt, scale) ppois(q, mu)
#' fit <- gam(y ~ x, data = dat, family = f, method = "REML")
#' qq_plot(fit, type = "quantile", seed = 1)
NULL

extend_family_distribution <- function(d, family, mu, wt, scale, y, eta) {
  factory <- family$gratia_diagnostics
  if (is.null(factory)) return(d)
  fail <- function(message) stop("Family <", family$family,
    "> gratia_diagnostics: ", message, call. = FALSE)
  if (!is.function(factory)) fail("must be a function.")
  extra <- factory(mu = mu, wt = wt, scale = scale, y = y, eta = eta)
  fields <- c("mean", "variance", "rd", "cdf", "qf", "cdf_left", "bounds", "log_lik",
    "diagnostic_residuals", "observe", "reference_available", "reference_reason",
    "mean_available", "variance_available", "moment_reason", "independent",
    "joint_available", "dependence_reason", "approximation", "continuous")
  if (!is.list(extra) || (length(extra) && (is.null(names(extra)) ||
      anyNA(names(extra)) || any(!nzchar(names(extra))) || anyDuplicated(names(extra)))))
    fail("return a named list with unique field names.")
  unknown <- setdiff(names(extra), fields)
  if (length(unknown)) fail(paste0("unknown field(s): ", paste(unknown, collapse = ", "), "."))
  hooks <- c("rd", "cdf", "qf", "cdf_left", "bounds", "diagnostic_residuals", "observe", "log_lik")
  for (name in intersect(hooks, names(extra)))
    if (!is.null(extra[[name]]) && !is.function(extra[[name]]))
      fail(paste0(name, " must be a function or NULL."))
  flags <- c("independent", "joint_available", "mean_available", "variance_available", "reference_available", "continuous")
  for (name in intersect(flags, names(extra)))
    if (!is.logical(extra[[name]]) || length(extra[[name]]) != 1L || is.na(extra[[name]]))
      fail(paste0(name, " must be TRUE or FALSE."))
  for (name in intersect(c("dependence_reason", "moment_reason", "reference_reason", "approximation"), names(extra)))
    if (!is.null(extra[[name]]) && (!is.character(extra[[name]]) ||
        length(extra[[name]]) != 1L || is.na(extra[[name]])))
      fail(paste0(name, " must be a single string or NULL."))
  d[names(extra)] <- extra
  if (is.function(d$log_lik)) {
    hook <- d$log_lik
    n_observations <- NROW(mu)
    d$log_lik <- local({
      fun <- hook
      function(y) {
        value <- fun(y)
        if (!is.numeric(value) || !is.null(dim(value)) ||
            length(value) != n_observations || anyNA(value) || any(value == Inf))
          fail("log_lik must return one valid log likelihood per observation.")
        value
      }
    })
  }
  d$cdf_bounds_supplied <- is.function(extra$bounds) || is.function(extra$cdf_left) ||
    isTRUE(extra$continuous)
  n <- if (is.matrix(d$mean)) length(d$mean) else NROW(mu)
  if (is.matrix(d$mean) && NROW(d$mean) != NROW(mu))
    fail("mean must have one row per observation.")
  for (name in c("mean", "variance")) {
    value <- d[[name]]
    if (!is.null(value) && (!is.numeric(value) || length(value) != n ||
        (isTRUE(d[[paste0(name, "_available")]]) && any(!is.finite(value))) ||
        (name == "variance" && any(value < 0, na.rm = TRUE))))
      fail(paste0(name, " must contain one valid value per response."))
  }
  if ("rd" %in% names(extra) && !"joint_available" %in% names(extra))
    d$joint_available <- is.function(d$rd)
  if (isTRUE(d$joint_available) && !is.function(d$rd))
    fail("joint_available requires an rd function.")
  for (name in intersect(c("rd", "qf", "diagnostic_residuals"), names(extra))) {
    hook <- d[[name]]
    if (is.null(hook)) next
    d[[name]] <- local({
      fun <- hook
      label <- name
      check <- function(value) {
        if (!is.numeric(value) || length(value) != n ||
            (label != "diagnostic_residuals" && anyNA(value)))
          fail(paste0(label, " must return one numeric value per response."))
        value
      }
      switch(label,
        rd = function() check(fun()),
        qf = function(p, log_p = FALSE, lower_tail = TRUE)
          check(fun(p, log_p = log_p, lower_tail = lower_tail)),
        diagnostic_residuals = function(y, type) check(fun(y, type)))
    })
  }
  d$marginal_rd <- d$rd
  probability <- function(value, log_p) {
    if (!is.numeric(value) || length(value) != n || anyNA(value) ||
        if (log_p) any(value > 0) else any(!is.finite(value) | value < 0 | value > 1))
      fail("a CDF returned invalid probabilities or the wrong number of responses.")
    value
  }
  for (name in intersect(c("cdf", "cdf_left", "qf"), names(extra))) {
    hook <- extra[[name]]
    if (is.null(hook)) next
    required <- c(if (name == "qf") "p" else "q", "log_p", "lower_tail")
    if (!all(required %in% names(formals(hook))))
      fail(paste0(name, " must accept ", paste(required, collapse = ", "), "."))
  }
  for (name in c("cdf", "cdf_left")) {
    hook <- d[[name]]
    if (!is.function(hook)) next
    d[[name]] <- local({
      fun <- hook
      wrapped <- function(q, log_p = FALSE, lower_tail = TRUE)
        probability(fun(q, log_p = log_p, lower_tail = lower_tail), log_p)
      attr(wrapped, "native_log_tails") <- if (name %in% names(extra)) TRUE else
        attr(hook, "native_log_tails")
      wrapped
    })
  }
  if ("cdf_left" %in% names(extra) && is.function(d$cdf_left) && !"bounds" %in% names(extra)) {
    d$bounds <- function(y) list(lower = y, upper = y, left_limit = rep(TRUE, length(y)),
      cdf_lower = function(log_p = FALSE, lower_tail = TRUE)
        d$cdf_left(y, log_p, lower_tail))
  }
  if (is.function(d$bounds)) {
    bounds <- d$bounds
    d$bounds <- function(y) {
      b <- bounds(y)
      if (!is.list(b) || !all(c("lower", "upper") %in% names(b)) ||
          !is.numeric(b$lower) || !is.numeric(b$upper) ||
          length(b$lower) != n || length(b$upper) != n ||
          anyNA(b$lower) || anyNA(b$upper) || any(b$lower > b$upper))
        fail("bounds must return ordered lower and upper values, one per response.")
      if (!is.null(b$left_limit) && (!is.logical(b$left_limit) ||
          !length(b$left_limit) %in% c(1L, n) || anyNA(b$left_limit)))
        fail("bounds left_limit must be logical, with one value or one per response.")
      for (name in c("cdf_lower", "cdf_upper")) {
        if (is.null(b[[name]])) next
        if (!is.function(b[[name]])) fail(paste0("bounds ", name, " must be a function."))
        b[[name]] <- local({
          fun <- b[[name]]
          function(log_p = FALSE, lower_tail = TRUE)
            probability(fun(log_p = log_p, lower_tail = lower_tail), log_p)
        })
      }
      b
    }
  }
  if (isTRUE(extra$continuous) && !"bounds" %in% names(extra))
    d$bounds <- function(y) list(lower = y, upper = y)
  if (any(c("bounds", "cdf") %in% names(extra)) && !"cdf_left" %in% names(extra)) {
    d$cdf_left <- if (is.function(d$cdf)) function(q, log_p = FALSE, lower_tail = TRUE) {
      b <- if (is.function(d$bounds)) d$bounds(q) else list(lower = q)
      if (is.function(b$cdf_lower)) b$cdf_lower(log_p, lower_tail) else
        d$cdf(b$lower, log_p, lower_tail)
    } else NULL
  }
  d
}

check_distribution_reference <- function(d) {
  if (identical(d$reference_available, FALSE))
    stop("Distribution-based QQ references are unavailable. ",
      d$reference_reason %||% "No observation process was supplied.", call. = FALSE)
  invisible(d)
}

observe_diagnostic_response <- function(d, y) {
  check_distribution_reference(d)
  if (!is.function(d$observe)) return(y)
  value <- d$observe(y)
  if (!is.numeric(value) || NROW(value) != NROW(y) || anyNA(value))
    stop("Family observation process must return one encoded response per observation.", call. = FALSE)
  value
}
