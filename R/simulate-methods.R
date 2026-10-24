#' Simulate responses from a fitted model
#'
#' Generate replicated responses using fitted parameter values.
#' [posterior_samples()] also includes uncertainty in the coefficients.
#' See [diagnostic_support].
#'
#' For `simulate.gam()` to function, the `family` component of the fitted
#' model must contain, or be updatable to contain, the required random
#' number generator. See [mgcv::fix.family.rd()].
#'
#' @param object a fitted GAM, typically the result of a call to [mgcv::gam()],
#'   [mgcv::gamm()], or [gamm4::gamm4()].
#' @param nsim numeric; number of response simulations to return. Supply one
#'   finite positive whole number, no larger than `.Machine$integer.max`.
#' @param seed numeric or `NULL`; a random seed for the simulations. An explicit
#'   seed restores the previous random-number state, including its absence,
#'   when simulation finishes or fails. With `NULL`, simulation advances or
#'   initializes the random-number state.
#' @param data data frame; new observations at which the posterior draws
#'   from the model should be evaluated. If not supplied, the data used to fit
#'   the model will be used for `newdata`, if available in `object`.
#' @param weights numeric; response sampling weights. An explicit vector
#'   overrides the fitting weights; a single value applies to every observation.
#'   With `data = NULL`, the default is the fitted prior weights, except
#'   factor-valued binomial responses retain one Bernoulli trial per observation.
#'   Native `gaulss()` ignores fitting weights in its likelihood, so its default
#'   response weights are ones; explicit sampling weights still override this.
#'   With prediction data, the default is a vector of ones. For ordinary Gaussian
#'   and Gamma responses, larger weights reduce response variance (precision
#'   weights). For binomial responses, weights give integer trial counts. Include
#'   positions restored by `na.exclude` when supplying weights for fitting rows.
#' @param ... arguments passed to methods. `simulate.gam()` and
#'   `simulate.scam()` pass `...` on to `predict.gam()`. As such you can pass
#'   additional arguments such as `terms`, `exclude`, to select which model
#'   terms are included in the predictions. This may be useful, for example,
#'   for excluding the effects of random effect terms.
#' @param newdata Deprecated. Use `data` instead.
#' @param conditioning character or `NULL`; optional mixed-model conditioning
#'   mode. gamm4 defaults to `"conditional"`, holding all fitted effects fixed.
#'   `"gam_component"` explicitly uses its extracted GAM. The existing gamm
#'   simulator retains its GAM-component default. Full mixed-model modes currently
#'   use fitting observations.
#' @param envir environment or `NULL`; functions and constants needed to
#'   evaluate model expressions in prediction data. With `NULL`, use the
#'   model's formula environment. Supply raw covariates in `data`.
#' @inheritParams posterior_samples marginalise
#' @param family_index numeric; for grouped-family models, one integer family
#'   index per requested prediction row. See Details.
#'
#' @details
#' With `marginalise`, each replicate draws new coefficients for selected
#' `re` and `fs` terms before drawing responses. Other coefficients and all
#' variance parameters stay fixed. Supply grouping columns for new data;
#' repeated group labels share effects within a replicate. Unfamiliar labels
#' are allowed only when those factors occur exclusively in selected terms.
#' The `random_effect_info` attribute records the fixed covariance estimates.
#'
#' With `data = NULL`, `na.exclude` restores excluded observations as `NA` in
#' every simulation; `na.omit` returns retained observations only. Positions
#' refer to the fitting data after any `subset`. Explicit `data` are evaluated
#' independently of training exclusions: missing responses do not prevent
#' simulation, while missing required predictors or weights yield `NA`.
#' Prediction `na.action` in `...` is honoured. Missing rows are never passed
#' to the response random-number generator.
#'
#' Inverse Gaussian models use a native generator with observation dispersion
#' equal to the model dispersion divided by the prior weight, consistently with
#' their CDF and quantile helpers. Weights must be non-negative; a zero weight
#' with positive dispersion uses the limiting point mass at zero. An existing
#' random generator supplied in the family's `rd` component is preserved.
#'
#' For models fitted with [mgcv::gfam()], each observation is simulated from
#' its component family using that component's fitted scale and distribution
#' parameters. With `data = NULL`, fitted family membership is used. For explicit
#' `data`, supply `family_index` or the index in the model's response expression:
#' the second column of a matrix response, the second argument of `cbind(y, index)`,
#' or a numeric index vector in place of a named matrix response. Actual response
#' observations are not needed. Missing indices follow prediction `na.action`;
#' invalid indices are errors. Explicit indices must agree with recoverable ones.
#'
#' Grouped simulations return one numeric response per observation: binomial
#' proportions (with integer trial counts supplied in `weights`), count responses,
#' or ordered-category codes, as appropriate. Component random generators retain
#' their usual treatment of prior weights. Ordered-categorical and `ziP` components
#' are simulated from their latent predictors. General families with multiple
#' linear predictors and matrix responses encoding censoring or binomial counts
#' are outside the scope supported by `gfam()`. Scalar censored-family components
#' produce draws from their uncensored latent distributions.
#'
#' @return (Currently) A data frame with `nsim` columns.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom stats simulate runif family
#' @importFrom mgcv fix.family.rd
#'
#' @export
#'
#' @rdname simulate
#'
#' @examples
#' load_mgcv()
#' dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' sims <- simulate(m1, nsim = 5, seed = 42)
#' head(sims)
`simulate.gam` <- function(object, nsim = 1, seed = NULL, data = newdata,
                           weights = NULL, ..., newdata = NULL,
                           family_index = NULL, envir = NULL, marginalise = NULL) {
  nsim <- sampling_count(nsim, "nsim")
  object <- with_model_envir(object, envir)
  training <- is.null(data)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (!is.null(seed)) {
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
          rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
  } else {
    if (!had_seed) runif(1)
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  }
  if (!is.null(marginalise)) {
    if (!is.null(family_index)) stop("'family_index' is unsupported with marginalisation.", call. = FALSE)
    context <- marginal_context(object, data, marginalise, response = TRUE, ...)
    betas <- matrix(rep(stats::coef(context$model), each = nsim), nrow = nsim)
    values <- marginal_response_values(context, betas, weights)
    values <- slice_observations(values, context$layout$map)
    sims <- as.data.frame(values)
    attr(sims, "seed") <- RNGstate
    attr(sims, "row_ids") <- context$layout$rows
    attr(sims, "random_effect_info") <- context$random_effect_info
    class(sims) <- c("simulate_gratia", class(sims))
    return(sims)
  }
  if (is_gfam_family(family(object))) {
    if (!is.null(newdata)) newdata_deprecated()
    sims <- simulate_gfam(object, nsim = nsim, data = data, weights = weights,
      family_index = family_index, ...)
    attr(sims, "seed") <- RNGstate
    class(sims) <- c("simulate_gratia", class(sims))
    return(sims)
  }
  if (!is.null(family_index)) {
    stop("'family_index' is only available for grouped-family models.", call. = FALSE)
  }
  ## rd function if available
  rd_fun <- missing_safe_rd(choose_rd_fun(object),
    n_response = if (family_type(object) == "multivariate_normal") n_eta(object) else 1L
  )

  ## dispersion or scale variable for simulation
  scale <- object[["sig2"]]
  if (is.null(scale)) {
    scale <- summary(object)[["dispersion"]]
  }

  if (!is.null(newdata)) {
    newdata_deprecated()
  }

  layout <- prediction_layout(object, data, na.action = list(...)$na.action %||% stats::na.pass)
  if (is.null(data) && is.null(weights)) {
    used <- model_used_rows(object)
    weights <- binomial_sampling_weights(used, used[["prior.weights"]])
  } else {
    n_input <- if (is.null(data)) length(model_row_map(object)) else NROW(data)
    if (is.null(weights)) weights <- rep(1, n_input)
    if (length(weights) == 1L) weights <- rep(weights, n_input)
    if (length(weights) != n_input) stop("Weights must have one value per prediction row.")
    valid_rows <- layout$rows[!is.na(layout$map)]
    weights <- weights[valid_rows]
  }
  object <- model_used_rows(object)
  data <- layout$data
  if (!length(weights)) weights <- 1

  # some families need link scale predictions
  # this duplicates code from fitted_values, and is perhaps overkill, but I'll
  # leave it in case I need something more complex for other families I haven't
  # looked at yet
  fam_type <- family_type(object)
  fam <- case_when(
    grepl(
      "^ordered_categorical",
      fam_type, ignore.case = TRUE
    ) == TRUE ~ "ocat",
    .default = "default"
  )
  prediction_type <- if (identical(fam, "default")) "response" else "link"
  mu <- if (training) {
    predict_model(object, type = prediction_type, ...)
  } else {
    predict_model(object, newdata = data, type = prediction_type, ...)
  }

  if (training) {
    correction <- fitting_link_correction(object)
    if (any(correction != 0, na.rm = TRUE)) {
      eta <- predict_model(object, type = "link", ...) + correction
      mu <- if (identical(fam, "ocat")) eta else
        family_prediction_parameters(family(object), eta)
    }
  }

  if (is_lss_family(stats::family(object))) {
    context <- lss_prediction_context(object, layout = layout, ...)
    mu <- context$spec$to_native(eta = lss_predictors(context, stats::coef(object)))
  }

  if (training && !is.null(object$AR1.rho) && object$AR1.rho != 0) {
    object$prior.weights <- weights
    distribution <- response_distribution(object)
    if (!is.function(distribution$rd)) stop("The working AR1 correction does not define a joint response simulator for this family.", call. = FALSE)
    rd_fun <- function(mu, wt, scale) distribution$rd() - distribution$mean + mu
  }

  # call RNG function
  sims <- replicate(
    nsim,
    if (all(is.na(layout$map))) {
      if (fam_type == "multivariate_normal") {
        matrix(NA_real_, length(layout$map), n_eta(object))
      } else {
        rep(NA_real_, length(layout$map))
      }
    } else {
      slice_observations(rd_fun(mu = mu, wt = weights, scale = scale), layout$map)
    },
    simplify = FALSE
  )

  if (
    is_multivariate_y(object) &&
      identical(fam_type, "multivariate_normal")
  ) {
    n_lp <- n_eta(object)
    sims <- sims |>
      lapply(FUN = c) |>
      data.frame() |>
      setNames(nm = paste("sim", seq_len(nsim), sep = "_")) |>
      add_column(
        .yvar = rep(
          paste0("response", seq_len(n_lp)),
          each = length(layout$map)
        ),
        .before = 1L
      )
    #rownames(sims) <- NULL
  } else {
    sims <- sims |>
      as.data.frame() |>
      setNames(nm = paste("sim", seq_len(nsim), sep = "_"))
  }

  attr(sims, "row_ids") <- layout$rows
  attr(sims, "seed") <- RNGstate
  class(sims) <- append(class(sims), "simulate_gratia", after = 0L)

  sims
}

#' @rdname simulate
#'
#' @export
simulate.gamm <- function(object, nsim = 1, seed = NULL, data = newdata,
    weights = NULL, ..., newdata = NULL, conditioning = NULL, envir = NULL) {
  nsim <- sampling_count(nsim, "nsim")
  marginal_not_supported(list(...), "simulate.gamm()")
  if (is.null(conditioning) || identical(conditioning, "gam_component")) {
    if (!is.null(newdata)) newdata_deprecated()
    return(simulate.gam(object$gam, nsim = nsim, seed = seed, data = data,
      weights = weights, envir = envir, ...))
  }
  if (!is.null(data) || !is.null(weights) || !is.null(envir) || length(list(...)))
    stop("Full mixed-model simulation currently uses the fitting observations.", call. = FALSE)
  simulate_gamm_response(object, nsim, seed, conditioning)
}

#' @rdname simulate
#'
#'
#' @export
`simulate.scam` <- function(object, nsim = 1, seed = NULL, data = newdata,
                            weights = NULL, ..., newdata = NULL, envir = NULL) {
  nsim <- sampling_count(nsim, "nsim")
  if (!is.null(object$AR1.rho) && object$AR1.rho != 0 && !is.null(data)) {
    stop("AR1 response draws require fitting observations; a joint correlation structure for new data is not defined.",
      call. = FALSE)
  }
  out <- simulate.gam(object, nsim = nsim, seed = seed, data = data,
                      weights = weights, ..., newdata = newdata, envir = envir)
  rng <- attr(out, "seed")
  out <- as.matrix(out)
  dimnames(out) <- NULL
  attr(out, "seed") <- rng
  class(out) <- c("simulate_gratia", class(out))
  out
}

#' @export
`print.simulate_gratia` <- function(x, ...) {
  orig <- x
  attr(x, "seed") <- NULL
  class(x) <- class(x)[-1]
  NextMethod()
  invisible(orig)
}
