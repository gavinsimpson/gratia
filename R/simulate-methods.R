#' Simulate from the posterior distribution of a GAM
#'
#' Simulations from the posterior distribution of a fitted GAM model involve
#' computing predicted values for the observation data for which simulated
#' data are required, then generating random draws from the probability
#' distribution used when fitting the model.
#'
#' For `simulate.gam()` to function, the `family` component of the fitted
#' model must contain, or be updatable to contain, the required random
#' number generator. See [mgcv::fix.family.rd()].
#'
#' @param object a fitted GAM, typically the result of a call to [mgcv::gam()],
#'   [mgcv::gamm()], or [gamm4::gamm4()].
#' @param nsim numeric; the number of posterior simulations to return.
#' @param seed numeric; a random seed for the simulations.
#' @param data data frame; new observations at which the posterior draws
#'   from the model should be evaluated. If not supplied, the data used to fit
#'   the model will be used for `newdata`, if available in `object`.
#' @param weights numeric; a vector of prior weights. If `newdata` is null
#'   then defaults to `object[["prior.weights"]]`, otherwise a vector of ones.
#' @param ... arguments passed to methods. `simulate.gam()` and
#'   `simulate.scam()` pass `...` on to `predict.gam()`. As such you can pass
#'   additional arguments such as `terms`, `exclude`, to select which model
#'   terms are included in the predictions. This may be useful, for example,
#'   for excluding the effects of random effect terms.
#' @param newdata Deprecated. Use `data` instead.
#' @param family_index numeric; for grouped-family models, one integer family
#'   index per requested prediction row. See Details.
#'
#' @details
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
                           family_index = NULL) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
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
  if (is.null(data)) {
    weights <- model_used_rows(object)[["prior.weights"]]
  } else {
    if (is.null(weights)) weights <- rep(1, NROW(data))
    if (length(weights) == 1L) weights <- rep(weights, NROW(data))
    if (length(weights) != NROW(data)) stop("Weights must have one value per prediction row.")
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
  mu <- if (identical(fam, "default")) {
    predict(object, newdata = data, type = "response", ...)
  } else {
    predict(object, newdata = data, type = "link", ...)
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

  attr(sims, "seed") <- RNGstate
  class(sims) <- append(class(sims), "simulate_gratia", after = 0L)

  sims
}

#' @rdname simulate
#'
#' @export
`simulate.gamm` <- function(object, nsim = 1, seed = NULL, data = newdata,
                            weights = NULL, ..., newdata = NULL) {
  if (!is.null(newdata)) {
    newdata_deprecated()
  }

  simulate.gam(object$gam,
    nsim = nsim, seed = seed, data = data,
    weights = weights, ...
  )
}

#' @rdname simulate
#'
#'
#' @export
`simulate.scam` <- function(object, nsim = 1, seed = NULL, data = newdata,
                            weights = NULL, ..., newdata = NULL) {
  out <- simulate.gam(object, nsim = nsim, seed = seed, data = data,
                      weights = weights, ..., newdata = newdata)
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
