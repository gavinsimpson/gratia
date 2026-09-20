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
#' @param object a fitted GAM, typically the result of a call to [mgcv::gam]`
#'   or [mgcv::gamm()].
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
                           weights = NULL, ..., newdata = NULL) {
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

  simulate(object$gam,
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
