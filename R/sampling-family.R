# Keep the fitted response and design attributes: censoring, grouped families,
# and multiple linear predictors use information beyond a numeric response.
sampling_family_target <- function(model) {
  sampling_target_check(model, "family")
  family <- stats::family(model)
  if (startsWith(tolower(family$family), "quasi")) stop(
    "The family MH backend requires a likelihood; quasi families are unsupported.",
    call. = FALSE)
  if (identical(family$family, "Cox PH")) stop(
    "The family MH backend does not support Cox proportional hazards models.",
    call. = FALSE)
  general <- inherits(family, "general.family")
  if (if (general) !is.function(family$ll) else
      !is.function(family$dev.resids) || !is.function(family$linkinv)) stop(
    "The family MH backend requires 'll', or 'dev.resids' and 'linkinv' functions.",
    call. = FALSE)
  S <- sampling_target_penalty(model, "family")
  model <- model_used_rows(model)
  beta <- unname(stats::coef(model))
  phi <- model$sig2
  if (length(phi) != 1L || !is.finite(phi) || phi <= 0) stop(
    "A positive fitted dispersion is required.", call. = FALSE)
  # predict.gam uses original covariates for both ordinary and discrete bam.
  # lpi attributes map columns to the predictors in general.family$ll().
  X <- mgcv::predict.gam(model, newdata = model$model, type = "lpmatrix",
    na.action = stats::na.pass)
  sampling_check_bam_fixed(model, X, "family")
  # mvn() caches cross-products of the mean design before adding covariance
  # coefficients. Prediction rebuilds X without that fitting-time attribute.
  if (identical(family$family, "Multivariate normal")) {
    mean_columns <- seq_len(max(unlist(attr(X, "lpi"))))
    attr(X, "XX") <- crossprod(X[, mean_columns, drop = FALSE])
  }
  n <- nrow(X)
  w <- model$prior.weights %||% rep(1, n)
  if (ncol(X) != length(beta) || any(!is.finite(X)) ||
      is.null(model$y) || NROW(model$y) != n ||
      length(w) != n || any(!is.finite(w) | w < 0)) stop(
    "The fitted response, weights, and design matrix do not align.", call. = FALSE)
  offsets <- sampling_fitting_offsets(model, X)
  offset <- if (general) offsets else offsets[[1L]]
  log_likelihood <- sampling_family_likelihood(X, model$y, w, offset,
    family, phi, general)
  V <- unname(stats::vcov(model))
  R <- tryCatch(chol(V), error = function(e) NULL)
  target <- list(X = X, S = S, beta = beta, phi = phi, V = V, R = R,
    log_likelihood = log_likelihood, names = names(stats::coef(model)),
    backend = "family")
  # Fail before starting workers when the family cannot evaluate its fitted model.
  if (!is.finite(sampling_target_eval(target, beta))) stop(
    "The family MH backend has non-finite posterior density at the fitted coefficients.",
    call. = FALSE)
  target
}

sampling_family_likelihood <- function(X, y, w, offset, family, phi, general) {
  force(X)
  force(y)
  force(w)
  force(offset)
  force(family)
  force(phi)
  if (general) {
    function(beta) {
      value <- family$ll(y, X, beta, w, family, offset = offset, deriv = 0)$l
      sampling_family_value(value / phi)
    }
  } else {
    offset <- offset %||% 0
    function(beta) {
      eta <- X %*% beta + offset
      if (is.function(family$valideta) && !isTRUE(family$valideta(eta))) return(-Inf)
      mu <- family$linkinv(eta)
      if (is.function(family$validmu) && !isTRUE(family$validmu(mu))) return(-Inf)
      sampling_family_value(-sum(family$dev.resids(y, mu, w)) / (2 * phi))
    }
  }
}

sampling_family_value <- function(value) {
  if (!is.numeric(value) || length(value) != 1L) stop(
    "The family evaluator must return one numeric log likelihood.", call. = FALSE)
  if (is.finite(value)) as.numeric(value) else -Inf
}

# Stored offsets describe the native fitting target; prediction additions
# must not be added twice. Prediction metadata supplies absent components only.
# General families can ignore a call-level offset retained in the frame,
# so the frame column cannot establish which offsets entered the likelihood.
sampling_fitting_offsets <- function(model, X, rows = NULL) {
  n_fit <- NROW(model$model)
  if (is.null(rows)) rows <- seq_len(n_fit)
  if (length(rows) != NROW(X) || anyNA(rows) ||
      any(rows < 1L | rows > n_fit)) stop(
    "Fitting offset rows do not align with the posterior design matrix.",
    call. = FALSE)
  offsets <- prediction_offsets(model, X)
  stored <- model$offset
  if (!is.list(stored)) stored <- list(stored)
  for (i in seq_along(offsets)) {
    actual <- if (i <= length(stored)) stored[[i]] else NULL
    if (is.null(actual)) next
    if (!length(actual) %in% c(1L, n_fit)) stop(
      "The fitted offsets do not align with the model frame.", call. = FALSE)
    actual <- if (length(actual) == 1L) rep(actual, length(rows)) else actual[rows]
    offsets[[i]] <- actual
  }
  if (any(vapply(offsets, function(x) any(!is.finite(x)), logical(1L)))) stop(
    "The fitted offsets must be finite.", call. = FALSE)
  offsets
}
