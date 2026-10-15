is_lss_family <- function(family) {
  inherits(family, "general.family") && family_type(family) %in%
    c("gaulss", "gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")
}

# Prepare all predictors once, keeping shared coefficient indices and offsets.
# Fitting-call offsets belong to fitting rows; formula offsets are evaluated
# at supplied data by predict.gam(). No observed response is needed here.
lss_prediction_context <- function(model, data = NULL, layout = NULL, ...) {
  if (is.null(layout)) layout <- prediction_layout(model, data,
    na.action = list(...)$na.action %||% stats::na.pass)
  model <- model_used_rows(model)
  X <- predict_model(model, newdata = layout$data, type = "lpmatrix", ...)
  idx <- attr(X, "lpi") %||% lss_eta_index(model)
  offsets <- attr(X, "model.offset")
  if (!is.list(offsets)) offsets <- list(offsets)
  offsets <- lapply(seq_along(idx), function(i) {
    off <- if (i <= length(offsets)) offsets[[i]] %||% 0 else 0
    rep_len(off, NROW(X))
  })
  if (layout$training) {
    eta <- matrix(predict_model(model, type = "link", ...) +
      fitting_link_correction(model), nrow = NROW(X))
    offsets <- lapply(seq_along(idx), function(i) {
      j <- idx[[i]]
      eta[, i] - drop(X[, j, drop = FALSE] %*% stats::coef(model)[j])
    })
  }
  family <- stats::family(model)
  if (family_type(family) == "gumbls") attr(family, "gratia.gumbel_mean") <-
    native_mgcv_hook(family$predict, mgcv::gumbls()$predict)
  list(model = model, family = family, X = X, index = idx,
    offsets = do.call(cbind, offsets), layout = layout)
}

lss_predictors <- function(context, beta) {
  eta <- lapply(context$index, function(j) {
    drop(context$X[, j, drop = FALSE] %*% beta[j])
  })
  do.call(cbind, eta) + context$offsets
}

lss_fitted_values <- function(context, betas, scale) {
  n <- NROW(context$X)
  values <- matrix(NA_real_, n, NROW(betas))
  if (all(is.na(context$layout$map))) return(values)
  # Bound temporary parameter matrices while using compiled matrix products.
  batch <- max(1L, floor(1e5 / n))
  starts <- seq.int(1L, NROW(betas), by = batch)
  for (start in starts) {
    rows <- seq.int(start, min(start + batch - 1L, NROW(betas)))
    predictors <- if (scale == "linear_predictor") 1L else seq_along(context$index)
    eta <- lapply(predictors, function(i) {
      j <- context$index[[i]]
      sims <- context$X[, j, drop = FALSE] %*% t(betas[rows, j, drop = FALSE])
      as.numeric(sims + context$offsets[, i])
    })
    values[, rows] <- if (scale == "linear_predictor") eta[[1L]] else {
      mu <- family_prediction_parameters(context$family, do.call(cbind, eta))
      lss_expectation(context$family, mu)
    }
  }
  values
}

lss_expectation <- function(family, mu) {
  if (family_type(family) == "gevlss" && any(mu[, 3L] >= 1, na.rm = TRUE)) {
    stop("The GEV expected response does not exist when shape >= 1; ",
      "use scale = 'linear_predictor' for location, or request response draws.",
      call. = FALSE)
  }
  value <- family_response_mean(family, mu)
  if (any(!is.finite(value))) stop("The expected response cannot be evaluated ",
    "as a finite number for these coefficient draws and covariates (family <",
    family_name(family), ">).", call. = FALSE)
  value
}

lss_sampling_weights <- function(context, weights = NULL) {
  layout <- context$layout
  if (is.null(weights) && layout$training) {
    return(diagnostic_sampling_weights(context$model,
      context$model$prior.weights %||% rep(1, NROW(context$X))))
  }
  n <- layout$n_input
  if (is.null(weights)) weights <- rep(1, n)
  if (length(weights) == 1L) weights <- rep(weights, n)
  if (length(weights) != n) {
    stop("Weights must have one value per prediction row.", call. = FALSE)
  }
  if (all(is.na(layout$map))) return(1)
  rows <- if (layout$training) which(!is.na(layout$map)) else layout$input_rows
  weights <- weights[rows]
  if (!length(weights)) 1 else weights
}

lss_response_sampler <- function(context, betas, weights = NULL) {
  rd <- fix_family_rd(context$family)$rd
  if (!is.function(rd)) stop("No response generator for family <",
    family_name(context$family), ">.", call. = FALSE)
  rd <- missing_safe_rd(rd)
  wt <- lss_sampling_weights(context, weights)
  dispersion <- context$model$sig2 %||% 1
  out <- function(i) {
    response <- if (all(is.na(context$layout$map))) rep(NA_real_, NROW(context$X)) else {
      mu <- family_prediction_parameters(context$family, lss_predictors(context, betas[i, ]))
      rd(mu = mu, wt = wt, scale = dispersion)
    }
    value <- tibble::tibble(.row = seq_len(NROW(context$X)), .draw = i,
      .parameter = "response", .response = as.numeric(response))
    restore_prediction_table(value, context$layout)
  }
  attr(out, "sampling") <- attr(betas, "sampling")
  out
}
