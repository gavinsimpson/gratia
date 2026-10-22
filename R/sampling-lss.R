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
  offsets <- prediction_offsets(model, X)
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
  list(model = model, family = family, spec = distribution_spec(model), X = X, index = idx,
    offsets = do.call(cbind, offsets), layout = layout)
}

lss_predictors <- function(context, beta) {
  eta <- lapply(context$index, function(j) {
    drop(context$X[, j, drop = FALSE] %*% beta[j])
  })
  do.call(cbind, eta) + context$offsets
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

# Cache design blocks once. Keep the same matrix-vector products and one family
# generator call per draw so numerical results and RNG order remain unchanged.
lss_response_generator <- function(context, betas, weights = NULL) {
  rd <- fix_family_rd(context$family)$rd
  if (!is.function(rd)) stop("No response generator for family <",
    family_name(context$family), ">.", call. = FALSE)
  family_rd <- rd
  rd <- function(mu, wt, scale) {
    value <- family_rd(mu = mu, wt = wt, scale = scale)
    if ((!is.numeric(value) && !is.logical(value)) || !is.null(dim(value)) ||
        length(value) != NROW(mu)) stop("The response generator for family <",
      family_name(context$family), "> must return one numeric response per observation.",
      call. = FALSE)
    value
  }
  rd <- missing_safe_rd(rd)
  wt <- lss_sampling_weights(context, weights)
  dispersion <- context$model$sig2 %||% 1
  design <- lapply(context$index, function(j) context$X[, j, drop = FALSE])
  missing <- all(is.na(context$layout$map))
  function(i) {
    if (missing) return(rep(NA_real_, NROW(context$X)))
    eta <- do.call(cbind, lapply(seq_along(design), function(k) {
      drop(design[[k]] %*% betas[i, context$index[[k]]])
    })) + context$offsets
    mu <- context$spec$to_native(eta = eta)
    as.numeric(rd(mu = mu, wt = wt, scale = dispersion))
  }
}

# Streaming diagnostic calculations still request one draw at a time.
lss_response_sampler <- function(context, betas, weights = NULL) {
  generate <- lss_response_generator(context, betas, weights)
  out <- function(i) {
    value <- tibble::tibble(.row = seq_len(NROW(context$X)), .draw = i,
      .parameter = "response", .response = generate(i))
    restore_prediction_table(value, context$layout)
  }
  attr(out, "sampling") <- attr(betas, "sampling")
  out
}

# Public output necessarily contains every response. Fill its values directly
# and restore missing rows by position, then construct the table once.
lss_response_draws <- function(context, betas, weights = NULL) {
  generate <- lss_response_generator(context, betas, weights)
  layout <- context$layout
  n <- length(layout$map)
  draws <- NROW(betas)
  values <- matrix(NA_real_, n, draws)
  for (i in seq_len(draws)) values[, i] <- generate(i)[layout$map]
  dim(values) <- NULL
  tibble::tibble(.row = rep(layout$rows, draws),
    .draw = rep(seq_len(draws), each = n), .parameter = "response",
    .response = values)
}
