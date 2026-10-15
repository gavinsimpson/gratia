# Keep known additions separate from coefficient columns. mgcv stores formula
# offsets by predictor and fixed smooth contributions by smooth.
prediction_offsets <- function(model, X) {
  indices <- attr(X, "lpi", exact = TRUE) %||% lss_eta_index(model)
  formula <- attr(X, "model.offset", exact = TRUE)
  if (!is.list(formula)) formula <- list(formula)
  smooth <- attr(X, "offset", exact = TRUE)
  lapply(seq_along(indices), function(i) {
    off <- if (length(formula) >= i) formula[[i]] else NULL
    if (is.null(off)) off <- rep(0, nrow(X))
    if (length(off) == 1L) off <- rep(off, nrow(X))
    if (length(off) != nrow(X)) {
      stop("Prediction offsets must have one value per evaluation row.", call. = FALSE)
    }
    if (!is.null(smooth)) {
      if (is.null(dim(smooth))) smooth <- matrix(smooth, ncol = 1L)
      use <- vapply(model$smooth, function(sm) {
        any(smooth_coef_indices(sm) %in% indices[[i]])
      }, logical(1L))
      if (nrow(smooth) != nrow(X) || ncol(smooth) != length(use)) {
        stop("Fixed smooth contributions are not aligned with the prediction matrix.",
          call. = FALSE)
      }
      off <- off + rowSums(smooth[, use, drop = FALSE])
    }
    off
  })
}

# Native link prediction includes formula and fixed smooth contributions.
# Remove the model-frame argument offset before prediction, then add it once.
# This also preserves BAM's block prediction instead of allocating a full X.
fitting_linear_predictors <- function(model) {
  data <- model$model
  off <- fitting_argument_offset(model)
  data[["(offset)"]] <- NULL
  # A prepared model frame is checked against the local object's frame names.
  # Remove the special argument column from both, retaining evaluated formulas.
  model$model[["(offset)"]] <- NULL
  attr(data, "na.action") <- NULL
  eta <- predict_model(model, newdata = data, type = "link")
  if (is.null(off)) return(eta)
  if (!length(off) %in% c(1L, NROW(eta))) {
    stop("Fitting offsets are not aligned with the model frame.", call. = FALSE)
  }
  # A fitting offset argument belongs to the first predictor; offsets for
  # additional predictors must be specified in their respective formulas.
  if (is.matrix(eta)) eta[, 1L] <- eta[, 1L] + off else eta <- eta + off
  eta
}

fitting_argument_offset <- function(model) {
  attr(model, "gratia.fitting.argument.offset", exact = TRUE) %||%
    model$model[["(offset)"]]
}

# This repairs only gratia's local copy. The original fitted object and the
# fitter's own prediction methods retain their native conventions.
restore_fitting_predictors <- function(model) {
  if (!needs_fitting_offset_restore(model)) return(model)
  eta <- fitting_linear_predictors(model)
  attr(model, "gratia.fitting.predictors") <- TRUE
  if (isTRUE(all.equal(as.vector(eta), as.vector(model$linear.predictors),
      tolerance = 1e-10, check.attributes = FALSE))) return(model)
  model$linear.predictors <- eta
  f <- stats::family(model)
  if (!inherits(f, "general.family")) {
    model$fitted.values <- f$linkinv(eta)
    if (is.function(f$mu.eta)) {
      model$residuals <- (model$y - model$fitted.values) / f$mu.eta(eta)
    }
  } else {
    model$fitted.values <- family_prediction_parameters(f, eta)
  }
  model
}

needs_fitting_offset_restore <- function(model) {
  if (is.null(fitting_argument_offset(model)) ||
      isTRUE(attr(model, "gratia.fitting.predictors"))) {
    return(FALSE)
  }
  (inherits(model, "bam") && is.null(model$dinfo) &&
      "(offset)" %in% names(model$model)) ||
    !is.null(attr(model, "gratia.fitting.argument.offset", exact = TRUE))
}
