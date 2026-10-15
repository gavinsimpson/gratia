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
