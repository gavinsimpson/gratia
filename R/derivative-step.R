# Choose one absolute step per differentiation coordinate. Using the fitted
# range makes the default independent of how prediction rows are batched.
derivative_step <- function(model, focal, data = NULL, eps = NULL,
                            order = 1L, type = "central") {
  if (!is.null(eps)) {
    if (!is.numeric(eps) || length(eps) != 1L || is.na(eps) ||
        !is.finite(eps) || eps <= 0) {
      cli::cli_abort("{.arg eps} must be NULL or a single positive finite number.")
    }
    return(eps)
  }

  # Stored transformed coordinates and raw covariate summaries cover both
  # differentiation scales without recovering or inverting fitting data.
  x <- model_context(model)$data[[focal]]
  if (is.null(x)) x <- model$var.summary[[focal]]
  if (is.null(x) && !is.null(data)) x <- data[[focal]]
  if (!is.numeric(x) || is.matrix(x)) {
    cli::cli_abort("The differentiation coordinate must be a numeric vector.")
  }
  x <- x[is.finite(x)]
  if (!length(x)) {
    cli::cli_abort("Cannot choose {.arg eps} without finite values of {.val {focal}}.")
  }
  scale <- diff(range(x))
  # A constant coordinate has no range; use its magnitude, or one at zero.
  if (scale == 0) scale <- max(abs(x))
  if (scale == 0) scale <- 1

  # Balance O(h^p) approximation error with O(machine epsilon / h^order)
  # rounding error. Central differences have p = 2; one-sided ones have p = 1.
  p <- if (type == "central") 2L else 1L
  h <- scale * .Machine$double.eps^(1 / (order + p))

  # Even an appropriately scaled step can disappear when added to a very
  # large coordinate. Allow for the half-step used by first central differences.
  at <- if (is.null(data)) numeric() else data[[focal]]
  magnitude <- max(abs(c(x, at[is.finite(at)])))
  h <- max(h, 8 * .Machine$double.eps * magnitude)
  # Round upwards to a multiple of the floating-point spacing at the largest
  # coordinate. This also keeps half-steps representable after large offsets.
  if (magnitude > 0) {
    spacing <- 2 * .Machine$double.eps * 2^floor(log2(magnitude))
    if (is.finite(spacing) && spacing > 0) h <- ceiling(h / spacing) * spacing
  }
  if (!is.finite(h) || h <= 0) {
    cli::cli_abort("Cannot choose a finite difference step; supply {.arg eps} explicitly.")
  }
  h
}
