# Joint intervals for any vector whose deviations are X times coefficient
# deviations. Callers supply SEs computed from the same X and V, and retain
# responsibility for model preparation, RNG scoping and output classes.
simultaneous_intervals <- function(
  estimate, se, X, V, level = 0.95,
  n_sim = 10000, n_cores = 1, block_size = 1000
) {
  positive_integer <- function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x) &&
      x >= 1 && x <= .Machine$integer.max && x == floor(x)
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) {
    cli::cli_abort("{.arg level} must be a number strictly between 0 and 1.")
  }
  for (nm in c("n_sim", "n_cores", "block_size")) {
    if (!positive_integer(get(nm))) {
      cli::cli_abort("{.arg {nm}} must be a positive integer.")
    }
  }
  if (!is.numeric(estimate) || !is.numeric(se) ||
      length(se) != length(estimate) ||
      !is.matrix(X) || !is.numeric(X) || nrow(X) != length(estimate) ||
      !is.matrix(V) || !is.numeric(V) || ncol(X) < 1L ||
      !identical(dim(V), rep(ncol(X), 2L))) {
    cli::cli_abort("Incompatible estimates, standard errors, X and V.")
  }
  if (any(!is.finite(V)) || !isSymmetric(unname(V))) {
    cli::cli_abort("{.arg V} must be finite and symmetric.")
  }
  if (any(is.infinite(estimate)) || any(is.infinite(se)) ||
      any(is.infinite(X)) || any(se < 0, na.rm = TRUE)) {
    cli::cli_abort("Estimates and X must not be infinite; SEs must be non-negative and not infinite.")
  }
  valid <- !is.na(estimate) & !is.na(se) & rowSums(is.na(X)) == 0L
  rows <- which(valid & se > 0)
  lower <- upper <- rep(NA_real_, length(estimate))
  names(lower) <- names(upper) <- names(estimate) %||% names(se)
  critical <- if (any(valid)) 0 else NA_real_
  if (length(rows)) {
    deviations <- simultaneous_coefficient_deviations(V, n_sim, n_cores)
    maxima <- numeric(n_sim)
    # Reuse the joint coefficient draws in every block to preserve dependence.
    for (start in seq.int(1L, length(rows), by = block_size)) {
      idx <- rows[start:min(start + block_size - 1, length(rows))]
      errors <- tcrossprod(X[idx, , drop = FALSE], deviations)
      maxima <- update_simultaneous_maxima(maxima, errors, se[idx])
    }
    critical <- stats::quantile(maxima, probs = level, type = 8)
  }
  lower[valid] <- estimate[valid] - critical * se[valid]
  upper[valid] <- estimate[valid] + critical * se[valid]
  list(critical = critical, lower = lower, upper = upper)
}

# Generate once per coverage family so row blocks and comparisons share draws.
simultaneous_coefficient_deviations <- function(V, n_sim, n_cores) {
  if (any(!is.finite(V)) || !isSymmetric(unname(V))) {
    cli::cli_abort("{.arg V} must be finite and symmetric.")
  }
  # Do not silently regularise the covariance: that changes the uncertainty.
  if (inherits(tryCatch(chol(V), error = identity), "error")) {
    cli::cli_abort("Simultaneous intervals require a positive-definite covariance matrix.")
  }
  mvnfast::rmvn(n = n_sim, mu = rep(0, ncol(V)), sigma = V, ncores = n_cores)
}

# Shared calibration kernel for linear coefficient deviations and nonlinear
# posterior differences. Rows are evaluation points, columns are shared draws.
update_simultaneous_maxima <- function(maxima, deviations, se,
                                       zero_tolerance = rep(0, length(se))) {
  if (any(!is.finite(deviations)) || any(!is.finite(se)) || any(se < 0)) {
    cli::cli_abort("Simultaneous deviations and standard errors must be finite and standard errors non-negative.")
  }
  zero <- se == 0
  if (any(zero) && any(abs(deviations[zero, , drop = FALSE]) > zero_tolerance[zero])) {
    cli::cli_abort("Zero-variance draws disagree with the fitted difference; a simultaneous band cannot be calibrated.")
  }
  if (any(!zero)) {
    standardized <- abs(sweep(deviations[!zero, , drop = FALSE],
      1L, se[!zero], FUN = "/"))
    maxima <- pmax(maxima, apply(standardized, 2L, max))
  }
  maxima
}
