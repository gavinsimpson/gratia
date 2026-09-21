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
    stop("`level` must be a number strictly between 0 and 1.", call. = FALSE)
  }
  for (nm in c("n_sim", "n_cores", "block_size")) {
    if (!positive_integer(get(nm))) {
      stop("`", nm, "` must be a positive integer.", call. = FALSE)
    }
  }
  if (!is.numeric(estimate) || !is.numeric(se) ||
      length(se) != length(estimate) ||
      !is.matrix(X) || !is.numeric(X) || nrow(X) != length(estimate) ||
      !is.matrix(V) || !is.numeric(V) || ncol(X) < 1L ||
      !identical(dim(V), rep(ncol(X), 2L))) {
    stop("Incompatible estimates, standard errors, X and V.", call. = FALSE)
  }
  if (any(!is.finite(V)) || !isSymmetric(unname(V))) {
    stop("`V` must be finite and symmetric.", call. = FALSE)
  }
  if (any(is.infinite(estimate)) || any(is.infinite(se)) ||
      any(is.infinite(X)) || any(se < 0, na.rm = TRUE)) {
    stop("Estimates and X must not be infinite; SEs must be non-negative and not infinite.",
      call. = FALSE)
  }
  valid <- !is.na(estimate) & !is.na(se) & rowSums(is.na(X)) == 0L
  rows <- which(valid & se > 0)
  lower <- upper <- rep(NA_real_, length(estimate))
  names(lower) <- names(upper) <- names(estimate) %||% names(se)
  critical <- if (any(valid)) 0 else NA_real_
  if (length(rows)) {
    # mvnfast requires positive-definite covariance. Do not change uncertainty
    # by silently adding jitter to a singular or indefinite matrix.
    if (inherits(tryCatch(chol(V), error = identity), "error")) {
      stop("Simultaneous intervals require a positive-definite covariance matrix.",
        call. = FALSE)
    }
    deviations <- mvnfast::rmvn(
      n = n_sim, mu = rep(0, ncol(X)), sigma = V, ncores = n_cores
    )
    maxima <- numeric(n_sim)
    # Reuse the joint coefficient draws in every block to preserve dependence.
    for (start in seq.int(1L, length(rows), by = block_size)) {
      idx <- rows[start:min(start + block_size - 1, length(rows))]
      errors <- tcrossprod(X[idx, , drop = FALSE], deviations)
      standardized <- abs(sweep(errors, 1L, se[idx], FUN = "/"))
      maxima <- pmax(maxima, apply(standardized, 2L, max))
    }
    critical <- stats::quantile(maxima, probs = level, type = 8)
  }
  lower[valid] <- estimate[valid] - critical * se[valid]
  upper[valid] <- estimate[valid] + critical * se[valid]
  list(critical = critical, lower = lower, upper = upper)
}
