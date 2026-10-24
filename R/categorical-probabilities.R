# For logistic CDF F and survival S, a category probability is
# F(hi) * S(lo) * (1 - exp(-width)). Keep the original threshold width:
# subtracting eta first can round two distinct bounds to the same value.
ordinal_probability_transform <- function(eta, thresholds, gradient = FALSE) {
  cuts <- c(-Inf, thresholds, Inf)
  n <- length(eta)
  k <- length(cuts) - 1L
  log_p <- log_complement <- matrix(NA_real_, n, k)
  slope <- if (gradient) matrix(NA_real_, n, k) else NULL
  for (j in seq_len(k)) {
    lo <- cuts[j] - eta
    hi <- cuts[j + 1L] - eta
    log_lo <- plogis(lo, log.p = TRUE)
    log_hi <- plogis(hi, lower.tail = FALSE, log.p = TRUE)
    log_p[, j] <- plogis(hi, log.p = TRUE) +
      plogis(lo, lower.tail = FALSE, log.p = TRUE) +
      log(-expm1(-(cuts[j + 1L] - cuts[j])))
    log_complement[, j] <- logspace_add(log_lo, log_hi)
    if (gradient) slope[, j] <- exp(log_lo - log_complement[, j]) -
      exp(log_hi - log_complement[, j])
  }
  out <- list(probability = exp(log_p), logit = log_p - log_complement,
    derivative = exp(log_p + log_complement))
  if (gradient) out$logit_gradient <- list(slope)
  out
}

# Center before taking logs of normalized probabilities. Subtracting the large
# original predictors from their log total can lose the normalization constant.
categorical_log_softmax <- function(z) {
  z <- z - apply(z, 1L, max)
  z - log(rowSums(exp(z)))
}

# Each category's log odds compare its predictor with the sum over all other
# categories. Conditional probabilities among those other categories give the
# derivatives of its log odds, including when its probability rounds to one.
multinomial_probability_transform <- function(eta, gradient = FALSE) {
  eta <- as.matrix(eta)
  z <- cbind(numeric(nrow(eta)), eta)
  log_p <- categorical_log_softmax(z)
  log_complement <- matrix(NA_real_, nrow(z), ncol(z))
  slopes <- if (gradient) lapply(seq_len(ncol(z) - 1L), function(j)
    matrix(NA_real_, nrow(z), ncol(z))) else NULL
  for (j in seq_len(ncol(z))) {
    other <- seq_len(ncol(z))[-j]
    log_complement[, j] <- multinom_logsum(log_p[, other, drop = FALSE])
    if (gradient) {
      weights <- exp(categorical_log_softmax(z[, other, drop = FALSE]))
      for (i in seq_along(slopes)) slopes[[i]][, j] <-
        if (i + 1L == j) 1 else -weights[, match(i + 1L, other)]
    }
  }
  out <- list(probability = exp(log_p), logit = log_p - log_complement,
    derivative = exp(log_p + log_complement))
  if (gradient) out$logit_gradient <- slopes
  out
}

# Apply the chain rule from predictors to coefficients, allowing shared
# coefficients to contribute to more than one predictor.
categorical_coefficient_gradient <- function(context, transform) {
  n <- nrow(context$X)
  k <- ncol(transform$probability)
  J <- matrix(0, n * k, ncol(context$X))
  for (i in seq_along(context$index)) {
    columns <- context$index[[i]]
    for (j in seq_len(k)) {
      rows <- seq_len(n) + (j - 1L) * n
      J[rows, columns] <- J[rows, columns, drop = FALSE] +
        context$X[, columns, drop = FALSE] * transform$logit_gradient[[i]][, j]
    }
  }
  J
}

# Propagate uncertainty in log odds directly. For finite predictors, the logit
# and its uncertainty remain available even if exp(log_p) has rounded to a
# boundary. Missing or undefined inputs propagate through these operations.
categorical_probability_limits <- function(transform, logit_se, critical) {
  list(se = transform$derivative * logit_se,
    lower = plogis(transform$logit - critical * logit_se),
    upper = plogis(transform$logit + critical * logit_se))
}
