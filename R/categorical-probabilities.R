# Keep probabilities and their complements on the log scale until needed.
# This uses the same tail choice as the ordinal moment calculation: subtract
# survival probabilities when both CDF values are close to one. Log differences
# also retain the logit when the probability itself rounds to zero or one.
ordinal_probability_transform <- function(eta, thresholds) {
  cuts <- c(-Inf, thresholds, Inf)
  n <- length(eta)
  k <- length(cuts) - 1L
  log_p <- log_complement <- matrix(NA_real_, n, k)
  for (j in seq_len(k)) {
    lo <- cuts[j] - eta
    hi <- cuts[j + 1L] - eta
    a <- plogis(hi, log.p = TRUE)
    b <- plogis(lo, log.p = TRUE)
    upper <- which(lo > 0)
    a[upper] <- plogis(lo[upper], lower.tail = FALSE, log.p = TRUE)
    b[upper] <- plogis(hi[upper], lower.tail = FALSE, log.p = TRUE)
    log_p[, j] <- a + log(-expm1(b - a))
    log_complement[, j] <- logspace_add(plogis(lo, log.p = TRUE),
      plogis(hi, lower.tail = FALSE, log.p = TRUE))
  }
  list(probability = exp(log_p), logit = log_p - log_complement,
    derivative = exp(log_p + log_complement))
}

# Each category's log odds compare its predictor with the sum over all other
# categories. Subtracting log(1 - p) after computing p would lose that sum when
# a category probability rounds to one.
multinomial_probability_transform <- function(eta) {
  z <- cbind(0, as.matrix(eta))
  total <- multinom_logsum(z)
  log_p <- z - total
  log_complement <- vapply(seq_len(ncol(z)), function(j)
    multinom_logsum(z[, -j, drop = FALSE]) - total, numeric(nrow(z)))
  log_complement <- matrix(log_complement, nrow(z), ncol(z))
  list(probability = exp(log_p), logit = log_p - log_complement,
    derivative = exp(log_p + log_complement))
}

# Propagate uncertainty in log odds directly. For finite predictors, the logit
# and its uncertainty remain available even if exp(log_p) has rounded to a
# boundary. Missing or undefined inputs propagate through these operations.
categorical_probability_limits <- function(transform, logit_se, critical) {
  list(se = transform$derivative * logit_se,
    lower = plogis(transform$logit - critical * logit_se),
    upper = plogis(transform$logit + critical * logit_se))
}
