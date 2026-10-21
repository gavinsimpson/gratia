# These constructors use response parameters rather than a fitting package's
# family signatures. The location or Weibull scale need not be the mean.
logistic_response_distribution <- function(location, scale) {
  n <- length(location)
  scale <- rep_len(scale, n)
  cdf <- function(q, log_p = FALSE, lower_tail = TRUE)
    stats::plogis(q, location, scale, lower.tail = lower_tail, log.p = log_p)
  qf <- function(p, log_p = FALSE, lower_tail = TRUE)
    stats::qlogis(p, location, scale, lower.tail = lower_tail, log.p = log_p)
  rd <- function() stats::rlogis(n, location, scale)
  new_response_distribution(location, rd, cdf, qf,
    bounds = function(y) list(lower = y, upper = y),
    variance = pi^2 * scale^2 / 3, family = "logistic", cdf_left = cdf)
}

weibull_response_distribution <- function(scale, shape) {
  n <- length(scale)
  shape <- rep_len(shape, n)
  cdf <- function(q, log_p = FALSE, lower_tail = TRUE) {
    log_hazard <- shape * (log(pmax(q, 0)) - log(scale))
    log_probability <- -exp(log_hazard)
    if (lower_tail) {
      log_probability <- log1mexp(log_probability)
      # Keep log probabilities finite when the cumulative hazard underflows.
      tiny <- which(log_hazard < log(.Machine$double.eps))
      log_probability[tiny] <- log_hazard[tiny]
    }
    if (log_p) log_probability else exp(log_probability)
  }
  qf <- function(p, log_p = FALSE, lower_tail = TRUE) {
    log_probability <- quantile_log_probability(p, log_p)
    log_hazard <- log_negative_log_probability(log_probability,
      complement = lower_tail)
    exp(log(scale) + log_hazard / shape)
  }
  rd <- function() stats::rweibull(n, shape, scale)
  log_mean_factor <- lgamma(1 + 1 / shape)
  mean <- scale * exp(log_mean_factor)
  variance <- mean^2 * expm1(lgamma(1 + 2 / shape) - 2 * log_mean_factor)
  new_response_distribution(mean, rd, cdf, qf,
    bounds = function(y) list(lower = y, upper = y),
    variance = variance, family = "weibull", cdf_left = cdf)
}

inverse_gaussian_response_distribution <- function(mu, dispersion) {
  dispersion <- rep_len(dispersion, length(mu))
  cdf <- function(q, log_p = FALSE, lower_tail = TRUE)
    cdf_invgaussian(q, mu, wt = 1, scale = dispersion,
      log_p = log_p, lower_tail = lower_tail)
  qf <- function(p, log_p = FALSE, lower_tail = TRUE)
    qf_invgaussian(p, mu, wt = 1, scale = dispersion,
      log_p = log_p, lower_tail = lower_tail)
  rd <- function() rd_invgaussian(mu, wt = 1, scale = dispersion)
  new_response_distribution(mu, rd, cdf, qf,
    bounds = function(y) list(lower = y, upper = y),
    variance = dispersion * mu^3, family = "inverse_gaussian", cdf_left = cdf)
}

# The usual Gumbel has CDF exp(-exp(-z)); reversing it reflects the response
# about its location. GJRM calls these orientations rGU and GU, respectively.
gumbel_response_distribution <- function(location, scale, reverse = FALSE) {
  n <- length(location)
  scale <- rep_len(scale, n)
  direction <- if (reverse) -1 else 1
  cdf <- function(q, log_p = FALSE, lower_tail = TRUE) {
    z <- direction * (q - location) / scale
    lower <- if (reverse) !lower_tail else lower_tail
    log_probability <- -exp(-z)
    if (!lower) {
      log_probability <- log1mexp(log_probability)
      # In this tail, 1 - exp(-exp(-z)) equals exp(-z) to machine precision.
      tiny <- which(z > -log(.Machine$double.eps))
      log_probability[tiny] <- -z[tiny]
    }
    if (log_p) log_probability else exp(log_probability)
  }
  qf <- function(p, log_p = FALSE, lower_tail = TRUE) {
    log_probability <- quantile_log_probability(p, log_p)
    lower <- if (reverse) !lower_tail else lower_tail
    log_neg_log <- log_negative_log_probability(log_probability,
      complement = !lower)
    location - direction * scale * log_neg_log
  }
  rd <- function() qf(stats::runif(n))
  mean <- location + direction * 0.5772156649015329 * scale
  new_response_distribution(mean, rd, cdf, qf,
    bounds = function(y) list(lower = y, upper = y),
    variance = pi^2 * scale^2 / 6,
    family = if (reverse) "reverse_gumbel" else "gumbel", cdf_left = cdf)
}

# Calculate log(-log(p)), or log(-log(1-p)), from log(p). The latter tends
# to log(p) for tiny p, even when 1-p rounds to one.
log_negative_log_probability <- function(log_probability, complement = FALSE) {
  if (!complement) return(log(-log_probability))
  out <- log(-log1mexp(log_probability))
  tiny <- which(log_probability < log(.Machine$double.eps))
  out[tiny] <- log_probability[tiny]
  out
}
