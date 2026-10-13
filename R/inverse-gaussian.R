# Inverse Gaussian probabilities, using the dispersion parameterisation
# Var(Y) = phi * mu^3. Algorithms follow Giner & Smyth (2016), R Journal
# 8(1), 339-351, with log-domain Newton steps and stable normal-tail differences.

inverse_gaussian_args <- function(x, mu, wt, scale, log_p, lower_tail) {
  for (flag in list(log_p, lower_tail)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
      cli::cli_abort("{.arg log_p} and {.arg lower_tail} must be TRUE or FALSE.")
    }
  }
  args <- list(x = x, mu = mu, wt = wt, scale = scale)
  if (!all(vapply(args, is.numeric, logical(1)))) {
    cli::cli_abort("Inverse Gaussian arguments must be numeric.")
  }
  lengths <- lengths(args)
  n <- if (any(lengths == 0L)) 0L else max(lengths)
  args <- lapply(args, function(a) if (length(a) == n) a else rep_len(a, n))
  args$phi <- args$scale / args$wt
  args$missing <- is.na(args$x) | is.na(args$mu) | is.na(args$wt) |
    is.na(args$scale)
  args$invalid <- !args$missing & (args$mu <= 0 | args$wt < 0 |
    args$scale < 0 | is.nan(args$phi))
  args
}

# Michael-Schucany-Haas sampling, with observation dispersion phi = scale/wt.
# The smaller root has ratio r = exp(-2*asinh(abs(Z)*sqrt(phi*mu)/2)).
# Evaluating this in logs avoids cancellation, mu^2 overflow, and zero roots
# caused solely by an underflowed intermediate ratio.
rd_invgaussian <- function(mu, wt, scale) {
  a <- inverse_gaussian_args(mu, mu, wt, scale, FALSE, TRUE)
  out <- a$mu
  out[] <- NA_real_
  if (any(a$invalid)) {
    warning("NaNs produced")
    out[a$invalid] <- NaN
  }
  valid <- !a$missing & !a$invalid
  point_mean <- which(valid & a$phi == 0)
  out[point_mean] <- a$mu[point_mean]
  out[which(valid & a$phi == Inf)] <- 0

  stochastic <- which(valid & a$phi > 0 & a$phi < Inf)
  z <- stats::rnorm(length(stochastic))
  log_abs_z <- log(abs(z))
  logphi <- log(a$phi[stochastic])
  inverse_chisq <- which(a$mu[stochastic] == Inf)
  out[stochastic[inverse_chisq]] <- exp(-logphi[inverse_chisq] -
    2 * log_abs_z[inverse_chisq])

  regular <- which(a$mu[stochastic] < Inf)
  rows <- stochastic[regular]
  logmu <- log(a$mu[rows])
  log_ratio <- -2 * inverse_gaussian_log_asinh(log_abs_z[regular] +
    (logphi[regular] + logmu) / 2 - log(2))
  # Choose the larger root with probability r/(1+r), computed on the log
  # scale so the branch probability is not obtained by subtracting from one.
  large <- which(log(stats::runif(length(rows))) <
    log_ratio - log1p(exp(log_ratio)))
  log_ratio[large] <- -log_ratio[large]
  value <- exp(logmu + log_ratio)
  near_mean <- which(abs(log_ratio) < log(2))
  value[near_mean] <- a$mu[rows[near_mean]] * exp(log_ratio[near_mean])
  out[rows] <- value
  out
}

cdf_invgaussian <- function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
  a <- inverse_gaussian_args(q, mu, wt, scale, log_p, lower_tail)
  q <- a$x
  out <- q
  out[] <- NA_real_
  valid <- !a$missing & !a$invalid
  if (any(a$invalid)) {
    warning("NaNs produced")
    out[a$invalid] <- NaN
  }
  # Work in lower-tail log probabilities until the regular continuous case.
  below <- which(valid & q < 0)
  above <- which(valid & q == Inf)
  out[below] <- -Inf
  out[above] <- 0
  spike_zero <- which(valid & q >= 0 & a$phi == Inf)
  out[spike_zero] <- 0
  spike_mean <- which(valid & q >= 0 & q < Inf & a$phi == 0)
  out[spike_mean] <- ifelse(q[spike_mean] < a$mu[spike_mean], -Inf, 0)
  zero <- which(valid & q == 0 & a$phi > 0 & a$phi < Inf)
  out[zero] <- -Inf
  special <- c(below, above, spike_zero, spike_mean, zero)
  if (!lower_tail) out[special] <- log1mexp(out[special])

  inverse_chisq <- which(valid & q > 0 & q < Inf & a$mu == Inf &
    a$phi > 0 & a$phi < Inf)
  out[inverse_chisq] <- stats::pchisq(
    exp(-log(a$phi[inverse_chisq]) - log(q[inverse_chisq])), df = 1,
    lower.tail = !lower_tail, log.p = TRUE)
  regular <- which(valid & q > 0 & q < Inf & a$mu < Inf &
    a$phi > 0 & a$phi < Inf)
  t <- log(q[regular]) - log(a$mu[regular])
  near_mean <- which(abs(t) < log(2))
  t[near_mean] <- log1p((q[regular[near_mean]] - a$mu[regular[near_mean]]) /
    a$mu[regular[near_mean]])
  out[regular] <- inverse_gaussian_log_cdf(log(q[regular]),
    log(a$mu[regular]), log(a$phi[regular]), lower_tail, t)
  if (log_p) out else exp(out)
}

# Retain the old internal name, but give it the complete tail interface.
pinvgaussian <- cdf_invgaussian

# asinh(exp(x)), without overflow when the argument is very large.
inverse_gaussian_log_asinh <- function(x) {
  out <- asinh(exp(pmin(x, 0)))
  big <- which(x > 0)
  out[big] <- x[big] + log1p(sqrt(1 + exp(-2 * x[big])))
  out
}

# Normal Mills ratio R(z) = P(Z > z) / dnorm(z), for large positive z.
# Its asymptotic series is used only for z >= 16, where terms reach machine
# precision well before the divergent part of the series.
inverse_gaussian_mills_series <- function(logz, log_ratio = NULL) {
  term <- total <- rep(1, length(logz))
  for (k in seq_len(40L)) {
    term <- term * (-(2 * k - 1)) * exp(-2 * logz)
    add <- term
    if (!is.null(log_ratio)) {
      geometric <- (-expm1((2 * k + 1) * log_ratio)) / (-expm1(log_ratio))
      geometric[log_ratio == 0] <- 2 * k + 1
      add <- add * geometric
    }
    total <- total + add
    if (all(abs(add) <= .Machine$double.eps * abs(total))) break
  }
  total
}

# Regular case in logs, also accepting log(q) outside the range of doubles.
inverse_gaussian_log_cdf <- function(logq, logmu, logphi, lower_tail = TRUE,
    log_ratio = logq - logmu) {
  t <- log_ratio
  logr <- (logq + logphi) / 2
  log_sum <- pmax(t, 0) + log1p(exp(-abs(t)))
  logu <- ifelse(t > 0, t + log(-expm1(-abs(t))),
    log(-expm1(-abs(t)))) - logr
  u <- sign(t) * exp(logu)
  logv <- log_sum - logr
  v <- exp(logv)
  log_normal_density <- -exp(2 * logu - log(2)) - 0.5 * log(2 * pi)

  # b = 2/(phi*mu) + log Phi(-v). Expressing it through the Mills
  # ratio avoids subtracting two huge numbers when dispersion is tiny.
  b <- log_normal_density + stats::pnorm(v, lower.tail = FALSE,
    log.p = TRUE) + 0.5 * v^2 + 0.5 * log(2 * pi)
  large_v <- which(logv >= log(16))
  b[large_v] <- log_normal_density[large_v] - logv[large_v] +
    log(inverse_gaussian_mills_series(logv[large_v]))
  lower <- logspace_add(stats::pnorm(u, log.p = TRUE), b)
  upper <- rep(NA_real_, length(t))
  small_lower <- which(lower <= -log(2))
  upper[small_lower] <- log1mexp(lower[small_lower])

  # When u is large, subtract the two Mills series term by term. The
  # leading difference is 2/(r*u*v), even when u and v round identically.
  large_u <- which(logu >= log(16) & t > 0 & lower > -log(2))
  log_ratio <- log1p(-exp(log(2) - log_sum[large_u]))
  upper[large_u] <- log_normal_density[large_u] + log(2) -
    logr[large_u] - logu[large_u] - logv[large_u] +
    log(inverse_gaussian_mills_series(logu[large_u], log_ratio))

  remaining <- which(lower > -log(2) & !(seq_along(t) %in% large_u))
  h <- exp(log(2) - logr[remaining])
  close <- h * (1 + abs(u[remaining])) < 0.1
  direct <- remaining[!close]
  upper[direct] <- stats::pnorm(u[direct], lower.tail = FALSE, log.p = TRUE) +
    log1mexp(b[direct] - stats::pnorm(u[direct], lower.tail = FALSE,
      log.p = TRUE))

  # For close normal arguments, use R(u)-R(u+h) directly. Derivatives obey
  # R' = u*R-1 and R^(k+1) = u*R^k + k*R^(k-1).
  idx <- remaining[close]
  z <- u[idx]
  h <- h[close]
  r0 <- exp(stats::pnorm(z, lower.tail = FALSE, log.p = TRUE) +
    0.5 * z^2 + 0.5 * log(2 * pi))
  r1 <- z * r0 - 1
  difference <- -r1
  coefficient <- rep(1, length(idx))
  for (k in 2:12) {
    r2 <- z * r1 + (k - 1) * r0
    coefficient <- coefficient * h / k
    difference <- difference - coefficient * r2
    r0 <- r1
    r1 <- r2
  }
  upper[idx] <- log_normal_density[idx] + log(2) - logr[idx] + log(difference)

  # Complement the smaller tail only, retaining accuracy near zero and one.
  large_lower <- which(lower > -log(2))
  lower[large_lower] <- log1mexp(upper[large_lower])
  if (lower_tail) lower else upper
}

qf_invgaussian <- function(p, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
  a <- inverse_gaussian_args(p, mu, wt, scale, log_p, lower_tail)
  logp <- quantile_log_probability(a$x, log_p)
  out <- a$x
  out[] <- NA_real_
  if (any(a$invalid)) {
    warning("NaNs produced")
    out[a$invalid] <- NaN
  }
  valid <- !a$missing & !a$invalid & !is.na(logp)
  out[which(!is.na(a$x) & is.nan(logp))] <- NaN
  left <- which(valid & if (lower_tail) logp == -Inf else logp == 0)
  right <- which(valid & if (lower_tail) logp == 0 else logp == -Inf)
  out[left] <- 0
  out[right] <- Inf
  interior <- valid & logp < 0 & logp > -Inf
  out[which(interior & a$phi == 0)] <- a$mu[which(interior & a$phi == 0)]
  out[which(interior & a$phi == Inf)] <- 0
  inverse_chisq <- which(interior & a$mu == Inf & a$phi > 0 & a$phi < Inf)
  out[inverse_chisq] <- exp(-log(a$phi[inverse_chisq]) -
    log(stats::qchisq(logp[inverse_chisq], df = 1,
      lower.tail = !lower_tail, log.p = TRUE)))
  regular <- which(interior & a$mu < Inf & a$phi > 0 & a$phi < Inf)
  if (length(regular)) {
    target <- logp[regular]
    lower <- rep(lower_tail, length(regular))
    flip <- which(target > -log(2))
    target[flip] <- log1mexp(target[flip])
    lower[flip] <- !lower[flip]
    # Solve the two tails in separate vector batches.
    for (tail in c(TRUE, FALSE)) {
      idx <- which(lower == tail)
      out[regular[idx]] <- inverse_gaussian_quantile(target[idx],
        a$mu[regular[idx]], log(a$phi[regular[idx]]), tail)
    }
  }
  out
}

inverse_gaussian_quantile <- function(target, mu, logphi, lower_tail) {
  if (!length(target)) return(numeric())
  logmu <- log(mu)
  logd <- logmu + logphi
  # Iterate on log(q/mu), preserving tiny deviations from the mean.
  mode <- -inverse_gaussian_log_asinh(log(1.5) + logd)
  x <- mode
  lo <- rep(-Inf, length(x))
  hi <- rep(Inf, length(x))
  extreme <- which(target < log(1e-5))
  z <- stats::qnorm(target[extreme], lower.tail = lower_tail, log.p = TRUE)
  # Solve (q/mu - 1)/sqrt(q*phi) = z stably. This gives an upper
  # bound on the required quantile in either tail, since the CDF is a sum
  # and survival is a difference of the two normal terms.
  normal_bound <- sign(z) * 2 *
    inverse_gaussian_log_asinh(log(abs(z)) + logd[extreme] / 2 - log(2))
  if (lower_tail) {
    x[extreme] <- pmin(mode[extreme], normal_bound)
  } else {
    lo[extreme] <- mode[extreme]
    hi[extreme] <- normal_bound
    x[extreme] <- normal_bound
  }
  active <- seq_along(x)
  tolerance <- 64 * .Machine$double.eps * pmax(1, abs(target))
  for (iteration in seq_len(200L)) {
    if (!length(active)) break
    j <- active
    actual <- inverse_gaussian_log_cdf(logmu[j] + x[j], logmu[j],
      logphi[j], lower_tail, x[j])
    delta <- target[j] - actual
    below <- if (lower_tail) actual < target[j] else actual > target[j]
    lo[j[below]] <- x[j[below]]
    hi[j[!below]] <- x[j[!below]]
    done <- abs(delta) <= tolerance[j]
    active <- j[!done]
    if (!length(active)) break
    j <- active
    actual <- actual[!done]
    delta <- delta[!done]
    t <- x[j]
    logq <- logmu[j] + t
    logu <- pmax(t, 0) + log(-expm1(-abs(t))) - (logq + logphi[j]) / 2
    log_density_q <- -0.5 * (log(2 * pi) + logphi[j] + logq) -
      exp(2 * logu - log(2))
    # (p-F)/[q*f(q)] without forming p or F on the ordinary scale.
    log_difference <- pmax(target[j], actual) + log(-expm1(-abs(delta)))
    step <- sign(delta) * exp(log_difference - log_density_q)
    if (!lower_tail) step <- -step
    proposal <- x[j] + log1p(pmax(step, -1))
    fallback <- !is.finite(proposal) | proposal <= lo[j] | proposal >= hi[j]
    # Newton on the log probability is useful when an ordinary-CDF step
    # from a tail bound is too large. Accept it only inside the same bracket.
    log_step <- delta * exp(actual - log_density_q)
    if (!lower_tail) log_step <- -log_step
    log_proposal <- x[j] + log_step
    use_log <- (fallback | (!lower_tail & target[j] < log(1e-5))) &
      is.finite(log_proposal) &
      log_proposal > lo[j] & log_proposal < hi[j]
    proposal[use_log] <- log_proposal[use_log]
    fallback[use_log] <- FALSE
    k <- which(fallback)
    if (length(k)) {
      proposal[k] <- ifelse(is.finite(lo[j[k]]) & is.finite(hi[j[k]]),
        (lo[j[k]] + hi[j[k]]) / 2,
        ifelse(is.finite(lo[j[k]]), lo[j[k]] + log(2), hi[j[k]] - log(2)))
    }
    # A Newton correction smaller than floating point spacing cannot
    # improve x. Detect this before a rejected rounded step triggers bisection.
    rounded <- is.finite(log_step) &
      abs(log_step) <= 8 * .Machine$double.eps * abs(x[j])
    proposal[rounded] <- x[j[rounded]]
    stalled <- abs(proposal - x[j]) <= 8 * .Machine$double.eps * abs(x[j])
    x[j] <- proposal
    active <- j[!stalled]
  }
  if (length(active)) {
    warning("Inverse Gaussian quantile iteration did not converge")
    x[active] <- NA_real_
  }
  out <- exp(logmu + x)
  near_mean <- which(abs(x) < log(2))
  out[near_mean] <- mu[near_mean] * exp(x[near_mean])
  out
}
