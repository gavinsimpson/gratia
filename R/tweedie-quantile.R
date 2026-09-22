# Experimental internal quantiles for the compound Poisson-Gamma Tweedie.
# For Y | N = k ~ Gamma(k * alpha, beta), sum Gamma probabilities weighted
# by Poisson(lambda) probabilities. Truncation is bounded by omitted Poisson
# mass, independently of the evaluation point. No renormalisation is applied.
#
# `cdf_tol` bounds absolute truncation error; `probability_tol` controls relative
# error in the smaller of P(0 < Y <= y) and P(Y > y). `quantile_tol` is the
# root tolerance on log(y / mu), approximately a relative quantile tolerance.
# These are distinct error controls, not a certified quantile-error bound.
# `fallback = TRUE` opts into expensive upstream inversion for unresolved
# values. By default return the available approximation (or NA) and warn.
# `details` exposes numerical diagnostics for evaluation, not a public API.
qtweedie_mixture <- function(prob, mu, power, phi, log_p = FALSE,
    cdf_tol = 1e-12, probability_tol = 1e-8, quantile_tol = 1e-10,
    max_terms = 10000L, details = FALSE, fallback = FALSE) {
  if (!is.numeric(power) || length(power) != 1L || !is.finite(power) ||
      power <= 1 || power >= 2) {
    stop("'power' must be a finite scalar in (1, 2).")
  }
  pars <- list(prob = prob, mu = mu, phi = phi)
  lens <- lengths(pars)
  size <- max(lens)
  if (any(!vapply(pars, is.numeric, logical(1)))) {
    stop("'prob', 'mu', and 'phi' must be numeric vectors.")
  }
  if (size > 0L && (any(lens == 0L) || any(!lens %in% c(1L, size)))) {
    stop("Parameters must have length 1 or a common nonzero length.")
  }
  for (tol in list(cdf_tol, probability_tol, quantile_tol)) {
    if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) ||
        tol <= 0 || tol >= 1) stop("Tolerances must be scalars in (0, 1).")
  }
  if (length(max_terms) != 1L || !is.numeric(max_terms) ||
      !is.finite(max_terms) || max_terms < 1 || max_terms != floor(max_terms)) {
    stop("'max_terms' must be a positive integer.")
  }
  if (!is.logical(log_p) || length(log_p) != 1L || is.na(log_p) ||
      !is.logical(details) || length(details) != 1L || is.na(details)) {
    stop("'log_p' and 'details' must be TRUE or FALSE.")
  }
  if (!is.logical(fallback) || length(fallback) != 1L || is.na(fallback)) {
    stop("'fallback' must be TRUE or FALSE.")
  }
  prob <- rep_len(prob, size)
  mu <- rep_len(mu, size)
  phi <- rep_len(phi, size)
  if (any(!is.na(mu) & (!is.finite(mu) | mu < 0)) ||
      any(!is.na(phi) & (!is.finite(phi) | phi <= 0))) {
    stop("Nonmissing means must be finite and nonnegative; scales finite and positive.")
  }
  logu <- quantile_log_probability(prob, log_p)
  out <- rep(NA_real_, size)
  method <- rep("missing", size)
  reason <- rep(NA_character_, size)
  terms <- integer(size)
  log_omitted <- log_error <- log_quantile <- rep(NA_real_, size)
  valid <- !is.na(logu) & !is.na(mu) & !is.na(phi)
  zero <- valid & (mu == 0 | logu == -Inf)
  out[zero] <- 0
  method[zero] <- "boundary"
  log_quantile[zero] <- -Inf
  one <- valid & mu > 0 & logu == 0
  out[one] <- Inf
  method[one] <- "boundary"
  log_quantile[one] <- Inf
  alpha <- (2 - power) / (power - 1)
  for (i in which(valid & !zero & !one)) {
    lambda <- exp((2 - power) * log(mu[i]) - log(phi[i]) - log(2 - power))
    if (is.finite(lambda) && logu[i] <= -lambda) {
      out[i] <- 0
      method[i] <- "atom"
      log_quantile[i] <- -Inf
      next
    }
    result <- tweedie_mixture_quantile_one(logu[i], lambda, alpha,
      cdf_tol, probability_tol, quantile_tol, max_terms, allow_partial = !fallback)
    terms[i] <- result$terms
    log_omitted[i] <- result$log_omitted
    log_error[i] <- result$log_error
    if (is.null(result$reason)) {
      log_quantile[i] <- log(mu[i]) + result$root
      out[i] <- exp(log_quantile[i])
      method[i] <- if (out[i] == 0) "underflow" else "mixture"
    } else {
      reason[i] <- result$reason
      if (fallback) {
        out[i] <- tweedie_quantile_fallback(logu[i], mu[i], power, phi[i])
        method[i] <- "fallback"
        log_quantile[i] <- log(out[i])
      } else if (!is.null(result$root) && is.finite(result$root)) {
        log_quantile[i] <- log(mu[i]) + result$root
        out[i] <- exp(log_quantile[i])
        method[i] <- "approximate"
      } else {
        method[i] <- "unresolved"
      }
    }
  }
  if (!fallback && any(!is.na(reason))) {
    warning(sum(!is.na(reason)), " Tweedie quantile(s) exceeded the mixture work/accuracy ",
      "limits; returning available approximations (NA where no estimate exists). ",
      "Accuracy is not guaranteed. Increase 'max_terms' or set fallback = TRUE ",
      "to permit expensive inversion.", call. = FALSE)
  }
  if (details) {
    return(data.frame(quantile = out, log_quantile, method, terms,
      log_omitted_mass = log_omitted, log_tail_error = log_error, reason))
  }
  out
}

# Work in log(y / mu). This keeps root accuracy meaningful for tiny means and
# positive quantiles immediately above the atom, and respects Tweedie scaling.
tweedie_mixture_quantile_one <- function(logu, lambda, alpha,
    cdf_tol, probability_tol, quantile_tol, max_terms, allow_partial = TRUE) {
  result <- list(terms = 0L, log_omitted = NA_real_, log_error = NA_real_)
  fail <- function(reason) {
    result$reason <- reason
    result
  }
  gamma_scale <- 1 / (lambda * alpha)
  if (!is.finite(lambda) || lambda <= 0 || !is.finite(gamma_scale) ||
      gamma_scale <= 0) return(fail("unrepresentable mixture parameters"))

  # Subtract the atom stably; do not subtract two nearly equal probabilities.
  log_positive <- logu + log(-expm1(-lambda - logu))
  log_survival <- log1mexp(logu)
  lower <- log_positive <= log_survival
  target <- if (lower) log_positive else log_survival
  log_eps <- min(log(cdf_tol), target + log(probability_tol) - log(8))
  prepared <- tweedie_mixture_prepare(lambda, alpha, log_eps, max_terms,
    allow_partial = allow_partial)
  result$terms <- prepared$terms
  result$log_omitted <- prepared$log_omitted
  if (is.null(prepared$evaluate)) return(fail(prepared$reason))
  result$reason <- prepared$reason
  # A partial sum need not contain enough probability to bracket this target.
  # Do not manufacture a quantile by renormalising its retained weights.
  if (target >= prepared$log_mass) {
    return(fail("target exceeds retained mixture mass"))
  }
  objective <- function(z) prepared$evaluate(z, lower) - target
  # Choose a bracket around the mean, doubling its log-width as needed.
  at_mean <- objective(0)
  if (is.na(at_mean)) return(fail("nonfinite mixture evaluation"))
  left <- right <- 0
  fleft <- fright <- at_mean
  step <- 1
  for (iteration in seq_len(60L)) {
    if (lower) {
      if (fleft > 0) { left <- -step; fleft <- objective(left) }
      if (fright < 0) { right <- step; fright <- objective(right) }
    } else {
      if (fleft < 0) { left <- -step; fleft <- objective(left) }
      if (fright > 0) { right <- step; fright <- objective(right) }
    }
    if (is.na(fleft) || is.na(fright)) return(fail("nonfinite mixture evaluation"))
    if (fleft == 0 || fright == 0 || sign(fleft) != sign(fright)) break
    step <- step * 2
  }
  if (sign(fleft) == sign(fright) && fleft != 0) return(fail("bracketing failed"))
  if (at_mean == 0) {
    root <- 0
  } else {
    root <- tryCatch(stats::uniroot(objective, c(left, right),
      f.lower = fleft, f.upper = fright, tol = quantile_tol,
      maxiter = 200L, check.conv = TRUE)$root, error = function(e) NA_real_)
  }
  if (is.na(root)) return(fail("root solver failed"))
  result$root <- root
  result$log_error <- abs(objective(root))
  if (is.finite(result$log_error) && result$log_error > probability_tol) {
    # A log-quantile tolerance alone can be too loose in a steep tail. Refine
    # once with the same cached mixture instead of paying for inversion.
    froot <- objective(root)
    if (sign(froot) == sign(fleft)) {
      left <- root; fleft <- froot
    } else {
      right <- root; fright <- froot
    }
    root <- tryCatch(stats::uniroot(objective, c(left, right),
      f.lower = fleft, f.upper = fright, tol = min(quantile_tol / 1000, 1e-12),
      maxiter = 200L, check.conv = TRUE)$root, error = function(e) NA_real_)
    if (!is.na(root)) result$root <- root
    result$log_error <- if (is.na(root)) Inf else abs(objective(root))
  }
  if (!is.finite(result$log_error) || result$log_error > probability_tol) {
    return(fail("tail probability residual exceeds tolerance"))
  }
  result$root <- root
  result
}

# Keep fallback separate so tests can verify dispatch without running a costly
# inversion. Never round an interior log probability to an endpoint silently.
tweedie_quantile_fallback <- function(logu, mu, power, phi) {
  u <- exp(logu)
  if (u == 0 || u == 1) {
    warning("Tweedie fallback cannot represent this interior log probability.")
    return(NA_real_)
  }
  tryCatch(tweedie::qtweedie(u, mu = mu, xi = power, phi = phi),
    error = function(e) {
      warning("Tweedie inversion fallback failed: ", conditionMessage(e))
      NA_real_
    })
}

# Precompute the truncated mixture; the evaluator returns log probabilities
# for the positive component (lower tail) or the full survival probability.
tweedie_mixture_prepare <- function(lambda, alpha, log_eps, max_terms,
    allow_partial = TRUE) {
  result <- list(terms = 0L, log_omitted = NA_real_, log_error = NA_real_)
  fail <- function(reason) { result$reason <- reason; result }
  gamma_scale <- 1 / (lambda * alpha)
  if (!is.finite(lambda) || lambda <= 0 || !is.finite(gamma_scale) ||
      gamma_scale <= 0) return(fail("unrepresentable mixture parameters"))
  lo <- max(1, stats::qpois(log_eps - log(2), lambda, log.p = TRUE))
  hi <- stats::qpois(log_eps - log(2), lambda, lower.tail = FALSE, log.p = TRUE)
  n_terms <- hi - lo + 1
  if (!is.finite(n_terms) || n_terms < 1) {
    return(fail("unrepresentable Poisson summation range"))
  }
  if (n_terms > max_terms) {
    if (!allow_partial) return(fail("Poisson summation exceeds work limit"))
    # Retain a contiguous window around the Poisson mode, inside the desired
    # range. Starting at count one would discard almost everything at high rates.
    lo <- max(lo, min(floor(lambda) - floor((max_terms - 1) / 2),
      hi - max_terms + 1))
    hi <- lo + max_terms - 1
    n_terms <- hi - lo + 1
    if (hi > 2^53 - 1 || n_terms != max_terms) {
      return(fail("unrepresentable Poisson summation range"))
    }
    result$reason <- "Poisson summation exceeds work limit"
  }
  result$terms <- n_terms
  # Positive counts below lo, plus counts above hi; using the full lower tail
  # here is a conservative bound (the atom is handled separately).
  omitted_lower <- if (lo > 1) stats::ppois(lo - 1, lambda, log.p = TRUE) else -Inf
  omitted_upper <- stats::ppois(hi, lambda, lower.tail = FALSE, log.p = TRUE)
  logsum <- function(x) {
    m <- max(x)
    if (!is.finite(m)) return(m)
    m + log(sum(exp(x - m)))
  }
  result$log_omitted <- logsum(c(omitted_lower, omitted_upper))
  if (result$log_omitted > log_eps && is.null(result$reason)) {
    if (!allow_partial) return(fail("Poisson truncation bound exceeded"))
    result$reason <- "Poisson truncation bound exceeded"
  }
  counts <- seq.int(lo, hi)
  log_weights <- stats::dpois(counts, lambda, log = TRUE)
  result$log_mass <- logsum(log_weights)
  shapes <- counts * alpha
  if (any(!is.finite(shapes))) return(fail("unrepresentable Gamma shapes"))
  evaluate <- function(z, lower = TRUE) {
    log_x <- z - log(gamma_scale)
    # For x below the normal floating-point range, the leading term of the
    # incomplete-Gamma expansion has relative error O(x). Keeping log(x)
    # avoids underflow for positive quantiles arbitrarily close to the atom.
    if (log_x < log(.Machine$double.xmin)) {
      log_gamma <- shapes * log_x - lgamma(shapes + 1)
      if (!lower) log_gamma <- log1mexp(log_gamma)
    } else {
      log_gamma <- stats::pgamma(exp(log_x), shape = shapes,
        lower.tail = lower, log.p = TRUE)
    }
    logsum(log_weights + log_gamma)
  }
  result$evaluate <- evaluate
  result
}

# Experimental CDF companion for evaluating PIT/normal-score residual accuracy.
# Refine the Poisson truncation using the smaller evaluated tail, so absolute
# CDF accuracy does not conceal poor relative accuracy in extreme tails.
ptweedie_mixture <- function(q, mu, power, phi, lower_tail = TRUE, log_p = FALSE,
    cdf_tol = 1e-12, probability_tol = 1e-8, max_terms = 10000L,
    fallback = FALSE) {
  # Reuse the quantile input checks without computing any positive quantiles.
  size <- max(length(q), length(mu), length(phi))
  if (!is.numeric(q) || (size > 0 &&
      (length(q) == 0 || !length(q) %in% c(1L, size)))) {
    stop("'q' must be numeric with length 1 or a common nonzero length.")
  }
  if (!is.logical(lower_tail) || length(lower_tail) != 1L || is.na(lower_tail)) {
    stop("'lower_tail' must be TRUE or FALSE.")
  }
  invisible(qtweedie_mixture(rep(0, size), mu, power, phi, log_p = log_p,
    cdf_tol = cdf_tol, probability_tol = probability_tol, max_terms = max_terms,
    fallback = fallback))
  q <- rep_len(q, size); mu <- rep_len(mu, size); phi <- rep_len(phi, size)
  lower <- upper <- rep(NA_real_, size)
  unresolved <- logical(size)
  valid <- !is.na(q) & !is.na(mu) & !is.na(phi)
  below <- valid & q < 0
  above <- valid & (q == Inf | (mu == 0 & q >= 0))
  lower[below] <- -Inf; upper[below] <- 0
  lower[above] <- 0; upper[above] <- -Inf
  alpha <- (2 - power) / (power - 1)
  for (i in which(valid & !below & !above)) {
    lambda <- exp((2 - power) * log(mu[i]) - log(phi[i]) - log(2 - power))
    if (q[i] == 0) {
      lower[i] <- -lambda
      upper[i] <- log1mexp(-lambda)
      next
    }
    log_eps <- log(cdf_tol)
    resolved <- FALSE
    for (iteration in seq_len(5L)) {
      prepared <- tweedie_mixture_prepare(lambda, alpha, log_eps, max_terms,
        allow_partial = !fallback)
      if (is.null(prepared$evaluate)) break
      z <- log(q[i]) - log(mu[i])
      lp <- prepared$evaluate(z)
      m <- max(-lambda, lp)
      lf <- m + log(exp(-lambda - m) + exp(lp - m))
      ls <- prepared$evaluate(z, FALSE)
      # Preserve the computed sums even if a tighter budget cannot be met.
      # For partial sums do not fill missing mass by complementing the other tail.
      if (!is.na(lf) && !is.na(ls)) {
        lower[i] <- lf; upper[i] <- ls
      }
      if (!is.null(prepared$reason)) break
      tail <- min(lf, ls)
      if (!is.finite(tail)) break
      required <- min(log(cdf_tol), tail + log(probability_tol) - log(8))
      if (prepared$log_omitted <= required) {
        if (lf <= ls) {
          lower[i] <- lf; upper[i] <- log1mexp(lf)
        } else {
          upper[i] <- ls; lower[i] <- log1mexp(ls)
        }
        resolved <- TRUE
        break
      }
      log_eps <- required
    }
    if (!resolved) {
      unresolved[i] <- TRUE
      if (fallback) {
        f <- tweedie_cdf_fallback(q[i], mu[i], power, phi[i])
        lower[i] <- log(f); upper[i] <- log1p(-f)
      }
    }
  }
  if (any(unresolved)) {
    if (fallback) {
      warning("Mixture CDF work/accuracy limits reached; using the requested ",
        "ordinary-CDF fallback, which has limited log-tail accuracy.", call. = FALSE)
    } else {
      warning(sum(unresolved), " Tweedie CDF value(s) exceeded the mixture work/accuracy ",
        "limits; returning available partial sums (NA where no estimate exists). ",
        "Accuracy is not guaranteed. Increase 'max_terms' or set fallback = TRUE ",
        "to permit expensive inversion.", call. = FALSE)
    }
  }
  out <- if (lower_tail) lower else upper
  if (log_p) out else exp(out)
}

# Separate the expensive backend so opt-in dispatch can be tested directly.
tweedie_cdf_fallback <- function(q, mu, power, phi) {
  tweedie::ptweedie(q, mu = mu, xi = power, phi = phi)
}
