#' @importFrom mgcv fix.family.rd
`fix_family_rd` <- function(family, ncores = 1, ...) {
  family <- normalize_native_nb_parameters(family)
  # try to fix up the family used by mgcv to add the $rd component
  # for random deviate sampling

  # Add the inverse Gaussian sampler that uses the supplied weights before
  # mgcv adds one that ignores them. Keep a caller's existing rd function.
  if (inherits(family, "family") && is.null(family$rd) &&
      identical(family_type(family), "inverse_gaussian")) {
    family$rd <- rd_invgaussian
    return(family)
  }

  if (is.null(family$rd) && identical(family_type(family), "gamma")) {
    family$rd <- rd_gamma
    return(family)
  }
  if (is.null(family$rd) && identical(family_type(family), "binomial")) {
    family$rd <- rd_binomial
    return(family)
  }

  if (is.null(family$rd) && identical(family_type(family), "negative_binomial") &&
      has_theta(family)) {
    size <- theta(family)
    family$rd <- function(mu, wt, scale) stats::rnbinom(length(mu), mu = mu, size = size)
    return(family)
  }

  # try the obvious thing first and see if mgcv::fix.family.rd() already handles
  # family
  fam <- mgcv::fix.family.rd(family)

  # if `family` contains a NULL rd we move on, if it is non-null return early
  # as it doesn't need fixing
  if (!is.null(fam$rd)) {
    return(fam)
  }

  # handle special cases
  fn <- family_name(fam)

  # handle multivariate normal
  if (identical(fn, "Multivariate normal")) {
    # note: mgcv::mvn is documented to ignore prior weights
    # if we ever need to handle weights to scale V, see this post on CV
    # https://stats.stackexchange.com/a/162885/1390
    rd_mvn <- function(V) {
      function(mu, wt, scale) { # function needs to take wt and scale
        mgcv::rmvn(
          n = NROW(mu),
          mu = mu,
          V = V
        )
      }
    }
    fam$rd <- rd_mvn(solve(crossprod(fam$data$R)))
  }
  if (identical(fn, "twlss")) {
    # this uses some helpers to find the `a` and `b` used during fitting and
    # also to convert what `predict()` etc returns (theta) to power parameter
    rd_twlss <- function(a, b) {
      function(mu, wt, scale) {
        rtw(
          mu = mu[, 1], # fitted(model) for twlss is on response scale!
          p = theta_2_power(theta = mu[, 2], a, b),
          phi = exp(mu[, 3])
        )
      }
    }
    tw_pars <- get_tw_params(fam)
    fam$rd <- rd_twlss(a = tw_pars[1], b = tw_pars[2])
  }

  # If still NULL, handle censored families using the latent distribution.
  if (is.null(fam$rd)) {
    ft <- family_type(fam)

    rd_fun <- switch(
      EXPR = ft,
      "cnorm" = make_rd_cnorm(theta(fam)),
      "cpois" = rd_poisson,
      "clog"  = make_rd_clog(theta(fam)),
      NULL
    )

    # add the RD fun to the family
    fam$rd <- rd_fun
  }

  # return possibly modified family
  fam
}

#' @importFrom stats rnorm
rd_gaussian <- function(mu, wt, scale) {
  rnorm(length(mu), mean = mu, sd = sqrt(scale / wt))
}

#' @importFrom stats rpois
rd_poisson <- function(mu, wt, scale) {
  rpois(length(mu), lambda = mu)
}

#' @importFrom stats rlogis
rd_logistic <- function(mu, wt, scale) {
  rlogis(length(mu), location = mu, scale = scale / sqrt(wt))
}

# These families store their error scale in theta. Using model dispersion here
# would change the simulated distribution. The functions describe responses
# before censoring; censoring information supplies the observed CDF bounds.
make_rd_cnorm <- function(sigma) {
  force(sigma)
  function(mu, wt, scale) {
    rd_gaussian(mu, wt, scale = sigma^2)
  }
}

make_rd_clog <- function(sigma) {
  force(sigma)
  function(mu, wt, scale) {
    rd_logistic(mu, wt, scale = sigma)
  }
}

`fix_family_cdf` <- function(family) {
  family <- normalize_native_nb_parameters(family)
  # if `family` contains a NULL cdf we move on, if it is non-null return early
  # as it doesn't need fixing
  if (!is.null(family$cdf)) {
    return(family)
  }

  # handle special cases
  ft <- family_type(family)
  theta <- NULL
  if (has_theta(family)) {
    transf <- TRUE
    if (ft %in% c("tweedie")) { # which don't need transform?
      transf <- FALSE
    }
    theta <- theta(family, transform = transf)
  }

  # choose a CDF functions
  cdf_fun <- switch(
    EXPR = ft,
    "poisson"  = cdf_poisson,
    "cpois"    = cdf_poisson,
    "cnorm"    = make_cdf_cnorm(theta),
    "clog"     = make_cdf_clog(theta),
    "gaussian" = cdf_gaussian,
    "multivariate_normal" = make_cdf_mvn(family),
    "gaulss"   = cdf_gaulss,
    "gevlss"   = cdf_gevlss,
    "gumbls"   = cdf_gumbls,
    "gammals"  = cdf_gammals,
    "binomial" = cdf_binomial,
    "multinom" = make_cdf_multinom(family$nlp),
    "gamma"    = cdf_gamma,
    "ziplss"   = cdf_ziplss,
    "twlss"    = make_cdf_twlss(family),
    "scaled_t" = make_cdf_scat(nu = theta[1], sigma = theta[2]),
    "negative_binomial" = make_cdf_nb(theta),
    "inverse_gaussian"  = cdf_invgaussian,
    "beta_regression"   = make_cdf_beta(theta, eps = betar_eps(family)),
    "tweedie"  = make_cdf_tw(theta, ab = get_tw_params(family)),
    NULL
  )

  if (grepl("^ordered_categorical", ft))
    cdf_fun <- make_cdf_ocat(family$getTheta(TRUE))
  if (grepl("^zero_inflated_poisson", ft))
    cdf_fun <- make_cdf_zip(family$getTheta(TRUE))

  # add the CDF fun to the family
  family$cdf <- cdf_fun

  # return
  family
}

#' @importFrom stats ppois
`cdf_poisson` <- function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
  ppois(q, lambda = mu, lower.tail = lower_tail, log.p = log_p)
}

#' @importFrom stats pnorm
`cdf_gaussian` <- function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
  pnorm(q, mean = mu, sd = sqrt(scale / wt),
    lower.tail = lower_tail, log.p = log_p)
}

# MVN probabilities describe each response separately. Its fitted covariance
# supplies the marginal variances; mgcv's mvn family ignores prior weights.
make_cdf_mvn <- function(family) {
  sigma <- sqrt(diag(solve(crossprod(family$data$R))))
  function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    stats::pnorm(q, mean = mu, sd = rep(sigma, each = NROW(mu)),
      lower.tail = lower_tail, log.p = log_p)
  }
}

make_qf_mvn <- function(family) {
  sigma <- sqrt(diag(solve(crossprod(family$data$R))))
  function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    matrix(stats::qnorm(p, mean = mu, sd = rep(sigma, each = NROW(mu)),
      lower.tail = lower_tail, log.p = log_p), nrow = NROW(mu))
  }
}

make_cdf_cnorm <- function(sigma) {
  force(sigma)
  function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
    cdf_gaussian(q, mu, wt, scale = sigma^2, log_p = log_p,
      lower_tail = lower_tail)
  }
}

#' @importFrom stats plogis
make_cdf_clog <- function(sigma) {
  force(sigma)
  function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
    plogis(q, location = mu, scale = sigma / sqrt(wt),
      lower.tail = lower_tail, log.p = log_p)
  }
}

#' @importFrom stats pbinom
`cdf_binomial` <- function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
  pbinom(
    q * (wt + as.numeric(wt == 0)), size = wt, prob = mu,
    lower.tail = lower_tail, log.p = log_p
  )
  #pbinom(floor(wt * q), wt, mu)
}

#' @importFrom stats pgamma
`cdf_gamma` <- function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
  pgamma(q, shape = wt / scale, scale = mu * scale / wt,
    lower.tail = lower_tail, log.p = log_p)
}

#' @importFrom stats pnorm
`cdf_gaulss` <- function(q, mu, wt = 1, scale = 1, log_p = FALSE,
    lower_tail = TRUE) {
  pnorm(
    q, mean = mu[, 1, drop = TRUE],
    sd = sqrt(scale / wt) / mu[, 2, drop = TRUE],
    lower.tail = lower_tail, log.p = log_p
  )
}

`cdf_gumbls` <- function(q, mu, wt, scale, log_p = FALSE,
                           lower_tail = TRUE) {
  log_hazard <- -0.577215664901533 - (q - mu[, 1L]) / exp(mu[, 2L])
  extreme_value_probability(log_hazard, log_p, lower_tail)
}

#' @importFrom stats pgamma
`cdf_gammals` <- function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
  # mu[,1] is the mean, mu[,2] is log scale (dispersion)
  # exp(mu[,2]) == phi
  phi <- exp(mu[, 2])
  pgamma(q, shape = 1 / phi, scale = mu[, 1] * phi,
    lower.tail = lower_tail, log.p = log_p)
}

`cdf_gevlss` <- function(q, mu, wt, scale, log_p = FALSE, tol = 1e-6,
                           lower_tail = TRUE) {
  n <- max(length(q), nrow(mu))
  z <- (rep_len(q, n) - rep_len(mu[, 1], n)) / exp(rep_len(mu[, 2], n))
  xi <- rep_len(mu[, 3], n)
  logt <- -z
  nonzero <- which(!is.na(xi) & xi != 0)
  support <- 1 + xi * z
  inside <- nonzero[which(support[nonzero] > 0)]
  logt[inside] <- -log1p(xi[inside] * z[inside]) / xi[inside]
  logt[which(support <= 0 & xi > 0)] <- Inf
  logt[which(support <= 0 & xi < 0)] <- -Inf
  logt[is.na(xi)] <- NA_real_
  extreme_value_probability(logt, log_p, lower_tail)
}

# Give zero its own probability. Positive responses follow a Poisson
# distribution conditioned on Y > 0.
#' @importFrom stats ppois
`cdf_ziplss` <- function(q, mu, wt, scale, log_p = FALSE,
                           lower_tail = TRUE) {
  lambda <- exp(mu[, 1L])
  base <- distribution_from_parameters("poisson", lambda)
  positive <- truncate_distribution(base, mean = lambda / (-expm1(-lambda)))
  zero_inflated_distribution(positive, exp(-exp(mu[, 2L])))$cdf(q,
    log_p = log_p, lower_tail = lower_tail)
}

# Gumbel and GEV have F = exp(-exp(log_hazard)). exp(log_hazard) can round to
# zero, so calculate log P(Y > q) directly to retain very small probabilities.
extreme_value_probability <- function(log_hazard, log_p = FALSE,
                                      lower_tail = TRUE) {
  out <- -exp(log_hazard)
  if (!lower_tail) {
    out <- log1mexp(out)
    tiny <- which(log_hazard < -36)
    out[tiny] <- log_hazard[tiny]
  }
  if (log_p) out else exp(out)
}

## For distributions that estimate other parameters, we need to bind the values
## of those parameters inside the CDF function in the family. `theta` are then
## passed to the `make_cdf_xxx()` function as separate parameters with real
## names so it's easy to follow what is being done

#' @importFrom stats pt
`make_cdf_scat` <- function(nu, sigma) {
  function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
    pt(
      (q - mu) / sigma, # mgcv estimates sigma separately from dispersion
      df = nu,
      lower.tail = lower_tail, log.p = log_p
    )
  }
}

#' @importFrom stats pnbinom
`make_cdf_nb` <- function(theta) {
  function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
    pnbinom(
      q,
      size = theta,
      mu = mu,
      lower.tail = lower_tail, log.p = log_p
    )
  }
}

#' @importFrom stats pbeta
`make_cdf_beta` <- function(phi, eps) {
  cdf <- function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
    p <- pbeta(
      q,
      shape1 = phi * mu,
      shape2 = phi * (1 - mu),
      lower.tail = lower_tail, log.p = log_p
    )
    p
  }
  attr(cdf, "gratia_beta_cdf") <- TRUE
  cdf
}

#' @importFrom stats pgamma
`make_cdf_tw` <- function(theta, ab) {
  xi <- if (length(ab) == 1L) ab else theta_2_power(theta, a = ab[1], b = ab[2])
  force(xi)
  function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE,
      fallback = FALSE, max_terms = 10000L) {
    if (xi == 2) {
      return(pgamma(q, shape = 1 / scale, scale = mu * scale,
        lower.tail = lower_tail, log.p = log_p))
    }
    ptweedie_mixture(q, mu = mu, power = xi, phi = scale,
      lower_tail = lower_tail, log_p = log_p, fallback = fallback,
      max_terms = max_terms)
  }
}

# Inverse CDFs support QQ references and sampling after response transformations.
#' @importFrom mgcv fix.family.qf
`fix_family_qf` <- function(family) {
  family <- normalize_native_nb_parameters(family)

  # Keep a caller's inverse CDF before adding one that uses Gamma weights.
  if (!is.null(family$qf)) return(family)
  if (identical(family_type(family), "gamma")) {
    family$qf <- qf_gamma
    return(family)
  }
  if (identical(family_type(family), "binomial")) {
    family$qf <- qf_binomial
    return(family)
  }
  if (identical(family_type(family), "negative_binomial") && has_theta(family)) {
    size <- theta(family)
    family$qf <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE)
      stats::qnbinom(p, mu = mu, size = size, log.p = log_p, lower.tail = lower_tail)
    return(family)
  }
  # try obvious thing first and see if mgcv::fix.family.qf() already handles
  # family
  family <- mgcv::fix.family.qf(family)

  # if `family` contains a NULL qf we move on, if it is non-null return early
  # as it doesn't need fixing
  if (!is.null(family$qf)) {
    return(family)
  }

  # handle special cases - mgcv uses theta as a catchall vector of extra params
  # for some families. If a family has one or more of these they need to be
  # embedded in the qf function.
  # Here we grab theta and decide if it needs transforming
  ft <- family_type(family)
  theta <- NULL
  if (has_theta(family)) {
    transf <- TRUE
    if (ft %in% c("tweedie")) { # which don't need transform?
      transf <- FALSE
    }
    theta <- theta(family, transform = transf)
  }

  # choose QF functions - this is where we explicitly handle the special
  # families; they need a make_qf_foo function, while normal families
  # with no extra parameters just need a qf_foo function
  qfun <- switch(
    EXPR = ft,
    "cnorm"    = make_qf_cnorm(theta),
    "clog"     = make_qf_clog(theta),
    "cpois"    = qf_poisson,
    "multivariate_normal" = make_qf_mvn(family),
    "inverse_gaussian" = qf_invgaussian,
    "scaled_t" = make_qf_scat(nu = theta[1], sigma = theta[2]),
    "tweedie"  = make_qf_tw(theta, ab = get_tw_params(family)),
    #"nb"       = make_qf_betar(phi = theta),
    #"betar"    = make_qf_betar(theta),
    "gaulss"   = qf_gaulss,
    "gevlss"   = qf_gevlss,
    "gumbls"   = qf_gumbls,
    "gammals"  = qf_gammals,
    "ziplss"   = qf_ziplss,
    "multinom" = make_qf_multinom(family$nlp),
    "twlss"    = make_qf_twlss(family),
    NULL # if don't handle family, return NULL as qfun so family unchanged
  )

  if (grepl("^ordered_categorical", ft))
    qfun <- make_qf_ocat(family$getTheta(TRUE))
  if (grepl("^zero_inflated_poisson", ft))
    qfun <- make_qf_zip(family$getTheta(TRUE))

  # add the QF fun to the family
  family$qf <- qfun

  # return
  family
}

#' @importFrom stats qnorm
make_qf_cnorm <- function(sigma) {
  force(sigma)
  function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    qnorm(p, mean = mu, sd = sigma / sqrt(wt), log.p = log_p, lower.tail = lower_tail)
  }
}

#' @importFrom stats qlogis
make_qf_clog <- function(sigma) {
  force(sigma)
  function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    qlogis(p, location = mu, scale = sigma / sqrt(wt), log.p = log_p, lower.tail = lower_tail)
  }
}

#' @importFrom stats qpois
qf_poisson <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
  qpois(p, lambda = mu, log.p = log_p, lower.tail = lower_tail)
}

#' @importFrom stats qnorm
`qf_gaulss` <- function(p, mu, wt = 1, scale = 1, log_p = FALSE, lower_tail = TRUE) {
  qnorm(
    p,
    mean = mu[, 1, drop = TRUE],
    sd = sqrt(scale / wt) / mu[, 2, drop = TRUE],
    log.p = log_p, lower.tail = lower_tail
  )
}

`qf_gevlss` <- function(p, mu, wt, scale, log_p = FALSE, tol = 1e-6,
                          lower_tail = TRUE) {
  logt <- extreme_value_quantile_log_hazard(p, log_p, lower_tail)
  n <- max(length(logt), nrow(mu))
  logt <- rep_len(logt, n)
  location <- rep_len(mu[, 1], n)
  sigma <- exp(rep_len(mu[, 2], n))
  xi <- rep_len(mu[, 3], n)

  # expm1 retains precision when shape is small. Calculate the exact Gumbel
  # limit at shape 0, including the requested probability endpoints.
  z <- -logt
  nonzero <- which(!is.na(xi) & xi != 0)
  z[nonzero] <- expm1(-xi[nonzero] * logt[nonzero]) / xi[nonzero]
  z[is.na(xi)] <- NA_real_
  location + sigma * z
}

# mgcv uses a mean-centred parameterisation
qf_gumbls <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
  gamma <- 0.577215664901533 # euler's constant
  # mu[, 1] is mean, mu[, 2] is log(beta)
  mu[, 1] - exp(mu[, 2]) *
    (gamma + extreme_value_quantile_log_hazard(p, log_p, lower_tail))
}

#' @importFrom stats qgamma
`qf_gammals` <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
  # mu[,1] is the mean, mu[,2] is log scale (dispersion)
  # exp(mu[,2]) == phi
  phi <- exp(mu[, 2])
  qgamma(p, shape = 1 / phi, scale = mu[, 1] * phi, log.p = log_p,
    lower.tail = lower_tail)
}

#' @importFrom stats qpois
`qf_ziplss` <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
  logp <- quantile_log_probability(p, log_p)
  n <- max(length(logp), nrow(mu))
  logp <- rep_len(logp, n)
  lambda <- exp(rep_len(mu[, 1], n))
  log_zero <- -exp(rep_len(mu[, 2], n))
  log_presence <- log1mexp(log_zero)
  q <- rep(NA_real_, n)
  zero <- if (lower_tail) logp <= log_zero else logp >= log_presence
  q[which(zero)] <- 0
  positive <- which(!zero)
  if (length(positive)) {
    log_survival <- if (lower_tail) log1mexp(logp[positive]) else logp[positive]
    log_survival <- log_survival - log_presence[positive] + log1mexp(-lambda[positive])
    q[positive] <- qpois(log_survival, lambda[positive], lower.tail = FALSE, log.p = TRUE)
  }
  q
}

# Validate probabilities before taking logs, retaining NA and true endpoints.
quantile_log_probability <- function(p, log_p = FALSE) {
  invalid <- if (log_p) which(p > 0) else which(p < 0 | p > 1)
  if (length(invalid)) {
    warning("NaNs produced")
    p[invalid] <- NaN
  }
  if (log_p) p else log(p)
}

# log(1 - exp(x)) for x <= 0, without cancellation near zero.
log1mexp <- function(x) {
  out <- log(-expm1(x))
  small <- which(x < -log(2))
  out[small] <- log1p(-exp(x[small]))
  out
}

#' @importFrom stats qt
`make_qf_scat` <- function(nu, sigma) {
  function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    qt(
      p,
      df = nu,
      lower.tail = lower_tail,
      log.p = log_p
    ) * sigma + mu
  }
}

#' @importFrom stats qgamma
`make_qf_tw` <- function(theta, ab) {
  xi <- if (length(ab) == 1L) ab else theta_2_power(theta, a = ab[1], b = ab[2])
  force(xi)
  function(p, mu, wt, scale, log_p = FALSE, fallback = FALSE,
      max_terms = 10000L, lower_tail = TRUE) {
    if (xi == 2) {
      return(qgamma(p, shape = 1 / scale, scale = mu * scale, log.p = log_p,
        lower.tail = lower_tail))
    }
    original <- p
    if (!lower_tail) p <- if (log_p) log1mexp(p) else 1 - p
    value <- qtweedie_mixture(p, mu = mu, power = xi, phi = scale, log_p = log_p,
      fallback = fallback, max_terms = max_terms)
    if (!lower_tail) check_legacy_quantile_tail(value, original, p, log_p, log_p)
    value
  }
}


# Gamma shape is wt/scale and Gamma scale is mu*scale/wt. Use these parameters
# consistently in CDF, quantile and simulation functions.
rd_gamma <- function(mu, wt, scale) {
  stats::rgamma(length(mu), shape = wt / scale, scale = mu * scale / wt)
}
qf_gamma <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
  stats::qgamma(p, shape = wt / scale, scale = mu * scale / wt,
    lower.tail = lower_tail, log.p = log_p)
}

cdf_shash <- function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
  delta <- exp(mu[, 4L])
  z <- sinh(delta * asinh((q - mu[, 1L]) / (delta * exp(mu[, 2L]))) - mu[, 3L])
  stats::pnorm(z, lower.tail = lower_tail, log.p = log_p)
}

make_cdf_twlss <- function(family) {
  ab <- get_tw_params(family)
  function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    fitted_tweedie_cdf(q, mu = mu[, 1L],
      power = theta_2_power(mu[, 2L], ab[1L], ab[2L]),
      phi = exp(mu[, 3L]), lower_tail = lower_tail, log_p = log_p)
  }
}
make_qf_twlss <- function(family) {
  ab <- get_tw_params(family)
  function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    fitted_tweedie_quantile(p, mu = mu[, 1L],
      power = theta_2_power(mu[, 2L], ab[1L], ab[2L]),
      phi = exp(mu[, 3L]), log_p = log_p, lower_tail = lower_tail)
  }
}

# An ordered category records which interval contains a logistic latent value.
# The category CDFs provide the probability bounds for its randomized PIT.
make_cdf_ocat <- function(cutpoints) {
  force(cutpoints)
  function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    n <- max(length(q), length(mu))
    q <- rep_len(q, n)
    category <- floor(q)
    cuts <- c(-Inf, cutpoints, Inf)
    at <- pmax(0, pmin(length(cutpoints) + 1L, category)) + 1L
    stats::plogis(cuts[at] - rep_len(mu, n),
      lower.tail = lower_tail, log.p = log_p)
  }
}
make_qf_ocat <- function(cutpoints) {
  force(cutpoints)
  function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    latent <- stats::qlogis(p, location = mu,
      lower.tail = lower_tail, log.p = log_p)
    out <- rep(1, length(latent))
    for (cut in cutpoints) out <- out + (latent > cut)
    out
  }
}

# ziP and ziplss share a positive Poisson distribution and a separate zero
# probability. In ziP, fitted theta relates log intensity to the predictor
# for the probability of a positive response.
make_cdf_zip <- function(theta) {
  force(theta)
  function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    params <- cbind(mu, theta[1L] + theta[2L] * mu)
    base <- distribution_from_parameters("poisson", exp(mu))
    positive <- truncate_distribution(base, mean = exp(mu) / (-expm1(-exp(mu))))
    zero_inflated_distribution(positive, exp(-exp(params[, 2L])))$cdf(
      q, log_p = log_p, lower_tail = lower_tail)
  }
}
make_qf_zip <- function(theta) {
  force(theta)
  function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    base <- distribution_from_parameters("poisson", exp(mu))
    positive <- truncate_distribution(base, mean = exp(mu) / (-expm1(-exp(mu))))
    zero_inflated_distribution(positive, exp(-exp(theta[1L] + theta[2L] * mu)))$qf(
      p, log_p = log_p, lower_tail = lower_tail)
  }
}


qf_shash <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
  delta <- exp(mu[, 4L])
  z <- stats::qnorm(p, lower.tail = lower_tail, log.p = log_p)
  mu[, 1L] + delta * exp(mu[, 2L]) * sinh((asinh(z) + mu[, 3L]) / delta)
}


# With zero binomial trials, return the initialized zero proportion to avoid 0/0.
rd_binomial <- function(mu, wt, scale) {
  stats::rbinom(length(mu), size = wt, prob = mu) / (wt + as.numeric(wt == 0))
}
qf_binomial <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
  stats::qbinom(p, size = wt, prob = mu, log.p = log_p, lower.tail = lower_tail) /
    (wt + as.numeric(wt == 0))
}


# Some GLM families store the negative binomial size in the environment of
# variance(). Add a getter to the family copy so the shared helpers can read it.
normalize_native_nb_parameters <- function(family) {
  if (!identical(family_type(family), "negative_binomial") || has_theta(family) ||
      !is.function(family$variance)) return(family)
  e <- environment(family$variance)
  if (is.null(e) || !exists(".Theta", envir = e, inherits = FALSE)) return(family)
  size <- get(".Theta", envir = e, inherits = FALSE)
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0)
    return(family)
  family$getTheta <- function(trans = TRUE) if (trans) size else log(size)
  family
}


qf_gaussian <- function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
  stats::qnorm(p, mean = mu, sd = sqrt(scale / wt), log.p = log_p,
    lower.tail = lower_tail)
}

make_qf_beta <- function(phi, eps) {
  force(phi); force(eps)
  function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    q <- stats::qbeta(p, shape1 = phi * mu, shape2 = phi * (1 - mu),
      log.p = log_p, lower.tail = lower_tail)
    pmax(eps, pmin(1 - eps, q))
  }
}

# For very small S, log(-log(1-S)) approaches log(S), while evaluating 1-S
# loses precision. Use log(S) directly to retain finite upper quantiles.
extreme_value_quantile_log_hazard <- function(p, log_p = FALSE, lower_tail = TRUE) {
  logp <- quantile_log_probability(p, log_p)
  if (lower_tail) return(log(-logp))
  value <- log(-log1mexp(logp))
  small <- which(logp < -36)
  value[small] <- logp[small]
  value
}

# Converting a log or upper probability can round an interior value to 0 or 1.
# Reject a non-finite quantile caused by that conversion; genuine endpoint
# requests remain valid.
check_legacy_quantile_tail <- function(value, original, converted,
                                       original_log, converted_log) {
  interior <- if (original_log) is.finite(original) & original < 0 else
    is.finite(original) & original > 0 & original < 1
  endpoint <- if (converted_log) converted == 0 | converted == -Inf else
    converted == 0 | converted == 1
  lost <- rep_len(interior, length(value)) & rep_len(endpoint, length(value)) &
    !is.finite(value)
  if (any(lost, na.rm = TRUE)) stop("Extreme-tail quantiles are unsupported by this legacy ",
    "inverse-CDF hook; provide native log-probability and tail support.", call. = FALSE)
  invisible(value)
}

# multinom() uses K logits relative to category 0, including when K = 1.
# A vector denotes one observation for K > 1 and multiple observations for K = 1.
multinom_arguments <- function(x, mu, K) {
  if (is.null(dim(mu))) {
    if (K > 1L && length(mu) != K)
      stop("mu must contain K logits relative to category 0.", call. = FALSE)
    mu <- matrix(mu, ncol = K, byrow = TRUE)
  }
  if (!is.numeric(mu) || !is.matrix(mu) || ncol(mu) != K)
    stop("mu must contain K logits relative to category 0.", call. = FALSE)
  n <- max(length(x), nrow(mu))
  if (!length(x) || !nrow(mu)) return(list(x = numeric(), eta = matrix(0, 0, K + 1L)))
  if (!length(x) %in% c(1L, n) || !nrow(mu) %in% c(1L, n))
    stop("Supply one argument per observation, or a single value to repeat.", call. = FALSE)
  list(x = rep_len(x, n), eta = cbind(0, mu[rep_len(seq_len(nrow(mu)), n), , drop = FALSE]))
}

# Compute each tail from its own categories. Taking a complement of a rounded
# probability would lose small masses; log-sum-exp also avoids overflowing logits.
multinom_log_tail <- function(eta, q, lower_tail) {
  vapply(seq_len(nrow(eta)), function(i) {
    z <- eta[i, ]
    if (anyNA(z) || is.na(q[i])) return(NA_real_)
    if (any(!is.finite(z))) return(NaN)
    z <- z - max(z)
    include <- seq_along(z) - 1L <= floor(q[i])
    if (!lower_tail) include <- !include
    if (!any(include)) return(-Inf)
    if (all(include)) return(0)
    log_sum <- function(z) {
      m <- max(z)
      if (m == -Inf) return(-Inf)
      m + log(sum(exp(z - m)))
    }
    difference <- log_sum(z[!include]) - log_sum(z[include])
    -(max(difference, 0) + log1p(exp(-abs(difference))))
  }, numeric(1))
}

make_cdf_multinom <- function(K) {
  force(K)
  function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    a <- multinom_arguments(q, mu, K)
    value <- multinom_log_tail(a$eta, a$x, lower_tail)
    if (log_p) value else exp(value)
  }
}

make_qf_multinom <- function(K) {
  force(K)
  function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
    a <- multinom_arguments(quantile_log_probability(p, log_p), mu, K)
    p <- a$x
    out <- rep(NA_real_, length(p))
    # Use the smaller probability to distinguish jumps close to either endpoint.
    small_lower <- if (lower_tail) p <= -log(2) else p > -log(2)
    target <- p
    complement <- which(if (lower_tail) !small_lower else small_lower)
    target[complement] <- log1mexp(p[complement])
    for (j in 0:K) {
      lower <- multinom_log_tail(a$eta, rep(j, length(p)), TRUE)
      upper <- multinom_log_tail(a$eta, rep(j, length(p)), FALSE)
      # Allow a few rounding units at a jump, as R's discrete quantiles do.
      tolerance <- 4 * .Machine$double.eps * pmax(1, abs(target))
      reached <- ifelse(small_lower, lower >= target - tolerance,
        upper <= target + tolerance)
      rows <- which(is.na(out) & reached)
      out[rows] <- j
    }
    # Endpoints refer to the finite categorical support, even if a probability
    # has rounded to zero or one at the supplied logits.
    valid <- rowSums(!is.finite(a$eta)) == 0L
    out[which(valid & p == -Inf)] <- if (lower_tail) 0 else K
    out[which(valid & p == 0)] <- if (lower_tail) K else 0
    out
  }
}
