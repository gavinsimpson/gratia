#' @importFrom mgcv fix.family.rd
`fix_family_rd` <- function(family, ncores = 1, ...) {
  # try to fix up the family used by mgcv to add the $rd component
  # for random deviate sampling

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

  # if still null, try one of the other families that just need an rd_fun,
  # not an rd-factory one as these don't rely on other parameters
  if (is.null(fam$rd)) {
    ft <- family_type(fam)

    rd_fun <- switch(
      EXPR = ft,
      "cnorm" = rd_gaussian,
      "cpois" = rd_poisson,
      "clog"  = rd_logistic,
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
  rlogis(length(mu), location = mu, scale = scale)
}

`fix_family_cdf` <- function(family) {
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
    "gaussian" = cdf_gaussian,
    "gaulss"   = cdf_gaulss,
    "gevlss"   = cdf_gevlss,
    "gumbls"   = cdf_gumbls,
    "gammals"  = cdf_gammals,
    "binomial" = cdf_binomial,
    "gamma"    = cdf_gamma,
    "ziplss"    = cdf_ziplss,
    "scaled_t" = make_cdf_scat(nu = theta[1], sigma = theta[2]),
    "negative_binomial" = make_cdf_nb(theta),
    ## "inverse_gaussian"  = cdf_invgaussian, # FIXME: not sure I trust this yet
    "beta_regression"   = make_cdf_beta(theta, eps = betar_eps(family)),
    "tweedie"  = make_cdf_tw(theta, ab = get_tw_params(family)),
    NULL
  )

  # add the CDF fun to the family
  family$cdf <- cdf_fun

  # return
  family
}

#' @importFrom stats ppois
`cdf_poisson` <- function(q, mu, wt, scale, log_p = FALSE) {
  ppois(q, lambda = mu, log.p = log_p)
}

#' @importFrom stats pnorm
`cdf_gaussian` <- function(q, mu, wt, scale, log_p = FALSE) {
  pnorm(q, mean = mu, sd = sqrt(scale / wt), log.p = log_p)
}

#' @importFrom stats pbinom
`cdf_binomial` <- function(q, mu, wt, scale, log_p = FALSE) {
  pbinom(
    q * (wt + as.numeric(wt == 0)), size = wt, prob = mu, log.p = log_p
  )
  #pbinom(floor(wt * q), wt, mu)
}

#' @importFrom stats pgamma
`cdf_gamma` <- function(q, mu, wt, scale, log_p = FALSE) {
  # uggh this weird parameterisation in pgamma
  # FIXME: what about weights wt?
  pgamma(q, shape = 1 / scale, scale = mu * scale, log.p = log_p)
}

#' @importFrom stats qnorm
`pinvgaussian` <- function(q, mu, wt, scale, log_p = FALSE) {
  # only for lower tail of CDF
  # based on implementation of Giner & Smyth (2016) The R Journal **8*(1) 339
  q <- q / mu
  scale <- scale * mu
  r <- sqrt(q * scale)
  phi_m <- scale * mu
  a <- pnorm((q - 1) / r, lower.tail = TRUE, log.p = TRUE)
  b <- (2 / scale) + pnorm(-(q + 1) / r, lower.tail = TRUE, log.p = TRUE)
  p <- a + log1p(exp(b - a))
  if (isFALSE(log_p)) {
    p <- exp(p)
  }
  p
}

# FIXME: something seems off for small quantiles... statmod does something
# different for y > mu and y < mu, using the upper and lower tail of the
# pinvgaussian respectively.
`cdf_invgaussian` <- function(q, mu, wt, scale, log_p = FALSE) {
  pinvgaussian(q, mu = mu, wt = wt, scale = scale, log_p = log_p)
}

#' @importFrom stats pnorm
`cdf_gaulss` <- function(q, mu, wt, scale, log_p = FALSE) {
  pnorm(
    q, mean = mu[, 1, drop = TRUE],
    sd = 1 / mu[, 2, drop = TRUE], log.p = log_p
  )
}

`cdf_gumbls` <- function(q, mu, wt, scale, log_p = FALSE) {
  gamma <- 0.577215664901533 # euler's constant
  # mu[, 1] is mean, mu[, 2] is log(beta)
  # F(x) = exp(-exp(-gamma - \frac{x-mu}{beta}))
  exp(-exp(-gamma - ((q - mu[, 1]) / exp(mu[ ,2]))))
}

#' @importFrom stats pgamma
`cdf_gammals` <- function(q, mu, wt, scale, log_p = FALSE) {
  # mu[,1] is the mean, mu[,2] is log scale (dispersion)
  # exp(mu[,2]) == phi
  phi <- exp(mu[, 2])
  pgamma(q, shape = 1 / phi, scale = mu[, 1] * phi)
}

`cdf_gevlss` <- function(q, mu, wt, scale, log_p = FALSE, tol = 1e-6) {
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
  out <- -exp(logt)
  if (log_p) out else exp(out)
}

# implementation needs checking; ChatGPT used to derive the math for the CDF
#' @importFrom stats ppois
`cdf_ziplss` <- function(q, mu, wt, scale, log_p = FALSE) {
  lambda <- exp(mu[, 1])
  p <- -expm1(-exp(mu[, 2])) # = 1 - exp(-exp(eta))

  log_cdf <- numeric(nrow(mu))
  
  # y < 0
  log_cdf[q < 0] <- -Inf

  # 0 <= y < 1
  idx0 <- (q >= 0 & q < 1)
  log_cdf[idx0] <- log1p(-p[idx0]) # log(1 - p)

  # y >= 1
  idx <- (q >= 1)
  
  if (any(idx)) {
    y_i <- floor(q[idx])
    lam <- lambda[idx]
    p_i <- p[idx]
    
    # log P(Y=0)
    log_p0 <- -lam
    
    # log Poisson CDF
    log_cdfy <- ppois(y_i, lam, log.p = TRUE)
    
    # compute log(Fy - p0) safely
    # log(exp(log_Fy) - exp(log_p0))
    log_num <- log_cdfy + log1p(-exp(log_p0 - log_cdfy))
    
    # log(1 - p0)
    log_denom <- log1p(-exp(log_p0))
    
    # log truncated CDF
    log_trunc <- log_num - log_denom
    
    # combine: log((1-p) + p * trunc)
    # Use log-sum-exp
    a <- log1p(-p_i)        # log(1 - p)
    b <- log(p_i) + log_trunc
    
    m <- pmax(a, b)
    log_cdf[idx] <- m + log(exp(a - m) + exp(b - m))
  }

  if (isFALSE(log_p)) {
    log_cdf = exp(log_cdf) # stupid name, I know
  }

  log_cdf # may not be log_cdf but cdf, and likely won't be because log_p is FALSE
}

## For distributions that estimate other parameters, we need to bind the values
## of those parameters inside the CDF function in the family. `theta` are then
## passed to the `make_cdf_xxx()` function as separate parameters with real
## names so it's easy to follow what is being done

#' @importFrom stats pt
`make_cdf_scat` <- function(nu, sigma) {
  function(q, mu, wt, scale, log_p = FALSE) {
    pt(
      (q - mu) / sigma, # mgcv estimates sigma separately from dispersion
      df = nu,
      lower.tail = TRUE, log.p = log_p
    )
  }
}

#' @importFrom stats pnbinom
`make_cdf_nb` <- function(theta) {
  function(q, mu, wt, scale, log_p = FALSE) {
    pnbinom(
      q,
      size = theta,
      mu = mu,
      lower.tail = TRUE, log.p = log_p
    )
  }
}

#' @importFrom stats pbeta
`make_cdf_beta` <- function(phi, eps) {
  function(q, mu, wt, scale, log_p = FALSE) {
    p <- pbeta(
      q,
      shape1 = phi * mu,
      shape2 = phi * (1 - mu),
      lower.tail = TRUE, log.p = log_p
    )
    p
  }
}

#' @importFrom tweedie ptweedie
`make_cdf_tw` <- function(theta, ab) {
  fun <- if (length(ab) == 1L) {
    # here ab is the tweedie power specified in Tweedie()
    function(p, mu, wt, scale, log_p = FALSE){
      tweedie::ptweedie(
        p,
        mu = mu,
        phi = scale,
        xi = ab
      )
    }
  } else {
    function(q, mu, wt, scale, log_p = FALSE) {
      a <- ab[1] # tweedie lower and upper bounds used in fitting
      b <- ab[2]
      # compute tweedie power parameter xi
      xi <- if (theta > 0) {
        (b + a * exp(-theta)) / (1 + exp(-theta))
      } else {
        (b * exp(theta) + a) / (exp(theta) + 1)
      }
      tweedie::ptweedie(
        q,
        mu = mu,
        phi = scale, # think to handle weights we need scale / wt
        xi = xi
      )
    }
  }
  fun
}

# only really need this for QQ plots
#' @importFrom mgcv fix.family.qf
`fix_family_qf` <- function(family) {

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
    "scaled_t" = make_qf_scat(nu = theta[1], sigma = theta[2]),
    "tweedie"  = make_qf_tw(theta, ab = get_tw_params(family)),
    #"nb"       = make_qf_betar(phi = theta),
    #"betar"    = make_qf_betar(theta),
    "gaulss"   = qf_gaulss,
    "gevlss"   = qf_gevlss,
    "gumbls"   = qf_gumbls,
    "gammals"  = qf_gammals,
    "ziplss"   = qf_ziplss,
    NULL # if don't handle family, return NULL as qfun so family unchanged
  )

  # add the QF fun to the family
  family$qf <- qfun

  # return
  family
}

#' @importFrom stats qnorm
`qf_gaulss` <- function(p, mu, wt, scale, log_p = FALSE) {
  qnorm(
    p,
    mean = mu[, 1, drop = TRUE],
    sd = 1 / mu[, 2, drop = TRUE],
    log.p = log_p
  )
}

`qf_gevlss` <- function(p, mu, wt, scale, log_p = FALSE, tol = 1e-6) {
  logp <- quantile_log_probability(p, log_p)
  n <- max(length(logp), nrow(mu))
  logt <- log(-rep_len(logp, n))
  location <- rep_len(mu[, 1], n)
  sigma <- exp(rep_len(mu[, 2], n))
  xi <- rep_len(mu[, 3], n)

  # expm1 avoids cancellation for small shape, without a tol cutoff. Handle
  # the exact Gumbel limit separately, including p = 0 and p = 1.
  z <- -logt
  nonzero <- which(!is.na(xi) & xi != 0)
  z[nonzero] <- expm1(-xi[nonzero] * logt[nonzero]) / xi[nonzero]
  z[is.na(xi)] <- NA_real_
  location + sigma * z
}

# mgcv uses a mean-centred parameterisation
qf_gumbls <- function(p, mu, wt, scale, log_p = FALSE) {
  gamma <- 0.577215664901533 # euler's constant
  # mu[, 1] is mean, mu[, 2] is log(beta)
  mu[, 1] - exp(mu[, 2]) *
    (gamma + log(-quantile_log_probability(p, log_p)))
}

#' @importFrom stats qgamma
`qf_gammals` <- function(p, mu, wt, scale, log_p = FALSE) {
  # mu[,1] is the mean, mu[,2] is log scale (dispersion)
  # exp(mu[,2]) == phi
  phi <- exp(mu[, 2])
  qgamma(p, shape = 1 / phi, scale = mu[, 1] * phi, log.p = log_p)
}

#' @importFrom stats qpois
`qf_ziplss` <- function(p, mu, wt, scale, log_p = FALSE) {
  logp <- quantile_log_probability(p, log_p)
  n <- max(length(logp), nrow(mu))
  logp <- rep_len(logp, n)
  lambda <- exp(rep_len(mu[, 1], n))
  log_zero <- -exp(rep_len(mu[, 2], n))
  log_presence <- log1mexp(log_zero)
  q <- rep(NA_real_, n)
  q[which(logp <= log_zero)] <- 0

  positive <- which(logp > log_zero)
  if (length(positive)) {
    # Invert the conditional positive Poisson distribution using its upper
    # tail. This avoids rounding an interior transformed probability to one.
    log_survival <- log1mexp(logp[positive]) - log_presence[positive] +
      log1mexp(-lambda[positive])
    q[positive] <- qpois(log_survival, lambda[positive],
      lower.tail = FALSE, log.p = TRUE)
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
  function(p, mu, wt, scale, log_p = FALSE) {
    qt(
      p,
      df = nu,
      lower.tail = TRUE,
      log.p = log_p
    ) * sigma + mu
  }
}

#' @importFrom tweedie qtweedie
#' @importFrom stats qgamma
`make_qf_tw` <- function(theta, ab) {
  xi <- if (length(ab) == 1L) {
    ab
  } else {
    theta_2_power(theta, a = ab[1], b = ab[2])
  }
  force(xi)
  function(p, mu, wt, scale, log_p = FALSE) {
    logp <- quantile_log_probability(p, log_p)
    n <- max(length(logp), length(mu), length(scale))
    logp <- rep_len(logp, n)
    mu <- rep_len(mu, n)
    phi <- rep_len(scale, n)
    if (xi == 2) {
      return(qgamma(logp, shape = 1 / phi, scale = mu * phi, log.p = TRUE))
    }
    q <- rep(NA_real_, n)
    q[which(logp == -Inf)] <- 0
    q[which(logp == 0)] <- Inf
    interior <- which(is.finite(logp) & logp < 0)
    if (length(interior)) {
      # qtweedie() expects ordinary probabilities and fails for endpoint-only
      # input. Do not call it for endpoints or missing/invalid probabilities.
      prob <- exp(logp[interior])
      q[interior[prob == 0]] <- 0
      q[interior[prob == 1]] <- Inf
      keep <- which(prob > 0 & prob < 1)
      if (length(keep)) {
        idx <- interior[keep]
        q[idx] <- tweedie::qtweedie(prob[keep], mu = mu[idx],
          phi = phi[idx], xi = xi)
      }
    }
    q
  }
}
