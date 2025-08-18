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
    tw_pars <- get_tw_ab(fam)
    fam$rd <- rd_twlss(a = tw_pars[1], b = tw_pars[2])
  }

  # return modified family
  fam
}

`fix_family_cdf` <- function(family) {
  # if `family` contains a NULL cdf we move on, if it is non-null return early
  # as it doesn't need fixing
  if (!is.null(family$cdf)) {
    return(family)
  }

  # handle special cases
  fn <- family_name(family)

  # choose a CDF functions
  qfun <- switch(
    EXPR = fn,
    "poisson"  = cdf_poisson,
    "gaussian" = cdf_gaussian,
    "binomial" = cdf_binomial,
    "Gamma"    = cdf_gamma,
    NULL
  )

  # add the CDF fun to the family
  family$cdf <- qfun

  # don't think we need to throw and error - fix.family.rd doesn't
  #if (is.null(family$cdf)) {
  #  stop("No CDF functions available for this family")
  #}

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
    q * (wt + as.numeric(wt == 0)), size = wt, prob = mu, , log.p = log_p
  )
}

#' @importFrom stats pgamma
`cdf_gamma` <- function(q, mu, wt, scale, log_p = FALSE) {
  # uggh this weird parameterisation in pgamma
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

`cdf_invgaussian` <- function(q, mu, wt, scale, log_p = FALSE) {
  pinvgaussian(q, mu = mu, wt = wt, scale = scale, log_p = log_p)
}
