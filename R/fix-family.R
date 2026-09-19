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

# These families estimate their own scale in theta; the model dispersion is
# not the distribution scale. Helpers describe the latent, uncensored response.
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
  # uggh this weird parameterisation in pgamma
  # FIXME: what about weights wt?
  pgamma(q, shape = 1 / scale, scale = mu * scale,
    lower.tail = lower_tail, log.p = log_p)
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
`cdf_gaulss` <- function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
  pnorm(
    q, mean = mu[, 1, drop = TRUE],
    sd = 1 / mu[, 2, drop = TRUE], lower.tail = lower_tail, log.p = log_p
  )
}

`cdf_gumbls` <- function(q, mu, wt, scale, log_p = FALSE) {
  gamma <- 0.577215664901533 # euler's constant
  # mu[, 1] is mean, mu[, 2] is log(beta)
  # F(x) = exp(-exp(-gamma - \frac{x-mu}{beta}))
  exp(-exp(-gamma - ((q - mu[, 1]) / exp(mu[ ,2]))))
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

`cdf_gevlss` <- function(q, mu, wt, scale, log_p = FALSE, tol = 1e-6) {
  # check sigma --- doubt needed as this is deep in mgcv
  # if (any(mu[, 2] <= 0)) {
  #   stop("'sigma' must be positive")
  # }
  
  # standardized variable
  z <- (q - mu[, 1]) / mu[, 2] # (x - mu) / sigma
  
  # Allocate result
  out <- numeric(nrow(mu))
  
  # any small xi
  small <- abs(mu[, 3]) < tol
  
  # large xi
  if (any(!small)) {
    log_term <- log1p(mu[!small, 3] * z[!small])   # log(1 + xi*z)
    a <- -log_term / mu[!small, 3]
    
    out[!small] <- exp(-exp(a))
  }
  
  # small xi Taylor expansion around 0 for the approaching Gumble case
  if (any(small)) {
    a <- -z[small] + 0.5 * mu[small, 3] * z[small]^2
    out[small] <- exp(-exp(a))
  }
  
  # handle support (important for xi < 0)
  valid <- 1 + mu[, 3] * z > 0
  out[!valid & mu[, 3] > 0] <- 0
  out[!valid & mu[, 3] < 0] <- 1
  
  out
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
  function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
    # what to do with weights wt? I think this should be (q - mu) / sigma / wt
    # q <- (q - mu) / sigma
    pt(
      (q - mu) / scale, # scale the data such that q ~ t_{nu}()
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
  function(q, mu, wt, scale, log_p = FALSE,
    lower_tail = TRUE) {
    p <- pbeta(
      q,
      shape1 = phi * mu,
      shape2 = phi * (1 - mu),
      lower.tail = lower_tail, log.p = log_p
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
    "cnorm"    = make_qf_cnorm(theta),
    "clog"     = make_qf_clog(theta),
    "cpois"    = qf_poisson,
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
make_qf_cnorm <- function(sigma) {
  force(sigma)
  function(p, mu, wt, scale, log_p = FALSE) {
    qnorm(p, mean = mu, sd = sigma / sqrt(wt), log.p = log_p)
  }
}

#' @importFrom stats qlogis
make_qf_clog <- function(sigma) {
  force(sigma)
  function(p, mu, wt, scale, log_p = FALSE) {
    qlogis(p, location = mu, scale = sigma / sqrt(wt), log.p = log_p)
  }
}

#' @importFrom stats qpois
qf_poisson <- function(p, mu, wt, scale, log_p = FALSE) {
  qpois(p, lambda = mu, log.p = log_p)
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

`qf_gevlss` <- function(p, mu, wt, log_p = FALSE, tol = 1e-6) {
  # checks, but I doubt these are needed as this is deep in mgcv
  #if (any(p <= 0 | p >= 1)) {
  #  stop("All probabilities 'p' must be in (0, 1)")
  #}
  #if (any(mu[, 2] <= 0)) {
  #  stop("All 'sigma' must be positive")
  #}

  # extract vectors --- perhaps not if data is big?
  # sigma <- mu[, 2]
  # xi    <- mu[, 3]
  # mu    <- mu[, 1]

  t <- -log(p)
  logt <- log(t)

  # |xi| well away from zero so use textbook formula
  small <- abs(mu[, 3]) < tol
  # large <- !small

  out <- numeric(length(p))

  # textbook formula
  if (any(!small)) {
    out[!small] <- mu[!small, 1] + mu[!small, 2] *
      expm1(-mu[!small, 3] * logt[!small]) / mu[!small, 3]
  }

  # use Taylor expansion around xi = 0
  if (any(small)) {
    out[small] <- mu[small, 1] + mu[small, 2] * 
      (-logt[small] + 0.5 * mu[small, 3] * logt[small]^2)
  }

  out
}

# mgcv uses a mean-centred parameterisation
qf_gumbls <- function(p, mu, wt, scale, log_p = FALSE) {
  gamma <- 0.577215664901533 # euler's constant
  # mu[, 1] is mean, mu[, 2] is log(beta)
  mu[, 1] - exp(mu[, 2]) * (gamma + log(-log(p)))
}

#' @importFrom stats qgamma
`qf_gammals` <- function(p, mu, wt, scale, log_p = FALSE) {
  # mu[,1] is the mean, mu[,2] is log scale (dispersion)
  # exp(mu[,2]) == phi
  phi <- exp(mu[, 2])
  qgamma(p, shape = 1 / phi, scale = mu[, 1] * phi)
}

#' @importFrom stats qpois
`qf_ziplss` <- function(p, mu, wt, scale, log_p = FALSE) {
  # gamma is log(lambda), eta is prob of >0 on a cloglog scale
  # mu is matrix, col 1 is gamma, col 2 is eta
  n <- nrow(mu)
  
  # Parameters
  lambda <- exp(mu[,1])
  pr <- -expm1(-exp(mu[,2]))   # stable: 1 - exp(-exp(eta))
  
  # Output
  q <- numeric(n)
  
  # p <= 1 - pr
  idx0 <- (p <= (1 - p))
  q[idx0] <- 0
  
  # p > 1 - pr
  idx <- !idx0
  
  if (any(idx)) {
    p_i <- p[idx]
    lam <- lambda[idx]
    pr_i <- pr[idx]
    
    # log P(Y=0)
    log_pr0 <- -lam
    pr0 <- exp(log_pr0)
    
    # Stable computation of transformed probability
    # (p - (1 - pr)) / pr
    p_scaled <- (p_i - (1 - pr_i)) / pr_i
    
    # u_star = pr0 + (1 - pr0) * u_scaled
    # use expm1/log1p for stability
    p_star <- pr0 + (-expm1(log_pr0)) * p_scaled
    
    # Clamp to avoid qpois issues
    eps <- .Machine$double.eps^0.75
    p_star[p_star >= 1] <- 1 - eps
    p_star[p_star <= 0] <- eps
    
    # Invert Poisson CDF
    q[idx] <- qpois(p_star, lambda = lam)
  }
  
  q
}


#' @importFrom stats pt
`make_qf_scat` <- function(nu, sigma) {
  function(p, mu, wt, scale, log_p = FALSE) {
    qt(
      p,
      df = nu,
      lower.tail = TRUE,
      log.p = log_p
    ) * scale + mu
  }
}

#' @importFrom tweedie qtweedie
`make_qf_tw` <- function(theta, ab) {
  fun <- if (length(ab) == 1L) {
    # here ab is the tweedie power specified in Tweedie()
    function(p, mu, wt, scale, log_p = FALSE){
      tweedie::qtweedie(
        p,
        mu = mu,
        phi = scale,
        xi = ab
      )
    }
  } else {
    function(p, mu, wt, scale, log_p = FALSE) {
      a <- ab[1] # tweedie lower and upper bounds used in fitting
      b <- ab[2]
      # compute tweedie power parameter xi
      xi <- if (theta > 0) {
        (b + a * exp(-theta)) / (1 + exp(-theta))
      } else {
        (b * exp(theta) + a) / (exp(theta) + 1)
      }
      tweedie::qtweedie(
        p,
        mu = mu,
        phi = scale, # think to handle weights we need scale / wt
        xi = xi
      )
    }
  }
  fun
}
