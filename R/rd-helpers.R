# Functions that do bits of what family()$rd does
# - sometimes we don't want to work exactly like Simon expects. An example is
#   `multinom()` models in `posterior_samples()` where we take output from
#   `fitted_samples()` and use that to simulate new response values. But,
#   `multinom()$rd` expects a linear predictor matrix as input, not the class
#   probabilities. So, we have our own `multinom_rd` that takes a matrix of
#   class probabilities.

`multinom_rd` <- function(mu, wt, scale) {
  # mu is a matrix of class probabilities
  # we ignore weights and scale, but we need them as arg names
  cum_p <- t(apply(mu, 1, cumsum)) # cumulative probabilities
  apply(cum_p, 1, \(x) min(which(x > runif(1)))) - 1
}

# helper function for choosing an rd function
`choose_rd_fun` <- function(model) {
  fun <- switch(
    EXPR = family_type(model),
    multinom = multinom_rd,
    get_family_rd(model)
  )
  fun # return
}

#' Simulator for tweedie LSS models
#'
#' Simulate random deviates from a Tweedie distribution with given parameters
#' \eqn{\mu}{mu}, \eqn{p}{p}, and \eqn{\phi}{phi}. Works with vector values for
#' all parameters, unlike the version on *mgcv*.
#'
#' @param mu numeric vector of mean values of Tweedie distribution.
#' @param p numeric vector of values for the power parameter of the Tweedie
#'   distribution.
#' @param phi numeric vector of values for the scale parameter \eqn{\phi}{phi}
#'   of the Tweedie distribution.
#'
#' @details
#' Parameters must be finite numeric vectors of length one or a common nonzero
#' length. Length-one parameters are expanded to the common length; partial
#' recycling is not supported. If all parameters are empty, an empty numeric
#' vector is returned; mixing empty and nonempty parameters is an error.
#'
#' @export
#' @importFrom stats rpois rgamma
#' @importFrom tibble new_tibble
#' @importFrom vctrs vec_group_loc vec_chop df_list
`rtw` <- function(mu, p, phi) {
  pars <- list(mu = mu, p = p, phi = phi)
  lens <- lengths(pars)
  size <- max(lens)
  if (size == 0L && all(lens == 0L)) {
    return(numeric())
  }
  if (any(lens == 0L) || any(!lens %in% c(1L, size))) {
    stop("Parameters must have length 1 or a common nonzero length.")
  }
  if (any(!vapply(pars, function(x) is.numeric(x) && all(is.finite(x)),
                 logical(1)))) {
    stop("Parameters must be finite numeric vectors.")
  }
  mu <- rep_len(mu, size)
  p <- rep_len(p, size)
  phi <- rep_len(phi, size)
  if (any(p <= 1 | p >= 2)) {
    stop("'p' must be in interval (1, 2)")
  }
  if (any(phi <= 0)) {
    stop("scale parameter 'phi' must be positive")
  }
  if (any(mu < 0)) {
    stop("mean 'mu' must be non-negative")
  }
  lambda <- mu^(2 - p) / ((2 - p) * phi)
  shape <- (2 - p) / (p - 1)
  scale <- phi * (p - 1) * mu^(p - 1)
  n <- rpois(length(lambda), lambda)
  if (all(n == 0)) {
    return(numeric(length(n)))
  }
  gs <- rep(scale, n)
  tab <- new_tibble(
    df_list(
      y = rgamma(gs * 0 + 1, shape = rep(shape, n), scale = gs),
      lab = rep(seq_along(n), n)
    )
  )
  out <- numeric(length(n))
  chop_fun <- function(.x, .by) {
    idx <- vec_group_loc(.by)$loc
    chp <- vec_chop(.x, indices = idx)
    out <- lapply(chp, sum)
    unlist(out)
  }
  out[which(n != 0)] <- chop_fun(tab$y, tab$lab)
  out
}
