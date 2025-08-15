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

# simulator for tweedie LSS models
#' @importFrom stats rpois rgamma
#' @importFrom tibble new_tibble
#' @importFrom vctrs vec_group_loc vec_chop df_list
`rtw` <- function(mu, p, phi) {
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
  gs <- rep(scale, n)
  tab <- new_tibble(
    df_list(
      y = rgamma(gs * 0 + 1, shape = shape, scale = gs),
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
