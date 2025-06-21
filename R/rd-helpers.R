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
