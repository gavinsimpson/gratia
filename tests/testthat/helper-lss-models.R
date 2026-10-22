# All reference calculations below use the distribution definitions, separately
# from the parameter adapter used by the sampling functions.
lss_test_models <- local({
  cache <- new.env(parent = emptyenv())
  function(name = "gaulss") {
    if (exists(name, cache, inherits = FALSE)) return(cache[[name]])
    m <- withr::with_seed(281, {
      n <- 400L
      d <- data.frame(x = runif(n), o1 = runif(n, -.1, .1),
        o2 = runif(n, -.1, .1), o3 = runif(n, -.05, .05),
        o4 = runif(n, -.05, .05), w = rep(c(1, 2), n / 2))
      mu <- .4 + .3 * d$x + d$o1
      d$y <- switch(name,
        gaulss = rnorm(n, mu, .8),
        gammals = rgamma(n, 4, scale = exp(mu) / 4),
        gumbls = mu - .7 * log(-log(runif(n))),
        gevlss = mu + .7 * ((-log(runif(n)))^(-.1) - 1) / .1,
        shash = mu + 1.2 * .7 * sinh((asinh(rnorm(n)) + .4) / 1.2),
        twlss = mgcv::rTweedie(exp(mu), p = 1.5, phi = .6),
        ziplss = {
          u <- runif(n)
          lambda <- exp(mu)
          ifelse(runif(n) < .7, qpois(exp(-lambda) + u * (-expm1(-lambda)), lambda), 0)
        })
      family <- get(name, asNamespace("mgcv"))()
      formulas <- list(y ~ x + offset(o1), ~x + offset(o2),
        ~x + offset(o3), ~x + offset(o4))[seq_len(family$nlp)]
      if (name == "shash") {
        formulas <- list(y ~ x, ~x, ~x, ~x)
        d[c("o1", "o2", "o3", "o4")] <- 0
      }
      # mgcv can try points outside GEV support during optimisation.
      fit <- withCallingHandlers(
        mgcv::gam(formulas, data = d, weights = w, family = family, method = "REML"),
        warning = function(w) {
          if (name == "gevlss" && conditionMessage(w) == "NaNs produced")
            invokeRestart("muffleWarning")
        })
      attr(fit, "lss_test_data") <- d
      fit
    })
    cache[[name]] <- m
    m
  }
})

lss_test_beta <- function(name) {
  switch(name,
    gaulss = c(.4, .3, -.4, .2),
    gammals = c(.4, .3, -1, .2),
    gumbls = c(.4, .3, -1, .2),
    gevlss = c(.4, .3, -1, .2, -.5, .1),
    shash = c(.4, .3, -1, .2, .4, .1, .1, .1),
    twlss = c(.4, .3, .1, .2, -.4, .1),
    ziplss = c(.4, .3, -.1, .2))
}

lss_test_eta <- function(beta, d) {
  do.call(cbind, lapply(seq_len(length(beta) / 2), function(j) {
    beta[2 * j - 1] + beta[2 * j] * d$x + d[[paste0("o", j)]]
  }))
}

lss_test_mean <- function(name, eta) {
  switch(name,
    gaulss = eta[, 1], gammals = exp(eta[, 1]), twlss = exp(eta[, 1]),
    gumbls = eta[, 1] - digamma(1) * (exp(eta[, 2]) + exp(-7)),
    gevlss = {
      xi <- -1 + 1.5 * plogis(eta[, 3])
      eta[, 1] + exp(eta[, 2]) * (gamma(1 - xi) - 1) / xi
    },
    shash = vapply(seq_len(nrow(eta)), function(i) {
      sigma <- exp(eta[i, 2]) + .01
      delta <- exp(eta[i, 4])
      integrate(function(z) (eta[i, 1] + delta * sigma *
          sinh((asinh(z) + eta[i, 3]) / delta)) * dnorm(z), -10, 10,
        rel.tol = 1e-10)$value
    }, numeric(1)),
    ziplss = (-expm1(-exp(eta[, 2]))) * exp(eta[, 1]) / (-expm1(-exp(eta[, 1]))))
}

