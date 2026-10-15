# Run after installing this version of gratia, or after pkgload::load_all().
# This deliberately performs real refits rather than using an identity of the
# PSIS implementation. It is a manual validation because Monte Carlo error
# and fitting all 180 held-out models make it unsuitable for routine tests.
library("mgcv")
library("gratia")
library("loo")

log_mean_exp <- function(x) {
  largest <- max(x)
  largest + log(mean(exp(x - largest)))
}

validate_loo_refits <- function(n = 60L, draws = 8000L, seed = 278L) {
  withr::with_seed(seed, {
    d <- data.frame(x = seq(-1, 1, length.out = n))
    eta <- .3 + .7 * d$x + .3 * sin(3 * d$x)
    responses <- list(gaussian = eta + rnorm(n, sd = .7),
      poisson = rpois(n, exp(eta)),
      binomial = rbinom(n, 1, plogis(eta)))
    families <- list(gaussian = gaussian(), poisson = poisson(),
      binomial = binomial())
    results <- lapply(names(responses), function(name) {
      d$y <- responses[[name]]
      family <- families[[name]]
      fit <- gam(y ~ s(x, k = 5), data = d, family = family, method = "REML")
      ll <- log_lik(fit, n = draws, seed = seed)
      psis <- loo::loo(ll, r_eff = 1)
      refit_lpd <- matrix(NA_real_, n, 2,
        dimnames = list(NULL, c("fixed", "reestimated")))
      for (i in seq_len(n)) {
        for (mode in colnames(refit_lpd)) {
          fixed <- mode == "fixed"
          # The primary comparison holds the posterior approximation's fitted
          # smoothing and nuisance parameters fixed, just as log_lik() does.
          refit <- gam(y ~ s(x, k = 5), data = d[-i, ], family = family,
            method = "REML", sp = if (fixed) fit$sp else NULL,
            scale = if (fixed) fit$sig2 else 0)
          # Use the public sampler without using log_lik() for held-out densities.
          expected <- gratia::fitted_samples(refit, data = d[i, , drop = FALSE],
            n = draws, seed = seed + i)
          mu <- expected$.fitted
          density <- switch(name,
            gaussian = dnorm(d$y[i], mu, sqrt(refit$sig2), log = TRUE),
            poisson = dpois(d$y[i], mu, log = TRUE),
            binomial = dbinom(d$y[i], 1, mu, log = TRUE))
          refit_lpd[i, mode] <- log_mean_exp(density)
        }
      }
      pointwise <- unname(psis$pointwise[, "elpd_loo"])
      data.frame(family = name,
        psis_elpd = sum(pointwise),
        fixed_refit_elpd = sum(refit_lpd[, "fixed"]),
        reestimated_refit_elpd = sum(refit_lpd[, "reestimated"]),
        fixed_pointwise_rmse = sqrt(mean((pointwise - refit_lpd[, "fixed"])^2)),
        max_pareto_k = max(psis$diagnostics$pareto_k))
    })
    do.call(rbind, results)
  })
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  validation_results <- validate_loo_refits(
    n = if (length(args)) as.integer(args[1]) else 60L,
    draws = if (length(args) > 1L) as.integer(args[2]) else 8000L)
  print(validation_results)
  # Review Pareto diagnostics alongside discrepancies. The second refit column
  # measures how re-estimating smoothing and scale changes the target comparison.
  stopifnot(all(is.finite(as.matrix(validation_results[, -1L]))))
  stopifnot(all(validation_results$fixed_pointwise_rmse < .05))
}
