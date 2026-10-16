# One fresh process per model, backend and seed. Called by family-study.R.
args <- commandArgs(TRUE)
if (length(args) != 6L) stop("Supply family, rows, basis size, backend, seed and output path.")
library("gratia")
library("mgcv")
library("posterior")
case <- args[1L]
n_obs <- as.integer(args[2L])
k <- as.integer(args[3L])
backend <- args[4L]
seed <- as.integer(args[5L])
output <- args[6L]
n_draws <- as.integer(Sys.getenv("GRATIA_BENCH_DRAWS", "4000"))
burnin <- as.integer(Sys.getenv("GRATIA_BENCH_BURNIN", "1000"))
set.seed(281)
d <- data.frame(x = runif(n_obs), z = runif(n_obs), off = runif(n_obs, -0.2, 0.2))
eta <- 0.7 * sin(2 * pi * d$x) + d$off
if (case == "sparse") eta <- eta - 6 * (d$x > 0.4 & d$x < 0.8)
family <- switch(case,
  poisson = poisson(), binomial = binomial(), sparse = binomial(),
  multinomial = multinom(K = 2), gaulss = gaulss(), cnorm = cnorm(),
  tweedie = tw(), grouped = gfam(list(binomial(), poisson())))
d$y <- switch(case,
  poisson = rpois(n_obs, exp(eta)),
  binomial = rbinom(n_obs, 1, plogis(eta)),
  sparse = rbinom(n_obs, 1, plogis(eta)),
  gaulss = rnorm(n_obs, eta, exp(d$z / 3)),
  tweedie = rTweedie(exp(eta), p = 1.5, phi = 0.5),
  multinomial = {
    probabilities <- cbind(1, exp(eta), exp(-eta))
    apply(probabilities, 1, function(p) sample.int(3L, 1L, prob = p) - 1L)
  },
  cnorm = {
    raw <- rnorm(n_obs, eta, 0.7)
    cbind(pmax(raw, 0), ifelse(raw < 0, -Inf, raw))
  },
  grouped = {
    index <- rep(1:2, length.out = n_obs)
    y <- ifelse(index == 1, rbinom(n_obs, 1, plogis(eta)), rpois(n_obs, exp(eta)))
    cbind(y, index)
  })
formula <- if (case %in% c("multinomial", "gaulss")) {
  list(y ~ s(x, k = k) + offset(off), ~s(z, k = k))
} else y ~ s(x, k = k) + offset(off)
model <- gam(formula, family = family, data = d, method = "REML")
# This times the public operation: family/gratia setup is prepared once for all
# four chains. mgcv includes its own setup in each chain. Diagnostics are timed
# separately so their common cost does not obscure sampler differences.
elapsed <- system.time({
  b <- post_draws(model, n_draws = n_draws, chains = 4, method = "mh",
    mh_backend = backend, burnin = burnin, seed = seed)
})[["elapsed"]]
diagnostic_seconds <- system.time({
  summary <- summarise_draws(as_mcmc_draws(b))
})[["elapsed"]]
# A supplied matrix reuses the same coefficient draws for both transformations.
nd <- d[seq_len(min(200L, n_obs)), , drop = FALSE]
quantity_time <- function(fun) {
  warnings <- character()
  started <- proc.time()[["elapsed"]]
  value <- tryCatch(withCallingHandlers(fun(), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = identity)
  seconds <- proc.time()[["elapsed"]] - started
  if (inherits(value, "error")) return(list(seconds = NA_real_, note = conditionMessage(value)))
  if (is.null(value)) return(list(seconds = NA_real_, note = paste(warnings, collapse = "; ")))
  list(seconds = seconds, note = paste(warnings, collapse = "; "))
}
fitted <- quantity_time(function() fitted_samples(model, data = nd,
  method = "user", draws = b))
posterior <- quantity_time(function() posterior_samples(model, data = nd,
  method = "user", draws = b, seed = 28))
row <- data.frame(family = case, observations = n_obs, coefficients = length(coef(model)),
  backend = backend, seed = seed, draws = n_draws, chains = 4L,
  burnin_per_chain = burnin, setup_and_sampling_seconds = elapsed,
  min_ess_bulk = min(summary$ess_bulk), min_ess_tail = min(summary$ess_tail),
  max_rhat = max(summary$rhat), bulk_ess_per_second = min(summary$ess_bulk) / elapsed,
  tail_ess_per_second = min(summary$ess_tail) / elapsed,
  diagnostics_seconds = diagnostic_seconds, fitted_seconds = fitted$seconds,
  posterior_seconds = posterior$seconds, fitted_note = fitted$note, posterior_note = posterior$note)
write.csv(row, output, row.names = FALSE)
write.csv(as.data.frame(summary), paste0(output, ".coefficients.csv"), row.names = FALSE)
