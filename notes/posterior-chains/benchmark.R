# Run through run-benchmarks.R to record maximum resident memory per process.
args <- commandArgs(TRUE)
if (length(args) != 5L) stop("Supply family, observation count, basis size, method, and output path.")
library("gratia")
library("mgcv")
library("posterior")
family_name <- args[1]
n_obs <- as.integer(args[2])
k <- as.integer(args[3])
method <- args[4]
n <- as.integer(Sys.getenv("GRATIA_BENCH_DRAWS", "1000"))
burnin <- as.integer(Sys.getenv("GRATIA_BENCH_BURNIN", "250"))
set.seed(281)
d <- data.frame(x = runif(n_obs), offset = runif(n_obs, -0.3, 0.3))
eta <- 0.7 * sin(2 * pi * d$x) + d$offset
fam <- switch(family_name,
  gaussian = gaussian(), poisson = poisson(), binomial = binomial(),
  sparse = binomial(), gamma = Gamma(link = "log"), nb = nb())
if (family_name == "sparse") eta <- eta - 6 * (d$x > 0.4 & d$x < 0.8)
d$y <- switch(family_name,
  gaussian = rnorm(n_obs, eta, 0.7), poisson = rpois(n_obs, exp(eta)),
  binomial = rbinom(n_obs, 1, plogis(eta)), sparse = rbinom(n_obs, 1, plogis(eta)),
  gamma = rgamma(n_obs, 3, scale = exp(eta) / 3), nb = rnbinom(n_obs, mu = exp(eta), size = 2))
m <- gam(y ~ s(x, k = k) + offset(offset), data = d, family = fam, method = "REML")
ns <- asNamespace("gratia")
setup <- system.time({
  target <- if (method != "mgcv") ns$sampling_target(m,
    compiled = method != "mh_r") else NULL
})[["elapsed"]]
time <- system.time({
  if (method == "mgcv") {
    b <- post_draws(m, n_draws = n, chains = 4, seed = 281, method = "mh",
      burnin = burnin, mh_backend = if (method == "mgcv") "mgcv" else "gratia",
      n_cores = if (method == "parallel") 4L else 1L)
  } else {
    pieces <- lapply(1:4, function(chain) withr::with_seed(281 + chain, {
      switch(method,
        mh_r = ns$sampling_mh(target, n / 4, burnin, 1, 40, 0.25)$bs,
        mh = ns$sampling_mh(target, n / 4, burnin, 1, 40, 0.25)$bs,
        mala = ns$sampling_mala(target, n / 4, burnin)$bs,
        elliptical = ns$sampling_elliptical(target, n / 4, burnin)$bs)
    }))
    b <- ns$sampling_finish(do.call(rbind, pieces), ns$sampling_layout(n, 4),
      coefficient_names = names(coef(m)))
  }
})[["elapsed"]]
summary <- summarise_draws(as_mcmc_draws(b))
# Include representative end-user transformations separately from sampling.
prediction <- system.time(fitted_samples(m, data = d[seq_len(min(200, n_obs)), ],
  method = "user", draws = b))[["elapsed"]]
response <- system.time(posterior_samples(m, data = d[seq_len(min(200, n_obs)), ],
  method = "user", draws = b, seed = 28))[["elapsed"]]
write.csv(data.frame(family = family_name, n_obs = n_obs, coefficients = length(coef(m)),
  method = method, draws = n, burnin_per_chain = burnin, setup_seconds = setup,
  sampling_seconds = time, total_seconds = setup + time, min_ess_bulk = min(summary$ess_bulk),
  min_ess_tail = min(summary$ess_tail), max_rhat = max(summary$rhat),
  bulk_ess_per_second = min(summary$ess_bulk) / (setup + time),
  tail_ess_per_second = min(summary$ess_tail) / (setup + time),
  fitted_seconds = prediction, posterior_seconds = response), args[5], row.names = FALSE)
