# Run one conditional-posterior comparison in a fresh R process.
args <- commandArgs(TRUE)
stopifnot(length(args) == 4L)
library("gratia")
library("mgcv")
library("posterior")
case <- args[1L]
method <- args[2L]
seed <- as.integer(args[3L])
output <- args[4L]
n_draws <- as.integer(Sys.getenv("GRATIA_DIFFICULT_DRAWS", "16000"))
burnin <- as.integer(Sys.getenv("GRATIA_DIFFICULT_BURNIN", "4000"))
chains <- 4L
stopifnot(n_draws %% chains == 0L)
ns <- asNamespace("gratia")
# The sparse case reproduces the data construction in family-benchmark.R.
set.seed(281)
n_obs <- if (case == "sparse") 2000L else 1500L
k <- switch(case, sparse = 30L, rare_poisson = 25L, sparse_high_p = 50L)
d <- data.frame(x = runif(n_obs), z = runif(n_obs), off = runif(n_obs, -0.2, 0.2))
eta <- 0.7 * sin(2 * pi * d$x) + d$off
if (case == "rare_poisson") {
  eta <- eta - 3.5 - 4 * (d$x > 0.4 & d$x < 0.8)
  d$y <- rpois(n_obs, exp(eta))
  fam <- poisson()
} else {
  eta <- eta - 6 * (d$x > 0.4 & d$x < 0.8)
  d$y <- rbinom(n_obs, 1, plogis(eta))
  fam <- binomial()
}
fit_seconds <- system.time({
  model <- gam(y ~ s(x, k = k) + offset(off), family = fam, data = d, method = "REML")
})[["elapsed"]]
setup_seconds <- system.time({
  target <- switch(method,
    mgcv = NULL,
    family = ns$sampling_family_target(model),
    ns$sampling_target(model, compiled = TRUE))
})[["elapsed"]]
sampling_seconds <- system.time({
  results <- withr::with_seed(seed, {
    streams <- ns$sampling_streams(chains)
    lapply(seq_len(chains), function(i) {
      task <- list(chain = i, stream = streams[[i]])
      if (method %in% c("mgcv", "gratia", "family")) {
        return(ns$sampling_mh_worker(task, if (method == "mgcv") model else NULL,
          target, n_draws / chains, burnin, 1L, 40, 0.25, dispersed = TRUE))
      }
      withr::with_preserve_seed({
        assign(".Random.seed", streams[[i]], .GlobalEnv)
        result <- switch(method,
          mala = ns$sampling_mala(target, n_draws / chains, burnin),
          elliptical = ns$sampling_elliptical(target, n_draws / chains, burnin))
        list(bs = result$bs, info = data.frame(.chain = i,
          fixed_acceptance = if (method == "mala") result$accept else NA_real_,
          rw_acceptance = NA_real_,
          step_size = if (method == "mala") result$step_size else NA_real_,
          evaluations = if (method == "elliptical") result$evaluations else NA_real_))
      })
    })
  })
  b <- ns$sampling_finish(do.call(rbind, lapply(results, `[[`, "bs")),
    ns$sampling_layout(n_draws, chains), coefficient_names = names(coef(model)))
})[["elapsed"]]
ids <- attr(b, "sampling")$draw_ids
stopifnot(nrow(b) == n_draws, identical(ids$.draw, seq_len(n_draws)),
  all(table(ids$.chain) == n_draws / chains))
# Record user-facing costs at a common supplied grid. Keep diagnostics outside
# the sampling timer, and use exactly the same coefficient draws throughout.
nd <- data.frame(x = seq(0.05, 0.95, length.out = 21L), z = 0.5, off = 0)
fitted_seconds <- system.time({
  fitted <- fitted_samples(model, data = nd, method = "user", draws = b)
})[["elapsed"]]
derivative_seconds <- system.time({
  derivative <- derivative_samples(model, data = nd, focal = "x", type = "central",
    eps = 1e-5, scale = "response", method = "user", draws = b)
})[["elapsed"]]
response_seconds <- system.time({
  response <- posterior_samples(model, data = nd, method = "user", draws = b, seed = 28)
})[["elapsed"]]
stopifnot(identical(sort(unique(fitted$.draw)), ids$.draw),
  identical(sort(unique(response$.draw)), ids$.draw),
  all(table(fitted$.chain) == n_draws / chains * nrow(nd)))
# Sorting makes the transformation independent of tidy output row ordering.
fm <- matrix(fitted$.fitted[order(fitted$.row, fitted$.draw)], nrow = n_draws)
dm <- matrix(derivative$.derivative[order(derivative$.row, derivative$.draw)], nrow = n_draws)
# Check both transformations against direct use of the fitted bases, including
# the response-scale chain rule. These do not reuse the public sampling helpers.
X <- predict(model, newdata = nd, type = "lpmatrix")
linear <- X %*% t(b)
expected <- fam$linkinv(linear)
upper <- lower <- nd
upper$x <- upper$x + 1e-5
lower$x <- lower$x - 1e-5
D <- (predict(model, newdata = upper, type = "lpmatrix") -
  predict(model, newdata = lower, type = "lpmatrix")) / 2e-5
independent_derivative <- fam$mu.eta(linear) * (D %*% t(b))
# Public derivatives use finite differences of the expected response itself.
stopifnot(max(abs(t(fm) - expected)) < 1e-10,
  max(abs(t(dm) - independent_derivative)) < 1e-4)
quantities <- cbind(
  expected_low = fm[, 4L], expected_sparse = fm[, 14L], expected_high = fm[, 19L],
  expected_average = rowMeans(fm),
  contrast_sparse_low = fm[, 14L] - fm[, 4L],
  contrast_high_low = fm[, 19L] - fm[, 4L],
  derivative_low = dm[, 4L], derivative_sparse = dm[, 14L],
  derivative_high = dm[, 19L])
quantity_draws <- ns$sampling_finish(quantities, ids)
diagnostics_seconds <- system.time({
  coefficient_summary <- summarise_draws(as_mcmc_draws(b),
    "mean", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean", "mcse_sd")
  quantity_summary <- summarise_draws(as_mcmc_draws(quantity_draws),
    "mean", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean", "mcse_sd")
})[["elapsed"]]
total <- setup_seconds + sampling_seconds
summarise_group <- function(summary, group) {
  data.frame(group = group, min_ess_bulk = min(summary$ess_bulk),
    min_ess_tail = min(summary$ess_tail), max_rhat = max(summary$rhat),
    max_mcse_over_sd = max(summary$mcse_mean / summary$sd),
    bulk_ess_per_second = min(summary$ess_bulk) / total,
    tail_ess_per_second = min(summary$ess_tail) / total)
}
row <- cbind(data.frame(case = case, observations = n_obs, events = sum(d$y),
  coefficients = length(coef(model)), method = method, seed = seed,
  draws = n_draws, chains = chains, burnin_per_chain = burnin,
  fit_seconds = fit_seconds, setup_seconds = setup_seconds,
  sampling_seconds = sampling_seconds, total_seconds = total,
  fitted_seconds = fitted_seconds, derivative_seconds = derivative_seconds,
  response_seconds = response_seconds, diagnostics_seconds = diagnostics_seconds),
  rbind(summarise_group(coefficient_summary, "coefficients"),
    summarise_group(quantity_summary, "quantities")))
write.csv(row, output, row.names = FALSE)
summary <- rbind(cbind(as.data.frame(coefficient_summary), group = "coefficients"),
  cbind(as.data.frame(quantity_summary), group = "quantities"))
write.csv(summary, paste0(output, ".diagnostics.csv"), row.names = FALSE)
info <- do.call(rbind, lapply(results, `[[`, "info"))
write.csv(info, paste0(output, ".chains.csv"), row.names = FALSE)
