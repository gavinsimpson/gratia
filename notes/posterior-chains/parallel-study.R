library("gratia")
library("mgcv")
library("mirai")
set.seed(281)
d <- data.frame(x = runif(10000))
d$y <- rpois(nrow(d), exp(sin(2 * pi * d$x)))
m <- gam(y ~ s(x, k = 30), data = d, family = poisson(), method = "REML")
run <- function() {
  rows <- list()
  for (backend in c("mgcv", "gratia")) {
    for (mode in c("one", "four_serial", "four_temporary", "four_reused")) {
      if (mode == "four_reused") {
        mirai::daemons(4L)
        on.exit(mirai::daemons(0L), add = TRUE)
        # Warm each worker's namespace before timing a reused pool.
        invisible(post_draws(m, n_draws = 4, chains = 4, n_cores = 4,
          method = "mh", mh_backend = backend, burnin = 2, seed = 281))
      }
      elapsed <- system.time(b <- post_draws(m, n_draws = 1000,
        chains = if (mode == "one") 1L else 4L,
        n_cores = if (mode %in% c("one", "four_serial")) 1L else 4L,
        method = "mh", mh_backend = backend, burnin = 1000, seed = 281,
        diagnostics = TRUE))[["elapsed"]]
      ds <- sampling_diagnostics(b)$coefficients
      rows[[length(rows) + 1L]] <- data.frame(backend = backend, mode = mode,
        seconds = elapsed, max_rhat = max(ds$rhat), min_ess_bulk = min(ds$ess_bulk),
        min_ess_tail = min(ds$ess_tail))
      if (mode == "four_reused") mirai::daemons(0L)
    }
  }
  do.call(rbind, rows)
}
write.csv(run(), "notes/posterior-chains/parallel.csv", row.names = FALSE)
