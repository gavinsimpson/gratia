pkgload::load_all(quiet = TRUE)
library("mgcv")
library("gratia")
set.seed(1907)
d <- data.frame(x = seq(0, 1, length.out = 250))
d$y <- sin(6 * d$x) + rnorm(nrow(d), sd = .25)
settings <- expand.grid(basis = c("cr", "tp"), k = c(6L, 12L),
  n = c(100L, 1000L), quantity = c("smooth", "derivative2"), stringsAsFactors = FALSE)
results <- lapply(seq_len(nrow(settings)), function(i) {
  s <- settings[i, ]
  model <- gam(y ~ s(x, bs = s$basis, k = s$k), data = d, method = "REML")
  times <- lapply(c("simulation", "tube"), function(method) {
    elapsed <- replicate(3L, {
      gc()
      unname(system.time({
        if (s$quantity == "smooth") {
          band <- smooth_estimates(model, n = s$n, interval = "simultaneous",
            simultaneous_method = method, n_sim = 10000L, seed = 41)
        } else {
          set.seed(41)
          band <- derivatives(model, n = s$n, order = 2, type = "central",
            interval = "simultaneous", simultaneous_method = method, n_sim = 10000L)
        }
      })["elapsed"])
    })
    data.frame(s, method = method, median_seconds = median(elapsed))
  })
  do.call(rbind, times)
})
results <- do.call(rbind, results)
write.csv(results, "notes/tube-bands/timings.csv", row.names = FALSE)
print(results, row.names = FALSE)
