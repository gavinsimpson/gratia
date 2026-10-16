library("gratia")
library("mgcv")
ns <- asNamespace("gratia")
set.seed(281)
d <- data.frame(x = runif(10000))
d$y <- rpois(nrow(d), exp(sin(2 * pi * d$x)))
m <- bam(y ~ s(x, k = 20), data = d, family = poisson(), discrete = TRUE)
rows <- list()
for (dense in c(TRUE, FALSE)) {
  elapsed <- system.time(target <- ns$sampling_target(m,
    dense_limit = if (dense) 5e6 else 0, block_size = 2000, compiled = TRUE))[["elapsed"]]
  evaluate <- system.time(for (i in 1:20) {
    value <- ns$sampling_target_eval(target, coef(m) + 0.001 * i, gradient = TRUE)
  })[["elapsed"]]
  rows[[length(rows) + 1L]] <- data.frame(dense = dense, setup_seconds = elapsed,
    evaluations_seconds = evaluate, target_bytes = as.numeric(object.size(target)),
    final_value = value$value)
}
write.csv(do.call(rbind, rows), "notes/posterior-chains/block.csv", row.names = FALSE)
