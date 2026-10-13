# Run from the package root. Fitting and QQ calculations are excluded so this
# measures conditional-density estimation and drawing independently.
pkgload::load_all()
library("ggplot2")
set.seed(1310)
rows <- list()
for (n in c(10000L, 100000L, 1000000L)) {
  eta <- runif(n, -1, 2)
  mu <- exp(eta)
  y <- rpois(n, mu)
  residuals <- (y - mu) / sqrt(mu)
  for (method in c("histogram", "kde")) {
    for (iteration in seq_len(3L)) {
      gc()
      elapsed <- system.time({
        df <- if (method == "kde") {
          gratia:::diagnostic_density_grid(eta, residuals, c(50L, 50L))
        } else gratia:::diagnostic_grid(eta, residuals, c(50L, 50L), "conditional")
      })[["elapsed"]]
      stopifnot(sum(df$count) == n, nrow(df) <= 2500L)
      p <- gratia:::render_diagnostic_grid(df, "conditional", "Linear predictor",
        "Pearson residuals", method, NULL, NULL, intercept = 0,
        fill_transform = if (method == "kde") "sqrt" else "identity")
      path <- tempfile(fileext = ".png")
      drawing <- system.time({
        grDevices::png(path, width = 1000, height = 800, res = 120)
        tryCatch(print(p), finally = grDevices::dev.off())
      })[["elapsed"]]
      unlink(path)
      rows[[length(rows) + 1L]] <- data.frame(n, method, iteration,
        estimation_seconds = elapsed, drawing_seconds = drawing,
        plot_bytes = as.numeric(object.size(p)), plotting_rows = nrow(df))
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, "notes/scalable-diagnostics/density-benchmark-results.csv",
  row.names = FALSE)
print(aggregate(cbind(estimation_seconds, drawing_seconds, plot_bytes, plotting_rows) ~
  n + method, results, median))
print(sessionInfo())
