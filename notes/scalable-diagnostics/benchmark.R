# Run from the package root after pkgload::load_all(). Fitting is excluded.
# Timings are descriptive, not CI thresholds. QQ references use method='normal'
# so their sorting cost is measured separately from drawing and simulation.
# Results: notes/scalable-diagnostics/benchmark-results.csv
library("mgcv")
library("ggplot2")
set.seed(1210)
rows <- list()
for (n in c(10000L, 100000L, 1000000L)) {
  dat <- data.frame(x = runif(n))
  dat$y <- sin(6 * dat$x) + rnorm(n, sd = 0.3)
  model <- bam(y ~ s(x, k = 10), data = dat, discrete = TRUE)
  reference_seconds <- unname(system.time(gratia:::qq_normal(model))["elapsed"])
  for (scalable in c(FALSE, TRUE)) {
    gc()
    construction <- system.time(
      p <- appraise(
        model,
        method = "normal",
        scalable = scalable,
        qq_bins = 1000L,
        density_bins = c(50L, 50L)
      )
    )["elapsed"]
    path <- tempfile(fileext = ".png")
    rendering <- system.time({
      grDevices::png(path, width = 1200, height = 1000, res = 120)
      tryCatch(print(p), finally = grDevices::dev.off())
    })["elapsed"]
    rows[[length(rows) + 1L]] <- data.frame(
      n = n,
      scalable = scalable,
      reference_seconds = reference_seconds,
      construction_seconds = unname(construction),
      rendering_seconds = unname(rendering),
      plot_bytes = as.numeric(object.size(p)),
      qq_rows = nrow(p[[1]]$data),
      residual_rows = nrow(p[[2]]$data),
      histogram_rows = nrow(p[[3]]$data),
      observed_rows = nrow(p[[4]]$data)
    )
    if (n == 10000L && scalable) {
      file.copy(
        path,
        "/private/tmp/gratia-scalable-preview.png",
        overwrite = TRUE
      )
    }
    unlink(path)
    rm(p)
  }
}
write.csv(
  do.call(rbind, rows),
  "notes/scalable-diagnostics/benchmark-results.csv",
  row.names = FALSE
)
print(do.call(rbind, rows))
print(sessionInfo())
