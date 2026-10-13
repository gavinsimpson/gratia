# Run from the repository root:
# Rscript notes/inverse-gaussian/benchmark-appraise.R
# Timings include actual printing to a 1200 x 1000 Cairo PNG device. Package
# loading, simulation of the fitting data and GAM fitting are recorded separately.
pkgload::load_all(quiet = TRUE)
output <- file.path("notes", "inverse-gaussian", "appraise-benchmark")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
writeLines(c(capture.output(sessionInfo()), capture.output(Sys.info())),
  file.path(output, "session-info.txt"))

sizes <- c(1000L, 10000L, 50000L)
repetitions <- 5L
set.seed(609)
d <- data.frame(x0 = runif(max(sizes)), x1 = runif(max(sizes)),
  x2 = runif(max(sizes)), x3 = runif(max(sizes)))
d$wt <- exp(.8 * (d$x0 - .5))
eta <- .2 + sin(2 * pi * d$x0) + .4 * cos(2 * pi * d$x1) +
  .8 * (d$x2 - .5)^2 - .4 * d$x3
d$y <- rd_invgaussian(exp(eta), d$wt, .4)

elapsed <- function() unname(proc.time()[["elapsed"]])
render <- function(p, path) {
  grDevices::png(path, width = 1200, height = 1000, res = 144, type = "cairo")
  on.exit(grDevices::dev.off())
  print(p)
  invisible(NULL)
}
configurations <- list(
  default = list(),
  uniform_1 = list(method = "uniform", n_uniform = 1L),
  simulate_50 = list(method = "simulate", n_simulate = 50L),
  normal = list(method = "normal")
)
timings <- fits <- helper_timings <- list()
models <- list()
temporary_plot <- tempfile(fileext = ".png")

for (n in sizes) {
  cat("Fitting", n, "observations\n"); flush.console()
  data <- d[seq_len(n), ]
  invisible(gc())
  start <- elapsed()
  m <- mgcv::gam(y ~ s(x0, k = 10) + s(x1, k = 10) + s(x2, k = 10) +
      s(x3, k = 10), data = data, weights = wt,
    family = stats::inverse.gaussian(link = "log"), method = "REML")
  fits[[length(fits) + 1L]] <- data.frame(n, seconds = elapsed() - start,
    edf = sum(m$edf), dispersion = m$sig2,
    fitted_min = min(fitted(m)), fitted_max = max(fitted(m)))
  models[[as.character(n)]] <- m
  create <- function(configuration) {
    do.call(appraise, c(list(model = m, seed = 131), configurations[[configuration]]))
  }

  # First graphics use after package loading/fitting, before any warm-up.
  if (n == sizes[1L]) {
    start <- elapsed()
    p <- create("default")
    constructed <- elapsed()
    render(p, file.path(output, "default-1000.png"))
    finished <- elapsed()
    utils::write.csv(data.frame(n, construct = constructed - start,
      render = finished - constructed, total = finished - start),
      file.path(output, "first-call.csv"), row.names = FALSE)
  }
  # Warm every path, including the graphics device, before repeated measurement.
  for (configuration in names(configurations)) render(create(configuration), temporary_plot)
  for (iteration in seq_len(repetitions)) {
    order <- withr::with_seed(200 + iteration, sample(names(configurations)))
    for (configuration in order) {
      invisible(gc())
      start <- elapsed()
      p <- create(configuration)
      constructed <- elapsed()
      render(p, temporary_plot)
      finished <- elapsed()
      timings[[length(timings) + 1L]] <- data.frame(n, configuration, iteration,
        construct = constructed - start, render = finished - constructed,
        total = finished - start)
    }
  }
  mu <- fitted(m)
  wt <- m$prior.weights
  phi <- m$sig2
  probabilities <- withr::with_seed(131, sample((seq_len(n) - .5) / n))
  helpers <- list(
    qf = function() qf_invgaussian(probabilities, mu, wt, phi),
    cdf_both_log_tails = function() {
      cdf_invgaussian(m$y, mu, wt, phi, log_p = TRUE)
      cdf_invgaussian(m$y, mu, wt, phi, log_p = TRUE, lower_tail = FALSE)
    },
    rd = function() rd_invgaussian(mu, wt, phi),
    qq_uniform_10 = function() withr::with_seed(131, qq_uniform(m, n = 10)),
    qq_simulate_50 = function() withr::with_seed(131, qq_simulate(m, n = 50)),
    quantile_residuals = function() quantile_residuals(m, type = "quantile")
  )
  for (helper in names(helpers)) {
    for (iteration in seq_len(repetitions)) {
      inner <- if (helper %in% c("qf", "cdf_both_log_tails", "rd")) 10L else 1L
      invisible(gc())
      start <- elapsed()
      for (i in seq_len(inner)) invisible(helpers[[helper]]())
      helper_timings[[length(helper_timings) + 1L]] <- data.frame(n, helper,
        iteration, seconds = (elapsed() - start) / inner)
    }
  }
  cat("Finished", n, "observations\n"); flush.console()
  utils::write.csv(do.call(rbind, timings), file.path(output, "timings.csv"),
    row.names = FALSE)
  utils::write.csv(do.call(rbind, fits), file.path(output, "fits.csv"), row.names = FALSE)
  utils::write.csv(do.call(rbind, helper_timings), file.path(output, "helpers.csv"),
    row.names = FALSE)
  if (n == 10000L) {
    writeLines(capture.output(summary(m)), file.path(output, "model-summary.txt"))
    render(create("default"), file.path(output, "default-10000.png"))
  }
}

timings <- do.call(rbind, timings)
medians <- aggregate(cbind(construct, render, total) ~ n + configuration,
  data = timings, FUN = median)
utils::write.csv(medians, file.path(output, "medians.csv"), row.names = FALSE)
ranges <- aggregate(total ~ n + configuration, data = timings,
  FUN = function(x) c(min = min(x), max = max(x)))
utils::write.csv(ranges, file.path(output, "ranges.csv"), row.names = FALSE)
helper_medians <- aggregate(seconds ~ n + helper,
  data = do.call(rbind, helper_timings), FUN = median)
utils::write.csv(helper_medians, file.path(output, "helper-medians.csv"),
  row.names = FALSE)
print(medians)
print(helper_medians)

# Sample CPU profiles separately from timing runs; profiling adds overhead.
profile <- function(label, iterations, fun) {
  path <- file.path(output, paste0(label, ".Rprof"))
  utils::Rprof(path, interval = .001)
  tryCatch(for (i in seq_len(iterations)) invisible(fun()),
    finally = utils::Rprof(NULL))
  summary <- utils::summaryRprof(path)
  utils::write.csv(summary$by.total, file.path(output, paste0(label, "-total.csv")))
  utils::write.csv(summary$by.self, file.path(output, paste0(label, "-self.csv")))
  writeLines(c(paste("Samples:", summary$sampling.time / summary$sample.interval),
    capture.output(head(summary$by.total, 25)),
    capture.output(head(summary$by.self, 25))),
    file.path(output, paste0(label, "-summary.txt")))
}
m <- models[["10000"]]
cat("Profiling 10,000-observation model\n"); flush.console()
profile("default-construct", 60L, function() appraise(m, seed = 131))
profile("default-total", 15L, function() render(appraise(m, seed = 131), temporary_plot))
profile("simulate-construct", 12L,
  function() appraise(m, method = "simulate", n_simulate = 50, seed = 131))
mu <- fitted(m); wt <- m$prior.weights; phi <- m$sig2
probabilities <- withr::with_seed(131, sample((seq_along(mu) - .5) / length(mu)))
profile("qf", 300L, function() qf_invgaussian(probabilities, mu, wt, phi))
profile("rd", 1000L, function() rd_invgaussian(mu, wt, phi))
unlink(temporary_plot)
cat("Benchmark complete\n")
