# Supplement to benchmark-appraise.R. Compare the optimized production function
# with a private copy that restores legacy quantile names.
pkgload::load_all(quiet = TRUE)
output <- file.path("notes", "inverse-gaussian", "appraise-benchmark")
set.seed(609)
n <- 50000L
d <- data.frame(x0 = runif(n), x1 = runif(n), x2 = runif(n), x3 = runif(n))
d$wt <- exp(.8 * (d$x0 - .5))
eta <- .2 + sin(2 * pi * d$x0) + .4 * cos(2 * pi * d$x1) +
  .8 * (d$x2 - .5)^2 - .4 * d$x3
d$y <- rd_invgaussian(exp(eta), d$wt, .4)
d <- d[seq_len(10000L), ]
m <- mgcv::gam(y ~ s(x0, k = 10) + s(x1, k = 10) + s(x2, k = 10) +
    s(x3, k = 10), data = d, weights = wt,
  family = stats::inverse.gaussian(link = "log"), method = "REML")

# Restore names only in the private legacy closure. The production function
# uses names=FALSE; no namespace bindings or source files are modified here.
legacy_quantile <- function(x, probs, names = TRUE, ...) {
  stats::quantile(x, probs, names = TRUE, ...)
}
legacy_env <- new.env(parent = environment(qq_simulate))
legacy_env$quantile <- legacy_quantile
legacy <- qq_simulate
environment(legacy) <- legacy_env
regular <- function() withr::with_seed(131, legacy(m, n = 50))
plain <- function() withr::with_seed(131, qq_simulate(m, n = 50))
a <- regular(); b <- plain()
stopifnot(identical(unname(a$theoretical), unname(b$theoretical)),
  identical(unname(a$lower), unname(b$lower)),
  identical(unname(a$upper), unname(b$upper)),
  identical(unname(a$residuals), unname(b$residuals)))
results <- list()
for (iteration in seq_len(10L)) {
  for (version in withr::with_seed(400 + iteration, sample(c("original", "names_false")))) {
    invisible(gc())
    start <- proc.time()[["elapsed"]]
    if (version == "original") invisible(regular()) else invisible(plain())
    results[[length(results) + 1L]] <- data.frame(n = nrow(d), iteration, version,
      seconds = proc.time()[["elapsed"]] - start)
  }
}
results <- do.call(rbind, results)
utils::write.csv(results, file.path(output, "interval-names-integrated-timings.csv"), row.names = FALSE)
summary <- aggregate(seconds ~ version, results, median)
utils::write.csv(summary, file.path(output, "interval-names-integrated-medians.csv"), row.names = FALSE)
print(summary)
cat("QQ numerical values are bitwise identical; only names differ.\n")
