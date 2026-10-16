# Run after installation, with R_LIBS set before starting R.
# Sampling and diagnostics are separate from transformation timings.
library("gratia")
library("mgcv")
source("notes/posterior-chains/lss-fixtures.R")
n_draws <- 1000L
rows <- list()
for (name in c("gaulss", "gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")) {
  fixture <- lss_benchmark_fixture(name)
  m <- fixture$model
  nd <- fixture$data
  n <- NROW(m$model)
  for (seed in 281:283) {
    b <- post_draws(m, n_draws = n_draws, chains = 4, seed = seed)
    time <- function(fun) {
      gc()
      started <- proc.time()[["elapsed"]]
      warnings <- character()
      value <- tryCatch(withCallingHandlers(fun(), warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }), error = identity)
      seconds <- proc.time()[["elapsed"]] - started
      if (inherits(value, "error")) return(list(seconds = NA_real_,
        note = conditionMessage(value)))
      list(seconds = seconds, note = paste(warnings, collapse = "; "))
    }
    fitted <- time(function() fitted_samples(m, data = nd, method = "user", draws = b))
    response <- time(function() posterior_samples(m, data = nd, method = "user", draws = b, seed = seed))
    fixed <- time(function() predicted_samples(m, data = nd, n_draws = n_draws, seed = seed))
    rows[[length(rows) + 1L]] <- data.frame(family = name, seed = seed,
      fitting_rows = n, coefficients = length(coef(m)), evaluation_rows = nrow(nd),
      draws = n_draws, chains = 4L, fitted_seconds = fitted$seconds,
      posterior_seconds = response$seconds, predicted_seconds = fixed$seconds,
      fitted_note = fitted$note, posterior_note = response$note, predicted_note = fixed$note)
    cat(name, seed, fitted$seconds, response$seconds, fixed$seconds, "\n")
    flush.console()
  }
}
write.csv(do.call(rbind, rows), "notes/posterior-chains/lss-prediction-benchmarks.csv", row.names = FALSE)
writeLines(trimws(c(paste("mgcv", packageVersion("mgcv")), capture.output(sessionInfo())), which = "right"),
  "notes/posterior-chains/lss-benchmark-environment.txt")
