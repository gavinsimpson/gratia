# Run after installation, with R_LIBS set before starting R.
# Sampling and diagnostics are separate from transformation timings.
library("gratia")
library("mgcv")
n_draws <- 1000L
rows <- list()
for (name in c("gaulss", "gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")) {
  set.seed(281)
  n <- 2000L
  d <- data.frame(x = runif(n), o1 = runif(n, -.1, .1), o2 = runif(n, -.1, .1),
    o3 = runif(n, -.05, .05), o4 = runif(n, -.05, .05))
  f <- get(name, asNamespace("mgcv"))()
  beta <- switch(name,
    gaulss = c(.4, .3, -.4, .2), gammals = c(.4, .3, -1, .2),
    gumbls = c(.4, .3, -1, .2), gevlss = c(.4, .3, -1, .2, -.5, .1),
    shash = c(.4, .3, -1, .2, .4, .1, .1, .1),
    twlss = c(.4, .3, .1, .2, -.4, .1), ziplss = c(.4, .3, -.1, .2))
  eta <- do.call(cbind, lapply(seq_len(f$nlp), function(j)
    beta[2 * j - 1] + beta[2 * j] * d$x + if (name == "shash") 0 else d[[paste0("o", j)]]))
  f <- gratia:::fix_family_rd(f)
  d$y <- f$rd(gratia:::family_prediction_parameters(f, eta), rep(1, n), 1)
  formulas <- list(y ~ x + offset(o1), ~x + offset(o2), ~x + offset(o3),
    ~x + offset(o4))[seq_len(f$nlp)]
  if (name == "shash") formulas <- list(y ~ x, ~x, ~x, ~x)
  m <- withCallingHandlers(gam(formulas, family = f, data = d, method = "REML"),
    warning = function(w) {
      if (name == "gevlss" && conditionMessage(w) == "NaNs produced")
        invokeRestart("muffleWarning")
    })
  # Genuinely new covariates and formula offsets; no observed response.
  nd <- d[1:200, setdiff(names(d), "y")]
  nd$x <- seq(.001, .999, length.out = 200)
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
