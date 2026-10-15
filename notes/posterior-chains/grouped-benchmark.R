# Run from the package root after installing the development package.
# Keep BLAS thread counts fixed across all benchmark runs.
library("gratia")
library("mgcv")
set.seed(281)
n <- 2000L
train <- data.frame(x = runif(n), index = rep(1:3, length.out = n),
  off = rep(c(-0.2, 0.1, 0.3), length.out = n),
  weight = rep(c(3, 2, 1), length.out = n))
train$group <- factor(train$index)
eta <- 0.2 + sin(4 * train$x) / 2 + train$off
train$y <- numeric(n)
for (i in 1:3) {
  rows <- which(train$index == i)
  train$y[rows] <- switch(i,
    rbinom(length(rows), train$weight[rows], plogis(eta[rows])) / train$weight[rows],
    rnorm(length(rows), eta[rows], 0.4 / sqrt(train$weight[rows])),
    rpois(length(rows), exp(eta[rows])))
}
models <- list(
  gam = gam(cbind(y, index) ~ group + s(x, k = 8) + offset(off),
    family = gfam(list(binomial(), gaussian(), poisson())), weights = weight,
    data = train, method = "REML", optimizer = c("outer", "bfgs")),
  bam = bam(cbind(y, index) ~ group + s(x, k = 8) + offset(off),
    family = gfam(list(binomial(), gaussian(), poisson())), weights = weight,
    data = train, method = "fREML", discrete = TRUE))
nd <- train[1:200, setdiff(names(train), "y")]
nd$x <- seq(0.01, 0.99, length.out = NROW(nd))
measure <- function(fun) {
  gc()
  started <- proc.time()[["elapsed"]]
  warnings <- character()
  value <- tryCatch(withCallingHandlers(fun(), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = identity)
  if (inherits(value, "error")) return(list(seconds = NA_real_, note = conditionMessage(value)))
  list(seconds = proc.time()[["elapsed"]] - started, note = paste(warnings, collapse = "; "))
}
results <- list()
for (name in names(models)) for (seed in 281:283) {
  model <- models[[name]]
  draws <- post_draws(model, n_draws = 1000, chains = 4, seed = seed)
  fitted <- measure(function() fitted_samples(model, data = nd,
    method = "user", draws = draws))
  response <- measure(function() posterior_samples(model, data = nd,
    method = "user", draws = draws, seed = seed))
  fixed <- measure(function() predicted_samples(model, data = nd, n_draws = 1000, seed = seed))
  results[[length(results) + 1L]] <- data.frame(model = name, seed = seed,
    fitting_rows = n, coefficients = length(coef(model)), evaluation_rows = NROW(nd),
    draws = 1000L, chains = 4L, fitted_seconds = fitted$seconds,
    posterior_seconds = response$seconds, predicted_seconds = fixed$seconds,
    fitted_note = fitted$note, posterior_note = response$note, predicted_note = fixed$note)
  cat(name, seed, fitted$seconds, response$seconds, fixed$seconds, "\n")
}
write.csv(do.call(rbind, results), "notes/posterior-chains/grouped-benchmarks.csv", row.names = FALSE)
writeLines(c(paste("mgcv", packageVersion("mgcv")), capture.output(sessionInfo())),
  "notes/posterior-chains/grouped-benchmark-environment.txt")
