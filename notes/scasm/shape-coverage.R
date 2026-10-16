# Run from the package root with Rscript notes/scasm/shape-coverage.R.
# Also run SHAPE_BOUNDARY_ONLY=1 Rscript notes/scasm/shape-coverage.R for
# exactly binding truths; these results have a separate filename prefix.
# The fixed seeds and complete replication summaries make the experiment
# reproducible. All guarantees and comparisons concern the listed grid points.
library("mgcv")
pkgload::load_all(".", quiet = TRUE)

out_dir <- file.path("notes", "scasm")
boundary_only <- identical(Sys.getenv("SHAPE_BOUNDARY_ONLY", "0"), "1")
output_path <- function(suffix) file.path(out_dir,
  paste0(if (boundary_only) "shape-coverage-boundary-" else "shape-coverage-", suffix))
level <- .95
oracle_repetitions <- as.integer(Sys.getenv("SHAPE_ORACLE_REPETITIONS", "2000"))
model_repetitions <- as.integer(Sys.getenv("SHAPE_MODEL_REPETITIONS", "400"))
bootstrap_repetitions <- 399L
set.seed(20261016)

# This independent implementation minimizes all chords through pairs of input
# points bracketing each evaluation point. It does not use the production
# lower-hull algorithm.
convex_minorant_reference <- function(y, x) {
  vapply(seq_along(x), function(i) {
    pairs <- expand.grid(a = seq_len(i), b = seq.int(i, length(x)))
    pairs <- pairs[pairs$a < pairs$b, ]
    a <- pairs$a
    b <- pairs$b
    min(c(y[i], ((x[b] - x[i]) * y[a] + (x[i] - x[a]) * y[b]) /
      (x[b] - x[a])))
  }, numeric(1))
}

reference_operator <- function(y, data, constraints) {
  if (length(constraints) != 1L) return(NULL)
  con <- constraints[[1L]]
  x <- data[[names(constraints)]]
  if (length(con) != 1L) return(NULL)
  switch(con, "m+" = sort(y), "m-" = sort(y, decreasing = TRUE),
    "c+" = convex_minorant_reference(y, x),
    "c-" = -convex_minorant_reference(-y, x), "+" = pmax(y, 0))
}

enforce <- function(estimate, lower, upper, data, constraints, interval) {
  shape_intervals(estimate = as.numeric(estimate), lower = as.numeric(lower), upper = as.numeric(upper),
    data = data, constraints = constraints, interval = interval, level = level)
}

one_result <- function(truth, estimate, lower, upper, experiment, scenario,
                       replication, source, interval, enforced) {
  covered <- lower <= truth + 1e-10 & upper >= truth - 1e-10
  data.frame(experiment, scenario, replication, source, interval, enforced,
    simultaneous = all(covered), pointwise_mean = mean(covered),
    mean_width = mean(upper - lower), maximum_width = max(upper - lower),
    mean_error = mean(estimate - truth),
    mean_squared_error = mean((estimate - truth)^2))
}

results <- list()
errors <- list()
warnings_log <- list()
bindings <- list()
pointwise_counts <- list()
bias_sums <- list()
reference_error <- 0
lost_simultaneous_events <- 0L
unordered_intervals <- 0L
maximum_width_increases <- 0L
add_result <- function(...) {
  z <- one_result(...)
  results[[length(results) + 1L]] <<- z
  key <- paste(z$experiment, z$scenario, z$source, z$interval, z$enforced,
    sep = "|")
  args <- list(...)
  covered <- args$lower <= args$truth + 1e-10 &
    args$upper >= args$truth - 1e-10
  pointwise_counts[[key]] <<- if (is.null(pointwise_counts[[key]])) {
    as.numeric(covered)
  } else pointwise_counts[[key]] + covered
  er <- args$estimate - args$truth
  bias_sums[[key]] <<- if (is.null(bias_sums[[key]])) er else bias_sums[[key]] + er
  invisible(z)
}

evaluate_pair <- function(truth, estimate, lower, upper, data, constraints,
                          experiment, scenario, replication, source, interval) {
  before <- add_result(truth = truth, estimate = estimate, lower = lower,
    upper = upper, experiment = experiment, scenario = scenario,
    replication = replication, source = source, interval = interval,
    enforced = FALSE)
  shaped <- enforce(estimate, lower, upper, data, constraints, interval)
  after <- add_result(truth = truth, estimate = shaped$.estimate,
    lower = shaped$.lower_ci, upper = shaped$.upper_ci,
    experiment = experiment, scenario = scenario, replication = replication,
    source = source, interval = interval, enforced = TRUE)
  lost_simultaneous_events <<- lost_simultaneous_events +
    as.integer(before$simultaneous && !after$simultaneous)
  unordered_intervals <<- unordered_intervals +
    as.integer(any(shaped$.lower_ci > shaped$.upper_ci + 1e-10))
  maximum_width_increases <<- maximum_width_increases +
    as.integer(max(shaped$.upper_ci - shaped$.lower_ci) >
      max(upper - lower) + 1e-10)
  for (component in if (replication == 1L) c("estimate", "lower", "upper") else character()) {
    expected <- reference_operator(get(component), data, constraints)
    if (!is.null(expected)) {
      actual <- shaped[[switch(component, estimate = ".estimate",
        lower = ".lower_ci", upper = ".upper_ci")]]
      reference_error <<- max(reference_error, max(abs(actual - expected)))
    }
  }
  invisible(NULL)
}

grid <- data.frame(x = seq(0, 1, length.out = 41L))
irregular <- data.frame(x = seq(0, 1, length.out = 41L)^1.7)
tensor <- expand.grid(x = seq(0, 1, length.out = 7L),
  z = seq(0, 1, length.out = 7L))
oracle_cases <- list(
  nearly_flat_increasing = list(data = grid, constraints = list(x = "m+"),
    truth = .02 * grid$x),
  nearly_flat_decreasing = list(data = grid, constraints = list(x = "m-"),
    truth = -.02 * grid$x),
  nearly_linear_convex = list(data = irregular,
    constraints = list(x = "c+"), truth = .6 * irregular$x + .01 * irregular$x^2),
  nearly_linear_concave = list(data = irregular,
    constraints = list(x = "c-"), truth = .6 * irregular$x - .01 * irregular$x^2),
  nearly_zero_positive = list(data = grid, constraints = list(x = "+"),
    truth = .002 + .001 * grid$x),
  increasing_convex_positive = list(data = grid,
    constraints = list(x = c("+", "m+", "c+")),
    truth = .002 + .01 * grid$x + .01 * grid$x^2),
  increasing_tensor = list(data = tensor,
    constraints = list(x = "m+", z = "m+"), truth = .02 * tensor$x + .03 * tensor$z))
if (boundary_only) {
  oracle_cases <- list(
    exactly_flat_increasing = list(data = grid, constraints = list(x = "m+"),
      truth = rep(0, nrow(grid))),
    exactly_linear_convex = list(data = irregular, constraints = list(x = "c+"),
      truth = .8 * irregular$x))
}

for (scenario in names(oracle_cases)) {
  case <- oracle_cases[[scenario]]
  sigma <- .08 + .22 * case$data$x
  n_grid <- length(case$truth)
  for (replication in seq_len(oracle_repetitions)) {
    estimate <- case$truth + rnorm(n_grid, sd = sigma)
    for (interval in c("pointwise", "simultaneous")) {
      critical <- if (interval == "pointwise") qnorm((1 + level) / 2) else
        qnorm((1 + level^(1 / n_grid)) / 2)
      evaluate_pair(case$truth, estimate, estimate - critical * sigma,
        estimate + critical * sigma, case$data, case$constraints,
        "known_gaussian", scenario, replication, "known_variance", interval)
    }
  }
  message("Known-Gaussian experiment completed: ", scenario)
}

# Vp supplies the usual empirical-Bayes Gaussian covariance. Ve supplies the
# fixed-smoothing-parameter frequentist covariance. Critical values for the
# former simultaneous band use 1,999 Gaussian coefficient draws for each fit.
model_cases <- list(
  nearly_flat_increasing = list(constraints = "m+", truth = function(x) .03 * x),
  nearly_linear_convex = list(constraints = "c+",
    truth = function(x) .8 * x + .03 * x^2),
  increasing_convex_boundary = list(constraints = c("m+", "c+"),
    truth = function(x) .03 * (x + x^2)),
  curved_increasing = list(constraints = "m+", truth = function(x) exp(2 * x)))
if (boundary_only) {
  model_cases <- list(
    exactly_flat_increasing = list(constraints = "m+", truth = function(x) 0 * x),
    exactly_linear_convex = list(constraints = "c+", truth = function(x) .8 * x))
}

normal_draws <- matrix(rnorm(9L * 1999L), nrow = 9L)
failures <- function(error, scenario, replication, stage) {
  errors[[length(errors) + 1L]] <<- data.frame(scenario, replication, stage,
    message = conditionMessage(error))
  NULL
}
run_fit <- function(form, d, scenario, replication, stage, ...) {
  tryCatch(withCallingHandlers(scasm(form, data = d, ...),
    warning = function(w) {
      warnings_log[[length(warnings_log) + 1L]] <<-
        data.frame(scenario, replication, stage, message = conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) failures(e, scenario, replication, stage))
}

for (scenario in names(model_cases)) {
  case <- model_cases[[scenario]]
  truth <- case$truth(grid$x)
  constraints <- list(x = case$constraints)
  for (replication in seq_len(model_repetitions)) {
    d <- data.frame(x = c(0, runif(148L), 1))
    d$y <- case$truth(d$x) + rnorm(nrow(d), sd = .2)
    form <- y ~ s(x, bs = "sc", xt = case$constraints, k = 8)
    fit <- run_fit(form, d, scenario, replication, "fit", bs = bootstrap_repetitions)
    if (is.null(fit)) next
    slack <- drop(fit$Ain %*% coef(fit)) - fit$bin
    binding_tolerance <- 1e-7 * max(1, abs(slack))
    bindings[[length(bindings) + 1L]] <- data.frame(scenario, replication,
      minimum_slack = min(slack),
      active_inequalities = sum(slack <= binding_tolerance),
      total_inequalities = length(slack), smoothing_parameter = fit$sp[1L])
    X <- predict(fit, newdata = grid, type = "lpmatrix")
    estimate <- drop(X %*% coef(fit))
    for (covariance in c("Vp", "Ve")) {
      V <- fit[[covariance]]
      eig <- eigen(V, symmetric = TRUE)
      tolerance <- 100 * ncol(V) * .Machine$double.eps * max(abs(V))
      if (any(eig$values < -tolerance)) {
        failures(simpleError("Covariance is not positive semidefinite"),
          scenario, replication, paste0("covariance_", covariance))
        next
      }
      se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
      draws <- X %*% (eig$vectors %*% (sqrt(pmax(0, eig$values)) * normal_draws))
      critical_sim <- unname(quantile(apply(abs(draws / pmax(se, 1e-14)),
        2L, max), level))
      for (interval in c("pointwise", "simultaneous")) {
        critical <- if (interval == "pointwise") qnorm((1 + level) / 2) else critical_sim
        evaluate_pair(truth, estimate, estimate - critical * se,
          estimate + critical * se, grid, constraints, "scasm", scenario,
          replication, paste0("gaussian_", covariance), interval)
      }
    }
    for (corrected in c(TRUE, FALSE)) {
      copy <- fit
      if (!corrected) attr(copy$bs, "svar.only") <- NULL
      native <- predict(copy, newdata = grid, se.fit = level)
      if (any(!is.finite(c(native$ll, native$ul)))) {
        failures(simpleError("Native prediction contains nonfinite interval limits"),
          scenario, replication, "bootstrap_prediction")
        next
      }
      evaluate_pair(truth, estimate, drop(native$ll), drop(native$ul), grid,
        constraints, "scasm", scenario, replication,
        if (corrected) "native_bootstrap_corrected" else "native_bootstrap_raw",
        "pointwise")
    }
    low_penalty <- run_fit(form, d, scenario, replication, "low_penalty_fit",
      sp = 1e-8, bs = 0)
    if (!is.null(low_penalty)) {
      low_estimate <- drop(predict(low_penalty, newdata = grid))
      add_result(truth = truth, estimate = low_estimate, lower = low_estimate,
        upper = low_estimate, experiment = "scasm", scenario = scenario,
        replication = replication, source = "low_penalty_estimate_only",
        interval = "none", enforced = FALSE)
    }
  }
  message("scasm experiment completed: ", scenario)
}

records <- do.call(rbind, results)
write.csv(records, output_path("replications.csv"), row.names = FALSE)
group_columns <- c("experiment", "scenario", "source", "interval", "enforced")
keys <- do.call(paste, c(records[group_columns], sep = "|"))
summary <- do.call(rbind, lapply(split(records, keys), function(z) {
  key <- do.call(paste, c(z[1L, group_columns], sep = "|"))
  point_coverage <- pointwise_counts[[key]] / nrow(z)
  bias <- bias_sums[[key]] / nrow(z)
  data.frame(z[1L, group_columns], repetitions = nrow(z),
    simultaneous_coverage = mean(z$simultaneous),
    simultaneous_mc_se = sqrt(mean(z$simultaneous) * (1 - mean(z$simultaneous)) / nrow(z)),
    mean_pointwise_coverage = mean(point_coverage),
    minimum_pointwise_coverage = min(point_coverage),
    maximum_pointwise_mc_se = max(sqrt(point_coverage * (1 - point_coverage) / nrow(z))),
    mean_width = mean(z$mean_width), mean_maximum_width = mean(z$maximum_width),
    mean_bias = mean(bias), maximum_absolute_bias = max(abs(bias)),
    root_mean_squared_error = sqrt(mean(z$mean_squared_error)))
}))
rownames(summary) <- NULL
write.csv(summary, output_path("summary.csv"), row.names = FALSE)
failure_data <- if (length(errors)) do.call(rbind, errors) else
  data.frame(scenario = character(), replication = integer(), stage = character(), message = character())
write.csv(failure_data, output_path("failures.csv"), row.names = FALSE)
warning_data <- if (length(warnings_log)) do.call(rbind, warnings_log) else
  data.frame(scenario = character(), replication = integer(), stage = character(), message = character())
write.csv(warning_data, output_path("warnings.csv"), row.names = FALSE)
binding_data <- do.call(rbind, bindings)
write.csv(binding_data, output_path("bindings.csv"), row.names = FALSE)
report <- c("Finite-grid shape-interval simulation", "",
  paste("R:", getRversion()), paste("mgcv:", packageVersion("mgcv")),
  paste("Seed:", 20261016), paste("Level:", level),
  paste("Exactly binding truth scenarios only:", boundary_only),
  paste("Known-Gaussian repetitions per scenario:", oracle_repetitions),
  paste("scasm repetitions attempted per scenario:", model_repetitions),
  paste("Native bootstrap draws:", bootstrap_repetitions),
  paste("Gaussian simultaneous calibration draws:", ncol(normal_draws)),
  paste("Lost originally covering whole-grid events:", lost_simultaneous_events),
  paste("Unordered transformed intervals:", unordered_intervals),
  paste("Increases in maximum interval width:", maximum_width_increases),
  paste("Maximum discrepancy against independent one-dimensional operators:",
    format(reference_error, digits = 16)),
  paste("Recorded fit or interval failures:", nrow(failure_data)), "",
  paste("Recorded fitting warnings:", nrow(warning_data)), "",
  "The known-Gaussian experiment starts with exact pointwise or exact whole-grid",
  "coverage under independent normal errors with known, unequal variances.",
  "The whole-grid critical value is the exact Sidak value for the listed points.",
  "Coverage probabilities and Monte Carlo standard errors concern repeated",
  "independent datasets, not independent locations within a dataset.",
  "At 95% coverage the Monte Carlo SE is approximately 0.0049 for 2,000",
  "replications and 0.0109 for 400 replications. Worst-case SE is 0.0112 and",
  "0.0250 respectively. The CSV reports actual SEs and successful counts.", "",
  "The scasm experiment uses 148 independent uniform design points and fixed",
  "endpoints 0 and 1 (n = 150), Gaussian noise SD 0.2, a cubic B-spline basis",
  "with k = 8, and smoothing selected by",
  "scasm. The fitted response includes its intercept, so monotonicity and",
  "convexity restrictions remain appropriate on the response scale.",
  "Vp denotes approximate Bayesian covariance conditional on fitted smoothing",
  "parameters; Ve denotes frequentist covariance. The 1,999 fixed normal draws",
  "are reused to approximate each Gaussian whole-grid critical value. That error is",
  "additional to the reported binomial Monte Carlo errors for coverage.",
  "Neither covariance is assumed to yield exact frequentist coverage at a",
  "binding inequality. Native percentile limits are evaluated with and without",
  "mgcv's svar.only endpoint correction, using the same bootstrap coefficients.",
  "A covariance with an eigenvalue below the package's numerical tolerance is",
  "recorded as a covariance failure and excluded from Gaussian summaries.",
  "Only negative eigenvalues within rounding tolerance are replaced by zero.",
  "The low-penalty estimate uses the same model basis and sp = 1e-8. Comparing",
  "its empirical bias with selected-penalty bias investigates sensitivity to",
  "smoothing; boundary constraints and finite-basis approximation still apply.",
  "This comparison does not isolate smoothing bias from those other sources",
  "of estimation error, and no low-penalty interval widths are calculated.",
  "Its zero-width records are for bias and RMSE only, not interval inference.", "",
  "The bindings CSV records active fitted inequalities, counted when slack is",
  "at most 1e-7 times max(1, max(abs(slack))), and the fitted smoothing parameter.",
  "Shape transformation preserves a simultaneously covering initial band",
  "sample by sample when the truth obeys the fitted restrictions. It does not",
  "establish nominal simultaneous coverage for an initially pointwise interval.",
  "The minimum pointwise coverage is reported because marginal coverage at each",
  "fixed location need not survive operators that combine locations.",
  "All evaluations concern finite grids. No result here checks the fitted",
  "spline between evaluation points or the full continuous domain.",
  "Failed fits are retained in the failure CSV and omitted from interval",
  "summaries; the reported scasm coverage is conditional on a fitted model",
  "being returned without an R error. Returned models with warnings remain",
  "included, including any convergence warning recorded in the warning CSV.",
  "The successful-fit counts below refer to this same returned-model event.")
report_details <- function(summary, failure_data, warning_data, binding_data,
                            model_repetitions) {
  counts <- aggregate(replication ~ scenario, binding_data, length)
  names(counts)[2L] <- "successful_fits"
  counts$attempted_fits <- model_repetitions
  counts$failed_fits <- counts$attempted_fits - counts$successful_fits
  bindings_summary <- aggregate(active_inequalities ~ scenario, binding_data,
    function(x) mean(x > 0))
  names(bindings_summary)[2L] <- "fraction_with_active_inequalities"
  selected <- summary[summary$experiment == "scasm" & summary$enforced &
    (summary$source == "gaussian_Vp" & summary$interval == "simultaneous" |
      summary$source == "native_bootstrap_corrected" & summary$interval == "pointwise"),
    c("scenario", "source", "interval", "repetitions", "simultaneous_coverage",
      "simultaneous_mc_se", "mean_pointwise_coverage", "minimum_pointwise_coverage",
      "maximum_pointwise_mc_se", "mean_width")]
  oracle <- summary[summary$experiment == "known_gaussian" &
    (summary$interval == "simultaneous" | summary$enforced),
    c("scenario", "interval", "enforced", "repetitions", "simultaneous_coverage",
      "simultaneous_mc_se", "mean_pointwise_coverage", "minimum_pointwise_coverage",
      "maximum_pointwise_mc_se", "mean_width")]
  c("", "Exact fitting counts:", capture.output(print(counts, row.names = FALSE)),
    "", "Binding constraints among successful fits:",
    capture.output(print(bindings_summary, row.names = FALSE)), "",
    "Selected transformed-interval results (source-specific successful counts):",
    capture.output(print(selected, row.names = FALSE, digits = 5)), "",
    "Known-Gaussian results (including the initial simultaneous-band comparison):",
    capture.output(print(oracle, row.names = FALSE, digits = 5)), "",
    "Failure messages and counts:", capture.output(print(table(failure_data$stage,
      failure_data$message))), "", "Warning messages and counts:",
    capture.output(print(table(warning_data$message))), "",
    "The summary, failure, warning and binding CSVs and this script are the",
    "versioned numerical record. Full replication CSVs remain local generated",
    "files, ignored by Git, and can be reproduced with the two commands above.")
}
report <- c(report, report_details(summary, failure_data, warning_data,
  binding_data, model_repetitions))
writeLines(sub("[[:blank:]]+$", "", report), output_path("validation.txt"))
message("Simulation files written to ", out_dir)
