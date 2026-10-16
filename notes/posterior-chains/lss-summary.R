# Summarise the supplied-data study without hiding failed transformations.
directory <- "notes/posterior-chains"
x <- read.csv(file.path(directory, "lss-prediction-benchmarks.csv"))
summary <- do.call(rbind, lapply(unique(x$family), function(family) {
  selected <- x[x$family == family, ]
  median_time <- function(column) median(selected[[column]], na.rm = TRUE)
  data.frame(family = family, fitted_seconds = median_time("fitted_seconds"),
    posterior_seconds = median_time("posterior_seconds"),
    predicted_seconds = median_time("predicted_seconds"))
}))
write.csv(summary, file.path(directory, "lss-prediction-summary.csv"), row.names = FALSE)
format_time <- function(x) if (is.finite(x)) sprintf("%.3f", x) else "unavailable"
report <- c("# Posterior prediction for location-scale-shape models", "",
  "All seven installed mgcv LSS families now support supplied evaluation data.",
  "The [support audit](lss-audit.md) describes parameter conventions, expected",
  "responses, fitting limitations and the functions that consume joint draws.", "",
  "## Prediction and response costs", "",
  "Each model was fitted to 2,000 simulated observations with linear predictors",
  "containing four to eight coefficients in total. These timings evaluate 200",
  "genuinely new covariate rows without observed responses. Fitted and posterior",
  "response sampling reuse 1,000 Gaussian coefficient draws over four chains.",
  "Fixed-coefficient response sampling draws 1,000 replicated responses. The",
  "table reports medians over three seeds, in seconds. Coefficient sampling and",
  "diagnostics are excluded; parameter transformations, response generation, tidy",
  "tables and row/chain identities are included. BLAS/OpenMP threads were limited",
  "to one, and validation processes had finished before timing.", "",
  "Fitted expectations use bounded batches of matrix products and one output",
  "table. Responses reuse design blocks and construct one table per call,",
  "retaining one generator call per coefficient draw and preserving",
  "the random-number order of the existing fitting-observation sampler.", "",
  "| Family | Expected-response draws | Posterior response draws | Fixed-coefficient response draws |",
  "|:-------|------------------------:|-------------------------:|--------------------------------:|")
for (i in seq_len(nrow(summary))) {
  row <- summary[i, ]
  report <- c(report, sprintf("| %s | %s | %s | %s |", row$family,
    format_time(row$fitted_seconds), format_time(row$posterior_seconds),
    format_time(row$predicted_seconds)))
}
notes <- unique(unlist(x[c("fitted_note", "posterior_note", "predicted_note")]))
notes <- notes[!is.na(notes) & nzchar(notes)]
report <- c(report, "", if (length(notes)) c("Reported limitations:", paste0("* ", notes)) else
  "All 21 family/seed runs completed all three transformations without warnings or errors.")
mh <- read.csv(file.path(directory, "family-benchmarks.csv"))
mh <- mh[mh$family == "gaulss", ]
quality <- read.csv(file.path(directory, "family-summary.csv"))
quality <- quality[quality$family == "gaulss", ]
report <- c(report, "", "## Affected MH benchmark", "",
  "The family-backend study reran all six Gaussian location-scale cases: three",
  "seeds with mgcv and family backends, 2,000 fitting observations, 20 coefficients,",
  "4,000 retained draws over four serial chains and 1,000 burn-in iterations per",
  "chain. Prediction evaluates the first 200 rows using those retained draws.",
  sprintf("Median fitted transformation cost is %.3f seconds and median posterior response cost is %.3f seconds.",
    median(mh$fitted_seconds), median(mh$posterior_seconds)),
  sprintf("The family/mgcv minimum-coefficient ESS/s ratios are %.2f for bulk and %.2f for tails; maximum R-hat is %.3f.",
    quality$family_mgcv_bulk_ratio, quality$family_mgcv_tail_ratio, quality$max_rhat),
  "These paired ratios use the median over three seeds. The public sampling",
  "time includes setup and burn-in; prediction and response costs are separate.",
  "Process peak memory includes fitting, diagnostics and transformations.", "",
  "Raw timing and diagnostic results are in family-benchmarks.csv and",
  "family-coefficients.csv. The LSS transformation rows are in",
  "lss-prediction-benchmarks.csv, with failure notes retained if a transformation",
  "cannot run. lss-benchmark-environment.txt records package/runtime details.",
  "See integration-validation.txt for the latest tests and source-package check.", "",
  "## Reproduce", "", "Install this branch and set R_LIBS to its installation, then run:",
  "", "```sh", "Rscript notes/posterior-chains/family-study.R",
  "Rscript notes/posterior-chains/family-summary.R",
  "Rscript notes/posterior-chains/lss-prediction-benchmark.R",
  "Rscript notes/posterior-chains/lss-summary.R", "```")
writeLines(report, file.path(directory, "lss-report.md"))
print(summary)
