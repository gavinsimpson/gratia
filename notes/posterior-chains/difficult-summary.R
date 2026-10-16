# Summarise completed long runs without selecting only favourable seeds.
directory <- normalizePath("notes/posterior-chains")
runs <- read.csv(file.path(directory, "difficult-benchmarks.csv"))
diagnostics <- read.csv(file.path(directory, "difficult-diagnostics.csv"))
keys <- unique(runs[c("case", "method", "group")])
summary <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
  key <- keys[i, ]
  selected <- runs[runs$case == key$case & runs$method == key$method & runs$group == key$group, ]
  cbind(key, data.frame(seeds = nrow(selected),
    median_seconds = median(selected$total_seconds),
    min_seconds = min(selected$total_seconds), max_seconds = max(selected$total_seconds),
    median_bulk_ess_per_second = median(selected$bulk_ess_per_second),
    median_tail_ess_per_second = median(selected$tail_ess_per_second),
    worst_rhat = max(selected$max_rhat),
    worst_bulk_ess = min(selected$min_ess_bulk), worst_tail_ess = min(selected$min_ess_tail),
    worst_mcse_over_sd = max(selected$max_mcse_over_sd),
    all_runs_pass = nrow(selected) == 3L && all(selected$max_rhat < 1.01 &
      selected$min_ess_bulk >= 400 & selected$min_ess_tail >= 400),
    median_fitted_seconds = median(selected$fitted_seconds),
    median_derivative_seconds = median(selected$derivative_seconds),
    median_response_seconds = median(selected$response_seconds),
    median_peak_mb = median(selected$process_peak_mb)))
}))
write.csv(summary, file.path(directory, "difficult-summary.csv"), row.names = FALSE)
# Differences are comparisons against another Monte Carlo estimate, not a
# gold-standard integral. The matched-seed methods share random-number streams;
# the combined MCSE is a descriptive scale, not a calibrated hypothesis test.
comparisons <- list()
for (case in unique(diagnostics$case)) for (seed in unique(diagnostics$seed)) {
  reference <- diagnostics[diagnostics$case == case & diagnostics$seed == seed &
    diagnostics$method == "gratia", ]
  for (method in setdiff(unique(diagnostics$method), "gratia")) {
    other <- diagnostics[diagnostics$case == case & diagnostics$seed == seed &
      diagnostics$method == method, ]
    paired <- merge(reference, other, by = c("variable", "group"), suffixes = c("_gratia", "_other"))
    if (nrow(paired) == 0L) next
    comparisons[[length(comparisons) + 1L]] <- data.frame(case = case, seed = seed,
      method = method, group = paired$group, variable = paired$variable,
      mean_difference = paired$mean_other - paired$mean_gratia,
      combined_mcse = sqrt(paired$mcse_mean_other^2 + paired$mcse_mean_gratia^2),
      difference_over_combined_mcse = abs(paired$mean_other - paired$mean_gratia) /
        sqrt(paired$mcse_mean_other^2 + paired$mcse_mean_gratia^2),
      rhat_gratia = paired$rhat_gratia, rhat_other = paired$rhat_other)
  }
}
if (length(comparisons)) write.csv(do.call(rbind, comparisons),
  file.path(directory, "difficult-mean-comparisons.csv"), row.names = FALSE)
# Format one line per model and method, with coefficient/derived diagnostics
# side by side. Include failed screens explicitly; throughput alone is not a
# reason to select an unconverged method.
rows <- summary[summary$group == "coefficients", ]
lines <- c("| Target | Method | Seconds | Coefficient R-hat | Quantity R-hat | Bulk ESS/s, coefficients | Tail ESS/s, coefficients | Bulk ESS/s, quantities | Tail ESS/s, quantities | Pass all runs |",
  "|:--|:--|--:|--:|--:|--:|--:|--:|--:|:--|")
for (i in seq_len(nrow(rows))) {
  row <- rows[i, ]
  q <- summary[summary$case == row$case & summary$method == row$method & summary$group == "quantities", ]
  lines <- c(lines, sprintf("| %s | %s | %.2f | %.4f | %.4f | %.1f | %.1f | %.1f | %.1f | %s |",
    row$case, row$method, row$median_seconds, row$worst_rhat, q$worst_rhat,
    row$median_bulk_ess_per_second, row$median_tail_ess_per_second,
    q$median_bulk_ess_per_second, q$median_tail_ess_per_second,
    if (row$all_runs_pass && q$all_runs_pass) "yes" else "no"))
}
writeLines(lines, file.path(directory, "difficult-table.md"))
cat(paste(lines, collapse = "\n"), "\n")
