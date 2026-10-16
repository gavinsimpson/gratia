# Aggregate complete datasets, not correlated evaluation points, for MC errors.
args <- commandArgs(TRUE)
stopifnot(length(args) >= 1L)
directory <- normalizePath(args[1L])
destination <- if (length(args) >= 2L) args[2L] else "notes/posterior-chains"
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
files <- list.files(directory, "-[0-9]+[.]rds$", full.names = TRUE)
protocol <- readRDS(file.path(directory, "protocol.rds"))$protocol
records <- unlist(lapply(files, function(f) {
  x <- readRDS(f); stopifnot(identical(x$protocol, protocol))
  unlist(x$results, recursive = FALSE)
}), recursive = FALSE)
mcse <- function(x) if (length(x) > 1L) stats::sd(x) / sqrt(length(x)) else NA_real_
rows <- diagnostics <- availability <- list()
for (r in records) {
  id <- data.frame(case = r$case, replicate = r$replicate, engine = r$engine, method = r$method)
  availability[[length(availability) + 1L]] <- cbind(id, status = r$status,
    stage = if (r$status == "OK") "complete" else r$stage,
    warnings = paste(r$warnings, collapse = "; "))
  if (r$status != "OK") next
  pass <- logical()
  for (sampler in c("conditional_mh", "adjusted_mh", "unconditional_gaussian")) {
    s <- r$summary[[sampler]]
    d <- rbind(s$coefficient_diagnostics[, c("rhat", "ess_bulk", "ess_tail", "mcse_mean")],
      s$diagnostics[, c("rhat", "ess_bulk", "ess_tail", "mcse_mean")])
    good <- all(is.finite(as.matrix(d))) && max(d$rhat) < 1.01 &&
      min(d$ess_bulk) >= 400 && min(d$ess_tail) >= 400
    pass[sampler] <- good
    diagnostics[[length(diagnostics) + 1L]] <- cbind(id, sampler,
      max_rhat = max(d$rhat), min_bulk_ess = min(d$ess_bulk),
      min_tail_ess = min(d$ess_tail), max_mcse_mean = max(d$mcse_mean),
      passes = good, elapsed = r$sampling_elapsed[[sampler]],
      adjustment_covariance_relative_error = r$covariance_relative_error,
      adjustment_min_covariance_ratio = min(r$eigenvalue_ratios), adjustment_max_covariance_ratio = max(r$eigenvalue_ratios),
      source_mh_mean_shift_from_mode = r$mean_shift,
      source_mh_fixed_acceptance = mean(r$chains$fixed_acceptance),
      source_mh_rw_acceptance = mean(r$chains$rw_acceptance))
  }
  matched_pass <- all(pass[c("conditional_mh", "adjusted_mh")])
  for (sampler in names(r$summary)) {
    v <- r$summary[[sampler]]$intervals
    stopifnot(nrow(v) == 44L, all(is.finite(as.matrix(v[, c("truth", "lower", "median", "upper")]))),
      all(v$lower <= v$median), all(v$median <= v$upper),
      all(v$covered == (v$lower <= v$truth & v$upper >= v$truth)))
    for (quantity in unique(v$quantity)) for (region in c("interior", "endpoints", "all")) {
      z <- v[v$quantity == quantity, ]
      if (region == "interior") z <- z[z$point > 1L & z$point < 11L, ]
      if (region == "endpoints") z <- z[z$point %in% c(1L, 11L), ]
      rows[[length(rows) + 1L]] <- cbind(id, sampler, quantity, region,
        matched_pass = matched_pass, coverage = mean(z$covered),
        width = mean(z$upper - z$lower), median_error = mean(z$median - z$truth))
    }
  }
}
availability <- do.call(rbind, availability)
diagnostics <- do.call(rbind, diagnostics)
rows <- do.call(rbind, rows)
stopifnot(!anyDuplicated(availability[c("case", "replicate", "engine")]),
  nrow(availability) == protocol$count * length(protocol$cases) * 2L)
aggregate_rows <- list()
keys <- c("case", "engine", "method", "sampler", "quantity", "region")
for (cohort in c("all_returned", "matched_mixing_screen")) {
  x <- if (cohort == "all_returned") rows else rows[rows$matched_pass, ]
  groups <- split(x, interaction(x[keys], drop = TRUE))
  for (g in groups) aggregate_rows[[length(aggregate_rows) + 1L]] <- cbind(g[1L, keys],
    cohort = cohort, datasets = nrow(g), coverage = mean(g$coverage),
    coverage_mcse = mcse(g$coverage), width = mean(g$width),
    width_mcse = mcse(g$width), median_error = mean(g$median_error),
    median_error_mcse = mcse(g$median_error))
}
aggregate_rows <- do.call(rbind, aggregate_rows)
pair_rows <- list()
comparisons <- list(c("adjusted_mh", "conditional_mh"),
  c("adjusted_mh", "unconditional_gaussian"), c("conditional_mh", "conditional_gaussian"))
pair_keys <- c("case", "replicate", "engine", "method", "quantity", "region")
for (pair in comparisons) {
  a <- rows[rows$sampler == pair[1L], ]; b <- rows[rows$sampler == pair[2L], ]
  paired <- merge(a, b, by = pair_keys, suffixes = c("_a", "_b"))
  stopifnot(nrow(paired) == nrow(a), nrow(a) == nrow(b))
  for (cohort in c("all_returned", "matched_mixing_screen")) {
    x <- if (cohort == "all_returned") paired else paired[paired$matched_pass_a & paired$matched_pass_b, ]
    group_keys <- c("case", "engine", "method", "quantity", "region")
    groups <- split(x, interaction(x[group_keys], drop = TRUE))
    for (g in groups) pair_rows[[length(pair_rows) + 1L]] <- cbind(g[1L, group_keys],
      comparison = paste(pair, collapse = " minus "), cohort = cohort,
      datasets = nrow(g), coverage_difference = mean(g$coverage_a - g$coverage_b),
      coverage_difference_mcse = mcse(g$coverage_a - g$coverage_b),
      width_difference = mean(g$width_a - g$width_b),
      width_difference_mcse = mcse(g$width_a - g$width_b))
  }
}
pair_rows <- do.call(rbind, pair_rows)
for (nm in c("availability", "diagnostics", "coverage", "paired")) {
  value <- switch(nm, availability = availability, diagnostics = diagnostics,
    coverage = aggregate_rows, paired = pair_rows)
  utils::write.csv(value, file.path(destination, paste0("mh-adjustment-", nm, ".csv")),
    row.names = FALSE)
}
saveRDS(rows, file.path(directory, "dataset-outcomes.rds"))
cat("Attempted fits:", nrow(availability), "; complete comparisons:",
  sum(availability$status == "OK"), "\n")
print(with(availability, table(case, engine, status == "OK")))
cat("Diagnostic screen:\n")
print(with(diagnostics, table(sampler, passes)))
cat("Worst R-hat:", max(diagnostics$max_rhat), "; minimum bulk/tail ESS:",
  min(diagnostics$min_bulk_ess), min(diagnostics$min_tail_ess), "\n")
cat("Maximum covariance relative error:", max(diagnostics$adjustment_covariance_relative_error), "\n")
print(aggregate_rows[aggregate_rows$cohort == "all_returned" &
  aggregate_rows$region == "interior" & aggregate_rows$quantity == "derivative",
  c("case", "engine", "sampler", "datasets", "coverage", "coverage_mcse", "width")],
  row.names = FALSE)
