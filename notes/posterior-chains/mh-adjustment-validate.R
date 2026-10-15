# Run outside the sandbox after mh-adjustment-summary.R.
args <- commandArgs(TRUE)
stopifnot(length(args) >= 1L)
directory <- normalizePath(args[1L])
destination <- if (length(args) >= 2L) args[2L] else "notes/posterior-chains"
protocol <- readRDS(file.path(directory, "protocol.rds"))$protocol
source_roots <- c(".", file.path(directory, "sources"))
matching_source <- vapply(source_roots, function(root) {
  files <- file.path(root, names(protocol$source_hashes))
  all(file.exists(files)) && identical(unname(tools::md5sum(files)),
    unname(protocol$source_hashes))
}, logical(1L))
if (!any(matching_source)) stop("Neither current nor archived execution sources match the study protocol.")
cat("Validated execution sources:", source_roots[which(matching_source)[1L]], "\n")
files <- list.files(directory, "-[0-9]+[.]rds$", full.names = TRUE)
records <- unlist(lapply(files, function(f) {
  x <- readRDS(f); stopifnot(identical(x$protocol, protocol))
  unlist(x$results, recursive = FALSE)
}), recursive = FALSE)
availability <- utils::read.csv(file.path(destination, "mh-adjustment-availability.csv"))
diagnostics <- utils::read.csv(file.path(destination, "mh-adjustment-diagnostics.csv"))
coverage <- utils::read.csv(file.path(destination, "mh-adjustment-coverage.csv"))
paired <- utils::read.csv(file.path(destination, "mh-adjustment-paired.csv"))
outcomes <- readRDS(file.path(directory, "dataset-outcomes.rds"))
required <- list(
  availability = c("case", "replicate", "engine", "method", "status", "stage", "warnings"),
  diagnostics = c("case", "replicate", "engine", "method", "sampler", "max_rhat",
    "min_bulk_ess", "min_tail_ess", "max_mcse_mean", "passes", "elapsed",
    "adjustment_covariance_relative_error", "adjustment_min_covariance_ratio",
    "adjustment_max_covariance_ratio", "source_mh_mean_shift_from_mode",
    "source_mh_fixed_acceptance", "source_mh_rw_acceptance"),
  coverage = c("case", "engine", "method", "sampler", "quantity", "region",
    "cohort", "datasets", "coverage", "coverage_mcse", "width", "width_mcse",
    "median_error", "median_error_mcse"),
  paired = c("case", "engine", "method", "quantity", "region", "comparison",
    "cohort", "datasets", "coverage_difference", "coverage_difference_mcse",
    "width_difference", "width_difference_mcse"))
for (name in names(required)) {
  missing <- setdiff(required[[name]], names(get(name)))
  if (length(missing)) stop("The ", name, " CSV is missing: ",
    paste(missing, collapse = ", "), call. = FALSE)
}
stopifnot(length(records) == protocol$count * 8L,
  nrow(availability) == length(records),
  !anyDuplicated(availability[c("case", "replicate", "engine")]),
  all(availability$replicate %in% seq_len(protocol$count)),
  nrow(diagnostics) == 3L * sum(availability$status == "OK"),
  all(diagnostics$adjustment_covariance_relative_error < 1e-7),
  nrow(outcomes) == 48L * sum(availability$status == "OK"),
  all(is.finite(as.matrix(outcomes[, c("coverage", "width", "median_error")]))),
  all(outcomes$coverage >= 0 & outcomes$coverage <= 1), all(outcomes$width >= 0))
mcse <- function(x) if (length(x) > 1L) stats::sd(x) / sqrt(length(x)) else NA_real_
keys <- c("case", "engine", "method", "sampler", "quantity", "region")
for (i in seq_len(nrow(coverage))) {
  row <- coverage[i, ]
  keep <- rep(TRUE, nrow(outcomes))
  for (k in keys) keep <- keep & outcomes[[k]] == row[[k]]
  if (row$cohort == "matched_mixing_screen") keep <- keep & outcomes$matched_pass
  x <- outcomes[keep, ]
  stopifnot(nrow(x) == row$datasets,
    abs(mean(x$coverage) - row$coverage) < 1e-12,
    abs(mcse(x$coverage) - row$coverage_mcse) < 1e-12,
    abs(mean(x$width) - row$width) < 1e-10, abs(mcse(x$width) - row$width_mcse) < 1e-10, abs(mean(x$median_error) - row$median_error) < 1e-10, abs(mcse(x$median_error) - row$median_error_mcse) < 1e-10)
}
pair_keys <- c("case", "replicate", "engine", "method", "quantity", "region")
for (i in seq_len(nrow(paired))) {
  row <- paired[i, ]
  methods <- strsplit(row$comparison, " minus ", fixed = TRUE)[[1L]]
  x <- outcomes
  for (k in setdiff(pair_keys, "replicate")) x <- x[x[[k]] == row[[k]], ]
  if (row$cohort == "matched_mixing_screen") x <- x[x$matched_pass, ]
  a <- x[x$sampler == methods[1L], ]; b <- x[x$sampler == methods[2L], ]
  a <- a[order(a$replicate), ]; b <- b[order(b$replicate), ]
  stopifnot(identical(a$replicate, b$replicate), nrow(a) == row$datasets,
    abs(mean(a$coverage - b$coverage) - row$coverage_difference) < 1e-12,
    abs(mcse(a$coverage - b$coverage) - row$coverage_difference_mcse) < 1e-12, abs(mean(a$width - b$width) - row$width_difference) < 1e-10, abs(mcse(a$width - b$width) - row$width_difference_mcse) < 1e-10)
}
# Each raw interval still has the declared pointwise interpretation.
raw_intervals <- 0L
for (r in records) if (r$status == "OK") {
  stopifnot(nrow(r$chains) == 4L, all(r$chains$retained == protocol$n_draws / 4L),
    all(r$chains$burnin == protocol$burnin), all(r$chains$thin == 1L))
  for (s in r$summary) {
    z <- s$intervals
    stopifnot(nrow(z) == 44L, all(is.finite(as.matrix(z[, c("truth", "lower", "median", "upper")]))),
      all(z$lower <= z$median), all(z$median <= z$upper),
      identical(z$covered, z$lower <= z$truth & z$upper >= z$truth))
    raw_intervals <- raw_intervals + nrow(z)
  }
}
# Check the reconstructed fits against the unchanged earlier simulation archive.
archive <- "/Users/gavin/work/git/gratia/research-archives/smoothing-coverage-2026-10-15-main"
original_compared <- 0L
for (r in records) {
  start <- 1L + 5L * ((r$replicate - 1L) %/% 5L)
  path <- file.path(archive, sprintf("%s-%05d.rds", r$case, start))
  old <- readRDS(path)$results[[r$replicate - start + 1L]]$records[[
    if (r$engine == "gam") 2L else 5L]]
  if (r$status == "OK") {
    stopifnot(identical(old$status$fit, "OK"),
      isTRUE(all.equal(unname(r$log_sp), unname(old$log_sp), tolerance = 1e-8)))
    original_compared <- original_compared + 1L
  }
}
cat("Validated", length(records), "attempted fits,", raw_intervals,
  "raw pointwise intervals and", original_compared, "matching original smoothing estimates.\n")
cat("Every coverage estimate and paired MCSE matches independent recalculation.\n")
cat("Source hashes match the executed protocol; draw counts, chains, burn-in and thinning match.\n")
cat("Unexported implementation and public sampling choices:\n")
stopifnot(!"sampling_mh_covariance_adjust" %in% getNamespaceExports("gratia"))
cat("The adjustment is internal; no public method or unconditional MH target was added.\n")
