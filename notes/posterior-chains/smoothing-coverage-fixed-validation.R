# Validate the corrected partly fixed Gaussian fREML coverage study.
# Run outside the sandbox with the corrected package installed in R_LIBS.
# Arguments: original main archive, corrected case archive, draw-check archive.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
source("notes/posterior-chains/smoothing-coverage-functions.R")
load_case <- function(directory) {
  files <- list.files(directory, "^normal_fixed-[0-9]+\\.rds$", full.names = TRUE)
  batches <- lapply(files, readRDS)
  protocol <- readRDS(file.path(directory, "protocol.rds"))$protocol
  stopifnot(all(vapply(batches, function(b) identical(b$protocol, protocol), logical(1))))
  values <- unlist(lapply(batches, `[[`, "results"), recursive = FALSE)
  values[order(vapply(values, `[[`, integer(1), "replicate"))]
}
old <- load_case(args[1L]); current <- load_case(args[2L])
stopifnot(length(old) == 200L, length(current) == 200L)
for (i in seq_along(old)) {
  stopifnot(identical(old[[i]]$truth, current[[i]]$truth),
    identical(old[[i]]$evaluation, current[[i]]$evaluation))
  for (cfg in 1:5) {
    a <- old[[i]]$records[[cfg]]; b <- current[[i]]$records[[cfg]]
    a$interval_seconds <- b$interval_seconds <- NULL
    if (cfg < 5L) stopifnot(identical(a, b)) else {
      stopifnot(identical(a$intervals$conditional, b$intervals$conditional),
        identical(a$intervals$unconditional, b$intervals$unconditional),
        identical(a$log_sp, b$log_sp), identical(a$dispersion, b$dispersion),
        identical(b$status$setup, "OK"), identical(b$status$mixture, "OK"))
    }
  }
}
cat("200 datasets: all four unaffected configurations identical, excluding elapsed times.\n")
cat("bam fREML: native conditional/unconditional intervals and smoothing estimates unchanged; all 200 mixtures now returned.\n")

# Compare seeded draws with the original study evaluator at its recorded commit.
# Pinning that baseline keeps this check meaningful after later commits.
# Audit fields newly added are excluded.
previous <- new.env(parent = asNamespace("gratia"))
baseline <- readRDS(file.path(args[1L], "protocol.rds"))$protocol$commit
stopifnot(length(baseline) == 1L, grepl("^[[:xdigit:]]{40}$", baseline))
baseline_source <- system2("git", c("show", paste0(baseline, ":R/sampling-smoothing.R")), stdout = TRUE)
stopifnot(is.null(attr(baseline_source, "status")))
eval(parse(text = baseline_source), previous)
setup_fn <- get("sampling_smoothing_setup", asNamespace("gratia"))
draw_fn <- get("sampling_smoothing_draws", asNamespace("gratia"))
for (case in c("normal_estimated", "normal_shared")) {
  cases <- coverage_cases()
  spec <- cases[cases$case == case, , drop = FALSE]
  fixture <- coverage_data(spec, 1L)
  cfgs <- coverage_configurations()
  for (cfg in seq_len(nrow(cfgs))) {
    fit <- coverage_fit(spec, fixture, cfgs$engine[cfg], cfgs$method[cfg])
    a <- previous$sampling_smoothing_setup(fit); b <- setup_fn(fit)
    b$audit <- b$audit[names(a$audit)]
    stopifnot(identical(a, b))
    x <- previous$sampling_smoothing_draws(a, n_draws = 100L, seed = 271L)
    y <- draw_fn(b, n_draws = 100L, seed = 271L)
    stopifnot(identical(x, y))
  }
}
cat("Ten unaffected fits: cached setups and 100 seeded draws identical to baseline",
  baseline, "excluding added audit fields.\n")

spec <- subset(coverage_cases(), case == "normal_fixed")
values <- lapply(seq_len(200L), function(i) {
  fixture <- coverage_data(spec, i)
  fit <- coverage_fit(spec, fixture, "bam", "fREML")
  setup <- setup_fn(fit, block_size = 23L)
  stopifnot(identical(setup$audit$dispersion_source,
    "Pearson: weighted RSS / (n - sum(edf))"),
    setup$audit$covariance_error < 1e-7,
    setup$audit$mean_error < 1e-7)
  data.frame(replicate = i, reported_REML_dispersion = fit$sig2,
    coefficient_Pearson_dispersion = setup$phi,
    Pearson_over_REML = setup$phi / fit$sig2,
    mean_error = setup$audit$mean_error, covariance_error = setup$audit$covariance_error)
})
values <- do.call(rbind, values)
print(summary(values[, -1L]))
write.csv(values, "notes/posterior-chains/smoothing-coverage-fixed-dispersion.csv", row.names = FALSE)
summary_dir <- file.path(args[2L], "summaries")
for (topic in c("summary", "paired", "availability")) {
  from <- file.path(summary_dir, paste0("smoothing-coverage-", topic, ".csv"))
  to <- paste0("notes/posterior-chains/smoothing-coverage-fixed-", topic, ".csv")
  stopifnot(file.copy(from, to, overwrite = TRUE))
}
draws <- read.csv(file.path(args[3L], "smoothing-coverage-draw-check.csv"))
stopifnot(nrow(draws) == 6L, all(draws$status == "OK"))
stopifnot(file.copy(file.path(args[3L], "smoothing-coverage-draw-check.csv"),
  "notes/posterior-chains/smoothing-coverage-fixed-draw-check.csv", overwrite = TRUE))
cat("Six actual-draw checks passed. Largest absolute CDF discrepancy:", max(draws$maximum_cdf_error), "\n")
resolution <- read.csv("notes/posterior-chains/smoothing-coverage-fixed-resolution.csv")
stopifnot(nrow(resolution) == 20L, all(resolution$main_returned == 20L),
  all(resolution$resolution_returned == 20L))
cat("256/1024-component resolution: all 20 datasets returned in each of five configurations.\n")
cat("Largest absolute interior coverage change:", max(abs(resolution$interior_coverage_change)), "\n")
cat("Largest group-specific 95th percentile endpoint change / interval width:",
  max(resolution$p95_endpoint_change_over_width), "\n")
saveRDS(list(source_hashes = tools::md5sum(c("R/sampling-smoothing.R",
  "tests/testthat/test-sampling-smoothing.R", "notes/posterior-chains/smoothing-coverage-fixed-validation.R")),
  validated = Sys.time(), datasets = 200L, seeded_fits = 10L),
  file.path(args[2L], "validation.rds"))
