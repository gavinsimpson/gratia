# Review checks against the frozen implementation that generated the study.
args <- commandArgs(TRUE)
archive <- if (length(args)) args[1L] else
  "/Users/gavin/work/git/gratia/research-archives/mh-adjustment-2026-10-15-main"
source("notes/posterior-chains/smoothing-coverage-functions.R")
old <- new.env(parent = asNamespace("gratia"))
sys.source(file.path(archive, "sources/R/sampling-mh-adjustment.R"), envir = old)
adjust <- get("sampling_mh_covariance_adjust", asNamespace("gratia"))
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
selection <- data.frame(case = c("normal_known", "normal_weak", "normal_null",
  "binomial_rare", "binomial_rare", "binomial_rare"), replicate = c(1L, 1L, 1L, 1L, 4L, 73L))
cases <- coverage_cases()
for (j in seq_len(nrow(selection))) for (i in 1:2) {
  case <- selection$case[j]; replicate <- selection$replicate[j]
  engine <- if (i == 1L) "gam" else "bam"
  method <- if (i == 1L) "REML" else "fREML"
  spec <- cases[match(case, cases$case), , drop = FALSE]
  fixture <- coverage_data(spec, replicate)
  fit <- withr::with_seed(30261015L + match(case, cases$case) * 10000L +
    replicate * 10L + if (i == 1L) 2L else 5L,
    coverage_fit(spec, fixture, engine, method))
  seed <- 54261015L + match(case, cases$case) * 10000L + replicate * 10L + i
  draws <- gratia::mh_draws(fit, n_draws = 16000L, chains = 4L, burnin = 4000L,
    n_cores = 1L, mh_backend = "gratia", diagnostics = TRUE, seed = seed)
  before <- old$sampling_mh_covariance_adjust(fit, draws)
  after <- adjust(fit, draws)
  stopifnot(identical(before, after))
  cat(case, replicate, engine, ": adjusted coefficients, audit and diagnostics identical.\n")
}
h <- readRDS(file.path(archive, "review-before-hashes.rds"))
for (p in c("mh-adjustment-coverage.csv", "mh-adjustment-paired.csv",
    "mh-adjustment-availability.csv", "mh-adjustment-resolution.csv", "mh-adjustment-followup.csv")) {
  f <- file.path("notes/posterior-chains", p)
  stopifnot(identical(unname(tools::md5sum(f)), unname(h[f])))
}
old_csv <- utils::read.csv(file.path(archive, "diagnostics-before-review.csv"))
new_csv <- utils::read.csv("notes/posterior-chains/mh-adjustment-diagnostics.csv")
renames <- c(covariance_relative_error = "adjustment_covariance_relative_error",
  min_covariance_ratio = "adjustment_min_covariance_ratio",
  max_covariance_ratio = "adjustment_max_covariance_ratio",
  mean_shift_from_mode = "source_mh_mean_shift_from_mode",
  fixed_acceptance = "source_mh_fixed_acceptance", rw_acceptance = "source_mh_rw_acceptance")
names(old_csv)[match(names(renames), names(old_csv))] <- unname(renames)
stopifnot(identical(old_csv, new_csv))
cat("Coverage, paired differences, availability, resolution and follow-up files are byte-identical.\n")
cat("Diagnostic CSV changed only the six labels identifying fit-level source-MH/adjustment fields.\n")
