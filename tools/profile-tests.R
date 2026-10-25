# Profile an installed package, including shared setup and file-level work.
# Usage: Rscript --vanilla tools/profile-tests.R OUTPUT_DIRECTORY [LIBRARY]
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) > 2L) {
  stop("Supply an output directory and, optionally, an installed-package library.",
    call. = FALSE)
}
output <- args[[1L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
if (length(args) == 2L) .libPaths(c(args[[2L]], .libPaths()))
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
library("gratia")
options(device = function(...) grDevices::pdf(file = NULL))
environment <- Sys.getenv(c("NOT_CRAN", "VDIFFR_RUN_TESTS", "GRATIA_RUN_HGAM",
  "TESTTHAT_PARALLEL", "TESTTHAT_CPUS", "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS"))
print(environment)
print(sessionInfo())
# Paper examples have their own runner and are never part of this profile.
timing <- system.time(result <- testthat::test_package("gratia",
  filter = "hgam-paper", invert = TRUE, reporter = "summary",
  stop_on_failure = FALSE, stop_on_warning = FALSE))
frame <- as.data.frame(result)
saveRDS(result, file.path(output, "results.rds"))
write.csv(frame[, !vapply(frame, is.list, logical(1)), drop = FALSE],
  file.path(output, "tests.csv"), row.names = FALSE)
files <- aggregate(frame[c("real", "user", "system", "passed")],
  frame["file"], sum)
files <- files[order(files$real, decreasing = TRUE), ]
write.csv(files, file.path(output, "files.csv"), row.names = FALSE)
counts <- colSums(frame[c("failed", "skipped", "error", "warning", "passed")])
capture.output(list(timing = timing, counts = counts, environment = environment,
  session = sessionInfo()), file = file.path(output, "summary.txt"))
print(timing)
print(counts)
if (any(counts[c("failed", "error", "warning")] > 0)) {
  stop("Tests failed or warned; see the saved results.", call. = FALSE)
}
