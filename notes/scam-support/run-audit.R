# Run from the repository root, or pass the repository and output directories.
# All computations use R; package tests should be run outside the sandbox.
args <- commandArgs(TRUE)
repo <- normalizePath(if (length(args)) args[1L] else getwd(), mustWork = TRUE)
out <- if (length(args) > 1L) args[2L] else file.path(repo, "notes", "scam-support", "results")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out, mustWork = TRUE)
scripts <- file.path(repo, "notes", "scam-support", "scripts")
files <- sort(list.files(scripts, pattern = "[.]R$", full.names = TRUE))
stopifnot(length(files) == 5L)
for (file in files) {
  cat("Running", basename(file), "\n")
  status <- system2(file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", shQuote(file), shQuote(repo), shQuote(out)),
    stdout = file.path(out, paste0(basename(file), ".log")),
    stderr = file.path(out, paste0(basename(file), ".log")))
  if (status != 0L) stop("Audit step failed: ", basename(file), ". See its log in ", out)
}
api <- read.csv(file.path(out, "api-inventory.csv"), colClasses = "character",
  na.strings = character())
summary <- unique(api["operation"])
for (model in unique(api$model)) {
  summary[[model]] <- vapply(summary$operation, function(operation) {
    error <- api$error[api$model == model & api$operation == operation]
    if (!length(error)) "not run" else if (!nzchar(error)) "returned" else
      if (grepl("no applicable method|Don't know how", error)) "no method" else
        "error or explicit restriction"
  }, character(1))
}
write.csv(summary, file.path(out, "api-summary.csv"), row.names = FALSE)
cat("Audit results:", out, "\n")
