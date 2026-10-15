directory <- "notes/posterior-chains"
x <- read.csv(file.path(directory, "grouped-benchmarks.csv"))
summary <- do.call(rbind, lapply(unique(x$model), function(model) {
  selected <- x[x$model == model, ]
  data.frame(model = model, fitted_seconds = median(selected$fitted_seconds, na.rm = TRUE),
    posterior_seconds = median(selected$posterior_seconds, na.rm = TRUE),
    predicted_seconds = median(selected$predicted_seconds, na.rm = TRUE))
}))
write.csv(summary, file.path(directory, "grouped-summary.csv"), row.names = FALSE)
p <- file.path(directory, "grouped-support.md")
text <- readLines(p)
start <- match("## Prediction timings", text)
stopifnot(!is.na(start))
text <- c(text[seq_len(start)], "",
  "The installed final source evaluates 200 new covariate rows using 1,000",
  "supplied Gaussian coefficient draws across four chains, from a fit to 2,000",
  "observations with binomial, Gaussian and Poisson components. Fixed-parameter",
  "responses use 1,000 replicates. Coefficient sampling is excluded. BLAS/OpenMP",
  "threads are limited to one; these runs follow the longer study and precede",
  "package validation. Times are medians over three seeds, in seconds.", "",
  "| Fit | Expected-response draws | Posterior response draws | Fixed-coefficient response draws |",
  "|:----|------------------------:|-------------------------:|--------------------------------:|")
for (i in seq_len(nrow(summary))) text <- c(text, sprintf("| %s | %.3f | %.3f | %.3f |",
  summary$model[i], summary$fitted_seconds[i], summary$posterior_seconds[i], summary$predicted_seconds[i]))
notes <- unique(unlist(x[c("fitted_note", "posterior_note", "predicted_note")]))
notes <- notes[!is.na(notes) & nzchar(notes)]
text <- c(text, "", if (length(notes)) c("Reported limitations:", paste0("* ", notes)) else
  "All six fit/seed runs completed without warnings or errors.", "",
  "Reproduce with grouped-benchmark.R followed by grouped-summary.R, after",
  "installing this branch and setting R_LIBS to its installation. Raw results:",
  "grouped-benchmarks.csv; runtime details: grouped-benchmark-environment.txt.")
writeLines(text,p)
print(summary)
