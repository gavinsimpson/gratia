directory <- "notes/posterior-chains"
x <- read.csv(file.path(directory, "response-performance.csv"))
stopifnot(all(x$exact_seeded_match))
summary <- do.call(rbind, lapply(unique(x$family), function(family) {
  selected <- x[x$family == family, ]
  old <- selected[selected$method == "previous", ]
  current <- selected[selected$method == "current", ]
  key <- c("seed", "repetition")
  pairs <- merge(old, current, by = key, suffixes = c("_previous", "_current"))
  stopifnot(nrow(pairs) == nrow(old), all(pairs$seconds_current > 0))
  data.frame(family = family, previous_seconds = median(old$seconds),
    current_seconds = median(current$seconds),
    paired_speedup = median(pairs$seconds_previous / pairs$seconds_current),
    repetitions_per_seed = length(unique(old$repetition)), seeds = length(unique(old$seed)))
}))
write.csv(summary, file.path(directory, "response-summary.csv"), row.names = FALSE)
allocations <- read.csv(file.path(directory, "response-allocations.csv"))
report <- c("# Posterior response generation performance", "",
  "This study compares the previous LSS posterior-response calculation with the",
  "optimised implementation in the same R process. The reference calculation",
  "repeats model-matrix column extraction and constructs/restores one tibble per",
  "coefficient draw. The current calculation caches invariant design blocks,",
  "fills response values directly, restores rows by position and builds one table.", "",
  "Both calculations retain the same matrix-vector products, distribution-parameter",
  "transformations and one family-generator call per coefficient draw. Every",
  "timed output is checked for exact equality, including responses, row positions,",
  "chain/iteration/global draw identities and attached sampling information.", "",
  "## Paired elapsed times", "",
  "Each family is fitted to 2,000 observations. Each transformation evaluates 200",
  "new covariate rows using 1,000 supplied Gaussian coefficient draws over four",
  "chains. Coefficient generation is excluded; prediction setup, parameter",
  "transformations, response generation, table construction and metadata are timed.",
  "Three seeds are evaluated with five repetitions each, alternating execution",
  "order and collecting garbage before each call. BLAS/OpenMP threads are limited",
  "to one; the longer sampler study and validation tests finished before these timings.", "",
  "Times are medians in seconds. Speedup is the median previous/current ratio",
  "across matched seed/repetition pairs.", "",
  "| Family | Previous | Current | Speedup |",
  "|:-------|---------:|--------:|--------:|")
for (i in seq_len(nrow(summary))) report <- c(report, sprintf("| %s | %.3f | %.3f | %.2f |",
  summary$family[i], summary$previous_seconds[i], summary$current_seconds[i], summary$paired_speedup[i]))
report <- c(report, "", "## Allocation traffic", "",
  "Rprofmem records total R allocation traffic for one representative gaulss",
  "transformation after timing. This measures allocated bytes and allocation",
  "counts during the call, not maximum resident memory or the size of live objects.", "",
  "| Implementation | Allocated MB | Allocations | Output MB |",
  "|:---------------|-------------:|------------:|----------:|")
for (i in seq_len(nrow(allocations))) report <- c(report, sprintf("| %s | %.2f | %s | %.2f |",
  allocations$method[i], allocations$allocated_mb[i], allocations$allocation_count[i], allocations$output_mb[i]))
report <- c(report, "", "The public result still stores every requested observation-by-draw response.",
  "Streaming diagnostic calculations retain their one-draw-at-a-time interface",
  "and share the cached numeric generator. The optimisation does not vectorise",
  "random generators across draws, which could change seeded responses.", "",
  "Grouped-family responses use the same single-table construction; their new-data",
  "timings and support boundaries are recorded separately in grouped-support.md.", "",
  "Reproduce after installing the branch and setting R_LIBS:", "", "```sh",
  "OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 Rscript notes/posterior-chains/response-performance.R",
  "Rscript notes/posterior-chains/response-summary.R", "```", "",
  "Raw paired timings: response-performance.csv. Allocation measurements:",
  "response-allocations.csv. Runtime details: response-environment.txt.")
writeLines(report, file.path(directory, "response-report.md"))
print(summary)
