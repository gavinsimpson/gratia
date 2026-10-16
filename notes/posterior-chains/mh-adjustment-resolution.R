# Compare the predeclared first five datasets at the longer draw count.
args <- commandArgs(TRUE)
stopifnot(length(args) >= 2L)
read_records <- function(directory) unlist(lapply(
  list.files(directory, "-[0-9]+[.]rds$", full.names = TRUE),
  function(f) unlist(readRDS(f)$results, recursive = FALSE)), recursive = FALSE)
main <- read_records(args[1L])
long <- read_records(args[2L])
destination <- if (length(args) >= 3L) args[3L] else "notes/posterior-chains"
rows <- list()
for (r in long) {
  candidates <- Filter(function(x) x$case == r$case && x$engine == r$engine &&
    x$replicate == r$replicate, main)
  stopifnot(length(candidates) == 1L)
  a <- candidates[[1L]]
  if (r$status != "OK" || a$status != "OK") stop("Resolution comparison contains a failed fit.")
  stopifnot(isTRUE(all.equal(r$log_sp, a$log_sp, tolerance = 1e-10)))
  for (sampler in names(r$summary)) {
    x <- a$summary[[sampler]]$intervals
    y <- r$summary[[sampler]]$intervals
    stopifnot(identical(x[c("quantity", "point", "truth")], y[c("quantity", "point", "truth")]))
    for (quantity in unique(x$quantity)) {
      z <- x[x$quantity == quantity, ]; w <- y[y$quantity == quantity, ]
      width <- z$upper - z$lower
      stopifnot(all(width > 0))
      error <- pmax(abs(z$lower - w$lower), abs(z$upper - w$upper)) / width
      rows[[length(rows) + 1L]] <- data.frame(case = r$case, replicate = r$replicate,
        engine = r$engine, method = r$method, sampler = sampler, quantity = quantity,
        max_endpoint_change_over_width = max(error),
        mean_endpoint_change_over_width = mean(error),
        coverage_change = mean(w$covered - z$covered))
    }
  }
}
rows <- do.call(rbind, rows)
utils::write.csv(rows, file.path(destination, "mh-adjustment-resolution.csv"), row.names = FALSE)
cat("Resolution comparisons:", nrow(rows), "quantity summaries from", length(long), "fits.\n")
print(aggregate(max_endpoint_change_over_width ~ sampler, rows, max))
print(aggregate(mean_endpoint_change_over_width ~ sampler, rows, mean))
