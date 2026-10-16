# Compare the prespecified higher-resolution subset against main-study replicates.
# Arguments: main raw directory, resolution raw directory, output CSV path.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
load_case <- function(directory, case) {
  paths <- list.files(directory, paste0("^", case, "-[0-9]+\\.rds$"), full.names = TRUE)
  result <- unlist(lapply(paths, function(p) readRDS(p)$results), recursive = FALSE)
  result[order(vapply(result, `[[`, integer(1L), "replicate"))]
}
main_protocol <- readRDS(file.path(args[1L], "protocol.rds"))$protocol
resolution_protocol <- readRDS(file.path(args[2L], "protocol.rds"))$protocol
stopifnot(identical(main_protocol$source_hashes, resolution_protocol$source_hashes))
rows <- list()
for (case in resolution_protocol$cases$case) {
  main <- load_case(args[1L], case); resolution <- load_case(args[2L], case)
  stopifnot(length(resolution) == resolution_protocol$replicates)
  main <- main[seq_along(resolution)]
  for (cfg in seq_len(nrow(main_protocol$configurations))) {
    first <- main[[1L]]$records[[cfg]]
    valid <- vapply(seq_along(main), function(i) !is.null(main[[i]]$records[[cfg]]$intervals$mixture), logical(1L))
    resolved <- vapply(seq_along(resolution), function(i) !is.null(resolution[[i]]$records[[cfg]]$intervals$mixture), logical(1L))
    common <- valid & resolved
    for (q in seq_len(4L)) {
      endpoint_errors <- coverage_difference <- numeric()
      for (i in which(common)) {
        a <- main[[i]]$records[[cfg]]$intervals$mixture[, , q]
        b <- resolution[[i]]$records[[cfg]]$intervals$mixture[, , q]
        stopifnot(identical(main[[i]]$truth, resolution[[i]]$truth))
        truth <- main[[i]]$truth[, q]
        endpoint_errors <- c(endpoint_errors, abs(a[, c(1L, 3L)] - b[, c(1L, 3L)]) / (b[, 3L] - b[, 1L]))
        coverage_difference <- c(coverage_difference, mean((b[2:10, 1L] <= truth[2:10] & truth[2:10] <= b[2:10, 3L]) -
          (a[2:10, 1L] <= truth[2:10] & truth[2:10] <= a[2:10, 3L])))
      }
      rows[[length(rows) + 1L]] <- data.frame(case, engine = first$engine, method = first$method,
        quantity = colnames(main[[1L]]$truth)[q], attempted = length(main),
        main_returned = sum(valid), resolution_returned = sum(resolved), common_returned = sum(common),
        median_endpoint_change_over_width = if (length(endpoint_errors)) median(endpoint_errors) else NA_real_,
        p95_endpoint_change_over_width = if (length(endpoint_errors)) unname(quantile(endpoint_errors, .95)) else NA_real_,
        maximum_endpoint_change_over_width = if (length(endpoint_errors)) max(endpoint_errors) else NA_real_,
        interior_coverage_change = if (length(coverage_difference)) mean(coverage_difference) else NA_real_,
        coverage_change_mcse = if (length(coverage_difference) > 1L) sd(coverage_difference) / sqrt(length(coverage_difference)) else NA_real_)
    }
  }
}
write.csv(do.call(rbind, rows), args[3L], row.names = FALSE)
