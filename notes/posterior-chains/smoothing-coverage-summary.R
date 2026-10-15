# Summarise complete research checkpoints. Arguments: raw directory, CSV directory.
coverage_estimate <- function(x) {
  x <- x[is.finite(x)]
  count <- length(x)
  c(estimate = if (count) mean(x) else NA_real_,
    mcse = if (count > 1L) stats::sd(x) / sqrt(count) else NA_real_, n = count)
}

coverage_summarise <- function(directory, output) {
  metadata <- readRDS(file.path(directory, "protocol.rds"))
  protocol <- metadata$protocol
  configurations <- protocol$configurations
  samplers <- c("conditional", "unconditional", "mixture")
  summary <- points <- paired <- statuses <- list()
  for (case in protocol$cases$case) {
    files <- list.files(directory, pattern = paste0("^", case, "-[0-9]+\\.rds$"), full.names = TRUE)
    batches <- lapply(files, readRDS)
    stopifnot(all(vapply(batches, function(x) identical(x$protocol, protocol), logical(1))))
    datasets <- unlist(lapply(batches, `[[`, "results"), recursive = FALSE)
    replicates <- vapply(datasets, `[[`, integer(1), "replicate")
    stopifnot(!anyDuplicated(replicates), setequal(replicates, seq_len(protocol$replicates)))
    datasets <- datasets[order(replicates)]
    spec <- protocol$cases[protocol$cases$case == case, , drop = FALSE]
    count <- length(datasets); grid <- nrow(datasets[[1]]$truth)
    quantities <- colnames(datasets[[1]]$truth)
    truth <- datasets[[1]]$truth
    stopifnot(all(vapply(datasets, function(x) identical(x$truth, truth) && length(x$records) == nrow(configurations), logical(1))))
    for (cfg in seq_len(nrow(configurations))) {
      engine <- configurations$engine[cfg]; method <- configurations$method[cfg]
      records <- lapply(datasets, function(x) x$records[[cfg]])
      intervals <- array(NA_real_, c(count, grid, 3L, length(quantities), 3L))
      for (i in seq_along(records)) {
        record <- records[[i]]
        stopifnot(record$replicate == i, record$case == case, record$engine == engine, record$method == method)
        if (identical(record$status$fit, "OK")) stopifnot(identical(record$actual_method, method))
        for (s in seq_along(samplers)) {
          interval <- record$intervals[[samplers[s]]]
          status <- record$status[[samplers[s]]]
          if (is.null(status)) status <- paste("Fit failed:", record$status$fit)
          if (!is.null(interval)) {
            stopifnot(identical(status, "OK"), identical(dim(interval), c(grid, 3L, length(quantities))),
              all(is.finite(interval)), all(interval[, 1L, ] <= interval[, 2L, ]),
              all(interval[, 2L, ] <= interval[, 3L, ]))
            intervals[i, , , , s] <- interval
          } else stopifnot(status != "OK")
          statuses[[length(statuses) + 1L]] <- data.frame(case, replicate = i, engine, method,
            sampler = samplers[s], status, fit_status = record$status$fit,
            maximum_log_sp_sd = if (length(record$log_sp_sd)) max(record$log_sp_sd) else NA_real_,
            fit_warnings = record$fit_warnings, setup_warnings = record$setup_warnings,
            mixture_warnings = record$mixture_warnings, unconditional_warnings = record$unconditional_warnings,
            stringsAsFactors = FALSE)
        }
      }
      common <- apply(intervals, 1L, function(x) all(is.finite(x)))
      for (q in seq_along(quantities)) {
        coverage <- width <- bias <- array(NA_real_, c(count, grid, 3L))
        for (s in seq_along(samplers)) {
          lower <- intervals[, , 1L, q, s]; upper <- intervals[, , 3L, q, s]
          median <- intervals[, , 2L, q, s]
          target <- matrix(truth[, q], count, grid, byrow = TRUE)
          coverage[, , s] <- lower <= target & target <= upper
          width[, , s] <- upper - lower
          bias[, , s] <- median - target
          for (stratum in c("interior", "boundary")) {
            j <- if (stratum == "interior") 2:(grid - 1L) else c(1L, grid)
            covered <- rowMeans(coverage[, j, s, drop = FALSE], dims = 1L)
            widths <- rowMeans(width[, j, s, drop = FALSE], dims = 1L)
            biases <- rowMeans(bias[, j, s, drop = FALSE], dims = 1L)
            mse <- rowMeans(bias[, j, s, drop = FALSE]^2, dims = 1L)
            estimate <- coverage_estimate(covered)
            returned_and_covered <- covered; returned_and_covered[!is.finite(covered)] <- 0
            row <- data.frame(case, family = spec$family, observations = spec$n, engine, method,
              sampler = samplers[s], quantity = quantities[q], stratum,
              attempted = count, returned = as.integer(estimate["n"]), coverage = unname(estimate["estimate"]),
              coverage_mcse = unname(estimate["mcse"]),
              returned_and_covered = mean(returned_and_covered),
              mean_width = mean(widths, na.rm = TRUE), median_bias = mean(biases, na.rm = TRUE),
              median_rmse = sqrt(mean(mse, na.rm = TRUE)))
            summary[[length(summary) + 1L]] <- row
          }
          for (j in seq_len(grid)) {
            estimate <- coverage_estimate(coverage[, j, s])
            points[[length(points) + 1L]] <- data.frame(case, engine, method, sampler = samplers[s],
              quantity = quantities[q], x = datasets[[1]]$evaluation$x[j],
              returned = unname(estimate["n"]), coverage = unname(estimate["estimate"]),
              coverage_mcse = unname(estimate["mcse"]), mean_width = mean(width[, j, s], na.rm = TRUE))
          }
        }
        for (stratum in c("interior", "boundary")) {
          j <- if (stratum == "interior") 2:(grid - 1L) else c(1L, grid)
          for (s in 2:3) {
            baseline <- rowMeans(coverage[, j, 1L, drop = FALSE], dims = 1L)
            candidate <- rowMeans(coverage[, j, s, drop = FALSE], dims = 1L)
            difference <- coverage_estimate((candidate - baseline)[common])
            conditional_coverage <- coverage_estimate(baseline[common])
            candidate_coverage <- coverage_estimate(candidate[common])
            paired[[length(paired) + 1L]] <- data.frame(case, engine, method,
              comparison = paste0(samplers[s], " - conditional"), quantity = quantities[q], stratum,
              common_returned = sum(common),
              conditional_coverage = unname(conditional_coverage["estimate"]),
              candidate_coverage = unname(candidate_coverage["estimate"]),
              difference = unname(difference["estimate"]), difference_mcse = unname(difference["mcse"]),
              conditional_width = mean(width[common, j, 1L], na.rm = TRUE),
              candidate_width = mean(width[common, j, s], na.rm = TRUE))
          }
        }
      }
    }
  }
  summary <- do.call(rbind, summary); points <- do.call(rbind, points)
  paired <- do.call(rbind, paired); statuses <- do.call(rbind, statuses)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  write.csv(summary, file.path(output, "smoothing-coverage-summary.csv"), row.names = FALSE)
  write.csv(points, file.path(output, "smoothing-coverage-pointwise.csv"), row.names = FALSE)
  write.csv(paired, file.path(output, "smoothing-coverage-paired.csv"), row.names = FALSE)
  write.csv(statuses, file.path(directory, "fit-and-sampler-status.csv"), row.names = FALSE)
  availability <- aggregate(I(status == "OK") ~ case + engine + method + sampler,
    data = statuses, FUN = sum)
  names(availability)[5L] <- "returned"; availability$attempted <- protocol$replicates
  write.csv(availability, file.path(output, "smoothing-coverage-availability.csv"), row.names = FALSE)
  failures <- statuses[statuses$status != "OK", c("case", "engine", "method", "sampler", "status")]
  failures$status <- sub("Smoothing component [0-9]+ failed:", "Smoothing component failed:", failures$status)
  failures <- if (nrow(failures)) aggregate(list(count = rep(1L, nrow(failures))), failures, sum) else
    transform(failures, count = integer())
  write.csv(failures, file.path(output, "smoothing-coverage-failures.csv"), row.names = FALSE)
  invisible(list(summary = summary, paired = paired, statuses = statuses, points = points))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(TRUE)
  if (length(args) != 2L) stop("Supply raw checkpoint directory and summary output directory.")
  coverage_summarise(args[1L], args[2L])
}
