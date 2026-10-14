# Simulate complete response datasets with the package's native simulator to
# retain dependence between observations. At each row, use its simulated values
# to estimate moments and locate observed or replicated responses among them.
# One fixed pool of simulations keeps the residual definition the same for both.
prepare_marginal_response_distribution <- function(distribution, seed = NULL,
    n = getOption("gratia.diagnostics.marginal_draws", 249L),
    max_cells = getOption("gratia.diagnostics.max_cells", 1e7)) {
  if (is.function(distribution$cdf) && (!is.null(distribution$mean) ||
      !distribution_moment_available(distribution, "mean"))) return(distribution)
  if (!isTRUE(distribution$joint_available) || !is.function(distribution$rd))
    stop("Integrated response calibration requires a valid joint response generator. ",
      distribution$dependence_reason %||% "", call. = FALSE)
  n <- validate_diagnostic_bins(n, "marginal reference draws", minimum = 19L)
  if (!is.numeric(max_cells) || length(max_cells) != 1L ||
      !is.finite(max_cells) || max_cells < 1)
    stop("'gratia.diagnostics.max_cells' must be positive.", call. = FALSE)
  seed <- seed %||% getOption("gratia.diagnostics.marginal_seed", 1L)
  reference_seed <- diagnostic_seed(seed, sample.int(.Machine$integer.max, 1L))
  storage <- diagnostic_seed(reference_seed, {
    first <- as.numeric(distribution$rd())
    nr <- length(first)
    if (!nr || any(!is.finite(first))) stop(
      "Native response draws must be finite and aligned with fitting observations.", call. = FALSE)
    storage <- new.env(parent = emptyenv())
    storage$n <- n; storage$nr <- nr
    disk <- as.double(nr) * n > max_cells
    directory <- if (disk) tempfile("gratia-marginal-") else NULL
    success <- FALSE
    if (disk) {
      dir.create(directory)
      on.exit(if (!success) unlink(directory, recursive = TRUE), add = TRUE)
      paths <- file.path(directory, paste0("draw-", seq_len(n)))
    } else values <- matrix(NA_real_, nrow = n, ncol = nr)
    mean <- m2 <- numeric(nr)
    for (i in seq_len(n)) {
      value <- if (i == 1L) first else as.numeric(distribution$rd())
      if (length(value) != nr || any(!is.finite(value))) stop(
        "Native response draws changed their observation layout.", call. = FALSE)
      if (distribution_moment_available(distribution, "mean")) {
        delta <- value - mean
        mean <- mean + delta / i
        if (distribution_moment_available(distribution, "variance"))
          m2 <- m2 + delta * (value - mean)
      }
      if (disk) writeBin(value, paths[i], size = 8L) else values[i, ] <- value
    }
    storage$mean <- mean; storage$variance <- m2 / (n - 1L)
    sort_columns <- function(x) matrix(apply(x, 2L, sort), nrow = n)
    if (disk) {
      block <- max(1L, min(nr, floor(max_cells / n)))
      starts <- seq.int(1L, nr, by = block)
      storage$blocks <- lapply(starts, function(start) {
        count <- min(block, nr - start + 1L)
        values <- vapply(paths, function(path) {
          con <- file(path, "rb"); on.exit(close(con))
          seek(con, where = as.double(start - 1L) * 8, origin = "start")
          readBin(con, "double", n = count, size = 8L)
        }, numeric(count))
        sorted <- sort_columns(t(matrix(values, nrow = count, ncol = n)))
        path <- file.path(directory, paste0("block-", start, ".rds"))
        saveRDS(sorted, path, compress = FALSE)
        list(rows = seq.int(start, length.out = count), path = path)
      })
      unlink(paths)
      storage$directory <- directory
      reg.finalizer(storage, function(e) unlink(e$directory, recursive = TRUE), onexit = TRUE)
    } else storage$values <- sort_columns(values)
    success <- TRUE
    storage
  })
  query <- function(q) {
    q <- rep_len(as.numeric(q), storage$nr)
    if (!is.null(storage$query) && identical(q, storage$query)) return(storage$counts)
    less <- leq <- numeric(storage$nr)
    visit <- function(values, rows) {
      for (j in seq_along(rows)) {
        row <- rows[j]
        if (is.na(q[row])) less[row] <<- leq[row] <<- NA_real_ else {
          less[row] <<- findInterval(q[row], values[, j], left.open = TRUE)
          leq[row] <<- findInterval(q[row], values[, j])
        }
      }
    }
    if (!is.null(storage$values)) visit(storage$values, seq_len(storage$nr)) else
      for (block in storage$blocks) visit(readRDS(block$path), block$rows)
    storage$query <- q
    storage$counts <- list(less = less, leq = leq)
    storage$counts
  }
  probability <- function(count, denominator, log_p, lower_tail) {
    p <- if (lower_tail) count / denominator else (denominator - count) / denominator
    if (log_p) log(p) else p
  }
  cdf <- function(q, log_p = FALSE, lower_tail = TRUE)
    probability(query(q)$leq, n, log_p, lower_tail)
  qf <- function(p, log_p = FALSE, lower_tail = TRUE) {
    p <- rep_len(if (log_p) exp(p) else p, storage$nr)
    if (!lower_tail) p <- 1 - p
    out <- rep(NA_real_, storage$nr)
    visit <- function(values, rows) for (j in seq_along(rows)) {
      row <- rows[j]
      if (is.finite(p[row]) && p[row] >= 0 && p[row] <= 1)
        out[row] <<- values[max(1L, ceiling(n * p[row])), j]
    }
    if (!is.null(storage$values)) visit(storage$values, seq_len(storage$nr)) else
      for (block in storage$blocks) visit(readRDS(block$path), block$rows)
    out
  }
  bounds <- function(y) list(lower = y, upper = y, left_limit = TRUE,
    cdf_lower = function(log_p = FALSE, lower_tail = TRUE)
      probability(query(y)$less, n + 1L, log_p, lower_tail),
    cdf_upper = function(log_p = FALSE, lower_tail = TRUE)
      probability(query(y)$leq + 1L, n + 1L, log_p, lower_tail))
  distribution["mean"] <- list(if (distribution_moment_available(distribution, "mean"))
    storage$mean else NULL)
  distribution["variance"] <- list(if (distribution_moment_available(distribution, "variance"))
    storage$variance else NULL)
  distribution$cdf <- cdf
  distribution$cdf_left <- function(q, log_p = FALSE, lower_tail = TRUE)
    probability(query(q)$less, n, log_p, lower_tail)
  distribution$qf <- qf
  distribution$bounds <- bounds
  distribution$calibration <- "simulation ranks"
  distribution$n_reference <- n
  distribution$approximation <- paste(c(distribution$approximation,
    paste0("Monte Carlo response law (", n, " draws); simulation-rank calibration")), collapse = "; ")
  distribution
}
