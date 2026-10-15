# Run each sampler in a fresh R process and record process peak memory.
benchmark_script_directory <- function() {
  script <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(script) != 1L) {
    stop("Run this script with Rscript, or supply 'directory' to run_benchmarks().",
      call. = FALSE)
  }
  # Rscript encodes spaces in the --file argument as ~+~.
  script <- gsub("~+~", " ", sub("^--file=", "", script), fixed = TRUE)
  dirname(normalizePath(script, mustWork = TRUE))
}

benchmark_peak_mb <- function(log, platform = Sys.info()[["sysname"]]) {
  pattern <- switch(platform,
    Darwin = "([0-9]+)[[:space:]]+maximum resident set size",
    Linux = "Maximum resident set size \\(kbytes\\):[[:space:]]*([0-9]+)",
    stop("Peak memory collection supports macOS and Linux.", call. = FALSE)
  )
  output <- paste(log, collapse = "\n")
  match <- regmatches(output, regexec(pattern, output))[[1L]]
  if (length(match) != 2L) {
    stop("Could not read peak resident memory from /usr/bin/time output.",
      call. = FALSE)
  }
  # macOS reports bytes; GNU time on Linux reports kilobytes.
  as.numeric(match[2L]) / if (platform == "Darwin") 1e6 else 1000
}

benchmark_case <- function(script, family, n_obs, basis_size, method) {
  platform <- Sys.info()[["sysname"]]
  if (!platform %in% c("Darwin", "Linux")) {
    stop("Peak memory collection supports macOS and Linux.", call. = FALSE)
  }
  if (!file.exists("/usr/bin/time")) {
    stop("The benchmark runner requires /usr/bin/time.", call. = FALSE)
  }
  temp <- tempfile("gratia-benchmark-")
  dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE), add = TRUE)
  output <- file.path(temp, "result.csv")
  stdout <- file.path(temp, "stdout.txt")
  stderr <- file.path(temp, "stderr.txt")
  args <- c(if (platform == "Darwin") "-l" else "-v",
    shQuote(file.path(R.home("bin"), "Rscript")), shQuote(script),
    shQuote(family), shQuote(as.character(n_obs)),
    shQuote(as.character(basis_size)), shQuote(method), shQuote(output))
  status <- system2("/usr/bin/time", args, stdout = stdout, stderr = stderr,
    env = c("OMP_NUM_THREADS=1", "OPENBLAS_NUM_THREADS=1",
      "VECLIB_MAXIMUM_THREADS=1"))
  log <- readLines(stderr, warn = FALSE)
  if (status != 0L) {
    stop("Benchmark failed for ", family, " n=", n_obs, " p=", basis_size,
      " ", method, ":\n",
      paste(c(log, readLines(stdout, warn = FALSE)), collapse = "\n"),
      call. = FALSE)
  }
  result <- utils::read.csv(output, stringsAsFactors = FALSE)
  if (nrow(result) != 1L) {
    stop("Each benchmark must produce exactly one result row.", call. = FALSE)
  }
  result$process_peak_mb <- benchmark_peak_mb(log, platform)
  result
}

run_benchmarks <- function(
  directory = benchmark_script_directory(),
  output = file.path(directory, "benchmarks.csv"),
  cases = data.frame(
    family = c("gaussian", rep("poisson", 5L), "binomial", "sparse", "gamma", "nb"),
    n_obs = c(1000, 1000, 10000, 1000, 10000, 100000, 1000, 1000, 1000, 1000),
    basis_size = c(10, 10, 10, 30, 30, 30, 10, 30, 10, 10)
  ),
  methods = c("mgcv", "mh_r", "mh", "mala", "elliptical")
) {
  rows <- list()
  script <- normalizePath(file.path(directory, "benchmark.R"), mustWork = TRUE)
  for (i in seq_len(nrow(cases))) {
    for (method in methods) {
      result <- benchmark_case(script, cases$family[i], cases$n_obs[i],
        cases$basis_size[i], method)
      rows[[length(rows) + 1L]] <- result
      # Save completed cases so an interrupted study retains its results.
      utils::write.csv(do.call(rbind, rows), output, row.names = FALSE)
      cat(sprintf("%s n=%s p=%s %s: %s seconds\n", cases$family[i],
        cases$n_obs[i], cases$basis_size[i], method, result$sampling_seconds))
      flush.console()
    }
  }
  invisible(do.call(rbind, rows))
}

if (sys.nframe() == 0L) run_benchmarks()
