# Run against an installed development build, for example:
# R CMD INSTALL --library=/tmp/gratia-parallel-library .
# OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
#   VECLIB_MAXIMUM_THREADS=1 R_LIBS=/tmp/gratia-parallel-library \
#   Rscript notes/parallel-smooth-operations/benchmark.R
# Fitting, input-size inspection and worker startup are outside warmed timings.
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1,
  MKL_NUM_THREADS = 1, VECLIB_MAXIMUM_THREADS = 1)
library("gratia")
library("mgcv")
library("mirai")

benchmark_smooth_operations <- function(repetitions = 3L) {
  stopifnot(!isTRUE(mirai::daemons_set()))
  on.exit(mirai::daemons(0), add = TRUE)
  package_path <- getNamespaceInfo(asNamespace("gratia"), "path")
  stopifnot(exists("smooth_task_map", envir = asNamespace("gratia"),
    inherits = FALSE))
  message("Benchmarking gratia ", packageVersion("gratia"), " at ", package_path)
  set.seed(348)
  message("Fitting benchmark models outside timed regions")
  d <- data_sim("eg1", n = 50000, seed = 348)
  curve <- mgcv::bam(y ~ s(x0, k = 40) + s(x1, k = 40) +
    s(x2, k = 40) + s(x3, k = 40), data = d, discrete = TRUE, nthreads = 1)
  surface <- mgcv::bam(y ~ te(x0, x1, k = c(6, 6)) +
    te(x2, x3, k = c(6, 6)), data = d, discrete = TRUE, nthreads = 1)
  d$group <- factor(rep(letters[1:5], length.out = nrow(d)))
  factor_model <- mgcv::bam(y ~ group + s(x2, by = group, k = 30),
    data = d, discrete = TRUE, nthreads = 1)
  jobs <- list(
    derivatives_small = function() derivatives(curve, n = 100),
    derivatives_dense = function() derivatives(curve, n = 6000,
      interval = "simultaneous", n_sim = 3000),
    partial_small = function() partial_derivatives(surface, n = 100),
    partial_dense = function() partial_derivatives(surface, n = 6000,
      interval = "simultaneous", n_sim = 3000, seed = 348),
    samples_small = function() smooth_samples(curve, n_grid = 100,
      n_draws = 20, seed = 348),
    samples_many = function() smooth_samples(curve, n_grid = 1000,
      n_draws = 500, seed = 348),
    differences_small = function() difference_smooths(factor_model,
      select = "s(x2)", n = 100),
    differences_dense = function() difference_smooths(factor_model,
      select = "s(x2)", n = 1800, interval = "simultaneous",
      n_sim = 3000, seed = 348)
  )
  models <- c(rep(list(curve), 2), rep(list(surface), 2),
    rep(list(curve), 2), rep(list(factor_model), 2))
  measure <- function(job) {
    gc()
    set.seed(348)
    elapsed <- system.time(value <- job())[["elapsed"]]
    list(elapsed = elapsed, output_bytes = as.numeric(object.size(value)))
  }
  # Record serialized task sizes in a separate, untimed pass.
  trace("smooth_task_map", where = asNamespace("gratia"), print = FALSE,
    tracer = quote({
      .GlobalEnv$.gratia_benchmark_tasks <- append(
        .GlobalEnv$.gratia_benchmark_tasks, list(tasks))
    }), exit = quote({
      .GlobalEnv$.gratia_benchmark_returns <- append(
        .GlobalEnv$.gratia_benchmark_returns, list(returnValue()))
    }))
  sizes <- lapply(seq_along(jobs), function(i) {
    message("Inspecting task inputs: ", names(jobs)[i])
    .GlobalEnv$.gratia_benchmark_tasks <- list()
    .GlobalEnv$.gratia_benchmark_returns <- list()
    value <- jobs[[i]]()
    stages <- .GlobalEnv$.gratia_benchmark_tasks
    tasks <- do.call(c, stages)
    returns <- do.call(c, .GlobalEnv$.gratia_benchmark_returns)
    data.frame(workload = names(jobs)[i],
      model_bytes = as.numeric(object.size(models[[i]])),
      stages = length(stages),
      tasks = length(tasks),
      task_bytes = sum(vapply(stages, function(x) length(serialize(x, NULL)), integer(1))),
      sent_task_bytes = sum(vapply(tasks, function(x) length(serialize(x, NULL)), integer(1))),
      largest_task_bytes = max(vapply(tasks, function(x) length(serialize(x, NULL)), integer(1))),
      returned_task_bytes = sum(vapply(returns, function(x) length(serialize(x, NULL)), integer(1))),
      largest_return_bytes = max(vapply(returns, function(x) length(serialize(x, NULL)), integer(1))),
      output_bytes = as.numeric(object.size(value)))
  })
  untrace("smooth_task_map", where = asNamespace("gratia"))
  rm(".gratia_benchmark_tasks", envir = .GlobalEnv)
  rm(".gratia_benchmark_returns", envir = .GlobalEnv)
  serial <- lapply(names(jobs), function(name) {
    message("Timing serial: ", name)
    replicate(repetitions, measure(jobs[[name]])$elapsed)
  })
  daemon_startup <- system.time({
    mirai::daemons(2L, dispatcher = FALSE, autoexit = NA)
  })[["elapsed"]]
  package_startup <- system.time({
    setup <- mirai::everywhere({
      .libPaths(libraries)
      loadNamespace("gratia", lib.loc = dirname(path))
      invisible(Sys.getpid())
    }, libraries = .libPaths(), path = getNamespaceInfo(asNamespace("gratia"), "path"))
    worker_pids <- unlist(mirai::collect_mirai(setup, options = ".stop"))
  })[["elapsed"]]
  stopifnot(length(unique(worker_pids)) == 2L,
    !Sys.getpid() %in% worker_pids)
  first <- lapply(names(jobs), function(name) {
    message("Timing first parallel call: ", name)
    measure(jobs[[name]])
  })
  parallel <- lapply(names(jobs), function(name) {
    message("Timing warmed parallel: ", name)
    replicate(repetitions, measure(jobs[[name]])$elapsed)
  })
  results <- do.call(rbind, sizes)
  results$fit_rows <- nrow(d)
  results$grid_rows <- c(100L, 6000L, 100L, 6000L, 100L, 1000L, 100L, 1800L)
  results$posterior_draws <- c(NA_integer_, NA_integer_, NA_integer_,
    NA_integer_, 20L, 500L, NA_integer_, NA_integer_)
  results$interval_simulations <- c(NA_integer_, 3000L, NA_integer_, 3000L,
    NA_integer_, NA_integer_, NA_integer_, 3000L)
  results$serial_seconds <- vapply(serial, median, numeric(1))
  results$parallel_first_seconds <- vapply(first, `[[`, numeric(1), "elapsed")
  results$parallel_warm_seconds <- vapply(parallel, median, numeric(1))
  results$workers <- 2L
  results$repetitions <- repetitions
  results$daemon_startup_seconds <- daemon_startup
  results$package_startup_seconds <- package_startup
  results$speedup <- results$serial_seconds / results$parallel_warm_seconds
  write.csv(results, "notes/parallel-smooth-operations/results.csv", row.names = FALSE)
  timings <- do.call(rbind, lapply(seq_along(jobs), function(i) {
    data.frame(workload = names(jobs)[i],
      mode = rep(c("serial", "parallel"), each = repetitions),
      repetition = rep(seq_len(repetitions), 2L),
      elapsed_seconds = c(serial[[i]], parallel[[i]]))
  }))
  write.csv(timings, "notes/parallel-smooth-operations/timings.csv", row.names = FALSE)
  writeLines(trimws(c(capture.output(sessionInfo()),
    paste("Physical cores:", parallel::detectCores(logical = FALSE)),
    paste("Logical cores:", parallel::detectCores(logical = TRUE)),
    paste("Development package path:", package_path),
    paste("Worker processes:", paste(worker_pids, collapse = ", ")),
    paste("Daemon startup (seconds):", daemon_startup),
    paste("Worker package loading (seconds):", package_startup)), which = "right"),
    "notes/parallel-smooth-operations/session.txt")
  print(results, row.names = FALSE)
  invisible(results)
}
benchmark_smooth_operations()
