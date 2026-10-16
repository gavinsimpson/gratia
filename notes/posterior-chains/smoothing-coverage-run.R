# Run in a separate R process from the repository root. No package code changes.
# Arguments: output directory, replicates (200), components (256), workers (8),
# optional comma-separated case names. Existing matching checkpoints are reused.
args <- commandArgs(TRUE)
if (!length(args)) stop("Supply a directory for raw simulation checkpoints.")
directory <- normalizePath(args[1L], mustWork = FALSE)
count <- function(index, default) if (length(args) >= index) as.integer(args[index]) else default
replicates <- count(2L, 200L); components <- count(3L, 256L); workers <- count(4L, 8L)
stopifnot(replicates > 0L, components > 0L, components %% 2L == 0L, workers > 0L)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
functions_path <- normalizePath("notes/posterior-chains/smoothing-coverage-functions.R")
source(functions_path)
cases <- coverage_cases()
if (length(args) >= 5L) cases <- cases[match(strsplit(args[5L], ",", fixed = TRUE)[[1L]], cases$case), , drop = FALSE]
stopifnot(!anyNA(cases$case))
core <- c("R/sampling-smoothing.R", "R/sampling-smoothing-family.R", "R/sampling-smoothing-audit.R")
files <- c(core, "notes/posterior-chains/smoothing-coverage-functions.R",
  "notes/posterior-chains/smoothing-coverage-run.R")
protocol <- list(replicates = replicates, components = components,
  batch_size = 5L, cases = cases, configurations = coverage_configurations(),
  source_hashes = tools::md5sum(files),
  commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  R = as.character(getRversion()), mgcv = as.character(utils::packageVersion("mgcv")),
  gratia = as.character(utils::packageVersion("gratia")),
  pointwise_level = .95, integration = "antithetic normal smoothing draws and conditional Gaussian CDFs")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
metadata_path <- file.path(directory, "protocol.rds")
if (file.exists(metadata_path)) {
  previous <- readRDS(metadata_path)
  if (!identical(previous$protocol, protocol)) stop("Checkpoint protocol or source hashes differ; use a new output directory.")
} else saveRDS(list(protocol = protocol, started = Sys.time(), library_paths = .libPaths()), metadata_path)
jobs <- list()
for (i in seq_len(nrow(cases))) for (start in seq.int(1L, replicates, by = 5L)) {
  label <- sprintf("%s-%05d", cases$case[i], start)
  destination <- file.path(directory, paste0(label, ".rds"))
  if (file.exists(destination)) {
    checkpoint <- readRDS(destination)
    stopifnot(identical(checkpoint$protocol, protocol), identical(checkpoint$label, label))
  } else jobs[[label]] <- list(spec = cases[i, , drop = FALSE],
    replicates = seq.int(start, min(replicates, start + 4L)), label = label, destination = destination)
}
cat("Protocol:", nrow(cases), "cases x", replicates, "datasets x 5 fitter configurations;",
  components, "smoothing components. Remaining batches:", length(jobs), "\n")
flush.console()
run <- function() {
  if (!length(jobs)) return(invisible(NULL))
  profile <- paste0("gratia-smoothing-coverage-", Sys.getpid())
  mirai::daemons(workers, .compute = profile)
  on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
  tasks <- mirai::mirai_map(jobs, function(job, functions_path, protocol, library_paths) {
    .libPaths(library_paths)
    source(functions_path, local = .GlobalEnv)
    result <- lapply(job$replicates, function(replicate)
      coverage_replicate(job$spec, replicate, components = protocol$components))
    temporary <- tempfile("coverage-", tmpdir = dirname(job$destination))
    on.exit(unlink(temporary), add = TRUE)
    saveRDS(list(protocol = protocol, label = job$label, results = result), temporary)
    if (!file.rename(temporary, job$destination)) stop("Cannot save checkpoint: ", job$destination)
    job$label
  }, .args = list(functions_path = functions_path, protocol = protocol, library_paths = .libPaths()), .compute = profile)
  last <- -1L
  repeat {
    remaining <- sum(vapply(tasks, mirai::unresolved, logical(1L)))
    done <- length(tasks) - remaining
    if (done != last) {
      cat(format(Sys.time(), "%H:%M:%S"), "completed", done, "of", length(tasks), "batches\n")
      flush.console(); last <- done
    }
    if (!remaining) break
    Sys.sleep(2)
  }
  result <- tasks[]
  errors <- vapply(result, function(x) inherits(x, c("miraiError", "errorValue")), logical(1L))
  if (any(errors)) stop("Worker failures: ", paste(capture.output(print(result[errors])), collapse = " "))
}
run()
cat("Finished:", directory, "\n")
