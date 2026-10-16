# Outside-sandbox runner: output directory, workers (4), replicates (200),
# retained draws (16000), burn-in per chain (4000). Use a fresh pilot directory.
args <- commandArgs(TRUE)
stopifnot(length(args) >= 1L)
directory <- normalizePath(args[1L], mustWork = FALSE)
workers <- if (length(args) >= 2L) as.integer(args[2L]) else 4L
count <- if (length(args) >= 3L) as.integer(args[3L]) else 200L
n_draws <- if (length(args) >= 4L) as.integer(args[4L]) else 16000L
burnin <- if (length(args) >= 5L) as.integer(args[5L]) else 4000L
stopifnot(count > 0L, n_draws > 8L, n_draws %% 4L == 0L, workers > 0L, burnin >= 0L)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
source("notes/posterior-chains/mh-adjustment-functions.R")
files <- c("R/sampling-mh-adjustment.R", "R/samplers.R", "R/sampling-target.R",
  "src/sampling.cpp", "notes/posterior-chains/mh-adjustment-functions.R",
  "notes/posterior-chains/mh-adjustment-run.R",
  "notes/posterior-chains/smoothing-coverage-functions.R")
files <- files[file.exists(files)]
protocol <- list(count = count, n_draws = n_draws, chains = 4L, burnin = burnin,
  source_hashes = tools::md5sum(files), R = as.character(getRversion()),
  mgcv = as.character(utils::packageVersion("mgcv")),
  posterior = as.character(utils::packageVersion("posterior")),
  cases = c("normal_known", "normal_weak", "normal_null", "binomial_rare"),
  engine = c("gam REML", "bam fREML"), rng = c("L'Ecuyer-CMRG", "Inversion", "Rejection"))
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
meta <- file.path(directory, "protocol.rds")
if (file.exists(meta)) stopifnot(identical(readRDS(meta)$protocol, protocol)) else
  saveRDS(list(protocol = protocol, library_paths = .libPaths()), meta)
jobs <- list()
for (case in protocol$cases) for (start in seq.int(1L, count, by = 5L)) {
  label <- sprintf("%s-%05d", case, start)
  dest <- file.path(directory, paste0(label, ".rds"))
  if (file.exists(dest)) stopifnot(identical(readRDS(dest)$protocol, protocol)) else
    jobs[[label]] <- list(case = case, replicates = seq.int(start, min(start + 4L, count)), dest = dest)
}
run <- function() {
  if (!length(jobs)) return(invisible(NULL))
  profile <- paste0("gratia-mh-adjustment-", Sys.getpid())
  mirai::daemons(workers, .compute = profile)
  on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)
  tasks <- mirai::mirai_map(jobs, function(job, root, library_paths, protocol) {
    .libPaths(library_paths); setwd(root)
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    source("notes/posterior-chains/mh-adjustment-functions.R")
    results <- lapply(job$replicates, function(i) adjustment_study_replicate(
      job$case, i, protocol$n_draws, protocol$burnin))
    tmp <- tempfile("adjustment-", tmpdir = dirname(job$dest))
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(list(protocol = protocol, results = results), tmp)
    stopifnot(file.rename(tmp, job$dest))
    basename(job$dest)
  }, .args = list(root = normalizePath("."), library_paths = .libPaths(), protocol = protocol),
    .compute = profile)
  last <- -1L
  repeat {
    done <- length(tasks) - sum(vapply(tasks, mirai::unresolved, logical(1L)))
    if (done != last) {cat(format(Sys.time(), "%H:%M:%S"), done, "of", length(tasks), "batches\n"); flush.console(); last <- done}
    if (done == length(tasks)) break
    Sys.sleep(2)
  }
  values <- tasks[]
  if (any(vapply(values, function(x) inherits(x, c("miraiError", "errorValue")), logical(1L))))
    stop(paste(capture.output(print(values)), collapse = "\n"))
}
run()
