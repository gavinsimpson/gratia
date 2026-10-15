# Check that integrating newer main leaves the difficult-study MH targets and
# seeded draws unchanged. Run from the repository root, using two R processes:
# R_LIBS=/private/tmp/gratia-difficult-lib Rscript notes/posterior-chains/difficult-integration-parity.R write /private/tmp/difficult-parity.rds
# R_LIBS=/path/to/final/library Rscript notes/posterior-chains/difficult-integration-parity.R check /private/tmp/difficult-parity.rds
# The reference library must contain the c8c12350 study snapshot. The check
# library must contain the final integrated source. This is a short parity
# check, not a replacement for the recorded longer mixing study.
args <- commandArgs(TRUE)
if (length(args) != 2L || !args[1L] %in% c("write", "check")) {
  stop("Supply mode (write or check), then the reference RDS path.", call. = FALSE)
}
mode <- args[1L]
path <- args[2L]
library("gratia")
library("mgcv")

# Keep this construction identical to difficult-benchmark.R, including the
# unused z draws: changing their RNG consumption changes the simulated data.
make_model <- function(case) {
  withr::with_seed(281, {
    n_obs <- if (case == "sparse") 2000L else 1500L
    k <- switch(case, sparse = 30L, rare_poisson = 25L, sparse_high_p = 50L)
    d <- data.frame(x = runif(n_obs), z = runif(n_obs), off = runif(n_obs, -0.2, 0.2))
    eta <- 0.7 * sin(2 * pi * d$x) + d$off
    if (case == "rare_poisson") {
      eta <- eta - 3.5 - 4 * (d$x > 0.4 & d$x < 0.8)
      d$y <- rpois(n_obs, exp(eta))
      fam <- poisson()
    } else {
      eta <- eta - 6 * (d$x > 0.4 & d$x < 0.8)
      d$y <- rbinom(n_obs, 1, plogis(eta))
      fam <- binomial()
    }
    gam(y ~ s(x, k = k) + offset(off), family = fam, data = d, method = "REML")
  })
}

settings <- list(n_draws = 400L, chains = 4L, burnin = 100L, seed = 281L,
  method = "mh", n_cores = 1L, thin = 1L, t_df = 40, rw_scale = 0.25)
cases <- c("sparse", "rare_poisson", "sparse_high_p")
backends <- c("mgcv", "gratia", "family")
results <- setNames(lapply(cases, function(case) {
  model <- make_model(case)
  X <- predict(model, newdata = model$model, type = "lpmatrix")
  # Store fitted target inputs as well as samples: exact checks should not
  # depend solely on whether a short chain happens to visit changed regions.
  target <- list(coefficients = coef(model), smoothing = model$sp,
    dispersion = model$sig2, response = model$y, weights = model$prior.weights,
    offset = model$offset, design = X, covariance = model$Vp)
  samplers <- setNames(lapply(backends, function(backend) {
    b <- do.call(post_draws, c(list(model = model, mh_backend = backend), settings))
    sampling <- attr(b, "sampling")
    stopifnot(nrow(b) == settings$n_draws,
      identical(sampling$draw_ids$.draw, seq_len(settings$n_draws)),
      all(table(sampling$draw_ids$.chain) == settings$n_draws / settings$chains))
    values <- matrix(as.numeric(b), nrow = nrow(b), dimnames = dimnames(b))
    info <- sampling$chains
    info <- info[, setdiff(names(info), "elapsed"), drop = FALSE]
    list(values = values, draw_ids = sampling$draw_ids, chains = info,
      fixed_acceptance = attr(b, "fixed_acceptance"),
      rw_acceptance = attr(b, "rw_acceptance"))
  }), backends)
  list(target = target, samplers = samplers)
}), cases)
result <- list(settings = settings, results = results)
if (mode == "write") {
  if (file.exists(path)) stop("Reference file already exists; use a fresh path.", call. = FALSE)
  saveRDS(result, path)
  cat("Saved three fitted targets and nine seeded MH results to ", path, "\n", sep = "")
} else {
  reference <- readRDS(path)
  if (!identical(reference$settings, settings)) stop("Sampling settings differ.", call. = FALSE)
  for (case in cases) {
    expected <- reference$results[[case]]
    actual <- results[[case]]
    for (component in names(actual$target)) {
      if (!identical(expected$target[[component]], actual$target[[component]])) {
        stop("Fitted target changed for ", case, ": ", component, ".\n",
          paste(all.equal(expected$target[[component]], actual$target[[component]],
            tolerance = 0), collapse = "\n"), call. = FALSE)
      }
    }
    for (backend in backends) for (component in names(actual$samplers[[backend]])) {
      if (!identical(expected$samplers[[backend]][[component]],
        actual$samplers[[backend]][[component]])) {
        stop("Seeded sampling changed for ", case, "/", backend, ": ", component, ".\n",
          paste(all.equal(expected$samplers[[backend]][[component]],
            actual$samplers[[backend]][[component]], tolerance = 0), collapse = "\n"),
          call. = FALSE)
      }
    }
  }
  cat("Exact integration parity passed: three fitted targets, nine MH results, ",
    "coefficient values, draw identities and acceptance information.\n", sep = "")
}
