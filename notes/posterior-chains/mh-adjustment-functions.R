# Research only. Reuse the fixed data-generating cases from the coverage study.
source("notes/posterior-chains/smoothing-coverage-functions.R")

adjustment_study_summary <- function(draws, fit, fixture) {
  projection <- coverage_projection(fit, fixture)
  values <- draws %*% t(projection$A)
  values <- sweep(values, 2L, projection$offset, "+")
  points <- projection$points
  values <- cbind(values, fixture$definition$inverse(values[, seq_len(points)]))
  colnames(values) <- as.vector(outer(seq_len(points), colnames(fixture$truth),
    function(i, q) paste0(q, "_", i)))
  meta <- attr(draws, "sampling")
  attr(values, "sampling") <- list(draw_ids = meta$draw_ids)
  diagnostic <- posterior::summarise_draws(gratia::as_mcmc_draws(values),
    "rhat", "ess_bulk", "ess_tail", "mcse_mean")
  q <- t(apply(values, 2L, stats::quantile, probs = c(.025, .5, .975), names = FALSE))
  truth <- as.vector(fixture$truth)
  intervals <- data.frame(quantity = rep(colnames(fixture$truth), each = points),
    point = rep(seq_len(points), ncol(fixture$truth)),
    x = rep(fixture$evaluation$x, ncol(fixture$truth)),
    truth = truth, lower = q[, 1L], median = q[, 2L], upper = q[, 3L])
  intervals$covered <- intervals$lower <= truth & intervals$upper >= truth
  list(intervals = intervals, diagnostics = as.data.frame(diagnostic),
    coefficient_diagnostics = as.data.frame(gratia::sampling_diagnostics(draws)$coefficients))
}

adjustment_study_replicate <- function(case, replicate, n_draws, burnin) {
  cases <- coverage_cases()
  spec <- cases[match(case, cases$case), , drop = FALSE]
  fixture <- coverage_data(spec, replicate)
  configurations <- data.frame(engine = c("gam", "bam"), method = c("REML", "fREML"))
  results <- vector("list", nrow(configurations))
  for (i in seq_len(nrow(configurations))) {
    cfg <- configurations[i, ]
    warnings <- character()
    stage <- "fitting"
    started <- proc.time()[["elapsed"]]
    results[[i]] <- withCallingHandlers(tryCatch({
      fit <- withr::with_seed(30261015L + match(case, cases$case) * 10000L +
        replicate * 10L + if (i == 1L) 2L else 5L,
        coverage_fit(spec, fixture, cfg$engine, cfg$method))
      if (!isTRUE(fit$converged) ||
          any(grepl("converg|step failure|not positive definite", warnings,
            ignore.case = TRUE))) stop("Native fitting failed the convergence screen.")
      if (is.null(fit$Vc)) stop("The native fit has no unconditional covariance.")
      fitted_elapsed <- proc.time()[["elapsed"]] - started
      stage <- "conditional MH"
      seed <- 54261015L + match(case, cases$case) * 10000L + replicate * 10L + i
      t0 <- proc.time()[["elapsed"]]
      mh <- gratia::mh_draws(fit, n_draws = n_draws, chains = 4L, burnin = burnin,
        n_cores = 1L, mh_backend = "gratia", diagnostics = TRUE, seed = seed)
      mh_elapsed <- proc.time()[["elapsed"]] - t0
      stage <- "covariance adjustment"
      t0 <- proc.time()[["elapsed"]]
      adjusted <- get("sampling_mh_covariance_adjust", asNamespace("gratia"))(fit, mh)
      adjustment_elapsed <- proc.time()[["elapsed"]] - t0
      stage <- "unconditional Gaussian"
      t0 <- proc.time()[["elapsed"]]
      gaussian <- gratia::gaussian_draws(fit, n_draws = n_draws, chains = 4L,
        unconditional = TRUE, mvn_method = "mgcv", diagnostics = TRUE, seed = seed + 1000000L)
      gaussian_elapsed <- proc.time()[["elapsed"]] - t0
      audit <- attr(adjusted, "sampling")$covariance_adjustment
      stopifnot(identical(attr(mh, "sampling")$draw_ids, attr(adjusted, "sampling")$draw_ids),
        audit$covariance_relative_error < 1e-7,
        max(abs(colMeans(mh) - colMeans(adjusted))) < 1e-7)
      samples <- list(conditional_mh = mh, adjusted_mh = adjusted,
        unconditional_gaussian = gaussian)
      summary <- lapply(samples, adjustment_study_summary, fit = fit, fixture = fixture)
      timings <- c(conditional_mh = mh_elapsed, adjusted_mh = mh_elapsed + adjustment_elapsed,
        unconditional_gaussian = gaussian_elapsed)
      # Analytic normal intervals are a fourth control, without Monte Carlo error.
      conditional_normal <- coverage_intervals(coverage_normal(coef(fit), fit$Vp,
        coverage_projection(fit, fixture)), fixture)
      normal <- data.frame(quantity = rep(colnames(fixture$truth), each = 11),
        point = rep(1:11, 4), x = rep(fixture$evaluation$x, 4),
        truth = as.vector(fixture$truth),
        lower = as.vector(conditional_normal[, "lower", ]),
        median = as.vector(conditional_normal[, "median", ]),
        upper = as.vector(conditional_normal[, "upper", ]))
      normal$covered <- normal$lower <= normal$truth & normal$upper >= normal$truth
      summary$conditional_gaussian <- list(intervals = normal)
      list(case = case, replicate = replicate, engine = cfg$engine, method = cfg$method,
        status = "OK", warnings = unique(warnings), fitted_elapsed = fitted_elapsed,
        sampling_elapsed = timings, adjustment_elapsed = adjustment_elapsed,
        chains = gratia::sampling_diagnostics(mh)$chains,
        covariance_relative_error = audit$covariance_relative_error,
        mean_shift = max(abs(colMeans(mh) - coef(fit))),
        eigenvalue_ratios = range(eigen(solve(cov(mh), fit$Vc), only.values = TRUE)$values),
        log_sp = log(fit$sp), summary = summary)
    }, error = function(e) list(case = case, replicate = replicate, engine = cfg$engine,
      method = cfg$method, status = conditionMessage(e), stage = stage,
      warnings = unique(warnings))), warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
      })
  }
  results
}
