# Run from the repository root after installing this checkout, or with
# Rscript notes/posterior-chains/smoothing-study.R --load-all.
# Optional second argument: retained draws per model (default 20000).
args <- commandArgs(trailingOnly = TRUE)
if ("--load-all" %in% args) pkgload::load_all(quiet = TRUE)
library("gratia")
library("mgcv")
ns <- asNamespace("gratia")
setup_smoothing <- get("sampling_smoothing_setup", ns)
conditional_smoothing <- get("sampling_smoothing_conditional", ns)
draw_smoothing <- get("sampling_smoothing_draws", ns)
n_arg <- args[grepl("^[0-9]+$", args)]
n <- if (length(n_arg)) as.integer(n_arg[1]) else 20000L
if (n < 1000L) stop("Use at least 1000 draws for the smoothing study.")
set.seed(517)
d <- data.frame(x = runif(600), z = runif(600), w = runif(600, .5, 2))
d$o <- .2 + .3 * d$x
d$y <- sin(6 * d$x) + cos(3 * d$z) + d$o + rnorm(600, sd = .4)
f <- y ~ s(x, k = 6) + s(z, k = 6) + offset(o)
models <- list(
  gam_REML = gam(f, data = d, weights = w, method = "REML"),
  gam_ML = gam(f, data = d, weights = w, method = "ML"),
  bam_fREML = bam(f, data = d, weights = w, method = "fREML"),
  bam_ML = bam(f, data = d, weights = w, method = "ML"),
  bam_REML = bam(f, data = d, weights = w, method = "REML"),
  linked_bam_ML = bam(y ~ s(x, k = 6, id = "shared") +
    s(z, k = 6, id = "shared") + offset(o), data = d,
    weights = w, method = "ML"),
  partly_fixed_bam_ML = bam(y ~ s(x, k = 6, sp = .1) +
    s(z, k = 6) + offset(o), data = d, weights = w, method = "ML"),
  linked_gam = gam(y ~ s(x, k = 6, id = "shared") +
    s(z, k = 6, id = "shared") + offset(o), data = d,
    weights = w, method = "REML"),
  linked_bam = bam(y ~ s(x, k = 6, id = "shared") +
    s(z, k = 6, id = "shared") + offset(o), data = d,
    weights = w, method = "fREML"),
  partly_fixed = gam(y ~ s(x, k = 6, sp = .1) + s(z, k = 6) + offset(o),
    data = d, weights = w, method = "REML"),
  tensor = gam(y ~ te(x, z, k = c(4, 4)) + offset(o),
    data = d, weights = w, method = "REML"),
  discrete_bam = bam(f, data = d, weights = w, method = "fREML", discrete = TRUE))
results <- vector("list", length(models))
names(results) <- names(models)
mixtures <- setups <- list()
for (nm in names(models)) {
  m <- models[[nm]]
  time_setup <- system.time(setup <- tryCatch(setup_smoothing(m), error = identity))[["elapsed"]]
  if (inherits(setup, "error")) {
    raw_V <- m$V.sp
    raw_sp_vcov <- mgcv::sp.vcov(m, edge.correct = FALSE)
    q <- length(m$sp)
    results[[nm]] <- data.frame(model = nm, supported = FALSE,
      draws = 0L, coefficients = length(coef(m)), free_sp = q,
      V_sp_dimension = nrow(raw_V),
      V_sp_log_scale_position = if (nrow(raw_V) == q + 1L) q + 1L else NA_integer_,
      sp_vcov_dimension = nrow(raw_sp_vcov),
      sp_vcov_log_scale_position = if (nrow(raw_sp_vcov) == q + 1L) q + 1L else NA_integer_,
      reason = conditionMessage(setup))
    next
  }
  timing <- system.time(mixture <- draw_smoothing(setup, n_draws = n, seed = 612))[["elapsed"]]
  setups[[nm]] <- setup
  mixtures[[nm]] <- mixture
  audit <- setup$audit
  conditional <- audit$conditional_covariance
  corrected <- audit$corrected_covariance
  results[[nm]] <- data.frame(model = nm, supported = TRUE, reason = "",
    draws = n, coefficients = ncol(conditional), free_sp = length(m$sp),
    V_sp_dimension = audit$V_sp_dimension,
    V_sp_log_scale_position = audit$V_sp_log_scale_position,
    sp_vcov_dimension = audit$sp_vcov_dimension,
    sp_vcov_log_scale_position = audit$sp_vcov_log_scale_position,
    mean_error = audit$mean_error, covariance_error = audit$covariance_error,
    raw_db_drho_error = audit$derivative_error,
    raw_db_drho_coordinates = audit$fitted_derivative_coordinates,
    setup_seconds = time_setup, sampling_seconds = timing,
    mean_sd_conditional = mean(sqrt(diag(conditional))),
    mean_sd_unconditional = mean(sqrt(diag(corrected))),
    mean_sd_mixture = mean(sqrt(diag(mixture$total_covariance))),
    mean_sd_empirical = mean(sqrt(diag(cov(mixture$bs)))),
    total_variance_empirical_relative_error =
      norm(cov(mixture$bs) - mixture$total_covariance, "F") /
        norm(mixture$total_covariance, "F"),
    fraction_between_conditional_means =
      sum(diag(mixture$covariance_conditional_means)) /
        sum(diag(mixture$total_covariance)))
  cat(nm, ": ", round(timing, 3), " seconds\n", sep = "")
}
fields <- unique(unlist(lapply(results, names)))
results <- do.call(rbind, lapply(results, function(x) {
  x[setdiff(fields, names(x))] <- NA
  x[fields]
}))
rownames(results) <- NULL
write.csv(results, "notes/posterior-chains/smoothing-results.csv", row.names = FALSE)
# Independent one-dimensional numerical integration for a linked smoothing
# parameter. Integrate a scalar coefficient's first and second moments under
# the log-normal mixture, rather than checking only sample covariance.
setup <- setups$linked_gam
mixture <- mixtures$linked_gam
j <- which.max(diag(mixture$total_covariance))
center <- setup$log_sp[1]
sd_rho <- sqrt(setup$log_sp_cov[1, 1])
integrand <- function(z, moment) vapply(z, function(zi) {
  conditional <- conditional_smoothing(setup, center + sd_rho * zi)
  value <- if (moment == 1L) conditional$mean[j] else
    conditional$covariance[j, j] + conditional$mean[j]^2
  value * dnorm(zi)
}, numeric(1L))
mean_exact <- integrate(integrand, -9, 9, moment = 1L, rel.tol = 1e-9)$value
second_exact <- integrate(integrand, -9, 9, moment = 2L, rel.tol = 1e-9)$value
variance_exact <- second_exact - mean_exact^2
quadrature <- data.frame(coefficient = setup$names[j],
  quadrature_mean = mean_exact, quadrature_variance = variance_exact,
  mixture_mean = mean(mixture$conditional_means[, j]),
  mixture_total_variance = mixture$total_covariance[j, j],
  empirical_mean = mean(mixture$bs[, j]), empirical_variance = var(mixture$bs[, j]))
write.csv(quadrature, "notes/posterior-chains/smoothing-quadrature.csv", row.names = FALSE)
# Compare cached conditional solves with fixed-sp refits on identical draws.
# Keep model fitting and proposal generation outside both measured operations.
rho <- mixtures$gam_REML$log_sp[seq_len(100L), , drop = FALSE]
setup <- setups$gam_REML
cached <- system.time(cached_means <- t(vapply(seq_len(nrow(rho)), function(i) {
  conditional_smoothing(setup, rho[i, ])$mean
}, numeric(ncol(setup$XtWX)))))[["elapsed"]]
refitting <- system.time(refit_means <- t(vapply(seq_len(nrow(rho)), function(i) {
  coef(gam(f, data = d, weights = w, method = "REML", sp = exp(rho[i, ]),
    scale = models$gam_REML$sig2))
}, numeric(ncol(setup$XtWX)))))[["elapsed"]]
comparison <- data.frame(conditional_updates = nrow(rho),
  cached_seconds = cached, refit_seconds = refitting,
  max_coefficient_difference = max(abs(cached_means - refit_means)))
write.csv(comparison, "notes/posterior-chains/smoothing-update-timings.csv", row.names = FALSE)
version <- paste0("R ", getRversion(), "; mgcv ", packageVersion("mgcv"))
supported <- results[results$supported, ]
rows <- vapply(seq_len(nrow(supported)), function(i) {
  x <- supported[i, ]
  sprintf("| %s | %d | %.4f | %.4f | %.4f | %.1f%% | %.2f |",
    x$model, x$free_sp, x$mean_sd_conditional, x$mean_sd_unconditional,
    x$mean_sd_mixture, 100 * x$fraction_between_conditional_means, x$sampling_seconds)
}, character(1L))
report <- c("# Internal smoothing-parameter uncertainty study", "",
  paste0("Generated with ", version, ". Each supported fit uses ", n,
    " draws; 600 fitting observations; offsets and unequal prior weights."), "",
  "The research implementation samples an approximate fitted marginal normal distribution for the free log-smoothing parameters, then samples the exact Gaussian conditional coefficient distribution at each value. Dispersion stays fixed. This mixture uses an approximate normal distribution of fitted log-smoothing parameters; it does not specify hyperpriors or sample a joint posterior for coefficients, smoothing parameters, and dispersion.", "",
  "## Covariance and coordinate audit", "",
  "For these Gaussian gam fits, V.sp contains free log-smoothing parameters first and an additional log-dispersion component last when dispersion is estimated. fREML bam's V.sp omits the log-dispersion component, although sp.vcov() includes it. The prototype uses the marginal free-log-smoothing covariance block and holds dispersion at its fitted value; it does not condition the log-smoothing approximation on a particular log-dispersion draw.", "",
  "Shared and partly fixed smoothing parameters are expanded with attr(V.sp, 'L'), with fixed multipliers retained in an additive log-scale offset. attr(V.sp, 'spind') is not used as a free-parameter index: for the shared gam example it includes a position belonging to log dispersion. The old feasibility prototype did not handle this case.", "",
  "In the installed mgcv, fREML bam stores db.drho before transforming derivatives to the fitted coefficient basis. Those raw derivatives disagree with derivatives of fitted coefficient means. The prototype calculates derivatives in fitted coordinates from the conditional precision, independently verified by centred numerical differences. gam and ML/REML bam derivatives agree with db.drho after the smoothing mapping is applied. Vc already uses the transformations internally and remains the reference for unconditional = TRUE.", "",
  sprintf("Across supported fits, the largest relative coefficient error at fitted sp was %.2g and the largest relative conditional covariance error was %.2g.",
    max(supported$mean_error), max(supported$covariance_error)), "",
  "## Marginal uncertainty", "",
  "The table averages marginal coefficient standard deviations. Mixture variances use E[Var(beta | log sp)] + Var[E(beta | log sp)], separating conditional uncertainty from variation in conditional means. The nonlinear mixture and mgcv's unconditional correction are different approximations, so agreement is assessed rather than assumed. A positive between-mean contribution does not guarantee a larger variance than at fitted sp: averaging the conditional covariance can reduce that component, as in the linked examples.", "",
  "| Fit | Free sp | Conditional SD | mgcv unconditional SD | Mixture SD | Variance from conditional means | Sampling seconds |",
  "| --- | ---: | ---: | ---: | ---: | ---: | ---: |", rows, "",
  sprintf("For the linked gam coefficient %s, independent numerical integration gives mean %.6f and variance %.6f; mixture conditional moments give %.6f and %.6f, and sampled coefficients give %.6f and %.6f.",
    quadrature$coefficient, quadrature$quadrature_mean, quadrature$quadrature_variance,
    quadrature$mixture_mean, quadrature$mixture_total_variance,
    quadrature$empirical_mean, quadrature$empirical_variance), "",
  sprintf("Across supported fits, the largest relative difference between empirical coefficient covariance and the total-variance calculation was %.1f%%. This difference includes coefficient-sampling Monte Carlo error.",
    100 * max(supported$total_variance_empirical_relative_error)), "",
  "## Cached conditional updates", "",
  "The design matrix is evaluated in bounded row blocks once. Weighted crossproducts, penalty blocks, smoothing mappings, offsets, and the fitted dispersion are cached; each subsequent update solves a coefficient-sized precision system. It does not refit the model or re-evaluate model-call data or weight expressions.", "",
  sprintf("For 100 identical log-smoothing draws, cached conditional means took %.3f seconds versus %.3f seconds for fixed-sp refits; the largest coefficient difference was %.2g. These are single-run local timings, not a general speed guarantee.",
    comparison$cached_seconds, comparison$refit_seconds, comparison$max_coefficient_difference), "",
  "## Remaining restrictions", "",
  "The prototype remains internal and supports identifiable independent Gaussian identity gam and non-discrete bam fits with ML, REML, or fREML covariance information. BAM fits with nonzero prescribed smooth contributions, additional penalties, minimum smoothing parameters, edge-corrected smoothing covariances, prefit objects, GAMM components, dependent observations, nonpositive penalty multipliers, and unsupported mappings are rejected. Numerically invalid smoothing draws fail explicitly rather than being silently discarded or clipped.", "",
  "Discrete bam is audited but not sampled: its compressed fitting design can define a different conditional target from evaluation at the original covariates. Reconstructing the compressed design and covariance consistently is required before extending this prototype. The subsequent [ordinary and extended-family extension](smoothing-families.md) supplies native conditional normal updates at fixed fitted nuisance parameters; this report retains the original Gaussian study.", "",
  "The log-smoothing covariance is a fitted approximation. Boundary fits and very diffuse approximations can require more careful treatment. Dispersion uncertainty and its dependence with smoothing parameters remain separate work; proposal covariance changes in conditional MH still do not integrate smoothing uncertainty.", "",
  "See [smoothing-methods.md](smoothing-methods.md) for the ML/REML/fREML fitting-method audit and family-parameter ordering. These internal draws propagate approximate estimation uncertainty; they do not perform Bayesian integration.", "",
  "Reproduce with: Rscript notes/posterior-chains/smoothing-study.R --load-all 20000",
  "", "Session details:", "", "```", capture.output(sessionInfo()), "```"
)
writeLines(trimws(report, which = "right"), "notes/posterior-chains/smoothing-report.md")
