# Reproduce from the repository root; --load-all uses the current checkout.
args <- commandArgs(TRUE)
if ("--load-all" %in% args) pkgload::load_all(quiet = TRUE) else library("gratia")
ns <- asNamespace("gratia")
setup_smoothing <- get("sampling_smoothing_setup", ns)
conditional_smoothing <- get("sampling_smoothing_conditional", ns)
draw_smoothing <- get("sampling_smoothing_draws", ns)
sampling_smoothing_family_bindings <- get("sampling_smoothing_family_bindings", ns)
info_smoothing <- get("sampling_smoothing_info", ns)
source("tests/testthat/helper-smoothing-families.R")
families <- c("poisson", "binomial", "Gamma", "quasi", "inverse.gaussian", "negbin",
  "Tweedie", "nb", "betar", "scat", "tw", "cnorm", "clog", "cpois", "bcg",
  "ocat", "ziP", "gfam", "gfam_ocat", "quasipoisson", "quasibinomial",
  "gaussian_log", "poisson_sqrt", "binomial_probit", "binomial_matrix",
  "binomial_factor")
rows <- list()
for (name in families) for (engine in c("gam", "bam")) {
  for (method in if (engine == "gam") c("ML", "REML") else c("ML", "REML", "fREML")) {
    label <- paste(name, engine, method, sep = "_")
    warnings <- character()
    row <- data.frame(family = name, engine = engine, requested = method,
      actual = NA_character_, status = "", observations = NA_integer_,
      coefficients = NA_integer_, estimated_family_parameters = NA_integer_,
      family_covariance_positions = "", log_sp_positions = "", log_scale_positions = "",
      mean_error = NA_real_, covariance_error = NA_real_, setup_seconds = NA_real_,
      draws = 25L, sampling_seconds = NA_real_, warnings = "")
    withCallingHandlers(tryCatch({
      fixture <- smoothing_family_fixture(if (name == "quasi") "Gamma" else name, engine, method)
      fit <- fixture$model
      if (name == "quasi") fit <- get(engine, asNamespace("mgcv"))(
        fixture$formula, data = fixture$data, weights = w,
        family = stats::quasi(link = "log", variance = "mu^2"), method = method,
        control = mgcv::gam.control(epsilon = 1e-8, maxit = 100L))
      row$actual <- fit$method
      row$observations <- nrow(fit$model)
      row$coefficients <- length(coef(fit))
      info <- info_smoothing(fit)
      row$estimated_family_parameters <- info$family_parameters$estimated
      row$family_covariance_positions <- paste(info$V_sp_layout$family, collapse = ",")
      row$log_sp_positions <- paste(info$V_sp_layout$log_sp, collapse = ",")
      row$log_scale_positions <- paste(info$V_sp_layout$log_scale, collapse = ",")
      row$setup_seconds <- system.time(setup <- setup_smoothing(fit))[["elapsed"]]
      row$mean_error <- setup$audit$mean_error
      row$covariance_error <- setup$audit$covariance_error
      changed <- conditional_smoothing(setup, setup$log_sp + .15)
      stopifnot(all(is.finite(changed$mean)))
      row$sampling_seconds <- system.time(draws <- draw_smoothing(setup,
        n_draws = row$draws, seed = 719L))[["elapsed"]]
      stopifnot(all(is.finite(draws$bs)))
      row$status <- "OK"
    }, error = function(e) row$status <<- conditionMessage(e)), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    })
    row$warnings <- paste(unique(warnings), collapse = "; ")
    rows[[label]] <- row
    cat(label, row$status, "\n"); flush.console()
  }
}
results <- do.call(rbind, rows)
rownames(results) <- NULL
write.csv(results, "notes/posterior-chains/smoothing-families.csv", row.names = FALSE)
ok <- results$status == "OK"
summary <- c("# Ordinary and extended-family smoothing uncertainty", "",
  sprintf("Investigated with R %s and mgcv %s. This work remains internal.",
    getRversion(), packageVersion("mgcv")), "",
  sprintf("%d of %d family/fitter/method combinations passed conditional reconstruction, a perturbed-smoothing update and %d mixture draws. See [the complete results](smoothing-families.csv) for parameter positions, native warnings, failures and timings.",
    sum(ok), nrow(results), results$draws[1L]), "",
  sprintf("Across passing fits, the largest relative reconstruction errors were %.3g for coefficients and %.3g for covariance. Each fit uses %d observations (%d for ziP), offsets and unequal weights except the ziP example. Fits use epsilon = 1e-8 and maxit = 100; default BAM tolerances did not reconstruct the beta-regression ML fit accurately enough. These are individual local timings, not a repeated-simulation calibration study.",
    max(results$mean_error[ok]), max(results$covariance_error[ok]), 320L, 600L), "",
  "## Conditional calculations", "",
  "The Gaussian identity path retains its cached exact conditional distribution. Other ordinary and extended families use mgcv's native normal approximation for coefficients at each sampled smoothing value. Setup caches the original fitted design as a matrix term and the original penalty blocks, preserving coefficient coordinates, unpenalised directions and smoothing mappings. It does not reconstruct data expressions, knots or constraints from the fitting call. The native iteratively reweighted fit updates coefficients and their covariance, with dispersion and family parameters fixed at their fitted values.", "",
  "Raw model-frame responses and weights are essential. Passing initialised binomial responses and prior weights back through family initialisation would multiply trial counts twice. Censoring limits, grouped-family labels, factors, retained fitting rows and fitting offsets are carried into the conditional setup. Copies of family environments prevent updates to fitted objects. For grouped families, initialisation can propose new cutpoints; these are replaced by the original fitted parameters before coefficient fitting.", "",
  "Some native family callbacks resolve helpers through the search path. The copied environments receive missing mgcv helper functions so sampling does not require attaching mgcv. This leaves the original fitted family's environments unchanged. A native initialiser that replaces the family object is rejected explicitly.", "",
  "Ordinary fixed-theta negbin fits can have nonunit fitted dispersion in bam, while gam forces unit dispersion. The native mode is unchanged; its coefficient covariance is rescaled to the original fitted dispersion. The reconstruction check independently verifies both quantities.", "",
  "## Families and covariance ordering", "",
  "The inventory covers ordinary Gaussian, Poisson, binomial, Gamma, inverse Gaussian, fixed-theta negative binomial and fixed-power Tweedie families; general quasi, quasi-Poisson and quasi-binomial; and non-default Gaussian, Poisson and binomial links. Binomial tests include proportions with trial weights, success/failure matrices and factors. Quasi families supply an approximate sampling distribution based on quasi-likelihood, rather than a likelihood posterior.", "",
  "All installed extended-family constructors are exercised: betar, cnorm, clog, cpois, bcg, nb, ocat, scat, tw and ziP, plus gfam with ordinary and extended components. The censoring examples include exact, left, right and interval observations. Grouped-family examples include a Gaussian scale and either negative-binomial theta or ordinal cutpoints.", "",
  "| Family | Fitted nuisance parameters retained during every update |",
  "| --- | --- |",
  "| betar | Beta precision |",
  "| cnorm, clog | Normal standard deviation or logistic scale, stored on the log scale |",
  "| cpois | None |",
  "| bcg | Box-Cox power and log standard deviation |",
  "| nb | Negative-binomial size, stored on the log scale |",
  "| ocat | Log cutpoint increments; first cutpoint fixed at -1 |",
  "| scat | Transformed Student degrees of freedom and scale |",
  "| tw | Transformed Tweedie power, plus separately fitted global dispersion |",
  "| ziP | Intercept and transformed slope linking presence to log Poisson intensity |",
  "| gfam | Component family parameters and any component dispersions |", "",
  "For gam, estimated family parameters precede log smoothing parameters in V.sp and its mapping. A separate log dispersion follows where estimated. Non-discrete bam updates family parameters separately: they are absent from V.sp, sp.vcov and db.drho. Its fREML V.sp also omits separate dispersion. Family-fixed scale can override bam's scale.estimated flag: for extended families a separate dispersion is present only when family$scale is negative. Component scales in gfam belong to its family-parameter vector. The audit records these distinctions. For gam, the free log-smoothing block is marginal within the retained joint covariance. BAM has no joint family-parameter block from which to recover that dependence; its retained smoothing covariance is used as supplied. Neither fitter generates joint nuisance-parameter draws, and equivalence of these covariance approximations is not assumed.", "",
  "## Restrictions and validation", "",
  "The censored-Poisson bam examples fail the reconstruction check: native conditional refits at the fitted smoothing values do not reproduce the fitted coefficients. [Additional probes](smoothing-family-rejections.csv) with vector responses, unit weights, absent offsets and tighter fitting tolerances also failed. This configuration remains rejected; no different mode or covariance is silently substituted. See the CSV for native convergence warnings. Censored-Poisson gam fits pass the same independent conditional-refit tests. An [independent uncensored Poisson check](smoothing-cpois-check.txt) gives a maximum absolute penalised-objective gradient of 193 at the BAM coefficients versus 6.8e-8 at a fixed-smoothing ordinary Poisson reference. The objective falls from 357.2 to 315.4. Thus the disagreement is also present without the conditional reconstruction code; accepting the original covariance without checking the fitted mode would conceal it. Reproduce with Rscript notes/posterior-chains/smoothing-cpois-check.R --load-all.", "",
  "General families with several linear predictors remain outside this extension. Only actual ML, REML and fREML fits with identifiable covariance ordering are accepted. NCV and QNCV remain excluded. Discrete bam, additional penalties, dependent observations, minimum smoothing parameters, edge-corrected covariance and unsupported fitting constraints retain their existing errors. Non-Gaussian updates cache a dense design, with a default limit of 20 million entries; the internal dense_limit control can change that limit. Gaussian identity updates still use bounded row blocks and cached crossproducts.", "",
  "Very diffuse normal approximations can produce numerically impossible smoothing draws or failed conditional fits. Such draws stop with an indexed error; they are not clipped, discarded or replaced. Every accepted native conditional must converge and have finite coefficients and positive definite covariance. Setup also checks the coefficients and covariance at the fitted smoothing values before generating any draws. If that check fails, the error reports both discrepancies and suggests tighter fitting tolerances; it never relaxes agreement automatically.", "",
  "Tests compare changed-smoothing predictions and their joint covariance against independent fixed-parameter fits, accounting for different coefficient bases in gam and bam. They cover shared and partly fixed smoothing parameters, tensor products, ML/REML/fREML ordering, weights, offsets, missing rows, family state, deterministic draws and zero log-smoothing covariance. Existing exact Gaussian tests and seeded results remain unchanged.", "",
  "This mixture propagates an approximate distribution of smoothing estimates through conditional normal coefficient distributions. Coefficient distributions are recomputed at sampled smoothing values with the original fitted dispersion and family parameters. No joint nuisance-parameter draws are made. It is approximate estimation-uncertainty propagation, not Bayesian integration. The next statistical step is repeated-simulation validation of intervals against conditional draws and mgcv's unconditional correction, including difficult and weakly identified fits. Public integration should follow that assessment.", "",
  "Reproduce: Rscript notes/posterior-chains/smoothing-families.R --load-all")
writeLines(summary, "notes/posterior-chains/smoothing-families.md")

# Separate failures in the native fitting configuration from family dispatch.
# These probes are retained even when reconstruction is rejected.
rejections <- list()
base <- smoothing_family_fixture("poisson")
for (weight in c("unit", "unequal")) for (offset in c(FALSE, TRUE)) {
  for (censor in c(FALSE, TRUE)) {
    d <- base$data
    if (censor) {
      bounds <- cbind(d$y, d$y)
      j <- d$y > 5
      bounds[j, 1] <- 4.5; bounds[j, 2] <- Inf
      d$y <- bounds
    }
    if (weight == "unit") d$w <- rep(1, nrow(d))
    if (!offset) d$o <- rep(0, nrow(d))
    warnings <- character()
    status <- withCallingHandlers(tryCatch({
      fit <- mgcv::bam(y ~ s(x, k = 5) + s(z, k = 5) + offset(o), data = d,
        weights = w, family = sampling_smoothing_family_bindings(mgcv::cpois()),
        method = "fREML", control = mgcv::gam.control(epsilon = 1e-10, maxit = 200L))
      setup_smoothing(fit)
      "OK"
    }, error = function(e) conditionMessage(e)), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    })
    label <- paste(weight, offset, censor)
    rejections[[label]] <- data.frame(family = "cpois", engine = "bam", method = "fREML",
      weights = weight, offset = offset, censored = censor, epsilon = 1e-10,
      status = status, warnings = paste(unique(warnings), collapse = "; "))
  }
}
write.csv(do.call(rbind, rejections), "notes/posterior-chains/smoothing-family-rejections.csv",
  row.names = FALSE)
