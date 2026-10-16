args <- commandArgs(TRUE)
repo <- if (length(args)) args[1L] else getwd()
out <- if (length(args) > 1L) args[2L] else file.path(repo, "notes", "scam-support", "results")
library("scam")
pkgload::load_all(repo, quiet = TRUE)
capture_result <- function(expr) {
  w <- msg <- character()
  val <- tryCatch(withCallingHandlers(expr, warning = function(e) {
    w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning")
  }, message = function(e) {
    msg <<- c(msg, conditionMessage(e)); invokeRestart("muffleMessage")
  }), error = identity)
  list(value = val, error = if (inherits(val, "error")) conditionMessage(val) else "",
    warnings = paste(unique(w), collapse = " | "), messages = paste(unique(msg), collapse = " | "))
}
inventory <- readRDS(file.path(out, "inventory.rds"))
d <- inventory$data
s <- mgcv::s
models <- list(
  single = inventory$models$mpi,
  multi = scam::scam(y ~ z + s(x, bs = "mpi", k = 8), data = d),
  factor = scam::scam(y ~ fac + s(x, by = fac, bs = "mpi", k = 8), data = d),
  bivariate = inventory$models$tedmi)
rows <- list()
details <- list()
grDevices::pdf(file.path(out, "api-plots.pdf"))
for (mn in names(models)) {
  cat("Model", mn, "class", class(models[[mn]]), "\n"); flush.console()
  m <- models[[mn]]
  sm <- m$smooth[[1L]]
  nd <- d[1:10, ]
  operations <- list(
    draw = function() draw(m, n = 30, n_2d = 12),
    assemble = function() assemble(m, n = 30, n_2d = 12),
    smooth_estimates = function() smooth_estimates(m, data = nd),
    smooth_estimates_conditional = function() smooth_estimates(m, data = nd, overall_uncertainty = FALSE),
    smooth_estimates_unconditional = function() smooth_estimates(m, data = nd, unconditional = TRUE),
    smooth_estimates_simultaneous = function() smooth_estimates(m, data = nd, interval = "simultaneous", n_sim = 100),
    spline_values = function() spline_values(sm, data = nd, model = m, unconditional = FALSE),
    fitted_values = function() fitted_values(m, data = nd),
    fitted_values_unconditional = function() fitted_values(m, data = nd, unconditional = TRUE),
    fitted_values_simultaneous = function() fitted_values(m, data = nd, interval = "simultaneous", n_sim = 100),
    conditional_values = function() conditional_values(m, condition = "x", n_vals = 10),
    conditional_differences = function() conditional_differences(m, by = "fac", condition = "x", n_vals = 10),
    difference_smooths = function() difference_smooths(m, n = 10),
    compare_smooths = function() compare_smooths(m, m, n = 10),
    parametric_effects = function() parametric_effects(m),
    ale = function() ale(m, select = "x", n = 10),
    derivatives = function() derivatives(m, data = nd),
    partial_derivatives = function() partial_derivatives(m, data = nd, focal = "x"),
    response_derivatives = function() response_derivatives(m, data = nd, focal = "x", n_sim = 10, seed = 7),
    response_derivatives_delta = function() response_derivatives(m, data = nd, focal = "x", uncertainty = "delta"),
    gaussian_draws = function() gaussian_draws(m, n_draws = 5, seed = 7),
    gaussian_draws_untransformed = function() gaussian_draws(m, n_draws = 5, seed = 7, parametrized = FALSE),
    post_draws = function() post_draws(m, n_draws = 5, seed = 7),
    post_draws_mh = function() post_draws(m, method = "mh", n_draws = 5, burnin = 0, seed = 7),
    post_draws_inla = function() post_draws(m, method = "inla", n_draws = 5, seed = 7),
    smooth_samples = function() smooth_samples(m, data = nd, n_draws = 5, seed = 7),
    fitted_samples = function() fitted_samples(m, data = nd, n_draws = 5, seed = 7),
    posterior_samples = function() posterior_samples(m, data = nd, n_draws = 5, seed = 7),
    predicted_samples = function() predicted_samples(m, data = nd, n_draws = 5, seed = 7),
    derivative_samples = function() derivative_samples(m, data = nd, focal = "x", n_draws = 5, seed = 7),
    simulate = function() simulate(m, nsim = 5, seed = 7),
    appraise = function() appraise(m, n_simulate = 5, seed = 7),
    appraise_scalable = function() appraise(m, scalable = TRUE, n_simulate = 5, seed = 7),
    qq_plot = function() qq_plot(m, n_simulate = 5, seed = 7),
    worm_plot = function() worm_plot(m, n_simulate = 5, seed = 7),
    observed_fitted_plot = function() observed_fitted_plot(m),
    residuals_hist_plot = function() residuals_hist_plot(m),
    residuals_linpred_plot = function() residuals_linpred_plot(m),
    residuals_covariate_plot = function() residuals_covariate_plot(m, covariate = "x"),
    residual_correlation = function() residual_correlation(m),
    quantile_residuals = function() quantile_residuals(m, seed = 7),
    dharma_residuals = function() dharma_residuals(m, n_simulate = 5, seed = 7),
    rootogram = function() rootogram(m),
    posterior_predictive_plot = function() posterior_predictive_plot(m, n = 5, seed = 7, scalable = FALSE),
    posterior_predictive_plot_scalable = function() posterior_predictive_plot(m, n = 5, seed = 7, scalable = TRUE),
    bayesplot_pp_check = function() bayesplot::pp_check(m, n_draws = 5, seed = 7),
    log_lik = function() log_lik(m, n_draws = 5, seed = 7),
    loo = function() loo::loo(m, n_draws = 5, seed = 7),
    add_fitted = function() add_fitted(d, m),
    add_residuals = function() add_residuals(d, m),
    add_fitted_samples = function() add_fitted_samples(nd, m, n_draws = 5, seed = 7),
    partial_residuals = function() partial_residuals(m),
    model_concurvity = function() model_concurvity(m),
    overview = function() overview(m),
    edf = function() edf(m),
    edf_alternative = function() edf(m, type = "alternative"),
    edf_unconditional = function() edf(m, type = "unconditional"),
    model_edf = function() model_edf(m),
    variance_comp = function() variance_comp(m),
    fixed_effects = function() fixed_effects(m),
    coef = function() coef(m),
    vcov = function() vcov(m),
    get_vcov = function() get_vcov(m),
    dispersion = function() dispersion(m),
    model_constant = function() model_constant(m),
    null_deviance = function() null_deviance(m),
    family = function() family(m),
    family_name = function() family_name(m),
    family_type = function() family_type(m),
    link = function() link(m),
    has_theta = function() has_theta(m),
    n_eta = function() n_eta(m),
    smooths = function() smooths(m),
    n_smooths = function() n_smooths(m),
    which_smooths = function() which_smooths(m, terms = smooths(m)[1L]),
    get_smooth = function() get_smooth(m, smooths(m)[1L]),
    smooth_coefs = function() smooth_coefs(m, select = smooths(m)[1L]),
    basis_size = function() basis_size(sm),
    smooth_type = function() smooth_type(sm),
    smooth_dim = function() smooth_dim(sm),
    smooth_terms = function() smooth_terms(sm),
    smooth_label = function() smooth_label(sm),
    smooth_coef_indices = function() smooth_coef_indices(sm),
    model_terms = function() model_terms(m),
    model_vars = function() model_vars(m),
    parametric_terms = function() parametric_terms(m),
    term_names = function() term_names(m),
    term_variables = function() term_variables(m),
    model_frame = function() model.frame(m),
    data_slice = function() data_slice(m, x = evenly(x, n = 10)),
    data_combos = function() data_combos(m),
    factor_combos = function() factor_combos(m),
    typical_values = function() typical_values(m),
    smooth_data = function() smooth_data(m, id = 1, n = 10),
    basis = function() basis(m, n = 10),
    penalty = function() penalty(m),
    lp_matrix = function() lp_matrix(m, data = nd)
  )
  for (op in names(operations)) {
    cat(mn, op, "\n"); flush.console()
    res <- capture_result(operations[[op]]())
    details[[paste(mn, op, sep = "/")]] <- res
    rows[[length(rows) + 1L]] <- data.frame(model = mn, operation = op,
      error = res$error, warnings = res$warnings, messages = res$messages,
      class = paste(class(res$value), collapse = ","))
  }
}
grDevices::dev.off()
write.csv(do.call(rbind, rows), file.path(out, "api-inventory.csv"), row.names = FALSE)
saveRDS(list(models = models, details = details), file.path(out, "api-details.rds"))
ns <- asNamespace("gratia")
all <- getNamespaceExports("gratia")
gam_methods <- as.data.frame(getNamespaceInfo(ns, "S3methods"))
names(gam_methods) <- c("generic", "class", "method", "unused")
write.csv(gam_methods[gam_methods$class %in% c("gam", "scam", "lm", "glm"), 1:3],
  file.path(out, "method-registrations.csv"), row.names = FALSE)
cat("Finished", length(rows), "operations\n")
