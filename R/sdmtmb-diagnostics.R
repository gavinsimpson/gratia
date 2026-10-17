# sdmTMB supplies fitted parameters and observation rows. Shared distribution
# functions calculate probabilities, including the mixture of zero and positive
# responses in delta models.
sdmtmb_has_random <- function(model) {
  length(model$tmb_obj$env$random) > 0L || length(model$tmb_random) > 0L
}

# Since sdmTMB 1.2.0, dispersion parameters are indexed by response family,
# rather than by linear predictor. A delta model has two predictors but only
# one positive-response dispersion parameter.
sdmtmb_dispersion <- function(model, pars, which_model) {
  slots <- model$family_spec$param_slot$ln_phi
  index <- if (!is.null(slots)) slots[1L] else
    if (length(pars$ln_phi) == 1L) 1L else which_model
  if (is.na(index)) return(1)
  exp(pars$ln_phi[index])
}

# Check which fitted effects are shared between observations. A model fitted
# with a spatial package need not have dependent responses.
sdmtmb_random_tags <- function(model) {
  index <- model$tmb_obj$env$random
  tags <- if (length(index)) unique(names(model$last.par.best)[index]) else character()
  tags[!is.na(tags)]
}

sdmtmb_shared_iid <- function(model, keep) {
  designs <- model$tmb_data$Zt_list
  if (!length(designs) || isTRUE(as.logical(model$tmb_data$exclude_RE))) return(FALSE)
  any(vapply(designs, function(z) {
    z <- abs(z[, keep, drop = FALSE]) > 0
    if (any(Matrix::rowSums(z) > 1L)) return(TRUE)
    groups <- rownames(z)
    if (is.null(groups) || !anyDuplicated(groups)) return(FALSE)
    # Correlated random slopes can make predictions dependent even when the
    # coefficients affect different observations within the same group.
    aggregate <- Matrix::sparseMatrix(i = match(groups, unique(groups)),
      j = seq_along(groups), x = 1,
      dims = c(length(unique(groups)), length(groups)))
    any(Matrix::rowSums((aggregate %*% z) > 0) > 1L)
  }, logical(1)))
}

sdmtmb_response_dependence <- function(model, conditioning, field_method, keep) {
  tags <- sdmtmb_random_tags(model)
  if (conditioning == "conditional" && field_method %in% c("mle-eb", "mle-mvn")) tags <- character()
  if (conditioning == "marginal") tags <- setdiff(tags, "b_smooth")
  native <- c("omega_s", "epsilon_st", "zeta_s", "re_b_pars", "b_rw_t", "b_smooth")
  unknown <- setdiff(tags, native)
  joint_available <- conditioning != "marginal" || !length(unknown)
  fields <- intersect(tags, c("omega_s", "zeta_s"))
  temporal_shared <- "epsilon_st" %in% tags &&
    (isTRUE(as.logical(model$tmb_data$ar1_fields)) ||
      isTRUE(as.logical(model$tmb_data$rw_fields)) ||
      anyDuplicated(model$tmb_data$year_i[keep]) > 0L)
  shared_rw <- "b_rw_t" %in% tags &&
    sum(rowSums(abs(model$tmb_data$X_rw_ik[keep, , drop = FALSE])) > 0) > 1L
  shared_smooth <- "b_smooth" %in% tags && any(vapply(model$tmb_data$Zs,
    function(z) sum(rowSums(abs(z[keep, , drop = FALSE])) > 0) > 1L, logical(1)))
  shared_iid <- "re_b_pars" %in% tags && sdmtmb_shared_iid(model, keep)
  dependent <- length(keep) > 1L && (length(fields) > 0L || temporal_shared || shared_rw ||
    shared_smooth || shared_iid || length(unknown) > 0L)
  reason <- if (!joint_available) paste0(
    "Native new-effect simulation has no verified treatment of latent block(s): ",
    paste(unknown, collapse = ", "), ".") else if (dependent) paste0(
      if (conditioning == "marginal") "New" else "Posterior fitted",
      " spatial, temporal, group or smooth effects induce response dependence.") else
        "The selected latent design has no shared random contribution between retained responses."
  list(integrated = length(tags) > 0L, independent = !dependent,
    joint_available = joint_available, reason = reason)
}

# Let sdmTMB simulate spatial, temporal and group effects and both parts of a
# delta response together. Select response components and rows afterwards.
sdmtmb_diagnostic_model <- function(model, conditioning = NULL,
    component = "response", field_method = c("mle-mvn", "mle-eb", "mle-mvn-integrated"), seed = NULL) {
  conditioning <- match.arg(conditioning %||% "conditional", c("conditional", "marginal"))
  field_method <- match.arg(field_method)
  component <- match.arg(component, c("response", "binomial", "positive"))
  if (isTRUE(model$has_dispformula)) stop(
    "sdmTMB response diagnostics do not yet support models fitted with `dispformula`.",
    call. = FALSE)
  delta <- isTRUE(model$family$delta)
  if (!delta && component != "response") stop(
    "Component diagnostics require a delta sdmTMB model.", call. = FALSE)
  # sdmTMB recommends residuals evaluated at one posterior draw of all fields.
  # Use that same draw to calculate observed probabilities and generate every
  # reference response. A separate seed keeps fields unchanged between panels.
  state <- NULL
  if (conditioning == "conditional" && field_method == "mle-mvn" &&
      length(model$tmb_obj$env$random)) {
    field_seed <- withr::with_seed(getOption("gratia.diagnostics.field_seed", 1L),
      sample.int(.Machine$integer.max, 1L))
    state <- withr::with_seed(field_seed, {
      ns <- asNamespace("sdmTMB")
      get("reinitialize", ns)(model)
      get(".one_sample_posterior", ns)(model)
    })
  }
  pred <- stats::predict(model, newdata = model$data, offset = model$offset)
  if (!is.null(state)) {
    for (index in seq_len(if (delta) 2L else 1L)) {
      sampled_eta <- stats::predict(model, newdata = model$data,
        offset = model$offset, mcmc_samples = matrix(state, ncol = 1L),
        model = index, nsim = 1L)
      pred[[if (delta) paste0("est", index) else "est"]] <- as.numeric(sampled_eta)
    }
  }
  response <- unname(as.matrix(model$response))
  pars <- sdmTMB::get_pars(model)
  which_model <- if (delta && component != "binomial") 2L else 1L
  fam <- if (delta) model$family[[which_model]] else stats::family(model)
  eta <- pred[[if (delta) paste0("est", which_model) else "est"]]
  mu <- if (startsWith(fam$family, "truncated_")) exp(eta) else fam$linkinv(eta)
  coupled <- delta && !identical(model$family$type, "standard")
  delta_mean <- if (coupled) {
    ns <- asNamespace("sdmTMB")
    if (!exists(".poisson_link_delta_mu_from_eta", ns, inherits = FALSE)) stop(
      "This sdmTMB version does not expose its coupled delta response transform.", call. = FALSE)
    get(".poisson_link_delta_mu_from_eta", ns)
  } else NULL
  if (coupled) mu <- delta_mean(model, which_model, pred$est1, pred$est2)
  y <- response[, which_model]
  if (delta && component == "response")
    y <- ifelse(response[, 1L] == 0, 0, response[, 2L])
  keep <- which(!is.na(y))
  phi <- sdmtmb_dispersion(model, pars, which_model)
  trials <- if (identical(fam$family, "binomial")) unname(model$tmb_data$size) else 1
  trials <- rep_len(trials, NROW(response))
  binomial <- identical(fam$family, "binomial")
  if (binomial) y <- y / (trials + as.numeric(trials == 0))
  family_name <- if (fam$family == "Beta") "beta" else fam$family
  dist <- distribution_from_parameters(family_name, mu[keep],
    dispersion = phi, trials = trials[keep],
    power = stats::plogis(pars$thetaf) + 1,
    df = if (family_name == "student") exp(pars$ln_student_df) + 1 else NULL)
  if (delta && component == "response") {
    p <- if (coupled) delta_mean(model, 1L, pred$est1, pred$est2)[keep] else
      model$family[[1L]]$linkinv(pred$est1)[keep]
    dist <- zero_inflated_distribution(dist, zprob = 1 - p)
    fam <- list(family = paste0("delta_", family_name), link = "identity",
      linkfun = identity, linkinv = identity)
    eta <- rep(NA_real_, length(y))
    eta[keep] <- dist$mean
  }
  dependence <- sdmtmb_response_dependence(model, conditioning, field_method, keep)
  # Marginal simulation replaces fitted effects with new draws from their
  # fitted prior distributions. Use mle-eb so no posterior field is drawn before
  # being replaced. sdmTMB keeps smooth effects at their fitted values here.
  native_type <- if (conditioning == "marginal" || field_method != "mle-mvn-integrated")
    "mle-eb" else "mle-mvn"
  dist$rd <- if (dependence$joint_available) function() {
    draw_seed <- sample.int(.Machine$integer.max, 1L)
    draw <- withr::with_seed(draw_seed, stats::simulate(model, nsim = 1L,
      seed = draw_seed, type = native_type, mle_mvn_samples = "multiple",
      mcmc_samples = if (is.null(state)) NULL else matrix(state, ncol = 1L),
      model = if (!delta || component == "response") NA_integer_ else which_model,
      re_form = if (conditioning == "marginal") NA else NULL, silent = TRUE))
    value <- as.numeric(draw)[keep]
    if (binomial) value <- value / (trials[keep] + as.numeric(trials[keep] == 0))
    value
  } else NULL
  dist$independent <- dependence$independent
  dist$joint_available <- dependence$joint_available
  dist$dependence_reason <- dependence$reason
  dist$conditioning <- conditioning
  dist$field_method <- if (conditioning == "marginal") "new fields" else field_method
  dist$approximation <- if (conditioning == "conditional" && field_method != "mle-eb" &&
      !length(model$tmb_obj$env$random)) "No fitted latent uncertainty; fixed coefficients at MLE." else
    if (conditioning == "marginal") paste(
    "New native spatial/temporal/group effects; fitted coefficients, covariance",
    "parameters and smooths retained.") else if (field_method == "mle-mvn-integrated") paste(
    "Integrated approximate fitted-field uncertainty; fixed coefficients at MLE.",
    "Posterior predictive ranks reuse fitting data and can be conservative;",
    "these differ from native single-field-draw goodness-of-fit residuals.") else
    if (field_method == "mle-mvn") paste(
      "Conditional on one native joint posterior field draw (mle-mvn);",
      "fixed coefficients and covariance parameters at MLE.") else paste(
      "Plug-in fitted effects (mle-eb); fixed coefficients at MLE.",
      "The native documentation does not recommend this mode for goodness-of-fit checks.")
  dist$field_state <- state
  dist$marginal_rd <- dist$rd
  if (dependence$integrated) {
    dist[c("mean", "cdf", "cdf_left", "qf", "bounds", "variance")] <- rep(list(NULL), 6L)
  }
  if (isTRUE(dist$joint_available) && (is.null(dist$mean) || !is.function(dist$cdf))) {
    # Use a separate fixed seed for the simulated reference values. Observed and
    # replicated responses are ranked against the same values, without reusing
    # the random numbers that resolve ties in the residual calculation.
    dist <- prepare_marginal_response_distribution(dist)
  }
  mu <- dist$mean
  # After averaging over effect draws, the mean need not correspond to one
  # fitted linear predictor. Plot this mean on the response scale and label it.
  if (dependence$integrated) eta[keep] <- mu %||% rep(NA_real_, length(keep))
  omitted <- setdiff(seq_len(NROW(response)), keep)
  na <- if (length(omitted)) structure(omitted, class = "exclude") else NULL
  proxy <- new_diagnostic_proxy(y[keep], mu, eta[keep], fam, dist,
    weights = trials[keep], model = model$data[keep, , drop = FALSE],
    original = model, na.action = na, conditioning = conditioning)
  proxy$diagnostic_component <- component
  proxy$field_method <- field_method
  proxy$eta_label <- if (dependence$integrated) "Integrated response expectation" else "Linear predictor"
  proxy
}

#' @export
response_distribution.sdmTMB <- function(model, conditioning = NULL,
    field_method = c("mle-mvn", "mle-eb", "mle-mvn-integrated"), ...) {
  sdmtmb_diagnostic_model(model, conditioning, field_method = field_method)$distribution
}

#' @export
diagnostic_data.sdmTMB <- function(model, type = NULL, seed = NULL,
    conditioning = NULL, component = "response", field_method = c("mle-mvn", "mle-eb", "mle-mvn-integrated"), ..., n_dharma = NULL) {
  diagnostic_data(sdmtmb_diagnostic_model(model, conditioning, component, field_method), type, seed, n_dharma = n_dharma)
}

#' @export
#' @rdname quantile_residuals
#' @param field_method For sdmTMB diagnostics, `"mle-mvn"` (the default) follows
#'   sdmTMB's recommended residual calculation. Draw the fitted random effects
#'   together once from their approximate posterior, then use that same draw
#'   to evaluate the response CDF and generate every reference response. Fixed
#'   coefficients and covariance parameters remain at their maximum likelihood
#'   estimates. The independent seed in `gratia.diagnostics.field_seed` (default
#'   1) keeps different diagnostic panels on the same draw of the effects.
#'   `"mle-eb"` uses the fitted empirical-Bayes effects without drawing their
#'   uncertainty; sdmTMB does not recommend this mode for goodness-of-fit testing.
#'   `"mle-mvn-integrated"` draws new values of the fitted effects for each
#'   simulation, from their approximate posterior. Residuals use the rank of
#'   each response among the simulated reference responses. This averages over
#'   uncertainty in the fitted effects, but reuses the data used in fitting and
#'   can make goodness-of-fit checks less likely to detect departures. These
#'   residuals differ from sdmTMB's calculation at a single draw of the effects.
#'   With `conditioning = "marginal"`, new spatial, temporal and group effects
#'   are drawn from their fitted prior distributions, while smooth effects remain
#'   at fitted values; `field_method` does not add another draw of the fields.
#'   These approximations do not guarantee that goodness-of-fit tests have their
#'   intended false-positive rate after parameter estimation. Posterior predictive checks separately include
#'   coefficient and field uncertainty; see [posterior_predictive_plot()].
#'   Student-t responses use the fitted location, scale and fixed or estimated
#'   degrees of freedom. The scale is not the response standard deviation.
#'   Pearson residuals require degrees of freedom greater than two, because
#'   their response variance is otherwise infinite. PIT, quantile residuals and
#'   posterior predictive checks remain available when this variance is infinite.
quantile_residuals.sdmTMB <- function(model, type = c("pit", "quantile"),
    seed = NULL, conditioning = NULL, component = "response",
    field_method = c("mle-mvn", "mle-eb", "mle-mvn-integrated"), ...) {
  quantile_residuals(sdmtmb_diagnostic_model(model, conditioning, component, field_method),
    type = match.arg(type), seed = seed, ...)
}

#' @export
#' @rdname appraise
#' @inheritParams quantile_residuals
appraise.sdmTMB <- function(model, conditioning = NULL,
    component = "response", field_method = c("mle-mvn", "mle-eb", "mle-mvn-integrated"), ...) {
  appraise(sdmtmb_diagnostic_model(model, conditioning, component, field_method), ...)
}
#' @export
#' @rdname qq_plot
#' @inheritParams quantile_residuals
qq_plot.sdmTMB <- function(model, conditioning = NULL,
    component = "response", field_method = c("mle-mvn", "mle-eb", "mle-mvn-integrated"), ...) {
  qq_plot(sdmtmb_diagnostic_model(model, conditioning, component, field_method), ...)
}
#' @export
#' @rdname worm_plot
#' @inheritParams quantile_residuals
worm_plot.sdmTMB <- function(model, conditioning = NULL,
    component = "response", field_method = c("mle-mvn", "mle-eb", "mle-mvn-integrated"), ...) {
  worm_plot(sdmtmb_diagnostic_model(model, conditioning, component, field_method), ...)
}
