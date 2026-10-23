# The fitted GAM component describes the population mean. The additional
# random effects supplied via random= belong to mer; they must not silently
# enter a GAM coefficient vector or its covariance matrix.
gamm4_gam <- function(object) {
  if (!is_gamm4(object)) {
    stop("Expected a gamm4 model with valid 'gam' and 'mer' components.",
      call. = FALSE)
  }
  gam <- object[["gam"]]
  # gamm4 stores residuals(mer), which defaults to deviance residuals for
  # glmerMod fits. mgcv expects this slot to contain working residuals.
  gam$residuals <- stats::residuals(object[["mer"]], type = "working")
  # lme4 retains offsets supplied through ... even when the GAM frame does not.
  # Read the known contribution directly; full mixed predictions also contain
  # group effects and cannot supply a GAM-component offset by subtraction.
  frame <- stats::model.frame(object[["mer"]])
  off <- stats::model.offset(frame)
  if (!is.null(off)) {
    rows <- match(rownames(gam$model), rownames(frame))
    if (anyNA(rows)) {
      stop("GAM and mixed-model offset rows are not aligned.", call. = FALSE)
    }
    gam$offset <- off[rows]
    arg <- frame[["(offset)"]]
    if (!is.null(arg)) attr(gam, "gratia.fitting.argument.offset") <- arg[rows]
  }
  attr(gam, "gratia.gamm4") <- TRUE
  gam
}

#' Support for models fitted by gamm4
#'
#' Models fitted by [gamm4::gamm4()] (version 0.2-7 or later) have class
#' `"gamm4"`. Pass the fitted object directly to gratia methods; historical
#' unclassed lists are not supported.
#'
#' @details Smooths, parametric effects, predictions, derivatives, simulations,
#' and posterior draws describe the population GAM component. Terms supplied
#' through `random` are excluded, including their realized effects, predictive
#' variation, and uncertainty. `conditional_values()` and
#' `conditional_differences()` condition on covariate values, not on realized
#' group effects. Group variables occurring only in `random` are not required
#' in prediction data, so new groups receive the same population predictions.
#' Use the `mer` component with lme4 for predictions conditional on group
#' effects and simulations including their variation. Diagnostics and response
#' simulations use the full fitted mixed model by default. With
#' `conditioning = "conditional"`, all fitted smooth and additional random
#' effects are held fixed for response references. `"marginal"` draws new effects
#' from their fitted distribution. `"gam_component"` explicitly selects the
#' extracted GAM calculations used by earlier versions.
#' Marginal GLMM checks average over random effects using lme4's joint response
#' simulations. Residuals then use ranks in a shared reference sample, as
#' described in [diagnostic_support]. Gaussian identity-link models instead
#' use analytic CDFs of the marginal response distribution.
#'
#' Posterior response draws also default to the full conditional model. They
#' draw fixed and fitted random effects together, keeping covariance parameters
#' at their fitted values. These Gaussian approximations currently use fitting
#' observations only. To supply user coefficient draws or prediction data to
#' [posterior_samples()], select `conditioning = "gam_component"` explicitly.
#'
#' Coefficient draws from [post_draws()], [fitted_samples()], [predicted_samples()]
#' and [smooth_samples()] still use the fitted GAM covariance matrix, including
#' its uncertainty due to fitting the mixed model. User draws must have one
#' column per GAM coefficient. Metropolis-Hastings sampling is unsupported:
#' the GAM likelihood does not include the additional random effects.
#' Smoothness uncertainty-corrected covariance matrices and alternative EDF
#' are unavailable from gamm4; requested corrections warn and fall back to
#' the available covariance matrix or EDF. Variance components are extracted
#' from the mixed-model component. Smooth components use the same penalty
#' scale and labels as `gam()` and `bam()`, with a separate row for each
#' penalty of a tensor or factor smooth. Additional random effects retain
#' their mixed-model variances. Confidence intervals use likelihood profiles
#' by default; use `interval_method = "none"` to obtain estimates only.
#' Gaussian profiles use a maximum-likelihood refit even when the reported
#' estimates were fitted by REML. See [variance_comp] for interval details.
#' With `type = "terms"`, variances describe fitted term contributions,
#' including the additional random effects. Their covariance matrix is
#' returned in the `covariance` attribute. Term intervals use joint posterior
#' coefficient draws, retaining covariance with the additional fitted random
#' effects. Use `n_draws = 0` for term estimates without intervals.
#'
#' @name gamm4_support
NULL

#' @seealso [gamm4_support]
#' @export
#' @rdname edf
#' @name edf
#' @aliases edf.gamm4
edf.gamm4 <- function(object, ...) {
  edf.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname basis_size
#' @name basis_size
#' @aliases basis_size.gamm4
basis_size.gamm4 <- function(object, ...) {
  basis_size.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname lp_matrix
#' @name lp_matrix
#' @aliases lp_matrix.gamm4
lp_matrix.gamm4 <- function(model, ...) {
  lp_matrix.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname model_concurvity
#' @name model_concurvity
#' @aliases model_concurvity.gamm4
model_concurvity.gamm4 <- function(model, ...) {
  model_concurvity.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname penalty
#' @name penalty
#' @aliases penalty.gamm4
penalty.gamm4 <- function(object, ...) {
  penalty.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname model_constant
#' @name model_constant
#' @aliases model_constant.gamm4
model_constant.gamm4 <- function(model, ...) {
  model_constant.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname smooth_dim
#' @name smooth_dim
#' @aliases smooth_dim.gamm4
smooth_dim.gamm4 <- function(object) {
  smooth_dim.gam(gamm4_gam(object))
}

#' @seealso [gamm4_support]
#' @export
#' @rdname smooth_terms
#' @name smooth_terms
#' @aliases smooth_terms.gamm4
smooth_terms.gamm4 <- function(object, ...) {
  smooth_terms.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname smooth_label
#' @name smooth_label
#' @aliases smooth_label.gamm4
smooth_label.gamm4 <- function(object, id, ...) {
  smooth_label.gam(gamm4_gam(object), id = id, ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname which_smooths
#' @name which_smooths
#' @aliases which_smooths.gamm4
which_smooths.gamm4 <- function(object, ...) {
  which_smooths.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname n_smooths
#' @name n_smooths
#' @aliases n_smooths.gamm4
n_smooths.gamm4 <- function(object) {
  n_smooths.gam(gamm4_gam(object))
}

#' @seealso [gamm4_support]
#' @export
#' @rdname parametric_terms
#' @name parametric_terms
#' @aliases parametric_terms.gamm4
parametric_terms.gamm4 <- function(model, ...) {
  parametric_terms.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname term_variables
#' @name term_variables
#' @aliases term_variables.gamm4
term_variables.gamm4 <- function(object, ...) {
  term_variables.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname term_names
#' @name term_names
#' @aliases term_names.gamm4
term_names.gamm4 <- function(object, ...) {
  term_names.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname n_eta
#' @name n_eta
#' @aliases n_eta.gamm4
n_eta.gamm4 <- function(model, ...) {
  n_eta.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname dispersion
#' @name dispersion
#' @aliases dispersion.gamm4
dispersion.gamm4 <- function(model, ...) {
  dispersion.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname boundary
#' @name boundary
#' @aliases boundary.gamm4
boundary.gamm4 <- function(x, ...) {
  boundary.gam(gamm4_gam(x), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname typical_values
#' @name typical_values
#' @aliases typical_values.gamm4
typical_values.gamm4 <- function(object, ...) {
  typical_values.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname factor_combos
#' @name factor_combos
#' @aliases factor_combos.gamm4
factor_combos.gamm4 <- function(object, ...) {
  factor_combos.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname data_combos
#' @name data_combos
#' @aliases data_combos.gamm4
data_combos.gamm4 <- function(object, ...) {
  data_combos.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname fitted_values
#' @name fitted_values
#' @aliases fitted_values.gamm4
fitted_values.gamm4 <- function(object, ...) {
  fitted_values.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname conditional_values
#' @name conditional_values
#' @aliases conditional_values.gamm4
conditional_values.gamm4 <- function(model, ...) {
  conditional_values.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname conditional_differences
#' @name conditional_differences
#' @aliases conditional_differences.gamm4
conditional_differences.gamm4 <- function(model, ...) {
  conditional_differences.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname derivatives
#' @name derivatives
#' @aliases derivatives.gamm4
derivatives.gamm4 <- function(object, ...) {
  derivatives.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname partial_derivatives
#' @name partial_derivatives
#' @aliases partial_derivatives.gamm4
partial_derivatives.gamm4 <- function(object, ...) {
  partial_derivatives.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname response_derivatives
#' @name response_derivatives
#' @aliases response_derivatives.gamm4
response_derivatives.gamm4 <- function(object, ...) {
  response_derivatives.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname derivative_samples
#' @name derivative_samples
#' @aliases derivative_samples.gamm4
derivative_samples.gamm4 <- function(
    model,
    ...) {
  sampling_check_names(substitute(list(...)), derivative = TRUE)

  derivative_samples.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname posterior_samples
#' @name posterior_samples
#' @aliases posterior_samples.gamm4
posterior_samples.gamm4 <- function(
    model,
    ...,
    n_draws = NULL,
    seed = NULL,
    conditioning = NULL) {
  marginal_not_supported(list(...), "posterior_samples.gamm4()")
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  conditioning <- mixed_conditioning(model, conditioning)
  if (conditioning == "gam_component")
    return(posterior_samples.gam(gamm4_gam(model), n_draws = n_draws, seed = seed, ...))
  layout <- sampling_resolve(n_draws, NULL, NULL, method = "gaussian")
  n_draws <- layout$n_draws
  diagnostic_posterior_samples.default(model, n_draws, seed,
    conditioning = conditioning, ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname fitted_samples
#' @name fitted_samples
#' @aliases fitted_samples.gamm4
fitted_samples.gamm4 <- function(
    model,
    ...) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  fitted_samples.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname predicted_samples
#' @name predicted_samples
#' @aliases predicted_samples.gamm4
predicted_samples.gamm4 <- function(
    model,
    ...) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  predicted_samples.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname smooth_samples
#' @name smooth_samples
#' @aliases smooth_samples.gamm4
smooth_samples.gamm4 <- function(
    model,
    ...) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  smooth_samples.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname gaussian_draws
#' @name gaussian_draws
#' @aliases gaussian_draws.gamm4
gaussian_draws.gamm4 <- function(
    model,
    ...) {
  sampling_check_names(substitute(list(...)), derivative = FALSE, gaussian = TRUE)

  gaussian_draws.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname user_draws
#' @name user_draws
#' @aliases user_draws.gamm4
user_draws.gamm4 <- function(
    model,
    ...,
    draws,
    n_draws = NULL,
    chains = NULL) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  layout <- sampling_resolve(n_draws, chains, draws, method = "user")
  n_draws <- layout$n_draws
  chains <- layout$chains

  user_draws.gam(gamm4_gam(model), draws = draws, ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname post_draws
#' @name post_draws
#' @aliases generate_draws.gamm4
generate_draws.gamm4 <- function(
    model,
    ...) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  generate_draws.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname appraise
#' @name appraise
#' @aliases appraise.gamm4
appraise.gamm4 <- function(model, conditioning = NULL, ...) {
  appraise(mixed_diagnostic_model(model, conditioning), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname qq_plot
#' @name qq_plot
#' @aliases qq_plot.gamm4
qq_plot.gamm4 <- function(model, conditioning = NULL, ...) {
  qq_plot(mixed_diagnostic_model(model, conditioning), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname worm_plot
#' @name worm_plot
#' @aliases worm_plot.gamm4
worm_plot.gamm4 <- function(model, conditioning = NULL, ...) {
  worm_plot(mixed_diagnostic_model(model, conditioning), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname quantile_residuals
#' @name quantile_residuals
#' @aliases quantile_residuals.gamm4
quantile_residuals.gamm4 <- function(model, type = c("pit", "quantile"),
    seed = NULL, conditioning = NULL, ...) {
  quantile_residuals(mixed_diagnostic_model(model, conditioning),
    type = type, seed = seed, ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname rootogram
#' @name rootogram
#' @aliases rootogram.gamm4
rootogram.gamm4 <- function(object, ..., conditioning = NULL) {
  model <- mixed_diagnostic_model(object, conditioning)
  if (inherits(model, "gam")) return(rootogram.gam(model, ...))
  rootogram_from_distribution(response_distribution(model), model$y,
    family_name(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname parametric_effects
#' @name parametric_effects
#' @aliases parametric_effects.gamm4
parametric_effects.gamm4 <- function(object, ...) {
  parametric_effects.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname evaluate_parametric_term
#' @name evaluate_parametric_term
#' @aliases evaluate_parametric_term.gamm4
evaluate_parametric_term.gamm4 <- function(object, ...) {
  evaluate_parametric_term.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname assemble
#' @name assemble
#' @aliases assemble.gamm4
assemble.gamm4 <- function(object, ...) {
  assemble.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname fderiv
#' @name fderiv
#' @aliases fderiv.gamm4
fderiv.gamm4 <- function(model, ...) {
  fderiv.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname theta
#' @name theta
#' @aliases theta.gamm4
theta.gamm4 <- function(object, ...) {
  theta.gam(gamm4_gam(object), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname nb_theta
#' @name nb_theta
#' @aliases nb_theta.gamm4
nb_theta.gamm4 <- function(model) {
  nb_theta.gam(gamm4_gam(model))
}

#' @seealso [gamm4_support]
#' @export
#' @rdname post_draws
#' @name post_draws
#' @aliases post_draws.gamm4
post_draws.gamm4 <- function(
    model,
    ...) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  post_draws.default(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname add_fitted
#' @name add_fitted
#' @aliases add_fitted.gamm4
add_fitted.gamm4 <- function(data, model, ...) {
  add_fitted.gam(data, gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname add_residuals
#' @name add_residuals
#' @aliases add_residuals.gamm4
add_residuals.gamm4 <- function(data, model, ...) {
  add_residuals.gam(data, gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname add_partial_residuals
#' @name add_partial_residuals
#' @aliases add_partial_residuals.gamm4
add_partial_residuals.gamm4 <- function(data, model, ...) {
  add_partial_residuals.gam(data, gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname is_factor_term
#' @name is_factor_term
#' @aliases is_factor_term.gamm4
is_factor_term.gamm4 <- function(object, term, ...) {
  is_factor_term.gam(gamm4_gam(object), term, ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname simulate
#' @name simulate
#' @aliases simulate.gamm4
simulate.gamm4 <- function(object, nsim = 1, seed = NULL,
    conditioning = NULL, ...) {
  marginal_not_supported(list(...), "simulate.gamm4()")
  conditioning <- mixed_conditioning(object, conditioning)
  if (conditioning == "gam_component") {
    return(simulate.gam(gamm4_gam(object), nsim = nsim, seed = seed, ...))
  }
  if (length(list(...))) {
    stop("Full-model gamm4 simulation uses fitting observations and does not ",
      "accept additional prediction options; use the mer component for ",
      "prediction at new data.", call. = FALSE)
  }
  simulate_gamm_response(object, nsim, seed, conditioning)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname mh_draws
#' @name mh_draws
#' @aliases mh_draws.gamm4
mh_draws.gamm4 <- function(
    model,
    ...) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  mh_draws.gam(gamm4_gam(model), ...)
}

#' @seealso [gamm4_support]
#' @export
#' @rdname variance_comp
#' @name variance_comp
#' @aliases variance_comp.gamm4
variance_comp.gamm4 <- function(object, rescale = TRUE, coverage = 0.95,
    interval_method = c("profile", "none"), type = c("penalty", "terms"),
    n_draws = 1000L, seed = NULL, unconditional = FALSE, include_total = TRUE, ...) {
  gam <- gamm4_gam(object)
  type <- match.arg(type)
  if (type == "terms") {
    variance_terms_check_dots(...)
    gam <- model_used_rows(gam)
    return(variance_comp_terms(gam, gamm4_fitted_random_terms(object, gam),
      coverage = coverage, n_draws = n_draws, seed = seed,
      unconditional = unconditional, mixed = object, include_total = include_total))
  }
  if (!is.numeric(coverage) || length(coverage) != 1L || !is.finite(coverage) ||
      coverage <= 0 || coverage >= 1) {
    stop("'coverage' must be between zero and one.", call. = FALSE)
  }
  if (!is.logical(rescale) || length(rescale) != 1L || is.na(rescale)) {
    stop("'rescale' must be TRUE or FALSE.", call. = FALSE)
  }
  interval_method <- match.arg(interval_method)
  dots <- list(...)
  fixed_controls <- intersect(names(dots), c("which", "signames", "prof.scale",
    "fitted"))
  if (length(fixed_controls)) {
    stop("Profile parameters and scale are set by variance_comp(); do not ",
      "supply ", paste(shQuote(fixed_controls), collapse = ", "), ".",
      call. = FALSE)
  }
  if (interval_method == "none" && length(dots)) {
    stop("Profile controls require 'interval_method = \"profile\"'.",
      call. = FALSE)
  }
  mer <- object[["mer"]]
  blocks <- lme4::VarCorr(mer)
  vc <- as.data.frame(blocks)
  vc <- vc[is.na(vc$var2), , drop = FALSE]
  cnms <- lme4::getME(mer, "cnms")
  scale <- vc$grp == "Residual" & is.na(vc$var1)
  component <- ifelse(scale, "scale", paste(vc$grp, vc$var1, sep = ":"))
  # VarCorr gives repeated grouping factors unique suffixes. Profile names
  # instead retain the original grouping-factor names from cnms.
  group <- names(cnms)[match(vc$grp, names(blocks))]
  parameter <- ifelse(scale, "sigma", paste0("sd_", vc$var1, "|", group))
  scaling <- rep(1, nrow(vc))
  smooth_rows <- integer()
  for (sm in gam$smooth) {
    n <- length(sm$S)
    if (!n) next
    index <- match(sm$lmer.name, vc$grp)
    if (length(index) != n || anyNA(index) ||
        any(lengths(blocks[sm$lmer.name]) != 1L) || length(sm$S.scale) != n) {
      stop("Smooth penalties do not match the fitted gamm4 blocks.",
        call. = FALSE)
    }
    component[index] <- names(gam$sp)[seq.int(sm$first.sp, sm$last.sp)]
    if (rescale) scaling[index] <- sm$S.scale
    smooth_rows <- c(smooth_rows, index)
  }
  order <- c(smooth_rows, setdiff(which(!scale), smooth_rows), which(scale))
  bounds <- if (interval_method == "profile") {
    gamm4_variance_intervals(mer, parameter, component, coverage, ...)
  } else matrix(NA_real_, nrow(vc), 2L)
  tbl <- tibble::tibble(.component = component[order],
    .variance = (vc$vcov * scaling)[order],
    .std_dev = (vc$sdcor * sqrt(scaling))[order],
    .lower_ci = (bounds[, 1L] * sqrt(scaling))[order],
    .upper_ci = (bounds[, 2L] * sqrt(scaling))[order])
  class(tbl) <- c("variance_comp", class(tbl))
  tbl
}

gamm4_variance_intervals <- function(mer, parameter, component, coverage, ...) {
  bounds <- matrix(NA_real_, length(parameter), 2L)
  if (lme4::isGLMM(mer) &&
      as.logical(lme4::getME(mer, "devcomp")$dims[["useSc"]])) {
    warning("Profile intervals are unavailable for generalized gamm4 fits ",
      "with an estimated scale.", call. = FALSE)
    return(bounds)
  }
  repeated <- duplicated(parameter) | duplicated(parameter, fromLast = TRUE)
  if (any(repeated)) {
    warning("Profile intervals are unavailable for repeated covariance ",
      "parameter names: ", paste(unique(parameter[repeated]), collapse = ", "),
      ".", call. = FALSE)
  }
  for (i in which(!repeated)) {
    bounds[i, ] <- tryCatch({
      profile <- stats::profile(mer, which = parameter[i], signames = FALSE, ...)
      interval <- stats::confint(profile, level = coverage)
      unname(interval[parameter[i], , drop = TRUE])
    }, error = function(e) {
      warning("Could not compute a profile interval for '", component[i],
        "': ", conditionMessage(e), call. = FALSE)
      c(NA_real_, NA_real_)
    })
  }
  bounds
}

#' @seealso [gamm4_support]
#' @export
#' @rdname overview
#' @name overview
#' @aliases overview.gamm4
overview.gamm4 <- function(model, ...) {
  overview.gam(gamm4_gam(model), ...)
}
