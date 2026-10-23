# Explicit adapters retain native scam prediction and coefficient methods.
# Shared GAM calculations use the transformed coefficients and covariance.

#' @rdname edf
#' @export
edf.scam <- function(object, ..., type = c("default", "unconditional", "alternative")) {
  type <- match.arg(type)
  check_scam_unconditional(object, type == "unconditional")
  edf.gam(scam_gam_adapter(object), ..., type = type)
}

#' @rdname overview
#' @export
overview.scam <- function(model, ...) {
  overview.gam(model, ...)
}

#' @rdname fixed_effects.gam
#' @export
fixed_effects.scam <- function(object, ...) {
  fixed_effects.gam(scam_gam_adapter(object), ...)
}

#' @rdname model_terms
#' @export
model_terms.scam <- function(object, ...) {
  c(names(fixed_effects(object)), smooths(object))
}

#' @rdname model_constant
#' @export
model_constant.scam <- function(model, lp = NULL, ...) {
  if (!is.null(lp) && (length(lp) != 1L || is.na(lp) || lp != 1L)) {
    stop("SCAM models have one linear predictor.", call. = FALSE)
  }
  value <- stats::coef(model)["(Intercept)"]
  if (is.na(value)) value <- 0
  unname(value)
}

#' @rdname model_vars
#' @export
model_vars.scam <- function(model, ...) {
  model_vars.gam(scam_gam_adapter(model), ...)
}

#' @rdname smooth_terms
#' @export
smooth_terms.scam <- function(object, ...) {
  smooth_terms.gam(scam_gam_adapter(object), ...)
}

#' @rdname which_smooths
#' @export
which_smooths.scam <- function(object, terms, ...) {
  which_smooths.gam(scam_gam_adapter(object), terms = terms, ...)
}

#' @rdname parametric_terms
#' @export
parametric_terms.scam <- function(model, ...) {
  parametric_terms.gam(scam_gam_adapter(model), ...)
}

#' @rdname term_variables
#' @export
term_variables.scam <- function(object, term, ...) {
  term_variables.gam(scam_gam_adapter(object), term = term, ...)
}

#' @rdname term_names
#' @export
term_names.scam <- function(object, ...) {
  term_names.gam(scam_gam_adapter(object), ...)
}

#' @rdname n_eta
#' @export
n_eta.scam <- function(model, ...) 1L

#' @rdname dispersion
#' @export
dispersion.scam <- function(model, ...) model[["sig2"]]

#' @rdname basis_size
#' @export
basis_size.scam <- function(object, ...) {
  basis_size.gam(scam_gam_adapter(object), ...)
}

#' @rdname lp_matrix
#' @export
lp_matrix.scam <- function(model, data = NULL, ...) {
  X <- if (is.null(data)) {
    predict_model(model, type = "lpmatrix", ..., se.fit = FALSE)
  } else {
    predict_model(model, newdata = data, type = "lpmatrix", ...,
      se.fit = FALSE)
  }
  class(X) <- c("lp_matrix", class(X))
  X
}

#' @rdname lp_matrix
#' @param object a fitted SCAM model, for the `model.matrix()` method.
#' @export
model.matrix.scam <- function(object, ...) {
  predict_model(object, type = "lpmatrix", ...)
}

#' @rdname derivatives
#' @export
derivatives.scam <- function(object, ..., unconditional = FALSE) {
  check_scam_unconditional(object, unconditional)
  derivatives.gam(scam_gam_adapter(object), ..., unconditional = unconditional)
}

#' @rdname partial_derivatives
#' @export
partial_derivatives.scam <- function(object, ..., unconditional = FALSE) {
  check_scam_unconditional(object, unconditional)
  partial_derivatives.gam(scam_gam_adapter(object), ..., unconditional = unconditional)
}

#' @rdname ale
#' @export
ale.scam <- function(object, ..., unconditional = FALSE) {
  check_scam_unconditional(object, unconditional)
  ale.gam(scam_gam_adapter(object), ..., unconditional = unconditional)
}

#' @rdname conditional_values
#' @inheritParams fitted_values
#' @export
conditional_values.scam <- function(model, ..., unconditional = FALSE) {
  check_scam_unconditional(model, unconditional)
  conditional_values.gam(scam_gam_adapter(model), ..., unconditional = unconditional)
}

#' @rdname conditional_differences
#' @export
conditional_differences.scam <- function(model, ..., unconditional = FALSE) {
  check_scam_unconditional(model, unconditional)
  conditional_differences.gam(scam_gam_adapter(model), ..., unconditional = unconditional)
}

#' @rdname difference_smooths
#' @export
difference_smooths.scam <- function(model, ..., unconditional = FALSE) {
  check_scam_unconditional(model, unconditional)
  difference_smooths.gam(scam_gam_adapter(model), ..., unconditional = unconditional)
}

#' @rdname parametric_effects
#' @export
parametric_effects.scam <- function(object, ..., unconditional = FALSE) {
  check_scam_unconditional(object, unconditional)
  parametric_effects.gam(scam_gam_adapter(object), ..., unconditional = unconditional)
}

#' @rdname partial_residuals
#' @export
partial_residuals.scam <- function(object, ...) {
  partial_residuals.gam(scam_gam_adapter(object), ...)
}

#' @rdname add_fitted
#' @export
add_fitted.scam <- function(data, model, ...) {
  out <- add_fitted.gam(data, scam_gam_adapter(model), ...)
  if (".constant" %in% names(out)) out$.constant <- model_constant(model)
  out
}

#' @rdname add_residuals
#' @export
add_residuals.scam <- function(data, model, ...) {
  add_residuals.gam(data, scam_gam_adapter(model), ...)
}

#' @rdname add_partial_residuals
#' @export
add_partial_residuals.scam <- function(data, model, ...) {
  add_partial_residuals.gam(data, scam_gam_adapter(model), ...)
}

#' @rdname rootogram
#' @export
rootogram.scam <- function(object, ...) {
  rootogram.gam(scam_gam_adapter(object), ...)
}

#' @rdname pp_check
#' @exportS3Method bayesplot::pp_check scam
pp_check.scam <- function(object, ...) {
  pp_check.gam(scam_gam_adapter(object), ...)
}

#' @rdname confint.gam
#' @details For SCAM fits, `confint()` returns intervals for smooth functions,
#'   with `parm` selecting smooth labels. It uses transformed-coefficient
#'   Gaussian uncertainty conditional on the fitted smoothing parameters.
#'   By default, intercept uncertainty is excluded. These intervals do not
#'   enforce shape constraints on their limits.
#' @export
confint.scam <- function(object, parm = smooths(object), ...,
                         unconditional = FALSE, overall_uncertainty = FALSE) {
  check_scam_unconditional(object, unconditional)
  confint.gam(scam_gam_adapter(object), parm = parm, ...,
    unconditional = unconditional, overall_uncertainty = overall_uncertainty)
}

#' @rdname penalty
#' @details For SCAM fits, the returned penalties act on the unrestricted
#'   fitting parameters, before scam transforms selected coefficients to impose
#'   positivity. The `parameterisation` attribute records this distinction.
#' @export
penalty.scam <- function(object, ...) {
  out <- penalty.gam(scam_gam_adapter(object), ...)
  attr(out, "parameterisation") <- "unrestricted fitting parameters"
  out
}

#' @rdname variance_comp
#' @details For SCAM fits with `type = "penalty"`, `.variance` is the fitted
#'   response scale divided by
#'   the smoothing parameter, optionally undoing penalty rescaling. This ratio
#'   describes the Gaussian penalty on unrestricted fitting parameters; it is
#'   not a Gaussian variance for the transformed smooth coefficients. SCAM's
#'   GCV and UBRE fits do not supply the likelihood-based smoothing-parameter
#'   covariance needed for confidence limits, so both limit columns are `NA`.
#' @export
variance_comp.scam <- function(object, rescale = TRUE, coverage = 0.95,
    type = c("penalty", "terms"), n_draws = 1000L, seed = NULL,
    unconditional = FALSE, include_total = TRUE, ...) {
  type <- match.arg(type)
  if (type == "terms") {
    variance_terms_check_dots(...)
    return(variance_comp_terms(object, coverage = coverage, n_draws = n_draws,
      seed = seed, unconditional = unconditional, include_total = include_total))
  }
  # SCAM's penalty is Gaussian on the unrestricted fitting parameters.
  # This scale ratio does not describe Gaussian transformed smooth coefficients.
  object <- scam_gam_adapter(object)
  # scam retains a one-dimensional array where mgcv expects a vector.
  object$sp <- stats::setNames(as.numeric(object$sp),
    names(object$sp) %||% dimnames(object$sp)[[1L]])
  out <- variance_comp.gam(object, rescale = rescale,
    coverage = coverage, ...)
  out$.lower_ci <- out$.upper_ci <- rep(NA_real_, nrow(out))
  attr(out, "parameterisation") <- "unrestricted fitting parameters"
  attr(out, "interpretation") <- paste(
    "Variance is fitted response scale divided by the smoothing parameter.",
    "It describes the penalty on unrestricted fitting parameters.",
    "GCV and UBRE fits do not supply likelihood-based confidence limits.")
  out
}

#' @rdname model_concurvity
#' @export
model_concurvity.scam <- function(model, ...) {
  model <- scam_gam_adapter(model)
  # Both packages register model.matrix.scam. Give this copy its own matrix
  # method so mgcv always uses the basis aligned with transformed coefficients,
  # regardless of which package registered its SCAM method most recently.
  model[["X"]] <- predict_model(model, type = "lpmatrix")
  model[["coef"]] <- stats::coef(model)
  class(model) <- c("gratia_concurvity", class(model))
  model_concurvity.gam(model, ...)
}

#' @export
model.matrix.gratia_concurvity <- function(object, ...) {
  object[["X"]]
}

#' @rdname log_lik
#' @export
log_lik.scam <- function(model, ..., unconditional = FALSE) {
  check_scam_unconditional(model, unconditional)
  log_lik.gam(scam_gam_adapter(model), ...)
}

#' @rdname loo.gam
#' @exportS3Method loo::loo scam
loo.scam <- function(x, ...) {
  loo.gam(scam_gam_adapter(x), ...)
}

#' @rdname loo.gam
#' @exportS3Method loo::waic scam
waic.scam <- function(x, ...) {
  waic.gam(scam_gam_adapter(x), ...)
}
