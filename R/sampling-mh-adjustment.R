# Internal research adjustment, not a new MH target or a public method.
# Apply one pooled transformation to every chain. Adjusting chains separately
# would conceal disagreement between chains and invalidate diagnostics.
sampling_mh_covariance_adjust <- function(model, draws,
    diagnostics = !is.null(attr(draws, "sampling")$coefficients)) {
  if (!inherits(model, "gam") || inherits(model, "scam")) stop(
    "The MH covariance adjustment requires a gam or bam fit.", call. = FALSE)
  if (!is.logical(diagnostics) || length(diagnostics) != 1L || is.na(diagnostics))
    stop("'diagnostics' must be TRUE or FALSE.", call. = FALSE)
  if (!is.matrix(draws) || !is.numeric(draws) || any(!is.finite(draws)))
    stop("Supply a finite numeric matrix of joint coefficient draws.", call. = FALSE)
  nm <- names(stats::coef(model))
  p <- length(nm)
  if (ncol(draws) != p) stop(
    "The adjustment requires all model coefficients jointly; do not select coefficients first.",
    call. = FALSE)
  if (nrow(draws) <= p) stop(
    "More retained draws than coefficients are required to estimate a nonsingular covariance.",
    call. = FALSE)
  columns <- colnames(draws)
  if (!is.null(columns) && !identical(columns, nm))
    stop("Draw columns must contain every model coefficient in fitted coefficient order.",
      call. = FALSE)
  layout <- sampling_resolve(NULL, draws = draws, method = "user")
  meta <- attr(draws, "sampling")
  if (!is.null(meta$covariance_adjustment)) stop(
    "These draws already have a covariance adjustment; supply the original MH draws.",
    call. = FALSE)
  target <- model$Vc
  if (is.null(target)) stop(
    "The fit has no unconditional covariance (Vc); no conditional covariance is substituted.",
    call. = FALSE)
  if (!is.matrix(target) || !is.numeric(target) ||
      !identical(dim(target), c(p, p)) || any(!is.finite(target))) stop(
    "The unconditional covariance must be a finite matrix for all fitted coefficients.",
    call. = FALSE)
  factor_covariance <- function(V, label) {
    if (any(!is.finite(V))) stop(
      label, " must contain only finite values.", call. = FALSE)
    scale <- max(abs(V))
    if (scale == 0 || max(abs(V - t(V))) > 1e-10 * scale ||
        any(diag(V) <= 0)) stop(
      label, " must be symmetric and positive definite.", call. = FALSE)
    correlation <- V / outer(sqrt(diag(V)), sqrt(diag(V)))
    values <- eigen(correlation, symmetric = TRUE, only.values = TRUE)$values
    if (min(values) <= 1e-10 * max(values)) stop(
      label, " is singular or too ill-conditioned for a stable adjustment.",
      call. = FALSE)
    result <- tryCatch(chol((V + t(V)) / 2), error = function(e) NULL)
    if (is.null(result)) stop(label, " must be positive definite.", call. = FALSE)
    result
  }
  centre <- colMeans(draws)
  empirical <- stats::cov(draws)
  source_factor <- factor_covariance(empirical, "The empirical MH covariance")
  target_factor <- factor_covariance(target, "The unconditional covariance")
  # For upper Cholesky factors C = R'R, row draws use R_c^{-1} R_u.
  transformation <- backsolve(source_factor, target_factor)
  if (any(!is.finite(transformation))) stop(
    "The covariance transformation is not finite at these coefficient scales.",
    call. = FALSE)
  out <- sweep(sweep(draws, 2L, centre, "-") %*% transformation,
    2L, centre, "+")
  achieved <- stats::cov(out)
  covariance_error <- max(abs(achieved - target)) / max(abs(target))
  if (any(!is.finite(out)) || !is.finite(covariance_error) ||
      covariance_error > 1e-7) stop(
    "The target covariance cannot be represented accurately at these coefficient scales.",
    call. = FALSE)
  dimnames(out) <- dimnames(draws)
  ids <- meta$draw_ids %||% sampling_layout(nrow(draws), layout$chains)
  original_diagnostics <- if (is.null(meta)) NULL else
    sampling_diagnostics(draws)
  refreshed <- sampling_finish(out, ids, info = meta$chains,
    diagnostics = diagnostics, coefficient_names = columns %||% nm)
  # Keep acceptance rates and other attributes of the source MH run.
  attributes(out) <- attributes(draws)
  if (is.null(meta)) meta <- list(draw_ids = ids, chains = NULL, coefficients = NULL)
  meta$coefficients <- attr(refreshed, "sampling")$coefficients
  # Plots must follow the adjusted coefficients, not the source MH draws.
  meta$coefficient_draws <- attr(refreshed, "sampling")$coefficient_draws
  dimnames(target) <- dimnames(empirical) <- list(columns %||% nm, columns %||% nm)
  meta$covariance_adjustment <- list(
    method = "pooled empirical covariance to fitted Vc using Cholesky factors",
    centre = stats::setNames(centre, columns %||% nm),
    source_covariance = empirical, target_covariance = target,
    transformation = transformation,
    source_diagnostics = original_diagnostics,
    covariance_relative_error = covariance_error,
    singular_values = svd(transformation, nu = 0L, nv = 0L)$d,
    interpretation = paste("Approximate adjustment after MH sampling;",
      "acceptance rates describe the original conditional target.",
      "The transformation depends on coefficient order and estimated MH covariance."))
  attr(out, "sampling") <- meta
  out
}
