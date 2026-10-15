# Internal research implementation. These draws mix exact Gaussian conditional
# coefficient distributions over a fitted log-smoothing normal approximation.
# They propagate approximate estimation uncertainty, without Bayesian integration.
sampling_smoothing_setup <- function(model, block_size = 10000L) {
  sampling_target_check(model, "smoothing-mixture")
  fam <- stats::family(model)
  if (!identical(fam$family, "gaussian") || !identical(fam$link, "identity")) {
    stop("The smoothing-mixture prototype requires Gaussian identity fits.",
      call. = FALSE)
  }
  if (model$method %in% c("NCV", "QNCV")) stop(
    "NCV smoothing uncertainty is audited separately with sampling_smoothing_ncv_delta(); its inverse criterion Hessian is not a posterior covariance.",
    call. = FALSE)
  if (!model$method %in% c("REML", "ML", "fREML")) {
    stop("The smoothing-mixture prototype requires ML, REML, or fREML fitting.",
      call. = FALSE)
  }
  if (inherits(model, "bam") && !is.null(model$dinfo)) stop(
    "The smoothing-mixture prototype does not support discrete bam: the compressed fitting design can differ from evaluation at the original covariates.",
    call. = FALSE)
  if (!is.null(model$call$min.sp) && !identical(model$call$min.sp, quote(NULL))) stop(
    "The smoothing-mixture prototype does not yet support minimum smoothing parameters (min.sp).",
    call. = FALSE)
  if (!is.null(attr(model$outer.info$hess, "hess1"))) stop(
    "The smoothing-mixture prototype does not yet support edge-corrected smoothing covariance estimates.",
    call. = FALSE)
  # This also checks additional penalties and the fitted penalty ordering.
  sampling_target_penalty(model, "smoothing-mixture")
  block_size <- sampling_count(block_size, "block_size")
  model <- model_used_rows(model)
  phi <- model$sig2
  if (length(phi) != 1L || !is.finite(phi) || phi <= 0) stop(
    "A positive fitted dispersion is required.", call. = FALSE)
  sp <- model$sp
  q <- length(sp)
  full_sp <- model$full.sp %||% sp
  n_pen <- length(full_sp)
  info <- sampling_smoothing_info(model)
  V <- model$V.sp
  if (!q || any(!is.finite(sp) | sp <= 0) ||
      !is.matrix(V) || nrow(V) != ncol(V) ||
      !nrow(V) %in% c(q, q + 1L) || any(!is.finite(V))) stop(
    "Cannot identify a finite covariance for the free log-smoothing parameters.",
    call. = FALSE)
  # For ordinary Gaussian fits there are no family parameters before log(sp).
  # gam can retain log(scale) after them, whereas fREML bam's V.sp omits it.
  # spind indexes expanded derivatives in some mgcv versions, including shared
  # smoothing parameters; its length need not equal the number of free sp.
  L <- attr(V, "L")
  if (is.null(L)) {
    if (n_pen != q) stop("Cannot recover the smoothing-parameter mapping.",
      call. = FALSE)
    L <- diag(q)
  } else if (!is.matrix(L) || nrow(L) < n_pen || ncol(L) < q ||
             any(!is.finite(L))) stop(
    "Cannot recover the smoothing-parameter mapping.", call. = FALSE)
  L <- L[seq_len(n_pen), seq_len(q), drop = FALSE]
  if (any(!is.finite(full_sp) | full_sp <= 0)) stop(
    "The smoothing-mixture prototype requires positive fitted penalty multipliers.",
    call. = FALSE)
  lsp0 <- log(full_sp) - drop(L %*% log(sp))
  if (is.null(info$V_sp_layout)) stop(
    "Cannot identify the parameter ordering in V.sp for this fitting method.",
    call. = FALSE)
  log_sp_cov <- V[info$V_sp_layout$log_sp, info$V_sp_layout$log_sp, drop = FALSE]
  if (is.null(tryCatch(chol(log_sp_cov), error = function(e) NULL))) stop(
    "The fitted log-smoothing covariance is not positive definite.", call. = FALSE)
  p <- length(stats::coef(model))
  y <- as.numeric(model$y)
  w <- model$prior.weights %||% rep(1, length(y))
  offset <- model$offset %||% rep(0, length(y))
  if (length(offset) == 1L) offset <- rep(offset, length(y))
  if (length(y) != nrow(model$model) || length(w) != length(y) ||
      length(offset) != length(y) || any(!is.finite(y)) ||
      any(!is.finite(w) | w < 0) || any(!is.finite(offset))) stop(
    "The fitted response, weights, offsets, and model rows do not align.",
    call. = FALSE)
  XtWX <- matrix(0, p, p)
  XtWy <- numeric(p)
  blocks <- split(seq_along(y), ceiling(seq_along(y) / block_size))
  for (i in blocks) {
    X <- mgcv::predict.gam(model, newdata = model$model[i, , drop = FALSE],
      type = "lpmatrix", na.action = stats::na.pass)
    if (ncol(X) != p || any(!is.finite(X))) stop(
      "The smoothing-mixture design does not match the fitted coefficients.",
      call. = FALSE)
    sampling_check_bam_fixed(model, X, "smoothing-mixture")
    XtWX <- XtWX + crossprod(X, X * w[i])
    XtWy <- XtWy + drop(crossprod(X, w[i] * (y[i] - offset[i])))
  }
  penalties <- list()
  for (sm in model$smooth) for (S in sm$S) {
    penalties[[length(penalties) + 1L]] <- list(
      index = sm$first.para:sm$last.para, S = S)
  }
  setup <- list(XtWX = XtWX, XtWy = XtWy, phi = phi, penalties = penalties,
    log_sp = log(sp), log_sp_cov = log_sp_cov, L = L, lsp0 = lsp0,
    names = names(stats::coef(model)))
  conditional <- sampling_smoothing_conditional(setup, log(sp))
  beta <- unname(stats::coef(model))
  Vp <- unname(stats::vcov(model, unconditional = FALSE))
  mean_error <- max(abs(conditional$mean - beta)) / max(1, max(abs(beta)))
  covariance_error <- max(abs(conditional$covariance - Vp)) /
    max(.Machine$double.eps, max(abs(Vp)))
  if (mean_error > 1e-5 || covariance_error > 1e-4) stop(
    "The reconstructed Gaussian conditional distribution does not agree with the fitted coefficients and covariance; this fitting configuration is unsupported.",
    call. = FALSE)
  derivative <- matrix(0, p, q)
  for (k in seq_along(penalties)) {
    j <- penalties[[k]]$index
    v <- numeric(p)
    v[j] <- drop(penalties[[k]]$S %*% beta[j]) * full_sp[k]
    derivative <- derivative - drop(conditional$covariance %*% v / phi) %o% L[k, ]
  }
  fitted_derivative <- model$db.drho
  derivative_error <- NA_real_
  if (!is.null(fitted_derivative)) {
    if (ncol(fitted_derivative) == n_pen) {
      fitted_derivative <- fitted_derivative %*% L
    } else if (ncol(fitted_derivative) != q) stop(
      "Cannot match fitted coefficient derivatives to the free log-smoothing parameters.",
      call. = FALSE)
    derivative_error <- max(abs(derivative - fitted_derivative)) /
      max(1, max(abs(derivative)))
    if (!(inherits(model, "bam") && identical(model$method, "fREML")) &&
        derivative_error > 1e-4) stop(
      "The reconstructed smoothing derivatives do not agree with db.drho.",
      call. = FALSE)
  }
  sp_vcov <- mgcv::sp.vcov(model, edge.correct = FALSE)
  setup$audit <- list(method = model$method, fitting = info$fitting,
    covariance_source = "V.sp", parameter_layout = info$V_sp_layout,
    interpretation = "approximate estimation-uncertainty propagation",
    free_sp = names(sp),
    full_sp = names(full_sp), V_sp_dimension = nrow(V),
    V_sp_log_scale_position = if (nrow(V) == q + 1L) q + 1L else NA_integer_,
    sp_vcov_dimension = if (is.null(sp_vcov)) NA_integer_ else nrow(sp_vcov),
    sp_vcov_log_scale_position = if (!is.null(sp_vcov) &&
      nrow(sp_vcov) == q + 1L) q + 1L else NA_integer_,
    spind = attr(V, "spind"), mapping = L, fitted_log_sp_covariance = log_sp_cov,
    mean_error = mean_error, covariance_error = covariance_error,
    derivative_error = derivative_error, derivative = derivative,
    fitted_derivative_coordinates = if (inherits(model, "bam") &&
      identical(model$method, "fREML")) "internal fitting basis" else "fitted coefficients",
    conditional_covariance = Vp, corrected_covariance = model$Vc,
    dispersion = phi, dispersion_sampled = FALSE)
  setup
}

sampling_smoothing_conditional <- function(setup, log_sp) {
  if (length(log_sp) != length(setup$log_sp) || any(!is.finite(log_sp))) stop(
    "Supply one finite log value for each free smoothing parameter.",
    call. = FALSE)
  multiplier <- exp(drop(setup$L %*% log_sp) + setup$lsp0)
  if (any(!is.finite(multiplier) | multiplier <= 0)) stop(
    "A sampled smoothing parameter is outside the numerically supported range.",
    call. = FALSE)
  P <- setup$XtWX
  for (k in seq_along(setup$penalties)) {
    block <- setup$penalties[[k]]
    j <- block$index
    P[j, j] <- P[j, j, drop = FALSE] + multiplier[k] * block$S
  }
  R <- tryCatch(chol(P), error = function(e) NULL)
  if (is.null(R)) stop(
    "The Gaussian conditional precision is not positive definite.",
    call. = FALSE)
  inverse <- chol2inv(R)
  list(mean = drop(inverse %*% setup$XtWy),
    covariance = setup$phi * inverse)
}

sampling_smoothing_draws <- function(setup, n_draws = 1000L, seed = NULL,
                                     log_sp_cov = setup$log_sp_cov) {
  n_draws <- sampling_count(n_draws, "n_draws")
  q <- length(setup$log_sp)
  if (!is.matrix(log_sp_cov) || !identical(dim(log_sp_cov), c(q, q)) ||
      any(!is.finite(log_sp_cov)) ||
      max(abs(log_sp_cov - t(log_sp_cov))) > 1e-10 ||
      min(eigen(log_sp_cov, symmetric = TRUE, only.values = TRUE)$values) < -1e-10) {
    stop("Supply a finite positive semidefinite log-smoothing covariance of the expected size.",
      call. = FALSE)
  }
  draw <- function() {
    rho <- matrix(mgcv::rmvn(n_draws, setup$log_sp, log_sp_cov), nrow = n_draws)
    p <- ncol(setup$XtWX)
    beta <- means <- matrix(NA_real_, n_draws, p,
      dimnames = list(NULL, setup$names))
    average_covariance <- matrix(0, p, p)
    for (i in seq_len(n_draws)) {
      conditional <- sampling_smoothing_conditional(setup, rho[i, ])
      means[i, ] <- conditional$mean
      average_covariance <- average_covariance + conditional$covariance / n_draws
      beta[i, ] <- mgcv::rmvn(1L, conditional$mean, conditional$covariance)
    }
    between_covariance <- if (n_draws > 1L) stats::cov(means) else matrix(0, p, p)
    list(bs = beta, log_sp = rho, conditional_means = means,
      mean_conditional_covariance = average_covariance,
      covariance_conditional_means = between_covariance,
      total_covariance = average_covariance + between_covariance,
      audit = setup$audit)
  }
  if (is.null(seed)) draw() else withr::with_seed(seed, draw())
}
