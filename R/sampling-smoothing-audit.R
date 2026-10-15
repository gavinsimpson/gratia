# Internal audit of stored fitting information. Never evaluate the model call:
# its method can be an unavailable symbol, and mgcv can change it during fitting.
sampling_smoothing_info <- function(model) {
  method <- model$method
  bam <- inherits(model, "bam")
  discrete <- bam && !is.null(model$dinfo)
  fam <- stats::family(model)
  nth <- as.integer(fam$n.theta %||% 0L)
  q <- length(model$sp)
  likelihood <- method %in% c("ML", "REML", "fREML")
  # Extended-family bam updates theta separately from the smoothing optimiser.
  # Its scalar parameter ordering has not been validated for this research code.
  known_layout <- likelihood && !(bam && inherits(fam, "extended.family"))
  layout <- function(V, fast = FALSE) {
    if (!known_layout || !is.matrix(V)) return(NULL)
    scale <- isTRUE(model$scale.estimated) && !fast
    expected <- nth + q + as.integer(scale)
    if (!identical(dim(V), c(expected, expected))) return(NULL)
    list(family = seq_len(nth), log_sp = nth + seq_len(q),
      log_scale = if (scale) expected else integer(),
      family_scale = "family internal parameterisation")
  }
  theta <- if (is.function(fam$getTheta)) fam$getTheta() else NULL
  theta_response <- if (is.function(fam$getTheta))
    tryCatch(fam$getTheta(TRUE), error = function(e) NULL) else NULL
  V <- model$V.sp
  sp_vcov <- mgcv::sp.vcov(model, edge.correct = FALSE)
  list(sp = model$sp, full.sp = model$full.sp,
    V.sp = V, spind = attr(V, "spind"), sp.vcov = sp_vcov,
    V_sp_layout = layout(V, fast = bam && identical(method, "fREML")),
    sp_vcov_layout = layout(sp_vcov), db.drho = model$db.drho,
    Vc = model$Vc, Vg = model$Vg, method = method,
    family_parameters = list(estimated = nth, internal = theta,
      response = theta_response, sampled = FALSE),
    fitting = list(requested_method = model$call$method %||%
        if (bam) "fREML" else "GCV.Cp",
      actual_method = method, stored_score_label = names(model$gcv.ubre),
      stored_optimizer = model$optimizer, discrete = discrete,
      # estimate.gam selects BFGS for NCV even when gam's final object reports
      # "magic" for a Gaussian additive fit in mgcv 1.9-4.
      ncv_optimizer = if (!bam && method %in% c("NCV", "QNCV"))
        "BFGS (selected by estimate.gam)" else NULL))
}

# Audit a local delta-method contribution, not a coefficient posterior or a
# nonlinear mixture. mgcv's final gam Vg is a sandwich covariance in log(sp),
# and its NCV Vc is this contribution alone, not Vp plus the contribution.
sampling_smoothing_ncv_delta <- function(model) {
  sampling_target_check(model, "smoothing-mixture")
  if (!model$method %in% c("NCV", "QNCV")) stop(
    "The NCV audit requires an actual NCV or QNCV fit.", call. = FALSE)
  if (inherits(model, "bam")) stop(
    "Discrete bam NCV does not retain Vg: an inverse NCV Hessian cannot replace a smoothing-estimator covariance.",
    call. = FALSE)
  fam <- stats::family(model)
  if (!identical(fam$family, "gaussian") || !identical(fam$link, "identity")) stop(
    "The NCV delta-method research calculation currently requires Gaussian identity fits; family parameters are not propagated.",
    call. = FALSE)
  sampling_target_penalty(model, "smoothing-mixture")
  if (any((model$offset %||% 0) != 0)) stop(
    "The NCV delta-method research calculation does not support nonzero fitting offsets: the native NCV prediction convention needs separate verification.",
    call. = FALSE)
  q <- length(model$sp)
  if (!q || length(model$full.sp %||% model$sp) != q) stop(
    "The NCV audit cannot identify shared or fixed smoothing-parameter mappings from this fitted object.",
    call. = FALSE)
  V <- model$Vg
  # bfgs drops matrix dimensions when there is one free smoothing parameter.
  if (q == 1L && is.numeric(V) && length(V) == 1L && is.null(dim(V))) {
    V <- matrix(V, 1L, 1L)
  }
  J <- model$db.drho
  p <- length(stats::coef(model))
  if (!is.matrix(V) || !identical(dim(V), c(q, q)) || any(!is.finite(V)) ||
      max(abs(V - t(V))) > 1e-8 ||
      min(eigen(V, symmetric = TRUE, only.values = TRUE)$values) < -1e-8 ||
      !is.matrix(J) || !identical(dim(J), c(p, q)) || any(!is.finite(J))) stop(
    "Cannot identify a finite NCV smoothing-estimator covariance and matching coefficient derivatives.",
    call. = FALSE)
  propagated <- J %*% V %*% t(J)
  if (!is.matrix(model$Vc) || !identical(dim(model$Vc), c(p, p)) ||
      any(!is.finite(model$Vc)) ||
      max(abs(model$Vc - propagated)) > 1e-7 * max(1, max(abs(propagated)))) stop(
    "The stored NCV correction does not agree with db.drho and Vg; its parameter mapping is unsupported.",
    call. = FALSE)
  list(log_sp = log(model$sp), log_sp_covariance = V,
    derivative = J, propagated_covariance = propagated,
    covariance_source = "Vg", interpretation =
      "local propagation of approximate log-smoothing estimator covariance",
    # NCV can replace Vp using a jackknife calculation. Retain that information
    # without adding it to the delta term and risking counting variation twice.
    coefficient_covariance_replaced = !is.null(attr(model$Ve, "Vp")),
    fitting = sampling_smoothing_info(model)$fitting)
}
