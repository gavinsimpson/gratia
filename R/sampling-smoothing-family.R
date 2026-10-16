# Native conditional normal approximations for ordinary and extended families.
# The original fitted design is a matrix term: no basis, knots, constraints or
# response expressions are reconstructed from the fitting call.
sampling_smoothing_family_setup <- function(model, block_size, dense_limit = 2e7) {
  model <- model_used_rows(model)
  family <- stats::family(model)
  info <- sampling_smoothing_info(model)
  layout <- info$V_sp_layout
  V <- model$V.sp
  sp <- model$sp
  full_sp <- model$full.sp %||% sp
  q <- length(sp)
  p <- length(stats::coef(model))
  n <- NROW(model$model)
  if (!q || is.null(layout) || !is.matrix(V) || any(!is.finite(V)) ||
      any(!is.finite(sp) | sp <= 0) || any(!is.finite(full_sp) | full_sp <= 0)) stop(
    "Cannot identify a finite covariance and ordering for the free log-smoothing parameters.",
    call. = FALSE)
  log_sp_cov <- V[layout$log_sp, layout$log_sp, drop = FALSE]
  if (is.null(tryCatch(chol(log_sp_cov), error = function(e) NULL))) stop(
    "The fitted log-smoothing covariance is not positive definite.", call. = FALSE)
  L <- attr(V, "L")
  n_pen <- length(full_sp)
  if (is.null(L)) {
    if (n_pen != q) stop("Cannot recover the smoothing-parameter mapping.", call. = FALSE)
    L <- diag(q)
  } else {
    prefix <- length(layout$family)
    rows <- prefix + seq_len(n_pen)
    if (!is.matrix(L) || max(rows) > nrow(L) || max(layout$log_sp) > ncol(L) ||
        any(!is.finite(L))) stop("Cannot recover the smoothing-parameter mapping.", call. = FALSE)
    L <- L[rows, layout$log_sp, drop = FALSE]
  }
  dense_limit <- sampling_count(dense_limit, "dense_limit")
  if (as.double(n) * p > dense_limit) stop(
    "Native non-Gaussian conditional updates require a dense fitting design; increase dense_limit explicitly for this research calculation.",
    call. = FALSE)
  X <- matrix(NA_real_, n, p)
  for (start in seq.int(1L, n, by = block_size)) {
    rows <- seq.int(start, min(n, start + block_size - 1L))
    block <- mgcv::predict.gam(model, newdata = model$model[rows, , drop = FALSE],
      type = "lpmatrix", na.action = stats::na.pass)
    sampling_check_bam_fixed(model, block, "smoothing-mixture")
    if (ncol(block) != p || any(!is.finite(block))) stop(
      "The conditional fitting design does not match the fitted coefficients.", call. = FALSE)
    X[rows, ] <- block
  }
  # Raw responses and weights retain trials, factors, censoring and group labels.
  # The family's initialisation must not receive binomial trial weights twice.
  y <- stats::model.response(model$model)
  w <- stats::model.weights(model$model) %||% rep(1, n)
  offset <- model$offset %||% rep(0, n)
  if (length(offset) == 1L) offset <- rep(offset, n)
  if (NROW(y) != n || length(w) != n || any(!is.finite(w) | w < 0) ||
      length(offset) != n || any(!is.finite(offset))) stop(
    "The raw response, weights, offsets and fitting rows do not align.", call. = FALSE)
  phi <- model$sig2
  if (length(phi) != 1L || !is.finite(phi) || phi <= 0) stop(
    "A positive fitted dispersion is required.", call. = FALSE)
  penalties <- list()
  for (sm in model$smooth) for (S in sm$S) {
    penalties[[length(penalties) + 1L]] <- list(index = sm$first.para:sm$last.para, S = S)
  }
  setup <- list(log_sp = log(sp), log_sp_cov = log_sp_cov, L = L,
    lsp0 = log(full_sp) - drop(L %*% log(sp)), penalties = penalties,
    names = names(stats::coef(model)), phi = phi, beta = unname(stats::coef(model)),
    family = sampling_smoothing_freeze_family(family), conditional_backend = "family")
  S <- sampling_smoothing_penalty(setup, full_sp)
  setup$prefit <- mgcv::gam(.response ~ .design - 1,
    data = list(.response = y, .design = X), weights = w, offset = offset,
    family = sampling_smoothing_freeze_family(family), H = S, scale = phi,
    method = "REML", fit = FALSE)
  # Preserve the original intercept flag for native null-deviance reporting;
  # the matrix term already includes that coefficient in the fitted basis.
  setup$prefit$intercept <- attr(model$pterms, "intercept") > 0L
  conditional <- sampling_smoothing_conditional(setup, setup$log_sp)
  Vp <- unname(stats::vcov(model, unconditional = FALSE))
  mean_error <- max(abs(conditional$mean - setup$beta)) / max(1, max(abs(setup$beta)))
  covariance_error <- max(abs(conditional$covariance - Vp)) / max(abs(Vp))
  if (mean_error > 1e-4 || covariance_error > 1e-3) stop(
    sprintf(paste0("The fixed-parameter native conditional does not reproduce the fitted coefficients and covariance ",
      "(relative errors %.3g and %.3g). Refit with tighter convergence tolerances or use a supported fitting configuration."),
      mean_error, covariance_error),
    call. = FALSE)
  setup$audit <- list(method = model$method, fitting = info$fitting,
    covariance_source = "V.sp", parameter_layout = layout,
    interpretation = "approximate estimation-uncertainty propagation",
    conditional_approximation = "native coefficient normal approximation",
    free_sp = names(sp), full_sp = names(full_sp), mapping = L,
    fitted_log_sp_covariance = log_sp_cov, mean_error = mean_error,
    covariance_error = covariance_error, conditional_covariance = Vp,
    corrected_covariance = model$Vc, dispersion = phi, dispersion_sampled = FALSE,
    family_parameters = info$family_parameters,
    conditional_backend = "family", dense_design = TRUE)
  setup
}

sampling_smoothing_penalty <- function(setup, multiplier) {
  p <- length(setup$names)
  S <- matrix(0, p, p)
  for (k in seq_along(setup$penalties)) {
    block <- setup$penalties[[k]]
    j <- block$index
    S[j, j] <- S[j, j, drop = FALSE] + multiplier[k] * block$S
  }
  S
}

sampling_smoothing_freeze_family <- function(family) {
  # Copy mutable family environments, including grouped component families.
  family <- unserialize(serialize(family, NULL))
  family <- sampling_smoothing_family_bindings(family)
  if (inherits(family, "extended.family")) {
    theta <- family$getTheta()
    family$n.theta <- 0L
    pre <- family$preinitialize
    if (is.function(pre)) {
      family$preinitialize <- local({
        original <- pre; fixed <- theta
        function(y, family) {
          result <- original(y, family)
          if (!is.null(result$family)) stop(
            "Conditional family initialisation cannot replace the fitted family.", call. = FALSE)
          result$Theta <- NULL
          family$putTheta(fixed)
          result
        }
      })
      attributes(family$preinitialize) <- attributes(pre)
    }
  }
  family
}

sampling_smoothing_family_conditional <- function(setup, multiplier) {
  G <- setup$prefit
  G$H <- sampling_smoothing_penalty(setup, multiplier)
  G$family <- sampling_smoothing_freeze_family(setup$family)
  fit <- mgcv::gam(G = G, method = "REML", scale = setup$phi, start = setup$beta)
  mean <- unname(stats::coef(fit))
  # Ordinary negbin can force unit scale in gam even when bam estimated it.
  # Scaling the covariance retains the fitted dispersion and the same mode.
  covariance <- unname(fit$Vp) * (setup$phi / fit$sig2)
  if (!isTRUE(fit$converged) || length(mean) != length(setup$names) ||
      any(!is.finite(mean)) || !identical(dim(covariance), c(length(mean), length(mean))) ||
      any(!is.finite(covariance)) ||
      is.null(tryCatch(chol(covariance), error = function(e) NULL))) stop(
    "The fixed-parameter native conditional did not converge with a finite positive definite covariance.",
    call. = FALSE)
  if (is.function(setup$family$getTheta) &&
      !isTRUE(all.equal(fit$family$getTheta(), setup$family$getTheta(), tolerance = 1e-12))) stop(
    "The native conditional changed fitted family parameters.", call. = FALSE)
  list(mean = mean, covariance = covariance)
}

# Some mgcv family closures have private environments whose parents are the
# caller environment. Supply missing native helpers in the copied environment
# so fitting does not depend on attaching mgcv or modify the fitted family.
sampling_smoothing_family_bindings <- function(family) {
  ns <- asNamespace("mgcv")
  for (fun in family[vapply(family, is.function, logical(1L))]) {
    env <- environment(fun)
    if (is.null(env) || isNamespace(env) || identical(env, .GlobalEnv) ||
        identical(env, baseenv()) || environmentIsLocked(env)) next
    for (name in all.names(body(fun), functions = TRUE)) {
      if (!exists(name, env, inherits = TRUE) && exists(name, ns, inherits = FALSE)) {
        value <- get(name, ns, inherits = FALSE)
        if (is.function(value)) assign(name, value, env)
      }
    }
  }
  if (is.function(family$getfl) && is.function(family$putfl)) {
    components <- family$getfl()
    for (j in seq_along(components)) {
      components[[j]] <- sampling_smoothing_family_bindings(components[[j]])
    }
    family$putfl(components)
  }
  family
}
