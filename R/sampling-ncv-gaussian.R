# Internal research for independent Gaussian leave-one-out NCV. Matrices here
# are dense in observations; this is deliberately excluded from public samplers.
sampling_ncv_gaussian_setup <- function(model, max_n = 1000L) {
  native <- sampling_smoothing_ncv_delta(model)
  max_n <- sampling_count(max_n, "max_n")
  nei <- model$call$nei
  if (!is.null(nei) && !identical(nei, quote(NULL))) {
    parts <- if (is.call(nei) && identical(nei[[1L]], as.name("list")))
      as.list(nei)[-1L] else NULL
    if (is.null(parts) || !identical(names(parts), "jackknife")) stop(
      "The Gaussian NCV covariance prototype requires default leave-one-out neighbourhoods; an explicit nei expression cannot be verified.",
      call. = FALSE)
  }
  gamma <- model$call$gamma
  if (!is.null(gamma) && !(is.numeric(gamma) && length(gamma) == 1L &&
      is.finite(gamma) && gamma == 1)) stop(
    "The Gaussian NCV covariance prototype requires gamma = 1.", call. = FALSE)
  model <- model_used_rows(model)
  n <- nrow(model$model)
  if (n > max_n) stop(
    "The Gaussian NCV covariance prototype uses dense observation matrices; increase max_n explicitly for this research calculation.",
    call. = FALSE)
  w <- model$prior.weights %||% rep(1, n)
  if (length(w) != n || any(!is.finite(w) | w <= 0)) stop(
    "The Gaussian NCV covariance prototype requires positive finite precision weights.",
    call. = FALSE)
  X <- mgcv::predict.gam(model, newdata = model$model, type = "lpmatrix",
    na.action = stats::na.pass)
  p <- length(stats::coef(model))
  if (ncol(X) != p || any(!is.finite(X)) || length(model$y) != n ||
      any(!is.finite(model$y)) || length(model$sig2) != 1L || !is.finite(model$sig2) || model$sig2 <= 0) stop(
    "Cannot recover an aligned finite Gaussian fitting design, response and dispersion.",
    call. = FALSE)
  penalties <- list()
  for (sm in model$smooth) for (S in sm$S) {
    block <- matrix(0, p, p)
    j <- sm$first.para:sm$last.para
    block[j, j] <- S
    penalties[[length(penalties) + 1L]] <- block
  }
  setup <- list(Z = sqrt(w) * X, response = as.numeric(model$y),
    fitted = drop(X %*% stats::coef(model)), weights = w, phi = model$sig2,
    penalties = penalties, log_sp = log(model$sp), native = native)
  check <- sampling_ncv_gaussian_eval(setup)
  if (abs(check$criterion - as.numeric(model$gcv.ubre)) >
      1e-7 * max(1, abs(check$criterion)) ||
      max(abs(check$gradient - drop(model$outer.info$grad))) > 1e-5) stop(
    "The exact Gaussian leave-one-out criterion does not match the fitted NCV criterion or gradient.",
    call. = FALSE)
  setup
}

sampling_ncv_gaussian_eval <- function(setup, log_sp = setup$log_sp,
                                       response = setup$response) {
  q <- length(setup$log_sp)
  n <- nrow(setup$Z)
  if (length(log_sp) != q || any(!is.finite(log_sp)) ||
      any(!is.finite(exp(log_sp)) | exp(log_sp) <= 0) || length(response) != n ||
      any(!is.finite(response))) stop(
    "Supply finite smoothing log parameters and one finite response per fitting row.",
    call. = FALSE)
  P <- crossprod(setup$Z)
  for (j in seq_len(q)) P <- P + exp(log_sp[j]) * setup$penalties[[j]]
  inverse <- chol2inv(chol(P))
  ZI <- setup$Z %*% inverse
  A <- tcrossprod(ZI, setup$Z)
  Q <- diag(n) - A
  d <- diag(Q)
  if (any(d <= 1e-10)) stop(
    "A leave-one-out leverage denominator is too small.", call. = FALSE)
  B <- Q / d
  B1 <- lapply(seq_len(q), function(j) {
    A1 <- -exp(log_sp[j]) * tcrossprod(ZI %*% setup$penalties[[j]], ZI)
    -A1 / d + Q * diag(A1) / d^2
  })
  z <- sqrt(setup$weights) * response
  v <- drop(B %*% z)
  contributions <- vapply(B1, function(Bj) 2 * v * drop(Bj %*% z), numeric(n))
  # The derivative of K = B'B is symmetric, and the total gradient is z'Kj z.
  K1 <- lapply(B1, function(Bj) crossprod(Bj, B) + crossprod(B, Bj))
  list(criterion = sum(v^2), gradient = colSums(contributions),
    contributions = contributions, B = B, B1 = B1, K1 = K1)
}

# Exact moments at a fixed log(sp), conditional on a supplied Gaussian mean and
# dispersion. In whitened coordinates z ~ N(mean_z, phi I). These are moments of
# the criterion gradient, not exact moments of its nonlinear minimiser.
sampling_ncv_gaussian_moments <- function(op, mean_z, phi) {
  n <- nrow(op$B)
  q <- length(op$B1)
  if (length(mean_z) != n || any(!is.finite(mean_z)) || length(phi) != 1L ||
      !is.finite(phi) || phi < 0) stop(
    "Supply one finite Gaussian mean per row and a nonnegative dispersion.",
    call. = FALSE)
  v <- drop(op$B %*% mean_z)
  derivative_means <- lapply(op$B1, function(Bj) drop(Bj %*% mean_z))
  C00 <- phi * rowSums(op$B^2)
  C0 <- lapply(op$B1, function(Bj) phi * rowSums(op$B * Bj))
  mean_contributions <- vapply(seq_len(q), function(j) {
    2 * (v * derivative_means[[j]] + C0[[j]])
  }, numeric(n))
  noise <- signal <- expected_crossprod <- matrix(0, q, q)
  Kmu <- lapply(op$K1, function(Kj) drop(Kj %*% mean_z))
  for (j in seq_len(q)) for (k in seq_len(q)) {
    noise[j, k] <- 2 * phi^2 * sum(op$K1[[j]] * op$K1[[k]])
    signal[j, k] <- 4 * phi * sum(Kmu[[j]] * Kmu[[k]])
    dj <- derivative_means[[j]]
    dk <- derivative_means[[k]]
    Cjk <- phi * rowSums(op$B1[[j]] * op$B1[[k]])
    # E[4 v_i^2 d_ij d_ik], including the nonzero means of individual rows.
    expected_crossprod[j, k] <- 4 * sum(v^2 * dj * dk + v^2 * Cjk +
      2 * v * dj * C0[[k]] + 2 * v * dk * C0[[j]] + dj * dk * C00 +
      C00 * Cjk + 2 * C0[[j]] * C0[[k]])
  }
  mean_outer <- crossprod(mean_contributions)
  within <- expected_crossprod - mean_outer
  list(mean_criterion = sum(v^2) + phi * sum(op$B^2),
    mean_gradient = colSums(mean_contributions),
    mean_contributions = mean_contributions,
    gradient_covariance = noise + signal, noise_covariance = noise,
    signal_covariance = signal, expected_crossprod = expected_crossprod,
    mean_outer = mean_outer, within_observation_covariance = within,
    between_observation_covariance = noise + signal - within)
}

sampling_ncv_gaussian_covariance <- function(setup,
    mean_response = setup$fitted, dispersion = setup$phi,
    curvature = c("observed", "expected"), log_sp = setup$log_sp) {
  curvature <- match.arg(curvature)
  supplied_mean <- !missing(mean_response)
  if (length(mean_response) != nrow(setup$Z) || any(!is.finite(mean_response))) stop(
    "Supply one finite Gaussian mean per fitting row.", call. = FALSE)
  mean_z <- sqrt(setup$weights) * mean_response
  op <- sampling_ncv_gaussian_eval(setup, log_sp)
  moments <- sampling_ncv_gaussian_moments(op, mean_z, dispersion)
  gradient <- function(rho) {
    next_op <- sampling_ncv_gaussian_eval(setup, rho)
    if (curvature == "observed") next_op$gradient else
      sampling_ncv_gaussian_moments(next_op, mean_z, dispersion)$mean_gradient
  }
  h <- 1e-4
  H <- vapply(seq_along(log_sp), function(j) {
    step <- rep(0, length(log_sp)); step[j] <- h
    (gradient(log_sp + step) - gradient(log_sp - step)) / (2 * h)
  }, numeric(length(log_sp)))
  H <- (H + t(H)) / 2
  R <- tryCatch(chol(H), error = function(e) NULL)
  if (is.null(R)) stop(
    "The Gaussian NCV criterion curvature is not positive definite at this log(sp).",
    call. = FALSE)
  Hi <- chol2inv(R)
  V <- Hi %*% moments$gradient_covariance %*% Hi
  J <- setup$native$derivative
  list(log_sp_covariance = V, gradient_moments = moments, hessian = H,
    # Native J is evaluated at fitted log(sp); do not propagate it elsewhere.
    coefficient_mean_contribution = if (identical(as.numeric(log_sp), as.numeric(setup$log_sp)))
      J %*% V %*% t(J) else NULL,
    mean_source = if (supplied_mean) "supplied Gaussian mean" else "fitted Gaussian mean",
    dispersion = dispersion, curvature = curvature,
    interpretation = "local propagation of Gaussian NCV estimator uncertainty")
}
