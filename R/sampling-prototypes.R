# Research implementations: these methods are deliberately not public choices
# of method. Benchmarks compare them with MH on the same conditional target.
sampling_mala <- function(target, n, burnin = 1000L, step_size = 0.5) {
  R <- sampling_factor(target)
  p <- length(target$beta)
  z <- rep(0, p)
  evaluate <- function(z) {
    beta <- target$beta + drop(z %*% R)
    result <- sampling_target_eval(target, beta, gradient = TRUE)
    list(value = result$value, gradient = drop(R %*% result$gradient))
  }
  current <- evaluate(z)
  out <- matrix(NA_real_, n, p, dimnames = list(NULL, target$names))
  log_step <- log(step_size)
  accepted <- 0L
  for (i in seq_len(burnin + n)) {
    h <- exp(log_step)
    forward <- z + h^2 / 2 * current$gradient
    candidate <- forward + h * stats::rnorm(p)
    next_state <- evaluate(candidate)
    reverse <- candidate + h^2 / 2 * next_state$gradient
    ratio <- next_state$value - current$value +
      (sum((candidate - forward)^2) - sum((z - reverse)^2)) / (2 * h^2)
    probability <- if (is.finite(ratio)) exp(min(0, ratio)) else 0
    if (stats::runif(1) < probability) {
      z <- candidate
      current <- next_state
      if (i > burnin) accepted <- accepted + 1L
    }
    if (i <= burnin) {
      log_step <- max(log(1e-5), min(log(5),
        log_step + (probability - 0.574) / sqrt(i + 10)))
    } else out[i - burnin, ] <- target$beta + drop(z %*% R)
  }
  list(bs = out, accept = accepted / n, step_size = exp(log_step))
}

sampling_elliptical <- function(target, n, burnin = 1000L) {
  R <- sampling_factor(target)
  p <- length(target$beta)
  z <- rep(0, p)
  residual <- function(z) {
    # Divide the target by the N(beta_hat, Vp) reference, rather than
    # replacing the possibly improper smoothing prior by that reference.
    sampling_target_eval(target, target$beta + drop(z %*% R)) + sum(z^2) / 2
  }
  current <- residual(z)
  out <- matrix(NA_real_, n, p, dimnames = list(NULL, target$names))
  evaluations <- 0L
  for (i in seq_len(burnin + n)) {
    nu <- stats::rnorm(p)
    threshold <- current + log(stats::runif(1))
    angle <- stats::runif(1, 0, 2 * pi)
    lower <- angle - 2 * pi
    upper <- angle
    for (attempt in seq_len(10000L)) {
      candidate <- z * cos(angle) + nu * sin(angle)
      next_value <- residual(candidate)
      evaluations <- evaluations + 1L
      if (is.finite(next_value) && next_value >= threshold) break
      if (angle < 0) lower <- angle else upper <- angle
      angle <- stats::runif(1, lower, upper)
    }
    if (!is.finite(next_value) || next_value < threshold) stop(
      "Elliptical slice bracket did not find a draw.", call. = FALSE)
    z <- candidate
    current <- next_value
    if (i > burnin) out[i - burnin, ] <- target$beta + drop(z %*% R)
  }
  list(bs = out, evaluations = evaluations)
}

sampling_smoothing_info <- function(model) {
  V <- model$V.sp
  list(sp = model$sp, full.sp = model$full.sp,
    V.sp = V, spind = attr(V, "spind"),
    sp.vcov = mgcv::sp.vcov(model), db.drho = model$db.drho,
    Vc = model$Vc, method = model$method)
}

sampling_smoothing_mixture <- function(model, n = 100L, seed = 42L) {
  fam <- stats::family(model)
  if (fam$family != "gaussian" || fam$link != "identity") stop(
    "The smoothing-mixture prototype currently supports Gaussian identity fits.", call. = FALSE)
  V <- model$V.sp
  p <- length(model$sp)
  if (is.null(V) || p == 0L) stop("No log-smoothing covariance is available.", call. = FALSE)
  ind <- attr(V, "spind") %||% seq_len(p)
  if (length(ind) != p || max(ind) > nrow(V)) stop(
    "Cannot identify the log-smoothing covariance block.", call. = FALSE)
  withr::with_seed(seed, {
    rho <- mgcv::rmvn(n, log(model$sp), V[ind, ind, drop = FALSE])
    rho <- matrix(rho, nrow = n)
    out <- matrix(NA_real_, n, length(coef(model)))
    failures <- rep(NA_character_, n)
    for (i in seq_len(n)) {
      # Refitting updates both the conditional mode and covariance. This is
      # an approximate marginal mixture, not a joint posterior MCMC update.
      fit <- tryCatch(stats::update(model, data = model$model, sp = exp(rho[i, ]), scale = model$sig2),
        error = identity)
      if (inherits(fit, "error")) {
        failures[i] <- conditionMessage(fit)
      } else out[i, ] <- mgcv::rmvn(1L, stats::coef(fit), stats::vcov(fit))
    }
    colnames(out) <- names(coef(model))
    list(bs = out, log_sp = rho, failures = failures, info = sampling_smoothing_info(model))
  })
}
