# A conditional coefficient posterior. Constants independent of beta are omitted.
# The response and weights here are mgcv's initialised fitting quantities;
# in particular, binomial weights already include the numbers of trials.
sampling_target <- function(model, block_size = 10000L, dense_limit = 5e6,
                            compiled = FALSE) {
  sampling_target_check(model, "gratia")
  fam <- stats::family(model)
  family <- fam$family
  kind <- if (family == "gaussian" && fam$link == "identity") "gaussian" else
    if (family == "poisson" && fam$link == "log") "poisson" else
    if (family == "binomial" && fam$link == "logit") "binomial" else
    if (family == "Gamma" && fam$link == "log") "gamma" else
    if (startsWith(family, "Negative Binomial") && fam$link == "log") "nb" else NULL
  if (is.null(kind)) stop("The gratia MH backend supports Gaussian identity, ",
    "Poisson log, binomial logit, Gamma log, and negative binomial log models.",
    call. = FALSE)
  block_size <- sampling_count(block_size, "block_size")
  beta <- as.numeric(stats::coef(model))
  p <- length(beta)
  phi <- model$sig2
  if (length(phi) != 1L || !is.finite(phi) || phi <= 0) stop(
    "A positive fitted dispersion is required.", call. = FALSE)
  theta <- if (kind == "nb") {
    if (is.function(fam$getTheta)) {
      if (inherits(fam, "extended.family")) fam$getTheta(TRUE) else fam$getTheta()
    } else NULL
  } else 1
  if (kind == "nb" && (length(theta) != 1L || !is.finite(theta) || theta <= 0)) {
    stop("Cannot recover the fitted negative binomial shape.", call. = FALSE)
  }
  S <- sampling_target_penalty(model, "gratia")
  model <- model_used_rows(model)
  y <- as.numeric(model$y)
  w <- model$prior.weights %||% rep(1, length(y))
  offset <- model$offset %||% rep(0, length(y))
  if (length(offset) == 1L) offset <- rep(offset, length(y))
  if (length(y) != nrow(model$model) || length(w) != length(y) ||
      length(offset) != length(y) || any(!is.finite(y)) ||
      any(!is.finite(w) | w < 0) || any(!is.finite(offset))) stop(
    "The fitted response, weights, offsets, and model rows do not align.", call. = FALSE)
  blocks <- split(seq_along(y), ceiling(seq_along(y) / block_size))
  # predict.gam evaluates the fitted bases at original covariates, including
  # for discrete bam fits. It does not replace them by new fitted bases.
  design <- function(i) {
    X <- mgcv::predict.gam(model, newdata = model$model[i, , drop = FALSE],
      type = "lpmatrix", na.action = stats::na.pass)
    if (ncol(X) != p || any(!is.finite(X))) stop(
      "The posterior design matrix does not match the fitted coefficients.", call. = FALSE)
    X
  }
  dense <- length(y) * p <= dense_limit
  X <- if (dense) design(seq_along(y)) else NULL
  V <- unname(stats::vcov(model))
  R <- tryCatch(chol(V), error = function(e) NULL)
  list(model = if (dense) NULL else model, X = X, blocks = blocks,
    y = y, w = w, offset = offset, S = S, phi = phi, theta = theta,
    kind = kind, beta = beta, V = V, R = R,
    names = names(stats::coef(model)), compiled = compiled, backend = "gratia")
}

sampling_target_eval <- function(target, beta, gradient = FALSE) {
  if (identical(target$backend, "family")) {
    if (gradient) stop("The family MH backend does not provide posterior gradients.",
      call. = FALSE)
    value <- target$log_likelihood(beta) -
      sum(beta * drop(target$S %*% beta)) / (2 * target$phi)
    return(if (is.finite(value)) value else -Inf)
  }
  lp <- -sum(beta * drop(target$S %*% beta)) / (2 * target$phi)
  grad <- if (gradient) -drop(target$S %*% beta) / target$phi else NULL
  for (i in target$blocks) {
    X <- if (!is.null(target$X)) target$X[i, , drop = FALSE] else
      mgcv::predict.gam(target$model, newdata = target$model$model[i, , drop = FALSE],
        type = "lpmatrix", na.action = stats::na.pass)
    if (isTRUE(target$compiled)) {
      result <- sampling_likelihood_cpp(X, beta, target$y[i], target$w[i],
        target$offset[i], target$kind, target$phi, target$theta, gradient)
      lp <- lp + result$value
      if (gradient) grad <- grad + result$gradient
    } else {
      eta <- drop(X %*% beta) + target$offset[i]
      result <- sampling_likelihood(eta, target$y[i], target$w[i],
        target$kind, target$phi, target$theta)
      lp <- lp + sum(result$value)
      if (gradient) grad <- grad + drop(crossprod(X, result$score))
    }
  }
  if (!is.finite(lp)) lp <- -Inf
  if (gradient) list(value = lp, gradient = unname(grad)) else lp
}

sampling_likelihood <- function(eta, y, w, kind, phi, theta) {
  softplus <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))
  if (kind == "gaussian") {
    value <- -(y - eta)^2 / (2 * phi)
    score <- (y - eta) / phi
  } else if (kind == "poisson") {
    mu <- exp(eta)
    value <- (y * eta - mu) / phi
    score <- (y - mu) / phi
  } else if (kind == "binomial") {
    value <- (y * eta - softplus(eta)) / phi
    score <- (y - stats::plogis(eta)) / phi
  } else if (kind == "gamma") {
    ratio <- y * exp(-eta)
    value <- (-ratio - eta) / phi
    score <- (ratio - 1) / phi
  } else {
    value <- (y * eta - (y + theta) * softplus(eta - log(theta))) / phi
    score <- (y - (y + theta) * stats::plogis(eta - log(theta))) / phi
  }
  # Zero-weight observations contribute nothing, including at extreme eta.
  value[w == 0] <- score[w == 0] <- 0
  list(value = w * value, score = w * score)
}

sampling_mh <- function(target, n, burnin, thin, t_df, rw_scale,
                        batch_size = 256L, initial = NULL) {
  mu <- target$beta
  if (identical(target$backend, "family")) {
    out <- sampling_mh_family_cpp(target$S, mu, sampling_factor(target),
      initial %||% mu, target$phi, n, burnin, thin, t_df, rw_scale,
      target$log_likelihood)
    colnames(out$bs) <- target$names
    return(out)
  }
  if (isTRUE(target$compiled) && !is.null(target$X)) {
    out <- sampling_mh_cpp(target$X, target$S, mu, sampling_factor(target),
      initial %||% mu, target$y, target$w, target$offset, target$kind,
      target$phi, target$theta, n, burnin, thin, t_df, rw_scale)
    colnames(out$bs) <- target$names
    return(out)
  }
  R <- sampling_factor(target)
  p <- length(mu)
  beta <- initial %||% mu
  lp <- sampling_target_eval(target, beta)
  if (!is.finite(lp)) stop("The initial coefficient vector has non-finite posterior density.", call. = FALSE)
  log_q <- function(b) {
    z <- forwardsolve(t(R), b - mu)
    -(t_df + p) / 2 * log1p(sum(z^2) / t_df)
  }
  lq <- log_q(beta)
  total <- burnin + n * thin
  out <- matrix(NA_real_, n, p, dimnames = list(NULL, target$names))
  accept <- rw_accept <- attempts <- 0L
  rw <- is.finite(rw_scale) && rw_scale > 0
  pos <- 0L
  for (start in seq.int(1L, total, by = batch_size)) {
    size <- min(batch_size, total - start + 1L)
    proposal <- matrix(stats::rnorm(size * p), size, p) %*% R
    proposal <- proposal / sqrt(stats::rchisq(size, t_df) / t_df)
    proposal <- sweep(proposal, 2L, mu, "+")
    steps <- if (rw) matrix(stats::rnorm(size * p), size, p) %*% R * sqrt(rw_scale) else NULL
    u <- log(stats::runif(size))
    urw <- if (rw) log(stats::runif(size)) else NULL
    for (j in seq_len(size)) {
      iter <- start + j - 1L
      candidate <- proposal[j, ]
      next_lp <- sampling_target_eval(target, candidate)
      next_lq <- log_q(candidate)
      keep <- is.finite(next_lp) && u[j] < next_lp - lp + lq - next_lq
      if (keep) {
        beta <- candidate
        lp <- next_lp
        lq <- next_lq
      }
      if (iter > burnin) {
        attempts <- attempts + 1L
        accept <- accept + keep
      }
      if (rw) {
        candidate <- beta + steps[j, ]
        next_lp <- sampling_target_eval(target, candidate)
        keep_rw <- is.finite(next_lp) && urw[j] < next_lp - lp
        if (keep_rw) {
          beta <- candidate
          lp <- next_lp
          lq <- log_q(beta)
        }
        if (iter > burnin) rw_accept <- rw_accept + keep_rw
      }
      if (iter > burnin && (iter - burnin - 1L) %% thin == 0L) {
        pos <- pos + 1L
        out[pos, ] <- beta
      }
    }
  }
  list(bs = out, accept = accept / attempts,
    rw.accept = if (rw) rw_accept / attempts else NA_real_)
}

# A singular proposal cannot explore all fitted coefficient directions.
# Dropped-coefficient restrictions are not encoded in this target. Do not
# propose moves outside those restrictions by adding covariance jitter.
sampling_factor <- function(target) {
  if (is.null(target$R)) stop("The fitted coefficient covariance is singular; ",
    "the ", target$backend %||% "gratia",
    " MH backend requires identifiable coefficient directions.", call. = FALSE)
  target$R
}

# Shared checks and penalty setup run before any chains are started.
sampling_target_check <- function(model, backend) {
  if (!inherits(model, "gam") || inherits(model, "scam") ||
      isTRUE(attr(model, "gratia.gamm4"))) stop(
    "The ", backend, " MH backend requires an ordinary gam or bam fit.", call. = FALSE)
  if (any(startsWith(model$method %||% "", "lme.")) ||
      identical(model$method, "PQL")) stop(
    "The ", backend, " MH backend does not support GAMM components; use Gaussian sampling.",
    call. = FALSE)
  if (!is.null(model$AR1.rho) && model$AR1.rho != 0) stop(
    "The ", backend, " MH backend requires independent observations (rho = 0).", call. = FALSE)
}

sampling_target_penalty <- function(model, backend) {
  unsupported <- c("paraPen", "H", "G")
  if (!is.null(model$paraPen) || any(vapply(unsupported, function(nm) {
    !is.null(model$call[[nm]]) && !identical(model$call[[nm]], quote(NULL))
  }, logical(1L)))) stop("The ", backend, " MH backend does not yet support 'paraPen', 'H', or pre-fit 'G' objects.", call. = FALSE)
  p <- length(stats::coef(model))
  # full.sp expands shared and fixed smoothing parameters to penalty order.
  sp <- model$full.sp %||% model$sp
  n_pen <- sum(vapply(model$smooth, function(sm) length(sm$S), integer(1L)))
  if (length(sp) != n_pen || any(!is.finite(sp) | sp < 0)) stop(
    "Cannot match fitted smoothing parameters to the smooth penalties.", call. = FALSE)
  S <- matrix(0, p, p)
  k <- 0L
  for (sm in model$smooth) {
    j <- sm$first.para:sm$last.para
    for (penalty in sm$S) {
      k <- k + 1L
      S[j, j] <- S[j, j, drop = FALSE] + sp[k] * penalty
    }
  }
  S
}
