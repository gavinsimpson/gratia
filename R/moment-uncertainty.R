# Joint first-order approximation: conditional coefficient covariance plus
# sensitivity of those coefficients to mgcv's outer optimisation parameters.
# The ordering is family parameters, log smoothing parameters, then log scale.

# The outer ML/REML Hessian describes reml.scale, whereas sig2 and Vp can use
# the separate Pearson/Fletcher estimate. Use the likelihood scale throughout
# a local copy of the fit, including its conditional coefficient covariance.
moment_scale_model <- function(model) {
  scale <- model$reml.scale
  if (isTRUE(model$scale.estimated) && model$method %in% c("ML", "REML") &&
      length(scale) == 1L && is.finite(scale) && scale > 0) {
    ratio <- scale / model$sig2
    for (name in c("Vp", "Ve", "Vc")) {
      if (!is.null(model[[name]])) model[[name]] <- model[[name]] * ratio
    }
    model$sig2 <- model$scale <- scale
  }
  model
}

moment_parameter_context <- function(model, nuisance, unconditional) {
  family <- stats::family(model)
  p <- length(stats::coef(model))
  nth <- as.integer(family$n.theta %||% 0L)
  estimated_scale <- isTRUE(model$scale.estimated) &&
    (!inherits(family, "extended.family") || isTRUE(family$scale < 0))
  centre <- as.numeric(stats::coef(model))
  raw_theta <- if (is.function(family$getTheta) && inherits(family, "extended.family"))
    family$getTheta() else numeric()
  if (nuisance == "estimated" && family_type(family) == "negative_binomial" &&
      !inherits(family, "extended.family") && !model$method %in% c("ML", "REML"))
    stop("Legacy negbin fits do not retain enough information to propagate shape uncertainty. Use nb() with ML/REML, or explicitly use nuisance = 'fixed'.", call. = FALSE)
  q <- length(model$sp)
  global <- nth > 0L || estimated_scale
  smooth_only <- nuisance == "fixed" && unconditional && global && q > 0L
  if (!smooth_only && (nuisance == "fixed" || !global)) {
    # With no smoothing parameters, conditioning on global parameters leaves
    # Vp. Native Vc can still include global-parameter uncertainty in this case.
    native_correction <- unconditional && !(nuisance == "fixed" && global)
    return(list(centre = centre, covariance = get_vcov(model,
      unconditional = native_correction), nth = 0L, scale = FALSE,
      source = "coefficient covariance", fixed = nuisance == "fixed",
      raw_theta = raw_theta))
  }
  fail <- function() {
    if (smooth_only) stop(paste0(
      "Smoothing uncertainty conditional on fixed global parameters requires ",
      "a saved, positive-definite ML/REML outer Hessian and matching coefficient sensitivities. ",
      "Use unconditional = FALSE for this fitting configuration."), call. = FALSE)
    stop(paste0(
      "Joint uncertainty for estimated family parameters requires a gam fit with ",
      "a saved, positive-definite ML/REML outer Hessian and matching coefficient sensitivities. ",
      "This fitting configuration is unsupported; use nuisance = 'fixed' explicitly ",
      "or uncertainty = 'none'."), call. = FALSE)
  }
  k <- nth + q + as.integer(estimated_scale)
  H <- model$outer.info$hess
  D <- model$db.drho
  if (inherits(model, "scam") || !model$method %in% c("ML", "REML") ||
      !is.matrix(H) || !identical(dim(H), c(k, k)) || any(!is.finite(H)) ||
      !is.matrix(D) || nrow(D) != p || any(!is.finite(D)) ||
      length(raw_theta) < nth) fail()
  nu <- c(seq_len(nth), if (estimated_scale) k)
  # Ordinary coefficient estimates have zero derivative with respect to log
  # scale at fixed smoothing parameters in mgcv's parameterisation.
  if (estimated_scale) D <- cbind(D, 0)
  # db.drho uses the full penalty list; the Hessian uses free optimisation
  # parameters. Linked or fixed smoothing parameters need mgcv's saved map.
  L <- attr(model$V.sp, "L")
  if (!is.null(L)) {
    if (!is.matrix(L) || !identical(dim(L), c(ncol(D), k)) ||
        any(!is.finite(L))) fail()
    D <- D %*% L
  }
  if (!identical(dim(D), c(p, k))) fail()
  if (smooth_only) {
    selected <- nth + seq_len(q)
    precision <- (H[selected, selected, drop = FALSE] +
      t(H[selected, selected, drop = FALSE])) / 2
    R <- tryCatch(chol(precision), error = function(e) NULL)
    if (is.null(R)) fail()
    C <- chol2inv(R)
    sensitivity <- D[, selected, drop = FALSE]
    return(list(centre = centre, covariance = get_vcov(model, unconditional = FALSE) +
      sensitivity %*% C %*% t(sensitivity), nth = 0L, scale = FALSE,
      source = "smoothing uncertainty conditional on fixed global parameters",
      fixed = TRUE, joint_sampling = TRUE, raw_theta = raw_theta))
  }
  if (unconditional) {
    selected <- seq_len(k)
    source <- "outer Hessian and coefficient sensitivities"
  } else {
    selected <- nu
    source <- "outer Hessian conditional on smoothing parameters and coefficient sensitivities"
  }
  # Native V.sp may regularise all parameters, or use curvature from an edge
  # correction evaluated away from these coefficients and sensitivities.
  # Invert matching curvature here, so only smoothing uncertainty changes
  # when unconditional is requested and no extra prior is added to dispersion.
  H <- (H[selected, selected, drop = FALSE] +
    t(H[selected, selected, drop = FALSE])) / 2
  R <- tryCatch(chol(H), error = function(e) NULL)
  if (is.null(R)) fail()
  C <- chol2inv(R)
  D <- D[, selected, drop = FALSE]
  where <- match(nu, selected)
  base <- get_vcov(model, unconditional = FALSE)
  cross <- D %*% C[, where, drop = FALSE]
  covariance <- rbind(cbind(base + D %*% C %*% t(D), cross),
    cbind(t(cross), C[where, where, drop = FALSE]))
  centre <- c(centre, raw_theta[seq_len(nth)],
    if (estimated_scale) log(model$sig2))
  list(centre = centre, covariance = covariance, nth = nth,
    scale = estimated_scale, source = source, fixed = FALSE,
    raw_theta = raw_theta)
}

moment_gradient <- function(evaluate, centre, value) {
  J <- matrix(0, length(value), length(centre))
  for (j in seq_along(centre)) {
    h <- .Machine$double.eps^(1 / 3) * max(1, abs(centre[j]))
    a <- b <- centre
    a[j] <- a[j] + h
    b[j] <- b[j] - h
    J[, j] <- (evaluate(a) - evaluate(b)) / (2 * h)
  }
  J
}

# Analytic derivatives for Gaussian, Poisson, Gamma, inverse Gaussian and
# Tweedie models. Other families differentiate the complete mapping numerically,
# including any covariance coefficients.
moment_analytic_gradient <- function(context, parameters, point, moments) {
  f <- context$family
  if (is_gfam_family(f)) return(NULL)
  ft <- family_type(f)
  if (!ft %in% c("gaussian", "poisson", "gamma", "inverse_gaussian",
      "tweedie", "gammals", "twlss")) return(NULL)
  n <- NROW(context$X)
  p <- length(stats::coef(context$model))
  eta <- lss_predictors(context, stats::coef(context$model))
  native <- family_prediction_parameters(f, eta)
  mu <- if (is.matrix(native)) native[, 1L] else as.vector(native)
  value <- moment_family_values(f, eta, context$model$sig2,
    stats::coef(context$model))$values
  dm <- dl <- dp <- matrix(0, n, length(moment_names()),
    dimnames = list(NULL, moment_names()))
  dm[, "mean"] <- 1
  v <- value[, "variance"]
  s <- value[, "sd"]
  cv <- value[, "cv"]
  skew <- value[, "skewness"]
  excess <- value[, "excess_kurtosis"]
  power <- switch(ft, poisson = 1, gamma = 2, gammals = 2,
    inverse_gaussian = 3, tweedie = {
      th <- moment_theta(f)
      if (length(th)) th[1L] else get_tw_params(f)[1L]
    }, twlss = {
      ab <- get_tw_params(f)
      theta_2_power(native[, 2L], ab[1L], ab[2L])
    }, 0)
  dm[, "variance"] <- power * v / mu
  dm[, "sd"] <- power / 2 * s / mu
  dm[, "cv"] <- (power / 2 - 1) * cv / mu
  if (ft != "gaussian") {
    dm[, "skewness"] <- (power / 2 - 1) * skew / mu
    dm[, "excess_kurtosis"] <- (power - 2) * excess / mu
    dl[, "variance"] <- v
    dl[, "sd"] <- s / 2
    dl[, "cv"] <- cv / 2
    dl[, "skewness"] <- skew / 2
    dl[, "excess_kurtosis"] <- excess
  } else {
    dm[, c("variance", "sd")] <- 0
    dl[, "variance"] <- v
    dl[, "sd"] <- s / 2
    dl[, "cv"] <- cv / 2
  }
  if (ft %in% c("tweedie", "twlss")) {
    dp[, "variance"] <- v * log(mu)
    dp[, "sd"] <- s * log(mu) / 2
    dp[, "cv"] <- cv * log(mu) / 2
    dp[, "skewness"] <- cv + skew * log(mu) / 2
    dp[, "excess_kurtosis"] <- (4 * power - 1) * cv^2 + excess * log(mu)
  }
  dm[, "kurtosis"] <- dm[, "excess_kurtosis"]
  dl[, "kurtosis"] <- dl[, "excess_kurtosis"]
  dp[, "kurtosis"] <- dp[, "excess_kurtosis"]
  J <- matrix(0, n * length(moments), length(parameters$centre))
  slope_mean <- if (ft == "gammals") mu * f$linfo[[1L]]$mu.eta(eta[, 1L]) else
    if (inherits(f, "general.family")) f$linfo[[1L]]$mu.eta(eta[, 1L]) else
      f$mu.eta(eta[, 1L])
  add <- function(J, slope, index) {
    for (k in seq_along(moments)) {
      rows <- (k - 1L) * n + seq_len(n)
      J[rows, index] <- J[rows, index, drop = FALSE] +
        context$X[, index, drop = FALSE] * slope[, k]
    }
    J
  }
  J <- add(J, dm[, moments, drop = FALSE] * slope_mean, context$index[[1L]])
  if (ft %in% c("gammals", "twlss")) {
    j <- if (ft == "gammals") 2L else 3L
    J <- add(J, dl[, moments, drop = FALSE] * f$linfo[[j]]$mu.eta(eta[, j]),
      context$index[[j]])
  }
  if (ft == "twlss") {
    ab <- get_tw_params(f)
    z <- f$linfo[[2L]]$linkinv(eta[, 2L])
    slope <- (ab[2L] - ab[1L]) * stats::plogis(z) * stats::plogis(-z) *
      f$linfo[[2L]]$mu.eta(eta[, 2L])
    J <- add(J, dp[, moments, drop = FALSE] * slope, context$index[[2L]])
  }
  if (ft == "tweedie" && parameters$nth) {
    ab <- get_tw_params(f)
    z <- parameters$raw_theta[1L]
    slope <- (ab[2L] - ab[1L]) * stats::plogis(z) * stats::plogis(-z)
    J[, p + 1L] <- as.vector(dp[, moments, drop = FALSE]) * slope
  }
  if (parameters$scale) J[, ncol(J)] <- as.vector(dl[, moments, drop = FALSE])
  J
}

moment_delta <- function(value, J, V, parameter, level) {
  se <- sqrt(pmax(0, rowSums((J %*% V) * J)))
  z <- stats::qnorm((1 + level) / 2)
  lower <- value - z * se
  upper <- value + z * se
  positive <- parameter %in% c("variance", "sd", "cv", "kurtosis") &
    is.finite(value) & value > 0
  lower[positive] <- exp(log(value[positive]) - z * se[positive] / value[positive])
  upper[positive] <- exp(log(value[positive]) + z * se[positive] / value[positive])
  list(se = se, lower = lower, upper = upper)
}

moment_simulation <- function(evaluate, draws, level) {
  n <- length(evaluate(draws[1L, ]))
  values <- vapply(seq_len(nrow(draws)), function(i) evaluate(draws[i, ]),
    numeric(n))
  values <- matrix(values, nrow = n)
  defined <- rowMeans(is.finite(values))
  valid <- which(defined == 1)
  se <- lower <- upper <- rep(NA_real_, nrow(values))
  probs <- c((1 - level) / 2, (1 + level) / 2)
  if (length(valid)) {
    se[valid] <- apply(values[valid, , drop = FALSE], 1L, stats::sd)
    bounds <- vapply(valid, function(i) stats::quantile(values[i, ],
      probs = probs, names = FALSE), numeric(2L))
    lower[valid] <- bounds[1L, ]
    upper[valid] <- bounds[2L, ]
  }
  list(se = se, lower = lower, upper = upper, defined = defined)
}
