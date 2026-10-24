#' Posterior draws of pointwise log likelihoods
#'
#' Evaluate each fitting observation at draws from the coefficient posterior.
#' This leaves the scalar [stats::logLik()] method supplied by mgcv unchanged.
#'
#' @param model A fitted independent-response `gam`, `bam`, or `scam` model.
#' @param n_draws Positive integer; total coefficient draws across chains.
#'   Defaults to 4000 when generating draws. Supplied draws determine their own
#'   count; an explicit count must agree.
#' @inheritParams post_draws
#' @param seed Optional random seed. The caller's random-number state is retained.
#' @param method One of `"gaussian"`, `"mh"` or `"user"`; see [post_draws()].
#' @param draws A numeric matrix with one row per draw and one column per
#'   coefficient, in fitted coefficient order. Used with `method = "user"`.
#' @param unconditional Include the smoothing-parameter covariance correction
#'   when available, using [mgcv::vcov.gam()]. Defaults to `FALSE`.
#' @param ... Additional arguments to [post_draws()], such as `burnin`, `thin`
#'   or `mvn_method`. Prediction at new observations and frequentist covariance
#'   matrices are not supported.
#'
#' @details
#' The result contains log densities or log probability masses, including their
#' normalising constants and fitting weights. Smoothing penalties and priors
#' are excluded. Likelihood weights need not have the same interpretation as
#' the weights used by [posterior_samples()] to generate responses.
#'
#' Ordinary and extended response families, distributional GAMs, multinomial,
#' multivariate-normal and grouped families are supported. A multivariate-normal
#' observation contributes one joint log density; its precision coefficients
#' vary with each draw. Grouped observations use their fitted component family.
#' Custom families can supply the `log_lik` hook in [family_diagnostics]. Quasi,
#' censored and Cox families, residual-correlated BAMs and full mixed models
#' are not supported. Only retained fitting observations are evaluated.
#'
#' Gaussian draws approximate the coefficient posterior. Parameters represented
#' by coefficients vary jointly, including distributional-model scale and shape
#' predictors. Other scale, shape and smoothing parameters remain at fitted
#' values. Even with `unconditional = TRUE`, those additional parameters are
#' not jointly sampled. Ordinary PSIS-LOO uses these draws without correcting
#' their Gaussian approximation. Pareto diagnostics assess reweighting, not the
#' accuracy of the Gaussian approximation. See [loo.gam] for optional loo methods.
#'
#' Independent-response SCAM fits are supported using the coefficient
#' approximation selected by [post_draws()]. The prediction matrix multiplies
#' transformed coefficients. Smoothing parameters and response scale remain at
#' their fitted values; smoothing-parameter uncertainty is unavailable.
#'
#' @return A numeric matrix with draws in rows and retained fitting observations
#'   in columns. Attribute `observation` gives their original fitting-row indices
#'   after any fitting subset; `gratia` describes the sampling assumptions.
#'   The `sampling` attribute retains chain identities and requested coefficient
#'   diagnostics; [as_mcmc_draws()] converts the likelihood matrix by chain.
#'   Impossible observations may have log likelihood `-Inf`; invalid numerical
#'   calculations produce an error. LOO and WAIC require finite log likelihoods.
#' @seealso [posterior_samples()], [family_diagnostics]
#' @export
#' @examples
#' load_mgcv()
#' m <- gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
#' ll <- log_lik(m, n_draws = 500, seed = 42)
#' dim(ll)
#' if (requireNamespace("loo", quietly = TRUE)) {
#'   loo::loo(ll, r_eff = 1)
#' }
log_lik <- function(model, ...) {
  sampling_check_names(substitute(list(...)))
  UseMethod("log_lik")
}

#' @export
log_lik.default <- function(model, ...) {
  stop("Pointwise posterior log likelihoods are unavailable for <",
    class(model)[1L], "> models.", call. = FALSE)
}

#' @rdname log_lik
#' @export
log_lik.gam <- function(model, ..., n_draws = NULL, chains = NULL,
                        diagnostics = FALSE, seed = NULL,
                        method = c("gaussian", "mh", "user"), draws = NULL,
                        unconditional = FALSE) {
  sampling_check_names(substitute(list(...)))
  args <- c(list(method = match.arg(method)), list(...))
  if (!missing(draws)) args$draws <- draws
  if (!missing(unconditional)) args$unconditional <- unconditional
  if (!missing(chains)) args$chains <- chains
  if (!missing(diagnostics)) args$diagnostics <- diagnostics
  prep <- posterior_likelihood_prepare(model, n = n_draws, seed = seed,
    sampling_args = args)
  posterior_likelihood_matrix(prep)
}

#' @rdname log_lik
#' @export
log_lik.bam <- log_lik.gam

posterior_likelihood_prepare <- function(model, n = NULL, seed = NULL,
                                         sampling_args = list()) {
  if (!inherits(model, "gam") || inherits(model, c("gamm", "gamm4")) ||
      isTRUE(attr(model, "gratia.gamm4"))) {
    stop("Pointwise likelihoods require an independent gam, bam or scam fit.", call. = FALSE)
  }
  if (!is.null(model$AR1.rho) && model$AR1.rho != 0) stop(
    "Pointwise likelihoods do not support residual-correlated fits.", call. = FALSE)
  sampling_args <- posterior_adapter_sampling_args(sampling_args, likelihood = TRUE)
  method <- sampling_args$method
  layout <- sampling_resolve(n %||% if (method != "user") 4000L,
    sampling_args$chains, sampling_args$draws, method)
  n <- layout$n_draws
  rows <- which(!is.na(model_row_map(model)))
  used <- model_used_rows(model)
  f <- stats::family(used)
  posterior_likelihood_check_family(f)
  trials <- NULL
  if (family_type(f) == "binomial") {
    response <- stats::model.response(stats::model.frame(used))
    trials <- if (is.matrix(response)) rowSums(response) else
      if (is.factor(response) || is.logical(response) ||
          all(response %in% c(0, 1))) rep(1, NROW(response)) else used$prior.weights
  }
  X <- if (is_gfam_family(f)) gfam_predict_eta(used, used$model, "lpmatrix") else
    predict_model(used, newdata = used$model, type = "lpmatrix")
  idx <- attr(X, "lpi") %||% list(seq_len(ncol(X)))
  eta <- matrix(used$linear.predictors, nrow = nrow(X))
  base <- vapply(idx, function(j) drop(X[, j, drop = FALSE] %*%
    stats::coef(used)[j]), numeric(nrow(X)))
  offsets <- eta - base
  betas <- do.call(post_draws, c(list(model = used, n_draws = n, seed = seed), sampling_args))
  if (!is.numeric(betas) || !is.matrix(betas) || !nrow(betas) ||
      ncol(betas) != length(stats::coef(used)) || any(!is.finite(betas))) stop(
    "Coefficient draws must be a finite numeric matrix with one column per fitted coefficient.",
    call. = FALSE)
  ids <- attr(betas, "sampling")$draw_ids %||% sampling_layout(nrow(betas), 1L)
  metadata <- list(method = method, n_draws = nrow(betas),
    chains = length(unique(ids$.chain)),
    approximation = if (method == "gaussian" && inherits(used, "scam"))
      "Gaussian approximation in unrestricted fitting coordinates, followed by the fitted positivity transformation" else
      if (method == "gaussian") "Gaussian coefficient posterior approximation" else
      if (method == "mh") "Metropolis-Hastings coefficient posterior" else "User-supplied coefficient draws",
    fixed_parameters = "Smoothing parameters and family parameters outside the coefficient vector remain at fitted values.",
    unconditional = isTRUE(sampling_args$unconditional),
    posterior_correction = FALSE)
  context <- if (is_gfam_family(f)) gfam_diagnostic_context(used) else NULL
  list(model = used, betas = betas, rows = rows, method = method,
    metadata = metadata, X = X, index = idx, offsets = offsets, context = context,
    trials = trials, draw_ids = ids)
}

posterior_likelihood_check_family <- function(family) {
  if (is_gfam_family(family)) {
    invisible(lapply(gfam_components(family), function(x)
      posterior_likelihood_check_family(x$family)))
    return(invisible(NULL))
  }
  ft <- family_type(family)
  if (startsWith(ft, "ordered_categorical")) ft <- "ordered_categorical"
  if (startsWith(ft, "quasi") || ft %in% c("cnorm", "clog", "cpois", "cox.ph", "cox_ph", "bcg"))
    stop("Pointwise posterior log likelihoods are unavailable for family <",
      family_name(family), ">.", call. = FALSE)
  known <- c("gaussian", "gamma", "inverse_gaussian", "poisson", "binomial",
    "negative_binomial", "tweedie", "beta_regression", "scaled_t", "ordered_categorical",
    "zero_inflated_poisson", "gaulss", "gammals", "gumbls", "gevlss", "shash",
    "twlss", "ziplss", "multinom", "multivariate_normal")
  if (!ft %in% known && !is.function(family$gratia_diagnostics)) stop(
    "No pointwise likelihood implementation for family <", family_name(family),
    ">; supply a gratia_diagnostics log_lik hook.", call. = FALSE)
  invisible(NULL)
}

posterior_likelihood_matrix <- function(prep) {
  out <- posterior_likelihood_evaluate(prep, seq_along(prep$rows))
  colnames(out) <- rownames(prep$model$model)
  attr(out, "observation") <- prep$rows
  attr(out, "gratia") <- prep$metadata
  out <- attach_sampling(out, prep$betas)
  metadata <- attr(out, "sampling")
  if (!is.null(metadata)) {
    metadata$quantity <- "log_lik"
    attr(out, "sampling") <- metadata
  }
  out
}

# Shared validation for adapters that accept a sampling_args list.
posterior_adapter_sampling_args <- function(args, likelihood = FALSE) {
  if (!is.list(args) || (length(args) &&
      (is.null(names(args)) || anyNA(names(args)) || any(!nzchar(names(args))) ||
       anyDuplicated(names(args))))) stop(
    "'sampling_args' must be a named list with distinct argument names.", call. = FALSE)
  sampling_check_names(as.call(c(list(quote(list)), args)))
  allowed <- unique(c("method", "chains", "diagnostics", "draws",
    sampling_method_controls("gaussian"), sampling_method_controls("mh")))
  if (likelihood) allowed <- setdiff(allowed, c("frequentist", "parametrized"))
  bad <- setdiff(names(args), allowed)
  if (length(bad)) stop("Unsupported posterior sampling argument(s): ",
    paste(bad, collapse = ", "), ". Supply n_draws at the top level.", call. = FALSE)
  method <- match.arg(args$method %||% "gaussian", c("gaussian", "mh", "user"))
  if (!is.null(args$draws) && method != "user") stop(
    "Supply coefficient draws with method = 'user'.", call. = FALSE)
  sampling_check_options(method, names(args),
    mh_backend = args$mh_backend %||% "mgcv", mvn_method = args$mvn_method %||% "mvnfast")
  args$method <- method
  args
}

# Keep this boundary deterministic: both loo's observation callback and the
# matrix extractor evaluate the same draws without sampling additional values.
posterior_likelihood_evaluate <- function(prep, rows) {
  model <- prep$model
  f <- stats::family(model)
  X <- prep$X[rows, , drop = FALSE]
  attr(X, "lpi") <- prep$index
  offsets <- prep$offsets[rows, , drop = FALSE]
  yy <- slice_observations(model$y, rows)
  if (!is.null(prep$trials)) attr(yy, "gratia_trials") <- prep$trials[rows]
  wt <- (model$prior.weights %||% rep(1, nrow(prep$X)))[rows]
  scale <- model$sig2 %||% 1
  out <- vapply(seq_len(nrow(prep$betas)), function(s) {
    beta <- prep$betas[s, ]
    eta <- vapply(prep$index, function(j) drop(X[, j, drop = FALSE] %*%
      beta[j]), numeric(length(rows))) + offsets
    extra <- if (is.null(prep$context))
      posterior_likelihood_hooks(f, yy, eta, wt, scale) else list()
    if (!is.null(prep$context)) {
      context <- prep$context
      value <- numeric(length(rows))
      for (i in seq_along(context$components)) {
        local_rows <- which(context$index[rows] == i)
        if (!length(local_rows)) next
        component <- context$components[[i]]
        value[local_rows] <- posterior_likelihood_family(component$family,
          slice_observations(yy, local_rows), eta[local_rows, , drop = FALSE],
          wt[local_rows], component$scale)
      }
      value
    } else if ("log_lik" %in% names(extra)) {
      posterior_likelihood_family(f, yy, eta, wt, scale)
    } else if (family_type(f) == "multivariate_normal") {
      R <- posterior_likelihood_mvn_R(beta, ncol(yy))
      residual <- (yy - eta) %*% t(R)
      -.5 * ncol(yy) * log(2 * pi) + sum(log(diag(R))) - .5 * rowSums(residual^2)
    } else if (inherits(f, "general.family") &&
        !family_type(f) %in% c("multinom", "ziplss", "shash", "gevlss", "twlss")) {
      offset <- if (all(offsets == 0)) NULL else
        lapply(seq_len(ncol(offsets)), function(j) offsets[, j])
      result <- f$ll(yy, X, beta, wt, f, offset = offset, deriv = 0)
      if (is.null(result$l0)) stop("Family likelihood does not supply pointwise contributions.", call. = FALSE)
      result$l0
    } else {
      posterior_likelihood_family(f, yy, eta, wt, scale)
    }
  }, numeric(length(rows)))
  out <- t(matrix(out, nrow = length(rows), ncol = nrow(prep$betas)))
  if (anyNA(out) || any(out == Inf)) {
    bad <- which(is.na(out) | out == Inf, arr.ind = TRUE)[1L, ]
    stop("Invalid log likelihood at observation ", prep$rows[rows[bad[2L]]],
      ", draw ", bad[1L], ".", call. = FALSE)
  }
  out
}

posterior_likelihood_mvn_R <- function(beta, d) {
  pars <- utils::tail(beta, d * (d + 1L) / 2L)
  R <- matrix(0, d, d)
  k <- 1L
  for (i in seq_len(d)) for (j in seq.int(i, d)) {
    R[i, j] <- if (i == j) exp(pars[k]) else pars[k]
    k <- k + 1L
  }
  R
}

posterior_likelihood_family <- function(f, y, eta, wt, scale) {
  ft <- family_type(f)
  mu <- family_prediction_parameters(f, eta)
  extra <- posterior_likelihood_hooks(f, y, eta, wt, scale)
  if (length(extra)) {
    if ("log_lik" %in% names(extra)) {
      fun <- extra$log_lik
      if (!is.function(fun)) stop("Family gratia_diagnostics log_lik must be a function.", call. = FALSE)
      value <- fun(y)
      if (!is.numeric(value) || !is.null(dim(value)) || length(value) != NROW(y) ||
          anyNA(value) || any(value == Inf)) stop(
        "Family log_lik hook must return one valid log likelihood per observation.", call. = FALSE)
      return(value)
    }
  }
  mu <- as.vector(mu)
  if (ft == "multinom") {
    z <- cbind(0, eta)
    max_eta <- apply(z, 1L, max)
    normalizer <- max_eta + log(rowSums(exp(z - max_eta)))
    return(z[cbind(seq_along(y), as.integer(y) + 1L)] - normalizer)
  }
  if (startsWith(ft, "ordered_categorical")) {
    cuts <- c(-Inf, f$getTheta(TRUE), Inf)
    lower <- cuts[y] - as.vector(eta)
    upper <- cuts[y + 1L] - as.vector(eta)
    # Choose the smaller tail before taking a difference, so a probability
    # close to one is never rounded to one before its logarithm is evaluated.
    upper_tail <- lower > 0
    large <- stats::plogis(upper, log.p = TRUE)
    small <- stats::plogis(lower, log.p = TRUE)
    large[upper_tail] <- stats::plogis(lower[upper_tail], lower.tail = FALSE, log.p = TRUE)
    small[upper_tail] <- stats::plogis(upper[upper_tail], lower.tail = FALSE, log.p = TRUE)
    value <- large + log1mexp(small - large)
    value[wt == 0] <- 0
    return(wt * value)
  }
  # Native ziP takes latent predictors and has saturated log likelihood zero.
  if (startsWith(ft, "zero_inflated_poisson"))
    return(as.numeric(-f$dev.resids(y, as.vector(eta), wt) / 2))
  if (ft == "ziplss") {
    lambda <- exp(eta[, 1L])
    presence <- exp(eta[, 2L])
    log_p <- log(-expm1(-presence))
    small_presence <- eta[, 2L] < -36
    log_p[small_presence] <- eta[small_presence, 2L]
    log_truncation <- log(-expm1(-lambda)) - eta[, 1L]
    log_truncation[eta[, 1L] < -36] <- 0
    value <- -presence
    positive <- y > 0
    value[positive] <- log_p[positive] + (y[positive] - 1) * eta[positive, 1L] -
      lambda[positive] - lgamma(y[positive] + 1) - log_truncation[positive]
    return(value)
  }
  if (ft == "shash") {
    parameters <- matrix(vapply(seq_len(4L), function(j)
      f$linfo[[j]]$linkinv(eta[, j]), numeric(length(y))), nrow = length(y))
    tau <- parameters[, 2L]
    delta <- exp(parameters[, 4L])
    z <- (y - parameters[, 1L]) * exp(-tau - parameters[, 4L])
    transformed <- delta * asinh(z) - parameters[, 3L]
    log_cosh <- abs(transformed) + log1p(exp(-2 * abs(transformed))) - log(2)
    # Native shash$ll includes an extra tail-shape penalty. A density used
    # for predictive scoring must exclude that regularisation term.
    return(-tau - .5 * log(2 * pi) + log_cosh -
      .5 * log1p(z^2) - .5 * sinh(transformed)^2)
  }
  if (ft == "gevlss") {
    location <- f$linfo[[1L]]$linkinv(eta[, 1L])
    rho <- f$linfo[[2L]]$linkinv(eta[, 2L])
    shape <- f$linfo[[3L]]$linkinv(eta[, 3L])
    z <- (y - location) * exp(-rho)
    value <- rep(-Inf, length(y))
    zero <- shape == 0
    value[zero] <- -rho[zero] - z[zero] - exp(-z[zero])
    supported <- !zero & 1 + shape * z > 0
    log_t <- log1p(shape[supported] * z[supported])
    value[supported] <- -rho[supported] - log_t - log_t / shape[supported] -
      exp(-log_t / shape[supported])
    return(value)
  }
  if (ft == "twlss") {
    ab <- get_tw_params(f)
    mean <- f$linfo[[1L]]$linkinv(eta[, 1L])
    power <- theta_2_power(f$linfo[[2L]]$linkinv(eta[, 2L]), ab[1L], ab[2L])
    dispersion <- exp(f$linfo[[3L]]$linkinv(eta[, 3L]))
    return(mgcv::ldTweedie(y, mu = mean, p = power, phi = dispersion)[, 1L])
  }
  value <- switch(ft,
    gaussian = {
      ans <- numeric(length(y))
      take <- wt > 0
      ans[take] <- stats::dnorm(y[take], mu[take], sqrt(scale / wt[take]), log = TRUE)
      return(ans)
    },
    poisson = stats::dpois(y, mu, log = TRUE),
    binomial = {
      # Fitting y is initialized as a proportion and prior.weights includes
      # trial counts. The original model response is handled by preparation.
      trials <- attr(y, "gratia_trials") %||% wt
      replication <- ifelse(trials > 0, wt / trials, 0)
      value <- replication * stats::dbinom(round(y * trials), round(trials), mu, log = TRUE)
      value[wt == 0] <- 0
      return(value)
    },
    gamma = {
      ans <- numeric(length(y))
      take <- wt > 0
      ans[take] <- stats::dgamma(y[take], shape = wt[take] / scale,
        scale = mu[take] * scale / wt[take], log = TRUE)
      return(ans)
    },
    inverse_gaussian = {
      ans <- numeric(length(y))
      take <- wt > 0
      dispersion <- scale / wt[take]
      ans[take] <- -.5 * (log(2 * pi * dispersion) + 3 * log(y[take])) -
        (y[take] - mu[take])^2 / (2 * dispersion * mu[take]^2 * y[take])
      return(ans)
    },
    negative_binomial = stats::dnbinom(y, mu = mu, size = theta(f), log = TRUE),
    beta_regression = {
      precision <- theta(f)
      stats::dbeta(y, mu * precision, (1 - mu) * precision, log = TRUE)
    },
    tweedie = {
      power <- if (is.function(f$getTheta)) f$getTheta(TRUE) else get_tw_params(f)[1L]
      mgcv::ldTweedie(y, mu = mu, p = power, phi = scale)[, 1L]
    },
    NULL)
  if (!is.null(value)) {
    value[wt == 0] <- 0
    return(wt * value)
  }
  if (inherits(f, "extended.family") && is.function(f$ls)) {
    theta <- f$getTheta()
    saturated <- vapply(seq_along(y), function(i)
      f$ls(y[i], wt[i], theta, scale)$ls, numeric(1))
    return(as.numeric(saturated - f$dev.resids(y, mu, wt) / (2 * scale)))
  }
  stop("No pointwise likelihood implementation for family <", family_name(f),
    ">; supply a gratia_diagnostics log_lik hook.", call. = FALSE)
}

posterior_likelihood_hooks <- function(f, y, eta, wt, scale) {
  if (!is.function(f$gratia_diagnostics)) return(list())
  extra <- f$gratia_diagnostics(mu = family_prediction_parameters(f, eta),
    wt = wt, scale = scale, y = y, eta = eta)
  if (!is.list(extra) || (length(extra) && (is.null(names(extra)) ||
      anyNA(names(extra)) || any(!nzchar(names(extra))) || anyDuplicated(names(extra)))))
    stop("Family gratia_diagnostics must return a named list with distinct fields.", call. = FALSE)
  if ("independent" %in% names(extra)) {
    if (!is.logical(extra$independent) || length(extra$independent) != 1L || is.na(extra$independent))
      stop("Family gratia_diagnostics independent must be TRUE or FALSE.", call. = FALSE)
    if (!extra$independent) stop("Pointwise likelihoods require independent response observations. ",
      extra$dependence_reason %||% "The family declares dependent responses.", call. = FALSE)
  }
  extra
}
