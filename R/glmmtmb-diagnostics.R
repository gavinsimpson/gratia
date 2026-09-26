# Response generators condition on all fitted latent effects. glmmTMB's
# simulate() instead draws new latent effects, including spline coefficients.
glmmtmb_family <- function(object) {
  fam <- stats::family(object)
  supported <- c("gaussian", "poisson", "binomial", "nbinom1", "nbinom2",
    "beta", "Gamma", "tweedie", "truncated_poisson", "truncated_nbinom1",
    "truncated_nbinom2")
  if (!fam$family %in% supported) {
    stop("QQ simulation is not available for glmmTMB family '", fam$family, "'.")
  }
  # Removing na.action on a local frame gives predictions in fitting order.
  object <- glmmtmb_used_fit(object)
  mu <- fam$linkinv(stats::predict(object, type = "link"))
  phi <- stats::predict(object, type = "disp")
  # predict(type = "disp") follows sigma.glmmTMB: Gamma returns 1/sqrt(shape),
  # whereas the response generator requires shape itself.
  if (fam$family == "Gamma") phi <- 1 / phi^2
  zprob <- stats::predict(object, type = "zprob")
  y <- stats::model.response(object$frame)
  trials <- if (is.matrix(y)) rowSums(y) else if (is.factor(y)) rep(1, length(y)) else {
    w <- stats::model.weights(object$frame)
    if (is.null(w)) rep(1, length(y)) else w
  }
  power <- if (fam$family == "tweedie") unname(glmmTMB::family_params(object))[1L] else NULL
  fam$rd <- glmmtmb_rd(fam$family, mu, phi, zprob, trials, power)
  fam
}

glmmtmb_rd <- function(name, mu, phi, zprob, trials, power = NULL) {
  force(name); force(mu); force(phi); force(zprob); force(trials); force(power)
  fitted_mu <- mu
  function(mu, wt, scale) {
    mu <- fitted_mu
    n <- length(mu)
    y <- switch(name,
      gaussian = stats::rnorm(n, mu, phi),
      poisson = stats::rpois(n, mu),
      binomial = stats::rbinom(n, trials, mu) / trials,
      nbinom1 = stats::rnbinom(n, mu = mu, size = mu / phi),
      nbinom2 = stats::rnbinom(n, mu = mu, size = phi),
      beta = stats::rbeta(n, mu * phi, (1 - mu) * phi),
      Gamma = stats::rgamma(n, shape = phi, scale = mu / phi),
      tweedie = rtw(mu, p = power, phi = phi),
      truncated_poisson = glmmtmb_truncated_rng(mu),
      truncated_nbinom1 = glmmtmb_truncated_rng(mu, size = mu / phi),
      truncated_nbinom2 = glmmtmb_truncated_rng(mu, size = phi),
      stop("Unsupported glmmTMB response generator."))
    if (any(zprob > 0)) y[stats::runif(n) < zprob] <- 0
    y
  }
}

# Invert the upper tail conditional on Y > 0. Log probabilities avoid losing
# all precision when P(Y > 0) is tiny; rejection sampling can stall there.
glmmtmb_truncated_rng <- function(mu, size = NULL) {
  lp <- if (is.null(size)) stats::ppois(0, mu, lower.tail = FALSE, log.p = TRUE) else
    stats::pnbinom(0, mu = mu, size = size, lower.tail = FALSE, log.p = TRUE)
  lp <- lp + log(stats::runif(length(mu)))
  if (is.null(size)) stats::qpois(lp, mu, lower.tail = FALSE, log.p = TRUE) else
    stats::qnbinom(lp, mu = mu, size = size, lower.tail = FALSE, log.p = TRUE)
}

glmmtmb_used_fit <- function(object) {
  attr(object$frame, "na.action") <- NULL
  object$na.action <- NULL
  object
}

# Use glmmTMB's own residual definitions for both observed and simulated data.
# Replacing only the local response leaves fitted parameters and predictions
# unchanged and never refits or changes the original object's TMB environment.
glmmtmb_residuals <- function(object, type, y = NULL) {
  type <- match.arg(type, c("pearson", "response", "deviance"))
  object <- glmmtmb_used_fit(object)
  if (!is.null(y)) {
    old <- stats::model.response(object$frame)
    if (is.matrix(old)) {
      size <- rowSums(old)
      y <- cbind(y * size, (1 - y) * size)
    }
    object$frame[[1L]] <- y
  }
  r <- tryCatch(stats::residuals(object, type = type), error = function(e) {
    stop("glmmTMB ", type, " residuals are unavailable: ", conditionMessage(e),
      ". Try type = 'response'.", call. = FALSE)
  })
  if (any(!is.finite(r))) {
    stop("glmmTMB ", type, " residuals are not finite. Try type = 'response'.")
  }
  unname(r)
}

glmmtmb_diagnostic_model <- function(object, type = "pearson", simulation = FALSE) {
  used <- glmmtmb_used_fit(object)
  fam <- if (simulation) glmmtmb_family(used) else stats::family(used)
  y <- stats::model.response(used$frame)
  if (is.matrix(y)) y <- y[, 1L] / rowSums(y)
  if (is.factor(y)) y <- as.numeric(y != levels(y)[1L])
  structure(list(model = used$frame, y = y, family = fam,
    fitted.values = unname(stats::predict(used, type = "response")),
    linear.predictors = unname(stats::predict(used, type = "link")),
    residuals = glmmtmb_residuals(used, type), residual_type = type,
    source = used, na.action = attr(object$frame, "na.action")),
    class = "gratia_glmmtmb_diagnostic")
}

#' @export
model.frame.gratia_glmmtmb_diagnostic <- function(formula, ...) formula$model
#' @export
family.gratia_glmmtmb_diagnostic <- function(object, ...) object$family
#' @export
family_name.gratia_glmmtmb_diagnostic <- function(object, ...) object$family$family
#' @export
fitted.gratia_glmmtmb_diagnostic <- function(object, ...) object$fitted.values
#' @export
residuals.gratia_glmmtmb_diagnostic <- function(object, type = "pearson", ...) {
  if (identical(type, object$residual_type)) object$residuals else
    glmmtmb_residuals(object$source, type)
}

#' @export
family_name.glmmTMB <- function(object, ...) stats::family(object)$family

# Called by the shared QQ rendering methods, before any GAM-specific access.
qq_simulate_glmmtmb <- function(model, n, type, level, detrend) {
  if (length(n) != 1L || !is.finite(n) || n < 1L || n != as.integer(n)) {
    stop("n_simulate must be a positive integer.")
  }
  sims <- replicate(n, sort(glmmtmb_residuals(model$source, type,
    y = model$family$rd(model$fitted.values, NULL, NULL))))
  sims <- matrix(sims, nrow = length(model$y), ncol = n)
  out <- stats::quantile(sims, probs = (seq_along(model$y) - 0.5) / length(model$y))
  int <- apply(sims, 1L, stats::quantile, probs = c((1 - level) / 2, (1 + level) / 2))
  r <- sort(stats::residuals(model, type = type))
  if (detrend) {
    r <- r - out
    int <- sweep(int, 2L, out)
  }
  tibble::tibble(theoretical = unname(out), residuals = unname(r),
    lower = unname(int[1L, ]), upper = unname(int[2L, ]))
}

glmmtmb_qq_method <- function(method) {
  if (!is.null(method) && !identical(method, "simulate")) {
    stop("glmmTMB QQ plots currently support method = 'simulate' only.")
  }
  "simulate"
}

#' @rdname glmmTMB-methods
#' @param method QQ method; `NULL` selects `"simulate"`, the supported method.
#' @param type Native glmmTMB residual type: `"pearson"` (default),
#'   `"response"`, or `"deviance"`, when available for the family.
#' @export
qq_plot.glmmTMB <- function(model, ..., method = NULL, type = "pearson") {
  method <- glmmtmb_qq_method(method)
  qq_plot.gam(glmmtmb_diagnostic_model(model, type, simulation = TRUE),
    ..., method = method, type = type)
}
#' @export
qq_plot.gratia_glmmtmb_diagnostic <- function(model, ..., method = NULL, type = "pearson") {
  qq_plot.gam(model, ..., method = glmmtmb_qq_method(method), type = type)
}
#' @rdname glmmTMB-methods
#' @export
worm_plot.glmmTMB <- function(model, ..., method = NULL, type = "pearson") {
  worm_plot.gam(glmmtmb_diagnostic_model(model, type, simulation = TRUE),
    ..., method = glmmtmb_qq_method(method), type = type)
}
#' @export
worm_plot.gratia_glmmtmb_diagnostic <- function(model, ..., method = NULL, type = "pearson") {
  worm_plot.gam(model, ..., method = glmmtmb_qq_method(method), type = type)
}
#' @rdname glmmTMB-methods
#' @export
appraise.glmmTMB <- function(model, ..., method = NULL, type = "pearson") {
  method <- glmmtmb_qq_method(method)
  appraise.gam(glmmtmb_diagnostic_model(model, type, simulation = TRUE),
    ..., method = method, type = type)
}
