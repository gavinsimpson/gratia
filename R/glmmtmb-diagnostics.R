# Conditional generators hold all fitted random effects fixed. glmmTMB's
# simulate() draws new effects, including spline coefficients; the shared
# distribution selects a generator for the requested conditioning.
glmmtmb_family <- function(object, conditioning = NULL) {
  fam <- stats::family(object)
  distribution <- response_distribution(object, conditioning = conditioning)
  fam$rd <- if (is.function(distribution$rd))
    function(mu, wt, scale) distribution$rd() else NULL
  fam$cdf <- if (is.function(distribution$cdf))
    function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE)
      distribution$cdf(q, log_p = log_p, lower_tail = lower_tail) else NULL
  fam$qf <- if (is.function(distribution$qf))
    function(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE)
      distribution$qf(p, log_p = log_p, lower_tail = lower_tail) else NULL
  fam
}

# Use the same response-distribution constructor as other fitters, supplying
# glmmTMB's parameter definitions and observed binomial trial counts.
#' @export
response_distribution.glmmTMB <- function(model, conditioning = NULL, ...) {
  conditioning <- conditioning %||% "conditional"
  conditioning <- match.arg(conditioning, c("conditional", "marginal"))
  used <- glmmtmb_used_fit(model)
  f <- stats::family(used)
  y <- stats::model.response(used$frame)
  trials <- if (is.matrix(y)) rowSums(y) else if (is.factor(y)) rep(1, length(y)) else {
    w <- stats::model.weights(used$frame)
    if (is.null(w)) rep(1, length(y)) else w
  }
  dependence <- glmmtmb_dependence(used, conditioning)
  if (conditioning == "marginal") {
    d <- new_response_distribution(mean = NULL, rd = function() {
      draw <- stats::simulate(used, nsim = 1)[[1L]]
      if (is.matrix(draw)) draw[, 1L] / pmax(rowSums(draw), 1) else
        if (f$family == "binomial") draw / pmax(trials, 1) else
          if (is.factor(draw)) as.numeric(draw) else draw
    }, family = f)
    return(glmmtmb_distribution_metadata(d, dependence, conditioning, used))
  }
  supported <- glmmtmb_distribution_families()
  if (!f$family %in% supported)
    return(glmmtmb_distribution_metadata(new_response_distribution(
      unname(stats::predict(used, type = "response")), family = f), dependence,
      conditioning, used))
  mu <- f$linkinv(stats::predict(used, type = "link"))
  phi <- stats::predict(used, type = "disp")
  power <- if (f$family == "tweedie") unname(glmmTMB::family_params(used))[1L] else NULL
  d <- glmmtmb_response_distribution(f, mu, phi, trials, power,
    zprob = stats::predict(used, type = "zprob"))
  glmmtmb_distribution_metadata(d, dependence, conditioning, used)
}

# Random effects in different predictors are independent. Within a predictor,
# multiply its design Z by a covariance factor to check whether observations
# share variation. Independent effects for individual observations need not
# make marginal responses dependent.
glmmtmb_dependence <- function(model, conditioning = "conditional") {
  if (conditioning == "conditional") return(list(independent = TRUE,
    reason = "Responses are independent given fitted conditional, zero-inflation and dispersion effects."))
  vc <- glmmTMB::VarCorr(model)
  native <- model$obj$env$data
  reasons <- character()
  latent <- FALSE
  for (component in c("cond", "zi", "disp")) {
    Z <- native[[switch(component, cond = "Z", zi = "Zzi", disp = "Zdisp")]]
    if (is.null(Z) || !ncol(Z)) next
    latent <- TRUE
    terms <- model$modelInfo$reStruc[[paste0(component, "ReStruc")]]
    covariances <- vc[[component]]
    if (length(terms) != length(covariances))
      stop("Native glmmTMB covariance blocks are not aligned with random terms.")
    factors <- list()
    for (i in seq_along(terms)) {
      V <- as.matrix(covariances[[i]])
      factor <- tryCatch(t(chol(V)), error = function(e) {
        es <- eigen(V, symmetric = TRUE)
        sweep(es$vectors, 2L, sqrt(pmax(0, es$values)), `*`)
      })
      block <- Matrix::Matrix(factor, sparse = TRUE)
      factors[[i]] <- if (terms[[i]]$blockReps == 1L) block else
        kronecker(Matrix::Diagonal(terms[[i]]$blockReps), block)
    }
    factor <- Matrix::bdiag(factors)
    if (ncol(Z) != nrow(factor))
      stop("Native glmmTMB covariance factors are not aligned with the random design.")
    if (row_dependence_from_design(Z %*% factor))
      reasons <- c(reasons, paste0(component, " predictor shared or correlated latent effects"))
  }
  list(independent = !length(reasons), reason = if (length(reasons))
    paste(reasons, collapse = "; ") else if (latent)
      "Marginal latent effects are independent across observations." else
      "There are no latent effects shared between fitting observations.")
}

glmmtmb_distribution_metadata <- function(distribution, dependence, conditioning,
                                           model = NULL) {
  if (!is.null(model) && identical(stats::family(model)$family, "t")) {
    df <- unname(glmmTMB::family_params(model))[1L]
    distribution$mean_available <- df > 1
    distribution$variance_available <- df > 2
    distribution$moment_reason <- paste0("Student-t degrees of freedom = ",
      signif(df, 5), "; a finite mean requires df > 1 and a finite variance df > 2.")
    if (!distribution$mean_available) distribution["mean"] <- list(NULL)
    if (!distribution$variance_available) distribution["variance"] <- list(NULL)
  }
  distribution$independent <- dependence$independent
  distribution$dependence_reason <- dependence$reason
  distribution$conditioning <- conditioning
  distribution$joint_available <- is.function(distribution$rd)
  distribution$marginal_rd <- distribution$rd
  distribution
}

glmmtmb_distribution_families <- function() {
  c("gaussian", "poisson", "binomial", "nbinom1", "nbinom2", "beta", "Gamma",
    "tweedie", "lognormal", "truncated_poisson", "truncated_nbinom1", "truncated_nbinom2")
}

# phi is returned by glmmTMB's predict(type = "disp"): response standard
# deviation for Gaussian/lognormal, 1/sqrt(shape) for Gamma, and the package's
# dispersion parameter for other families. Fitted and posterior calculations
# use this same translation to response parameters.
glmmtmb_response_distribution <- function(family, mu, phi, trials = 1,
                                           power = NULL, zprob = 0) {
  name <- if (is.character(family)) family else family$family
  if (!name %in% glmmtmb_distribution_families())
    return(new_response_distribution((1 - zprob) * mu, family = family))
  dispersion <- switch(name, Gamma = 1 / phi^2,
    lognormal = sqrt(log1p((phi / mu)^2)), phi)
  d <- distribution_from_parameters(name, mu, dispersion, trials, power)
  if (any(zprob > 0)) d <- zero_inflated_distribution(d, zprob)
  d$family <- family
  d
}

#' @export
response_distribution.gratia_glmmtmb_diagnostic <- function(model, conditioning = NULL, ...) {
  mode <- conditioning %||% model$conditioning
  if (identical(mode, model$conditioning) && !is.null(model$distribution))
    return(model$distribution)
  response_distribution(model$source, conditioning = mode)
}

#' @export
diagnostic_reference_residuals.gratia_glmmtmb_diagnostic <- function(model, y, type) {
  if (model$conditioning == "marginal")
    return(glmmtmb_marginal_residuals(response_distribution(model), y, type))
  glmmtmb_residuals(model$source, type, y, conditioning = model$conditioning)
}

# Family-compatible generator retained for existing callers.
glmmtmb_rd <- function(name, mu, phi, zprob, trials, power = NULL) {
  d <- distribution_from_parameters(name, mu, phi, trials, power)
  if (any(zprob > 0)) d <- zero_inflated_distribution(d, zprob)
  function(mu, wt, scale) d$rd()
}

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

# Evaluate native residuals at simulated responses without refitting or changing
# the original object's TMB environment.
glmmtmb_residuals <- function(object, type, y = NULL, conditioning = NULL) {
  type <- match.arg(type, c("pearson", "response", "deviance"))
  object <- glmmtmb_used_fit(object)
  conditioning <- match.arg(conditioning %||% "conditional", c("conditional", "marginal"))
  if (conditioning == "marginal") {
    if (is.null(y)) {
      y <- stats::model.response(object$frame)
      if (is.matrix(y)) y <- y[, 1L] / pmax(rowSums(y), 1)
      if (is.factor(y)) y <- if (stats::family(object)$family %in%
          c("binomial", "betabinomial", "combinomial"))
        as.numeric(y != levels(y)[1L]) else as.numeric(y)
    }
    d <- prepare_marginal_response_distribution(response_distribution(object, conditioning))
    return(glmmtmb_marginal_residuals(d, y, type))
  }
  check_distribution_residual_moments(response_distribution(object), type)
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
  observed <- stats::model.response(object$frame)
  if (identical(stats::family(object)$family, "binomial")) {
    trials <- if (is.matrix(observed)) rowSums(observed) else
      if (is.factor(observed)) rep(1, length(observed)) else
        stats::model.weights(object$frame) %||% rep(1, length(observed))
    r[which(trials == 0)] <- 0
  }
  if (any(!is.finite(r))) {
    stop("glmmTMB ", type, " residuals are not finite. Try type = 'response'.")
  }
  unname(r)
}

# Calculate residuals using moments of the response simulations. With nonlinear
# links, setting random effects to zero and applying the inverse link does not
# give the marginal response mean.
glmmtmb_marginal_residuals <- function(distribution, y, type) {
  check_distribution_residual_moments(distribution, type)
  if (type == "response") return(unname(y - distribution$mean))
  if (type == "pearson") {
    r <- (y - distribution$mean) / sqrt(distribution$variance)
    r[which(distribution$variance == 0 & y == distribution$mean)] <- 0
    return(unname(r))
  }
  if (type %in% c("pit", "quantile"))
    return(distribution_quantile_residuals(distribution, y, type))
  stop("Marginal glmmTMB deviance residuals are undefined for the integrated ",
    "response distribution; use type = 'response', 'pearson' or 'quantile'.",
    call. = FALSE)
}

glmmtmb_diagnostic_model <- function(object, type = NULL, simulation = FALSE,
                                      conditioning = NULL) {
  used <- glmmtmb_used_fit(object)
  conditioning <- match.arg(conditioning %||% "conditional", c("conditional", "marginal"))
  type <- type %||% if (conditioning == "marginal") "response" else "pearson"
  distribution <- if (conditioning == "marginal")
    prepare_marginal_response_distribution(response_distribution(used, conditioning)) else NULL
  re <- if (conditioning == "marginal") NA else NULL
  fam <- if (simulation && conditioning == "conditional") glmmtmb_family(used, conditioning) else
    stats::family(used)
  y <- stats::model.response(used$frame)
  if (is.matrix(y)) y <- y[, 1L] / pmax(rowSums(y), 1)
  if (is.factor(y)) y <- if (fam$family %in% c("binomial", "betabinomial", "combinomial"))
    as.numeric(y != levels(y)[1L]) else as.numeric(y)
  structure(list(model = used$frame, y = y, family = fam,
    fitted.values = if (conditioning == "marginal")
      distribution$mean %||% rep(NA_real_, length(y)) else
      unname(stats::predict(used, type = "response", re.form = re)),
    linear.predictors = unname(stats::predict(used, type = "link", re.form = re)),
    residuals = if (type %in% c("pit", "quantile")) NULL else
      if (conditioning == "marginal") glmmtmb_marginal_residuals(distribution, y, type) else
        glmmtmb_residuals(used, type), residual_type = type,
    source = used, distribution = distribution, conditioning = conditioning, na.action = attr(object$frame, "na.action")),
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
  if (type %in% c("pit", "quantile"))
    return(distribution_quantile_residuals(response_distribution(object), object$y, type))
  if (identical(type, object$residual_type)) object$residuals else
    if (object$conditioning == "marginal")
      glmmtmb_marginal_residuals(response_distribution(object), object$y, type) else
      glmmtmb_residuals(object$source, type)
}

#' @export
family_name.glmmTMB <- function(object, ...) stats::family(object)$family

# Called by the shared QQ rendering methods, before any GAM-specific access.
qq_simulate_glmmtmb <- function(model, n, type, level, detrend) {
  qq_simulate(model, n = n, type = type, level = level, detrend = detrend)
}

glmmtmb_qq_method <- function(method, type = "pearson") {
  if (type %in% c("pit", "quantile")) return(method)
  method %||% "simulate"
}

#' @rdname glmmTMB-methods
#' @param conditioning character or `NULL`; `"conditional"` (default) or
#'   `"marginal"`; see [diagnostic_support].
#' @param method QQ method; `NULL` selects `"simulate"` for traditional residuals.
#' @param type Residual type: `"pearson"`, `"response"`, `"deviance"`, `"pit"`,
#'   or `"quantile"`, when available for the family. The default is Pearson for
#'   conditional checks and response residuals for marginal checks.
#' @export
qq_plot.glmmTMB <- function(model, ..., method = NULL, type = NULL,
                            conditioning = NULL) {
  proxy <- glmmtmb_diagnostic_model(model, type, simulation = TRUE,
    conditioning = conditioning)
  type <- proxy$residual_type
  qq_plot.gam(proxy, ..., method = glmmtmb_qq_method(method, type), type = type)
}
#' @export
qq_plot.gratia_glmmtmb_diagnostic <- function(model, ..., method = NULL, type = NULL,
                                              conditioning = NULL) {
  if (!is.null(conditioning)) model <- glmmtmb_diagnostic_model(model$source, type,
    simulation = TRUE, conditioning = conditioning)
  type <- type %||% model$residual_type
  qq_plot.gam(model, ..., method = glmmtmb_qq_method(method, type), type = type)
}
#' @rdname glmmTMB-methods
#' @export
worm_plot.glmmTMB <- function(model, ..., method = NULL, type = NULL,
                              conditioning = NULL) {
  proxy <- glmmtmb_diagnostic_model(model, type, simulation = TRUE,
    conditioning = conditioning)
  type <- proxy$residual_type
  worm_plot.gam(proxy, ..., method = glmmtmb_qq_method(method, type), type = type)
}
#' @export
worm_plot.gratia_glmmtmb_diagnostic <- function(model, ..., method = NULL, type = NULL,
                                                conditioning = NULL) {
  if (!is.null(conditioning)) model <- glmmtmb_diagnostic_model(model$source, type,
    simulation = TRUE, conditioning = conditioning)
  type <- type %||% model$residual_type
  worm_plot.gam(model, ..., method = glmmtmb_qq_method(method, type), type = type)
}
#' @rdname glmmTMB-methods
#' @export
appraise.glmmTMB <- function(model, ..., method = NULL, type = NULL,
                             conditioning = NULL) {
  proxy <- glmmtmb_diagnostic_model(model, type, simulation = TRUE,
    conditioning = conditioning)
  type <- proxy$residual_type
  appraise.gam(proxy, ..., method = glmmtmb_qq_method(method, type), type = type)
}
