# Use parameters from GJRM's final likelihood fit. Its intermediate GAM fits
# can have different parameter values and should not define the diagnostics.
check_gjrm_diagnostics <- function(model) {
  if (!inherits(model, "SemiParBIV") || !isTRUE(model$univar.gamlss))
    stop("Only univariate GJRM::gamlss() models are supported.", call. = FALSE)
  if (!model$margins[1L] %in% c("N", "LN", "GA", "GAi", "P", "NBI", "NBII",
      "BE", "LO", "WEI", "IG", "GU", "rGU") ||
      isTRUE(model$surv.flex) || isTRUE(model$VC$surv) ||
      (!is.null(model$cens) && any(model$cens != 1)))
    stop("GJRM diagnostics require an uncensored N, LN, GA, GAi, P, NBI, NBII, ",
      "BE, LO, WEI, IG, GU or rGU model.",
      call. = FALSE)
  invisible(model)
}

# Translate GJRM's fitted parameters to the shared response distributions.
# NBII has Var(Y) = mu*(1+sigma), so it uses the NB1 constructor here. Some GJRM
# versions store Poisson probabilities in fit$p for this family; calculate the
# CDF from the likelihood parameters so it matches the fitted model.
gjrm_response_distribution <- function(model, eta = NULL, sigma = NULL, nu = NULL) {
  check_gjrm_diagnostics(model)
  margin <- model$margins[1L]
  eta <- as.vector(eta %||% model$eta1)
  sigma <- as.vector(sigma %||% model$sigma2 %||% 1)
  distribution <- switch(margin,
    BE = distribution_from_parameters("beta", stats::plogis(eta),
      dispersion = 1 / sigma^2 - 1),
    LO = logistic_response_distribution(eta, sigma),
    WEI = weibull_response_distribution(scale = exp(eta), shape = sigma),
    IG = inverse_gaussian_response_distribution(exp(eta), sigma^2),
    GU = gumbel_response_distribution(eta, sigma, reverse = TRUE),
    rGU = gumbel_response_distribution(eta, sigma))
  if (!is.null(distribution)) return(distribution)
  family <- switch(margin, N = "gaussian", LN = "lognormal", GA =, GAi = "Gamma",
    P = "poisson", NBI = "nbinom2", NBII = "nbinom1")
  mu <- switch(margin, N =, GAi = eta, LN = exp(eta + sigma^2 / 2), exp(eta))
  dispersion <- switch(margin, GA =, GAi = 1 / sigma^2, NBI = 1 / sigma, sigma)
  distribution_from_parameters(family, mu, dispersion = dispersion)
}

#' @export
response_distribution.gamlss <- function(model, conditioning = NULL, ...) {
  check_distribution_conditioning(conditioning)
  gjrm_response_distribution(model)
}

gjrm_diagnostic_model <- function(model) {
  check_gjrm_diagnostics(model)
  distribution <- gjrm_response_distribution(model)
  y <- as.vector(model$y1)
  if (length(distribution$mean) != length(y) || length(model$eta1) != length(y))
    stop("GJRM fitted distribution is not aligned with its response.", call. = FALSE)
  fam <- switch(model$margins[1L], N = stats::gaussian(), LN = stats::gaussian(),
    GA =, GAi = stats::Gamma(link = if (model$margins[1L] == "GAi") "identity" else "log"),
    P = stats::poisson(), list(family = distribution$family))
  # A lognormal response does not have Gaussian response deviance.
  if (model$margins[1L] == "LN") fam$dev.resids <- NULL
  frame <- model$gam1$model
  if (is.null(frame) || NROW(frame) != length(y)) frame <- data.frame(.response = y)
  proxy <- new_diagnostic_proxy(y, distribution$mean, as.vector(model$eta1), fam,
    distribution, model = frame, original = model,
    na.action = model$gam1$na.action)
  proxy$diagnostic_default_type <- "quantile"
  proxy$eta_label <- gjrm_diagnostic_predictor_label(model)
  proxy
}

# The first predictor controls Weibull scale and Gumbel/logistic location.
gjrm_diagnostic_predictor_label <- function(model) {
  parameter <- switch(model$margins[1L], WEI = "scale",
    GU =, rGU =, LO = "location", "mu")
  paste0("Linear predictor (", parameter, ")")
}

#' @export
#' @rdname quantile_residuals
quantile_residuals.gamlss <- function(model, type = c("pit", "quantile"),
                                     seed = NULL, ...) {
  quantile_residuals(gjrm_diagnostic_model(model), type = type, seed = seed, ...)
}

#' @export
#' @rdname simulate
simulate.gamlss <- function(object, nsim = 1L, seed = NULL, ...) {
  nsim <- validate_diagnostic_bins(nsim, "nsim")
  if (length(list(...))) stop("GJRM simulations currently use the fitting observations.", call. = FALSE)
  proxy <- gjrm_diagnostic_model(object)
  dist <- proxy$distribution
  out <- diagnostic_seed(seed, {
    values <- replicate(nsim, dist$rd(), simplify = FALSE)
    as.data.frame(lapply(values, restore_model_rows, model = proxy))
  })
  names(out) <- paste0("sim_", seq_len(nsim))
  class(out) <- c("simulate_gratia", class(out))
  out
}
