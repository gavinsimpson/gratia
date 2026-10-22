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

# Joint fits are checked one response at a time. The distribution within each
# response retains fitted random effects; responses are never simulated as an
# independent joint draw.
check_gjrm_joint_diagnostics <- function(model, response = seq_along(model$margins)) {
  kind <- model$model %||% model$Model
  if (!inherits(model, "gjrm") || isTRUE(model$univar.gamlss) ||
      !kind %in% c("B", "T") ||
      (isTRUE(model$triv) && !identical(kind, "T")))
    stop("GJRM diagnostics support ordinary bivariate B and trivariate binary T models.", call. = FALSE)
  if (isTRUE(model$surv.flex) || isTRUE(model$surv) || isTRUE(model$VC$surv) ||
      !is.null(model$VC$K1) || !is.null(model$VC$K2) ||
      any(unlist(model$VC[c("left.trunc1", "left.trunc2")]) != 0))
    stop("GJRM response diagnostics require uncensored, untruncated, non-ordinal models.", call. = FALSE)
  if (isTRUE(model$fixed.margins) || isTRUE(model$VC$fixed.margins))
    stop("GJRM response diagnostics are unavailable for fixed-margin fits.", call. = FALSE)
  gjrm_check_diagnostic_predictors(model)
  margins <- model$margins[response]
  supported <- c("N", "LN", "GA", "GAi", "P", "NBI", "NBII", "BE", "LO",
    "WEI", "IG", "GU", "rGU", "probit", "logit", "cloglog")
  if (any(!margins %in% supported) ||
      (identical(kind, "T") && any(!margins %in% c("probit", "logit", "cloglog"))))
    stop("GJRM response diagnostics do not support these margins.", call. = FALSE)
  invisible(model)
}

# Holding an endogenous predictor fixed does not supply the observational
# response distribution. Copula-conditioned references are needed for these
# models; their smooths remain available through the plotting methods.
gjrm_check_diagnostic_predictors <- function(model) {
  variables <- function(expr) {
    if (is.call(expr)) {
      operator <- as.character(expr[[1L]])[1L]
      if (operator %in% c("$", "[[") && length(expr) == 3L &&
          (is.name(expr[[3L]]) || is.character(expr[[3L]])))
        return(as.character(expr[[3L]]))
      return(unique(unlist(lapply(as.list(expr)[-1L], variables), use.names = FALSE)))
    }
    if (is.name(expr)) as.character(expr) else character()
  }
  responses <- lapply(model$formula[seq_along(model$margins)], function(f)
    if (length(f) == 3L) variables(f[[2L]]) else character())
  used <- unique(unlist(lapply(seq_along(model$formula), function(i) {
    formula <- model$formula[[i]]
    rhs <- variables(formula[[length(formula)]])
    candidates <- if (i <= length(responses))
      unlist(responses[-i], use.names = FALSE) else unlist(responses, use.names = FALSE)
    intersect(rhs, candidates)
  }), use.names = FALSE))
  if (length(used)) stop("GJRM diagnostics do not yet support a modeled response used as a predictor. ",
    "Diagnostic references must be conditioned through the fitted copula. Response ",
    "variables in mean or auxiliary predictors: ", paste(used, collapse = ", "), ".",
    call. = FALSE)
  invisible(model)
}

gjrm_diagnostic_response <- function(model, response = NULL) {
  check_gjrm_joint_diagnostics(model, integer())
  if (is.null(response)) {
    check_gjrm_joint_diagnostics(model)
    return(seq_along(model$margins))
  }
  if (!is.numeric(response) || length(response) != 1L || is.na(response) ||
      !is.finite(response) || response != floor(response) ||
      !response %in% seq_along(model$margins))
    stop("'response' must be NULL or one response number in the model.", call. = FALSE)
  check_gjrm_joint_diagnostics(model, response)
  as.integer(response)
}

gjrm_joint_margin_proxy <- function(model, i) {
  margin <- model$margins[i]
  eta <- as.vector(model[[paste0("eta", i)]])
  y <- as.vector(model[[paste0("y", i)]])
  if (!length(y) || length(y) != length(eta))
    stop("GJRM fitted response parameters are not aligned.", call. = FALSE)
  binary <- margin %in% c("probit", "logit", "cloglog")
  if (binary) {
    fam <- stats::binomial(link = margin)
    probability <- switch(margin, probit = stats::pnorm(eta),
      logit = stats::plogis(eta), cloglog = -expm1(-exp(eta)))
    probability <- pmin(model$VC$max.pr %||% 1,
      pmax(model$VC$min.pr %||% 0, probability))
    distribution <- distribution_from_parameters("binomial", probability)
  } else {
    # Continuous-continuous fits store both scales; binary-continuous fits
    # use sigma2 for their single continuous response.
    scale <- if (!is.null(model$sigma21) || !is.null(model$sigma22))
      model[[paste0("sigma2", i)]] %||% model[[paste0("sigma", i)]] else model[[paste0("sigma", i)]]
    if (margin != "P" && (is.null(scale) || !length(scale) %in% c(1L, length(eta)) ||
        any(!is.finite(scale)) || any(scale <= 0)))
      stop("GJRM fitted scale parameters are unavailable or invalid.", call. = FALSE)
    single <- list(margins = margin, eta1 = eta, sigma2 = scale,
      univar.gamlss = TRUE)
    class(single) <- "SemiParBIV"
    distribution <- gjrm_response_distribution(single)
    fam <- switch(margin, N = stats::gaussian(), LN = stats::gaussian(),
      GA =, GAi = stats::Gamma(link = if (margin == "GAi") "identity" else "log"),
      P = stats::poisson(), list(family = distribution$family))
    if (margin == "LN") {
      fam$family <- "lognormal"
      fam$dev.resids <- NULL
    }
  }
  if (length(y) != length(eta) || length(y) != length(distribution$mean))
    stop("GJRM fitted response parameters are not aligned.", call. = FALSE)
  gam <- model[[paste0("gam", i)]]
  frame <- gam$model
  if (is.null(frame) || NROW(frame) != length(y)) frame <- data.frame(.response = y)
  proxy <- new_diagnostic_proxy(y, distribution$mean, eta, fam, distribution,
    model = frame, original = model, na.action = gam$na.action)
  proxy$eta_label <- if (binary) "Linear predictor" else
    gjrm_diagnostic_predictor_label(list(margins = margin))
  proxy
}

gjrm_joint_diagnostic_source <- function(model, response = NULL) {
  selected <- gjrm_diagnostic_response(model, response)
  components <- lapply(selected, function(i) gjrm_joint_margin_proxy(model, i))
  labels <- vapply(selected, function(i) {
    formula <- model$formula[[i]]
    label <- if (length(formula) == 3L) paste(deparse(formula[[2L]]), collapse = "") else paste("Response", i)
    paste0(label, " (", model$margins[i], ")")
  }, character(1))
  if (anyDuplicated(labels)) labels <- paste0("Response ", selected, ": ", labels)
  names(components) <- labels
  y <- unlist(lapply(components, `[[`, "y"), use.names = FALSE)
  mu <- unlist(lapply(components, `[[`, "fitted.values"), use.names = FALSE)
  eta <- unlist(lapply(components, `[[`, "linear.predictors"), use.names = FALSE)
  distributions <- lapply(components, `[[`, "distribution")
  family <- paste(unique(vapply(components, function(x) x$family$family, character(1))),
    collapse = ", ")
  distribution <- new_response_distribution(mean = mu,
    variance = unlist(lapply(distributions, `[[`, "variance"), use.names = FALSE),
    family = family, independent = length(components) == 1L,
    joint_available = FALSE, conditioning = "conditional",
    dependence_reason = "responses share the fitted copula; checks are separated by response",
    rd = NULL)
  out <- new_diagnostic_proxy(y, mu, eta, list(family = family),
    distribution, original = model)
  class(out) <- c("gratia_gjrm_diagnostic", class(out))
  out$components <- components
  out$response <- selected
  out$eta_label <- if (length(unique(vapply(components, `[[`, character(1), "eta_label"))) == 1L)
    components[[1L]]$eta_label else "Linear predictor"
  out
}

#' @export
response_distribution.gjrm <- function(model, conditioning = NULL, response = NULL, ...) {
  check_distribution_conditioning(conditioning)
  source <- gjrm_joint_diagnostic_source(model, response)
  if (length(source$components) == 1L) source$components[[1L]]$distribution else source$distribution
}

#' @export
diagnostic_data.gjrm <- function(model, type = NULL, seed = NULL,
    conditioning = NULL, ..., n_dharma = NULL, response = NULL) {
  check_distribution_conditioning(conditioning)
  diagnostic_data(gjrm_joint_diagnostic_source(model, response), type, seed,
    n_dharma = n_dharma)
}

#' @export
diagnostic_data.gratia_gjrm_diagnostic <- function(model, type = NULL, seed = NULL,
    conditioning = NULL, ..., n_dharma = NULL, response = NULL) {
  check_distribution_conditioning(conditioning)
  if (!is.null(response)) return(diagnostic_data(model$original, type, seed,
    n_dharma = n_dharma, response = response))
  type <- type %||% "quantile"
  # Derive one stream for every original response before selecting any of
  # them. A seeded selected-response call then matches its column in an
  # all-response call, without changing the caller's random-number state.
  seeds <- if (is.null(seed)) rep(list(NULL), length(model$original$margins)) else
    as.list(diagnostic_seed(seed, sample.int(.Machine$integer.max,
      length(model$original$margins))))
  parts <- lapply(seq_along(model$components), function(i) {
    component <- model$components[[i]]
    cached <- attr(component, "gratia.diagnostics")
    changed <- !is.null(seed) && !identical(seed, model$seed)
    changed <- changed || (type == "dharma" && !is.null(n_dharma) &&
      !identical(n_dharma, cached$n_dharma))
    if (changed) attr(component, "gratia.diagnostics") <- NULL
    diagnostic_data(component, type, seed = seeds[[model$response[i]]],
      n_dharma = n_dharma)
  })
  model$seed <- seed %||% model$seed
  frames <- lapply(seq_along(parts), function(i) {
    part <- parts[[i]]$data
    part$.response <- model$response[i]
    part$.family <- factor(rep(names(parts)[i] %||% names(model$components)[i], nrow(part)),
      levels = names(model$components))
    part
  })
  # Keep the calculated residuals on the source, so all four panels share the
  # same randomisation when a caller does not supply a seed.
  for (i in seq_along(parts)) {
    attr(model$components[[i]], "gratia.diagnostics") <- list(type = type,
      residuals = parts[[i]]$data$residuals, n_dharma = if (type == "dharma") parts[[i]]$n_dharma else NULL,
      seed = seeds[[model$response[i]]])
  }
  out <- new_diagnostic_data(dplyr::bind_rows(frames), model, type,
    vapply(model$components, function(x) x$family$family, character(1)),
    max(vapply(parts, `[[`, numeric(1), "n_obs")), model$eta_label)
  out$by_family <- TRUE
  if (type == "dharma") { out$n_dharma <- parts[[1L]]$n_dharma; out$seed <- seed }
  out
}

#' @export
diagnostic_qq_data.gratia_gjrm_diagnostic <- function(model, diagnostics,
    method = NULL, n_uniform = 10, n_simulate = 50, level = .9, by_family = NULL) {
  diagnostic_data_by_family(diagnostics, by_family)
  seeds <- if (diagnostics$type %in% c("deviance", "response", "pearson") &&
      !identical(method, "normal")) as.list(sample.int(.Machine$integer.max,
        length(model$original$margins))) else rep(list(NULL), length(model$original$margins))
  references <- lapply(seq_along(model$components), function(i) {
    rows <- diagnostics$data$.response == model$response[i]
    part <- diagnostics
    part$data <- diagnostics$data[rows, setdiff(names(diagnostics$data), ".family"), drop = FALSE]
    part$source <- model$components[[i]]
    part$n_obs <- nrow(part$data)
    reference <- diagnostic_seed(seeds[[model$response[i]]],
      diagnostic_qq_data.default(part$source, part, method,
        n_uniform, n_simulate, level, FALSE))
    if (identical(reference$method, "normal") && !diagnostics$type %in% c("pit", "quantile")) {
      reference$data$.qq_intercept <- reference$intercept
      reference$data$.qq_slope <- reference$slope
    }
    reference$data$.family <- factor(rep(names(model$components)[i], nrow(reference$data)),
      levels = names(model$components))
    reference
  })
  out <- references[[1L]]
  out$data <- dplyr::bind_rows(lapply(references, `[[`, "data"))
  out$center <- unlist(lapply(references, `[[`, "center"), use.names = FALSE)
  out$by_family <- TRUE
  out$colour_family <- FALSE
  out
}

#' @rdname quantile_residuals
#' @export
quantile_residuals.gjrm <- function(model, type = c("pit", "quantile"),
    seed = NULL, response = NULL, ...) {
  type <- match.arg(type)
  d <- diagnostic_data(model, type, seed, response = response, ...)
  values <- split(d$data$residuals, factor(d$data$.response,
    levels = d$source$response))
  out <- lapply(seq_along(values), function(i)
    restore_model_rows(values[[i]], d$source$components[[i]]))
  if (!is.null(response)) return(out[[1L]])
  out <- do.call(cbind, out)
  colnames(out) <- names(d$source$components)
  out
}

#' @rdname appraise
#' @export
appraise.gjrm <- function(model, ..., type = "quantile", seed = NULL,
    conditioning = NULL, response = NULL, ppcheck = FALSE, by_family = NULL, n_dharma = 250L) {
  if (!is.null(by_family) && !identical(by_family, TRUE))
    stop("GJRM diagnostics separate responses into facets; 'by_family' must be NULL or TRUE.", call. = FALSE)
  if (!is.logical(ppcheck) || length(ppcheck) != 1L || is.na(ppcheck))
    stop("'ppcheck' must be TRUE or FALSE.", call. = FALSE)
  if (isTRUE(ppcheck)) stop("Joint GJRM posterior predictive checks are unavailable.", call. = FALSE)
  d <- diagnostic_data(model, type, seed, conditioning, response = response, n_dharma = n_dharma)
  appraise.gam(d, ..., type = type, seed = seed, by_family = TRUE,
    ppcheck = FALSE, n_dharma = n_dharma)
}

#' @rdname qq_plot
#' @inheritParams appraise
#' @export
qq_plot.gjrm <- function(model, ..., type = "quantile", seed = NULL, response = NULL) {
  scalable_qq(model, ..., type = type, seed = seed, response = response, aggregate = FALSE)
}

#' @rdname worm_plot
#' @inheritParams appraise
#' @export
worm_plot.gjrm <- function(model, ..., type = "quantile", seed = NULL, response = NULL) {
  scalable_qq(model, ..., type = type, seed = seed, response = response, worm = TRUE, aggregate = FALSE)
}

#' @export
simulate.gjrm <- function(object, nsim = 1L, seed = NULL, ...) {
  stop("Joint GJRM response simulation is unavailable; responses must retain their fitted copula.", call. = FALSE)
}

#' @export
diagnostic_posterior_sampler.gjrm <- function(model, n, seed = NULL, conditioning = NULL, ...) {
  stop("Joint GJRM posterior predictive checks are unavailable.", call. = FALSE)
}

# Reuse the point layers and labels used by the prepared appraisal panels.
gjrm_diagnostic_point_plot <- function(d, panel, point_col = "grey20",
    point_alpha = 1, line_col = "red", xlab = NULL, ylab = NULL,
    title = NULL, subtitle = NULL, caption = NULL, rank = NULL, by_family = NULL) {
  gratia_plot_arguments(c("point_col", "point_alpha", "line_col"), environment())
  diagnostic_data_by_family(d, by_family)
  predictor <- identical(panel, "predictor")
  if (!is.null(rank) && (!is.logical(rank) || length(rank) != 1L || is.na(rank)))
    stop("'rank' must be TRUE, FALSE or NULL.", call. = FALSE)
  if (predictor) {
    d$data <- diagnostic_predictor_data(d$data, d$source, TRUE, rank)
    if (isTRUE(attr(d$data, "ranked"))) d$eta_label <- "Within-component linear predictor rank"
  }
  render_diagnostic_points(d$data, if (predictor) "eta" else "fitted",
    if (predictor) "residuals" else "observed",
    xlab %||% if (predictor) d$eta_label else "Fitted values",
    ylab %||% if (predictor) diagnostic_residual_label(d$type) else "Response",
    title %||% if (predictor) "Residuals vs linear predictor" else "Observed vs fitted values",
    point_col, point_alpha, if (predictor) line_col else NULL,
    line_intercept = if (d$type %in% c("pit", "dharma")) .5 else 0) +
    ggplot2::facet_wrap(ggplot2::vars(.data$.family), scales = "free") +
    ggplot2::labs(subtitle = subtitle, caption = caption)
}

#' @export
diagnostic_qq_data.gjrm <- function(model, diagnostics, method = NULL,
    n_uniform = 10, n_simulate = 50, level = .9, by_family = NULL) {
  diagnostic_qq_data(diagnostics$source, diagnostics, method = method,
    n_uniform = n_uniform, n_simulate = n_simulate, level = level,
    by_family = by_family)
}
