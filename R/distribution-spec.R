# Parameter descriptions and transformations shared by fitted predictions.
# Model adapters choose the provider: identical family names from different
# fitting packages need not describe the same parameters. Data extraction,
# design matrices and conditioning belong to the model context; distribution
# transformations belong here. New providers implement this internal S3
# interface without changing the fitted-output or response-sampling engines.
distribution_spec <- function(object, ...) UseMethod("distribution_spec")

#' @export
distribution_spec.default <- function(object, ...) {
  stop("No distribution specification for <", class(object)[1L], ">.", call. = FALSE)
}

#' @export
distribution_spec.gam <- function(object, ...) {
  spec <- distribution_spec(stats::family(object))
  if (isTRUE(object$scale.estimated) && !is_lss_family(stats::family(object)) &&
      !any(spec$parameters$parameter %in% c("sd", "dispersion"))) {
    spec$parameters <- dplyr::bind_rows(spec$parameters,
      parameter_description("dispersion", "phi", "Global likelihood dispersion",
        NA_integer_, "global", "fitted dispersion; held fixed during coefficient sampling"))
  }
  spec
}
#' @export
distribution_spec.scam <- function(object, ...) distribution_spec.gam(object, ...)
#' @export
distribution_spec.gamm <- function(object, ...) distribution_spec(object$gam)
#' @export
distribution_spec.gamm4 <- function(object, ...) distribution_spec(gamm4_gam(object))

parameter_description <- function(parameter, symbol, definition, predictor,
                                  source = "predictor", native = definition,
                                  link = NA_character_, component = NA_character_) {
  tibble::tibble(parameter = parameter, symbol = symbol, definition = definition,
    native = native, predictor = as.integer(predictor), source = source,
    link = link, component = component, returned = source != "global",
    uncertainty = ifelse(source == "global", "fixed", "joint coefficient draws"))
}

#' Describe a model family's distribution parameters
#'
#' Inspect the meaning and order of distribution parameters independently of
#' the numerical representation used by the fitting package.
#' @param object A fitted mgcv model, an existing GAM wrapper, or an mgcv
#'   family object. Other fitting packages require their own specification.
#' @param ... Arguments reserved for methods.
#' @return A tibble with `parameter`, `symbol`, `definition`, `native`,
#'   `predictor`, `source`, `link`, `component`, `returned`, and `uncertainty`.
#'   Predictor indices follow the fitting package's equation order. Global
#'   parameters have no predictor and are omitted from fitted parameter output;
#'   they remain fixed during coefficient sampling. Derived rows identify
#'   constraints such as a multinomial reference category. The `package` and
#'   `family` attributes identify the parameterisation provider. The
#'   `parameterisation` attribute describes the mathematical distribution.
#'   `expectation_condition` records conditions for a finite expected response.
#' @details
#' `fitted_values()` and `fitted_samples()` default to the parameters marked
#' `returned`. On the response scale these are natural distribution parameters,
#' which can differ from the fitting package's native response predictions.
#' For example, Gaussian LSS output contains standard deviation, whereas mgcv
#' predicts reciprocal standard deviation. GEV location is not its expectation.
#' `link` describes the native representation: for example, `ziplss()` uses
#' an identity link for log rate, while natural rate is the exponential of
#' that predictor.
#' The sinh-arcsinh skewness and tailweight parameters control distribution
#' shape; they are not the response's standardized skewness and kurtosis.
#'
#' `link()` and `inv_link()` retain the fitting package's native conventions
#' and their existing aliases. Use the definitions here to interpret them.
#' @seealso [fitted_values()], [fitted_samples()]
#' @export
#' @examples
#' family_parameters(mgcv::gevlss())
#' family_parameters(mgcv::gaulss())
#' family_parameters(mgcv::ziplss())
family_parameters <- function(object, ...) {
  spec <- distribution_spec(object, ...)
  out <- spec$parameters
  attr(out, "package") <- spec$package
  attr(out, "family") <- spec$family
  attr(out, "parameterisation") <- spec$parameterisation
  attr(out, "expectation_condition") <- spec$expectation_condition
  out
}

#' @export
distribution_spec.family <- function(object, ...) {
  f <- object
  ft <- family_type(f)
  if (is_gfam_family(f)) {
    components <- lapply(gfam_components(f), function(x) distribution_spec(x$family))
    parameters <- dplyr::bind_rows(lapply(seq_along(components), function(i) {
      p <- components[[i]]$parameters
      p$component <- paste0("component", i)
      p
    }))
    return(list(package = "mgcv", family = ft,
      parameterisation = "One component family per observation, in family-list order",
      parameters = parameters,
      capabilities = list(natural_parameters = all(vapply(components,
        function(x) x$capabilities$natural_parameters, logical(1L))),
        predictors = TRUE, expectation = all(vapply(components,
          function(x) x$capabilities$expectation, logical(1L)))),
      expectation_condition = vapply(components, `[[`, character(1L), "expectation_condition"),
      components = components, categorical = FALSE, multivariate = FALSE,
      coupled = TRUE, unknown = FALSE, expectation_predictor = NULL))
  }
  lss <- is_lss_family(f)
  categorical <- grepl("^ordered_categorical", ft) || ft == "ocat"
  multinom <- ft == "multinom"
  mvn <- ft == "multivariate_normal"
  zip <- grepl("^zero_inflated_poisson", ft) || ft == "zip"
  names <- switch(ft,
    gaulss = c("mean", "sd"), gammals = c("mean", "dispersion"),
    gumbls = c("location", "scale"), gevlss = c("location", "scale", "shape"),
    shash = c("location", "scale", "skewness", "tailweight"),
    ziplss = c("rate", "presence_probability"),
    twlss = c("mean", "power", "dispersion"),
    bcg = "transformed_mean", elf = "quantile",
    if (categorical) "location" else if (zip) "rate" else
      if (ft %in% c("scaled_t", "clog")) "location" else "mean")
  nlp <- if (lss || multinom || mvn) f$nlp else 1L
  if (multinom) names <- rep("probability", nlp + 1L)
  if (mvn) names <- rep("mean", nlp)
  unknown <- inherits(f, "general.family") && !lss && !multinom && !mvn &&
    !is_gfam_family(f)
  if (unknown) names <- "predictor1"
  definitions <- c(mean = "Mean of the response distribution", sd = "Standard deviation",
    location = "Location parameter, not necessarily the expected response",
    scale = "Positive scale parameter", shape = "Distribution shape parameter",
    skewness = "Sinh-arcsinh skewness parameter epsilon, not standardized skewness",
    tailweight = "Positive sinh-arcsinh tailweight delta, not response kurtosis",
    rate = "Poisson rate before zero truncation",
    presence_probability = "Probability of a positive response",
    power = "Tweedie variance power p", dispersion = "Dispersion phi in Var(Y) = phi * mean^p",
    probability = "Probability of membership in this category",
    transformed_mean = "Gaussian mean of the Box-Cox transformed response",
    quantile = "Modelled response quantile, not the expected response",
    predictor1 = "First native linear predictor; natural parameterisation unavailable")
  if (ft == "gammals") definitions["dispersion"] <- "Dispersion phi in Var(Y) = phi * mean^2"
  symbols <- c(mean = "mu", sd = "sigma", location = "mu", scale = "sigma",
    shape = "xi", skewness = "epsilon", tailweight = "delta", rate = "lambda",
    presence_probability = "p", power = "p", dispersion = "phi",
    probability = "p", predictor1 = "eta1", transformed_mean = "mu", quantile = "q")
  indices <- if (multinom) c(NA_integer_, seq_len(nlp)) else seq_along(names)
  native <- names
  if (lss) native <- switch(ft,
    gaulss = c("mean", "reciprocal standard deviation"),
    gammals = c("mean", "log dispersion"),
    gumbls = c("mean", "log scale"),
    gevlss = c("location", "log scale", "shape"),
    shash = c("location", "log scale", "skewness parameter", "log tailweight"),
    ziplss = c("log rate", "cloglog presence probability"),
    twlss = c("mean", "bounded power predictor", "log dispersion"))
  links <- if (lss || multinom || mvn) {
    vapply(f$linfo, function(x) x$name %||% x$link %||% "native link", character(1L))
  } else f$link %||% "native link"
  if (multinom) links <- c("reference category (zero predictor)", links)
  pars <- parameter_description(names, unname(symbols[names]), unname(definitions[names]),
    indices, ifelse(is.na(indices), "derived", "predictor"), native, rep_len(links, length(names)),
    if (multinom || mvn) paste0(if (multinom) "category" else "response", seq_along(names)) else NA_character_)
  global <- if (categorical) "thresholds" else if (mvn) "covariance" else
    if (!lss && is.function(f$getTheta)) switch(ft,
      negative_binomial = "shape", tweedie = "power", beta_regression = "precision",
      scaled_t = c("df", "scale"), cnorm = "sd", clog = "scale",
      bcg = c("power", "scale"), if (zip) c("presence_intercept", "presence_slope") else "family_parameters") else character()
  if (ft == "gaussian") global <- c(global, "sd")
  if (ft %in% c("gamma", "inverse_gaussian", "quasi", "quasipoisson", "quasibinomial", "tweedie"))
    global <- c(global, "dispersion")
  global_definitions <- c(shape = "Negative-binomial shape theta; Var(Y) = mean + mean^2 / theta",
    power = if (ft == "bcg") "Box-Cox transformation power" else "Tweedie variance power p",
    precision = "Beta precision phi; Var(Y) = mean * (1 - mean) / (1 + phi)",
    df = "Student-t degrees of freedom", scale = if (ft == "bcg")
      "Standard deviation of the Gaussian transformed response" else "Distribution scale, not necessarily standard deviation",
    sd = "Standard deviation of the Gaussian response, before any censoring",
    dispersion = "Dispersion multiplying the family's variance function",
    presence_intercept = "Intercept in the complementary log-log predictor of presence",
    presence_slope = "Slope relating log Poisson rate to the complementary log-log predictor of presence",
    thresholds = "Ordered category thresholds on the latent logistic scale",
    covariance = "Covariance matrix of the multivariate Gaussian responses",
    family_parameters = "Additional native family parameters")
  if (length(global)) pars <- dplyr::bind_rows(pars,
    parameter_description(global, NA_character_, unname(global_definitions[global]),
      NA_integer_, "global", ifelse(global == "sd", "square root of fitted dispersion",
        "native family parameters; held fixed during coefficient sampling")))

  to_parameters <- function(eta) {
    eta <- as.matrix(eta)
    if (unknown) stop("Natural distribution parameters are not implemented for family <",
      family_name(f), ">; use scale = 'link' for its first predictor.", call. = FALSE)
    if (multinom) return(multinomial_probability_transform(eta)$probability)
    if (mvn || categorical) return(eta)
    if (!lss) return(matrix(if (zip) exp(eta[, 1L]) else f$linkinv(eta[, 1L]), ncol = 1L))
    p <- do.call(cbind, lapply(seq_len(ncol(eta)), function(j) f$linfo[[j]]$linkinv(eta[, j])))
    if (ft == "gaulss") p[, 2L] <- 1 / p[, 2L]
    if (ft == "gammals") p[, 1:2] <- exp(p[, 1:2])
    if (ft == "gumbls") p[, 2L] <- exp(p[, 2L])
    if (ft == "gevlss") p[, 2L] <- exp(p[, 2L])
    if (ft == "shash") p[, c(2L, 4L)] <- exp(p[, c(2L, 4L)])
    if (ft == "ziplss") {
      p[, 1L] <- exp(p[, 1L]); p[, 2L] <- -expm1(-exp(p[, 2L]))
    }
    if (ft == "twlss") {
      bounds <- get_tw_params(f)
      p[, 2L] <- twlss_theta_2_power(p[, 2L], bounds[1L], bounds[2L])
      p[, 3L] <- exp(p[, 3L])
    }
    p
  }
  to_native <- function(p = NULL, eta = NULL) {
    # Keep the native evaluation path when predictors are available. Avoiding
    # exp/log round trips preserves native generators and their seeded draws.
    if (!is.null(eta)) return(family_prediction_parameters(f, eta))
    p <- as.matrix(p)
    if (ft == "gaulss") p[, 2L] <- 1 / p[, 2L]
    if (ft == "gammals") p[, 2L] <- log(p[, 2L])
    if (ft == "gumbls") {
      p[, 1L] <- p[, 1L] - digamma(1) * p[, 2L]
      p[, 2L] <- log(p[, 2L])
    }
    if (ft == "gevlss") p[, 2L] <- log(p[, 2L])
    if (ft == "shash") p[, c(2L, 4L)] <- log(p[, c(2L, 4L)])
    if (ft == "ziplss") {
      p[, 1L] <- log(p[, 1L]); p[, 2L] <- log(-log1p(-p[, 2L]))
    }
    if (ft == "twlss") {
      bounds <- get_tw_params(f)
      p[, 2L] <- qlogis((p[, 2L] - bounds[1L]) / diff(bounds))
      p[, 3L] <- log(p[, 3L])
    }
    if (zip) p[, 1L] <- log(p[, 1L])
    if (!lss && !multinom && !mvn) return(as.vector(p))
    p
  }
  expectation <- function(p) {
    if (ft == "cox_ph") stop("Cox survival predictions require evaluation times and strata; ",
      "a location parameter alone does not define an expected response.", call. = FALSE)
    if (unknown) stop("The expected response is not implemented for family <", family_name(f), ">.", call. = FALSE)
    if (multinom || mvn) return(p)
    if (categorical) {
      return(ordinal_probability_transform(p[, 1L], f$getTheta(TRUE))$probability)
    }
    if (lss) return(matrix(lss_expectation(f, to_native(p)), ncol = 1L))
    if (ft == "bcg") {
      th <- f$getTheta(TRUE)
      if (th[1L] != 0) stop("Original-response expectations for bcg require a fixed Box-Cox parameter of zero.", call. = FALSE)
      return(matrix(exp(p[, 1L] + th[2L]^2 / 2), ncol = 1L))
    }
    if (ft == "elf") stop("An expected response is not implemented for this quantile family.", call. = FALSE)
    check_family_expectation(f)
    if (is.function(f$predict)) {
      value <- f$predict(f, se = FALSE, eta = f$linkfun(to_native(p)))[[1L]]
      if (!is.numeric(value) || length(value) != nrow(p) ||
          (!is.null(dim(value)) && ncol(value) != 1L)) stop(
        "Fitted expectations require a scalar response mean per row for this family.", call. = FALSE)
      return(matrix(value, ncol = 1L))
    }
    matrix(family_response_mean(f, to_native(p)), ncol = 1L)
  }
  parameterisation <- switch(ft,
    gaulss = "Gaussian(mean, sd); Var(Y) = sd^2",
    gammals = "Gamma(mean, dispersion); Var(Y) = dispersion * mean^2",
    gumbls = "Y = location - scale * log(-log(U)); U uniform on (0, 1)",
    gevlss = "GEV(location, scale, shape); location is not the expected response",
    shash = "Y = location + scale * tailweight * sinh((asinh(Z) + skewness) / tailweight); Z standard normal",
    ziplss = "P(Y > 0) = presence_probability; Y | Y > 0 is zero-truncated Poisson(rate)",
    twlss = "Tweedie(mean, power, dispersion); Var(Y) = dispersion * mean^power",
    paste(family_name(f), paste(names, collapse = ", ")))
  list(package = "mgcv", family = ft, parameterisation = parameterisation, parameters = pars,
    capabilities = list(natural_parameters = !unknown, predictors = TRUE,
      expectation = !unknown && ft != "elf"),
    expectation_condition = switch(ft, gevlss = "shape < 1", scaled_t = "df > 1",
      bcg = "fixed Box-Cox power = 0", "finite parameters in the distribution domain"),
    expectation_size = if (categorical) length(f$getTheta(TRUE)) + 1L else
      if (multinom || mvn) length(names) else 1L,
    to_parameters = to_parameters, to_native = to_native, expectation = expectation,
    category_transform = if (categorical) function(eta, gradient = FALSE)
      ordinal_probability_transform(eta[, 1L], f$getTheta(TRUE), gradient) else
      if (multinom) multinomial_probability_transform else NULL,
    categorical = categorical || multinom, multivariate = mvn,
    coupled = multinom, unknown = unknown,
    expectation_predictor = if (ft %in% c("gaulss", "gammals", "twlss") ||
      (!lss && !categorical && !zip && !multinom && !mvn && !unknown)) 1L else NULL)
}

prediction_choice <- function(quantity, scale) {
  quantity <- match.arg(quantity, c("parameters", "expectation"))
  scale <- match.arg(scale, c("response", "link", "linear_predictor", "linear predictor"))
  if (quantity == "expectation" && scale != "response") stop(
    "Expected responses are available only on the response scale. ",
    "Use quantity = 'parameters' for parameter predictors on the link scale.", call. = FALSE)
  list(quantity = quantity, scale = if (scale == "response") scale else "link")
}

distribution_output <- function(spec, eta, quantity, scale) {
  if (quantity == "expectation" && spec$unknown) return(spec$expectation(eta))
  if (quantity == "expectation") return(as.matrix(spec$expectation(spec$to_parameters(eta))))
  if (scale == "response") return(as.matrix(spec$to_parameters(eta)))
  if (spec$coupled) return(cbind(0, eta))
  if (spec$unknown) return(eta[, 1L, drop = FALSE])
  eta
}

distribution_labels <- function(spec, quantity, n) {
  pars <- spec$parameters[spec$parameters$returned, ]
  if (quantity == "parameters") return(tibble::tibble(.parameter = pars$parameter,
    .component = pars$component))
  tibble::tibble(.parameter = rep("expectation", n),
    .component = if (spec$categorical) paste0("category", seq_len(n)) else
      if (spec$multivariate) paste0("response", seq_len(n)) else rep(NA_character_, n))
}

distribution_table <- function(spec, value, quantity, draw = NULL) {
  labels <- distribution_labels(spec, quantity, ncol(value))
  # Rows vary first, then parameters, then coefficient draws.
  out <- tibble::tibble(.row = rep(seq_len(nrow(value)), ncol(value)),
    .parameter = rep(labels$.parameter, each = nrow(value)),
    .fitted = as.numeric(value))
  if (any(!is.na(labels$.component))) {
    id <- if (spec$categorical) ".category" else if (spec$multivariate) ".y" else ".component"
    out[[id]] <- rep(labels$.component, each = nrow(value))
  }
  if (!is.null(draw)) out$.draw <- draw
  out
}

attach_prediction_spec <- function(out, spec, quantity, scale, se_scale = NULL) {
  attr(out, "prediction_info") <- list(package = spec$package, family = spec$family,
    parameterisation = spec$parameterisation,
    capabilities = spec$capabilities, expectation_condition = spec$expectation_condition,
    quantity = quantity, scale = scale, se_scale = se_scale, parameters = spec$parameters,
    global_parameters = "held fixed during coefficient sampling")
  out
}

distribution_fitted_draws <- function(context, betas, quantity, scale) {
  spec <- context$spec %||% distribution_spec(context$model)
  n <- context$n_rows %||% nrow(context$X)
  if (is.function(context$predictors)) {
    # Other fitters can supply predictors directly, including spatial fields
    # or nonlinear terms, without manufacturing an mgcv model or design matrix.
    output <- lapply(seq_len(nrow(betas)), function(i) {
      value <- if (all(is.na(context$layout$map))) {
        matrix(NA_real_, n, if (quantity == "parameters")
          sum(spec$parameters$returned) else spec$expectation_size)
      } else distribution_output(spec, context$predictors(betas[i, ]), quantity, scale)
      distribution_table(spec, value, quantity, i)
    })
  } else {
    # Matrix products are batched to bound temporary memory while retaining draw order.
    batch <- max(1L, floor(1e5 / max(1L, n)))
    output <- vector("list", nrow(betas))
    for (start in seq.int(1L, nrow(betas), by = batch)) {
      rows <- seq.int(start, min(start + batch - 1L, nrow(betas)))
      eta <- lapply(seq_along(context$index), function(i) {
        j <- context$index[[i]]
        as.numeric(context$X[, j, drop = FALSE] %*% t(betas[rows, j, drop = FALSE]) + context$offsets[, i])
      })
      eta <- do.call(cbind, eta)
      value <- if (all(is.na(context$layout$map))) {
        matrix(NA_real_, n * length(rows), if (quantity == "parameters")
          sum(spec$parameters$returned) else spec$expectation_size)
      } else distribution_output(spec, eta, quantity, scale)
      for (i in seq_along(rows)) {
        k <- (i - 1L) * n + seq_len(n)
        output[[rows[i]]] <- distribution_table(spec, value[k, , drop = FALSE], quantity, rows[i])
      }
    }
  }
  out <- dplyr::bind_rows(output)
  out <- restore_prediction_table(out, context$layout)
  out <- dplyr::relocate(out, dplyr::any_of(c(".row", ".draw", ".parameter")))
  attach_prediction_spec(out, spec, quantity, scale)
}

# These fitted family parameters are fixed across coefficient draws.
check_family_expectation <- function(family) {
  if (family_type(family) == "scaled_t" && family$getTheta(TRUE)[1L] <= 1) {
    stop("The expected response does not exist for scaled t with df <= 1.",
      call. = FALSE)
  }
  invisible(NULL)
}
