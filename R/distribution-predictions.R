# Pointwise limits for independently transformed predictors retain mgcv's
# link-scale construction. Coupled quantities use the full joint covariance.
fit_vals_grouped <- function(object, data, context, quantity, scale,
                             ci_level, unconditional, ...) {
  X <- gfam_predict_eta(object, data, type = "lpmatrix", ...)
  eta <- as.numeric(gfam_predict_eta(object, data, type = "link", ...))
  V <- get_vcov(object, unconditional = unconditional)
  se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  transform <- function(x) {
    if (quantity == "expectation" && gfam_category_expectations(context)) return(x)
    if (scale == "link") return(x)
    as.numeric(gfam_response_means(matrix(x, ncol = 1L), context, quantity))
  }
  value <- transform(eta)
  critical <- coverage_normal(ci_level)
  lower <- transform(eta - critical * se)
  upper <- transform(eta + critical * se)
  if (quantity == "expectation" && !gfam_category_expectations(context)) {
    h <- .Machine$double.eps^(1 / 3) * pmax(1, abs(eta))
    se <- se * abs((transform(eta + h) - transform(eta - h)) / (2 * h))
  }
  dplyr::bind_cols(data, tibble::tibble(.row = seq_len(nrow(data)),
    .parameter = if (quantity == "expectation") "expectation" else "predictor1",
    .fitted = value, .se = se, .lower_ci = pmin(lower, upper), .upper_ci = pmax(lower, upper)))
}

fit_vals_distribution <- function(object, data, spec, quantity, scale,
                                  ci_level, unconditional, empty = FALSE, ...) {
  if (empty) {
    value <- matrix(NA_real_, nrow(data), if (quantity == "parameters")
      sum(spec$parameters$returned) else spec$expectation_size)
    fit <- distribution_table(spec, value, quantity)
    fit$.se <- fit$.lower_ci <- fit$.upper_ci <- rep(NA_real_, nrow(fit))
    return(dplyr::bind_cols(data[fit$.row, , drop = FALSE], fit))
  }
  independent <- (quantity == "parameters" && !spec$coupled) ||
    (quantity == "expectation" && (!is.null(spec$expectation_predictor) || spec$multivariate))
  V <- if (!independent) get_vcov(object, unconditional = unconditional) else NULL
  pred <- predict_model(object, newdata = data, type = "link", se.fit = TRUE,
    unconditional = if (independent) unconditional else
      unconditional && !is.null(object$Vc), ...)
  eta <- matrix(pred$fit, nrow = nrow(data))
  se <- matrix(pred$se.fit, nrow = nrow(data))
  crit <- coverage_normal(ci_level)
  value <- distribution_output(spec, eta, quantity, scale)
  if (independent) {
    lower <- distribution_output(spec, eta - crit * se, quantity, scale)
    upper <- distribution_output(spec, eta + crit * se, quantity, scale)
    std_err <- if (spec$unknown) se[, 1L, drop = FALSE] else se
    if (quantity == "expectation" && !spec$multivariate) {
      std_err <- se[, spec$expectation_predictor, drop = FALSE]
      # Differentiate the expectation, not its link, for response-scale SEs.
      step <- .Machine$double.eps^(1 / 3) * pmax(1, abs(eta[, 1L]))
      plus <- minus <- eta
      plus[, 1L] <- plus[, 1L] + step
      minus[, 1L] <- minus[, 1L] - step
      slope <- (distribution_output(spec, plus, quantity, scale) -
        distribution_output(spec, minus, quantity, scale)) / (2 * step)
      std_err <- abs(slope) * std_err
    }
  } else {
    context <- lss_prediction_context(object, data = data, ...)
    categorical <- spec$categorical && scale == "response"
    if (categorical) {
      transform <- spec$category_transform(eta, gradient = TRUE)
      J <- categorical_coefficient_gradient(context, transform)
    } else {
      evaluate <- function(beta) as.numeric(distribution_output(spec,
        lss_predictors(context, beta), quantity, scale))
      J <- moment_gradient(evaluate, stats::coef(object), as.numeric(value))
    }
    std_err <- matrix(sqrt(pmax(0, rowSums((J %*% V) * J))), nrow(value))
    if (categorical) {
      limits <- categorical_probability_limits(transform, std_err, crit)
      std_err <- limits$se
      lower <- limits$lower
      upper <- limits$upper
    } else {
      lower <- value - crit * std_err
      upper <- value + crit * std_err
    }
  }
  fit <- distribution_table(spec, value, quantity)
  fit$.se <- as.numeric(std_err)
  fit$.lower_ci <- as.numeric(pmin(lower, upper))
  fit$.upper_ci <- as.numeric(pmax(lower, upper))
  # Each compact prediction row can occur once per parameter/category.
  dplyr::bind_cols(data[fit$.row, , drop = FALSE], fit) |>
    dplyr::relocate(".row", .before = 1L)
}

distribution_bootstrap_supported <- function(spec, quantity) {
  !spec$coupled && !spec$multivariate && !spec$unknown &&
    (quantity == "parameters" || (!spec$categorical && !is.null(spec$expectation_predictor)))
}

distribution_bootstrap_limits <- function(object, data, fit, spec, quantity,
                                          scale, ci_level, unconditional, ...) {
  limits <- predict_model(object, newdata = data, type = "link",
    se.fit = ci_level, unconditional = unconditional, ...)
  eta_lower <- matrix(limits$ll, nrow = nrow(data))
  eta_upper <- matrix(limits$ul, nrow = nrow(data))
  lower <- as.numeric(distribution_output(spec, eta_lower, quantity, scale))
  upper <- as.numeric(distribution_output(spec, eta_upper, quantity, scale))
  if (length(lower) != nrow(fit) || length(upper) != nrow(fit))
    stop("Native bootstrap limits do not match the fitted prediction rows.", call. = FALSE)
  fit$.lower_ci <- pmin(lower, upper)
  fit$.upper_ci <- pmax(lower, upper)
  fit
}
