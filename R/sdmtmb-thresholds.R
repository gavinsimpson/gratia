# Threshold parameters are nonlinear and remain separate from the linear
# coefficients used to reconstruct mgcv smooths.
sdmtmb_threshold <- function(object, component, fixed, X, covariance) {
  type <- object$threshold_function
  if (is.null(type) || !type %in% 1:2) return(NULL)
  kind <- c("breakpt", "logistic")[[type]]
  variable <- object$threshold_parameter
  index <- sdmtmb_parameter_indices(object, "b_threshold", component)
  expected <- if (type == 1L) 2L else 3L
  if (length(variable) != 1L || !variable %in% names(object$data) ||
      length(index) != expected) {
    stop("Cannot align sdmTMB threshold parameters.", call. = FALSE)
  }
  intercept <- which(colnames(X) == "(Intercept)")
  indices <- c(index, fixed[intercept])
  list(
    kind = kind, variable = variable,
    label = paste0(kind, "(", variable, ")"),
    parameters = unname(object$last.par.best[index]),
    indices = indices, intercept = length(intercept) > 0L,
    covariance = if (covariance) sdmtmb_joint_covariance(object, indices)
  )
}

sdmtmb_threshold_curve <- function(x, parameters, kind) {
  if (kind == "breakpt") {
    slope <- parameters[1L]
    cut <- parameters[2L]
    value <- pmin(x, cut)
    # At the cut, use the plateau-side derivative, as in native prediction.
    gradient <- cbind(value, slope * (x >= cut))
    estimate <- slope * value
  } else {
    midpoint <- parameters[1L]
    log_width <- parameters[2L]
    height <- parameters[3L]
    difference <- x - midpoint
    # Compute the argument on a log scale to avoid overflowing exp(-log_width).
    log_scale <- log(log(19)) - log_width
    argument <- sign(difference) * exp(log(abs(difference)) + log_scale)
    probability <- stats::plogis(argument)
    log_density <- stats::dlogis(argument, log = TRUE)
    midpoint_gradient <- -height * exp(log_density + log_scale)
    width_gradient <- -height * sign(argument) *
      exp(log_density + log(abs(argument)))
    width_gradient[!is.na(argument) & (argument == 0 | is.infinite(argument))] <- 0
    gradient <- cbind(midpoint_gradient, width_gradient, probability)
    estimate <- height * probability
  }
  list(estimate = estimate, gradient = gradient)
}

sdmtmb_threshold_effects <- function(
  object, terms = NULL, data = NULL, unnest = TRUE, n = 100,
  overall_uncertainty = TRUE, back_transform = FALSE,
  transform = lifecycle::deprecated(), ...
) {
  back_transform <- parametric_back_transform(back_transform, transform,
    supplied = !missing(back_transform))
  available <- names(parametric_terms(object))
  if (is.null(terms)) terms <- available
  if (!all(terms %in% available)) {
    stop("Term is not in the parametric part of model: ",
      paste(setdiff(terms, available), collapse = ", "), call. = FALSE)
  }
  threshold <- object$threshold
  suffix <- if (object$predictor == 1L) "" else paste0(".", object$predictor - 1L)
  label <- paste0(threshold$label, suffix)
  ordinary <- object
  ordinary$threshold <- NULL
  results <- list()
  want <- setdiff(terms, label)
  if (length(want)) {
    results[[1L]] <- parametric_effects.gam(ordinary, terms = want,
      data = data, unnest = unnest, n = n,
      overall_uncertainty = overall_uncertainty,
      back_transform = back_transform, ...)
  }
  if (label %in% terms) {
    variable <- threshold$variable
    observed <- object$model[[variable]]
    if (is.null(data)) {
      if (length(n) != 1L || !is.numeric(n) || !is.finite(n) ||
          n < 2 || n > .Machine$integer.max || n != floor(n)) {
        stop("'n' must be an integer of at least 2.", call. = FALSE)
      }
      limits <- range(observed, na.rm = TRUE)
      x <- seq(limits[1L], limits[2L], length.out = n)
      if (threshold$kind == "breakpt") {
        cut <- threshold$parameters[2L]
        if (cut >= limits[1L] && cut <= limits[2L]) x <- sort(unique(c(x, cut)))
      }
    } else {
      if (!is.data.frame(data) || !variable %in% names(data)) {
        stop("Threshold evaluation data must contain '", variable, "'.",
          call. = FALSE)
      }
      x <- data[[variable]]
    }
    if (!is.numeric(x) || is.matrix(x) || any(is.infinite(x))) {
      stop("Threshold covariates must be finite numeric values or NA.",
        call. = FALSE)
    }
    curve <- sdmtmb_threshold_curve(x, threshold$parameters, threshold$kind)
    G <- curve$gradient
    if (threshold$intercept) {
      G <- cbind(G, if (isTRUE(overall_uncertainty)) 1 else 0)
    }
    V <- threshold$covariance
    se <- sqrt(pmax(0, rowSums((G %*% V) * G)))
    effect <- tibble::tibble(
      .term = label, .type = "numeric", .value = x,
      .partial = unname(curve$estimate), .se = unname(se)
    )
    effect <- add_parametric_model_labels(effect, object)
    axes <- parametric_axes(object, paste(deparse(as.name(variable)), collapse = ""),
      stats::setNames(data.frame(x), variable), stats::setNames(".value", variable))
    axes <- parametric_axis_info(axes)
    attr(axes, "observations_retained") <- TRUE
    attr(axes, "observed") <- tibble::tibble(.value = observed)
    if (!unnest) {
      effect <- tidyr::nest(effect, data = c(".value", ".partial", ".se"))
    }
    attr(effect, "axis_info") <- stats::setNames(list(axes), label)
    attr(effect, "factor_levels") <- stats::setNames(list(NA), label)
    attr(effect, "back_transform") <- back_transform
    results[[length(results) + 1L]] <- effect
  }
  mixed_bind_parametric_effects(results)
}
