# Use the group labels stored with the fit so each observation retains its
# family. gfam_components() reconstructs functions with each family's fitted
# nuisance parameters.
gfam_diagnostic_context <- function(model) {
  model <- model_used_rows(model)
  if (!length(model$y)) {
    stop("Grouped diagnostics need at least one fitted observation.", call. = FALSE)
  }
  context <- gfam_prediction_context(model)
  context$model <- model
  context$eta <- as.numeric(model$linear.predictors)
  context$weights <- model$prior.weights
  context$y <- model$y
  if (!is.numeric(context$y) || !is.null(dim(context$y)))
    stop("Grouped diagnostics require a scalar response per observation.", call. = FALSE)
  censor <- attr(context$y, "censor")
  if (!is.null(censor) && (length(censor) != length(context$y) || anyNA(censor)))
    stop("Grouped censoring bounds are incomplete or not aligned with responses.",
      call. = FALSE)
  if (any(!is.finite(context$weights)) || any(context$weights < 0))
    stop("Invalid grouped-family weights.", call. = FALSE)
  context$mu <- numeric(length(context$eta))
  for (i in seq_along(context$components)) {
    rows <- which(context$index == i)
    if (!length(rows)) next
    context$mu[rows] <- context$families[[i]]$linkinv(context$eta[rows])
  }
  context
}

gfam_component_distribution <- function(context, i, family = context$components[[i]]$family) {
  rows <- which(context$index == i)
  latent <- grepl("^ordered_categorical|^zero_inflated_poisson", family_type(family))
  distribution_from_family(family,
    if (latent) context$eta[rows] else context$mu[rows], context$weights[rows],
    context$components[[i]]$scale, slice_observations(context$y, rows),
    eta = context$eta[rows])
}

gfam_diagnostic_residuals <- function(context, y = context$y,
                                      type = "deviance") {
  out <- rep(NA_real_, length(y))
  for (i in seq_along(context$components)) {
    rows <- which(context$index == i)
    if (!length(rows)) next
    component <- context$components[[i]]
    family <- component$family
    if (is.function(family$gratia_diagnostics)) {
      d <- gfam_component_distribution(context, i)
      check_distribution_residual_moments(d, type)
      if (is.function(d$diagnostic_residuals)) {
        out[rows] <- d$diagnostic_residuals(slice_observations(y, rows), type)
        next
      }
    }
    ordinal <- startsWith(family$family, "Ordered Categorical")
    if (ordinal && type != "deviance") {
      stop("Grouped ordinal diagnostics support only deviance residuals.",
        call. = FALSE)
    }
    if (type == "pearson" && !is.function(family$variance)) {
      stop("Pearson residuals are unavailable for grouped component ", i,
        " <", family$family, ">.", call. = FALSE)
    }
    yy <- slice_observations(y, rows)
    mu <- context$mu[rows]
    wt <- context$weights[rows]
    if (is.function(family$residuals)) {
      # The native residual function supplies the reference log likelihood used
      # in beta and zero-inflated deviance residuals. Calling dev.resids alone
      # omits that adjustment.
      proxy <- context$model
      proxy$family <- family
      proxy$model <- proxy$model[rows, , drop = FALSE]
      for (name in c("weights", "residuals")) proxy[[name]] <- proxy[[name]][rows]
      proxy$y <- yy
      proxy$fitted.values <- mu
      proxy$linear.predictors <- context$eta[rows]
      proxy$prior.weights <- wt
      proxy$sig2 <- component$scale
      subset_offset <- function(x) if (length(x) > 1L) x[rows] else x
      proxy$offset <- if (is.list(proxy$offset)) lapply(proxy$offset, subset_offset)
        else subset_offset(proxy$offset)
      value <- family$residuals(proxy, type = type)
      out[rows] <- as.numeric(value) / if (type == "deviance")
        sqrt(component$scale) else 1
    } else {
      out[rows] <- switch(type,
        deviance = deviance_residuals(yy, mu, wt, family$dev.resids,
          context$model) / sqrt(component$scale),
        response = yy - mu,
        pearson = pearson_residuals(yy, mu, wt, family$variance))
    }
  }
  out
}

diagnostic_residuals <- function(model, type, seed = NULL, n_dharma = NULL) {
  if (type %in% c("response", "pearson"))
    check_distribution_residual_moments(response_distribution(model), type)
  cached <- attr(model, "gratia.diagnostics")
  if (identical(type, "dharma")) {
    n <- dharma_simulation_count(model, n_dharma)
    if (!is.null(cached) && identical(cached$type, type) &&
        identical(cached$n_dharma, n) &&
        (is.null(seed) || identical(seed, cached$seed)))
      return(restore_model_rows(cached$residuals, model))
    used <- model_used_rows(model)
    return(restore_model_rows(model_dharma_residuals(used, n, seed), model))
  }
  if (!is.null(cached) && identical(cached$type, type)) {
    return(restore_model_rows(cached$residuals, model))
  }
  if (identical(type, "surrogate")) {
    return(surrogate_residuals(model, seed = seed))
  }
  if (type %in% c("pit", "quantile")) {
    return(quantile_residuals(model, type = type, seed = seed))
  }
  if (is_gfam_family(family(model))) {
    r <- gfam_diagnostic_residuals(gfam_diagnostic_context(model), type = type)
    return(restore_model_rows(r, model))
  }
  distribution <- response_distribution(model_used_rows(model))
  if (is.function(distribution$diagnostic_residuals)) {
    used <- model_used_rows(model)
    return(restore_model_rows(distribution$diagnostic_residuals(used$y, type), model))
  }
  stats::residuals(model, type = type)
}

gfam_diagnostic_method <- function(model, method = NULL) {
  context <- gfam_diagnostic_context(model)
  active <- sort(unique(context$index))
  families <- context$families[active]
  distributions <- lapply(active, function(i) gfam_component_distribution(context, i))
  qf <- vapply(distributions, function(d) is.function(d$qf), logical(1))
  rd <- vapply(distributions, function(d) is.function(d$rd), logical(1))
  if (!identical(method, "normal")) lapply(distributions, check_distribution_reference)
  label <- function(which) paste0(active[which], " <",
    vapply(families[which], `[[`, character(1), "family"), ">", collapse = ", ")
  if (is.null(method)) {
    if (all(rd)) return("simulate")
    if (all(qf)) {
      message("Using inverse-CDF reference quantiles: no generator for grouped ",
        "component ", label(!rd), ".")
      return("uniform")
    }
    stop("Grouped-family reference distributions are unavailable for component ",
      label(!rd | !qf), "; 'method = \"normal\"' is an explicit approximation.",
      call. = FALSE)
  }
  method <- match.arg(method, c("uniform", "simulate", "normal", "direct"))
  if (method == "direct") {
    message('`method = "direct"` is deprecated, use `"uniform"`')
    method <- "uniform"
  }
  if (method == "uniform" && !all(qf)) {
    if (!all(rd)) stop("No quantile function or generator for grouped component ",
      label(!qf | !rd), ".", call. = FALSE)
    message("Using simulated reference quantiles: no quantile function for ",
      "grouped component ", label(!qf), ".")
    return("simulate")
  }
  if (method == "simulate" && !all(rd)) {
    if (!all(qf)) stop("No generator or quantile function for grouped component ",
      label(!qf | !rd), ".", call. = FALSE)
    message("Using inverse-CDF reference quantiles: no generator for grouped ",
      "component ", label(!rd), ".")
    return("uniform")
  }
  method
}

# Select another residual type only when CDF support is missing. Invalid inputs
# or failed probability calculations must be reported as errors.
gfam_cdf_support <- function(context) {
  active <- sort(unique(context$index))
  unsupported <- character()
  for (i in active) {
    f <- context$families[[i]]
    ft <- family_type(f)
    known <- ft %in% c("gaussian", "poisson", "binomial", "negative_binomial", "gamma",
      "beta_regression", "scaled_t", "tweedie", "inverse_gaussian", "cnorm", "clog", "cpois") ||
      grepl("^ordered_categorical|^zero_inflated_poisson", ft)
    supplied <- is.function(f$gratia_diagnostics)
    d <- if (supplied) gfam_component_distribution(context, i) else NULL
    if ((!known && !(supplied && isTRUE(d$cdf_bounds_supplied))) ||
        !(if (supplied) is.function(d$cdf) && is.function(d$bounds)
        else is.function(fix_family_cdf(f)$cdf))) unsupported <- c(unsupported,
      paste0(i, " <", f$family, "> (no validated response-CDF bounds)"))
  }
  unsupported
}

diagnostic_type <- function(model, type = NULL, allow_pit = TRUE) {
  if (is.null(type)) {
    default <- attr(model, "gratia.default_residual_type") %||% model$diagnostic_default_type
    if (is_multinom_diagnostics(model) && is.null(default)) default <- model$type
    if (!is.null(default)) return(default)
    if (!is_gfam_family(family(model))) return("deviance")
    unsupported <- gfam_cdf_support(gfam_diagnostic_context(model))
    if (!length(unsupported)) return("quantile")
    message("Using deviance residuals: quantile residuals are unavailable for ",
      "grouped component ", paste(unsupported, collapse = ", "), ".")
    return("deviance")
  }
  choices <- if (allow_pit) c("deviance", "response", "pearson", "pit", "quantile", "dharma", "surrogate")
    else c("deviance", "response", "pearson", "quantile", "dharma", "surrogate")
  type <- match.arg(type, choices)
  if (type == "surrogate") check_surrogate_model(model)
  type
}

diagnostic_by_family <- function(model, type, by_family = NULL) {
  if (!is.null(by_family) && (!is.logical(by_family) || length(by_family) != 1L ||
      is.na(by_family))) stop("'by_family' must be TRUE, FALSE, or NULL.", call. = FALSE)
  is_gfam_family(family(model)) &&
    if (is.null(by_family)) !type %in% c("quantile", "pit", "dharma") else by_family
}

gfam_checked_cdf <- function(cdf, component = NULL) {
  check <- function(value, q, mu, log_p) {
    n <- max(length(q), NROW(mu))
    valid <- is.numeric(value) && length(value) == n &&
      !anyNA(value) && !any(is.nan(value))
    valid <- valid && if (log_p) all(value <= 0) else
      all(is.finite(value) & value >= 0 & value <= 1)
    if (!valid) stop("Grouped-family CDF returned invalid probabilities",
      if (!is.null(component)) paste0(" for component ", component), ".", call. = FALSE)
    as.numeric(value)
  }
  wrapped <- if ("lower_tail" %in% names(formals(cdf))) {
    function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
      check(cdf(q, mu, wt, scale, log_p = log_p, lower_tail = lower_tail), q, mu, log_p)
    }
  } else {
    function(q, mu, wt, scale, log_p = FALSE) {
      check(cdf(q, mu, wt, scale, log_p = log_p), q, mu, log_p)
    }
  }
  attr(wrapped, "gratia_beta_cdf") <- attr(cdf, "gratia_beta_cdf")
  wrapped
}

gfam_quantile_residuals <- function(context, type = "quantile") {
  unsupported <- gfam_cdf_support(context)
  if (length(unsupported)) stop("Quantile residuals are unavailable for grouped ",
    "component ", paste(unsupported, collapse = ", "), ".", call. = FALSE)
  out <- rep(NA_real_, length(context$y))
  for (i in sort(unique(context$index))) {
    rows <- which(context$index == i)
    component <- context$components[[i]]
    f <- fix_family_cdf(component$family)
    ft <- family_type(f)
    y <- slice_observations(context$y, rows)
    wt <- context$weights[rows]
    mu <- context$mu[rows]
    if (any(!is.finite(wt)) || any(wt < 0) || any(!is.finite(mu)))
      stop("Invalid grouped-family weights or fitted values.", call. = FALSE)
    if (ft %in% c("gaussian", "cnorm", "clog") && any(wt <= 0))
      stop("Gaussian and logistic precision weights must be positive.", call. = FALSE)
    if (ft == "binomial" && (any(wt != floor(wt)) || any(y < 0 | y > 1) ||
        any(abs(y * wt - round(y * wt)) > 1e-7)))
      stop("Binomial components need integer trial counts and valid proportions.", call. = FALSE)
    if ((ft %in% c("poisson", "negative_binomial") || grepl("^zero_inflated_poisson", ft)) &&
        any(y < 0 | abs(y - round(y)) > 1e-7))
      stop("Count components need non-negative integer responses.", call. = FALSE)
    if (!ft %in% c("cnorm", "clog", "cpois") && any(!is.finite(y)))
      stop("Invalid grouped-family responses.", call. = FALSE)
    if ((ft == "gamma" && any(y <= 0)) ||
        (ft == "beta_regression" && any(y <= 0 | y >= 1)) ||
        (ft == "tweedie" && any(y < 0)))
      stop("Grouped-family response is outside the distribution's support.", call. = FALSE)
    if (!is.function(f$gratia_diagnostics))
      f$cdf <- gfam_checked_cdf(f$cdf, paste0(i, " <", f$family, ">"))
    if (grepl("^ordered_categorical", ft) &&
        any(y < 1 | y > length(f$getTheta(TRUE)) + 1 | y != floor(y)))
      stop("Ordinal components need valid integer category codes.", call. = FALSE)
    value <- distribution_quantile_residuals(gfam_component_distribution(context, i, f), y, type)
    if (any(!is.finite(value))) stop("Non-finite ", type,
      " residuals for grouped component ", i, " <", f$family,
      ">; check response support and CDF precision.", call. = FALSE)
    out[rows] <- value
  }
  out
}

# Store calculated residuals on the local model copy used by appraise().
# Randomized residuals then use the same draws in every panel.
cache_diagnostic_residuals <- function(model, type, seed) {
  used <- model_used_rows(model)
  r <- diagnostic_residuals(used, type, seed)
  attr(model, "gratia.diagnostics") <- list(type = type, residuals = r)
  model
}

diagnostic_component_data <- function(data, model) {
  if (is_gfam_family(family(model))) {
    labels <- gfam_diagnostic_labels(model)
    index <- attr(family(model)$getfl(), "fi")
    data$.family <- factor(restore_model_rows(labels[index], model), levels = labels)
  }
  data
}

calibrated_qq_data <- function(model, level, detrend, by_family, type = "quantile", n_simulate = 50L) {
  used <- model_used_rows(model)
  data <- diagnostic_component_data(data.frame(residuals = as.vector(
    diagnostic_residuals(used, type))), used)
  calibrated_residual_reference(data, type, level, detrend, by_family,
    distribution = response_distribution(used), n_simulate = n_simulate)
}

calibrated_qq_plot <- function(model, method, level, detrend, by_family,
                               ylab, xlab, title, subtitle, caption, ci_col,
                               ci_alpha, point_col, point_alpha, line_col, type = "quantile", n_simulate = 50L) {
  if (!is.null(method) && !(type == "quantile" && identical(method, "normal"))) {
    if (type == "surrogate") stop(
      "Surrogate residuals use a fixed logistic reference; use method = NULL.",
      call. = FALSE)
    stop("Calibrated residuals use a fixed standard-normal reference; use ",
      "'type = \"deviance\"' for uniform or simulated response references.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) stop("'level' must be between zero and one.", call. = FALSE)
  data <- calibrated_qq_data(model, level, detrend, by_family, type, n_simulate)
  plt <- gratia_plot(data, ggplot2::aes(x = .data$theoretical, y = .data$residuals))
  if (detrend) plt <- plt + geom_gratia_diagnostic_reference_hline(yintercept = 0, colour = line_col)
  else plt <- plt + geom_gratia_diagnostic_reference_abline(intercept = 0, slope = 1, colour = line_col)
  if ("lower" %in% names(data)) plt <- plt + geom_gratia_diagnostic_interval_ribbon(
    ggplot2::aes(ymin = .data$lower, ymax = .data$upper), fill = ci_col, alpha = ci_alpha)
  if (".family" %in% names(data) && !by_family) {
    plt <- plt + geom_gratia_diagnostic_point(ggplot2::aes(colour = .data$.family), alpha = point_alpha) +
      gratia_default_scale(scale_colour_okabe_ito(limits = gfam_diagnostic_labels(model),
        drop = FALSE))
  } else plt <- plt + geom_gratia_diagnostic_point(colour = point_col, alpha = point_alpha)
  plt <- plt + ggplot2::labs(x = xlab %||% calibrated_reference_axis(type),
    y = ylab %||% paste0(diagnostic_residual_label(type),
      if (detrend) " (Deviation)" else ""),
    title = title %||% if (detrend) "Worm plot of residuals" else "QQ plot of residuals",
    subtitle = subtitle %||% calibrated_reference_subtitle(type),
    caption = caption %||% diagnostic_qq_caption(model, data))
  if (".family" %in% names(data) && !by_family)
    plt <- plt + ggplot2::labs(colour = "Family component")
  gfam_diagnostic_facets(plt, model, by_family)
}

# Each simulated response keeps the family assigned to that observation.
# Calculate its residual with that family before sorting across all rows or
# within each component.
gfam_qq <- function(model, n, type, method, level = 0.9, detrend = FALSE,
                    by_family = TRUE) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
      n < 1L || n != trunc(n)) {
    stop("The number of reference samples must be a positive integer.", call. = FALSE)
  }
  context <- gfam_diagnostic_context(model)
  if (!isTRUE(by_family)) {
    return(gfam_qq_reference(context, n, type, method, level, detrend))
  }
  labels <- gfam_diagnostic_labels(model)
  active <- sort(unique(context$index))
  pieces <- lapply(active, function(i) {
    rows <- which(context$index == i)
    part <- context
    for (name in c("eta", "mu", "weights", "y", "index"))
      part[[name]] <- slice_observations(context[[name]], rows)
    part$model$model <- context$model$model[rows, , drop = FALSE]
    for (name in c("y", "linear.predictors", "fitted.values", "weights",
        "prior.weights", "residuals"))
      part$model[[name]] <- slice_observations(context$model[[name]], rows)
    offset <- context$model$offset
    subset_offset <- function(x) if (length(x) > 1L) x[rows] else x
    part$model$offset <- if (is.list(offset)) lapply(offset, subset_offset)
      else subset_offset(offset)
    ans <- gfam_qq_reference(part, n, type, method, level, detrend)
    ans$.family <- factor(labels[i], levels = labels)
    ans
  })
  dplyr::bind_rows(pieces)
}

gfam_qq_reference <- function(context, n, type, method, level, detrend) {
  r <- sort(gfam_diagnostic_residuals(context, type = type))
  nr <- length(r)
  if (method == "normal") {
    theoretical <- stats::qnorm(stats::ppoints(nr))
    sd <- stats::IQR(r) / 1.349
    med <- stats::median(r) + theoretical * sd
    p <- stats::pnorm(theoretical)
    crit_se <- coverage_normal(level) * sd * sqrt(p * (1 - p) / nr) /
      stats::dnorm(theoretical)
    return(tibble::tibble(theoretical = theoretical,
      residuals = if (detrend) r - med else r,
      lower = med - crit_se - if (detrend) med else 0,
      upper = med + crit_se - if (detrend) med else 0,
      .qq_intercept = stats::median(r), .qq_slope = sd))
  }
  probabilities <- (seq_len(nr) - 0.5) / nr
  distributions <- lapply(seq_along(context$families), function(i) {
    if (any(context$index == i)) gfam_component_distribution(context, i) else NULL
  })
  distribution <- combine_response_distributions(distributions, context$index)
  check_distribution_reference(distribution)
  qq_reference_draws(r, draw = function(probabilities) {
    if (method == "simulate") {
      distribution$rd()
    } else {
      p <- sample(probabilities, nr)
      y <- numeric(nr)
      for (i in seq_along(context$families)) {
        rows <- which(context$index == i)
        if (!length(rows)) next
        y[rows] <- distributions[[i]]$qf(p[rows])
      }
      y
    }
  }, residual = function(y) gfam_diagnostic_residuals(context,
    observe_diagnostic_response(distribution, y), type),
    n = n, method = method, level = level, detrend = detrend)
}

gfam_diagnostic_labels <- function(model) {
  families <- family(model)$getfl()
  paste0(seq_along(families), ": ",
    vapply(families, `[[`, character(1), "family"))
}

gfam_diagnostic_facets <- function(plot, model, by_family) {
  if (isTRUE(by_family) && is_gfam_family(family(model))) {
    if (!".family" %in% names(plot$data)) {
      labels <- gfam_diagnostic_labels(model)
      index <- attr(family(model)$getfl(), "fi")
      plot$data$.family <- factor(restore_model_rows(labels[index], model),
        levels = labels)
    }
    # Excluded fitting rows have no retained component membership. Do not
    # create an extra NA panel from their placeholder rows.
    plot$data <- plot$data[!is.na(plot$data$.family), , drop = FALSE]
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data[[".family"]]), scales = "free")
  }
  plot
}

# The shared distribution supplies expected category values for ordinal response
# plots, so this compatibility check imposes no additional restriction.
check_gfam_appraise <- function(model) invisible(model)


# Rank predictors using only finite rows included in the plot. Use each family's
# own denominator; tied constants appear at 0.5.
diagnostic_predictor_data <- function(data, model, by_family, rank = NULL) {
  grouped <- is_gfam_family(family(model)) || inherits(model, "gratia_gjrm_diagnostic")
  if (!is.null(rank) && (!is.logical(rank) || length(rank) != 1L || is.na(rank)))
    stop("'rank' must be TRUE, FALSE, or NULL.", call. = FALSE)
  ranked <- if (is.null(rank)) grouped && !by_family else rank
  if (ranked) {
    included <- is.finite(data$eta) & is.finite(data$residuals)
    if (grouped) included <- included & !is.na(data$.family)
    groups <- if (grouped) data$.family else rep(1L, nrow(data))
    value <- rep(NA_real_, nrow(data))
    for (group in unique(groups[included])) {
      rows <- which(included & groups == group)
      value[rows] <- (base::rank(data$eta[rows], ties.method = "average") - .5) /
        length(rows)
    }
    data$eta <- value
  }
  attr(data, "ranked") <- ranked
  data
}


# Fitted values can differ by a few floating-point bits while displaying as the
# same number. Use a single tick for these panels.
gfam_observed_breaks <- function(data) {
  ranges <- lapply(split(data$fitted, data$.family, drop = TRUE), range)
  constant <- vapply(ranges, function(x)
    diff(x) <= sqrt(.Machine$double.eps) * max(1, abs(x)), logical(1))
  centres <- vapply(ranges, mean, numeric(1))
  # ggplot expands panel limits. Compare that expanded range with each family's
  # fitted range to recognize a constant panel and choose its central tick.
  expanded <- lapply(ranges, function(x) {
    width <- if (diff(x) == 0) 1 else diff(x)
    x + c(-.05, .05) * width
  })
  function(limits) {
    tolerance <- sqrt(.Machine$double.eps) * max(1, abs(limits))
    if (diff(range(limits)) <= tolerance) return(mean(limits))
    distance <- vapply(expanded, function(x) max(abs(x - limits)), numeric(1))
    closest <- which.min(distance)
    if (constant[closest] && distance[closest] <= tolerance &&
        centres[closest] >= min(limits) && centres[closest] <= max(limits))
      return(unname(centres[closest]))
    ticks <- scales::breaks_extended(n = 3L)(limits)
    ticks <- ticks[ticks >= min(limits) & ticks <= max(limits)]
    if (length(ticks) < 2L) {
      ticks <- scales::breaks_extended(n = 5L)(limits)
      ticks <- ticks[ticks >= min(limits) & ticks <= max(limits)]
    }
    if (length(ticks) < 2L) ticks <- seq(min(limits), max(limits), length.out = 3L)
    if (length(ticks) > 3L) ticks <- ticks[round(seq(1L, length(ticks), length.out = 3L))]
    unname(ticks)
  }
}

# Selection uses the original family-list positions, even for repeated families.
# Copies retain fitted coefficients and nuisance parameters; only observation
# slots and the getfl() index are restricted. The original fitted family is never changed.
diagnostic_response_selection <- function(model, response) {
  if (is.null(response)) return(invisible(model))
  source <- if (inherits(model, "gratia_diagnostics")) model$source else model
  grouped <- tryCatch(is_gfam_family(stats::family(source)), error = function(e) FALSE)
  joint <- inherits(diagnostic_original(source), "gjrm") &&
    !isTRUE(diagnostic_original(source)$univar.gamlss)
  if (!grouped && !joint) stop("'response' selection is available for grouped-family GAMs and joint GJRM diagnostics.",
    call. = FALSE)
  invisible(model)
}

gfam_select_diagnostics <- function(model, response = NULL, supplied = FALSE, seed = NULL) {
  original <- attr(model, "gratia.gfam.original", exact = TRUE)
  if (!supplied) return(model)
  diagnostic_response_selection(model, response)
  if (!is.null(original)) model <- original
  if (is.null(response)) {
    cached <- attr(model, "gratia.diagnostics")
    if (!is.null(original) && !is.null(seed) && !identical(cached$seed, seed))
      attr(model, "gratia.diagnostics") <- NULL
    return(model)
  }
  families <- stats::family(model)$getfl()
  if (!is.numeric(response) || length(response) != 1L || is.na(response) ||
      !is.finite(response) || response != floor(response) ||
      !response %in% seq_along(families))
    stop("'response' must be NULL or one integer family-list position in the model.", call. = FALSE)
  original <- model
  row_map <- which(!is.na(model_row_map(model)))
  model <- model_used_rows(model)
  index <- as.integer(attr(model$family$getfl(), "fi"))
  rows <- which(index == response)
  if (!length(rows)) stop("The selected response has no retained fitting observations.", call. = FALSE)
  n <- nrow(model$model)
  for (name in c("y", "fitted.values", "linear.predictors", "residuals", "weights",
      "prior.weights", "working.weights", "AR.start", "X")) {
    if (!is.null(model[[name]]) && NROW(model[[name]]) == n)
      model[[name]] <- slice_observations(model[[name]], rows)
  }
  model$model <- model$model[rows, , drop = FALSE]
  attr(model$model, "na.action") <- NULL
  subset_offset <- function(x) if (length(x) > 1L) slice_observations(x, rows) else x
  model$offset <- if (is.list(model$offset)) lapply(model$offset, subset_offset)
    else subset_offset(model$offset)
  model$na.action <- NULL
  attr(families, "fi") <- index[rows]
  model$family$getfl <- local({ value <- families; function() value })
  attr(model, "gratia.fitting.predictors") <- TRUE
  attr(model, "gratia.gfam.original") <- original
  attr(model, "gratia.gfam.response") <- as.integer(response)
  attr(model, "gratia.gfam.rows") <- row_map[rows]
  attr(model, "gratia.gfam.retained") <- rows
  attr(model, "gratia.diagnostics") <- NULL
  model
}

# Preserve the established component-order random draws when all components
# support the calculation. A supported selected component can still be checked
# when an unselected component lacks a CDF or response generator.
gfam_cache_selected_residuals <- function(model, type, seed = NULL, n_dharma = NULL) {
  original <- attr(model, "gratia.gfam.original", exact = TRUE)
  if (is.null(original) || !type %in% c("pit", "quantile", "dharma")) return(model)
  count <- if (type == "dharma") dharma_simulation_count(model, n_dharma) else NULL
  cached <- attr(model, "gratia.diagnostics")
  if (identical(cached$type, type) && (is.null(seed) || identical(cached$seed, seed)) &&
      (type != "dharma" || identical(cached$n_dharma, count))) return(model)
  full <- model_used_rows(original)
  full_cache <- attr(full, "gratia.diagnostics")
  if (!is.null(seed) && !identical(full_cache$seed, seed))
    attr(full, "gratia.diagnostics") <- NULL
  supported <- tryCatch({
    if (type == "dharma") {
      distribution <- response_distribution(full)
      is.function(distribution$rd %||% distribution$marginal_rd)
    } else !length(gfam_cdf_support(gfam_diagnostic_context(full)))
  }, error = function(e) FALSE)
  # A failure in an unselected component must not prevent checking the
  # selected component. Its own calculation below still reports invalid data
  # or unsupported operations rather than silently replacing residuals.
  residuals <- if (supported) tryCatch(
    diagnostic_residuals(full, type, seed, n_dharma = count),
    error = function(e) NULL) else NULL
  if (!is.null(residuals)) {
    attr(original, "gratia.diagnostics") <- list(type = type, residuals = residuals,
      seed = seed, n_dharma = count)
    attr(model, "gratia.gfam.original") <- original
    residuals <- residuals[attr(model, "gratia.gfam.retained")]
  } else {
    attr(model, "gratia.diagnostics") <- NULL
    residuals <- if (type == "dharma") model_dharma_residuals(model, count, seed) else
      diagnostic_seed(seed, gfam_quantile_residuals(gfam_diagnostic_context(model), type))
  }
  attr(model, "gratia.diagnostics") <- list(type = type, residuals = residuals,
    seed = seed, n_dharma = count)
  model
}
