# Diagnostics use fitting rows, never a new-data membership vector. Component
# theta closures are reconstructed locally by gfam_components().
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
    context$mu[rows] <- context$families[[i]]$linkinv(context$eta[rows])
  }
  context
}

gfam_diagnostic_residuals <- function(context, y = context$y,
                                      type = "deviance") {
  out <- rep(NA_real_, length(y))
  for (i in seq_along(context$components)) {
    rows <- which(context$index == i)
    if (!length(rows)) next
    component <- context$components[[i]]
    family <- component$family
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
      # Beta and zero-inflated families adjust saturated log likelihoods in
      # their native residual hook; dev.resids alone is not a deviance.
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

diagnostic_residuals <- function(model, type, seed = NULL) {
  cached <- attr(model, "gratia.diagnostics")
  if (!is.null(cached) && identical(cached$type, type)) {
    return(restore_model_rows(cached$residuals, model))
  }
  if (type %in% c("pit", "quantile")) {
    return(quantile_residuals(model, type = type, seed = seed))
  }
  if (is_gfam_family(family(model))) {
    r <- gfam_diagnostic_residuals(gfam_diagnostic_context(model), type = type)
    return(restore_model_rows(r, model))
  }
  stats::residuals(model, type = type)
}

gfam_diagnostic_method <- function(model, method = NULL) {
  context <- gfam_diagnostic_context(model)
  active <- sort(unique(context$index))
  families <- context$families[active]
  qf <- vapply(families, function(f) is.function(fix_family_qf(f)$qf), logical(1))
  rd <- vapply(families, function(f) is.function(fix_family_rd(f)$rd), logical(1))
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

# Unsupported distributional configurations are distinct from invalid input or
# numerical CDF failures, which must surface as errors rather than trigger fallback.
gfam_cdf_support <- function(context) {
  known <- c("gaussian", "poisson", "binomial", "negative_binomial", "gamma",
    "beta_regression", "scaled_t", "tweedie", "cnorm", "clog", "cpois")
  active <- sort(unique(context$index))
  unsupported <- character()
  for (i in active) {
    f <- context$families[[i]]
    ft <- family_type(f)
    rows <- which(context$index == i)
    reason <- NULL
    if (!ft %in% known) reason <- "no validated response-CDF bounds"
    if (ft %in% known && !is.function(fix_family_cdf(f)$cdf)) reason <- "no CDF"
    if (ft == "gamma" && any(context$weights[rows] != 1))
      reason <- "non-unit Gamma weights"
    if (!is.null(reason)) unsupported <- c(unsupported,
      paste0(i, " <", f$family, "> (", reason, ")"))
  }
  unsupported
}

diagnostic_type <- function(model, type = NULL, allow_pit = FALSE) {
  if (is.null(type)) {
    if (!is_gfam_family(family(model))) return("deviance")
    unsupported <- gfam_cdf_support(gfam_diagnostic_context(model))
    if (!length(unsupported)) return("quantile")
    message("Using deviance residuals: quantile residuals are unavailable for ",
      "grouped component ", paste(unsupported, collapse = ", "), ".")
    return("deviance")
  }
  choices <- if (allow_pit) c("deviance", "pearson", "response", "pit", "quantile")
    else c("deviance", "response", "pearson", "quantile")
  match.arg(type, choices)
}

diagnostic_by_family <- function(model, type, by_family = NULL) {
  if (!is.null(by_family) && (!is.logical(by_family) || length(by_family) != 1L ||
      is.na(by_family))) stop("'by_family' must be TRUE, FALSE, or NULL.", call. = FALSE)
  is_gfam_family(family(model)) &&
    if (is.null(by_family)) !type %in% c("quantile", "pit") else by_family
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
  if ("lower_tail" %in% names(formals(cdf))) {
    function(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE) {
      check(cdf(q, mu, wt, scale, log_p = log_p, lower_tail = lower_tail), q, mu, log_p)
    }
  } else {
    function(q, mu, wt, scale, log_p = FALSE) {
      check(cdf(q, mu, wt, scale, log_p = log_p), q, mu, log_p)
    }
  }
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
    if (ft %in% c("poisson", "negative_binomial") &&
        any(y < 0 | abs(y - round(y)) > 1e-7))
      stop("Count components need non-negative integer responses.", call. = FALSE)
    if (!ft %in% c("cnorm", "clog", "cpois") && any(!is.finite(y)))
      stop("Invalid grouped-family responses.", call. = FALSE)
    if ((ft == "gamma" && any(y <= 0)) ||
        (ft == "beta_regression" && any(y <= 0 | y >= 1)) ||
        (ft == "tweedie" && any(y < 0)))
      stop("Grouped-family response is outside the distribution's support.", call. = FALSE)
    f$cdf <- gfam_checked_cdf(f$cdf, paste0(i, " <", f$family, ">"))
    value <- do_quantile_residuals(y, mu, wt, component$scale, f, type)
    if (any(!is.finite(value))) stop("Non-finite ", type,
      " residuals for grouped component ", i, " <", f$family,
      ">; check response support and CDF precision.", call. = FALSE)
    out[rows] <- value
  }
  out
}

# One compact vector is cached only on the local appraisal copy of a model.
# Every residual panel then sees precisely the same randomised observations.
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

calibrated_qq_data <- function(model, level, detrend, by_family) {
  used <- model_used_rows(model)
  r <- diagnostic_residuals(used, "quantile")
  data <- diagnostic_component_data(data.frame(residuals = r), used)
  data$.row <- seq_len(nrow(data))
  data <- data[is.finite(data$residuals), , drop = FALSE]
  if (!nrow(data)) stop("No finite quantile residuals are available.", call. = FALSE)
  parts <- if (by_family) split(data, data$.family, drop = TRUE) else list(data)
  dplyr::bind_rows(lapply(parts, function(part) {
    part <- part[order(part$residuals), , drop = FALSE]
    n <- nrow(part)
    i <- seq_len(n)
    theoretical <- stats::qnorm(stats::ppoints(n))
    alpha <- (1 - level) / 2
    part$theoretical <- theoretical
    part$lower <- stats::qnorm(stats::qbeta(alpha, i, n + 1 - i))
    part$upper <- stats::qnorm(stats::qbeta(1 - alpha, i, n + 1 - i))
    if (detrend) {
      part$residuals <- part$residuals - theoretical
      part$lower <- part$lower - theoretical
      part$upper <- part$upper - theoretical
    }
    part
  }))
}

calibrated_qq_plot <- function(model, method, level, detrend, by_family,
                               ylab, xlab, title, subtitle, caption, ci_col,
                               ci_alpha, point_col, point_alpha, line_col) {
  if (!is.null(method) && !identical(method, "normal")) {
    stop("Quantile residuals use a fixed standard-normal reference; use ",
      "'type = \"deviance\"' for uniform or simulated response references.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) stop("'level' must be between zero and one.", call. = FALSE)
  data <- calibrated_qq_data(model, level, detrend, by_family)
  plt <- ggplot2::ggplot(data, ggplot2::aes(x = .data$theoretical, y = .data$residuals))
  if (detrend) plt <- plt + ggplot2::geom_hline(yintercept = 0, colour = line_col)
  else plt <- plt + ggplot2::geom_abline(intercept = 0, slope = 1, colour = line_col)
  plt <- plt + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
    fill = ci_col, alpha = ci_alpha)
  if (".family" %in% names(data) && !by_family) {
    plt <- plt + ggplot2::geom_point(ggplot2::aes(colour = .data$.family), alpha = point_alpha) +
      ggokabeito::scale_colour_okabe_ito(limits = gfam_diagnostic_labels(model),
        drop = FALSE)
  } else plt <- plt + ggplot2::geom_point(colour = point_col, alpha = point_alpha)
  plt <- plt + ggplot2::labs(x = xlab %||% "Standard normal quantiles",
    y = ylab %||% if (detrend) "Quantile residuals (Deviation)" else "Quantile residuals",
    title = title %||% if (detrend) "Worm plot of residuals" else "QQ plot of residuals",
    subtitle = subtitle %||% "Fixed standard-normal reference; pointwise intervals",
    caption = caption)
  if (".family" %in% names(data) && !by_family)
    plt <- plt + ggplot2::labs(colour = "Family component")
  gfam_diagnostic_facets(plt, model, by_family)
}

# Keep the observed family mixture fixed, residualise each response using its
# own observation model, then pool sorted residuals exactly as for single families.
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
      part[[name]] <- context[[name]][rows]
    part$model$model <- context$model$model[rows, , drop = FALSE]
    for (name in c("y", "linear.predictors", "fitted.values", "weights",
        "prior.weights", "residuals"))
      part$model[[name]] <- context$model[[name]][rows]
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
  generators <- lapply(context$families, function(f) fix_family_rd(f)$rd)
  quantiles <- lapply(context$families, function(f) fix_family_qf(f)$qf)
  sims <- vapply(seq_len(n), function(j) {
    if (method == "simulate") {
      y <- gfam_response_draw(context$eta, context$weights, context, generators)
    } else {
      p <- sample(probabilities, nr)
      y <- rep(NA_real_, nr)
      for (i in seq_along(context$families)) {
        rows <- which(context$index == i)
        if (!length(rows)) next
        if (!is.function(quantiles[[i]])) {
          stop("Quantile function for grouped component ", i, " <",
            context$families[[i]]$family, "> not available.", call. = FALSE)
        }
        y[rows] <- quantiles[[i]](p[rows], context$mu[rows],
          context$weights[rows], context$components[[i]]$scale)
      }
    }
    sort(gfam_diagnostic_residuals(context, y, type))
  }, numeric(nr))
  sims <- matrix(sims, nrow = nr, ncol = n)
  theoretical <- if (method == "uniform") rowMeans(sims) else
    as.numeric(stats::quantile(sims, probabilities))
  out <- tibble::tibble(theoretical = theoretical,
    residuals = if (detrend) r - theoretical else r)
  if (method == "simulate") {
    alpha <- (1 - level) / 2
    bounds <- apply(sims, 1L, stats::quantile, probs = c(alpha, 1 - alpha))
    out$lower <- bounds[1L, ] - if (detrend) theoretical else 0
    out$upper <- bounds[2L, ] - if (detrend) theoretical else 0
  }
  out
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

# Ordinal fitted values are latent predictors, not scalar expected responses.
check_gfam_appraise <- function(model) {
  if (is_gfam_family(family(model)) && any(vapply(
      gfam_components(family(model)), function(x)
        startsWith(x$family$family, "Ordered Categorical"), logical(1)))) {
    stop("Grouped ordinal models do not yet support observed-versus-fitted ",
      "diagnostics; use qq_plot() or worm_plot() with deviance residuals.",
      call. = FALSE)
  }
}


# The rank denominator uses precisely the observations included in this plot,
# separately in each indexed component. Average ties give .5 for constants.
diagnostic_predictor_data <- function(data, model, by_family, rank = NULL) {
  grouped <- is_gfam_family(family(model))
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


# Numerically constant fits can have an extremely narrow (nonzero) range,
# leading default tick generators to print many identical rounded labels.
gfam_observed_breaks <- function(data) {
  ranges <- lapply(split(data$fitted, data$.family, drop = TRUE), range)
  constant <- vapply(ranges, function(x)
    diff(x) <= sqrt(.Machine$double.eps) * max(1, abs(x)), logical(1))
  centres <- vapply(ranges, mean, numeric(1))
  # Break callbacks receive expanded panel limits. Match those to retained
  # component ranges, so exactly constant fits also get one central tick.
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
