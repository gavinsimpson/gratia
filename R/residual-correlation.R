#' Ordered residual correlation diagnostics
#'
#' Compute lag correlations within independent series. These are descriptive
#' checks of ordered residual structure, with no independence-based confidence
#' bands or significance tests.
#'
#' @param model a supported fitted model.
#' @param type residual type. For an AR1 BAM the default is `"standardized"`,
#'   using mgcv's stored `std.rsd`. Otherwise the default is `"quantile"`.
#'   BAM also supports `"working"`, using weighted residuals from the final
#'   iteratively reweighted least-squares fit, and `"working.standardized"`,
#'   adjusting those residuals for the fitted AR1 correlation (whitening).
#'   Extended families are supported if they supply the expected deviance
#'   derivatives required for these working residuals.
#' @param lag_max positive integer; maximum observation lag.
#' @param series optional vector identifying series, in the order of observations
#'   used for fitting or of the original observations. BAM's `AR.start` boundaries are always
#'   retained, even if `series` is supplied. Other fitters need an explicit series
#'   and appropriately ordered fitting data when separate time series are present.
#' @param seed optional seed for randomized residuals.
#' @inheritParams appraise
#' @param ... arguments used to prepare the model for diagnostics, including
#'   `conditioning` and, for sdmTMB, `field_method`.
#' @return `residual_correlation()` returns a tibble with lag, correlation and
#'   the number of finite observation pairs used at each lag. `residual_correlation_plot()` returns a ggplot.
#' @details
#' Use `type = "dharma"` and `n_dharma` for simulation-based PIT residuals
#' on the uniform scale; see [dharma_residuals()].
#' Use `type = "surrogate"` for `ocat()` models to calculate correlations of
#' residuals on the latent logistic scale; see [surrogate_residuals()].
#'
#' BAM's generalized estimating equations (GEE) AR1 correction acts on the
#' weighted working response used for fitting. Whitening
#' these working residuals can reveal remaining serial structure. The stored
#' `std.rsd` in current mgcv adjusts its final residuals for AR1 correlation,
#' which need not be the weighted working residuals for non-Gaussian models.
#' Both checks are approximate. They can reveal remaining serial structure, but
#' do not specify a joint response distribution, establish normality or provide
#' calibrated goodness-of-fit tests.
#' Fitted parameters, leverage and discrete likelihoods affect correlations.
#'
#' Lags use observation positions within series, not elapsed time or spatial
#' distance. Means are removed within each series, and correlations use only
#' finite pairs at each lag; missing residuals and omitted fitting rows do not
#' close gaps. If series are interleaved, supply `series` for all original
#' observations so omitted rows can be assigned to the right series. If `series`
#' covers only observations used for fitting, an omitted position is counted
#' as a gap in every series with observations on both sides of it. Observations
#' assigned to another series never create gaps. Significance bands that assume
#' independent residuals are omitted. Check spatial
#' residual patterns with their coordinates and a suitable distance diagnostic.
#' @seealso [diagnostic_support], [qq_plot()]
#' @export
residual_correlation <- function(model, type = NULL, lag_max = 20L,
                                 series = NULL, seed = NULL, ..., n_dharma = 250L) {
  if (is.null(type) && inherits(model, "gratia_diagnostics")) type <- model$type
  if (!is.null(type) && !type %in% c("standardized", "working", "working.standardized"))
    type <- diagnostic_type(model, type)
  lag_max <- validate_diagnostic_bins(lag_max, "lag_max")
  prepared <- if (identical(type, "dharma"))
    diagnostic_ordered_residuals(model, type, seed,
      n_dharma = if (missing(n_dharma)) NULL else n_dharma, ...) else
    diagnostic_ordered_residuals(model, type, seed, ...)
  data <- prepared$data
  full_series <- NULL
  if (!is.null(series)) {
    original_length <- prepared$n_original %||% max(data$.row)
    if (length(series) == original_length || length(series) != nrow(data)) {
      if (length(series) < max(data$.row))
        stop("'series' must identify each fitting observation.", call. = FALSE)
      full_series <- series
      series <- series[data$.row]
    }
    data$.series <- interaction(data$.series, series, drop = TRUE)
  }
  positions <- ordered_residual_positions(data, full_series)
  out <- ordered_lag_correlations(data$residuals, data$.series, lag_max,
    positions = positions)
  attr(out, "description") <- prepared$description
  attr(out, "residual_type") <- prepared$type
  out
}

#' @rdname residual_correlation
#' @export
residual_correlation_plot <- function(model, type = NULL, lag_max = 20L,
                                      series = NULL, seed = NULL, ..., n_dharma = 250L) {
  data <- residual_correlation(model, type, lag_max, series, seed, n_dharma = if (missing(n_dharma)) NULL else n_dharma, ...)
  gratia_plot(data, ggplot2::aes(x = .data$lag, y = .data$correlation)) +
    geom_gratia_correlation_reference_hline(yintercept = 0, colour = "grey60") +
    geom_gratia_correlation_segment(ggplot2::aes(xend = .data$lag, yend = 0)) +
    geom_gratia_correlation_point() + ggplot2::labs(x = "Observation lag",
      y = "Residual lag correlation", title = "Ordered residual correlation")
}

diagnostic_ordered_residuals <- function(model, type = NULL, seed = NULL, ...) {
  UseMethod("diagnostic_ordered_residuals")
}
#' @export
diagnostic_ordered_residuals.default <- function(model, type = NULL, seed = NULL, ...) {
  type <- type %||% if (inherits(model, "gratia_diagnostics")) model$type else "quantile"
  prepared <- diagnostic_data(model, type = type, seed = seed, ...)
  if (is_multinom_diagnostics(prepared)) stop(
    "For multinomial surrogate utilities, inspect each matrix column separately for ordered correlations.", call. = FALSE)
  data <- prepared$data
  compact_rows <- unique(data$.row)
  rows <- ordered_model_rows(prepared$source, prepared$n_obs, compact_rows)
  data$.row <- rows$rows[match(data$.row, compact_rows)]
  data$.series <- data$.response
  list(data = data, type = type, n_original = rows$n_original,
    description = diagnostic_reference_description(response_distribution(prepared$source),
      paste(if (type == "dharma") "DHARMa-style" else type,
        "residuals; descriptive correlation; no significance bands")))
}
#' @export
diagnostic_ordered_residuals.gam <- function(model, type = NULL, seed = NULL, ...) {
  ar <- !is.null(model$AR1.rho) && model$AR1.rho != 0
  type <- type %||% if (ar) "standardized" else "quantile"
  if (!type %in% c("standardized", "working", "working.standardized")) {
    out <- diagnostic_ordered_residuals.default(model, type, seed, ...)
  } else {
    used <- model_used_rows(model)
    if (type == "standardized") {
      r <- used$std.rsd
      if (is.null(r)) stop("No native standardized AR1 residuals are stored.", call. = FALSE)
    } else {
      r <- bam_weighted_working_residuals(used)
      if (type == "working.standardized")
        r <- ar1_whiten(r, used$AR1.rho %||% 0, used$model[["(AR.start)"]])
    }
    rows <- ordered_model_rows(model, length(r))
    out <- list(data = data.frame(residuals = unname(r),
      .row = rows$rows, .series = 1L), type = type, n_original = rows$n_original,
      description = paste("Approximate BAM", type,
        "residual correlation; no significance bands or joint response claim"))
  }
  if (ar) {
    start <- model$model[["(AR.start)"]] %||% rep(FALSE, nrow(out$data))
    start[1L] <- TRUE
    out$data$.series <- cumsum(start)
  }
  out
}

# Reconstruct residuals of BAM's final weighted working response. The stored
# residuals(type = 'working') can instead contain deviance residuals after fitting.
bam_weighted_working_residuals <- function(model) {
  family <- stats::family(model)
  mu <- model$fitted.values
  if (is_gfam_family(family)) stop(
    "Weighted working residual correlation is unavailable for indexed families; use standardized residuals.", call. = FALSE)
  if (inherits(family, "extended.family")) {
    derivatives <- getFromNamespace("dDeta", "mgcv")(
      model$y, mu, model$prior.weights, family$getTheta(), family)
    curvature <- derivatives$EDeta2
    out <- -derivatives$Deta / sqrt(2 * pmax(curvature, 0))
    out[!is.finite(curvature) | curvature <= 0] <- NA_real_
    return(out)
  }
  if (!is.function(family$variance) || !is.function(family$mu.eta)) stop(
    "Weighted working residuals are unavailable for this family.", call. = FALSE)
  sign(family$mu.eta(model$linear.predictors)) * (model$y - mu) *
    sqrt(model$prior.weights / family$variance(mu))
}

ar1_whiten <- function(values, rho, start = NULL) {
  start <- start %||% rep(FALSE, length(values))
  start[1L] <- TRUE
  out <- values
  rows <- which(!start)
  out[rows] <- (values[rows] - rho * values[rows - 1L]) / sqrt(1 - rho^2)
  out
}

ordered_lag_correlations <- function(values, series, lag_max, positions = NULL) {
  groups <- split(seq_along(values), series, drop = TRUE)
  if (is.null(positions)) {
    positions <- integer(length(values))
    for (rows in groups) positions[rows] <- seq_along(rows)
  }
  centered <- values
  for (rows in groups) centered[rows] <- values[rows] - mean(values[rows], na.rm = TRUE)
  dplyr::bind_rows(lapply(seq.int(0L, lag_max), function(lag) {
    pairs <- lapply(groups, function(rows) {
      target <- match(positions[rows] + lag, positions[rows])
      valid <- !is.na(target)
      cbind(rows[valid], rows[target[valid]])
    })
    pairs <- do.call(rbind, pairs)
    x <- centered[pairs[, 1L]]; y <- centered[pairs[, 2L]]
    valid <- is.finite(x) & is.finite(y)
    correlation <- if (sum(valid) >= 2L && stats::sd(x[valid]) > 0 &&
      stats::sd(y[valid]) > 0) stats::cor(x[valid], y[valid]) else NA_real_
    tibble::tibble(lag = lag, correlation = correlation, n_pairs = sum(valid))
  }))
}


# Keep gaps from omitted observations for both na.omit and na.exclude. Public
# residual methods retain their existing rules about restoring omitted rows.
ordered_model_rows <- function(model, n, fallback = seq_len(n)) {
  action <- stats::na.action(model)
  if (is.null(action)) return(list(rows = fallback, n_original = max(fallback)))
  omitted <- as.integer(action)
  original <- seq_len(n + length(omitted))
  list(rows = original[!original %in% omitted], n_original = length(original))
}

ordered_residual_positions <- function(data, full_series = NULL) {
  if (!is.null(full_series)) {
    positions <- integer(length(full_series))
    for (rows in split(seq_along(full_series), full_series, drop = TRUE))
      positions[rows] <- seq_along(rows)
    return(positions[data$.row])
  }
  # Rows assigned to another series must not create gaps. If an omitted row's
  # series is unknown, count a gap in each series with observations on both sides.
  omitted <- setdiff(seq_len(max(data$.row)), unique(data$.row))
  gaps <- findInterval(data$.row, omitted)
  positions <- integer(nrow(data))
  for (rows in split(seq_len(nrow(data)), data$.series, drop = TRUE))
    positions[rows] <- seq_along(rows) + gaps[rows]
  positions
}
