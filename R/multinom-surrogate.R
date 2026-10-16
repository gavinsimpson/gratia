# Multinomial surrogate utilities use the full choice set, including category 0.
# A fitted multinom() stores K predictors, not K+1 response probabilities.
multinom_utility_locations <- function(model) {
  eta <- as.matrix(model$linear.predictors)
  k <- stats::family(model)$nlp
  if (ncol(eta) != k || any(!is.finite(eta)))
    stop("Multinomial surrogate residuals require finite fitted predictors.", call. = FALSE)
  out <- cbind(0, eta)
  colnames(out) <- as.character(0:k)
  out
}

# log(exp(a) + exp(b)), without exponentiating a large positive number.
multinom_logadd <- function(a, b) {
  hi <- pmax(a, b)
  hi + log1p(exp(pmin(a, b) - hi))
}

multinom_logsum <- function(locations) {
  hi <- locations[, 1L]
  for (j in seq_len(ncol(locations))[-1L]) hi <- pmax(hi, locations[, j])
  total <- numeric(nrow(locations))
  for (j in seq_len(ncol(locations)))
    total <- total + exp(locations[, j] - hi)
  hi + log(total)
}

# For U_j = mu_j + G_j, exp(-U_j) are independent exponentials
# with rates exp(mu_j). Their minimum T has rate sum(exp(mu_j)),
# and is independent of the winning label. Conditional on T and that label,
# the other exponential times are T plus independent exponentials with their
# original rates (the memoryless property). Thus M = -log(T) is Gumbel with
# location log-sum-exp(mu), even conditional on the observed winner.
# A losing centred error is -log(exp(mu_j-M) + E_j) - Euler's constant.
# Work on the log scale so even a winner with probability below exp(-1000)
# needs only one draw. Draw complete observation rows in order: changing the
# chunk size does not change the random numbers assigned to an observation.
multinom_surrogate_draw <- function(locations, y, chunk_size = 10000L) {
  chunk_size <- validate_diagnostic_bins(chunk_size, "chunk_size")
  n <- nrow(locations); jn <- ncol(locations)
  if (length(y) != n || anyNA(y) || any(y != round(y) | y < 0 | y >= jn) ||
      any(!is.finite(locations)))
    stop("Invalid multinomial utilities or observed categories.", call. = FALSE)
  out <- matrix(NA_real_, n, jn, dimnames = dimnames(locations))
  euler <- -digamma(1)
  for (first in seq.int(1L, n, by = chunk_size)) {
    rows <- seq.int(first, min(n, first + chunk_size - 1L))
    mu <- locations[rows, , drop = FALSE]
    loge <- log(matrix(stats::rexp(length(rows) * (jn + 1L)),
      nrow = length(rows), byrow = TRUE))
    maximum <- multinom_logsum(mu) - loge[, 1L]
    for (j in seq_len(jn)) {
      a <- mu[, j] - maximum
      error <- -multinom_logadd(a, loge[, j + 1L])
      winner <- y[rows] == j - 1L
      error[winner] <- -a[winner]
      out[rows, j] <- error - euler
    }
  }
  out
}

is_multinom_diagnostics <- function(model) {
  inherits(model, "gratia_diagnostics") &&
    identical(model$representation, "multinom utilities")
}

is_multinom_surrogate <- function(model, type = NULL) {
  if (inherits(model, "gratia_diagnostics")) return(is_multinom_diagnostics(model) &&
    (is.null(type) || identical(type, model$type)))
  type <- type %||% attr(model, "gratia.default_residual_type")
  identical(type, "surrogate") && identical(family_type(model), "multinom")
}

# Keep n rows and the residual matrix. In particular, scalable diagnostics do
# not allocate a full long table with repeated responses and plotting values.
multinom_diagnostic_data <- function(model, rows, action, seed) {
  r <- diagnostic_residuals(model, "surrogate", seed)
  mu <- multinom_utility_locations(model)
  model$na.action <- action
  structure(list(data = tibble::tibble(observed = model$y, .row = rows),
    residuals = r, locations = mu, log_total = multinom_logsum(mu),
    source = model, type = "surrogate", family = "multinom",
    representation = "multinom utilities", reference = "centred Gumbel",
    n_obs = nrow(mu), eta_label = "Fitted category probability"),
    class = "gratia_diagnostics")
}

# Every component uses its own fitted probability on the horizontal axis.
# This also gives category 0 a meaningful varying predictor, although its
# utility location is fixed at zero. Observed/fitted panels compare the
# category indicator with that same probability.
multinom_diagnostic_component <- function(d, j) {
  probability <- exp(d$locations[, j] - d$log_total)
  dat <- tibble::tibble(observed = as.numeric(d$data$observed == j - 1L),
    fitted = probability, eta = probability, residuals = d$residuals[, j],
    .row = d$data$.row, .response = 1L)
  new_diagnostic_data(dat, d$source, d$type, d$family, d$n_obs,
    eta_label = d$eta_label)
}

multinom_category <- function(data, d, j) {
  data$.family <- factor(rep(paste("Category", colnames(d$residuals)[j]), nrow(data)),
    levels = paste("Category", colnames(d$residuals)))
  data
}

centred_gumbel_quantile <- function(p) -log(-log(p)) + digamma(1)

# Each component has one independent observation per row under the known-
# parameter null. Beta order-statistic bands apply separately to each marginal;
# they are pointwise bands, not a simultaneous statement about all utilities.
multinom_qq_component <- function(d, j, level) {
  dat <- tibble::tibble(residuals = d$residuals[, j], .row = d$data$.row)
  dat <- dat[order(dat$residuals), , drop = FALSE]
  n <- nrow(dat); i <- seq_len(n); alpha <- (1 - level) / 2
  dat$theoretical <- centred_gumbel_quantile(stats::ppoints(n))
  dat$lower <- centred_gumbel_quantile(stats::qbeta(alpha, i, n + 1 - i))
  dat$upper <- centred_gumbel_quantile(stats::qbeta(1 - alpha, i, n + 1 - i))
  dat
}

multinom_qq_reference <- function(d, level, data = NULL) {
  if (is.null(data)) data <- dplyr::bind_rows(lapply(seq_len(ncol(d$residuals)),
    function(j) multinom_category(multinom_qq_component(d, j, level), d, j)))
  list(data = data, center = data$theoretical, intercept = 0, slope = 1,
    method = "centred Gumbel", subtitle = "Method: centred Gumbel",
    axis = "Centred Gumbel quantiles", by_family = TRUE, colour_family = FALSE)
}

multinom_qq_plot <- function(d, method = NULL, level = .9, qq_bins = 1000L,
    aggregate = FALSE, worm = FALSE, xlab = NULL, ylab = NULL, title = NULL,
    subtitle = NULL, caption = NULL, ci_col = "black", ci_alpha = .2,
    point_col = "grey20", point_alpha = 1, line_col = "red") {
  if (!is.null(method)) stop("Surrogate residuals use a fixed reference; use method = NULL.", call. = FALSE)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) stop("'level' must be between zero and one.", call. = FALSE)
  validate_diagnostic_bins(qq_bins, "qq_bins", minimum = 3L)
  parts <- lapply(seq_len(ncol(d$residuals)), function(j) {
    dat <- multinom_qq_component(d, j, level)
    n <- nrow(dat)
    # Rows identify observations only before aggregation. Never average IDs.
    if (aggregate && n > qq_bins) dat$.row <- NULL
    ref <- list(data = dat, center = dat$theoretical)
    dat <- bin_diagnostic_qq(ref,
      if (aggregate) qq_bins else max(3L, n), worm)
    multinom_category(dat, d, j)
  })
  df <- dplyr::bind_rows(parts)
  reference <- multinom_qq_reference(d, level, df)
  render_diagnostic_qq(df, reference, d$type, worm, xlab, ylab, title,
    subtitle, caption, ci_col, ci_alpha, point_col, point_alpha, line_col)
}

multinom_residual_plot <- function(d, panel, scalable = FALSE,
    density_bins = c(50L, 50L), density = "conditional", density_method = "kde",
    bandwidth = NULL, n_bins = "sturges", rank = NULL,
    xlab = NULL, ylab = NULL, title = NULL, subtitle = NULL, caption = NULL,
    point_col = "grey20", point_alpha = 1, line_col = "red") {
  if (!is.null(rank) && (!is.logical(rank) || length(rank) != 1L || is.na(rank)))
    stop("'rank' must be TRUE, FALSE, or NULL.", call. = FALSE)
  histogram <- panel == "histogram"
  observed <- panel == "observed"
  parts <- lapply(seq_len(ncol(d$residuals)), function(j) {
    dat <- multinom_diagnostic_component(d, j)$data
    if (histogram) {
      dat <- diagnostic_histogram(dat$residuals, n_bins)
      dat$value <- dat$count
    } else {
      if (isTRUE(rank) && !observed)
        dat$eta <- (base::rank(dat$eta, ties.method = "average") - .5) / nrow(dat)
      if (scalable) {
        dat <- if (!observed && density == "conditional" && density_method == "kde")
          diagnostic_density_grid(dat$eta, dat$residuals, density_bins, bandwidth) else
          diagnostic_grid(if (observed) dat$fitted else dat$eta,
            if (observed) dat$observed else dat$residuals, density_bins, density)
      }
    }
    multinom_category(dat, d, j)
  })
  df <- dplyr::bind_rows(parts)
  xlabel <- xlab %||% if (histogram) "Surrogate residuals" else
    if (isTRUE(rank) && !observed) "Within-category fitted probability rank" else d$eta_label
  ylabel <- ylab %||% if (histogram) "Frequency" else
    if (observed) "Observed category indicator" else "Surrogate residuals"
  heading <- title %||% if (histogram) "Histogram of residuals" else
    if (observed) "Observed vs fitted values" else "Residuals vs fitted category probability"
  if (histogram) return(render_grouped_histogram(df, TRUE,
    xlabel, ylabel, heading, subtitle, caption))
  if (scalable) return(render_diagnostic_grid(df, density, xlabel, ylabel,
    heading, subtitle, caption, intercept = if (!observed) 0 else NULL,
    line_col = line_col,
    fill_transform = if (!observed && density == "conditional" && density_method == "kde") "sqrt" else "identity"))
  render_diagnostic_points(df, if (observed) "fitted" else "eta",
    if (observed) "observed" else "residuals", xlabel, ylabel, heading,
    point_col, point_alpha, if (!observed) line_col else NULL) +
    ggplot2::facet_wrap(ggplot2::vars(.data$.family), scales = "free") +
    ggplot2::labs(subtitle = subtitle, caption = caption)
}

multinom_appraise <- function(d, method, level, qq_bins, scalable, worm,
    density_bins, density_method, bandwidth, n_bins, rank,
    ci_col, ci_alpha, point_col, point_alpha, line_col, ppcheck = NULL,
    ncol = NULL, nrow = NULL, guides = "keep", plot_args = list()) {
  qq <- multinom_qq_plot(d, method, level, qq_bins, scalable, worm,
    ci_col = ci_col, ci_alpha = ci_alpha, point_col = point_col,
    point_alpha = point_alpha, line_col = line_col)
  predictor <- multinom_residual_plot(d, "predictor", scalable,
    density_bins = density_bins, density_method = density_method,
    bandwidth = bandwidth, rank = rank, point_col = point_col,
    point_alpha = point_alpha, line_col = line_col)
  histogram <- ppcheck %||% multinom_residual_plot(d, "histogram", scalable, n_bins = n_bins)
  observed <- multinom_residual_plot(d, "observed", scalable,
    density_bins = density_bins, density = "count", point_col = point_col, point_alpha = point_alpha)
  layout <- prepare_plot_layout(4L, ncol = ncol, nrow = nrow)
  wrap_diagnostic_appraisal(qq, predictor, histogram, observed, byrow = TRUE,
    ncol = layout$ncol, nrow = layout$nrow, guides = guides, plot_args = plot_args)
}
