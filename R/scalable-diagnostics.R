# Internal extension contract: diagnostic_data() returns gratia_diagnostics,
# containing a compact, fitting-order table (data), the model used for QQ
# references (source), retained observation count (n_obs), type and family.
# data has observed, fitted, eta, residuals, .row and .response columns.
# A new model implements diagnostic_data.<class>() and, if it cannot use the
# existing GAM QQ calculations, diagnostic_qq_data.<source class>(). The latter
# returns list(data, method, intercept, slope, center); data contains theoretical,
# residuals and optional lower/upper, and center is the worm reference per row.
# Aggregators and renderers never inspect model classes. These S3 generics are
# deliberately internal until the extension contract has broader model coverage.
diagnostic_data <- function(model, type = NULL, seed = NULL) {
  UseMethod("diagnostic_data")
}

#' @export
diagnostic_data.default <- function(model, type = NULL, seed = NULL) {
  stop(
    "Diagnostic data are not available for <",
    class(model)[1L],
    ">.",
    call. = FALSE
  )
}

#' @export
diagnostic_data.gratia_diagnostics <- function(
  model,
  type = NULL,
  seed = NULL
) {
  if (!is.null(type) && !identical(type, model$type)) {
    stop(
      "Prepared diagnostic data use type = '",
      model$type,
      "'.",
      call. = FALSE
    )
  }
  model
}

diagnostic_seed <- function(seed, code) {
  if (is.null(seed)) force(code) else withr::with_seed(seed, code)
}

new_diagnostic_data <- function(
  data,
  source,
  type,
  family,
  n_obs,
  eta_label = "Linear predictor"
) {
  if (
    !all(
      c("observed", "fitted", "eta", "residuals", ".row", ".response") %in%
        names(data)
    )
  ) {
    stop("Incomplete diagnostic data.", call. = FALSE)
  }
  structure(
    list(
      data = data,
      source = source,
      type = type,
      family = family,
      n_obs = n_obs,
      eta_label = eta_label
    ),
    class = "gratia_diagnostics"
  )
}

#' @export
diagnostic_data.gam <- function(model, type = NULL, seed = NULL) {
  type <- diagnostic_type(model, type, allow_pit = TRUE)
  model <- model_used_rows(model)
  r <- diagnostic_residuals(model, type = type, seed = seed)
  if (type == "quantile") {
    attr(model, "gratia.diagnostics") <- list(type = type, residuals = r)
  }
  y <- model$y
  if (is.null(y)) {
    y <- stats::model.response(stats::model.frame(model))
  }
  grouped <- is_gfam_family(stats::family(model))
  fit <- if (grouped) {
    context <- gfam_diagnostic_context(model)
    if (
      any(vapply(
        context$families,
        function(f) startsWith(f$family, "Ordered Categorical"),
        logical(1)
      ))
    ) {
      context$mu
    } else {
      as.numeric(gfam_response_means(matrix(context$eta, ncol = 1L), context))
    }
  } else {
    stats::fitted(model)
  }
  eta <- model$linear.predictors
  if (is.null(eta)) {
    eta <- fit
  }
  n <- NROW(stats::model.frame(model))
  # Scalar residuals in distributional models describe the response, while
  # fitted/eta matrices also contain scale and shape predictors.
  nr <- if (is.matrix(r)) NCOL(r) else 1L
  first <- function(x) if (is.matrix(x) && nr == 1L) x[, 1L] else as.vector(x)
  fit <- first(fit)
  eta <- first(eta)
  if (is.matrix(y) && nr == 1L) {
    if (identical(family_type(model), "binomial")) {
      y <- y[, 1L] / rowSums(y)
    } else {
      y <- y[, 1L]
    }
  }
  if (is.factor(y)) {
    y <- as.numeric(y != levels(y)[1L])
  }
  y <- as.vector(y)
  r <- as.vector(r)
  if (any(lengths(list(y, fit, eta)) != length(r))) {
    stop(
      "Response, fitted values, predictors and residuals are not aligned.",
      call. = FALSE
    )
  }
  dat <- tibble::tibble(
    observed = y,
    fitted = fit,
    eta = eta,
    residuals = r,
    .row = rep(seq_len(n), nr),
    .response = rep(seq_len(nr), each = n)
  )
  dat <- diagnostic_component_data(dat, model)
  new_diagnostic_data(dat, model, type, stats::family(model)$family, n)
}

#' @export
diagnostic_data.lm <- function(model, type = NULL, seed = NULL) {
  model <- model_used_rows(model)
  if (inherits(model, "glm")) {
    if (is.null(model$sig2)) model$sig2 <- summary(model)$dispersion
  } else {
    r <- stats::residuals(model)
    model$sig2 <- sum((r - mean(r))^2) / stats::df.residual(model)
  }
  if (is.null(model$prior.weights)) {
    model$prior.weights <- diagnostic_weights(model)
  }
  if (is.null(model$linear.predictors)) {
    model$linear.predictors <- model$fitted.values
  }
  diagnostic_data.gam(model, type, seed)
}

#' @export
diagnostic_data.glm <- diagnostic_data.lm

#' @export
diagnostic_data.gamm4 <- function(model, type = NULL, seed = NULL) {
  diagnostic_data.gam(gamm4_gam(model), type, seed)
}

#' @export
diagnostic_data.glmmTMB <- function(model, type = NULL, seed = NULL) {
  if (is.null(type)) {
    type <- "pearson"
  }
  diagnostic_data(glmmtmb_diagnostic_model(model, type), type, seed)
}

#' @export
diagnostic_data.gratia_glmmtmb_diagnostic <- function(
  model,
  type = NULL,
  seed = NULL
) {
  if (is.null(type)) {
    type <- "pearson"
  }
  diagnostic_data.gam(model, type, seed)
}

#' @export
diagnostic_data.gamlss <- function(model, type = NULL, seed = NULL) {
  if (!inherits(model, "SemiParBIV") || !isTRUE(model$univar.gamlss)) {
    stop("Only univariate GJRM::gamlss() models are supported.", call. = FALSE)
  }
  fam <- model$margins[1L]
  if (
    !fam %in% c("N", "LN", "GA", "GAi", "P", "NBI", "NBII") ||
      isTRUE(model$surv.flex) ||
      isTRUE(model$VC$surv) ||
      (!is.null(model$cens) && any(model$cens != 1))
  ) {
    stop(
      "GJRM diagnostics require an uncensored N, LN, GA, GAi, P, NBI or NBII model.",
      call. = FALSE
    )
  }
  if (is.null(type)) {
    type <- "quantile"
  }
  if (!identical(type, "quantile")) {
    stop(
      "GJRM diagnostics currently support type = 'quantile' only.",
      call. = FALSE
    )
  }
  eta <- as.vector(model$eta1)
  fit <- switch(
    fam,
    N = eta,
    GAi = eta,
    LN = exp(eta + as.vector(model$sigma2)^2 / 2),
    exp(eta)
  )
  p <- as.vector(model$fit$p)
  if (fam %in% c("P", "NBI", "NBII")) {
    mass <- as.vector(model$fit$pdf2)
    if (length(mass) != length(p)) {
      stop("GJRM fitted probability masses are unavailable.")
    }
    p <- diagnostic_seed(seed, stats::runif(length(p), pmax(0, p - mass), p))
  }
  n <- length(model$y1)
  if (any(lengths(list(p, eta, fit)) != n)) {
    stop(
      "GJRM fitted distribution is not aligned with its response.",
      call. = FALSE
    )
  }
  dat <- tibble::tibble(
    observed = as.vector(model$y1),
    fitted = fit,
    eta = eta,
    residuals = stats::qnorm(p),
    .row = seq_len(n),
    .response = 1L
  )
  new_diagnostic_data(
    dat,
    model,
    type,
    fam,
    n,
    eta_label = "Linear predictor (mu)"
  )
}

# Source-specific reference calculation. Existing numerical methods are kept.
diagnostic_qq_data <- function(
  model,
  diagnostics,
  method = NULL,
  n_uniform = 10,
  n_simulate = 50,
  level = 0.9,
  by_family = NULL
) {
  UseMethod("diagnostic_qq_data")
}

#' @export
diagnostic_qq_data.default <- function(
  model,
  diagnostics,
  method = NULL,
  n_uniform = 10,
  n_simulate = 50,
  level = 0.9,
  by_family = NULL
) {
  type <- diagnostics$type
  by_family <- diagnostic_data_by_family(diagnostics, by_family)
  grouped <- ".family" %in% names(diagnostics$data)
  if (type == "quantile") {
    if (!is.null(method) && !identical(method, "normal")) {
      stop(
        "Quantile residuals use a fixed standard-normal reference.",
        call. = FALSE
      )
    }
    df <- calibrated_qq_data(model, level, FALSE, by_family)
    return(list(
      data = df,
      method = "normal",
      intercept = 0,
      slope = 1,
      center = df$theoretical,
      by_family = by_family,
      colour_family = grouped && !by_family
    ))
  }
  if (!type %in% c("deviance", "response", "pearson")) {
    stop(
      "QQ references for this model require deviance, response, pearson or quantile residuals.",
      call. = FALSE
    )
  }
  method <- diagnostic_method(model, method)
  if (
    !grouped &&
      method == "uniform" &&
      is.null(fix_family_qf(stats::family(model))$qf)
  ) {
    method <- "simulate"
  }
  if (
    !grouped &&
      method == "simulate" &&
      is.null(fix_family_rd(stats::family(model))$rd)
  ) {
    method <- "normal"
  }
  df <- switch(
    method,
    uniform = qq_uniform(
      model,
      n = n_uniform,
      type = type,
      by_family = by_family
    ),
    simulate = qq_simulate(
      model,
      n = n_simulate,
      type = type,
      level = level,
      by_family = by_family
    ),
    normal = qq_normal(model, type = type, level = level, by_family = by_family)
  )
  intercept <- if (method == "normal") stats::median(df$residuals) else 0
  slope <- if (method == "normal") stats::IQR(df$residuals) / 1.349 else 1
  center <- if (method != "normal") {
    df$theoretical
  } else {
    if (".qq_intercept" %in% names(df)) {
      df$.qq_intercept + df$.qq_slope * df$theoretical
    } else {
      intercept + slope * df$theoretical
    }
  }
  list(
    data = df,
    method = method,
    intercept = intercept,
    slope = slope,
    center = center,
    by_family = by_family,
    colour_family = FALSE
  )
}

#' @export
diagnostic_qq_data.gratia_glmmtmb_diagnostic <- function(
  model,
  diagnostics,
  method = NULL,
  n_uniform = 10,
  n_simulate = 50,
  level = 0.9,
  by_family = NULL
) {
  method <- glmmtmb_qq_method(method)
  if (is.null(model$family$rd)) {
    model$family <- glmmtmb_family(model$source)
  }
  diagnostic_qq_data.default(
    model,
    diagnostics,
    method,
    n_uniform,
    n_simulate,
    level,
    by_family
  )
}

#' @export
diagnostic_qq_data.gamlss <- function(
  model,
  diagnostics,
  method = NULL,
  n_uniform = 10,
  n_simulate = 50,
  level = 0.9,
  by_family = NULL
) {
  if (!is.null(method) && !identical(method, "normal")) {
    stop(
      "GJRM quantile residuals support method = 'normal' only.",
      call. = FALSE
    )
  }
  r <- diagnostics$data$residuals
  r <- diagnostic_finite(tibble::tibble(residuals = r), "residuals")$residuals
  n <- length(r)
  i <- seq_len(n)
  alpha <- (1 - level) / 2
  df <- tibble::tibble(
    theoretical = stats::qnorm(stats::ppoints(n)),
    residuals = sort(r),
    lower = stats::qnorm(stats::qbeta(alpha, i, n + 1 - i)),
    upper = stats::qnorm(stats::qbeta(1 - alpha, i, n + 1 - i))
  )
  list(
    data = df,
    method = "normal",
    intercept = 0,
    slope = 1,
    center = df$theoretical
  )
}

validate_diagnostic_bins <- function(x, name, length = 1L, minimum = 1L) {
  if (
    !is.numeric(x) ||
      !length(x) %in% length ||
      anyNA(x) ||
      any(!is.finite(x)) ||
      any(x < minimum | x != floor(x)) ||
      any(x > .Machine$integer.max)
  ) {
    stop(
      "'",
      name,
      "' must contain ",
      paste(length, collapse = " or "),
      " integer(s) >= ",
      minimum,
      ".",
      call. = FALSE
    )
  }
  as.integer(x)
}

diagnostic_scalable <- function(scalable, scalable_threshold, n_obs) {
  validate_diagnostic_bins(scalable_threshold, "scalable_threshold")
  if (is.null(scalable)) {
    return(n_obs > scalable_threshold)
  }
  if (!is.logical(scalable) || length(scalable) != 1L || is.na(scalable)) {
    stop("'scalable' must be NULL, TRUE or FALSE.", call. = FALSE)
  }
  scalable
}

diagnostic_finite <- function(data, columns) {
  keep <- Reduce(`&`, lapply(data[columns], is.finite))
  if (!any(keep)) {
    stop("No finite observations available for this diagnostic.", call. = FALSE)
  }
  if (any(!keep)) {
    warning(
      "Dropped ",
      sum(!keep),
      " non-finite observation(s) from diagnostic.",
      call. = FALSE
    )
  }
  data[keep, , drop = FALSE]
}

# Bin in undetrended geometry, reserving the extreme points. Zero arc-length
# interiors collapse to one bin; empty arc-length bins are never materialized.
bin_diagnostic_qq <- function(reference, qq_bins = 1000L, worm = FALSE) {
  qq_bins <- validate_diagnostic_bins(qq_bins, "qq_bins", minimum = 3L)
  df <- reference$data
  if (".family" %in% names(df)) {
    parts <- lapply(
      split(seq_len(nrow(df)), df$.family, drop = TRUE),
      function(rows) {
        ref <- reference
        ref$data <- df[rows, setdiff(names(df), ".family"), drop = FALSE]
        ref$center <- reference$center[rows]
        out <- bin_diagnostic_qq(ref, qq_bins, worm)
        out$.family <- df$.family[rows[1L]]
        out
      }
    )
    return(dplyr::bind_rows(parts))
  }
  df$.center <- reference$center
  df <- diagnostic_finite(
    df,
    intersect(
      names(df),
      c("theoretical", "residuals", "lower", "upper", ".center")
    )
  )
  n <- nrow(df)
  if (n > qq_bins) {
    scale <- max(abs(c(df$theoretical, df$residuals)))
    if (scale == 0) {
      scale <- 1
    }
    h <- c(
      0,
      cumsum(sqrt(
        diff(df$theoretical / scale)^2 + diff(df$residuals / scale)^2
      ))
    )
    inside <- 2L:(n - 1L)
    span <- h[n] - h[1L]
    group <- if (span == 0) {
      rep(1L, length(inside))
    } else {
      pmin(qq_bins - 2L, floor(h[inside] / span * (qq_bins - 2L)) + 1L)
    }
    # rowsum and tabulate are linear in n; no per-bin scans of the observations.
    sums <- rowsum(
      as.matrix(df[inside, , drop = FALSE]),
      group,
      reorder = FALSE
    )
    counts <- tabulate(match(group, unique(group)))
    mid <- tibble::as_tibble(sums / counts)
    # These coefficients are constant within a component. Preserve them exactly
    # so summation round-off cannot create duplicate reference lines per bin.
    for (nm in intersect(c(".qq_intercept", ".qq_slope"), names(mid))) {
      mid[[nm]] <- df[[nm]][1L]
    }
    df <- rbind(df[1L, ], mid, df[n, ])
  }
  if (worm) {
    df$residuals <- df$residuals - df$.center
    if ("lower" %in% names(df)) {
      df$lower <- df$lower - df$.center
      df$upper <- df$upper - df$.center
    }
  }
  df$.center <- NULL
  df
}

# All ggplot mappings are made in renderer environments that contain only
# compact summaries, never the source model or an observation-level closure.
render_diagnostic_qq <- function(
  df,
  reference,
  type,
  worm = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ci_col = "black",
  ci_alpha = 0.2,
  point_col = "grey20",
  point_alpha = 1,
  line_col = "red"
) {
  # Retain scalar reference metadata only in the plotting environment.
  intercept <- reference$intercept
  slope <- reference$slope
  method <- reference$method
  by_family <- isTRUE(reference$by_family)
  colour_family <- isTRUE(reference$colour_family)
  reference <- NULL
  plt <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$theoretical, y = .data$residuals)
  )
  plt <- plt +
    if (worm) {
      ggplot2::geom_hline(yintercept = 0, colour = line_col)
    } else if (by_family && ".qq_slope" %in% names(df)) {
      ggplot2::geom_abline(
        data = unique(df[c(".family", ".qq_slope", ".qq_intercept")]),
        ggplot2::aes(slope = .data$.qq_slope, intercept = .data$.qq_intercept),
        colour = line_col
      )
    } else {
      ggplot2::geom_abline(
        intercept = intercept,
        slope = slope,
        colour = line_col
      )
    }
  if ("lower" %in% names(df)) {
    plt <- plt +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        fill = ci_col,
        alpha = ci_alpha
      )
  }
  if (colour_family) {
    plt <- plt +
      ggplot2::geom_point(
        ggplot2::aes(colour = .data$.family),
        alpha = point_alpha
      ) +
      ggokabeito::scale_colour_okabe_ito(
        limits = levels(df$.family),
        drop = FALSE
      ) +
      ggplot2::labs(colour = "Family component")
  } else {
    plt <- plt + ggplot2::geom_point(colour = point_col, alpha = point_alpha)
  }
  if (by_family) {
    plt <- plt +
      ggplot2::facet_wrap(ggplot2::vars(.data$.family), scales = "free")
  }
  plt <- plt +
    ggplot2::labs(
      x = if (is.null(xlab)) "Theoretical quantiles" else xlab,
      y = if (is.null(ylab)) {
        paste(
          stringr::str_to_title(type),
          "residuals",
          if (worm) "(Deviation)" else ""
        )
      } else {
        ylab
      },
      title = if (is.null(title)) {
        if (worm) "Worm plot of residuals" else "QQ plot of residuals"
      } else {
        title
      },
      subtitle = if (is.null(subtitle)) paste("Method:", method) else subtitle,
      caption = caption
    )
  compact_diagnostic_plot(plt)
}

#' Scalable model diagnostic plots
#'
#' Aggregate all observations before drawing model diagnostic plots with ggplot.
#' QQ curves use equal-arc-length bins with their endpoints preserved. Heatmaps
#' and histograms contain precomputed counts, so redrawing does not re-bin the
#' original data. The existing QQ reference calculations are retained: their
#' simulation matrices can still require memory proportional to observations
#' times simulations. Binning reduces rendering cost, not that calculation cost.
#'
#' @param model A fitted mgcv GAM, lm, glm, gamm4, glmmTMB, or supported
#'   univariate GJRM::gamlss model.
#' @param qq_bins Maximum number of QQ points per family component, including both endpoints;
#'   an integer of at least three.
#' @param density_bins Positive integer or pair of integers giving the number
#'   of horizontal and vertical heatmap bins.
#' @param density One of `"conditional"` or `"count"`. Conditional density is
#'   estimated using `density_method`. With `"histogram"`, it is the count
#'   divided by the horizontal-bin total and vertical-bin width. Empty
#'   horizontal bins are blank. Counts are always unsmoothed.
#' @param density_method Conditional-density estimator: `"kde"` (the default)
#'   uses [KernSmooth::bkde2D()] with Gaussian kernels; `"histogram"` uses
#'   unsmoothed rectangular bins. Only affects the residual heatmap.
#' @param bandwidth Optional pair of positive bandwidths for the predictor and
#'   residual axes, in their plotted units. `NULL` uses normal-reference
#'   bandwidths, adjusted for two dimensions, with a fallback for constant axes.
#'   Used only for `density_method = "kde"` and conditional density.
#' @param type Residual type; `NULL` follows [appraise()] defaults for mgcv, lm,
#'   glm and gamm4, selects Pearson for glmmTMB, and quantile for GJRM. The residual heatmap and
#'   histogram also accept PIT and quantile residuals where available.
#' @inheritParams qq_plot
#' @param level Coverage of pointwise QQ reference intervals, strictly between
#'   zero and one. Used by simulation and normal-reference methods.
#' @param xlab,ylab Axis labels; `NULL` selects suitable defaults.
#' @param title,subtitle,caption Plot title, subtitle and caption, passed to
#'   [ggplot2::labs()].
#' @param seed Random seed for QQ calculations or randomized residuals. Explicit
#'   seeds preserve the caller's RNG state, including an absent state.
#' @inheritParams residuals_hist_plot
#' @inheritParams residuals_linpred_plot
#' @param ... For `scalable_worm_plot()`, arguments passed to
#'   `scalable_qq_plot()` (except its internal worm setting).
#' @return A ggplot object containing aggregated plotting data.
#' @details GJRM support covers uncensored univariate N, LN, GA, GAi, P, NBI,
#'   and NBII models. Only `type = "quantile"` and `method = "normal"` are
#'   supported. Discrete CDF jumps are randomized; their accuracy is limited by
#'   GJRM's stored fitted probabilities. QQ bands are pointwise standard-normal
#'   order-statistic reference intervals conditional on the fitted distribution,
#'   without accounting for parameter estimation. Observed-versus-fitted plots
#'   use response means, including the lognormal mean correction. Residuals are
#'   plotted against the first distributional linear predictor.
#'
#'   Grouped-family models retain the residual defaults, component reference
#'   distributions, predictor ranks and layout controls of [appraise()]. QQ
#'   bin limits apply separately to each indexed component. Pooled residual
#'   heatmaps summarize the component mixture rather than colouring individual
#'   observations; observed-versus-fitted appraisals always separate components.
#'   Pooled histograms use faint component-coloured fills with opaque stepped
#'   upper boundaries, retaining common bin edges and component densities.
#'
#'   Kernel density estimates are calculated on a fixed grid before plotting.
#'   Each predictor slice is normalized to integrate to one over the displayed
#'   residual grid. Bandwidths, rather than `density_bins`, control smoothing;
#'   increase grid resolution when using small bandwidths. A square-root fill
#'   transformation reveals low-density regions while retaining density units
#'   in the legend. Gaussian kernels smooth discrete residual bands, which can
#'   exist even under a correctly specified count model; they do not calibrate
#'   traditional residuals to a common reference distribution.
#'
#'   Point styling affects QQ and worm points. Heatmap fill scales can be
#'   replaced with ggplot scales. Non-finite plotting observations are reported
#'   and omitted; fitting exclusions do not count as dropped observations.
#' @references Fasiolo, M., Nedellec, R., Goude, Y. and Wood, S. N. (2020).
#'   Scalable Visualization Methods for Modern Generalized Additive Models.
#'   Journal of Computational and Graphical Statistics, 29, 78-86.
#'   doi:10.1080/10618600.2019.1629942.
#' @seealso [appraise()], [qq_plot()], [worm_plot()]
#' @export
#' @examples
#' library("mgcv")
#' dat <- data_sim("eg1", n = 400, seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
#' scalable_qq_plot(m, seed = 42)
#' scalable_residuals_linpred_plot(m)
#' scalable_observed_fitted_plot(m, density_bins = c(30, 40))
#' appraise(m, scalable = TRUE, seed = 42)
scalable_qq_plot <- function(
  model,
  method = NULL,
  type = NULL,
  n_uniform = 10,
  n_simulate = 50,
  seed = NULL,
  level = 0.9,
  qq_bins = 1000L,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ci_col = "black",
  ci_alpha = 0.2,
  point_col = "grey20",
  point_alpha = 1,
  line_col = "red",
  by_family = NULL
) {
  scalable_qq(
    model,
    method,
    type,
    n_uniform,
    n_simulate,
    seed,
    level,
    qq_bins,
    FALSE,
    xlab,
    ylab,
    title,
    subtitle,
    caption,
    ci_col,
    ci_alpha,
    point_col,
    point_alpha,
    line_col,
    by_family = by_family
  )
}

#' @rdname scalable_qq_plot
#' @export
scalable_worm_plot <- function(model, ...) {
  scalable_qq(model, ..., worm = TRUE)
}

scalable_qq <- function(
  model,
  method = NULL,
  type = NULL,
  n_uniform = 10,
  n_simulate = 50,
  seed = NULL,
  level = 0.9,
  qq_bins = 1000L,
  worm = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ci_col = "black",
  ci_alpha = 0.2,
  point_col = "grey20",
  point_alpha = 1,
  line_col = "red",
  aggregate = TRUE,
  by_family = NULL
) {
  validate_diagnostic_bins(qq_bins, "qq_bins", minimum = 3L)
  validate_diagnostic_bins(n_uniform, "n_uniform")
  validate_diagnostic_bins(n_simulate, "n_simulate")
  if (
    !is.numeric(level) ||
      length(level) != 1L ||
      !is.finite(level) ||
      level <= 0 ||
      level >= 1
  ) {
    stop("Level must be 0 < level < 1.", call. = FALSE)
  }
  d <- diagnostic_data(model, type, seed)
  by_family <- diagnostic_data_by_family(d, by_family)
  reference <- diagnostic_seed(
    seed,
    diagnostic_qq_data(
      d$source,
      d,
      method,
      n_uniform,
      n_simulate,
      level,
      by_family
    )
  )
  df <- bin_diagnostic_qq(
    reference,
    qq_bins = if (aggregate) qq_bins else max(3L, nrow(reference$data)),
    worm = worm
  )
  render_diagnostic_qq(
    df,
    reference,
    d$type,
    worm,
    xlab,
    ylab,
    title,
    subtitle,
    caption,
    ci_col,
    ci_alpha,
    point_col,
    point_alpha,
    line_col
  )
}

diagnostic_grid <- function(x, y, density_bins, density) {
  density_bins <- validate_diagnostic_bins(
    density_bins,
    "density_bins",
    length = c(1L, 2L)
  )
  density_bins <- rep(density_bins, length.out = 2L)
  df <- diagnostic_finite(tibble::tibble(x = x, y = y), c("x", "y"))
  breaks <- function(x, n) {
    lim <- range(x)
    if (diff(lim) <= sqrt(.Machine$double.eps) * max(1, abs(lim))) {
      center <- mean(lim)
      lim <- center + c(-0.5, 0.5) * max(1, abs(center) * 0.01)
    }
    seq(lim[1], lim[2], length.out = n + 1L)
  }
  xb <- breaks(df$x, density_bins[1L])
  yb <- breaks(df$y, density_bins[2L])
  xi <- findInterval(df$x, xb, all.inside = TRUE)
  yi <- findInterval(df$y, yb, all.inside = TRUE)
  nx <- density_bins[1L]
  counts <- tabulate(xi + (yi - 1L) * nx, nbins = prod(as.double(density_bins)))
  id <- which(counts > 0)
  ix <- (id - 1L) %% nx + 1L
  iy <- (id - 1L) %/% nx + 1L
  value <- counts[id]
  if (density == "conditional") {
    value <- value / tabulate(xi, nbins = nx)[ix] / diff(yb)[iy]
  }
  tibble::tibble(
    xmin = xb[ix],
    xmax = xb[ix + 1L],
    ymin = yb[iy],
    ymax = yb[iy + 1L],
    count = counts[id],
    value = value
  )
}

# Keep bandwidth checks separate from estimation so appraise() can validate its
# controls before constructing panels, including when ordinary rendering is used.
validate_diagnostic_bandwidth <- function(bandwidth) {
  if (!is.null(bandwidth) &&
    (!is.numeric(bandwidth) || length(bandwidth) != 2L ||
      any(!is.finite(bandwidth)) || any(bandwidth <= 0))) {
    stop("'bandwidth' must be NULL or two finite positive numbers.", call. = FALSE)
  }
  bandwidth
}

# KernSmooth performs the linear binning and FFT convolution. Retain only its
# small grid, normalized by the marginal predictor density, in the final plot.
diagnostic_density_grid <- function(x, y, density_bins, bandwidth = NULL) {
  bins <- rep(validate_diagnostic_bins(density_bins, "density_bins",
    length = c(1L, 2L)), length.out = 2L)
  validate_diagnostic_bandwidth(bandwidth)
  df <- diagnostic_finite(tibble::tibble(x = x, y = y), c("x", "y"))
  axis_range <- function(z) {
    lim <- range(z)
    if (diff(lim) <= sqrt(.Machine$double.eps) * max(1, abs(lim))) {
      lim <- mean(lim) + c(-.5, .5) * max(1, abs(mean(lim)) * .01)
    }
    lim
  }
  xr <- axis_range(df$x)
  yr <- axis_range(df$y)
  if (is.null(bandwidth)) {
    automatic <- function(z, lim) {
      if (length(z) < 2L || diff(range(z)) <=
        sqrt(.Machine$double.eps) * max(1, abs(z))) {
        return(diff(lim) / 4)
      }
      # bw.nrd0 uses n^(-1/5); the two-dimensional rule uses n^(-1/6).
      stats::bw.nrd0(z) * length(z)^(1 / 30)
    }
    bandwidth <- c(automatic(df$x, xr), automatic(df$y, yr))
  }
  yr <- yr + c(-3, 3) * bandwidth[2L]
  if (any(!is.finite(yr))) {
    stop("'bandwidth' produces non-finite KDE limits; use smaller bandwidths.",
      call. = FALSE)
  }
  # Resolve KernSmooth's truncated Gaussian kernel even for a coarse display
  # grid. Average its estimate into the requested plotting cells below.
  grid <- pmax(2L, bins, ceiling(c(diff(xr), diff(yr)) / (3.4 * bandwidth)) + 2L)
  if (any(!is.finite(grid)) || prod(grid) > 2^20) {
    stop("The KDE grid is too large; increase 'bandwidth' or reduce 'density_bins'.",
      call. = FALSE)
  }
  estimate <- KernSmooth::bkde2D(as.matrix(df), bandwidth = bandwidth,
    gridsize = grid, range.x = list(xr, yr))
  joint <- pmax(estimate$fhat, 0) # Remove negative FFT rounding noise.
  xg <- estimate$x1
  yg <- estimate$x2
  edges <- function(z, lim) {
    if (length(z) == 1L) return(lim)
    delta <- diff(z)[1L]
    c(z - delta / 2, utils::tail(z, 1L) + delta / 2)
  }
  if (bins[1L] != grid[1L]) {
    xg <- if (bins[1L] == 1L) mean(xr) else
      seq(xr[1L], xr[2L], length.out = bins[1L])
    group <- findInterval(estimate$x1, edges(xg, xr), all.inside = TRUE)
    joint <- rowsum(joint, group, reorder = FALSE) / tabulate(group, nbins = bins[1L])
  }
  if (bins[2L] != grid[2L]) {
    yg <- if (bins[2L] == 1L) mean(yr) else
      seq(yr[1L], yr[2L], length.out = bins[2L])
    group <- findInterval(estimate$x2, edges(yg, yr), all.inside = TRUE)
    joint <- t(rowsum(t(joint), group, reorder = FALSE) /
      tabulate(group, nbins = bins[2L]))
  }
  xb <- edges(xg, xr)
  yb <- edges(yg, yr)
  xi <- findInterval(df$x, xb, all.inside = TRUE)
  yi <- findInterval(df$y, yb, all.inside = TRUE)
  counts <- tabulate(xi + (yi - 1L) * bins[1L], nbins = prod(as.double(bins)))
  marginal <- as.vector(joint %*% diff(yb))
  populated <- tabulate(xi, nbins = bins[1L]) > 0L & marginal > 0
  value <- joint / marginal
  value[!populated, ] <- NA_real_
  ix <- rep(seq_len(bins[1L]), bins[2L])
  iy <- rep(seq_len(bins[2L]), each = bins[1L])
  out <- tibble::tibble(xmin = xb[ix], xmax = xb[ix + 1L],
    ymin = yb[iy], ymax = yb[iy + 1L], count = counts,
    value = as.vector(value))
  out[is.finite(out$value), , drop = FALSE]
}

render_diagnostic_grid <- function(
  df,
  density,
  xlab,
  ylab,
  title,
  subtitle,
  caption,
  intercept = NULL,
  line_col = "red",
  fill_transform = "identity"
) {
  plt <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = .data$ymin,
      ymax = .data$ymax,
      fill = .data$value
    )
  ) +
    ggplot2::geom_rect(colour = NA) +
    ggplot2::scale_fill_viridis_c(
      name = if (density == "conditional") "Conditional density" else "Count",
      transform = fill_transform
    )
  if (!is.null(intercept)) {
    plt <- plt + ggplot2::geom_hline(yintercept = intercept, colour = line_col)
  }
  if (".family" %in% names(df)) {
    plt <- plt +
      ggplot2::facet_wrap(ggplot2::vars(.data$.family), scales = "free")
  }
  compact_diagnostic_plot(
    plt +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = title,
        subtitle = subtitle,
        caption = caption
      )
  )
}

#' @rdname scalable_qq_plot
#' @export
scalable_residuals_linpred_plot <- function(
  model,
  type = NULL,
  density_bins = c(50L, 50L),
  density = c("conditional", "count"),
  seed = NULL,
  xlab = NULL,
  ylab = NULL,
  title = "Residuals vs linear predictor",
  subtitle = NULL,
  caption = NULL,
  line_col = "red",
  by_family = NULL,
  rank = NULL,
  density_method = c("kde", "histogram"),
  bandwidth = NULL
) {
  density <- match.arg(density)
  density_method <- match.arg(density_method)
  validate_diagnostic_bandwidth(bandwidth)
  d <- diagnostic_data(model, type, seed)
  by_family <- diagnostic_data_by_family(d, by_family)
  dat <- d$data
  if (".family" %in% names(dat)) {
    dat <- diagnostic_predictor_data(dat, d$source, by_family, rank)
    if (isTRUE(attr(dat, "ranked"))) {
      d$eta_label <- "Within-component linear predictor rank"
    }
  } else if (!is.null(rank)) {
    if (!is.logical(rank) || length(rank) != 1L || is.na(rank)) {
      stop("'rank' must be TRUE, FALSE or NULL.", call. = FALSE)
    }
    if (rank) {
      included <- is.finite(dat$eta) & is.finite(dat$residuals)
      dat$eta[included] <- (base::rank(
        dat$eta[included],
        ties.method = "average"
      ) -
        .5) /
        sum(included)
      d$eta_label <- "Linear predictor rank"
    }
  }
  df <- diagnostic_grouped_grid(
    dat,
    "eta",
    "residuals",
    density_bins,
    density,
    by_family,
    density_method,
    bandwidth
  )
  render_diagnostic_grid(
    df,
    density,
    if (is.null(xlab)) d$eta_label else xlab,
    if (is.null(ylab)) {
      paste(stringr::str_to_title(d$type), "residuals")
    } else {
      ylab
    },
    title,
    subtitle,
    caption,
    if (d$type == "pit") 0.5 else 0,
    line_col,
    fill_transform = if (density == "conditional" && density_method == "kde") {
      "sqrt"
    } else "identity"
  )
}

#' @rdname scalable_qq_plot
#' @export
scalable_observed_fitted_plot <- function(
  model,
  density_bins = c(50L, 50L),
  density = c("count", "conditional"),
  xlab = "Fitted values",
  ylab = "Response",
  title = "Observed vs fitted values",
  subtitle = NULL,
  caption = NULL,
  by_family = NULL
) {
  density <- match.arg(density)
  d <- diagnostic_data(model)
  if (".family" %in% names(d$data)) {
    check_gfam_appraise(d$source)
  }
  by_family <- diagnostic_data_by_family(
    d,
    if (is.null(by_family)) TRUE else by_family
  )
  df <- diagnostic_grouped_grid(
    d$data,
    "fitted",
    "observed",
    density_bins,
    density,
    by_family
  )
  render_diagnostic_grid(df, density, xlab, ylab, title, subtitle, caption)
}

# Keep histogram centering at zero, including the existing n_bins heuristics.
diagnostic_histogram <- function(r, n_bins) {
  r <- diagnostic_finite(tibble::tibble(residuals = r), "residuals")$residuals
  if (is.character(n_bins)) {
    n_bins <- match.arg(n_bins, c("sturges", "scott", "fd"))
    n_bins <- switch(
      n_bins,
      sturges = grDevices::nclass.Sturges(r),
      scott = grDevices::nclass.scott(r),
      fd = grDevices::nclass.FD(r)
    ) +
      2L
  }
  n_bins <- validate_diagnostic_bins(n_bins, "n_bins")
  # Use ggplot's public statistic to retain its exact break, centering and
  # boundary conventions. The temporary observation plot is discarded here.
  tmp <- ggplot2::ggplot(
    tibble::tibble(residuals = r),
    ggplot2::aes(x = .data$residuals)
  ) +
    ggplot2::geom_histogram(bins = n_bins, center = 0)
  bins <- ggplot2::ggplot_build(tmp)$data[[1L]]
  tibble::tibble(xmin = bins$xmin, xmax = bins$xmax, count = bins$count)
}

render_diagnostic_hist <- function(df, xlab, ylab, title, subtitle, caption) {
  compact_diagnostic_plot(
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = 0,
        ymax = .data$count
      )
    ) +
      ggplot2::geom_rect(colour = "white", fill = "grey20") +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = title,
        subtitle = subtitle,
        caption = caption
      )
  )
}

#' @rdname scalable_qq_plot
#' @export
scalable_residuals_hist_plot <- function(
  model,
  type = NULL,
  n_bins = c("sturges", "scott", "fd"),
  seed = NULL,
  xlab = NULL,
  ylab = "Frequency",
  title = "Histogram of residuals",
  subtitle = NULL,
  caption = NULL,
  by_family = NULL
) {
  d <- diagnostic_data(model, type, seed)
  by_family <- diagnostic_data_by_family(d, by_family)
  if (".family" %in% names(d$data)) {
    return(scalable_grouped_histogram(
      d,
      n_bins,
      by_family,
      xlab,
      ylab,
      title,
      subtitle,
      caption
    ))
  }
  df <- diagnostic_histogram(d$data$residuals, n_bins)
  render_diagnostic_hist(
    df,
    if (is.null(xlab)) {
      paste(stringr::str_to_title(d$type), "residuals")
    } else {
      xlab
    },
    ylab,
    title,
    subtitle,
    caption
  )
}

render_diagnostic_points <- function(
  df,
  x,
  y,
  xlab,
  ylab,
  title,
  point_col,
  point_alpha,
  line_col = NULL
) {
  plt <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]]))
  if (!is.null(line_col)) {
    plt <- plt + ggplot2::geom_hline(yintercept = 0, colour = line_col)
  }
  plt +
    ggplot2::geom_point(colour = point_col, alpha = point_alpha) +
    ggplot2::labs(x = xlab, y = ylab, title = title)
}

#' @rdname qq_plot
#' @export
qq_plot.gamlss <- function(model, ..., type = "quantile", seed = NULL) {
  scalable_qq(model, ..., type = type, seed = seed, aggregate = FALSE)
}

#' @rdname worm_plot
#' @export
worm_plot.gamlss <- function(model, ..., type = "quantile", seed = NULL) {
  scalable_qq(
    model,
    ...,
    type = type,
    seed = seed,
    worm = TRUE,
    aggregate = FALSE
  )
}

# Strip quosure environments too: object.size() does not count the models they
# could otherwise keep alive through unevaluated renderer arguments.
compact_diagnostic_plot <- function(plot) {
  env <- environment(compact_diagnostic_plot)
  clean <- function(mapping) {
    for (nm in names(mapping)) {
      if (rlang::is_quosure(mapping[[nm]])) {
        mapping[[nm]] <- rlang::quo_set_env(mapping[[nm]], env)
      }
    }
    mapping
  }
  plot$mapping <- clean(plot$mapping)
  for (i in seq_along(plot$layers)) {
    plot$layers[[i]]$mapping <- clean(plot$layers[[i]]$mapping)
  }
  if (!is.null(plot$facet$params$facets)) {
    plot$facet$params$facets <- clean(plot$facet$params$facets)
  }
  plot$plot_env <- env
  plot
}

# Grouped-family plotting policy remains shared with the ordinary diagnostics.
diagnostic_data_by_family <- function(d, by_family = NULL) {
  if (
    !is.null(by_family) &&
      (!is.logical(by_family) || length(by_family) != 1L || is.na(by_family))
  ) {
    stop("'by_family' must be NULL, TRUE or FALSE.", call. = FALSE)
  }
  ".family" %in%
    names(d$data) &&
    if (is.null(by_family)) !d$type %in% c("quantile", "pit") else by_family
}

diagnostic_grouped_grid <- function(dat, x, y, bins, density, by_family,
  density_method = "histogram", bandwidth = NULL) {
  aggregate <- function(x, y) {
    if (density == "conditional" && density_method == "kde") {
      diagnostic_density_grid(x, y, bins, bandwidth)
    } else diagnostic_grid(x, y, bins, density)
  }
  if (!by_family) {
    return(aggregate(dat[[x]], dat[[y]]))
  }
  dplyr::bind_rows(lapply(split(dat, dat$.family, drop = TRUE), function(part) {
    out <- aggregate(part[[x]], part[[y]])
    out$.family <- part$.family[1L]
    out
  }))
}

scalable_grouped_histogram <- function(
  d,
  n_bins,
  by_family,
  xlab,
  ylab,
  title,
  subtitle,
  caption
) {
  by_family <- diagnostic_data_by_family(d, by_family)
  old <- residuals_hist_plot(
    d$source,
    type = d$type,
    n_bins = n_bins,
    by_family = by_family
  )
  # Preserve the existing shared breaks and per-component density normalization
  # for pooled histograms, and panel-specific counts for faceted histograms.
  built <- ggplot2::ggplot_build(old)
  values <- built$data[[1L]]
  labels <- levels(droplevels(d$data$.family))
  df <- tibble::tibble(
    xmin = values$xmin,
    xmax = values$xmax,
    count = values$count,
    value = if (by_family) values$count else values$density,
    .family = factor(
      labels[if (by_family) as.integer(values$PANEL) else values$group],
      levels = levels(d$data$.family)
    )
  )
  render_grouped_histogram(
    df,
    by_family,
    xlab %||% paste(stringr::str_to_title(d$type), "residuals"),
    if (identical(ylab, "Frequency") && !by_family) "Density" else ylab,
    title,
    subtitle,
    caption
  )
}

# Trace only the upper boundary, including vertical ends at the outer bin
# edges. Explicit vertices also handle unequal bin widths and empty bins.
diagnostic_histogram_outline <- function(df) {
  dplyr::bind_rows(lapply(split(df, df$.family, drop = TRUE), function(part) {
    part <- part[order(part$xmin), , drop = FALSE]
    n <- nrow(part)
    # Adjacent edges returned by stat_bin() can differ by rounding error.
    # Use one coordinate for each shared edge to keep transitions vertical.
    right <- c(part$xmin[-1L], part$xmax[n])
    tibble::tibble(
      x = c(part$xmin[1L], as.vector(rbind(part$xmin, right)), part$xmax[n]),
      y = c(0, rep(part$value, each = 2L), 0),
      .family = part$.family[1L]
    )
  }))
}

render_grouped_histogram <- function(
  df,
  by_family,
  xlab,
  ylab,
  title,
  subtitle,
  caption
) {
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = 0,
      ymax = .data$value
    )
  )
  if (by_family) {
    p <- p +
      ggplot2::geom_rect(fill = "grey20", colour = "white") +
      ggplot2::facet_wrap(ggplot2::vars(.data$.family), scales = "free")
  } else {
    p <- p +
      ggplot2::geom_rect(
        ggplot2::aes(fill = .data$.family),
        colour = NA,
        alpha = .15
      ) +
      ggplot2::geom_path(
        data = diagnostic_histogram_outline(df),
        ggplot2::aes(x = .data$x, y = .data$y, colour = .data$.family),
        inherit.aes = FALSE,
        linewidth = .6,
        alpha = 1
      ) +
      ggokabeito::scale_fill_okabe_ito(
        limits = levels(df$.family),
        drop = FALSE
      ) +
      ggokabeito::scale_colour_okabe_ito(
        limits = levels(df$.family),
        drop = FALSE
      ) +
      ggplot2::labs(fill = "Family component", colour = "Family component")
  }
  compact_diagnostic_plot(
    p +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = title,
        subtitle = subtitle,
        caption = caption
      )
  )
}
