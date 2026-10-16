#' Accumulated local effects of a model covariate
#'
#' Calculate one-dimensional accumulated local effects (ALE) of a numeric
#' covariate, combining all model terms involving that covariate.
#'
#' @param object a model fitted by [mgcv::gam()] or [mgcv::bam()].
#' @param focal character; the name of one raw numeric model covariate.
#' @param data optional reference data. Each row receives equal weight. By
#'   default, use the observations retained during fitting. Responses are not
#'   required. Supply raw covariates, including inputs to model expressions.
#' @param scale character; `"link"` (default), `"response"`, or
#'   `"linear_predictor"`, an alias for `"link"`.
#' @param n_bins positive integer; requested number of intervals. Empirical
#'   quantiles (`type = 1`) define boundaries; ties can reduce the number used.
#' @param breaks optional numeric interval boundaries, overriding `n_bins`.
#'   These must increase strictly, span exactly the reference range, and
#'   produce no empty intervals.
#' @param parameter character; one distribution parameter, using the names and
#'   aliases accepted by [link()]. `NULL` selects the first parameter.
#' @param which_eta optional integer selecting a linear predictor instead of
#'   `parameter`. If both are supplied they must select the same predictor.
#' @param center logical; subtract the empirical mean of the interpolated ALE
#'   curve over the reference observations? If `FALSE`, anchor the curve at
#'   zero at the first boundary.
#' @param interval character; `"confidence"` for pointwise intervals or
#'   `"simultaneous"` for bands covering all returned boundaries of this curve.
#' @param ci_level numeric; interval coverage, strictly between zero and one.
#' @param unconditional logical; include smoothing parameter uncertainty if
#'   available. Otherwise [get_vcov()] warns and uses the uncorrected covariance.
#' @param frequentist logical; use the frequentist coefficient covariance?
#' @param n_sim,n_cores positive integers; simulation count and number of cores
#'   for simultaneous intervals. Ignored for pointwise intervals.
#' @param seed optional integer seed for simultaneous intervals. An explicit
#'   seed preserves the caller's random-number state.
#' @param block_size positive integer; maximum reference rows processed at once.
#' @param exclude character; model terms to exclude, as in [mgcv::predict.gam()].
#' @param envir optional environment for functions and constants in model
#'   expressions; see [smooth_estimates()].
#' @param ... named prediction controls passed to [mgcv::predict.gam()]. Data,
#'   type, term selection, row handling and uncertainty arguments are reserved.
#'
#' @details
#' Within each interval, predict at its two boundaries for every reference
#' observation in that interval, retaining its other covariates. Average the
#' upper-minus-lower prediction differences, then accumulate these averages.
#' Intervals are right closed, with the minimum included in the first interval.
#' The returned curve is linearly interpolated between boundaries. Centring
#' subtracts its mean evaluated at the reference observations, rather than an
#' unweighted mean over boundaries. Fitting weights do not weight these averages.
#'
#' On the response scale, transform endpoint predictions before differencing.
#' Transforming a link-scale ALE curve does not produce response-scale ALE.
#' Response-scale effects are differences in natural parameter units, not
#' predicted parameter values, and can be negative.
#'
#' Distributional families `gaulss`, `gammals`, `gumbls`, `gevlss`, `shash`,
#' `twlss`, and `ziplss` are supported. Natural units mean standard deviation
#' for Gaussian scale, positive scale or dispersion for logged scales, natural
#' Tweedie power, positive sinh-arcsinh tail parameter, and Poisson rate or
#' probability of presence for `ziplss`. Location parameters need not be response
#' means. Overall means combining several parameters, multinomial probabilities,
#' multivariate covariance parameters and grouped-family outputs are unsupported.
#' For other families, link-scale ALE is available for a single linear predictor;
#' response-scale ALE requires a supported scalar mean family.
#'
#' Supported response-scale parameters are:
#' * `gaulss`: location (mean) and scale (standard deviation);
#' * `gammals`: location (mean) and scale (dispersion);
#' * `gumbls` and `gevlss`: location and positive scale, plus GEV shape;
#' * `shash`: location, positive scale, skewness and positive tail parameter
#'   (named `kurtosis` to match gratia's parameter names);
#' * `twlss`: location (mean), power and scale (dispersion);
#' * `ziplss`: location (Poisson rate) and `pi` (probability of presence).
#'
#' Raw-data recovery can require `envir` when a model was fitted locally and
#' its formula no longer retains the environment containing the fitting data.
#' Alternatively, supply reference `data` explicitly.
#'
#' Coefficient gradients are averaged, accumulated and centred in the same way
#' as the effects, retaining covariance across intervals and the centring
#' constant. Link-scale gradients are exact; response-scale intervals use the
#' delta method. Both interval types condition on the reference observations and
#' boundaries. Simultaneous coverage concerns the returned boundaries, not all
#' points between them. No uncertainty for the reference distribution is added.
#'
#' Formula offsets are included and reevaluated at interval boundaries. Offsets
#' supplied only through the fitting function's `offset` argument are omitted,
#' following explicit-newdata prediction semantics. BAM predictions use
#' `discrete = FALSE`, including for models fitted with `discrete = TRUE`.
#'
#' This implementation is written independently from the published ALE
#' definition, without using code from mgcViz or ALEPlot.
#'
#' @return A tibble of class `"ale"`, with `.focal` and `.parameter` identifying
#'   the curve, `.value` giving boundary coordinates, `.ale` the effects, `.se`
#'   their standard errors, `.crit` the interval critical value, and `.lower_ci`
#'   and `.upper_ci` the interval limits. `.bin` identifies the interval ending
#'   at each boundary and `.n` counts its reference rows. At the initial boundary
#'   both are zero. The `ale_info` attribute records computation settings without
#'   retaining the model or reference data.
#'
#' @references Apley, D. W. and Zhu, J. (2020). Visualizing the effects of
#'   predictor variables in black box supervised learning models.
#'   Journal of the Royal Statistical Society: Series B, 82, 1059--1086.
#'   \doi{10.1111/rssb.12377}.
#' @seealso [draw.ale()], [conditional_values()], [response_derivatives()]
#' @export
#' @examples
#' library("mgcv")
#' df <- data_sim("eg1", n = 200, seed = 42)
#' m <- gam(y ~ s(x0) + te(x1, x2, k = c(5, 5)) + s(x3),
#'   data = df, method = "REML")
#' a <- ale(m, focal = "x1")
#' draw(a)
#'
#' # Standard deviation in its natural units, rather than reciprocal SD
#' ml <- gam(list(y ~ s(x1), ~ x2), data = df, family = gaulss())
#' ale(ml, "x2", parameter = "scale", scale = "response") |> draw()
ale <- function(object, ...) {
  UseMethod("ale")
}

#' @rdname ale
#' @export
ale.default <- function(object, ...) {
  cli::cli_abort(
    "Don't know how to calculate ALE for {.cls {class(object)[1L]}}."
  )
}

#' @rdname ale
#' @export
ale.gam <- function(
  object,
  focal,
  data = NULL,
  scale = c("link", "response", "linear_predictor"),
  n_bins = 40L,
  breaks = NULL,
  parameter = NULL,
  which_eta = NULL,
  center = TRUE,
  interval = c("confidence", "simultaneous"),
  ci_level = 0.95,
  unconditional = FALSE,
  frequentist = FALSE,
  n_sim = 10000L,
  n_cores = 1L,
  seed = NULL,
  block_size = 10000L,
  exclude = NULL,
  envir = NULL,
  ...
) {
  if (inherits(object, "scam")) {
    ale.default(object)
  }
  scale <- match.arg(scale)
  if (scale == "linear_predictor") {
    scale <- "link"
  }
  interval <- match.arg(interval)
  validate_smooth_interval(interval, ci_level, n_sim, n_cores, seed)
  for (nm in c("center", "unconditional", "frequentist")) {
    value <- get(nm)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort("{.arg {nm}} must be one non-missing logical value.")
    }
  }
  ale_positive_integer(block_size, "block_size")
  if (is.null(breaks)) {
    ale_positive_integer(n_bins, "n_bins")
  }
  object <- with_model_envir(object, envir)
  if (
    !is.character(focal) ||
      length(focal) != 1L ||
      is.na(focal) ||
      !focal %in% model_vars(object)
  ) {
    cli::cli_abort("{.arg focal} must name one raw numeric model covariate.")
  }
  dots <- list(...)
  ale_prediction_controls(dots)
  if (inherits(object, "bam")) {
    dots$discrete <- FALSE
  }
  if (!is.null(exclude) && (!is.character(exclude) || anyNA(exclude))) {
    cli::cli_abort(
      "{.arg exclude} must be NULL or a character vector of model terms."
    )
  }
  target <- ale_target(object, parameter, which_eta, scale)
  target$columns <- lss_eta_index(object)[[target$index]]
  reference <- recover_raw_data(object, data = data, envir = envir)
  if (!is.data.frame(reference) || !nrow(reference)) {
    cli::cli_abort(
      "Reference {.arg data} must contain at least one observation."
    )
  }
  x <- reference[[focal]]
  if (
    !is.numeric(x) ||
      !is.null(dim(x)) ||
      inherits(x, c("Date", "POSIXt", "difftime"))
  ) {
    cli::cli_abort("{.arg focal} must select a numeric vector.")
  }
  # Evaluate only predictors: supplied missing responses never discard rows.
  mf <- evaluated_model_frame(object, reference)
  valid <- finite_predictor_rows(mf)
  if (length(valid) != nrow(reference) || any(!valid) || any(!is.finite(x))) {
    cli::cli_abort(
      "Reference data contain missing or non-finite model predictors; supply valid reference rows."
    )
  }
  grid <- ale_bins(x, n_bins, breaks)
  nb <- length(grid$breaks) - 1L
  counts <- tabulate(grid$bin, nbins = nb)
  p <- length(stats::coef(object))
  sums <- numeric(nb)
  gradients <- matrix(0, nb, p)
  # Use a compact local model so native prediction cannot restore training NAs.
  object <- model_used_rows(object)
  predict_at <- function(nd, type) {
    do.call(
      predict_model,
      c(
        list(
          object = object,
          newdata = nd,
          type = type,
          exclude = exclude,
          na.action = stats::na.pass
        ),
        dots
      )
    )
  }
  for (start in seq.int(1L, nrow(reference), by = block_size)) {
    rows <- seq.int(start, min(nrow(reference), start + block_size - 1))
    b <- grid$bin[rows]
    lower <- upper <- as.data.frame(reference[rows, , drop = FALSE])
    lower[[focal]] <- grid$breaks[b]
    upper[[focal]] <- grid$breaks[b + 1L]
    # Raw modified coordinates must be evaluated afresh, even if the reference
    # originated in a model frame containing stored expression values.
    attr(lower, "terms") <- attr(upper, "terms") <- NULL
    attr(lower, "gratia.evaluated") <- attr(upper, "gratia.evaluated") <- NULL
    lo <- ale_endpoint(lower, target, p, predict_at)
    hi <- ale_endpoint(upper, target, p, predict_at)
    delta <- hi$value - lo$value
    D <- hi$gradient - lo$gradient
    group <- rowsum(cbind(delta, D), b, reorder = FALSE)
    ids <- as.integer(rownames(group))
    sums[ids] <- sums[ids] + group[, 1L]
    gradients[ids, ] <- gradients[ids, , drop = FALSE] +
      group[, -1L, drop = FALSE]
  }
  estimate <- c(0, cumsum(sums / counts))
  J <- rbind(
    rep(0, p),
    matrix(apply(gradients / counts, 2L, cumsum), nrow = nb, ncol = p)
  )
  if (center) {
    # Each observed x interpolates two boundary rows. Aggregating these weights
    # centres values and gradients without constructing an n by bins matrix.
    fraction <- (x - grid$breaks[grid$bin]) /
      (grid$breaks[grid$bin + 1L] - grid$breaks[grid$bin])
    weights <- numeric(nb + 1L)
    left <- rowsum(1 - fraction, grid$bin, reorder = FALSE)
    right <- rowsum(fraction, grid$bin + 1L, reorder = FALSE)
    weights[as.integer(rownames(left))] <- left[, 1L]
    weights[as.integer(rownames(right))] <-
      weights[as.integer(rownames(right))] + right[, 1L]
    weights <- weights / length(x)
    estimate <- estimate - sum(weights * estimate)
    J <- sweep(J, 2L, colSums(J * weights), "-")
  }
  V <- get_vcov(
    object,
    unconditional = unconditional,
    frequentist = frequentist
  )
  if (!is.matrix(V) || !identical(dim(V), c(p, p)) || any(!is.finite(V))) {
    cli::cli_abort(
      "A finite coefficient covariance matrix is required for ALE intervals."
    )
  }
  se <- sqrt(pmax(0, rowSums((J %*% V) * J)))
  if (interval == "confidence") {
    critical <- coverage_normal(ci_level)
    lower <- estimate - critical * se
    upper <- estimate + critical * se
  } else {
    calculate <- function() {
      simultaneous_intervals(
        estimate,
        se,
        J,
        V,
        level = ci_level,
        n_sim = n_sim,
        n_cores = n_cores
      )
    }
    band <- if (is.null(seed)) {
      calculate()
    } else {
      withr::with_seed(seed, calculate())
    }
    critical <- band$critical
    lower <- band$lower
    upper <- band$upper
  }
  out <- tibble::tibble(
    .focal = focal,
    .parameter = target$name,
    .value = grid$breaks,
    .ale = as.numeric(estimate),
    .se = as.numeric(se),
    .crit = as.numeric(critical),
    .lower_ci = as.numeric(lower),
    .upper_ci = as.numeric(upper),
    .bin = seq.int(0L, nb),
    .n = c(0L, counts)
  )
  attr(out, "ale_info") <- list(
    scale = scale,
    center = center,
    breaks = grid$breaks,
    n_bins = nb,
    n_reference = length(x),
    parameter = target$name,
    parameter_label = target$label,
    which_eta = target$index,
    interval = interval,
    ci_level = ci_level,
    n_sim = if (interval == "simultaneous") n_sim else NULL,
    n_cores = if (interval == "simultaneous") n_cores else NULL,
    seed = if (interval == "simultaneous") seed else NULL,
    unconditional = unconditional,
    frequentist = frequentist,
    uncertainty = if (scale == "link") "linear" else "delta",
    exclude = exclude
  )
  class(out) <- c("ale", class(out))
  out
}

#' @rdname ale
#' @export
ale.bam <- function(object, ...) {
  ale.gam(object, ...)
}

ale_positive_integer <- function(value, name) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value < 1 ||
      value > .Machine$integer.max ||
      value != floor(value)
  ) {
    cli::cli_abort("{.arg {name}} must be a positive integer.")
  }
}

ale_prediction_controls <- function(dots) {
  nms <- names(dots)
  if (
    length(dots) &&
      (is.null(nms) || anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms))
  ) {
    cli::cli_abort(
      "Prediction controls in {.arg ...} must have unique, non-empty names."
    )
  }
  reserved <- c(
    "object",
    "newdata",
    "type",
    "se.fit",
    "terms",
    "na.action",
    "newdata.guaranteed",
    "unconditional",
    "iterms.type"
  )
  # Partial argument matching must not circumvent the reserved controls.
  bad <- nms[vapply(
    nms,
    function(nm) any(startsWith(reserved, nm)),
    logical(1L)
  )]
  if (length(bad)) {
    cli::cli_abort("Reserved prediction controls in {.arg ...}: {.val {bad}}.")
  }
  if ("discrete" %in% nms && !identical(dots$discrete, FALSE)) {
    cli::cli_abort("ALE prediction requires {.code discrete = FALSE}.")
  }
}

ale_bins <- function(x, n_bins, breaks) {
  limits <- range(x)
  if (limits[1L] == limits[2L]) {
    cli::cli_abort("The focal covariate must not be constant.")
  }
  if (is.null(breaks)) {
    breaks <- unique(as.numeric(stats::quantile(
      x,
      probs = seq(0, 1, length.out = n_bins + 1),
      type = 1,
      names = FALSE
    )))
  } else if (
    !is.numeric(breaks) ||
      !is.null(dim(breaks)) ||
      length(breaks) < 2L ||
      any(!is.finite(breaks)) ||
      any(diff(breaks) <= 0) ||
      breaks[1L] != limits[1L] ||
      breaks[length(breaks)] != limits[2L]
  ) {
    cli::cli_abort(
      "{.arg breaks} must increase strictly from the reference minimum to its maximum."
    )
  }
  breaks <- as.numeric(breaks)
  bin <- as.integer(cut(
    x,
    breaks,
    right = TRUE,
    include.lowest = TRUE,
    labels = FALSE
  ))
  if (anyNA(bin) || any(tabulate(bin, nbins = length(breaks) - 1L) == 0L)) {
    cli::cli_abort("{.arg breaks} must not produce empty intervals.")
  }
  list(breaks = breaks, bin = bin)
}

# Keep the parameter description with the results so plotting needs no model.
ale_parameter_label <- function(family, parameter, mean_families) {
  if (
    parameter == "location" &&
      family %in% c(mean_families, "gaulss", "gammals", "twlss")
  ) {
    return("mean")
  }
  if (parameter == "scale" && family == "gaulss") {
    return("standard deviation")
  }
  if (parameter == "scale" && family %in% c("gammals", "twlss")) {
    return("dispersion")
  }
  if (family == "ziplss") {
    return(
      if (parameter == "location") "Poisson rate" else "presence probability"
    )
  }
  parameter
}

# The adapter describes one scalar function of one linear predictor. Its
# derivative converts coefficient gradients to natural parameter units.
ale_target <- function(object, parameter, which_eta, scale) {
  f <- stats::family(object)
  fn <- family_type(object)
  multi <- fn %in%
    c("gaulss", "gammals", "gumbls", "gevlss", "shash", "twlss", "ziplss")
  if (
    is_gfam_family(f) ||
      (!multi && n_eta(object) != 1L) ||
      fn %in% c("multinom", "multivariate_normal")
  ) {
    cli::cli_abort("ALE does not support this model's distribution parameters.")
  }
  names <- if (multi) lss_parameters(object) else "location"
  aliases <- if (multi) link_parameter_map(f) else c(location = 1L, mu = 1L)
  if (
    !is.null(parameter) &&
      (!is.character(parameter) ||
        length(parameter) != 1L ||
        is.na(parameter) ||
        !parameter %in% names(aliases))
  ) {
    cli::cli_abort(
      "Unknown {.arg parameter}; choose from {.val {names(aliases)}}."
    )
  }
  index <- if (is.null(parameter)) 1L else unname(aliases[[parameter]])
  if (!is.null(which_eta)) {
    ale_positive_integer(which_eta, "which_eta")
    if (which_eta > length(names)) {
      cli::cli_abort(
        "{.arg which_eta} exceeds the number of linear predictors."
      )
    }
    if (!is.null(parameter) && which_eta != index) {
      cli::cli_abort(
        "{.arg parameter} and {.arg which_eta} select different linear predictors."
      )
    }
    index <- as.integer(which_eta)
  }
  supported <- c(
    "gaussian",
    "poisson",
    "binomial",
    "gamma",
    "inverse_gaussian",
    "quasi",
    "quasipoisson",
    "quasibinomial",
    "negative_binomial",
    "tweedie",
    "beta_regression",
    "scaled_t"
  )
  target <- list(
    index = index,
    name = names[index],
    label = ale_parameter_label(fn, names[index], supported),
    response = scale == "response"
  )
  if (scale == "link") {
    return(target)
  }
  if (!multi && !fn %in% supported) {
    cli::cli_abort(
      "Response-scale ALE is not supported for this family; use {.code scale = 'link'}."
    )
  }
  linfo <- if (multi) f$linfo[[index]] else f
  transform <- linfo$linkinv
  derivative <- linfo$mu.eta
  # Some mgcv links map to an internal logged parameter or reciprocal SD.
  post <- "identity"
  if (fn == "gaulss" && index == 2L) {
    post <- "reciprocal"
  }
  if (
    (fn %in% c("gammals", "gumbls", "gevlss") && index == 2L) ||
      (fn == "gammals" && index == 1L) ||
      (fn == "shash" && index %in% c(2L, 4L)) ||
      (fn == "twlss" && index == 3L)
  ) {
    post <- "exp"
  }
  if (fn == "twlss" && index == 2L) {
    post <- "power"
  }
  if (fn == "ziplss") {
    post <- if (index == 1L) "exp" else "presence"
  }
  if (!is.function(transform) || !is.function(derivative)) {
    cli::cli_abort(
      "The selected parameter requires an inverse link and its derivative."
    )
  }
  bounds <- if (post == "power") get_tw_params(f) else NULL
  target$evaluate <- function(eta) {
    value <- transform(eta)
    slope <- derivative(eta)
    if (post == "exp") {
      value <- exp(value)
      slope <- value * slope
    } else if (post == "reciprocal") {
      # For the logb link, sigma = b + exp(eta). Its derivative is exp(eta),
      # even when the native reciprocal-SD derivative has underflowed.
      slope <- exp(eta)
      value <- 1 / value
    } else if (post == "power") {
      prob <- stats::plogis(value)
      # Evaluate both logistic tails before replacing the predictor: 1 - prob
      # can round to zero while the derivative is still representable.
      slope <- (bounds[2L] - bounds[1L]) * prob * stats::plogis(-value) * slope
      value <- bounds[1L] + (bounds[2L] - bounds[1L]) * prob
    } else if (post == "presence") {
      rate <- exp(value)
      value <- -expm1(-rate)
      # The derivative tends to zero when exp(eta) overflows.
      slope <- exp(eta - rate) * slope
      slope[is.infinite(rate) & rate > 0] <- 0
    }
    list(value = as.numeric(value), slope = as.numeric(slope))
  }
  target
}

ale_endpoint <- function(data, target, p, predict_at) {
  eta <- predict_at(data, "link")
  if (is.matrix(eta)) {
    if (ncol(eta) < target$index) {
      cli::cli_abort("Missing selected linear predictor.")
    }
    eta <- eta[, target$index]
  }
  X <- predict_at(data, "lpmatrix")
  if (
    !is.matrix(X) ||
      nrow(X) != nrow(data) ||
      ncol(X) != p ||
      length(eta) != nrow(data) ||
      any(!is.finite(eta)) ||
      any(!is.finite(X))
  ) {
    cli::cli_abort(
      "Endpoint predictions and coefficient gradients must be finite and retain every reference row."
    )
  }
  X[, setdiff(seq_len(p), target$columns)] <- 0
  if (target$response) {
    values <- target$evaluate(eta)
    if (
      length(values$value) != nrow(data) ||
        length(values$slope) != nrow(data) ||
        any(!is.finite(values$value)) ||
        any(!is.finite(values$slope))
    ) {
      cli::cli_abort(
        "Non-finite endpoint values or gradients on the response scale."
      )
    }
    X <- X * values$slope
    eta <- values$value
  }
  list(value = as.numeric(eta), gradient = X)
}

#' Plot accumulated local effects
#'
#' Plot saved ALE estimates and intervals, without recomputing predictions.
#'
#' @param object a tibble of class `"ale"`, produced by [ale()].
#' @param ci logical; show the saved interval ribbon?
#' @param ref_line logical; draw a horizontal reference line at zero?
#' @param alpha numeric; opacity of the interval ribbon.
#' @param xlab,ylab optional axis labels. Defaults identify the focal covariate
#'   and selected parameter. The computation scale appears in the caption.
#'   Titles and subtitles are omitted. Use [ggplot2::labs()] to change or remove
#'   the caption, for example `+ ggplot2::labs(caption = NULL)`.
#' @param ... reserved for future use.
#' @return A [ggplot2::ggplot()] object.
#' @seealso [ale()]
#' @export
#' @examples
#' library("mgcv")
#' df <- data_sim("eg1", n = 100, seed = 2)
#' m <- gam(y ~ s(x2), data = df, method = "REML")
#' ale(m, "x2") |> draw()
draw.ale <- function(
  object,
  ci = TRUE,
  ref_line = TRUE,
  alpha = 0.2,
  xlab = NULL,
  ylab = NULL,
  ...
) {
  rlang::check_dots_empty()
  for (nm in c("ci", "ref_line")) {
    value <- get(nm)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort("{.arg {nm}} must be one non-missing logical value.")
    }
  }
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      !is.finite(alpha) ||
      alpha < 0 ||
      alpha > 1
  ) {
    cli::cli_abort("{.arg alpha} must be between zero and one.")
  }
  required <- c(".focal", ".parameter", ".value", ".ale")
  if (ci) {
    required <- c(required, ".lower_ci", ".upper_ci")
  }
  if (
    !all(required %in% names(object)) ||
      !nrow(object) ||
      length(unique(object$.focal)) != 1L ||
      length(unique(object$.parameter)) != 1L
  ) {
    cli::cli_abort(
      "{.arg object} must contain one ALE curve and its required columns."
    )
  }
  info <- attr(object, "ale_info", exact = TRUE)
  if (is.null(info)) {
    cli::cli_abort("Missing ALE settings; recompute with {.fun ale}.")
  }
  object <- object[order(object$.value), ]
  plt <- ggplot2::ggplot(object, ggplot2::aes(x = .data$.value, y = .data$.ale))
  if (ref_line) {
    plt <- plt + ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  }
  plt <- add_curve_interval(
    plt,
    lower_var = if (ci) ".lower_ci" else NULL,
    upper_var = ".upper_ci",
    ribbon_alpha = alpha
  )
  plt +
    ggplot2::labs(
      x = xlab %||% unique(object$.focal),
      y = ylab %||%
        paste0("ALE of ", info$parameter_label %||% unique(object$.parameter)),
      caption = if (info$scale == "link") "Link scale" else "Response scale"
    )
}
