#' Conditional differences of fitted values from a GAM
#'
#' Compare full fitted values between pairs of factor levels, conditional on
#' covariate values. Parametric effects and smooths are included unless excluded
#' explicitly. Unlike [difference_smooths()], these are differences of predictions,
#' rather than differences of selected smooth functions.
#'
#' @param by character vector naming factors that define comparison groups.
#'   For multiple factors, groups are joint combinations of their levels and
#'   all pairs of groups are compared. To compare one factor within levels of
#'   another, put the latter in `condition` instead.
#' @param condition character vector or list specifying evaluation covariates,
#'   as in [conditional_values()]. The first non-comparison covariate supplies
#'   the plotting axis. Up to four non-comparison conditions are supported.
#'   Named elements for factors in `by` restrict their comparison levels, e.g.
#'   `list("x", treatment = c("control", "high"))`. Other covariates are held
#'   at [typical_values()]. At least one non-comparison condition is required.
#' @param complete logical; include all combinations of comparison and
#'   conditioning factors? With `FALSE`, retain only combinations observed in
#'   the fitted data, and compare groups only within shared conditioning strata.
#'   This does not restrict numeric grids to overlapping observed ranges.
#' @param uncertainty character; `"delta"` uses analytic link-scale or
#'   delta-method response-scale standard errors and normal intervals.
#'   `"simulation"` uses shared posterior coefficient draws, with equal-tailed
#'   quantiles for pointwise intervals or calibrated symmetric simultaneous bands.
#' @param interval character; `"confidence"` (the default) gives pointwise
#'   intervals; `"simultaneous"` gives simultaneous intervals over the evaluated
#'   covariate combinations.
#' @param simultaneous_scope character; `"contrast"` calibrates a separate band
#'   for each pair, jointly over all its evaluation points and conditioning
#'   strata. `"all"` calibrates one band jointly over every returned comparison
#'   and evaluation point. Ignored for pointwise intervals.
#' @param n_sim integer; number of draws for posterior simulation or calibration
#'   of delta-method simultaneous intervals. Ignored for pointwise delta intervals
#'   and for simulation with `method = "user"`, which uses all supplied draws.
#' @param method character; posterior sampler used by [fitted_samples()].
#'   `"gaussian"` uses the Gaussian posterior approximation; `"mh"` uses
#'   Metropolis-Hastings; `"user"` uses supplied coefficient draws. `"inla"`
#'   is reserved by the sampling machinery but is not implemented.
#' @param draws matrix of posterior coefficient draws, one draw per row in model
#'   coefficient order, used with `method = "user"`. At least two are required.
#' @param seed integer or `NULL`; seed for posterior simulation or delta-method
#'   simultaneous calibration. An explicit seed preserves the caller's RNG state.
#'   With `NULL`, delta-method calibration advances the current RNG state;
#'   posterior simulation follows [fitted_samples()] seed handling.
#' @param n_cores integer; number of cores used by [mvnfast::rmvn()] for
#'   delta-method simultaneous calibration, or passed to [fitted_samples()].
#' @param unconditional logical; include smoothing-parameter uncertainty in the
#'   Bayesian covariance, if available, for delta intervals and Gaussian draws.
#'   This argument does not modify MH or user-supplied draws.
#' @param ... arguments passed to prediction, such as `exclude`. Arguments
#'   `type`, `newdata`, `se.fit`, and `terms` are not supported here.
#' @inheritParams conditional_values
#' @inheritParams post_draws
#' @inheritParams smooth_estimates
#'
#' @details
#' Differences are group 1 minus group 2, with groups ordered by model factor
#' levels. Both groups use the same values of all other covariates. On the
#' response scale the inverse link is applied to each prediction before
#' subtraction. Shared terms can therefore affect response-scale differences
#' even when they cancel on the link scale.
#'
#' Intervals describe uncertainty in conditional mean differences, not observation
#' noise. Simulation uses [fitted_samples()] once for the entire prediction
#' grid, retaining dependence between predictions by subtracting within draws.
#' The estimate is always the difference of fitted predictions. Pointwise
#' simulation intervals need not be centred on this estimate.
#'
#' Simultaneous intervals use the `ci_level` quantile of the maximum absolute
#' standardized deviation over the selected coverage set. They are symmetric
#' about `.diff`, with half-width `.crit * .se`. For `uncertainty = "delta"`,
#' one shared batch of zero-mean Gaussian coefficient deviations is generated
#' from the Bayesian covariance matrix using [mvnfast::rmvn()]. Response-scale
#' differences use a first-order Taylor approximation. The covariance must be
#' positive definite. Sampler arguments `method`, `draws`, `mvn_method`, `burnin`,
#' `thin`, `t_df`, and `rw_scale` apply only to `uncertainty = "simulation"`.
#'
#' For `uncertainty = "simulation"`, deviations are posterior difference draws
#' minus `.diff`, standardized by their sample standard deviations. Draws are
#' transformed to the response scale before differencing; invalid inverse-link
#' domains or response means cause an error rather than discarded draws.
#' Zero-variance draws that disagree with `.diff` cannot calibrate a band and
#' cause an error. Deterministic rows agreeing with `.diff` have zero width.
#'
#' Simultaneous coverage is approximate posterior coverage on the finite
#' evaluation grid, not between its points or outside its range, nor exact
#' frequentist coverage. Increase `n_vals` to evaluate a denser grid. Bands are
#' not clipped to the response-difference range (e.g. [-1, 1] for probabilities).
#' They need not contain the equal-tailed pointwise simulation intervals.
#'
#' `exclude` sets the named term contributions to zero. In particular, excluding
#' random effects does not integrate over their distribution. Offsets follow
#' [mgcv::predict.gam()] semantics. Supported models are `gam` and `bam` fits
#' with a single linear predictor and an ordinary scalar inverse-link mean
#' (including negative binomial, Tweedie, beta regression and scaled t families).
#'
#' @return A tibble of class `"conditional_differences"`, with `.contrast`,
#'   `.diff`, `.se`, `.lower_ci`, `.upper_ci`, and evaluation covariates.
#'   `.se` is on the requested scale; for simulation it is the standard deviation
#'   of difference draws. A single comparison factor has `.level_1` and
#'   `.level_2` columns. Multiple factors have `<factor>_1` and `<factor>_2`
#'   columns. Attributes record `by`, `condition`, `scale`, `uncertainty`,
#'   `ci_level`, `exclude`, `interval`, `simultaneous_scope`, and plotting
#'   `channels`. Simultaneous results also contain `.crit`, the calibrated
#'   critical value, constant within each coverage set.
#' @export
#' @examples
#' load_mgcv()
#' df <- data_sim("eg4", seed = 2)
#' m <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")
#' cd <- conditional_differences(m, by = "fac", condition = "x2")
#' draw(cd)
#'
#' # Restrict comparisons and fix a further covariate
#' conditional_differences(m, by = "fac",
#'   condition = list("x2", fac = c("1", "3"), x0 = 0.5))
#'
#' # Pointwise posterior bands from shared coefficient draws
#' conditional_differences(m, by = "fac", condition = "x2",
#'   uncertainty = "simulation", n_sim = 1000, seed = 42)
#'
#' # Link-scale simultaneous bands, separately for each comparison
#' conditional_differences(m, by = "fac", condition = "x2", scale = "link",
#'   interval = "simultaneous", n_vals = 200, n_sim = 1000, seed = 42)
#'
#' # Nonlinear response differences: simultaneous coverage across all pairs
#' set.seed(42)
#' df$count <- rpois(nrow(df), exp(0.2 + sin(3 * df$x2) + as.integer(df$fac) / 3))
#' mp <- gam(count ~ fac + s(x2, by = fac), family = poisson(),
#'   data = df, method = "REML")
#' conditional_differences(mp, by = "fac", condition = "x2", scale = "response",
#'   uncertainty = "simulation", interval = "simultaneous",
#'   simultaneous_scope = "all", n_vals = 200, n_sim = 1000, seed = 42) |>
#'   draw()
conditional_differences <- function(model, by, condition = NULL, data = NULL,
                                    scale = c("response", "link", "linear_predictor"),
                                    ...) {
  UseMethod("conditional_differences")
}

#' @export
#' @rdname conditional_differences
conditional_differences.gam <- function(model, by, condition = NULL, data = NULL,
    scale = c("response", "link", "linear_predictor"), n_vals = 100,
    ci_level = 0.95, complete = TRUE, unconditional = FALSE,
    uncertainty = c("delta", "simulation"), n_sim = 10000, seed = NULL,
    n_cores = 1, method = c("gaussian", "mh", "inla", "user"), draws = NULL,
    mvn_method = c("mvnfast", "mgcv"), burnin = 1000, thin = 1,
    t_df = 40, rw_scale = 0.25, envir = NULL, ...,
    interval = c("confidence", "simultaneous"),
    simultaneous_scope = c("contrast", "all")) {
  scale <- match.arg(scale)
  uncertainty <- match.arg(uncertainty)
  interval <- match.arg(interval)
  simultaneous_scope <- match.arg(simultaneous_scope)
  simultaneous <- identical(interval, "simultaneous")
  model <- with_model_envir(model, envir)
  fam <- stats::family(model)
  supported <- c("gaussian", "poisson", "binomial", "gamma",
    "inverse_gaussian", "quasi", "quasipoisson", "quasibinomial",
    "negative_binomial", "tweedie", "beta_regression", "scaled_t")
  if (inherits(model, "scam") || inherits(fam, "general.family") ||
      !family_type(model) %in% supported) {
    cli::cli_abort("Conditional differences are not supported for this model family or class.")
  }
  if (missing(by) || !is.character(by) || !length(by) || anyNA(by) ||
      anyDuplicated(by) || !all(by %in% model_vars(model)) ||
      !all(vapply(model$var.summary[by], is.factor, logical(1L)))) {
    cli::cli_abort("{.arg by} must name one or more distinct factors in the model.")
  }
  positive_integer <- function(x) is.numeric(x) && length(x) == 1L &&
    is.finite(x) && x >= 1 && x <= .Machine$integer.max && x == trunc(x)
  if (!positive_integer(n_vals)) cli::cli_abort("{.arg n_vals} must be a positive integer.")
  if (!is.numeric(ci_level) || length(ci_level) != 1L || !is.finite(ci_level) ||
      ci_level <= 0 || ci_level >= 1) {
    cli::cli_abort("{.arg ci_level} must be strictly between 0 and 1.")
  }
  if (!is.logical(unconditional) || length(unconditional) != 1L ||
      is.na(unconditional)) cli::cli_abort("{.arg unconditional} must be TRUE or FALSE.")
  dots <- list(...)
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    cli::cli_abort("Arguments in {.arg ...} must be named.")
  }
  if (any(names(dots) %in% c("type", "newdata", "se.fit", "terms"))) {
    cli::cli_abort("{.arg type}, {.arg newdata}, {.arg se.fit}, and {.arg terms} cannot be supplied; use {.arg exclude} to omit terms.")
  }
  if (uncertainty == "simulation") {
    method <- match.arg(method)
    mvn_method <- match.arg(mvn_method)
    if (method == "user" && (!is.matrix(draws) || !is.numeric(draws) ||
        nrow(draws) < 2L || ncol(draws) != length(stats::coef(model)) ||
        any(!is.finite(draws)))) {
      cli::cli_abort("{.arg draws} must be a finite numeric matrix with at least two rows and one column per model coefficient.")
    }
  }
  if (uncertainty == "simulation" || simultaneous) {
    if (!(uncertainty == "simulation" && method == "user") && (!positive_integer(n_sim) || n_sim < 2)) {
      cli::cli_abort("{.arg n_sim} must be an integer of at least 2.")
    }
    if (!positive_integer(n_cores)) cli::cli_abort("{.arg n_cores} must be a positive integer.")
    if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L ||
        !is.finite(seed) || abs(seed) > .Machine$integer.max || seed != trunc(seed))) {
      cli::cli_abort("{.arg seed} must be NULL or a single integer.")
    }
  }
  grid <- conditional_prediction_grid(model, condition, data, n_vals, complete,
    by = by)
  pd <- grid$data
  if (!nrow(pd) || anyNA(pd)) {
    cli::cli_abort("The conditional prediction grid must be non-empty and contain no missing values.")
  }
  groups <- unique(pd[by])
  # expand_grid() preserves model level order for comparison factors.
  groups <- groups[do.call(order, groups), , drop = FALSE]
  if (nrow(groups) < 2L) cli::cli_abort("At least two comparison groups are required.")
  group_id <- vctrs::vec_match(pd[by], groups)
  keys <- setdiff(names(pd), by)
  endpoints <- if (length(by) == 1L) c(".level_1", ".level_2") else {
    as.vector(outer(by, c("_1", "_2"), paste0))
  }
  reserved <- c(".contrast", ".diff", ".se", ".lower_ci", ".upper_ci", endpoints)
  if (simultaneous) reserved <- c(reserved, ".crit")
  if (any(keys %in% reserved)) {
    cli::cli_abort("Covariate names conflict with output columns: {.val {intersect(keys, reserved)}}.")
  }
  pairs <- utils::combn(seq_len(nrow(groups)), 2L)
  matched <- lapply(seq_len(ncol(pairs)), function(i) {
    a <- which(group_id == pairs[1L, i])
    b <- which(group_id == pairs[2L, i])
    idx <- vctrs::vec_match(pd[a, keys, drop = FALSE], pd[b, keys, drop = FALSE])
    keep <- !is.na(idx)
    list(a = a[keep], b = b[idx[keep]])
  })
  keep <- lengths(lapply(matched, `[[`, "a")) > 0L
  pairs <- pairs[, keep, drop = FALSE]
  matched <- matched[keep]
  if (!length(matched)) {
    cli::cli_abort("No comparison groups share conditioning values.")
  }
  eta <- as.numeric(predict_model(model, newdata = pd, type = "link", ...))
  if (length(eta) != nrow(pd) || any(!is.finite(eta))) {
    cli::cli_abort("Predictions must be finite and retain every prediction-grid row.")
  }
  response <- identical(scale, "response")
  fitted <- if (response && simultaneous) {
    difference_inverse_link(eta, fam)
  } else if (response) fam$linkinv(eta) else eta
  if (any(!is.finite(fitted))) cli::cli_abort("Non-finite predictions on the requested scale.")
  if (uncertainty == "delta") {
    X <- predict_model(model, newdata = pd, type = "lpmatrix", ...)
    V <- get_vcov(model, unconditional = unconditional)
    if (response) X <- X * as.numeric(fam$mu.eta(eta))
    crit <- coverage_normal(ci_level)
    if (simultaneous) {
      generate <- function() simultaneous_coefficient_deviations(V, n_sim, n_cores)
      deviations <- if (is.null(seed)) generate() else withr::with_seed(seed, generate())
    }
  } else {
    # One call ensures every scenario uses the same coefficient draw. Never
    # sample independently per group or treat these as posterior observations.
    simulate <- function() fitted_samples(model, n = n_sim, data = pd, seed = seed,
      scale = if (response && !simultaneous) "response" else "linear_predictor",
      method = method, n_cores = n_cores, draws = draws,
      unconditional = unconditional, mvn_method = mvn_method,
      burnin = burnin, thin = thin, t_df = t_df, rw_scale = rw_scale, ...)
    fs <- if (is.null(seed)) simulate() else withr::with_seed(seed, simulate())
    fs <- fs[order(fs$.draw, fs$.row), ]
    n_draw <- length(unique(fs$.draw))
    if (n_draw < 2L || nrow(fs) != nrow(pd) * n_draw ||
        any(!is.finite(fs$.fitted))) {
      cli::cli_abort("Simulation must return at least two finite fitted draws for every grid row.")
    }
    samples <- matrix(fs$.fitted, nrow = nrow(pd))
    if (response && simultaneous) samples <- difference_inverse_link(samples, fam)
    probs <- c((1 - ci_level) / 2, (1 + ci_level) / 2)
  }
  out <- vector("list", length(matched))
  maxima <- vector("list", length(matched))
  for (i in seq_along(matched)) {
    a <- matched[[i]]$a
    b <- matched[[i]]$b
    diff <- fitted[a] - fitted[b]
    if (simultaneous) {
      summary <- if (uncertainty == "delta") {
        difference_simultaneous_summary(diff, a, b, X = X, V = V,
          deviations = deviations)
      } else {
        difference_simultaneous_summary(diff, a, b, samples = samples)
      }
      se <- summary$se
      maxima[[i]] <- summary$maxima
      lower <- upper <- rep(NA_real_, length(diff))
    } else if (uncertainty == "delta") {
      D <- X[a, , drop = FALSE] - X[b, , drop = FALSE]
      se <- sqrt(pmax(0, rowSums((D %*% V) * D)))
      lower <- diff - crit * se
      upper <- diff + crit * se
    } else {
      ds <- samples[a, , drop = FALSE] - samples[b, , drop = FALSE]
      se <- apply(ds, 1L, stats::sd)
      bounds <- vapply(seq_len(nrow(ds)), function(j) {
        stats::quantile(ds[j, ], probs = probs, names = FALSE)
      }, numeric(2L))
      lower <- bounds[1L, ]
      upper <- bounds[2L, ]
    }
    tab <- tibble(.contrast = i)
    for (nm in by) {
      prefix <- if (length(by) == 1L) ".level" else nm
      tab[[paste0(prefix, "_1")]] <- groups[[nm]][pairs[1L, i]]
      tab[[paste0(prefix, "_2")]] <- groups[[nm]][pairs[2L, i]]
    }
    tab <- tab[rep(1L, length(a)), ]
    out[[i]] <- dplyr::bind_cols(tab, tibble(.diff = diff, .se = se,
      .lower_ci = lower, .upper_ci = upper), pd[a, keys, drop = FALSE])
  }
  if (simultaneous) {
    if (simultaneous_scope == "all") {
      joint <- Reduce(pmax, maxima)
      maxima <- rep(list(joint), length(maxima))
    }
    for (i in seq_along(out)) {
      critical <- unname(stats::quantile(maxima[[i]], ci_level, type = 8))
      out[[i]]$.crit <- critical
      out[[i]]$.lower_ci <- out[[i]]$.diff - critical * out[[i]]$.se
      out[[i]]$.upper_ci <- out[[i]]$.diff + critical * out[[i]]$.se
    }
  }
  out <- dplyr::bind_rows(out)
  channels <- setNames(rep(list(NULL), 4L), c("x", "colour", "f_col", "f_row"))
  channels[seq_along(grid$condition)] <- grid$condition
  attr(out, "channels") <- channels
  attr(out, "by") <- by
  attr(out, "condition") <- condition
  attr(out, "scale") <- scale
  attr(out, "uncertainty") <- uncertainty
  attr(out, "interval") <- interval
  attr(out, "simultaneous_scope") <- simultaneous_scope
  attr(out, "ci_level") <- ci_level
  attr(out, "exclude") <- dots$exclude
  class(out) <- c("conditional_differences", class(out))
  out
}

#' Plot conditional differences
#'
#' Display conditional differences with pointwise or simultaneous intervals and a
#' zero reference line. Each comparison is shown in a separate facet; additional
#' conditioning variables use colour and facets as in [draw.conditional_values()].
#' @param object an object returned by [conditional_differences()].
#' @inheritParams draw.conditional_values
#' @param ... arguments passed to [draw.conditional_values()].
#' @export
#' @examples
#' load_mgcv()
#' df <- data_sim("eg4", seed = 2)
#' m <- gam(y ~ fac + s(x2, by = fac), data = df, method = "REML")
#' draw(conditional_differences(m, by = "fac", condition = "x2"))
draw.conditional_differences <- function(object, facet_scales = "fixed",
    discrete_colour = NULL, discrete_fill = NULL, xlab = NULL, ylab = NULL, ...) {
  channels <- attr(object, "channels")
  by <- attr(object, "by")
  labels <- lapply(by, function(nm) {
    prefix <- if (length(by) == 1L) ".level" else nm
    paste0(nm, ": ", object[[paste0(prefix, "_1")]], " - ",
      object[[paste0(prefix, "_2")]])
  })
  # Include the identifier so distinct comparisons with identical text labels
  # (e.g. factor levels containing separators) can never be merged into a curve.
  object$.comparison <- paste0(object$.contrast, ". ",
    do.call(paste, c(labels, sep = "; ")))
  object$.comparison <- factor(object$.comparison, levels = unique(object$.comparison))
  facets <- c(".comparison", channels$f_col, channels$f_row)
  channels$f_col <- channels$f_row <- NULL
  attr(object, "channels") <- channels
  object$.fitted <- object$.diff
  if (is.null(ylab)) {
    ylab <- paste0("Difference (", attr(object, "scale"), " scale)")
  }
  draw.conditional_values(object, facet_scales = facet_scales,
    discrete_colour = discrete_colour, discrete_fill = discrete_fill,
    xlab = xlab, ylab = ylab, ...) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(facets)),
      scales = facet_scales,
      labeller = ggplot2::labeller(
        .comparison = function(x) sub("^[0-9]+\\. ", "", x),
        .default = ggplot2::label_both))
}

#' @export
`[.conditional_differences` <- function(x, i, j, drop = FALSE) {
  nms <- c("channels", "by", "condition", "scale",
    "uncertainty", "ci_level", "exclude", "interval", "simultaneous_scope")
  metadata <- attributes(x)[intersect(nms, names(attributes(x)))]
  out <- NextMethod()
  if (is.data.frame(out)) {
    for (nm in names(metadata)) attr(out, nm) <- metadata[[nm]]
  }
  out
}

# Accumulate one maximum per draw without materialising all comparison draws.
difference_simultaneous_summary <- function(estimate, a, b, X = NULL, V = NULL,
    deviations = NULL, samples = NULL, block_size = 1000L) {
  simulation <- !is.null(samples)
  n_draw <- if (simulation) ncol(samples) else nrow(deviations)
  maxima <- numeric(n_draw)
  se <- numeric(length(estimate))
  for (start in seq.int(1L, length(estimate), by = block_size)) {
    idx <- start:min(start + block_size - 1L, length(estimate))
    tolerance <- 64 * .Machine$double.eps * pmax(1, abs(estimate[idx]))
    if (simulation) {
      ds <- samples[a[idx], , drop = FALSE] - samples[b[idx], , drop = FALSE]
      se[idx] <- apply(ds, 1L, stats::sd)
      errors <- sweep(ds, 1L, estimate[idx], FUN = "-")
      # Equal scenarios can leave round-off-sized variation after subtraction.
      deterministic <- se[idx] <= tolerance &
        apply(abs(errors), 1L, max) <= tolerance
      se[idx[deterministic]] <- 0
    } else {
      D <- X[a[idx], , drop = FALSE] - X[b[idx], , drop = FALSE]
      se[idx] <- sqrt(pmax(0, rowSums((D %*% V) * D)))
      errors <- tcrossprod(D, deviations)
    }
    maxima <- update_simultaneous_maxima(maxima, errors, se[idx], tolerance)
  }
  list(se = se, maxima = maxima)
}

# Check the domain before transforming; a finite square or reciprocal alone
# does not establish that a simulated linear predictor is on the valid branch.
difference_inverse_link <- function(eta, family) {
  invalid <- switch(family$link,
    "sqrt" = eta < 0,
    "inverse" = eta == 0,
    "1/mu^2" = eta <= 0,
    FALSE)
  valid_eta <- is.null(family$valideta) || isTRUE(family$valideta(eta))
  if (any(!is.finite(eta)) || any(invalid) || !valid_eta) {
    cli::cli_abort("Simultaneous response intervals encounter an invalid inverse-link domain; use the link scale or a suitable posterior sampler.")
  }
  mu <- family$linkinv(eta)
  valid_mu <- is.null(family$validmu) || isTRUE(family$validmu(mu))
  if (any(!is.finite(mu)) || !valid_mu) {
    cli::cli_abort("Simultaneous response intervals require finite, valid response means; use the link scale or a suitable posterior sampler.")
  }
  if (!is.null(dim(eta))) dim(mu) <- dim(eta)
  mu
}
