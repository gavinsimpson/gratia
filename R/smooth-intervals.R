# Canonical interval names shared by the model-aware smooth interfaces.
normalize_smooth_interval <- function(interval, allow_null = FALSE) {
  if (is.null(interval) && allow_null) return(NULL)
  if (identical(interval, c("confidence", "simultaneous"))) interval <- interval[1L]
  interval <- match.arg(interval, c("confidence", "simultaneous", "credible"))
  if (interval == "credible") "confidence" else interval
}

validate_smooth_interval <- function(interval, level, n_sim, n_cores, seed) {
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) {
    cli::cli_abort("{.arg ci_level} must be a number strictly between 0 and 1.")
  }
  if (interval != "simultaneous") return(invisible(NULL))
  for (nm in c("n_sim", "n_cores")) {
    value <- get(nm)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 1 || value > .Machine$integer.max || value != floor(value)) {
      cli::cli_abort("{.arg {nm}} must be a positive integer.")
    }
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L ||
      !is.finite(seed) || abs(seed) > .Machine$integer.max || seed != trunc(seed))) {
    cli::cli_abort("{.arg seed} must be NULL or a single integer.")
  }
}

# Use precisely the design that supplies component-wise smooth uncertainty.
# Keep the fitted effect centre separate: overall mean uncertainty need not
# change the centre. Returning active columns preserves existing SE arithmetic.
smooth_uncertainty_design <- function(smooth, X, model, V,
                                      overall_uncertainty = TRUE) {
  cols <- smooth$first.para:smooth$last.para
  if (nrow(X) == 0L) return(list(X = X, cols = cols))
  n_cons <- attr(smooth, "nCons")
  if (isTRUE(overall_uncertainty) && !is.null(n_cons) && n_cons > 0L) {
    column_means <- model$cmX
    if (length(column_means) < ncol(V)) {
      column_means <- c(column_means, rep(0, ncol(V) - length(column_means)))
    }
    Xcm <- matrix(column_means, nrow(X), ncol(V), byrow = TRUE)
    if (!is.null(smooth$meanL1)) Xcm <- Xcm / smooth$meanL1
    Xcm[, cols] <- X
    eta_idx <- lss_eta_index(model)
    idx <- vapply(eta_idx, function(i) any(cols %in% i), logical(1))
    cols <- unique(unlist(eta_idx[idx]))
    X <- Xcm[, cols, drop = FALSE]
  }
  list(X = X, cols = cols)
}

# Work after smooth evaluation, so clipping, factor levels, raw coordinates
# and higher-dimensional slices are identical to the eventual plotting grids.
add_smooth_intervals <- function(estimates, smooths, model, interval, ci_level,
                                 unconditional, frequentist, overall_uncertainty,
                                 n_sim, n_cores, seed, interval_method = "gaussian",
                                 shape_specs = NULL) {
  interval <- normalize_smooth_interval(interval)
  validate_smooth_interval(interval, ci_level, n_sim, n_cores, seed)
  if (interval == "simultaneous" && inherits(model, "scam")) {
    cli::cli_abort("Simultaneous smooth intervals are not supported for SCAM models.")
  }
  calculate <- function() {
    V <- deviations <- NULL
    if (interval == "simultaneous") {
      V <- get_vcov(model, unconditional = unconditional, frequentist = frequentist)
      valid <- any(vapply(estimates, function(x) {
        d <- dplyr::bind_rows(x$data)
        if (!nrow(d)) return(FALSE)
        any(is.finite(d$.estimate) & is.finite(d$.se) & d$.se > 0)
      }, logical(1)))
      if (valid) deviations <- simultaneous_coefficient_deviations(V, n_sim, n_cores)
    }
    for (i in seq_along(estimates)) {
      pieces <- estimates[[i]]$data
      sizes <- vapply(pieces, nrow, integer(1))
      d <- dplyr::bind_rows(pieces)
      if (!nrow(d)) next
      if (interval == "confidence" && interval_method == "bootstrap") {
        X <- smooth_predict_matrix(smooths[[i]], d, model)
        cols <- smooths[[i]]$first.para:smooths[[i]]$last.para
        band <- scasm_bootstrap_intervals(model, X, d$.estimate, d$.se,
          level = ci_level, cols = cols)
        critical <- band$critical
        lower <- band$lower
        upper <- band$upper
      } else if (interval == "confidence") {
        critical <- coverage_normal(ci_level)
        lower <- d$.estimate - critical * d$.se
        upper <- d$.estimate + critical * d$.se
      } else {
        # All needed model coordinates are already evaluated by eval_smooth().
        # Do not evaluate expressions again from raw/nuisance grid columns.
        X <- smooth_predict_matrix(smooths[[i]], d, model)
        design <- smooth_uncertainty_design(smooths[[i]], X, model, V,
          overall_uncertainty = overall_uncertainty)
        full_X <- matrix(0, nrow(X), ncol(V))
        full_X[, design$cols] <- design$X
        band <- simultaneous_intervals(d$.estimate, d$.se, full_X, V,
          level = ci_level, n_sim = n_sim, n_cores = n_cores,
          deviations = deviations)
        critical <- band$critical
        lower <- band$lower
        upper <- band$upper
      }
      d$.crit <- rep(unname(critical), nrow(d))
      d$.lower_ci <- lower
      d$.upper_ci <- upper
      if (!is.null(shape_specs)) {
        d <- scasm_enforce_smooth_band(d, smooths[[i]], shape_specs[[i]],
          interval, ci_level)
      }
      start <- 0L
      for (j in seq_along(pieces)) {
        estimates[[i]]$data[[j]] <- d[start + seq_len(sizes[j]), , drop = FALSE]
        start <- start + sizes[j]
      }
    }
    estimates
  }
  if (is.null(seed) || interval == "confidence") calculate() else {
    withr::with_seed(seed, calculate())
  }
}

# Plotting saved estimates must not pretend to calibrate bands without a model.
validate_saved_smooth_interval <- function(object, interval) {
  info <- attr(object, "interval_info")
  bounds <- c(".lower_ci", ".upper_ci")
  has_bounds <- if ("data" %in% names(object) && !".estimate" %in% names(object)) {
    all(vapply(object$data, function(d) all(bounds %in% names(d)), logical(1)))
  } else all(bounds %in% names(object))
  if (!is.null(info) && !has_bounds) {
    cli::cli_abort(c("Stored interval metadata require both interval-bound columns.",
      "i" = "Recompute intervals from the fitted model with smooth_estimates()."))
  }
  if (!is.null(interval)) {
    interval <- normalize_smooth_interval(interval)
    if (interval == "simultaneous" &&
        (is.null(info) || info$interval != "simultaneous")) {
      cli::cli_abort(c("Simultaneous bands require the fitted model.",
        "i" = "Use smooth_estimates(model, interval = 'simultaneous') before drawing saved estimates."))
    }
    if (!is.null(info) && interval != info$interval) {
      cli::cli_abort("Requested interval differs from the intervals already stored in {.arg object}.")
    }
  }
  info
}
