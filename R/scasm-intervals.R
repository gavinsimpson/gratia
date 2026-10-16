# scasm deliberately retains the gam class. Its constraint storage and feasible
# starting coefficients distinguish its fits, including unconstrained tensors.
is_scasm_model <- function(model) {
  p <- length(stats::coef(model))
  A <- model$Ain
  b <- model$bin
  inherits(model, "gam") && is.matrix(A) && is.numeric(A) &&
    ncol(A) == p && is.numeric(b) && length(b) == nrow(A) &&
    is.numeric(model$beta0) && length(model$beta0) == p &&
    all(is.finite(A)) && all(is.finite(b)) && all(is.finite(model$beta0))
}

valid_scasm_bootstrap <- function(model) {
  is_scasm_model(model) && is.matrix(model$bs) && is.numeric(model$bs) &&
    nrow(model$bs) == length(stats::coef(model)) && ncol(model$bs) > 0L &&
    all(is.finite(model$bs))
}

scasm_interval_method <- function(model,
    interval_method = c("auto", "gaussian", "bootstrap"),
    interval = "confidence", supported = TRUE) {
  interval_method <- match.arg(interval_method)
  available <- valid_scasm_bootstrap(model)
  if (interval_method == "gaussian") return("gaussian")
  if (interval == "simultaneous") {
    if (interval_method == "bootstrap") {
      cli::cli_abort(c("Simultaneous bootstrap bands are not supported.",
        "i" = "Use pointwise bootstrap intervals or interval_method = 'gaussian'."))
    }
    if (available) {
      cli::cli_warn("Simultaneous bands use Gaussian uncertainty; stored scasm bootstrap draws are not used.")
    }
    return("gaussian")
  }
  if (interval_method == "bootstrap" && !available) {
    cli::cli_abort("Bootstrap intervals require valid stored scasm coefficient draws; fit the model with bs > 0.")
  }
  if (!supported && (available || interval_method == "bootstrap")) {
    if (interval_method == "bootstrap") {
      cli::cli_abort("Bootstrap intervals are not supported for this family prediction; use interval_method = 'gaussian'.")
    }
    cli::cli_warn("Bootstrap intervals are not supported for this family prediction; using Gaussian intervals.")
    return("gaussian")
  }
  if (available) "bootstrap" else "gaussian"
}

scasm_interval_info <- function(model, method, interval, level) {
  list(interval = interval, level = level, method = method,
    correction = if (method == "bootstrap" &&
      !is.null(attr(model$bs, "svar.only")) && !is.null(model$Ve)) {
      "svar.only"
    } else "none")
}

# Follow predict.gam's percentile limits and optional smoothing-bias correction.
# A fixed offset contributes to the limits but does not change their variance.
scasm_bootstrap_intervals <- function(model, X, estimate, se, level = 0.95,
                                      cols = seq_along(stats::coef(model))) {
  if (!valid_scasm_bootstrap(model)) {
    cli::cli_abort("Bootstrap intervals require valid stored scasm coefficient draws.")
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) {
    cli::cli_abort("{.arg level} must be a number strictly between 0 and 1.")
  }
  if (!is.matrix(X) || !is.numeric(X) || nrow(X) != length(estimate) ||
      length(se) != length(estimate) || ncol(X) != length(cols)) {
    cli::cli_abort("Incompatible bootstrap estimates, standard errors and prediction matrix.")
  }
  scasm_bootstrap_limits(X, estimate, se, stats::coef(model)[cols],
    model$bs[cols, , drop = FALSE],
    if (is.null(attr(model$bs, "svar.only")) || is.null(model$Ve)) NULL else
      model$Ve[cols, cols, drop = FALSE], level)
}

# Inputs have already been validated against the fitted model. Keeping only
# the required columns avoids copying all bootstrap coefficients to workers.
scasm_bootstrap_limits <- function(X, estimate, se, coefficients, bs, Ve, level) {
  lower <- upper <- rep(NA_real_, length(estimate))
  names(lower) <- names(upper) <- names(estimate)
  valid <- is.finite(estimate) & is.finite(se) & rowSums(!is.finite(X)) == 0L
  if (any(valid)) {
    XX <- X[valid, , drop = FALSE]
    ff <- XX %*% bs
    offset <- as.numeric(estimate[valid]) - drop(XX %*% coefficients)
    ff <- ff + offset
    delta <- if (is.null(Ve)) {
      0
    } else {
      mean(se[valid] - sqrt(pmax(0,
        rowSums((XX %*% Ve) * XX))))
    }
    alpha <- (1 - level) / 2
    adjustment <- delta * stats::qnorm(1 - alpha)
    lower[valid] <- apply(ff, 1L, stats::quantile, probs = alpha, type = 7) - adjustment
    upper[valid] <- apply(ff, 1L, stats::quantile, probs = 1 - alpha, type = 7) + adjustment
  }
  list(lower = lower, upper = upper, critical = NA_real_)
}
