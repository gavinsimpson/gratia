# Choose the coordinate explicitly: a smooth expression and its raw inputs
# are different differentiation variables (e.g. log(x) versus x).
derivative_focal <- function(model, smooth, focal = NULL, wrt = "smooth") {
  coords <- smooth_variable(smooth)
  if (wrt == "covariate") {
    coords <- unique(unlist(lapply(coords, function(x) all.vars(term_expression(x, model)))))
    coords <- intersect(coords, names(model$var.summary))
  }
  # Factor coordinates index curves; they are not differentiation variables.
  mf <- model$model
  coords <- coords[!vapply(coords, function(x) is.factor(mf[[x]]), logical(1))]
  if (is.null(focal)) {
    if (wrt == "covariate" && length(coords) != 1L) {
      cli::cli_abort("Specify {.arg focal} for a smooth with multiple raw covariates.")
    }
    focal <- coords[1L]
  }
  if (length(focal) != 1L || is.na(focal) || !focal %in% coords) {
    cli::cli_abort("Focal variable {.val {focal}} is not in smooth {.val {smooth$label}} on the {.val {wrt}} scale.")
  }
  focal
}

# Generate a slice in the requested coordinate. Do not invert transformations.
smooth_derivative_data <- function(model, smooth, n, focal, wrt, order, type, eps) {
  if (wrt == "covariate") {
    ref <- recover_raw_data(model)
    vars <- unique(unlist(lapply(terms_in_smooth(smooth), function(x) all.vars(term_expression(x, model)))))
    ref <- ref[, intersect(vars, names(ref)), drop = FALSE]
  } else {
    ref <- prepare_smooth_data(model, smooth, model$model)
    ref <- ref[, terms_in_smooth(smooth), drop = FALSE]
  }
  vals <- lapply(names(ref), function(nm) {
    x <- ref[[nm]]
    if (nm == focal) return(seq_min_max_eps(range(x, na.rm = TRUE), n = n,
      eps = eps, order = order, type = type))
    if (is.factor(x)) {
      if (identical(nm, by_variable(smooth)) && is_factor_by_smooth(smooth)) {
        return(x[match(by_level(smooth), x)])
      }
      return(unique(x))
    }
    if (identical(nm, by_variable(smooth))) return(1)
    value_closest_to_median(x)
  })
  names(vals) <- names(ref)
  out <- tidyr::expand_grid(!!!vals)
  if (wrt == "smooth") attr(out, "gratia.evaluated") <- TRUE
  out
}

# Resolve transformations before sending coordinates to a fresh R session.
# Coefficient draws retain the existing full-model simulation order, but only
# the selected columns cross the process boundary.
prepare_derivative_task <- function(model, sm, data, focal, wrt, type, order,
                                    h, betas, V, interval, interval_method,
                                    level, n_sim, ncores) {
  if (!is.numeric(data[[focal]]) || is.matrix(data[[focal]])) {
    cli::cli_abort("The differentiation coordinate must be a numeric vector.")
  }
  stencil <- smooth_derivative_stencil(model, sm, data, focal, wrt, type, order, h)
  idx <- smooth_coef_indices(sm)
  if (interval == "simultaneous" || interval_method == "bootstrap") {
    validate_smooth_interval(interval, level, n_sim, ncores, NULL)
  }
  bootstrap <- if (interval_method == "bootstrap") {
    bs <- model$bs[idx, , drop = FALSE]
    Ve <- if (!is.null(attr(model$bs, "svar.only")) && !is.null(model$Ve)) {
      model$Ve[idx, idx, drop = FALSE]
    } else NULL
    list(bs = bs, Ve = Ve)
  } else NULL
  list(sm = smooth_task_smooth(sm),
    data = smooth_task_data(data[, unique(c(terms_in_smooth(sm), focal)), drop = FALSE]),
    stencil = stencil, model = smooth_matrix_model(model),
    betas = betas[idx], V = V[idx, idx, drop = FALSE],
    h = h, order = order, focal = focal, interval = interval,
    level = level, bootstrap = bootstrap)
}

calculate_smooth_derivative <- function(sm, data, stencil, model, betas, V,
                                        h, order, focal, interval, level,
                                        bootstrap) {
  X <- smooth_derivative_matrix(sm, stencil, model, h, order, rownames(data), names(betas))
  d <- compute_derivative(sm, X, betas, V, data, focal)
  if (!is.null(bootstrap)) {
    band <- scasm_bootstrap_limits(X, d$deriv$.derivative, d$deriv$.se,
      betas, bootstrap$bs, bootstrap$Ve, level)
    return(tibble::add_column(d$deriv, .crit = rep(NA_real_, nrow(d$deriv)),
      .lower_ci = band$lower, .upper_ci = band$upper))
  }
  if (interval == "confidence") {
    return(derivative_pointwise_int(d$deriv, level, "normal"))
  }
  list(data = d$deriv, X = d$Xi, V = V)
}

smooth_derivative_stencil <- function(model, sm, data, focal, wrt, type, order, h) {
  deltas <- if (order == 1L) {
    switch(type, forward = c(h, 0), backward = c(0, -h),
      central = c(h / 2, -h / 2))
  } else {
    switch(type, forward = c(2 * h, h, 0), backward = c(0, -h, -2 * h),
      central = c(h, 0, -h))
  }
  lapply(deltas, function(delta) {
    d <- data
    d[[focal]] <- d[[focal]] + delta
    attr(d, "gratia.evaluated") <- identical(wrt, "smooth")
    attr(d, "terms") <- NULL
    smooth_task_data(prepare_smooth_data(model, sm, d)[, terms_in_smooth(sm), drop = FALSE])
  })
}

smooth_derivative_matrix <- function(sm, stencil, model, h, order, row_names, coef_names) {
  has_offset <- FALSE
  matrices <- lapply(stencil, function(d) {
    X <- smooth_predict_matrix(sm, d, model)
    off <- attr(X, "offset", exact = TRUE)
    if (!is.null(off)) has_offset <<- TRUE
    cbind(X, if (is.null(off)) rep(0, nrow(X)) else off)
  })
  X <- if (order == 1L) {
    (matrices[[1L]] - matrices[[2L]]) / h
  } else {
    (matrices[[1L]] - 2 * matrices[[2L]] + matrices[[3L]]) / h^2
  }
  offset <- X[, ncol(X)]
  X <- X[, -ncol(X), drop = FALSE]
  dimnames(X) <- list(row_names, coef_names)
  if (has_offset) attr(X, "offset") <- offset
  X
}

# Retain full coefficient order for callers of the finite-difference helper.
smooth_finite_difference <- function(model, smooth, data, focal, wrt, type, order, h) {
  if (!is.numeric(data[[focal]]) || is.matrix(data[[focal]])) {
    cli::cli_abort("The differentiation coordinate must be a numeric vector.")
  }
  stencil <- smooth_derivative_stencil(model, smooth, data, focal, wrt, type, order, h)
  idx <- smooth_coef_indices(smooth)
  X <- smooth_derivative_matrix(smooth, stencil, model, h, order,
    rownames(data), names(stats::coef(model))[idx])
  out <- matrix(0, nrow(X), length(stats::coef(model)),
    dimnames = list(rownames(data), names(stats::coef(model))))
  out[, idx] <- X
  if (!is.null(attr(X, "offset"))) attr(out, "offset") <- attr(X, "offset")
  out
}
