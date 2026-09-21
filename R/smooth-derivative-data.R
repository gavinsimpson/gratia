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
    cli::cli_abort("{.arg focal} must name a {.val {wrt}} coordinate of {.val {smooth$label}}.")
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

# Evaluate the selected smooth at each finite-difference point. This keeps
# offsets and unrelated smooths out of the calculation, and recomputes calls
# after raw covariates change instead of reusing stale transformed columns.
smooth_finite_difference <- function(model, smooth, data, focal, wrt, type, order, h) {
  if (!is.numeric(data[[focal]]) || is.matrix(data[[focal]])) {
    cli::cli_abort("The differentiation coordinate must be a numeric vector.")
  }
  at <- function(delta) {
    d <- data
    d[[focal]] <- d[[focal]] + delta
    attr(d, "gratia.evaluated") <- identical(wrt, "smooth")
    attr(d, "terms") <- NULL
    d <- prepare_smooth_data(model, smooth, d)
    smooth_predict_matrix(smooth, d, model)
  }
  # Use the same forward, backward, and central stencils as existing methods.
  if (order == 1L) {
    ans <- switch(type, forward = (at(h) - at(0)) / h,
      backward = (at(0) - at(-h)) / h,
      central = (at(h / 2) - at(-h / 2)) / h)
  } else {
    ans <- switch(type, forward = (at(2 * h) - 2 * at(h) + at(0)) / h^2,
      backward = (at(0) - 2 * at(-h) + at(-2 * h)) / h^2,
      central = (at(h) - 2 * at(0) + at(-h)) / h^2)
  }
  # Existing interval routines consume a matrix in full coefficient order.
  out <- matrix(0, nrow(ans), length(coef(model)),
    dimnames = list(rownames(data), names(coef(model))))
  out[, smooth_coef_indices(smooth)] <- ans
  out
}
