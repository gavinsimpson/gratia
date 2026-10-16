# Translate retained restrictions only when the coefficient restrictions imply
# the named shape of this component. Tensor bases can otherwise change sign.
scasm_shape_spec <- function(model, smooth, info) {
  if (!is_scasm_model(model) || is.null(info) || !isTRUE(info$retained)) {
    cli::cli_abort("Shape intervals require retained named scasm restrictions for every selected smooth.")
  }
  if (!identical(smooth$by, "NA") && is.null(smooth$by.level)) {
    cli::cli_abort("Shape intervals for numeric by variables are unsupported; evaluate the unmultiplied component separately.")
  }
  # In mgcv 1.9-4, the general inequality part of pc replaces Ain after
  # construction of the named constraints, but leaves xt unchanged. A retained
  # inequality matrix therefore need not contain the named restrictions.
  margins <- if (length(smooth$margin)) smooth$margin else list(smooth)
  if (any(vapply(c(list(smooth), margins),
      scasm_pc_replaces_named_constraints, logical(1)))) {
    cli::cli_abort("General pc inequalities can replace named scasm restrictions; automatic shape intervals are unsupported. Use shape_confidence_set() with the fitted restrictions.")
  }
  if (length(smooth$margin) &&
      (any(vapply(smooth$XP, Negate(is.null), logical(1))) ||
       !all(vapply(margins, inherits, logical(1), "Bspline.smooth")))) {
    cli::cli_abort(c("Tensor shape intervals require nonnegative B-spline margins without normal reparameterisation.",
      "i" = "Fit te() with np = FALSE and B-spline margins (bs = 'sc' or 'bs')."))
  }
  constraints <- list()
  for (i in seq_along(margins)) {
    margin <- margins[[i]]
    restriction <- info$margins[[i]]
    if (length(margin$term) != 1L || !inherits(margin, "Bspline.smooth")) {
      cli::cli_abort("Shape intervals currently require univariate B-spline margins.")
    }
    if (!is.null(margin$deriv) &&
        (!is.numeric(margin$deriv) || length(margin$deriv) != 1L ||
         !is.finite(margin$deriv) || margin$deriv != 0)) {
      cli::cli_abort("Automatic shape intervals require function-valued B-spline margins; derivative bases are unsupported.")
    }
    codes <- restriction$codes
    if (all(c("m+", "m-") %in% codes) || all(c("c+", "c-") %in% codes)) {
      cli::cli_abort("Shape intervals require compatible named restriction codes; contradictory requests use precedence rules in mgcv.")
    }
    if (length(codes) && !isTRUE(restriction$retained)) {
      cli::cli_abort("Shape intervals cannot enforce requests that were not retained in the fitted model.")
    }
    if (!length(codes)) next
    if (any(c("c+", "c-") %in% codes)) {
      if (margin$m[1L] < 2L) {
        cli::cli_abort("Named curvature intervals require a B-spline degree of at least two.")
      }
      spacing <- diff(margin$knots)
      # A nearly linear coefficient vector can have either curvature sign
      # under nonuniform knots. Permit rounding, not a statistical tolerance.
      if (any(!is.finite(spacing)) || any(spacing <= 0) ||
          max(abs(spacing / mean(spacing) - 1)) > 100 * .Machine$double.eps) {
        cli::cli_abort("Named scasm curvature restrictions with irregular knots do not ensure spline curvature; shape intervals are unsupported.")
      }
    }
    constraints[[margin$term]] <- codes
  }
  if (!length(constraints)) {
    cli::cli_abort("Shape intervals require named restrictions; use shape_confidence_set() for general coefficient constraints.")
  }
  if (sum(vapply(constraints, function(x) any(c("c+", "c-") %in% x), logical(1))) > 1L) {
    cli::cli_abort("Shape intervals with curvature in several coordinates are unsupported; use shape_confidence_set().")
  }
  constraints
}

check_shape_effect_transform <- function(info, constant = NULL, fun = NULL) {
  if (identical(info$method, "shape") &&
      (!is.null(constant) || !is.null(fun))) {
    cli::cli_abort("Shape intervals describe the unshifted smooth on the model scale; effect shifts and transformations are unsupported.")
  }
}

scasm_enforce_smooth_band <- function(d, smooth, constraints, interval, level) {
  margins <- if (length(smooth$margin)) smooth$margin else list(smooth)
  for (margin in margins) {
    x <- d[[margin$term]]
    knots <- margin$knots
    domain <- knots[c(margin$m[1L] + 1L, length(knots) - margin$m[1L])]
    # Linear extrapolation can lose positivity outside the boundary knots,
    # even when an evaluation coordinate is only slightly outside the domain.
    if (any(!is.finite(x)) || any(x < domain[1L] | x > domain[2L])) {
      cli::cli_abort("Shape intervals require finite coordinates inside the fitted B-spline domain.")
    }
  }
  if (any(!is.finite(d$.estimate) | !is.finite(d$.lower_ci) | !is.finite(d$.upper_ci))) {
    cli::cli_abort("Shape intervals require a complete finite evaluation grid; use dist = 0 and omit missing coordinates.")
  }
  band <- shape_intervals(d$.estimate, d$.lower_ci, d$.upper_ci,
    data = d[smooth$term], constraints = constraints,
    interval = if (interval == "confidence") "pointwise" else "simultaneous", level = level)
  d$.estimate <- band$.estimate
  d$.lower_ci <- band$.lower_ci
  d$.upper_ci <- band$.upper_ci
  d$.crit <- rep(NA_real_, nrow(d))
  d
}

scasm_shape_interval_info <- function(info, specs, model, frequentist, unconditional) {
  info$source <- info$method
  info$method <- "shape"
  info$shape <- specs
  info$operator <- "range, rearrangement, directional convex minorant"
  info$domain <- "evaluation grid"
  info$coordinate_scale <- "model"
  info$coverage <- "whole-grid containment preserved"
  info$pointwise_calibration <- if (info$interval == "confidence") "not guaranteed" else "not requested"
  info$interpretation <- if (info$source == "bootstrap") {
    "native bootstrap limits; no nominal coverage guarantee"
  } else if (frequentist) {
    "Gaussian approximation using frequentist covariance"
  } else "Gaussian approximation using Bayesian covariance; no constrained posterior constructed"
  info$se_covariance <- if (frequentist) "Ve" else if (unconditional && !is.null(model$Vc)) "Vc" else "Vp"
  info$unconditional_requested <- unconditional
  info$unconditional <- isTRUE(unconditional) && !isTRUE(frequentist) && !is.null(model$Vc)
  info
}
