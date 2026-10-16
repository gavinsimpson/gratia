#' Enforce named shapes on estimates and interval endpoints
#'
#' Apply shape-enforcing operators to an existing estimate and its lower and
#' upper endpoints. Construction and calibration of the initial uncertainty
#' remain the caller's responsibility.
#'
#' @param estimate,lower,upper finite numeric vectors, one value per row of
#'   `data`. The lower endpoints must not exceed the upper endpoints.
#' @param data a data frame of finite numeric coordinates. Rows may be in any
#'   order but must form a complete Cartesian product without duplicates.
#'   Coordinates may be irregularly spaced. A single coordinate gives a curve.
#' @param constraints a named list, with coordinate names as names and character
#'   vectors of restrictions as elements. Codes are `"m+"` (nondecreasing),
#'   `"m-"` (nonincreasing), `"c+"` (convex), `"c-"` (concave), and `"+"`
#'   (nonnegative). Curvature restrictions may occur in at most one coordinate.
#'   Monotonicity may be imposed in several coordinates. Positivity applies to
#'   the function, regardless of which coordinate carries its code.
#' @param interval character; whether the input is a `"simultaneous"` band or
#'   `"pointwise"` limits. This describes the input, and does not calibrate it.
#' @param level numeric; the nominal level of the input, strictly between zero
#'   and one.
#'
#' @details
#' The operator first thresholds at zero if requested, then sorts each
#' monotone coordinate in the order given by `constraints`, then takes the
#' greatest convex minorant in the curvature coordinate. Concavity uses the
#' negative of the greatest convex minorant of the negative values. The
#' minorant uses actual coordinate distances. Sorting gives equal mass to each
#' grid coordinate, including on irregular grids; it does not approximate
#' Lebesgue-weighted rearrangement on such grids.
#'
#' These finite-grid operators preserve order, leave a function with the
#' requested shapes unchanged, and do not increase maximum absolute distance.
#' Consequently, whenever the entire initial band contains a target with the
#' requested shapes on the grid, the transformed band also contains it. Its
#' maximum width cannot increase. This is a deterministic statement about a
#' whole-grid event; it does not guarantee nominal pointwise coverage after
#' transforming pointwise limits. Nor does it correct smoothing bias or an
#' invalid initial uncertainty calculation. For a posterior supported on the
#' restricted functions the analogous event inclusion applies to posterior
#' probability; this operation does not construct that posterior.
#'
#' The guarantees concern values on the supplied grid. Linear interpolation
#' of curves, or multilinear interpolation on rectangles, also has the named
#' directional shapes between grid points. The output is not a fitted spline
#' and need not obey coefficient inequalities, centring, integral constraints,
#' or other equalities. Use [shape_confidence_set()] to represent intersections
#' with such restrictions. Several curvature coordinates are unsupported:
#' sequential directional convexification need not preserve earlier convexity.
#' Extreme numeric ranges that prevent reliable slope comparisons or
#' interpolation in double precision produce an error.
#'
#' @return A tibble containing `data`, `.estimate`, `.lower_ci`, and
#'   `.upper_ci`, in the original row order. The `interval_info` attribute
#'   records the operator, restrictions, input interval type and coverage scope.
#'   Standard errors and Gaussian critical values are not recalculated.
#'
#' @references
#' Chernozhukov, V., Fernandez-Val, I. and Galichon, A. (2009).
#' Improving point and interval estimators of monotone functions by
#' rearrangement. *Biometrika*, **96**, 559--575.
#' \doi{10.1093/biomet/asp030}.
#'
#' Chen, X., Chernozhukov, V., Fernandez-Val, I., Kostyshak, S. and Luo, Y.
#' (2021). Shape-Enforcing Operators for Generic Point and Interval Estimators
#' of Functions. *Journal of Machine Learning Research*, **22**(220), 1--42.
#' <https://www.jmlr.org/papers/v22/20-513.html>.
#'
#' @export
#' @examples
#' x <- c(0, 0.1, 0.4, 0.8, 1)
#' f <- c(0.1, 0.4, 0.2, 0.9, 0.7)
#' shape_intervals(f, f - 0.3, f + 0.3, data.frame(x = x),
#'   constraints = list(x = c("+", "m+")))
shape_intervals <- function(estimate, lower, upper, data, constraints,
                            interval = c("simultaneous", "pointwise"),
                            level = 0.95) {
  interval <- match.arg(interval)
  validate_smooth_interval("confidence", level, 1, 1, NULL)
  axes <- validate_shape_grid(data, constraints)
  n <- nrow(data)
  for (value in list(estimate, lower, upper)) {
    if (!is.numeric(value) || !is.null(dim(value)) || length(value) != n ||
        any(!is.finite(value))) {
      cli::cli_abort("Estimates and endpoints must be finite numeric vectors with one value per grid row.")
    }
  }
  if (any(lower > upper)) cli::cli_abort("Lower endpoints must not exceed upper endpoints.")
  operator <- function(value) {
    if (any(vapply(constraints, function(x) "+" %in% x, logical(1)))) {
      value <- pmax(0, value)
    }
    for (nm in names(constraints)) {
      codes <- constraints[[nm]]
      if (any(c("m+", "m-") %in% codes)) {
        for (idx in axes[[nm]]) {
          value[idx] <- sort(value[idx], decreasing = "m-" %in% codes)
        }
      }
    }
    for (nm in names(constraints)) {
      codes <- constraints[[nm]]
      if (any(c("c+", "c-") %in% codes)) {
        sign <- if ("c-" %in% codes) -1 else 1
        for (idx in axes[[nm]]) {
          value[idx] <- sign * shape_convex_minorant(data[[nm]][idx], sign * value[idx])
        }
      }
    }
    value
  }
  out <- tibble::as_tibble(data)
  out$.estimate <- operator(estimate)
  out$.lower_ci <- operator(lower)
  out$.upper_ci <- operator(upper)
  attr(out, "interval_info") <- list(method = "shape", level = level,
    interval = if (interval == "pointwise") "confidence" else "simultaneous",
    source = "supplied", correction = "none", shape = constraints,
    operator = "range, rearrangement, directional convex minorant",
    domain = "evaluation grid", scope = "grid",
    coverage = "whole-grid containment preserved",
    pointwise_calibration = if (interval == "pointwise") "not guaranteed" else "not requested",
    interpretation = "inherited from input")
  out
}

# Return sorted lines parallel to each constrained coordinate. All rows must
# belong to the same rectangle so that sorting preserves earlier directions.
validate_shape_grid <- function(data, constraints) {
  if (!is.data.frame(data) || !ncol(data) || anyNA(names(data)) || anyDuplicated(names(data)) ||
      any(!nzchar(names(data))) ||
      !all(vapply(data, function(x) is.numeric(x) && is.null(dim(x)) &&
        all(is.finite(x)), logical(1)))) {
    cli::cli_abort("{.arg data} must contain distinct, named, finite numeric coordinate columns.")
  }
  if (any(c(".estimate", ".lower_ci", ".upper_ci") %in% names(data))) {
    cli::cli_abort("Coordinate names must not conflict with the returned estimate and endpoint columns.")
  }
  if (!is.list(constraints) || !length(constraints) ||
      is.null(names(constraints)) || anyNA(names(constraints)) || anyDuplicated(names(constraints)) ||
      any(!nzchar(names(constraints))) || !all(names(constraints) %in% names(data))) {
    cli::cli_abort("{.arg constraints} must be a nonempty list named by coordinate columns.")
  }
  for (codes in constraints) {
    if (!is.character(codes) || !length(codes) || anyNA(codes) ||
        !all(codes %in% c("+", "m+", "m-", "c+", "c-")) ||
        all(c("m+", "m-") %in% codes) || all(c("c+", "c-") %in% codes)) {
      cli::cli_abort("Each restriction must use compatible codes from '+', 'm+', 'm-', 'c+' and 'c-'.")
    }
  }
  curvature <- vapply(constraints, function(x) any(c("c+", "c-") %in% x), logical(1))
  if (sum(curvature) > 1L) {
    cli::cli_abort("Curvature in several coordinates is unsupported; use an explicit shape confidence set.")
  }
  values <- lapply(data, function(x) sort(unique(x)))
  if (anyDuplicated(data) || prod(lengths(values)) != nrow(data)) {
    cli::cli_abort("Shape operators require a complete Cartesian grid without duplicate rows; use dist = 0 for tensor evaluation.")
  }
  indices <- Map(match, data, values)
  lapply(stats::setNames(names(constraints), names(constraints)), function(nm) {
    other <- indices[setdiff(names(data), nm)]
    groups <- if (!length(other)) rep.int(1L, nrow(data)) else {
      do.call(interaction, c(other, list(drop = TRUE, lex.order = TRUE)))
    }
    lapply(split(seq_len(nrow(data)), groups), function(idx) idx[order(data[[nm]][idx])])
  })
}

# Lower hull with increasing slopes, followed by linear interpolation. Scale
# adjacent differences locally so large offsets and small values retain their
# geometry. Leave an already-convex vector exactly unchanged.
shape_convex_minorant <- function(x, y) {
  n <- length(x)
  if (n < 3L) return(y)
  if (all(y == y[1L])) return(y)
  x <- as.double(x)
  values <- as.double(y)
  slope_comparison <- function(a, b, i) {
    # Subtract before scaling: scaling a large common offset can change the
    # ratios of otherwise exactly representable coordinate differences.
    dy <- c(values[b] - values[a], values[i] - values[b])
    dx <- c(x[b] - x[a], x[i] - x[b])
    if (any(!is.finite(c(dx, dy)))) {
      cli::cli_abort("The coordinate or value range is too large for reliable convexification in double precision.")
    }
    nonzero <- dy != 0
    if (any(nonzero)) dy <- dy / max(abs(dy))
    dx <- dx / max(dx)
    if (any(dx <= 0) || any(dy[nonzero] == 0)) {
      cli::cli_abort("The coordinate or value range is too large for reliable convexification in double precision.")
    }
    c(dy[1L] * dx[2L], dy[2L] * dx[1L])
  }
  convex <- vapply(seq_len(n - 2L), function(a) {
    slopes <- slope_comparison(a, a + 1L, a + 2L)
    slopes[1L] <= slopes[2L]
  }, logical(1))
  if (all(convex)) return(y)
  hull <- integer(n)
  size <- 0L
  for (i in seq_len(n)) {
    while (size > 1L) {
      a <- hull[size - 1L]
      b <- hull[size]
      slopes <- slope_comparison(a, b, i)
      if (slopes[1L] < slopes[2L]) break
      size <- size - 1L
    }
    size <- size + 1L
    hull[size] <- i
  }
  hull <- hull[seq_len(size)]
  out <- y
  for (j in seq_len(length(hull) - 1L)) {
    a <- hull[j]
    b <- hull[j + 1L]
    idx <- seq.int(a, b)
    span <- x[b] - x[a]
    if (!is.finite(span)) {
      cli::cli_abort("The coordinate or value range is too large for reliable convexification in double precision.")
    }
    weight <- (x[idx] - x[a]) / span
    interpolated <- (1 - weight) * y[a] + weight * y[b]
    # Roundoff can take the weighted sum outside the endpoints' range, even
    # when both endpoints are equal. Clamping retains exact flat segments.
    # Each operation remains increasing in both endpoint values.
    out[idx] <- pmin(max(y[a], y[b]), pmax(min(y[a], y[b]), interpolated))
  }
  out[hull] <- y[hull]
  out
}
