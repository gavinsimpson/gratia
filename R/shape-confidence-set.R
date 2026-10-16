#' Intersect a band with linear restrictions on coefficients
#'
#' Represent the set of coefficient vectors whose evaluated functions are inside
#' an initial band and satisfy supplied linear inequalities and equalities.
#'
#' @param X A finite numeric matrix that maps coefficients to function values at
#'   the evaluation points. Each column corresponds to a coefficient.
#' @param lower,upper Numeric vectors of lower and upper limits, one per row of
#'   `X`. Infinite limits are allowed. Missing values are not allowed.
#' @param Ain A finite numeric matrix defining inequalities `Ain %*% b >= bin`.
#'   It must have the same number of columns as `X`. `NULL` means no inequalities.
#' @param bin A finite numeric vector, one entry per row of `Ain`. If `NULL`,
#'   zero is used for every inequality.
#' @param C A finite numeric matrix defining equalities `C %*% b = d`. It must
#'   have the same number of columns as `X`. `NULL` means no equalities.
#' @param d A finite numeric vector, one entry per row of `C`. If `NULL`, zero
#'   is used for every equality.
#' @param level A number strictly between zero and one recording the nominal
#'   level of the initial band. This does not calibrate the band.
#' @param interpretation Is the initial band a confidence or credible band?
#' @param band_type Record whether the initial limits were calibrated jointly
#'   (`"simultaneous"`), separately at each evaluation point (`"pointwise"`), or
#'   whether their calibration is unspecified (`"unspecified"`, the default).
#'
#' @details
#' The returned object represents the set
#' \deqn{\{b: \mathrm{lower} \le Xb \le \mathrm{upper},\quad
#' A_{in}b \ge b_{in},\quad Cb=d\}.}
#' The matrices are retained explicitly; the function does not optimise over the
#' set, test whether it is empty, or draw samples from it. Redundant and
#' inconsistent restrictions are allowed. Inconsistent restrictions represent
#' an empty set.
#'
#' The initial band restricts function values only at the supplied evaluation
#' points. The extent of the shape restriction depends on the supplied
#' coefficient inequalities. For example, appropriate spline coefficient
#' inequalities can guarantee monotonicity throughout a spline's domain, whereas
#' differences between evaluated function values guarantee it only on that grid.
#' This function does not infer restrictions from a fitted model or extend
#' restrictions between evaluation points.
#'
#' If the true coefficient vector satisfies the supplied restrictions, it is in
#' the intersection exactly when its evaluated function is in the initial band.
#' Thus intersection preserves the coverage of an initially valid simultaneous
#' confidence band, for truths satisfying those restrictions. Pointwise limits
#' alone do not supply a simultaneous confidence set. For credible bands, the
#' same posterior probability statement requires the posterior to be supported
#' entirely on the restricted coefficient space. Intersection does not fix
#' smoothing bias, invalid initial calibration, or an incorrect restriction.
#'
#' Coordinatewise minima and maxima of functions in this set need not themselves
#' belong to the set. This matters for coupled restrictions such as an integral
#' equal to one: two ordered functions with that integral must coincide almost
#' everywhere. Retaining the whole set avoids assigning such equalities to
#' separate endpoint curves.
#'
#' @return `shape_confidence_set()` returns a list of class
#'   `"shape_confidence_set"` containing `X`, `lower`, `upper`, `Ain`, `bin`, `C`,
#'   `d`, `level`, `interpretation`, and `band_type`. Missing constraint matrices
#'   are stored as matrices with zero rows. `in_shape_set()` returns a logical
#'   vector, one value per candidate coefficient vector.
#'
#' @examples
#' # Two non-negative masses whose sum is one.
#' region <- shape_confidence_set(
#'   X = diag(2), lower = c(0.1, 0.2), upper = c(0.8, 0.9),
#'   Ain = diag(2), C = matrix(c(1, 1), nrow = 1), d = 1,
#'   band_type = "simultaneous"
#' )
#' in_shape_set(region, c(0.4, 0.6))
#' in_shape_set(region, c(0.4, 0.7))
#' @export
shape_confidence_set <- function(X, lower, upper, Ain = NULL, bin = NULL,
                                 C = NULL, d = NULL, level = 0.95,
                                 interpretation = c("confidence", "credible"),
                                 band_type = c("unspecified", "simultaneous",
                                               "pointwise")) {
  shape_set_matrix(X, "X")
  p <- ncol(X)
  if (p < 1L) {
    stop("`X` must have at least one column.", call. = FALSE)
  }
  shape_set_vector(lower, "lower", nrow(X), finite = FALSE)
  shape_set_vector(upper, "upper", nrow(X), finite = FALSE)
  if (any(lower > upper)) {
    stop("`lower` must not exceed `upper`.", call. = FALSE)
  }
  shape_set_vector(level, "level", 1L)
  if (level <= 0 || level >= 1) {
    stop("`level` must be strictly between zero and one.", call. = FALSE)
  }
  interpretation <- match.arg(interpretation)
  band_type <- match.arg(band_type)
  inequality <- shape_set_constraints(Ain, bin, "Ain", "bin", p)
  equality <- shape_set_constraints(C, d, "C", "d", p)
  structure(list(
    X = X, lower = lower, upper = upper,
    Ain = inequality$matrix, bin = inequality$rhs,
    C = equality$matrix, d = equality$rhs,
    level = level, interpretation = interpretation, band_type = band_type
  ), class = "shape_confidence_set")
}

#' @rdname shape_confidence_set
#' @param object A set created by [shape_confidence_set()].
#' @param coefficients A finite numeric vector, or a finite numeric matrix with
#'   one candidate coefficient vector in each column. Rows must correspond to
#'   columns of `object$X` in the same order; names are not used for matching.
#' @param tolerance A finite non-negative number used as an absolute tolerance
#'   in function values and constraint residuals. The default is `1e-8`.
#'   Setting it to zero uses exact floating-point comparisons. Multiplying a
#'   constraint row by a constant changes the effect of an absolute tolerance.
#' @export
in_shape_set <- function(object, coefficients, tolerance = 1e-8) {
  if (!inherits(object, "shape_confidence_set")) {
    stop("`object` must be a shape confidence set.", call. = FALSE)
  }
  shape_set_vector(tolerance, "tolerance", 1L)
  if (tolerance < 0) {
    stop("`tolerance` must be non-negative.", call. = FALSE)
  }
  p <- ncol(object$X)
  if (is.matrix(coefficients)) {
    shape_set_matrix(coefficients, "coefficients")
    if (nrow(coefficients) != p) {
      stop("`coefficients` must have one row per column of `object$X`.",
        call. = FALSE)
    }
  } else {
    shape_set_vector(coefficients, "coefficients", p)
    coefficients <- matrix(coefficients, ncol = 1L)
  }
  inside <- rep(TRUE, ncol(coefficients))
  names(inside) <- colnames(coefficients)
  evaluated <- object$X %*% coefficients
  inequalities <- object$Ain %*% coefficients
  equalities <- object$C %*% coefficients
  if (any(!is.finite(c(evaluated, inequalities, equalities)))) {
    stop("Evaluating `coefficients` produced non-finite values.", call. = FALSE)
  }
  for (j in seq_len(ncol(coefficients))) {
    inside[j] <- all(evaluated[, j] >= object$lower - tolerance) &&
      all(evaluated[, j] <= object$upper + tolerance) &&
      all(inequalities[, j] >= object$bin - tolerance) &&
      all(abs(equalities[, j] - object$d) <= tolerance)
  }
  inside
}

shape_set_matrix <- function(x, name) {
  if (!is.matrix(x) || !is.numeric(x) || any(!is.finite(x))) {
    stop("`", name, "` must be a finite numeric matrix.", call. = FALSE)
  }
  invisible(x)
}

shape_set_vector <- function(x, name, size, finite = TRUE) {
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) != size ||
      anyNA(x) || (finite && any(!is.finite(x)))) {
    stop("`", name, "` must be a ", if (finite) "finite " else "",
      "numeric vector of length ", size, ".", call. = FALSE)
  }
  invisible(x)
}

shape_set_constraints <- function(x, rhs, name, rhs_name, p) {
  if (is.null(x)) {
    x <- matrix(numeric(), nrow = 0L, ncol = p)
  } else {
    shape_set_matrix(x, name)
    if (ncol(x) != p) {
      stop("`", name, "` must have the same number of columns as `X`.",
        call. = FALSE)
    }
  }
  if (is.null(rhs)) {
    rhs <- numeric(nrow(x))
  }
  shape_set_vector(rhs, rhs_name, nrow(x))
  list(matrix = x, rhs = rhs)
}
