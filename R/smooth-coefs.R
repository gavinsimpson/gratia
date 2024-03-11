#' Coefficients for a particular smooth
#'
#' Returns a vector of model coefficients of the parametric terms that represent
#' the supplied smooth.
#'
#' @param object a fitted GAM(M) object, or, for the `"mgcv.smooth"` method,
#'   an object that inherits from class `mgcv.smooth`.
#' @param select character; the label of the smooth whose coefficients will be
#'   returned.
#' @param term `r lifecycle::badge("deprecated")` Use `select` instead.
#' @param model a fitted GAM(M) object.
#' @param ... arguments passed to other methods.
#'
#' @return A numeric vector of model coefficients.
#'
#' @seealso [smooth_coef_indices()] for extracting the indices of the
#'   coefficients for a particular smooth.
#' 
#' @importFrom lifecycle deprecated is_present
#'
#' @author Gavin L. Simpson
#' @export
#'
#' @examples
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' load_mgcv()
#' df <- data_sim("eg1", seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' ## IGNORE_RDIFF_BEGIN
#' smooth_coefs(m, select = "s(x2)")
#' ## IGNORE_RDIFF_END
#' \dontshow{
#' options(op)
#' }
`smooth_coefs` <- function(object, ...) {
  UseMethod("smooth_coefs")
}

#' @export
#' @rdname smooth_coefs
#' @importFrom stats coef
`smooth_coefs.gam` <- function(object,
    select,
    term = deprecated(),
    ...) {
  if (lifecycle::is_present(term)) {
    lifecycle::deprecate_warn("0.8.9.9", "smooth_coefs(term)",
      "smooth_coefs(select)")
    select <- term
  }
  if (length(select) > 1L) {
    warning(
      "More than one smooth specified by `term`.\n",
      "Using only the first."
    )
    select <- select[1L]
  }
  sm <- get_smooth(object, term = select)
  if (length(sm) == 0) {
    stop("You didn't specify a smooth via 'select'")
  }
  i <- smooth_coef_indices(sm)
  coef(object)[i]
}

#' @export
#' @rdname smooth_coefs
`smooth_coefs.bam` <- function(object,
    select,
    term = deprecated(),
    ...) {
  if (lifecycle::is_present(term)) {
    lifecycle::deprecate_warn("0.8.9.9", "smooth_coefs(term)",
      "smooth_coefs(select)")
    select <- term
  }
  NextMethod()
}

#' @export
#' @rdname smooth_coefs
`smooth_coefs.gamm` <- function(object,
    select,
    term = deprecated(),
    ...) {
  if (lifecycle::is_present(term)) {
    lifecycle::deprecate_warn("0.8.9.9", "smooth_coefs(term)",
      "smooth_coefs(select)")
    select <- term
  }
  smooth_coefs(object$gam, select = select, ...)
}

#' @export
#' @rdname smooth_coefs
#' @importFrom stats coef
`smooth_coefs.gamm4` <- function(object,
    select,
    term = deprecated(),
    ...) {
  if (lifecycle::is_present(term)) {
    lifecycle::deprecate_warn("0.8.9.9", "smooth_coefs(term)",
      "smooth_coefs(select)")
    select <- term
  }
  sm <- get_smooth(object[["gam"]], term = select, ...)
  if (length(sm) == 0) {
    stop("You didn't specify a smooth via 'select'")
  }
  i <- smooth_coef_indices(sm)
  coef(object[["gam"]])[i]
}

#' @export
#' @rdname smooth_coefs
#' @importFrom stats coef
`smooth_coefs.list` <- function(object,
    select,
    term = deprecated(),
    ...) {
  if (lifecycle::is_present(term)) {
    lifecycle::deprecate_warn("0.8.9.9", "smooth_coefs(term)",
      "smooth_coefs(select)")
    select <- term
  }
  if (!is_gamm4(object)) {
    stop("'object' is a list but doesn't appear to be a 'gamm4()' model.")
  }
  sm <- get_smooth(object[["gam"]], term = select, ...)
  if (length(sm) == 0) {
    stop("You didn't specify a smooth via 'select'")
  }
  i <- smooth_coef_indices(sm)
  coef(object[["gam"]])[i]
}

#' @export
#' @rdname smooth_coefs
#' @importFrom stats coef
`smooth_coefs.mgcv.smooth` <- function(object, model, ...) {
  i <- smooth_coef_indices(object)
  coef(model)[i]
}

#' @export
#' @rdname smooth_coefs
#' @importFrom stats coef
`smooth_coefs.scam` <- function(object,
    select,
    term = deprecated(),
    ...) {
  if (lifecycle::is_present(term)) {
    lifecycle::deprecate_warn("0.8.9.9", "smooth_coefs(term)",
      "smooth_coefs(select)")
    select <- term
  }
  sm <- get_smooth(object, term = select, ...)
  if (length(sm) == 0) {
    stop("You didn't specify a smooth via 'select'")
  }
  i <- smooth_coef_indices(sm)
  coef(object)[i]
}

#' Indices of the parametric terms for a particular smooth
#'
#' Returns a vector of indices of the parametric terms that represent the
#' supplied smooth. Useful for extracting model coefficients and columns
#' of their covariance matrix.
#'
#' @param smooth an object that inherits from class `mgcv.smooth`
#'
#' @seealso [smooth_coefs()] for extracting the coefficients for a particular
#'   smooth.
#'
#' @return A numeric vector of indices.
#'
#' @author Gavin L. Simpson
#' @export
`smooth_coef_indices` <- function(smooth) {
  if (!is_mgcv_smooth(smooth)) {
    stop("Not an mgcv smooth object")
  }
  start <- smooth[["first.para"]]
  end <- smooth[["last.para"]]
  seq(from = start, to = end, by = 1L)
}
