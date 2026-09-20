#' Variance components of smooths from smoothness estimates
#'
#' A wrapper to [mgcv::gam.vcomp()] which returns the smoothing parameters
#'   expressed as variance components.
#'
#' @details This function is a wrapper to [mgcv::gam.vcomp()] which performs
#'   three additional services
#'
#' * it suppresses the annoying text output that [mgcv::gam.vcomp()] prints to
#'   the terminal,
#' * returns the variance of each smooth as well as the standard deviation, and
#' * returns the variance components as a tibble.
#'
#' @param object an R object. Currently only models fitted by [mgcv::gam()] or
#'   [mgcv::bam()] are supported.
#' @param rescale logical; for numerical stability reasons the penalty matrices
#'   of smooths are rescaled before fitting. If `rescale = TRUE`, this rescaling
#'   is undone, resulting in variance components that are on their original
#'   scale. This is needed if comparing with other mixed model software, such as
#'   `lmer()`.
#' @param coverage numeric; a value between 0 and 1 indicating the (approximate)
#'   coverage of the confidence interval that is returned.
#' @param ... arguments passed to other methods
#'
#' @export
`variance_comp` <- function(object, ...) {
  UseMethod("variance_comp")
}

#' @export
#' @rdname variance_comp
#'
#' @importFrom tibble rownames_to_column add_column
#' @importFrom rlang set_names
#' @importFrom utils capture.output
#' @importFrom mgcv gam.vcomp
`variance_comp.gam` <- function(object, rescale = TRUE, coverage = 0.95, ...) {
  capture.output(vcomps <- gam.vcomp(object,
    rescale = rescale, conf.lev = coverage
  ))
  if (is.list(vcomps) && !is.null(vcomps[["vc"]])) {
    vcomps <- vcomps[["vc"]]
  }
  if (is.null(vcomps)) {
    tbl <- tibble::tibble(
      .component = character(), .variance = double(),
      .std_dev = double(), .lower_ci = double(), .upper_ci = double()
    )
  } else if (is.matrix(vcomps)) {
    tbl <- tibble::tibble(
      .component = rownames(vcomps),
      .variance = unname(vcomps[, "std.dev"])^2,
      .std_dev = unname(vcomps[, "std.dev"]),
      .lower_ci = unname(vcomps[, "lower"]),
      .upper_ci = unname(vcomps[, "upper"])
    )
  } else if (is.numeric(vcomps) && is.null(dim(vcomps))) {
    tbl <- tibble::tibble(
      .component = names(vcomps),
      .variance = as.numeric(vcomps)^2,
      .std_dev = as.numeric(vcomps),
      .lower_ci = rep(NA_real_, length(vcomps)),
      .upper_ci = rep(NA_real_, length(vcomps))
    )
  } else {
    stop("Unsupported return format from mgcv::gam.vcomp().", call. = FALSE)
  }
  class(tbl) <- c("variance_comp", class(tbl))
  tbl
}
