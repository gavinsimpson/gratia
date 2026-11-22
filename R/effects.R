#' @rdname fixed_effects.gam
#' @export
`fixed_effects` <- function(object, ...) {
  UseMethod("fixed_effects")
}

#' Extract fixed effects estimates from a fitted model
#'
#' @param object a fitted model. Supported classes are models fitted by
#'   [mgcv::gam()], [mgcv::bam()], [mgcv::gamm()], [gamm4::gamm4()],
#'   [scam::scam()], [stats::glm()], and [stats::lm()].
#' @param ... arguments passed to other methods
#'
#' @details
#' Starting with gratia 0.12.1, use `fixed_effects()` for the models listed
#' above if you previously called `gratia::fixef()` or a `fixef()` method
#' supplied by gratia. For models fitted directly with nlme or lme4, use
#' [nlme::fixef()] or [lme4::fixef()], respectively.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#'
#' # run example if lme4 is available
#' if (require("lme4")) {
#'   data(sleepstudy, package = "lme4")
#'   m <- gam(
#'     Reaction ~ Days + s(Subject, bs = "re") +
#'       s(Days, Subject, bs = "re"),
#'     data = sleepstudy, method = "REML"
#'   )
#'   fixed_effects(m)
#' }
`fixed_effects.gam` <- function(object, ...) {
  coefs <- coef(object)
  smooth_idx <- unlist(lapply(object$smooth, function(sm) {
    seq.int(sm$first.para, sm$last.para)
  }), use.names = FALSE)
  coefs[setdiff(seq_along(coefs), smooth_idx)]
}

#' @rdname fixed_effects.gam
#' @export
`fixed_effects.gamm` <- function(object, ...) {
  object <- object$gam
  fixed_effects(object)
}

#' @rdname fixed_effects.gam
#' @export
`fixed_effects.gamm4` <- function(object, ...) {
  object <- gamm4_gam(object)
  fixed_effects(object)
}

#' @rdname fixed_effects.gam
#' @export
`fixed_effects.lm` <- function(object, ...) {
  coef(object)
}

#' @rdname fixed_effects.gam
#' @export
`fixed_effects.glm` <- function(object, ...) {
  coef(object)
}

#' @rdname fixed_effects.gam
#' @export
`fixed_effects.default` <- function(object, ...) {
  stop("No fixed-effects method for this object.", call. = FALSE)
}
