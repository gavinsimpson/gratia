#' Extract fixed effects estimates
#'
#' @param object a fitted GAM
#' @param ... arguments passed to other methods
#'
#' @importFrom nlme fixef
#' @name fixef
#' @export
NULL

#' Extract fixed effects estimates from a fitted GAM
#'
#' @param object a fitted GAM
#' @param ... arguments passed to other methods
#'
#' @export
#'
#' @examples
#' load_mgcv()
#'
#' # run example if lme4 is available
#' if (require("lme4")) {
#'
#' data(sleepstudy, package = "lme4")
#' m <- gam(Reaction ~ Days + s(Subject, bs = "re") +
#'            s(Days, Subject, bs = "re"),
#'          data = sleepstudy, method = "REML")
#' fixef(m)
#'
#' }
`fixef.gam` <- function(object, ...) {
    coefs <- coef(object)
    nms <- names(coefs)
    # drop everything that starts with s, te, ti, or t2 and is followed by a (
    sm_terms <- grepl('^[s te ti t2](?=\\()', names(coef(object)), perl = TRUE)
    nms <- nms[!sm_terms]
    # return
    coefs[nms]
}

#' @rdname fixef.gam
#' @export
`fixef.gamm` <- function(object, ...) {
    object <- object$gam
    fixef(object)
}

#' @rdname fixef.gam
#' @export
`fixef.lm` <- function(object, ...) {
    coef(object)
}

#' @rdname fixef.gam
#' @export
`fixef.glm` <- function(object, ...) {
    coef(object)
}

#' @rdname fixef.gam
#' @export
`fixed_effects` <- function(object, ...) {
    UseMethod("fixed_effects")
}

#' @rdname fixef.gam
#' @export
`fixed_effects.default` <- function(object, ...) {
    fixef(object, ...)
}
