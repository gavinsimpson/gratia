# Methods and code needed to make glmmTMB model fits work

# smooths()
#' @export
#' @rdname smooths
`smooths.glmmTMB` <- function(object) {
    no_sms <- is.null(object$modelInfo$reTrms$cond$smooth_info)
    out <- character(0)
    # some smooths
    if (isFALSE(no_sms)) {
        out <- vapply(object$modelInfo$reTrms$cond$smooth_info,
            FUN  = \(x) x[["sm"]][["label"]], FUN.VALUE = character(1))
    }

    out
}

# n_smooths()
#' @export
#' @rdname n_smooths
`n_smooths.glmmTMB` <- function(object) {
    length(object$modelInfo$reTrms$cond$smooth_info)
}

# get_smooths_by_id()


# smooth_estimates()
#' @export
#' @rdname smooth_estimates
`smooth_estimates.glmmTMB` <- function(object, ...) {
    smooth_estimates.gam(object, ...)
}

# draw()