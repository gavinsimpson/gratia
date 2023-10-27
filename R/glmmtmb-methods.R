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
#' @export
#' @rdname get_smooths_by_id
`get_smooths_by_id.glmmTMB` <- function(object, id) {
    if (missing(id)) {
        stop("`id` of smooth to extract must be provided.", call. = FALSE)
    }
    ns <- n_smooths(object)
    if (ns < 1L) {
        stop("Model ", deparse(substitute(object)), " contains no smooths.")
    }
    if (id > ns) {
        stop("Attempting to extract a smooth that doesn't exist.", call. = FALSE)
    }
    sm <- object$modelInfo$reTrms$cond$smooth_info[[id]][[1L]]
    stop_if_not_mgcv_smooth(sm)

    sm
}

# smooth_estimates()
#' @export
#' @rdname smooth_estimates
`smooth_estimates.glmmTMB` <- function(object, ...) {
    # wrong, but using this to see where things fail
    smooth_estimates.gam(object, ...)
}

# draw()