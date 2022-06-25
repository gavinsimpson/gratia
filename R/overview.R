#' Provides an overview of a model and the terms in that model
#'
#' @param model a fitted model object to overview.
#' @param ... arguments passed to other methods.
#' 
#' @export
`overview` <- function(model, ...) {
    UseMethod("overview")
}

#' @export
#' @rdname overview
`overview.gam` <- function(model, parametric = TRUE, ...) {
    sms <- smooths(model)
    types <- vapply(m1$smooth, smooth_type, character(1))
    edfs <- edf(model)[["edf"]]
    out <- tibble(term = sms, type = types, edf = edfs)
    out
}
