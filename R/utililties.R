`smooth_terms` <- function(obj, ...) {
    UseMethod("smooth_terms")
}

`smooth_terms.gam` <- function(obj, ...) {
    lapply(obj$smooth, `[[`, "term")
}

`smooth_terms.gamm` <- function(obj, ...) {
    smooth_terms(obj$gam, ...)
}

`smooth_terms.mgcv.smooth` <- function(obj, ...) {
    obj[["term"]]
}
