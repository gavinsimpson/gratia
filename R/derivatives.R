##' @title Derivatives of estimated smooths via finite differences
##'
##' @param object an R object to compute derivatives for.
##' @param ... arguments passed to other methods.
##'
##' @author Gavin L. Simpson
##'
##' @export
`derivatives` <- function(object, ...) {
    UseMethod("derivatives")
}

##' @rdname derivatives
##' @export
`derivatives.default` <- function(object, ...) {
    ## want to bail with a useful error;
    ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
    stop("Don't know how to calculate derivatives for <",
         class(object)[[1L]], ">",
         call. = FALSE)           # don't show the call, simpler error
}

##' @rdname derivatives
##'
##' @param newdata a data frame containing the values of the model covariates
##'   at which to evaluate the first derivatives of the smooths.
##' @param order numeric; the order of derivative.
##' @param type character; the type of finite difference used. One of
##'   `"forward"`, `"backward"`, or `"central"`.
##'
##' @export
`derivatives.gam` <- function(object, newdata, order = c(1, 2),
                              type = c("forward", "backward", "central"),
                              n = 200, eps = 1e-7, ...) {
    type <- match.arg(type)
}

##' @rdname derivatives
##'
##' @export
`derivatives.gamm` <- function(object, ...) {
    derivatives(object[["gam"]], ...)
}
