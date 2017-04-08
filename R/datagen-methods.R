##' Generate data over the range of variables used in smooths
##'
##' For each smooth in a GAM, generate new data over the range of the variables in volved in a smooth.
##'
##' @param x an object for which new data is required. Currently objects of classes `"gam"`, and `"gamm"` are supported, as are smooths from **mgcv** inheriting from class `"mgcv.smooth"`.
##' @param n numeric; the number of data values to generate per term in each smooth.
##' @param data data frame; for `"mgcv.smooth"` objects, the data used to fit the GAM need to be supplied.
##' @param ... arguments passed to methods
##' @return A data frame of new values spread over the range of the observed values.
##'
##' @author Gavin L. Simpson
##'
##' @export
##' @rdname datagen
##'
##' @examples
##' library("mgcv")
##' set.seed(2)
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##' df <- datagen(m1)
##' head(df)
##'
`datagen` <- function(x, ...) {
    UseMethod("datagen")
}

##' @export
##' @rdname datagen
`datagen.mgcv.smooth` <- function(x, n = 200, data, ...) {
    d <- x$dim          # how many dimensions in smooth
    term <- smooth_terms(x)      # what term are we dealing with

    ## some smooths can't be plotted, esp n-d ones where n > 2
    if (!x$plot.me || d > 2) {
        out <- data.frame()             # or should we throw error
    }

    if (d == 1L) {                      # 1-d smooths
        xvals <- data[[term]]
        newvals <- seq(min(xvals), max(xvals), length.out = n)
        out <- data.frame(term = rep(term, n), x = newvals)
    } else {                            # 2-d smooths
        stop("Currently, 2-d smooths are not supported")
    }

    ## return
    out
}

##' @export
##' @rdname datagen
`datagen.gam` <- function(x, n = 200, ...) {
    out <- lapply(x$smooth, datagen, n = n, data = x$model)
    do.call("rbind", out)
}

##' @export
##' @rdname datagen
`datagen.gamm` <- function(x, ...) {
    datagen(x$gam)
}
