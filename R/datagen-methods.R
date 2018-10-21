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
##'
##' ## 1d example
##' set.seed(2)
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##' df <- datagen(m1)
##' head(df)
##'
##' ## 2d example
##' dat <- gamSim(2, n = 400, dist = "normal", scale = 2)
##' m2 <- gam(y ~ s(x, z), data = dat$data, method = "REML")
##' df <- datagen(m2)
##' head(df)
##' ## alternative showing using the mgcv.smooth method for a single smooth
##' df2 <- datagen(m2[["smooth"]][[1L]], data = dat$data)
##' head(df2)
`datagen` <- function(x, ...) {
    UseMethod("datagen")
}

##' @export
##' @rdname datagen
`datagen.mgcv.smooth` <- function(x, n = 100, data, ...) {
    d <- smooth_dim(x)                 # how many dimensions in smooth
    term <- smooth_terms(x)            # what term are we dealing with

    ## some smooths can't be plotted, esp n-d ones where n > 2
    if (!x$plot.me || d > 2L) {
        out <- data.frame()  # FIXME: or should we throw error/message
    }

    if (d == 1L) {                      # 1-d smooths
        xvals <- data[[term]]
        newvals <- seq(min(xvals), max(xvals), length.out = n)
        out <- data.frame(term = rep(smooth_label(x), n), x = newvals)
    } else {                            # 2-d smooths
        xvals <- data[[term[1]]]
        zvals <- data[[term[2]]]
        newx <- seq(min(xvals), max(xvals), length.out = n)
        newz <- seq(min(zvals), max(zvals), length.out = n)
        out <- expand.grid(x1 = newx, x2 = newz)
        out <- cbind(smooth = rep(smooth_label(x), n^2), out)
    }

    ## return
    out
}

##' @export
##' @rdname datagen
`datagen.fs.interaction` <- function(x, n = 100, data, ...) {
    d <- smooth_dim(x)                 # how many dimensions in smooth
    term <- smooth_variable(x)         # what term are we dealing with
    fterm <- smooth_factor_variable(x) # get factor associated with smooth

    ## term should be length 2, which is the smooth variable
    term <- term[term != fterm]

    ## some smooths can't be plotted, esp n-d ones where n > 2
    if (!x$plot.me || d > 2L) {
        out <- data.frame()  # FIXME: or should we throw error/message
    }

    ## get new values of continuous var
    xvals <- data[[term]]
    newx <- seq(min(xvals), max(xvals), length.out = n)

    ## get the factor var and its levels
    f <- data[[fterm]]
    fvals <- levels(f)
    nlevs <- nlevels(f)

    out <- setNames(expand.grid(x = newx, f = fvals),
                    c(term, fterm))
    out <- cbind(smooth = rep(smooth_label(x), n * nlevs), out)

    out                                 # return
}

##' @export
##' @rdname datagen
`datagen.gam` <- function(x, n = 200, ...) {
    out <- lapply(x[["smooth"]], datagen, n = n, data = x[["model"]])
    do.call("rbind", out)               # FIXME: this can't possibly be right for multiple smooths
}

##' @export
##' @rdname datagen
`datagen.gamm` <- function(x, ...) {
    datagen(x[["gam"]])
}

##' Prepare a data slice through covariates
##'
##' @export
`data_slice` <- function(object, ...) {
    UseMethod("data_slice")
}

##' @export
`data_slice.default` <- function(object, ...) {
    stop("Don't know how to create a data slice from <", class(object)[[1L]],
         ">", call. = FALSE)
}

##' @export
`data_slice.gam` <- function(object, var_1, var_2, ...) {

}
