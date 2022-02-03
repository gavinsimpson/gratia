#' Generate data over the range of variables used in smooths
#'
#' For each smooth in a GAM, generate new data over the range of the variables
#' involved in a smooth.
#'
#' @param x an object for which new data is required. Currently objects of
#'   classes `"gam"`, and `"gamm"` are supported, as are smooths from **mgcv**
#'   inheriting from class `"mgcv.smooth"`.
#' @param n numeric; the number of data values to generate per term in each
#'   smooth.
#' @param data data frame; for `"mgcv.smooth"` objects, the data used to fit
#'   the GAM need to be supplied.
#' @param ... arguments passed to methods
#' @return A data frame of new values spread over the range of the
#'   observed values.
#'
#' @author Gavin L. Simpson
#'
#' @rdname datagen
#'
#' @keywords internal
`datagen` <- function(x, ...) {
    UseMethod("datagen")
}

#' @export
#' @rdname datagen
`datagen.mgcv.smooth` <- function(x, n = 100, data, ...) {
    d <- smooth_dim(x)                 # how many dimensions in smooth
    term <- smooth_terms(x)            # what term are we dealing with

    ## some smooths can't be plotted, esp n-d ones where n > 2
    if (!x$plot.me || d > 2L) {
        out <- data.frame()  # FIXME: or should we throw error/message
    }

    if (d == 1L) {                      # 1-d smooths
        xvals <- data[[term]]
        newvals <- seq_min_max(xvals, n = n)
        out <- data.frame(smooth = rep(smooth_label(x), n), x = newvals)
    } else if (d == 2L) {                            # 2-d smooths
        xvals <- data[[term[1]]]
        zvals <- data[[term[2]]]
        newx <- seq_min_max(xvals, n = n)
        newz <- seq_min_max(zvals, n = n)
        out <- expand.grid(x1 = newx, x2 = newz)
        out <- cbind(smooth = rep(smooth_label(x), n^2), out)
    } else {
        stop("Cannot handle smooths of three (3) or more terms.")
    }

    ## return
    out
}

#' @export
#' @rdname datagen
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

#' @export
#' @rdname datagen
`datagen.gam` <- function(x, smooth = NULL, n = 200, ...) {
    if (is.null(smooth)) {
        stop("Argument 'smooth' must be specified and not 'NULL'.")
    }
    if (length(smooth) > 1L) {
        stop("More than one smooth requested in argument 'smooth'.")
    }
    sm <- smooths(x)
    select <- check_user_select_smooths(sm, select = smooth)
    datagen(get_smooths_by_id(x, which(select))[[1L]],
            n = n, data = x[["model"]])
}

#' @export
#' @rdname datagen
`datagen.gamm` <- function(x, ...) {
    if (!is.gamm(x)) {
        stop("Model doesn't appear to be a 'gamm()' model object.")
    }
    datagen(x[["gam"]], ...)
}

#' @export
#' @rdname datagen
`datagen.list` <- function(x, ...) {
    if (!is_gamm4(x)) {
        stop("Model doesn't appear to be a 'gamm4()' model object.")
    }
    datagen(x[["gam"]], ...)
}

