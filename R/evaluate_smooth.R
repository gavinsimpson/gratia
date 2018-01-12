##' Evaluate a smooth
##'
##' Evaluate a smooth at a grid of evenly spaced value over the range of the covariate associated with the smooth. Alternatively, a set of points at which the smooth should be evaluated can be supplied.
##'
##' @param object an object of class `"gam"` or `"gamm"`.
##' @param smooth character; a single smooth to evaluate.
##' @param n numeric; the number of points over the range of the covariate at which to evaluate the smooth.
##' @param newdata a vector or data frame of points at which to evaluate the smooth.
##' @param unconditional logical; should confidence intervals include the uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian covariance matrix will be used.
##' @param inc.mean logical; should the uncertainty in the model constant term be
##'  included in the standard error of the evaluate values of the smooth?
##'  Currently not implemented.
##' @param dist numeric; if greater than 0, this is used to determine when
##'   a location is too far from data to be plotted when plotting 2-D smooths.
##'   The data are scaled into the unit square before deciding what to exclude,
##'   and `dist` is a distance within the unit square. See
##'   [mgcv::exclude.too.far()] for further details.
##' @param ... arguments passed to other methods.
##'
##' @return A data frame, which is of class `"evaluated_1d_smooth"` or `evaluated_2d_smooth`, which inherit from classes `"evaluated_smooth"` and `"data.frame"`.
##'
##' @importFrom mgcv PredictMat exclude.too.far
##' @importFrom stats setNames
##'
##' @export
##'
##' @examples
##' library("mgcv")
##' set.seed(2)
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' head(evaluate_smooth(m1, "s(x1)"))
##'
##' ## 2d example
##' set.seed(2)
##' dat <- gamSim(2, n = 4000, dist = "normal", scale = 1)
##' m2 <- gam(y ~ s(x, z, k = 30), data = dat$data, method = "REML")
##'
##' head(evaluate_smooth(m2, "s(x,z)", n = 100))
`evaluate_smooth` <- function(object, ...) {
    UseMethod("evaluate_smooth")
}

##' @export
##' @rdname evaluate_smooth
`evaluate_smooth.gam` <- function(object, smooth, n = 100, newdata = NULL,
                                  unconditional = FALSE, inc.mean = FALSE,
                                  dist = 0.1, ...) {
    ## simplify GAMM objects
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    ## to keep this simple, only evaluate a single smooth at a time
    if (length(smooth) > 1L) {
        message("Supplied more than 1 'smooth'; using only the first")
        smooth <- smooth[1L]
    }
    smooth_ids <- which_smooth(object, smooth) # which smooths match 'smooth'
    smooth_labels <- select_smooth(object, smooth)

    ## Need to handle by smooths here
    ## use get_smooth() on each smooth
    ## SMOOTHS <- object[["smooth"]][smooth_ids]  # take matched smooths
    SMOOTHS <- get_smooths_by_id(smooth_ids, object) # extract the mgcv.smooth object

    ## if 2d smooth, call separate fun
    if (smooth_dim(SMOOTHS[[1]]) == 1L) {
        evaluated <- evaluate_1d_smooth(SMOOTHS, n = n, model = object,
                                        newdata = newdata, inc.mean = inc.mean,
                                        unconditional = unconditional)
    } else if (smooth_dim(SMOOTHS[[1]]) == 2L) {
        evaluated <- evaluate_2d_smooth(SMOOTHS, n = n, model = object,
                                        newdata = newdata, inc.mean = inc.mean,
                                        unconditional = unconditional,
                                        dist = dist)
    } else {
        stop("Only univariate and bivariate smooths are currently supported.")
    }

    evaluated
}

##' @export
##' @rdname evaluate_smooth
`evaluate_smooth.gamm` <- function(object, ...) {
    evaluate_smooth(object[["gam"]])
}

`evaluate_1d_smooth` <- function(object, n = NULL, model = NULL, newdata = NULL,
                                 unconditional = FALSE, inc.mean = FALSE) {
    ## If more than one smooth, these should be by variables smooths
    is.by <- vapply(object, FUN = is_by_smooth, FUN.VALUE = logical(1L))
    if (length(object) > 1L) {
        if (!all(is.by)) {
            msg <- paste("Hmm, something went wrong identifying the requested smooth. Found:\n",
                         paste(vapply(object, FUN = smooth_label,
                                      FUN.VALUE = character(1L)),
                               collapse = ', '),
                         "\nNot all of these are 'by' variable smooths. Contact Maintainer.")
            stop(msg)
        }
    }

    ## get by variable info
    by_var <- unique(vapply(object, FUN = by_variable, FUN.VALUE = character(1)))

    ## get variable for this smooth
    smooth_var <- unique(vapply(object, FUN = smooth_variable, FUN.VALUE = character(1)))

    newx <- if (is.null(newdata)) {
                setNames(datagen(object[[1]], n = n,
                                 data = model[["model"]])[, "x", drop = FALSE],
                         smooth_var)
    } else if (is.data.frame(newdata)) { # data frame; select out smooth
        if (!smooth_var %in% names(newdata)) {
            stop(paste("Variable", smooth_var, "not found in 'newdata'."))
        }
        newdata[, smooth_var, drop = FALSE]
    } else if (is.numeric(newdata)) {   # vector; coerce to data frame
        setNames(data.frame(newdata), smooth_var)
    } else {                            # object we can't handle; bail out
        stop("'newdata', if supplied, must be a numeric vector or a data frame.")
    }

    ## if we have a by variable, repeat newx for each level of that variable
    is.factor.by     <- vapply(object, FUN = is_factor_by_smooth,     FUN.VALUE = logical(1L))
    is.continuous.by <- vapply(object, FUN = is_continuous_by_smooth, FUN.VALUE = logical(1L))
    if (any(is.by)) {
        if (any(is.factor.by)) { # (is.factor(model[["model"]][[by_var]])) {
            levs <- levels(model[["model"]][[by_var]])
            newx <- cbind(newx, .by_var = rep(levs, each = n))
        } else {                        # continuous by
            newx <- cbind(newx, .by_var = mean(model[["model"]][[by_var]]))
        }
        names(newx)[NCOL(newx)] <- by_var
    }

    evaluated <- vector("list", length(object))
    for (i in seq_along(evaluated)) {
        ind <- seq_len(NROW(newx))
        if (any(is.factor.by)) {
            ind <- newx[, by_var] == levs[i]
        }
        evaluated[[i]] <- spline_values(object[[i]],
                                        newdata = newx[ind, , drop = FALSE],
                                        unconditional = unconditional,
                                        model = model, inc.mean = inc.mean,
                                        term = smooth_var)
    }

    evaluated <- do.call("rbind", evaluated)

    if (any(is.factor.by)) {
        evaluated <- cbind(evaluated,
                           by_var = rep(levels(model[["model"]][[by_var]]), each = n))
        names(evaluated)[NCOL(evaluated)] <- by_var
    }

    names(evaluated)[2] <- smooth_var
    class(evaluated) <- c("evaluated_1d_smooth", "evaluated_smooth", "data.frame")

    evaluated
}

`evaluate_2d_smooth` <- function(object, n = NULL, model = NULL, newdata = NULL,
                                 unconditional = FALSE, inc.mean = FALSE, dist = 0.1) {
    ## If more than one smooth, these should be by variables smooths
    is.by <- vapply(object, FUN = is_by_smooth, FUN.VALUE = logical(1L))
    if (length(object) > 1L) {
        if (!all(is.by)) {
            msg <- paste("Hmm, something went wrong identifying the requested smooth. Found:\n",
                         paste(vapply(object, FUN = smooth_label,
                                      FUN.VALUE = character(2L)),
                               collapse = ', '),
                         "\nNot all of these are 'by' variable smooths. Contact Maintainer.")
            stop(msg)
        }
    }

    ## get by variable info
    by_var <- unique(vapply(object, FUN = by_variable, FUN.VALUE = character(1)))

    ## get variables for this smooth
    smooth_var <- unique(vapply(object, FUN = smooth_variable, FUN.VALUE = character(2L)))

    newx <- if (is.null(newdata)) {
                setNames(datagen(object[[1]], n = n,
                                 data = model[["model"]])[, c("x1", "x2"), drop = FALSE],
                         smooth_var)
    } else if (is.data.frame(newdata)) { # data frame; select out smooth
        if (!smooth_var %in% names(newdata)) {
            stop(paste("Variable", smooth_var, "not found in 'newdata'."))
        }
        newdata[, smooth_var, drop = FALSE]
    } else if (is.numeric(newdata)) {   # vector; coerce to data frame
        setNames(data.frame(newdata), smooth_var)
    } else {                            # object we can't handle; bail out
        stop("'newdata', if supplied, must be a numeric vector or a data frame.")
    }

    ## if we have a by variable, repeat newx for each level of that variable
    is.factor.by     <- vapply(object, FUN = is_factor_by_smooth,     FUN.VALUE = logical(1L))
    is.continuous.by <- vapply(object, FUN = is_continuous_by_smooth, FUN.VALUE = logical(1L))
    if (any(is.by)) {
        if (any(is.factor.by)) { # (is.factor(model[["model"]][[by_var]])) {
            levs <- levels(model[["model"]][[by_var]])
            newx <- cbind(newx, .by_var = rep(levs, each = n))
        } else {                        # continuous by
            newx <- cbind(newx, .by_var = mean(model[["model"]][[by_var]]))
        }
        names(newx)[NCOL(newx)] <- by_var
    }

    evaluated <- vector("list", length(object))
    for (i in seq_along(evaluated)) {
        ind <- seq_len(NROW(newx))
        if (any(is.factor.by)) {
            ind <- newx[, by_var] == levs[i]
        }
        evaluated[[i]] <- spline_values(object[[i]],
                                        newdata = newx[ind, , drop = FALSE],
                                        unconditional = unconditional,
                                        model = model, inc.mean = inc.mean,
                                        term = smooth_var)
    }

    evaluated <- do.call("rbind", evaluated)

    if (any(is.factor.by)) {
        evaluated <- cbind(evaluated,
                           by_var = rep(levels(model[["model"]][[by_var]]), each = n))
        names(evaluated)[NCOL(evaluated)] <- by_var
    }

    ## exclude values too far from data
    if (dist > 0) {
        ind <- mgcv::exclude.too.far(newx[, smooth_var[1L]],
                                     newx[, smooth_var[2L]],
                                     model[["model"]][, smooth_var[1L]],
                                     model[["model"]][, smooth_var[2L]],
                                     dist = dist)
        evaluated[ind, c("est", "se")] <- NA
    }

    names(evaluated)[2:3] <- smooth_var
    class(evaluated) <- c("evaluated_2d_smooth", "evaluated_smooth", "data.frame")

    ## return
    evaluated
}

## loop over smooths and predict
`spline_values` <- function(smooth, newdata, model, unconditional,
                            inc.mean = FALSE, term) {
    X <- PredictMat(smooth, newdata)   # prediction matrix
    start <- smooth[["first.para"]]
    end <- smooth[["last.para"]]
    para.seq <- start:end
    coefs <- coef(model)[para.seq]
    fit <- X %*% coefs

    label <- smooth_label(smooth)
    V <- get_vcov(model, unconditional = unconditional,
                  term = label)
    if (isTRUE(inc.mean)) {
        stop("'inc.mean == TRUE' situation not currently supported")
    } else {
        rs <- rowSums((X %*% V) * X)
        se.fit <- sqrt(pmax(0, rs))
    }
    d <- smooth_dim(smooth)
    ## Return
    out <- if (d == 1L) {
               data.frame(smooth = rep(label, nrow(X)), x = newdata[, 1L],
                          est = fit, se = se.fit)
           } else {
               data.frame(smooth = rep(label, nrow(X)),
                          x1 = newdata[, 1L], x2 = newdata[, 2L],
                          est = fit, se = se.fit)
           }
    out
}
