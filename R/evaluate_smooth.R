##' Evaluate a smooth
##'
##' Evaluate a smooth at a grid of evenly spaced value over the range of the covariate associated with the smooth. Alternatively, a set of points at which the smooth should be evaluated can be supplied.
##'
##' @param object an object of class `"gam"` or `"gamm"`.
##' @param term character; a single term to evaluate.
##' @param n numeric; the number of points over the range of the covariate at which to evaluate the smooth.
##' @param newdata a vector or data frame of points at which to evaluate the smooth.
##' @param unconditional logical; should confidence intervals include the uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian covariance matrix will be used.
##' @param inc.mean logical; should the uncertainty in the model constant term be included in the standard error of the evaluate values of the smooth? Currently not implemented.
##'
##' @importFrom mgcv PredictMat
##' @importFrom stats setNames
##'
##' @export
##'
##' @examples
##' library("mgcv")
##' set.seed(2)
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' head(evaluate_smooth(mod, "x1"))
evaluate_smooth <- function(object, term, n = 200, newdata = NULL,
                            unconditional = FALSE, inc.mean = FALSE) {
    ## simplify GAMM objects
    if (is.gamm(object)) {
        object <- object$gam
    }
    ## to keep this simple, only evaluate a single term at a time
    if (length(term) > 1L) {
        message("Supplied more than 1 'term'; using only the first")
        term <- term[1L]
    }
    term <- select_terms(object, term) # match the supplied term
    smooth <- get_smooth(object, term) # extract the mgcv.smooth object
    newx <- if (is.null(newdata)) {
        setNames(datagen(smooth, n = n,
                         data = object$model)[, "x", drop = FALSE],
                 term)
    } else if (is.data.frame(newdata)) { # data frame; select out term
        if (!term %in% names(newdata)) {
            stop(paste("Smooth", term, "not found in 'newdata'."))
        }
        newdata[, term, drop = FALSE]
    } else if (is.numeric(newdata)) {   # vector; coerce to data frame
        setNames(data.frame(newdata), term)
    } else {                            # object we can't handle; bail out
        stop("'newdata', if supplied, must be a numeric vector or a data frame.")
    }

    X <- PredictMat(smooth, newx)               # prediction matrix
    start <- smooth$first.para
    end <- smooth$last.para
    para.seq <- start:end
    coefs <- coef(object)[para.seq]
    fit <- X %*% coefs

    V <- get_vcov(object, unconditional = unconditional, term = term)
    if (isTRUE(inc.mean)) {
        stop("'inc.mean == TRUE' situation not currently supported")
    } else {
        rs <- rowSums((X %*% V) * X)
        se.fit <- sqrt(pmax(0, rs))
    }

    data.frame(term = rep(term, n), x = newx[,1L], est = fit, se = se.fit)
}
