##' Evaluate a smooth
##'
##' Evaluate a smooth at a grid of evenly spaced value over the range of the covariate associated with the smooth.
##'
##' @param object an object of class `"gam"` or `"gamm"`
##' @param term character; a single term to evaluate
##' @param n numeric; the number of points over the range of the covariate at which to evaluate the smooth
##' @param inc.mean logical; should the uncertainty in the model constant term be included in the standard error of the evaluate values of the smooth? Currently not implemented.
##'
##' @importFrom mgcv PredictMat
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
evaluate_smooth <- function(object, term, n = 200, inc.mean = FALSE) {
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
    smooth <- get_smooth(object, term)
    newx <- datagen(smooth, n = n, data = object$model)[, "x", drop = FALSE]
    names(newx) <- term
    X <- PredictMat(smooth, newx)               # prediction matrix
    start <- smooth$first.para
    end <- smooth$last.para
    para.seq <- start:end
    coefs <- coef(object)[para.seq]
    fit <- X %*% coefs

    Vp <- object$Vp         # variance covariance matrix of parameters
    if (isTRUE(inc.mean)) {
        stop("'inc.mean == TRUE' situation not currently supported")
    } else {
        rs <- rowSums((X %*% Vp[para.seq, para.seq, drop = FALSE]) * X)
        se.fit <- sqrt(pmax(0, rs))
    }

    data.frame(term = rep(term, n), x = newx[,1L], est = fit, se = se.fit)
}
