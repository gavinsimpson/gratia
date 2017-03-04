##' First derivatives of fitted GAM functions
##'
##' The first derivative of the smooth functions of a GAM model calculated using finite differences.
##'
##' .. content for \details{} ..
##' @param model A fitted GAM. Currently only models fitted by [mgcv::gam()] and [mgcv::gamm()] are supported.
##' @param ... Arguments that are passed to other methods.
##'
##' @return An object of class `"fderiv"` is returned.
##'
##' @author Gavin L. Simpson
##'
##' @export
`fderiv` <- function(model, ...) {
    UseMethod("fderiv")
}

##' @rdname fderiv
##'
##' @param newdata a data frame containing the values of the model covariates at which to evaluate the first derivatives of the smooths.
##' @param term character; vector of one or more terms for which derivatives are required. If missing, derivatives for all smooth terms will be returned.
##' @param n integer; if `newdata` is missing the original data can be reconstructed from `model` and then `n` controls the number of values over the range of each covariate with which to populate `newdata`.
##' @param eps numeric; the value of the finite difference used to approximate the first derivative.
##' @param unconditional logical; if `TRUE`, the smoothing parameter uncertainty corrected covariance matrix is used, *if available*, otherwise the uncorrected Bayesian posterior covariance matrix is used.
##'
##' @export
`fderiv.gam` <- function(model, newdata, term, n = 200, eps = 1e-7,
                         unconditional = FALSE) {
    m.terms <- attr(terms(model), "term.labels")
    if(missing(newdata)) {
        newdata <- sapply(model.frame(model)[, m.terms, drop = FALSE],
                       function(x) seq(min(x), max(x), length = n))
        names(newdata) <- m.terms
    }
    X0 <- predict(model, data.frame(newdata), type = "lpmatrix")
    X1 <- predict(model, data.frame(newdata + eps), type = "lpmatrix")
    Xp <- (X1 - X0) / eps
    Xp.r <- NROW(Xp)
    Xp.c <- NCOL(Xp)
    ## dims of bs
    bs.dims <- sapply(model$smooth, "[[", "bs.dim") - 1
    ## number of smooth terms
    t.labs <- attr(model$terms, "term.labels")
    ## match the term with the the terms in the model
    if(!missing(term)) {
        want <- grep(term, t.labs)
        if(!identical(length(want), length(term))) {
            stop("One or more 'term's not found in model!")
        }
        t.labs <- t.labs[want]
    }
    nt <- length(t.labs)
    ## list to hold the derivatives
    lD <- vector(mode = "list", length = nt)
    names(lD) <- t.labs
    ## Bayesian covar
    Vb <- vcov(model, unconditional = unconditional)
    ## loop over terms, selecting the columns of Xp for the ith
    ## smooth
    for(i in seq_len(nt)) {
        Xi <- Xp * 0
        want <- grep(t.labs[i], colnames(X1))
        Xi[, want] <- Xp[, want]
        df <- Xi %*% coef(model)
        df.sd <- rowSums(Xi %*% Vb * Xi)^.5
        lD[[i]] <- list(deriv = df, se.deriv = df.sd)
    }
    class(lD) <- "fderiv"
    lD$model <- model
    lD$eps <- eps
    lD$eval <- newdata
    lD
}

##' @rdname fderiv
##' @export
`fderiv.gamm` <- function(model, ...) {
    model <- model$gam
    fderiv.gam(model, ...)
}
