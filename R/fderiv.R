##' First derivatives of fitted GAM functions
##'
##' The first derivative of the smooth functions of a GAM model calculated using finite differences.
##'
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
##' @importFrom stats coef model.frame predict terms vcov
##'
##' @export
##'
##' @examples
##' library("mgcv")
##' set.seed(2)
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' ## first derivatives of all smooths...
##' fd <- fderiv(mod)
##'
##' ## ...and a selected smooth
##' fd2 <- fderiv(mod, term = "x1")
##'
##' ## Models with factors
##' set.seed(2)
##' dat <- gamSim(4, n = 400, dist = "normal", scale = 2)
##' mod <- gam(y ~ s(x0) + s(x1) + fac, data = dat, method = "REML")
##'
##' ## first derivatives of all smooths...
##' fd <- fderiv(mod)
##'
##' ## ...and a selected smooth
##' fd2 <- fderiv(mod, term = "x1")
`fderiv.gam` <- function(model, newdata, term, n = 200, eps = 1e-7,
                         unconditional = FALSE, ...) {
    ## any factors used in the model?
    facs <- attr(model$terms, "dataClasses") == "factor"

    ## where to predict/evaluate derivatives at
    if (missing(newdata)) {
        ## all model terms
        m.terms <- attr(model$terms, "term.labels")
        ## model.frame used to fit model
        mf <- model.frame(model)

        ## remove response
        respvar <- attr(model$terms, "response")
        if (!identical(respvar, 0)) {
            mf <- mf[, -respvar, drop = FALSE]
            ff <- facs[-respvar]
        }

        if (any(ff)) {
            ## need to supply something for each factor
            rep_first_factor_value <- function(f, n) {
                stopifnot(is.factor(f))
                levs <- levels(f)
                factor(rep(f[1], length.out = n), levels = levs)
            }
            f.mf <- sapply(mf[, ff, drop = FALSE],
                           rep_first_factor_value, n = n)

            ## remove factors
            mf <- mf[, !ff, drop = FALSE]
        }

        ## generate newdata at `n` locations
        newdata <- sapply(mf,
                          function(x) seq(min(x), max(x), length = n))

        if (any(ff)) {
            newdata <- cbind(mf, f.mf)
        }
        colnames(newdata) <- c(m.terms[!ff], m.terms[ff])
    }

    ## re-arrange
    newdata <- newdata[, m.terms, drop = FALSE]

    ## copy into newdata2
    newdata2 <- newdata

    if (any(ff)) {
        newdata2[, !ff] <- newdata2[, !ff, drop = FALSE] + eps
    } else {
        newdata2 <- newdata2 + eps
    }

    ## compute Xp for evaluation points
    X0 <- predict(model, as.data.frame(newdata), type = "lpmatrix")
    X1 <- predict(model, as.data.frame(newdata2), type = "lpmatrix")
    Xp <- (X1 - X0) / eps
    Xp.r <- NROW(Xp)
    Xp.c <- NCOL(Xp)

    ## number of smooth terms
    ## t.labs <- attr(model$terms, "term.labels")

    ## match the term with the the terms in the model
    if(!missing(term)) {
        m.terms <- select_terms(model, term)
    }
    nt <- length(m.terms)
    ## list to hold the derivatives
    lD <- vector(mode = "list", length = nt)
    names(lD) <- m.terms
    ## Bayesian covar
    Vb <- vcov(model, unconditional = unconditional)
    ## loop over terms, selecting the columns of Xp for the ith
    ## smooth
    for(i in seq_len(nt)) {
        Xi <- Xp * 0 # create new matrix with dim(Xp) but filled with 0
        want <- grep(m.terms[i], colnames(X1)) # which columns in Lp are for current term
        Xi[, want] <- Xp[, want]              # fill in 0-matrix with Lp data
        df <- Xi %*% coef(model)              # predict derive given model coefs
        df.sd <- rowSums(Xi %*% Vb * Xi)^.5   # standard error of predictions
        lD[[i]] <- list(deriv = df, se.deriv = df.sd, Xi = Xi)
    }
    out <- structure(list(derivatives = lD, terms = m.terms, model = model,
                          eps = eps, eval = newdata, unconditional = unconditional),
                     class = "fderiv")
    out
}

##' @rdname fderiv
##' @export
`fderiv.gamm` <- function(model, ...) {
    model <- model$gam
    fderiv.gam(model, ...)
}
