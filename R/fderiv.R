#' First derivatives of fitted GAM functions
#' 
#' @description
#' `r lifecycle::badge('deprecated')`
#' 
#' This function was deprecated because it was limited to first order forward
#' finite differences for derivatives only, but couldn't be improved to offer
#' the needed functionality without breaking backwards compatability with papers
#' and blog posts that already used `fderiv()`. A replacement, [derivatives()],
#' is now available and recommended for new analyses.
#'
#' @param model A fitted GAM. Currently only models fitted by [mgcv::gam()] and
#'   [mgcv::gamm()] are supported.
#' @param ... Arguments that are passed to other methods.
#'
#' @return An object of class `"fderiv"` is returned.
#' 
#' @keywords internal
#'
#' @author Gavin L. Simpson
#'
#' @export
`fderiv` <- function(model, ...) {
    lifecycle::deprecate_warn("0.7.0", "fderiv()", "derivatives()")
    UseMethod("fderiv")
}

#' @rdname fderiv
#'
#' @param newdata a data frame containing the values of the model covariates at
#'   which to evaluate the first derivatives of the smooths.
#' @param term character; vector of one or more terms for which derivatives are
#'   required. If missing, derivatives for all smooth terms will be returned.
#' @param n integer; if `newdata` is missing the original data can be
#'   reconstructed from `model` and then `n` controls the number of values over
#'   the range of each covariate with which to populate `newdata`.
#' @param eps numeric; the value of the finite difference used to approximate
#'  the first derivative.
#' @param unconditional logical; if `TRUE`, the smoothing parameter uncertainty
#'  corrected covariance matrix is used, *if available*, otherwise the
#'  uncorrected Bayesian posterior covariance matrix is used.
#' @param offset numeric; value of offset to use in generating predictions.
#'
#' @importFrom stats coef model.frame predict terms vcov
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(lifecycle_verbosity = "quiet")
#' }
#' dat <- data_sim("eg1", seed = 2)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' ## first derivatives of all smooths...
#' fd <- fderiv(mod)
#' ## now use -->
#' fd <- derivatives(mod)
#'
#' ## ...and a selected smooth
#' fd2 <- fderiv(mod, term = "x1")
#' ## now use -->
#' fd2 <- derivatives(mod, term = "s(x1)")
#'
#' ## Models with factors
#' dat <- data_sim("eg4", n = 400, dist = "normal", scale = 2, seed = 2)
#' mod <- gam(y ~ s(x0) + s(x1) + fac, data = dat, method = "REML")
#'
#' ## first derivatives of all smooths...
#' fd <- fderiv(mod)
#' ## now use -->
#' fd <- derivatives(mod)
#'
#' ## ...and a selected smooth
#' fd2 <- fderiv(mod, term = "x1")
#' ## now use -->
#' fd2 <- derivatives(mod, term = "s(x1)")
`fderiv.gam` <- function(model, newdata, term, n = 200, eps = 1e-7,
                         unconditional = FALSE, offset = NULL, ...) {

    ## where to predict/evaluate derivatives at
    if (missing(newdata)) {
        ## model.frame used to fit model
        mf <- model.frame(model)

        ## remove response
        respvar <- attr(model$terms, "response")
        if (!identical(respvar, 0)) {
            mf <- mf[, -respvar, drop = FALSE]
        }

        ## remove offset() var; model.frame returns both `offset(foo(var))` and
        ##  `var`,
        ## so we can just remove the former, but we also want to set the offset
        ## variable `var` to something constant. FIXME
        if (is.null(offset)) {
            offset <- 1L
        }
        mf <- fix_offset(model, mf, offset_val = offset)

        ff <- vapply(mf, is.factor, logical(1L))

        m.terms <- names(mf)

        if (any(ff)) {
            ## need to supply something for each factor
            rep_first_factor_value <- function(f, n) {
                stopifnot(is.factor(f))
                levs <- levels(f)
                factor(rep(f[1], length.out = n), levels = levs)
            }
            f.mf <- lapply(mf[, ff, drop = FALSE], rep_first_factor_value,
                           n = n)
            f.mf <- do.call("cbind.data.frame", f.mf)

            ## remove factors
            mf <- mf[, !ff, drop = FALSE]
        }

        ## generate newdata at `n` locations
        newdata <- lapply(mf,
                          function(x) seq(min(x), max(x), length = n))
        newdata <- do.call("data.frame", list(newdata, check.names = FALSE))

        if (any(ff)) {
            newdata <- cbind(newdata, f.mf)
        }
        colnames(newdata) <- c(m.terms[!ff], m.terms[ff])

        ## re-arrange
        newdata <- newdata[, m.terms, drop = FALSE]

        ## copy into newdata2
        newdata2 <- newdata

        if (any(ff)) {
            newdata2[, !ff] <- newdata2[, !ff, drop = FALSE] + eps
        } else {
            newdata2 <- newdata2 + eps
        }
    } else {
        ff <- vapply(newdata, is.factor, logical(1L))
        ## copy into newdata2
        newdata2 <- newdata
        ## handle factors when shifting by eps
        if (any(ff)) {
            newdata2[, !ff] <- newdata2[, !ff, drop = FALSE] + eps
        } else {
            newdata2 <- newdata2 + eps
        }
    }

    ## compute Xp for evaluation points
    X0 <- predict(model, as.data.frame(newdata), type = "lpmatrix")
    X1 <- predict(model, as.data.frame(newdata2), type = "lpmatrix")
    Xp <- (X1 - X0) / eps
    Xp.r <- NROW(Xp)
    Xp.c <- NCOL(Xp)

    ## match the term with the the terms in the model
    if(!missing(term)) {
        S <- select_terms(model, term)
        S <- add_s(S)
    } else {
        S <- smooths(model)             # model smooths
    }

    nt <- length(S)      # how many smooths do we need derivatives for

    ## list to hold the derivatives
    lD <- vector(mode = "list", length = nt)
    names(lD) <- S

    ## Bayesian covar
    Vb <- vcov(model, unconditional = unconditional)

    ## loop over smooth terms, selecting the columns of Xp for the
    ## ith smooth
    for(i in seq_len(nt)) {
        Xi <- Xp * 0 # create new matrix with dim(Xp) but filled with 0
        want <- grep(S[i], colnames(X1), fixed = TRUE) # which columns in Lp are for current term
        Xi[, want] <- Xp[, want]              # fill in 0-matrix with Lp data
        df <- Xi %*% coef(model)              # predict derive given model coefs
        df.sd <- rowSums(Xi %*% Vb * Xi)^.5   # standard error of predictions
        lD[[i]] <- list(deriv = df, se.deriv = df.sd, Xi = Xi)
    }
    out <- structure(list(derivatives = lD, terms = S, model = model,
                          eps = eps, eval = newdata,
                          unconditional = unconditional),
                     class = "fderiv")
    out
}

#' @rdname fderiv
#' @export
`fderiv.gamm` <- function(model, ...) {
    model <- model$gam
    fderiv.gam(model, ...)
}
