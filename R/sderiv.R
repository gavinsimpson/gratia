##' Second derivatives of fitted GAM functions
##'
##' The second derivative of the smooth functions of a GAM model calculated using finite differences.
##'
##' @param model A fitted GAM. Currently only models fitted by [mgcv::gam()] and [mgcv::gamm()] are supported.
##' @param ... Arguments that are passed to other methods.
##'
##' @return An object of class `"fderiv"` is returned.
##'
##' @author Gavin L. Simpson
##'
##' @export
`sderiv` <- function(model, ...) {
        UseMethod("sderiv")
}

##' @rdname sderiv
##'
##' @param newdata a data frame containing the values of the model covariates at which to evaluate the second derivatives of the smooths.
##' @param term character; vector of one or more terms for which derivatives are required. If missing, derivatives for all smooth terms will be returned.
##' @param n integer; if `newdata` is missing the original data can be reconstructed from `model` and then `n` controls the number of values over the range of each covariate with which to populate `newdata`.
##' @param eps numeric; the value of the finite difference used to approximate the second derivative.
##' @param unconditional logical; if `TRUE`, the smoothing parameter uncertainty corrected covariance matrix is used, *if available*, otherwise the uncorrected Bayesian posterior covariance matrix is used.
##' @param offset numeric; value of offset to use in generating predictions.
##' @param d is the order of the difference operator: can be 2, 4, or 6, representing the second derivative estimates obtained from 2nd differences, 4th differences, or 6th differences
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
##' ## second derivatives of all smooths...
##' sd <- sderiv(mod)
##'
##' ## ...and a selected smooth
##' sd2 <- sderiv(mod, term = "x1")
##'
##' ## Models with factors
##' set.seed(2)
##' dat <- gamSim(4, n = 400, dist = "normal", scale = 2)
##' mod <- gam(y ~ s(x0) + s(x1) + fac, data = dat, method = "REML")
##'
##' ## second derivatives of all smooths...
##' sd <- sderiv(mod)
##'
##' ## ...and a selected smooth
##' sd2 <- sderiv(mod, term = "x1")
`sderiv.gam` <- function(model, newdata, term, n = 200, eps = 1, d = 6,
                         unconditional = FALSE, offset = NULL, ...) {
        
        if(is.na(match(d, c(1, 2, 4, 6)))) {
                stop("d must be 2, 4 or 6.")
        }
        
        hvec <- 1:(n/2)
        hvec <- (hvec - 1) %/% (d/2)
        hvec[hvec > eps] <- eps
        if(n %% 2 != 0){
                hvec <- c(hvec, max(hvec), rev(hvec))	# (n odd)
        } else{
                hvec <- c(hvec, rev(hvec))
        }
        hvec[hvec == 0] <- 1	#
        
        # similarly, dvec stores the actual values (2, 4, or 6) for the
        # difference operator d along the series:
        dvec <- rep(d, n)
        if(d == 6) {
                dvec[c(2, n - 1)] <- 2
                dvec[c(3, n - 2)] <- 4
        } else if(d == 4) {
                dvec[c(2, n - 1)] <- 2
        }
        dvec[c(1, n)] <- 0
        
        ## where to predict/evaluate derivatives at
        if (missing(newdata)) {
                ## model.frame used to fit model
                mf <- model.frame(model)
                
                ## remove response
                respvar <- attr(model$terms, "response")
                if (!identical(respvar, 0)) {
                        mf <- mf[, -respvar, drop = FALSE]
                }
                
                ## remove offset() var; model.frame returns both `offset(foo(var))` and `var`,
                ## so we can just remove the former, but we also want to set the offset
                ## variable `var` to something constant. FIXME
                if (is.null(offset)) {
                        offset <- 1L
                }
                mf <- fix_offset(model, mf, offset_value = offset)
                
                ff <- vapply(mf, is.factor, logical(1L))
                
                m.terms <- names(mf)
                
                if (any(ff)) {
                        ## need to supply something for each factor
                        rep_first_factor_value <- function(f, n) {
                                stopifnot(is.factor(f))
                                levs <- levels(f)
                                factor(rep(f[1], length.out = n), levels = levs)
                        }
                        f.mf <- lapply(mf[, ff, drop = FALSE], rep_first_factor_value, n = n)
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
                
                ## copy into newdata
                nd_0 <- newdata
                
                if (any(ff)) {
                        nd_0[, !ff] <- nd_0[, !ff, drop = FALSE]
                        
                } else {
                        nd_0 <- nd_0
                }
                
        } else {
                ff <- vapply(newdata, is.factor, logical(1L))
                ## copy into newdata2
                nd_0 <- newdata
                ## handle factors when shifting by eps
                if (any(ff)) {
                        nd_0[, !ff] <- nd_0[, !ff, drop = FALSE]
                } else {
                        nd_0 <- nd_0
                }
        }
        
        difference_operator <- function(diff_op = 1, xinds, hvals){
                
                if(!diff_op %in% c(1, 2,4,6)){
                        stop("diff_op must be 1, 2, 4 or 6.")
                }
                
                x_0  <- predict(model, as.data.frame(nd_0[xinds, , drop = FALSE]), type = "lpmatrix")
                x_p  <- predict(model, as.data.frame(nd_0[xinds, , drop = FALSE] + hvals), type = "lpmatrix")
                x_m  <- predict(model, as.data.frame(nd_0[xinds, , drop = FALSE] - hvals), type = "lpmatrix")
                x_p2 <- predict(model, as.data.frame(nd_0[xinds, , drop = FALSE] + 2 * hvals), type = "lpmatrix")
                x_m2 <- predict(model, as.data.frame(nd_0[xinds, , drop = FALSE] - 2 * hvals), type = "lpmatrix")
                x_p3 <- predict(model, as.data.frame(nd_0[xinds, , drop = FALSE] + 3 * hvals), type = "lpmatrix")
                x_m3 <- predict(model, as.data.frame(nd_0[xinds, , drop = FALSE] - 3 * hvals), type = "lpmatrix")
                
                # if(diff_op == 1){
                #         xp <- (x_p - x_0) / hvals
                # }
                if(diff_op == 2){
                        xp <- (x_p - 2 * x_0 + x_m)/hvals^2
                }
                if(diff_op == 4){
                        xp <- (-x_p2 + 16 * x_p - 30 * x_0 + 16 * x_m - x_m2)/(12 * hvals^2)
                }
                if(diff_op == 6){
                        xp <- (2 * x_p3 - 27 * x_p2 + 270 * x_p - 490 * x_0 + 270 * x_m - 27 * x_m2 + 2 * x_m3)/(180 * hvals^2)
                }
                xp
        }
        
        
        ## compute Xp for evaluation points
        # X_0 <- predict(model, as.data.frame(newdata), type = "lpmatrix")
        # X_p <- predict(model, as.data.frame(newdata), type = "lpmatrix")
        # Xp <- (X_p - X_0) / eps
        # Xp.r <- NROW(Xp)
        # Xp.c <- NCOL(Xp)
        ## create an empty model matrix
        X_p <- predict(model, nd_0, type = "lpmatrix") * 0
        
        # X_p[dvec == 2, ] <- difference_operator(diff_op = 2,
        #                                         xinds = (1:n)[dvec == 2],
        #                                         hvals = hvec[dvec == 2])
        # X_p[dvec == 4, ] <- difference_operator(diff_op = 4,
        #                                         xinds = (1:n)[dvec == 4],
        #                                         hvals = hvec[dvec == 4])
        # X_p[dvec == 6, ] <- difference_operator(diff_op = 6,
        #                                         xinds = (1:n)[dvec == 6],
        #                                         hvals = hvec[dvec == 6])
        
        X_p <- difference_operator(diff_op = 6,
                                   xinds = (1:n),
                                   hvals = hvec)
        
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
                Xi <- X_p * 0 # create new matrix with dim(Xp) but filled with 0
                want <- grep(S[i], colnames(X_p), fixed = TRUE) # which columns in Lp are for current term
                Xi[, want] <- X_p[, want]              # fill in 0-matrix with Lp data
                df <- Xi %*% coef(model)              # predict derive given model coefs
                df.sd <- rowSums(Xi %*% Vb * Xi)^.5   # standard error of predictions
                lD[[i]] <- list(deriv = df, se.deriv = df.sd, Xi = Xi)
        }
        out <- structure(list(derivatives = lD, terms = S, model = model,
                              eps = eps, eval = newdata, unconditional = unconditional),
                         class = "fderiv")
        out
}

##' @rdname sderiv
##' @export
`sderiv.gamm` <- function(model, ...) {
        model <- model$gam
        sderiv.gam(model, ...)
}