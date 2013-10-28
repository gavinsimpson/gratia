##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param mod 
##' @param ... 
##' @return 
##' @author Gavin
`fderiv` <- function(mod, ...) {
    UseMethod(mod)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param mod 
##' @param n 
##' @param eps 
##' @param newdata 
##' @return 
##' @author Gavin
`fderiv.gam` <- function(mod, n = 200, eps = 1e-7, newdata) {
    if(inherits(mod, "gamm"))
        mod <- mod$gam
    m.terms <- attr(terms(mod), "term.labels")
    if(missing(newdata)) {
        newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                       function(x) seq(min(x), max(x), length = n))
        names(newD) <- m.terms
    } else {
        newD <- newdata
    }
    X0 <- predict(mod, data.frame(newD), type = "lpmatrix")
    newD <- newD + eps
    X1 <- predict(mod, data.frame(newD), type = "lpmatrix")
    Xp <- (X1 - X0) / eps
    Xp.r <- NROW(Xp)
    Xp.c <- NCOL(Xp)
    ## dims of bs
    bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
    ## number of smooth terms
    t.labs <- attr(mod$terms, "term.labels")
    nt <- length(t.labs)
    ## list to hold the derivatives
    lD <- vector(mode = "list", length = nt)
    names(lD) <- t.labs
    ## loop over terms, selecting the columns of Xp for the ith
    ## smooth
    for(i in seq_len(nt)) {
        Xi <- Xp * 0
        want <- grep(t.labs[i], colnames(X1))
        Xi[, want] <- Xp[, want]
        df <- Xi %*% coef(mod)
        df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
        lD[[i]] <- list(deriv = df, se.deriv = df.sd)
    }
    class(lD) <- "fderiv"
    lD$gamModel <- mod
    lD$eps <- eps
    lD$eval <- newD - eps
    lD
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param mod 
##' @param ... 
##' @return 
##' @author Gavin
`fderiv.gamm` <- function(mod, ...) {
    mod <- mod$gam
    fderiv.gam(mod, ...)
}
