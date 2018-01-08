##' Point-wise and simultaneous confidence intervals for derivatives of smooths
##'
##' Calculates point-wise confidence or simultaneous intervals for the first derivatives of smooth terms in a fitted GAM.
##'
##' @param object an object of class `"fderiv"` containing the estimated derivatives.
##' @param parm which parameters (smooth terms) are to be given intervals as a vector of terms. If missing, all parameters are considered.
##' @param level numeric, `0 < level < 1`; the confidence level of the point-wise or simultaneous interval. The default is `0.95` for a 95\% interval.
##' @param type character; the type of interval to compute. One of `"confidence"` for point-wise intervals, or `"simultaneous"` for simultaneous intervals.
##' @param nsim integer; the number of simulations used in computing the simultaneous intervals.
##' @param ... additional arguments for methods
##'
##' @return a data frame with components:
##' 1. `term`; factor indicating to which term each row relates,
##' 2. `lower`; lower limit of the confidence or simultaneous interval,
##' 3. `est`; estimated derivative
##' 4. `upper`; upper limit of the confidence or simultaneous interval.
##'
##' @author Gavin L. Simpson
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
##' ## point-wise interval
##' ci <- confint(fd, type = "confidence")
##' head(ci)
##'
##' ## simultaneous interval for smooth term of x1
##' set.seed(42)
##' x1.sint <- confint(fd, parm = "x1", type = "simultaneous", nsim = 1000)
##' head(x1.sint)
`confint.fderiv` <- function(object, parm, level = 0.95,
                             type = c("confidence", "simultaneous"), nsim = 10000, ...) {
    ## Process arguments
    ## parm is one of the terms in object
    parm <- if(missing(parm)) {
        object$terms
    } else {
        terms <- object$terms
        want <- parm %in% terms
        if (any(!want)) {
            msg <- paste("Terms:", paste(parm[!want], collapse = ", "), "not found in `object`")
            stop(msg)
        }
        parm[want]
    }
    ## parm <- add_s(parm)

    ## level should be length 1, numeric and 0 < level < 1
    if ((ll <- length(level)) > 1L) {
        warning(paste("`level` should be length 1, but supplied length: ",
                      ll, ". Using the first only."))
        level <- rep(level, length.out = 1L)
    }
    if (!is.numeric(level)) {
        stop(paste("`level` should be numeric, but supplied:", level))
    }
    if (! (0 < level) && (level < 1)) {
        stop(paste("`level` should lie in interval [0,1], but supplied:", level))
    }

    ## which type of interval is required
    type <- match.arg(type)

    ## generate intervals
    interval <- if (type == "confidence") {
        confidence(object, terms = parm, level = level)
    } else {
        simultaneous(object, terms = parm, level = level, nsim = nsim)
    }

    class(interval) <- c("confint.fderiv", "data.frame")

    ## return
    interval
}

##' @importFrom stats quantile vcov
##' @importFrom MASS mvrnorm
`simultaneous` <- function(x, terms, level, nsim) {
    ## wrapper the computes each interval
    `simInt` <- function(x, Vb, bu, level, nsim) {
        Xi <- x[["Xi"]]           # derivative Lp, zeroed except for this term
        se <- x[["se.deriv"]]     # std err of deriv for current term
        d  <- x[["deriv"]]        # deriv for current term
        simDev <- Xi %*% t(bu)      # simulate deviations from expected
        absDev <- abs(sweep(simDev, 1, se, FUN = "/")) # absolute deviations
        masd <- apply(absDev, 2L, max)  # & maxabs deviation per sim
        ## simultaneous interval critical value
        crit <- quantile(masd, prob = level, type = 8)
        ## return as data frame
        data.frame(lower = d - (crit * se), est = d, upper = d + (crit * se))
    }

    ## bayesian covar matrix, possibly accounting for estimating smooth pars
    Vb <- vcov(x[["model"]], unconditional = x$unconditional)
    ## simulate un-biased deviations given bayesian covar matrix
    buDiff <- MASS::mvrnorm(n = nsim, mu = rep(0, nrow(Vb)), Sigma = Vb)
    ## apply wrapper to compute simultaneous interval critical value and
    ## corresponding simultaneous interval for each term
    res <- lapply(x[["derivatives"]][terms], FUN = simInt,
                  Vb = Vb, bu = buDiff, level = level, nsim = nsim)
    ## how many values per term - currently all equal
    lens <- vapply(res, FUN = NROW, FUN.VALUE = integer(1))
    res <- do.call("rbind", res)        # row-bind each component of res
    res <- cbind(term = rep(terms, times = lens), res) # add on term ID
    rownames(res) <- NULL                              # tidy up
    res                                                # return
}

##' @importFrom stats qnorm
`confidence` <- function(x, terms, level) {
    ## wrapper the computes each interval
    `confInt` <- function(x, level) {
        se <- x[["se.deriv"]]     # std err of deriv for current term
        d  <- x[["deriv"]]        # deriv for current term
        ## confidence interval critical value
        crit <- qnorm(1 - ((1 - level) / 2))
        ## return as data frame
        data.frame(lower = d - (crit * se), est = d, upper = d + (crit * se))
    }

    ## apply wrapper to compute confidence interval critical value and
    ## corresponding confidence interval for each term
    res <- lapply(x[["derivatives"]][terms], FUN = confInt, level = level)
    ## how many values per term - currently all equal
    lens <- vapply(res, FUN = NROW, FUN.VALUE = integer(1))
    res <- do.call("rbind", res)        # row-bind each component of res
    res <- cbind(term = rep(terms, times = lens), res) # add on term ID
    rownames(res) <- NULL                              # tidy up
    res                                                # return
}

##' Point-wise and simultaneous confidence intervals for smooths
##'
##' Calculates point-wise confidence or simultaneous intervals for the smooth terms of a fitted GAM.
##'
##' @param object an object of class `"gam"` or `"gamm"`.
##' @param parm which parameters (smooth terms) are to be given intervals as a vector of terms. If missing, all parameters are considered, although this is not currently implemented.
##' @param level numeric, `0 < level < 1`; the confidence level of the point-wise or simultaneous interval. The default is `0.95` for a 95\% interval.
##' @param newdata data frame; containing new values of the covariates used in the model fit. The selected smooth(s) wil be evaluated at the supplied values.
##' @param n numeric; the number of points to evaluate smooths at.
##' @param type character; the type of interval to compute. One of `"confidence"` for point-wise intervals, or `"simultaneous"` for simultaneous intervals.
##' @param nsim integer; the number of simulations used in computing the simultaneous intervals.
##' @param shift logical; should the constant term be add to the smooth?
##' @param transform logical; should the smooth be evaluated on a transformed scale? For generalised models, this involves applying the inverse of the link function used to fit the model. Alternatively, the name of, or an actual, function can be supplied to transform the smooth and it's confidence interval.
##' @param unconditional logical; if `TRUE` (and `freq == FALSE`) then the Bayesian smoothing parameter uncertainty corrected covariance matrix is returned, if available.
##' @param ... additional arguments for methods
##'
##' @return a data frame with components:
##' 1. `term`; factor indicating to which term each row relates,
##' 2. `x`; the vector of values at which the smooth was evaluated,
##' 3. `lower`; lower limit of the confidence or simultaneous interval,
##' 4. `est`; estimated value of the smooth
##' 5. `upper`; upper limit of the confidence or simultaneous interval,
##' 6. `crit`; critical value for the `100 * level`% confidence interval.
##'
##' @author Gavin L. Simpson
##'
##' @importFrom stats family
##' @importFrom mgcv PredictMat
##' @importFrom stats quantile vcov setNames
##' @importFrom MASS mvrnorm
##'
##' @export
##'
##' @examples
##' library("mgcv")
##' set.seed(2)
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' ## point-wise interval
##' ci <- confint(mod, parm = "x1", type = "confidence")
##' head(ci)
##'
##' ## simultaneous interval for smooth term of x1
##' set.seed(42)
##' si <- confint(mod, parm = "x1", type = "simultaneous", nsim = 100)
##' head(si)
`confint.gam` <- function(object, parm, level = 0.95, newdata = NULL, n = 200,
                          type = c("confidence", "simultaneous"), nsim = 10000,
                          shift = FALSE, transform = FALSE, unconditional = FALSE,
                          ...) {
    parm <- add_s(parm)
    parm <- select_smooth(object, parm) # select_terms(object, parm)

    ## how many data points if newdata supplied
    if (!is.null(newdata)) {
        n <- NROW(newdata)
    }

    type <- match.arg(type)

    ilink <- if (is.logical(transform)) { # transform is logical
                 if (isTRUE(transform)) { # transform == TRUE
                     family(object)$linkinv
                 } else {               # transform == FALSE
                     function(eta) { eta }
                 }
    } else if (!is.null(transform)) {   # transform is a fun
        match.fun(transform)
    }

    out <- vector("list", length = length(parm)) # list for results
    if (isTRUE(type == "simultaneous")) {
        ## need VCOV for simultaneous intervals
        V <- get_vcov(object, unconditional = unconditional)
        ## simulate un-biased deviations given bayesian covar matrix
        buDiff <- MASS::mvrnorm(n = nsim, mu = rep(0, nrow(V)), Sigma = V)
    }

    for (i in seq_along(out)) {
        out[[i]] <- evaluate_smooth(object, parm[i], n = n, newdata = newdata)
        crit <- if (isTRUE(type == "confidence")) {
            qnorm(1 - ((1 - level) / 2))
        } else {
            smooth <- get_smooth(object, parm[i])
            start <- smooth[["first.para"]]
            end <- smooth[["last.para"]]
            para.seq <- start:end
            newx <- setNames(data.frame(out[[i]][, 2L]), smooth_variable(smooth))
            Cg <- PredictMat(smooth, newx)
            simDev <- Cg %*% t(buDiff[, para.seq])
            absDev <- abs(sweep(simDev, 1L, out[[i]][, "se"], FUN = "/"))
            masd <- apply(absDev, 2L, max)
            quantile(masd, probs = level, type = 8)
        }
        out[[i]] <- cbind(out[[i]],
                          lower = out[[i]][, "est"] - (crit * out[[i]][, "se"]),
                          upper = out[[i]][, "est"] + (crit * out[[i]][, "se"]),
                          crit  = rep(crit, length.out = nrow(out[[i]])))
    }

    const <- coef(object)
    nms <- names(const)
    test <- grep("Intercept", nms)
    const <- ifelse(length(test) == 0L, 0, const[test])

    ## simplify to a data frame for return
    out <- do.call("rbind", out)
    out[, "est"]   <- ilink(out[, "est"] + const)
    out[, "lower"] <- ilink(out[, "lower"] + const)
    out[, "upper"] <- ilink(out[, "upper"] + const)

    class(out) <- c("confint.gam", "data.frame")
    out                                 # return
}

##' @rdname confint.gam
##'
##' @importFrom stats confint
##'
##' @export
`confint.gamm` <- function(object, ...) {
    confint(object[["gam"]], ...)
}
