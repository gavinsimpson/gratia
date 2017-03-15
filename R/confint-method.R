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

    ## level should be length 1, numeric and 0 < level < 1
    if ((ll <- length(level)) > 1L) {
        warning(paste("`level` should be length 1, but supplied length: ",
                      ll, ". Using the first only."))
        level <- rep(level, length.out = 1L)
    }
    if (!is.numeric(level)) {
        stop(paste("`level` should be numeric, but supplied:" level))
    }
    if ((0 < level) && (level < 1)) {
        stop(paste("`level` should lie in interval [0,1], but supplied:" level))
    }

    ## which type of interval is required
    type <- match.arg(type)

    ## generate intervals
    interval <- if (type == "confidence") {
        confidence(object, terms = parm, level = level)
    } else {
        simultaneous(object, terms = parm, level = level, nsim = nsim)
    }

    ## return
    intervals
}

##' @importFrom MASS mvrnorm
`simultaneous` <- function(x, terms, level, nsim) {
    ## wrapper the computes each interval
    `simInt` <- function(x, Vb, bu, level, nsim) {
        Xi <- x[["Xi"]]           # derivative Lp, zeroed except for this term
        se <- x[["se.deriv"]]     # std err of deriv for current term
        d  <- x[["deriv"]]        # deriv for current term
        simDev <- Xi %*% t(buDiff)      # simulate deviations from expected
        absDev <- abs(sweep(simDev, 1, se, FUN = "/")) # absolute deviations
        masd <- apply(absDev, 2L, max)  # & maxabs deviation per sim
        ## simultaneous interval critical value
        crit <- quantile(masd, prob = level, type = 8)
        ## return as data frame
        data.frame(lower = d - (crit * se), est = d, upper = d + (crit * se))
    }

    ## bayesian covar matrix, possibly accounting for estimating smooth pars
    Vb <- vcov(x$model, unconditional = x$unconditional)
    ## simulate un-biased deviations given bayesian covar matrix
    buDiff <- MASS::mvrnorm(n = nsim, mu = rep(0, nrow(Vb)), Sigma = Vb)
    ## apply wrapper to compute simultaneous interval critical value and
    ## corresponding simultaneous interval
    res <- lapply(x[["derivatives"]][terms], FUN = simInt,
                  Vb = Vb, bu = buDiff, level = level, nsim = nsim)
    ## how many values per term - currently all equal
    lens <- vapply(res, FUN = NROW, FUN.VALUE = integer(1))
    res <- do.call("rbind", res)        # row-bind each component of res
    res <- cbind(term = rep(terms, times = lens), res) # add on term ID
    rownames(res) <- NULL                              # tidy up
    res                                                # return
}

`confidence` <- function(x, terms, level) {
    ##
}
