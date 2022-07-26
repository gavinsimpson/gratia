#' Point-wise and simultaneous confidence intervals for derivatives of smooths
#'
#' Calculates point-wise confidence or simultaneous intervals for the first
#'   derivatives of smooth terms in a fitted GAM.
#'
#' @param object an object of class `"fderiv"` containing the estimated
#'   derivatives.
#' @param parm which parameters (smooth terms) are to be given intervals as a
#'   vector of terms. If missing, all parameters are considered.
#' @param level numeric, `0 < level < 1`; the confidence level of the
#'   point-wise or simultaneous interval. The default is `0.95` for a 95%
#'   interval.
#' @param type character; the type of interval to compute. One of `"confidence"`
#'   for point-wise intervals, or `"simultaneous"` for simultaneous intervals.
#' @param nsim integer; the number of simulations used in computing the
#'   simultaneous intervals.
#' @param ncores number of cores for generating random variables from a
#'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
#'   Parallelization will take place only if OpenMP is supported (but appears
#'   to work on Windows with current `R`).
#' @param ... additional arguments for methods
#'
#' @return a data frame with components:
#' 1. `term`; factor indicating to which term each row relates,
#' 2. `lower`; lower limit of the confidence or simultaneous interval,
#' 3. `est`; estimated derivative
#' 4. `upper`; upper limit of the confidence or simultaneous interval.
#'
#' @author Gavin L. Simpson
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 2, cli.unicode = FALSE)
#' }
#' dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' # new data to evaluate the derivatives at, say over the middle 50% of range
#' # of each covariate
#' middle <- function(x, n = 25, coverage = 0.5) {
#'   v <- (1 - coverage) / 2
#'   q <- quantile(x, prob = c(0 + v, 1 - v), type = 8)
#'   seq(q[1], q[2], length = n)
#' }
#' new_data <- sapply(dat[c("x0", "x1", "x2", "x3")], middle)
#' new_data <- data.frame(new_data)
#' ## first derivatives of all smooths...
#' fd <- fderiv(mod, newdata = new_data)
#'
#' ## point-wise interval
#' ci <- confint(fd, type = "confidence")
#' ci
#'
#' ## simultaneous interval for smooth term of x2
#' \dontshow{
#' set.seed(42)
#' }
#' x2_sint <- confint(fd, parm = "x2", type = "simultaneous",
#'                    nsim = 10000, ncores = 2)
#' \donttest{
#' x2_sint
#' }
#' \dontshow{options(op)}
`confint.fderiv` <- function(object, parm, level = 0.95,
                             type = c("confidence", "simultaneous"),
                             nsim = 10000,
                             ncores = 1L, ...) {
    ## Process arguments
    ## parm is one of the terms in object
    parm <- if(missing(parm)) {
                object$terms
            } else {
                parm <- add_s(parm)
                terms <- object$terms
                want <- parm %in% terms
                if (any(!want)) {
                    msg <- paste("Terms:", paste(parm[!want], collapse = ", "),
                                 "not found in `object`")
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
        stop(paste("`level` should lie in interval [0,1], but supplied:",
             level))
    }

    ## which type of interval is required
    type <- match.arg(type)

    ## generate intervals
    interval <- if (type == "confidence") {
        confidence(object, terms = parm, level = level)
    } else {
        simultaneous(object, terms = parm, level = level, nsim = nsim,
                     ncores = ncores)
    }

    interval <- as_tibble(interval)

    class(interval) <- c("confint.fderiv", class(interval))

    ## return
    interval
}

#' @importFrom stats quantile vcov
#' @importFrom mvnfast rmvn
`simultaneous` <- function(x, terms, level, nsim, ncores) {
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
    buDiff <- rmvn(n = nsim, mu = rep(0, nrow(Vb)), sigma = Vb, ncores = ncores)
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

#' @importFrom stats qnorm
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

#' Point-wise and simultaneous confidence intervals for smooths
#'
#' Calculates point-wise confidence or simultaneous intervals for the smooth
#' terms of a fitted GAM.
#'
#' @param object an object of class `"gam"` or `"gamm"`.
#' @param parm which parameters (smooth terms) are to be given intervals as a
#'   vector of terms. If missing, all parameters are considered, although this
#'   is not currently implemented.
#' @param level numeric, `0 < level < 1`; the confidence level of the point-wise
#'   or simultaneous interval. The default is `0.95` for a 95% interval.
#' @param data data frame; new values of the covariates used in the model fit.
#'   The selected smooth(s) wil be evaluated at the supplied values.
#' @param n numeric; the number of points to evaluate smooths at.
#' @param type character; the type of interval to compute. One of `"confidence"`
#'   for point-wise intervals, or `"simultaneous"` for simultaneous intervals.
#' @param nsim integer; the number of simulations used in computing the
#'   simultaneous intervals.
#' @param shift logical; should the constant term be add to the smooth?
#' @param transform logical; should the smooth be evaluated on a transformed
#'   scale? For generalised models, this involves applying the inverse of the
#'   link function used to fit the model. Alternatively, the name of, or an
#'   actual, function can be supplied to transform the smooth and it's
#'   confidence interval.
#' @param unconditional logical; if `TRUE` (and `freq == FALSE`) then the
#'   Bayesian smoothing parameter uncertainty corrected covariance matrix is
#'   returned, if available.
#' @param ncores number of cores for generating random variables from a
#'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
#'   Parallelization will take place only if OpenMP is supported (but appears
#'   to work on Windows with current `R`).
#' @param partial_match logical; should matching `parm` use a partial match or
#'   an exact match? Can only be used if `length(parm)` is `1`.
#' @param ... additional arguments for methods
#' @param newdata DEPRECATED! data frame; containing new values of the
#'   covariates used in the model fit. The selected smooth(s) wil be evaluated
#'   at the supplied values.
#'
#' @return a data frame with components:
#' 1. `term`; factor indicating to which term each row relates,
#' 2. `x`; the vector of values at which the smooth was evaluated,
#' 3. `lower`; lower limit of the confidence or simultaneous interval,
#' 4. `est`; estimated value of the smooth
#' 5. `upper`; upper limit of the confidence or simultaneous interval,
#' 6. `crit`; critical value for the `100 * level`% confidence interval.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom stats family qnorm
#' @importFrom mgcv PredictMat
#' @importFrom stats quantile vcov setNames
#' @importFrom dplyr bind_rows
#' @importFrom tibble add_column
#' @importFrom mvnfast rmvn
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 2, cli.unicode = FALSE)
#' }
#' dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' # new data to evaluate the smooths at, say over the middle 50% of range
#' # of each covariate
#' middle <- function(x, n = 50, coverage = 0.5) {
#'   v <- (1 - coverage) / 2
#'   q <- quantile(x, prob = c(0 + v, 1 - v), type = 8)
#'   seq(q[1], q[2], length = n)
#' }
#' new_data <- sapply(dat[c("x0", "x1", "x2", "x3")], middle)
#' new_data <- data.frame(new_data)
#'
#' ## point-wise interval for smooth of x2
#' ci <- confint(mod, parm = "s(x2)", type = "confidence", data = new_data)
#' ci
#'
#' ## simultaneous interval for smooth of x2
#' \dontshow{
#' set.seed(42)
#' }
#' si <- confint(mod, parm = "s(x2)", data = new_data,
#'               type = "simultaneous", nsim = 3000, ncores = 2)
#' si
#' \dontshow{
#' options(op)
#' }
`confint.gam` <- function(object, parm, level = 0.95, data = newdata, n = 200,
                          type = c("confidence", "simultaneous"), nsim = 10000,
                          shift = FALSE, transform = FALSE,
                          unconditional = FALSE,
                          ncores = 1, partial_match = FALSE,
                          ..., newdata = NULL) {
    S <- smooths(object)
    ## select smooths
    select <- check_user_select_smooths(smooths = S,
                                        select = parm,
                                        partial_match = partial_match)
    ## did it match anything?
    if (!any(select)) {
        stop("No smooths matched <", paste(parm, collapse = ", "),
             ">. Try adding `partial_match = TRUE`?", call. = FALSE)
    }
    take <- select & (smooth_dim(object) <= 2L)
    S <- S[take]

    ## look to see if smooth is a by variable
    by_levs <- NULL
    is_by <- vapply(object[["smooth"]][take], is_by_smooth, logical(1L))
    if (any(is_by)) {
        S <- vapply(strsplit(S, ":"), `[[`, character(1L), 1L)
        by_levs <- vapply(object[["smooth"]][take], by_level, character(1L))
        by_var <- vapply(object[["smooth"]][take], by_variable, character(1L))
    }
    ## unique smooths (counts all levels of a by factor as a single smooth)
    uS <- unique(S)

    # warn if user uses newdata
    if (! is.null(newdata)) {
        newdata_deprecated()
    }

    ## how many data points if data supplied
    if (!is.null(data)) {
        n <- NROW(data)
    }

    ilink <- if (is.logical(transform)) { # transform is logical
                 if (isTRUE(transform)) { # transform == TRUE
                     family(object)$linkinv
                 } else {               # transform == FALSE
                     function(eta) { eta }
                 }
    } else if (!is.null(transform)) {   # transform is a fun
        match.fun(transform)
    }

    ## which type of confidence interval
    type <- match.arg(type)
    ## list to hold results
    out <- vector("list", length = length(uS)) # list for results

    if (isTRUE(type == "confidence")) {
        for (i in seq_along(out)) {
            out[[i]] <- evaluate_smooth(object, uS[i], n = n, newdata = data)
            out[[i]][["crit"]] <- coverage_normal(level)
        }
    } else {
        ## function to do simultaneous intervals for a smooth
        ## this should be outlined as an actual function...
        ## @param smooth list; the individual smooth to work on
        ## @param level numeric; the confidence level
        ## @param data dataframe; values to compute confidence interval at
        sim_interval <- function(smooth, level, data) {
            start <- smooth[["first.para"]]
            end <- smooth[["last.para"]]
            para.seq <- start:end
            Cg <- PredictMat(smooth, data)
            simDev <- Cg %*% t(buDiff[, para.seq])
            absDev <- abs(sweep(simDev, 1L, data[["se"]], FUN = "/"))
            masd <- apply(absDev, 2L, max)
            quantile(masd, probs = level, type = 8)
        }
        ## need VCOV for simultaneous intervals
        V <- get_vcov(object, unconditional = unconditional)
        ## simulate un-biased deviations given bayesian covar matrix
        buDiff <- rmvn(n = nsim, mu = rep(0, nrow(V)), sigma = V,
                       ncores = ncores)
        ## loop over smooths
        for (i in seq_along(out)) {
            ## evaluate smooth
            out[[i]] <- evaluate_smooth(object, uS[i], n = n, newdata = data)

            # if this is a by var smooth, we need to do this for each level of
            # by var
            if (is.null(by_levs)) {        # not by variable smooth
                smooth <- get_smooth(object, parm) # get the specific smooth
                crit <- sim_interval(smooth, level = level, data = out[[i]])
                out[[i]][["crit"]] <- crit # add on the critical value
            } else {                       # is a by variable smooth
                # filter out rows that have nothing to do with this by smooth &
                # set of levels we are evaluating
                out[[i]] <- out[[i]][out[[i]][[6L]] %in% by_levs, ]
                out[[i]][["crit"]] <- 0    # fill in a variable critS
                smooth <- old_get_smooth(object, parm)
                for (l in seq_along(by_levs)) {
                    ## the 6L should really refer to the by_variable column...
                    ind <- out[[i]][[6L]] == by_levs[l] # take only needed rows
                    # handle case where user has specific a specific by smooth
                    # level "s(x2):fac1"
                    sm <- if (is_mgcv_smooth(smooth)) {
                        smooth
                    } else {
                        smooth[[l]]
                    }
                    crit <- sim_interval(sm, level = level,
                                         data = out[[i]][ind, ])
                    out[[i]][["crit"]][ind] <- crit # add on the critical value
                }
            }
        }
    }

    if (shift) {
        const <- coef(object)
        nms <- names(const)
        test <- grep("Intercept", nms)
        const <- ifelse(length(test) == 0L, 0, const[test])
    } else {
        const <- 0
    }

    ## simplify to a data frame for return
    #out <- do.call("bind_rows", out)
    out <- bind_rows(out)

    # This was needed with `[.evaluated_smooth` before switching to
    # NextMethod() to call the next S3 `[` method.
    # See: https://github.com/tidyverse/tibble/issues/511#issuecomment-431225229
    # Note needed, it seems now that NextMethod() is used but extending tibbles
    # is not currently well documented.
    # class(out) <- class(out)[-(1:2)]

    ## using se and crit, compute the lower and upper intervals
    out <- add_column(out,
                      lower = out$est - (out$crit * out$se),
                      upper = out$est + (out$crit * out$se))

    ## transform
    out[["est"]]   <- ilink(out[["est"]] + const)
    out[["lower"]] <- ilink(out[["lower"]] + const)
    out[["upper"]] <- ilink(out[["upper"]] + const)

    ## prepare for return
    class(out) <- c("confint.gam", class(out))
    out                                 # return
}

#' @rdname confint.gam
#'
#' @importFrom stats confint
#'
#' @export
`confint.gamm` <- function(object, ...) {
    confint(object[["gam"]], ...)
}

#' @rdname confint.gam
#'
#' @importFrom stats confint
#' 
#' @export
`confint.list` <- function(object, ...) {
    if (!is_gamm4(object)) {
        stop("`object` does not appear to a `gamm4` model object",
             call. = FALSE)
    }
    confint(object[["gam"]], ...)
}
