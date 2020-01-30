##' @title Derivatives of estimated smooths via finite differences
##'
##' @param object an R object to compute derivatives for.
##' @param ... arguments passed to other methods.
##'
##' @author Gavin L. Simpson
##'
##' @export
`derivatives` <- function(object, ...) {
    UseMethod("derivatives")
}

##' @rdname derivatives
##' @export
`derivatives.default` <- function(object, ...) {
    ## want to bail with a useful error;
    ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
    stop("Don't know how to calculate derivatives for <",
         class(object)[[1L]], ">",
         call. = FALSE)           # don't show the call, simpler error
}

##' @rdname derivatives
##'
##' @export
`derivatives.gamm` <- function(object, ...) {
    derivatives(object[["gam"]], ...)
}

##' @param term character; vector of one or more smooth terms for which
##'   derivatives are required. If missing, derivatives for all smooth terms
##'   will be returned. Can be a partial match to a smooth term.
##' @param newdata a data frame containing the values of the model covariates
##'   at which to evaluate the first derivatives of the smooths.
##' @param order numeric; the order of derivative.
##' @param type character; the type of finite difference used. One of
##'   `"forward"`, `"backward"`, or `"central"`.
##' @param n numeric; the number of points to evaluate the derivative at.
##' @param eps numeric; the finite difference.
##' @param interval character; the type of interval to compute. One of
##'   `"confidence"` for point-wise intervals, or `"simultaneous"` for
##'   simultaneous intervals.
##' @param n_sim integer; the number of simulations used in computing the
##'   simultaneous intervals.
##' @param level numeric; `0 < level < 1`; the confidence level of the
##'   point-wise or simultaneous interval. The default is `0.95` for a 95%
##'   interval.
##' @param unconditional logical; use smoothness selection-corrected Bayesian
##'   covariance matrix?
##' @param frequentist logical; use the frequentist covariance matrix?
##' @param offset numeric; a value to use for any offset term
##' @param ncores number of cores for generating random variables from a
##'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
##'   Parallelization will take place only if OpenMP is supported (but appears
##'   to work on Windows with current `R`).
##'
##' @export
##'
##' @rdname derivatives
##'
##' @return A tibble, currently with the following variables:
##' * `smooth`: the smooth each row refers to,
##' * `var`: the name of the variable involved in the smooth,
##' * `data`: values of `var` at which the derivative was evaluated,
##' * `derivative`: the estimated derivative,
##' * `se`: the standard error of the estimated derivative,
##' * `crit`: the critical value such that `derivative` ± `(crit * se)` gives
##'   the upper and lower bounds of the requested confidence or simultaneous
##'   interval (given `level`),
##' * `lower`: the lower bound of the confidence or simultaneous interval,
##' * `upper`: the upper bound of the confidence or simultaneous interval.
##'
##' @examples
##'
##' load_mgcv()
##' \dontshow{
##' set.seed(42)
##' op <- options(cli.unicode = FALSE)
##' }
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
##' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' ## first derivatives of all smooths using central finite differences
##' derivatives(mod, type = "central")
##' \dontshow{
##' options(op)
##' }
`derivatives.gam` <- function(object, term, newdata, order = 1L,
                              type = c("forward", "backward", "central"),
                              n = 200, eps = 1e-7,
                              interval = c("confidence", "simultaneous"),
                              n_sim = 10000, level = 0.95,
                              unconditional = FALSE, frequentist = FALSE,
                              offset = NULL, ncores = 1, ...) {
    ## handle term
    smooth_ids <- if (!missing(term)) {
        which_smooths(object, term) # which smooths match 'term'
    } else {
        seq_len(n_smooths(object))
    }

    ## handle type
    type <- match.arg(type)

    ## handle order
    if (!order %in% c(1L, 2L)) {
        stop("Only 1st or 2nd derivatives are supported: `order %in% c(1,2)`")
    }

    ## handle newdata
    need_newdata <- FALSE
    if (missing(newdata)) {
        need_newdata <- TRUE
    }

    ## handle interval
    interval <- match.arg(interval)

    ## get the required covariance matrix
    Vb <- get_vcov(object, unconditional = unconditional,
                   frequentist = frequentist)
    ## extract model coefs
    betas <- coef(object)

    ## how many smooths need working on
    ns <- length(smooth_ids)
    result <- vector(mode = "list", length = ns)

    ## loop over the smooths and compute derivatives from finite differences
    for (i in seq_along(smooth_ids)) {
        ## generate newdata if not supplied
        if (need_newdata) {
            newdata <- derivative_data(object, id = smooth_ids[[i]], n = n,
                                       offset = offset, order = order,
                                       type = type, eps = eps)
        }

        ## generate list of finite difference predictions for the first or second
        ##   derivatives or the required type
        fd <- finite_diff_lpmatrix(object, type = type, order = order,
                                   newdata = newdata, h = eps)

        ## compute the finite differences
        X <- finite_difference(fd, order, type, eps)

        ## compute derivatives
        d <- compute_derivative(smooth_ids[[i]], lpmatrix = X, betas = betas,
                                Vb = Vb, model = object, newdata = newdata)

        ## compute intervals
        if (identical(interval, "confidence")) {
            result[[i]] <- derivative_pointwise_int(d[["deriv"]], level = level,
                                                    distrib = "normal")
        } else {
            result[[i]] <- derivative_simultaneous_int(d[["deriv"]], d[["Xi"]],
                                                       level = level, Vb = Vb,
                                                       n_sim = n_sim,
                                                       ncores = ncores)
        }
    }

    ## results in a list of tibbles that we need to bind row-wise
    result <- do.call("bind_rows", result)

    class(result) <- c("derivatives", class(result)) # add class
    result                                           # return
}

##' @importFrom tibble add_column
`derivative_pointwise_int` <- function(x, level, distrib = c("normal", "t"),
                                       df) {
    distrib <- match.arg(distrib)
    crit <- if (distrib == "normal") {
        coverage_normal(level = level)
    } else{
        coverage_t(level = level, df = df)
    }
    adj <- (crit * x[["se"]])
    derivative <- add_column(x,
                             crit  = rep(crit, nrow(x)),
                             lower = x[["derivative"]] - adj,
                             upper = x[["derivative"]] + adj)
    derivative
}

##' @importFrom tibble add_column
##' @importFrom stats quantile
##' @importFrom mvnfast rmvn
`derivative_simultaneous_int` <- function(x, Xi, level, Vb, n_sim, ncores) {
    ## simulate un-biased deviations given bayesian covariance matrix
    buDiff <- rmvn(n = n_sim, mu = rep(0, nrow(Vb)), sigma = Vb, ncores = ncores)
    simDev <- tcrossprod(Xi, buDiff) # Xi %*% t(bu) # simulate deviations from expected
    absDev <- abs(sweep(simDev, 1L, x[["se"]], FUN = "/")) # absolute deviations
    masd <- apply(absDev, 2L, max)  # & max abs deviation per sim
    ## simultaneous interval critical value
    crit <- quantile(masd, prob = level, type = 8)
    adj <- (crit * x[["se"]])
    derivative <- add_column(x,
                             crit  = rep(crit, nrow(x)),
                             lower = x[["derivative"]] - adj,
                             upper = x[["derivative"]] + adj)
    derivative
}

## fd is a list of predicted values returned by the various foo_finite_diffX
## functions below
`finite_difference` <- function(fd, order, type, eps) {
    if (isTRUE(order == 1L)) {
        xf <- fd[["xf"]]
        xb <- fd[["xb"]]
        X  <- (xf - xb) / eps
    } else {
        xf <- fd[["xf"]]
        xb <- fd[["xb"]]
        x  <- fd[["x"]]
        X  <- switch(type,
                     forward  = (xb - (2*xf) + x) / eps^2,
                     backward = (x - (2*xf) + xb) / eps^2,
                     central  = (xf - (2*x) + xb) / eps^2)
    }

    X
}

##' @importFrom tibble tibble
`compute_derivative` <- function(id, lpmatrix, betas, Vb, model, newdata) {
    sm <- get_smooths_by_id(model, id)[[1L]]
    sm_var <- smooth_variable(sm)
    ## handle fs smooths
    fs_var <- NULL
    if (is_fs_smooth(sm)) {
        fs_var <- sm_var[-1L]
        sm_var <- sm_var[1L]
    }
    sm_lab <- smooth_label(sm)
    want <- grep(sm_lab, colnames(lpmatrix), fixed = TRUE)
    Xi <- lpmatrix * 0                  # zero out the Xp matrix
    Xi[, want] <- lpmatrix[, want]      # copy bits of Xp we need
    d <- drop(Xi %*% betas)             # estimate derivative
    se <- rowSums(Xi %*% Vb * Xi)^0.5   # standard errors
    ## build return tibble
    deriv <- tibble(smooth = rep(sm_lab, length(d)),
                    var = rep(sm_var, length(d)),
                    data = newdata[[sm_var]],
                    derivative = d,
                    se = se)
    if (!is.null(fs_var)) {
        deriv <- add_column(deriv, fs_var = newdata[[fs_var]], .after = 2L)
    }
    result <- list(deriv = deriv, Xi = Xi)
    result
}

`finite_diff_lpmatrix` <- function(object, type, order, newdata = NULL, h = 1e-7) {
    result <- if (order == 1L) {
        switch(type,
               forward  = forward_finite_diff1(object, newdata, h),
               backward = backward_finite_diff1(object, newdata, h),
               central  = central_finite_diff1(object, newdata, h))
    } else {
        switch(type,
               forward  = forward_finite_diff2(object, newdata, h),
               backward = backward_finite_diff2(object, newdata, h),
               central  = central_finite_diff2(object, newdata, h))

    }

    result
}

`forward_finite_diff1` <- function(model, newdata, h = 1e-7) {
    ind <- is_factor_var(newdata)       # exclude factors
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create newdata2 as newdata + h
    newdata2 <- shift_values(newdata, h = h, i = ind, FUN = '+')

    ## predict for x
    x0 <- predict(model, newdata, type = "lpmatrix")

    ## predict for x + h
    x1 <- predict(model, newdata2, type = "lpmatrix")

    list(xf = x1, xb = x0)
}

`backward_finite_diff1` <- function(model, newdata, h = 1e-7) {
    ind <- is_factor_var(newdata)       # exclude factors
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create newdata2 as newdata - h
    newdata2 <- shift_values(newdata, h = h, i = ind, FUN = '-')

    ## predict for x
    x0 <- predict(model, newdata, type = "lpmatrix")

    ## predict for x - h
    x1 <- predict(model, newdata2, type = "lpmatrix")

    list(xf = x0, xb = x1)              # intentionally flipped order
}

`central_finite_diff1` <- function(model, newdata, h = 1e-7) {
    ind <- is_factor_var(newdata)       # exclude factors
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create newdata as newdata + 0.5h
    newdata1 <- shift_values(newdata, h = h/2, i = ind, FUN = '+')
    ## create newdata2 as newdata - 0.5h
    newdata2 <- shift_values(newdata, h = h/2, i = ind, FUN = '-')

    ## predict for x + 0.5h
    x0 <- predict(model, newdata1, type = "lpmatrix")

    ## predict for x - 0.5h
    x1 <- predict(model, newdata2, type = "lpmatrix")

    list(xf = x0, xb = x1)
}

`forward_finite_diff2` <- function(model, newdata, h = 1e-7) {
    ind <- is_factor_var(newdata)       # exclude factors
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create newdata as newdata + h
    newdata1 <- shift_values(newdata, h = h, i = ind, FUN = '+')
    ## create newdata2 as newdata + 2h
    newdata2 <- shift_values(newdata, h = 2*h, i = ind, FUN = '+')

    ## predict for x + h
    x0 <- predict(model, newdata1, type = "lpmatrix")

    ## predict for x + 2h
    x1 <- predict(model, newdata2, type = "lpmatrix")

    ## predict for x
    x2 <- predict(model, newdata, type = "lpmatrix")

    list(xf = x0, xb = x1, x = x2)
}

`backward_finite_diff2` <- function(model, newdata, h = 1e-7) {
    ind <- is_factor_var(newdata)       # exclude factors
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create newdata as newdata - h
    newdata1 <- shift_values(newdata, h = h, i = ind, FUN = '-')
    ## create newdata2 as newdata - 2h
    newdata2 <- shift_values(newdata, h = 2*h, i = ind, FUN = '-')

    ## predict for x - h
    x0 <- predict(model, newdata1, type = "lpmatrix")

    ## predict for x - 2h
    x1 <- predict(model, newdata2, type = "lpmatrix")

    ## predict for x
    x2 <- predict(model, newdata, type = "lpmatrix")

    list(xf = x0, xb = x1, x = x2)
}

`central_finite_diff2` <- function(model, newdata, h = 1e-7) {
    ind <- is_factor_var(newdata)       # exclude factors
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create newdata as newdata + h
    newdata1 <- shift_values(newdata, h = h, i = ind, FUN = '+')
    ## create newdata2 as newdata - h
    newdata2 <- shift_values(newdata, h = h, i = ind, FUN = '-')

    ## predict for x + h
    x0 <- predict(model, newdata1, type = "lpmatrix")

    ## predict for x - h
    x1 <- predict(model, newdata2, type = "lpmatrix")

    ## predict for x
    x2 <- predict(model, newdata, type = "lpmatrix")

    list(xf = x0, xb = x1, x = x2)
}

##' @importFrom dplyr bind_cols setdiff
##' @importFrom tibble as_tibble
##' @importFrom rlang exec !!!
##' @importFrom tidyr expand_grid
`derivative_data` <- function(model, id, n, offset = NULL,
                              order = NULL, type = NULL, eps = NULL) {
    mf <- model.frame(model)           # model.frame used to fit model

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
    mf <- fix_offset(model, mf, offset_val = offset)
    ff <- vapply(mf, is.factor, logical(1L)) # which, if any, are factors vars
    ## list of model terms (variable names); extract these from `var.summary`
    ## because model.frame() on a gamm() contains extraneous variables, related
    ## to the mixed model form for lme()
    m.terms <- names(model[["var.summary"]])

    ## need a list of terms used in current smooth
    sm <- get_smooths_by_id(model, id)[[1L]]
    smooth_vars <- unique(smooth_variable(sm))
    ## Handle special smooths, like 'fs', which involves a factor
    fs_var <- NULL
    if (is_fs_smooth(sm)) {
        ## second element of smooth_var will be a factor
        fs_var <- smooth_vars[ff]
        smooth_vars <- smooth_vars[!ff]
    }
    ## is smooth a factor by? If it is, extract the by variable
    by_var <- if (is_factor_by_smooth(sm)) {
        by_variable(sm)
    } else {
        NULL
    }
    used_vars <- c(smooth_vars, fs_var, by_var)

    ## generate covariate values for the smooth
    newlist <- lapply(mf[smooth_vars], seq_min_max_eps, n = n,
                      order = order, type = type, eps = eps)
    ## handle fs smooths
    if (!is.null(fs_var)) {
        fs_levs <- levels(mf[[fs_var]])
        new_fs <- setNames(list(factor(fs_levs, levels = fs_levs)), fs_var)
        newlist <- append(newlist, new_fs)
    }
    ## handle factor by --- FIXME: what about numeric by?
    if (!is.null(by_var)) {
        ## ordered or simple factor? Grab class as a function to apply below
        FUN <- match.fun(data.class(mf[[by_var]]))
        ## extract levels of factor by var,
        levs <- levels(mf[[by_var]])
        ## coerce level for this smooth to correct factor type with FUN
        ##   return as a list with the correct names
        newfac <- setNames(list(FUN(by_level(sm), levels = levs)), by_var)
        ## append this list to the list of new smooth covariate values
        newlist <- append(newlist, newfac)
    }
    newdata <- exec(expand_grid, !!!newlist) # actually compute expand.grid-alike

    ## need to provide single values for all other covariates in data
    unused_vars <- dplyr::setdiff(m.terms, used_vars)
    unused_summ <- model[["var.summary"]][unused_vars]
    ## FIXME: put this in utils.R with a better name!
    ## this basically just reps the data (scalar) for the closest observation
    ## to the median over all observations
    `rep_fun` <- function(x, n) {
        ## if `x` isn't a factor, select the second element of `x` which
        ## is the value of the observation in the data closest to median
        ## of set of observations in data used to fit the model.
        if (!is.factor(x)) {
            x <- x[2L]
        }
        ## repeat `x` as many times as is needed
        rep(x, times = n)
    }
    n_new <- NROW(newdata)
    unused_data <- as_tibble(lapply(unused_summ, FUN = rep_fun, n = n_new))
    ## add unnused_data to newdata so we're ready to predict
    newdata <- bind_cols(newdata, unused_data)

    newdata <- newdata[, m.terms, drop = FALSE] # re-arrange
    newdata
}

##' @importFrom dplyr bind_cols setdiff
##' @importFrom tibble as_tibble
##' @importFrom rlang exec !!!
##' @importFrom tidyr nesting
`smooth_data` <- function(model, id, n, offset = NULL) {
    mf <- model.frame(model)           # model.frame used to fit model

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
    mf <- fix_offset(model, mf, offset_val = offset)
    ff <- vapply(mf, is.factor, logical(1L)) # which, if any, are factors vars
    ## list of model terms (variable names); extract these from `var.summary`
    ## because model.frame() on a gamm() contains extraneous variables, related
    ## to the mixed model form for lme()
    m.terms <- names(model[["var.summary"]])

    ## need a list of terms used in current smooth
    sm <- get_smooths_by_id(model, id)[[1L]]
    smooth_vars <- unique(smooth_variable(sm))
    ## is smooth a factor by? If it is, extract the by variable
    by_var <- if (is_factor_by_smooth(sm)) {
        by_variable(sm)
    } else {
        NULL
    }
    used_vars <- c(smooth_vars, by_var)

    ## generate covariate values for the smooth
    newlist <- lapply(mf[smooth_vars], seq_min_max, n = n)
    if (!is.null(by_var)) {
        ## ordered or simple factor? Grab class as a function to apply below
        FUN <- match.fun(data.class(mf[[by_var]]))
        ## extract levels of factor by var,
        levs <- levels(mf[[by_var]])
        ## coerce level for this smooth to correct factor type with FUN
        ##   return as a list with the correct names
        newfac <- setNames(list(FUN(by_level(sm), levels = levs)), by_var)
        ## append this list to the list of new smooth covariate values
        newlist <- append(newlist, newfac)
    }
    newdata <- exec(nesting, !!!newlist) # actually compute expand.grid-alike

    ## need to provide single values for all other covariates in data
    unused_vars <- dplyr::setdiff(m.terms, used_vars)
    unused_summ <- model[["var.summary"]][unused_vars]
    ## FIXME: put this in utils.R with a better name!
    ## this basically just reps the data (scalar) for the closest observation
    ## to the median over all observations
    `rep_fun` <- function(x, n) {
        ## if `x` isn't a factor, select the second element of `x` which
        ## is the value of the observation in the data closest to median
        ## of set of observations in data used to fit the model.
        if (!is.factor(x)) {
            x <- x[2L]
        }
        ## repeat `x` as many times as is needed
        rep(x, times = n)
    }
    n_new <- NROW(newdata)
    ## unused_data <- as_tibble(lapply(unused_summ, FUN = rep_fun, n = n_new))
    ## add unnused_data to newdata so we're ready to predict
    ## newdata <- bind_cols(newdata, unused_data)

    ## newdata <- newdata[, m.terms, drop = FALSE] # re-arrange
    newdata
}
