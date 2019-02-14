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
##' @param ncores number of cores for generating random variables from a multivariate normal distribution. Passed to `mvnfast::rmvn`. Parallelization will take place only if OpenMP is supported (but appears to work on Windows with current `R`).
##'
##' @export
##'
##' @rdname derivatives
##'
##' @examples
##'
##' suppressPackageStartupMessages(library("mgcv"))
##' \dontshow{
##' set.seed(42)
##' op <- options(cli.unicode = FALSE)
##' }
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
##' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' ## first derivative of all smooths using central finite differenc
##' derivatives(mod, type = "central")
##' \dontshow{options(op)}
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
    if (missing(newdata)) {
        ## newdata <- prediction_data(object, n = n, offset = offset, eps = eps)
        newdata <- derivative_data(object, n = n, offset = offset,
                                   order = order, type = type, eps = eps)
    }

    ## handle interval
    interval <- match.arg(interval)

    ## generate list of finite difference predictions for the first or second
    ##   derivatives or the required type
    fd <- finite_diff_lpmatrix(object, type = type, order = order,
                               newdata = newdata, h = eps)

    ## compute the finite differences
    X <- finite_difference(fd, order, type, eps)

    ## get the required covariance matrix
    Vb <- get_vcov(object, unconditional = unconditional,
                   frequentist = frequentist)
    ## extract model coefs
    betas <- coef(object)

    ## how many smooths need working on
    ns <- length(smooth_ids)
    result <- Xis <- vector(mode = "list", length = ns)

    ## loop over the smooths and compute derivatives from finite differences
    for (i in seq_along(smooth_ids)) {
        d <- compute_derivative(smooth_ids[[i]], lpmatrix = X, betas = betas,
                                Vb = Vb, model = object, newdata = newdata)
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

    ## confidence/simultaneous intervals

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
`derivative_simultaneous_int` <- function(x, Xi, level, Vb, n_sim, ncores) {
    ## simulate un-biased deviations given bayesian covariance matrix
    buDiff <-  mvnfast::rmvn(n = nsim, mu = rep(0, nrow(V)), sigma = V, ncores = ncores)
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

##' @importFrom tibble data_frame
`compute_derivative` <- function(id, lpmatrix, betas, Vb, model, newdata) {
    sm <- get_smooths_by_id(model, id)[[1L]]
    sm_var <- smooth_variable(sm)
    sm_lab <- smooth_label(sm)
    want <- grep(sm_lab, colnames(lpmatrix), fixed = TRUE)
    Xi <- lpmatrix * 0                  # zero out the Xp matrix
    Xi[, want] <- lpmatrix[, want]      # copy bits of Xp we need
    d <- drop(Xi %*% betas)             # estimate derivative
    se <- rowSums(Xi %*% Vb * Xi)^0.5   # standard errors
    result <- list(deriv = data_frame(smooth = rep(sm_lab, length(d)),
                                      var = rep(sm_var, length(d)),
                                      data = newdata[[sm_var]],
                                      derivative = d,
                                      se = se),
                   Xi = Xi)
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

##' @importFrom dplyr bind_cols
##' @importFrom tibble as_tibble
`derivative_data` <- function(model, n, offset = NULL,
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
    m.terms <- names(mf)        # list of model terms (variable names)

    if (any(ff)) {
        ## need to supply something for each factor
        f.mf <- as_tibble(lapply(mf[, ff, drop = FALSE], rep_first_factor_value, n = n))
        mf <- mf[, !ff, drop = FALSE] # remove factors from model frame
    }

    ## generate newdata at `n` locations
    newdata <- as_tibble(vapply(mf, seq_min_max_eps, numeric(n), n = n,
                                order = order, type = type, eps = eps))

    ## if there were any factors, add back the factor columns
    if (any(ff)) {
        newdata <- bind_cols(newdata, f.mf)
    }
    names(newdata) <- c(m.terms[!ff], m.terms[ff])

    newdata <- newdata[, m.terms, drop = FALSE] # re-arrange
    newdata
}

##' @importFrom dplyr bind_cols
##' @importFrom tibble as_tibble
`prediction_data` <- function(model, n, offset = NULL) {
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
    m.terms <- names(mf)        # list of model terms (variable names)

    if (any(ff)) {
        ## need to supply something for each factor
        f.mf <- as_tibble(lapply(mf[, ff, drop = FALSE], rep_first_factor_value, n = n))
        mf <- mf[, !ff, drop = FALSE] # remove factors from model frame
    }

    ## generate newdata at `n` locations
    newdata <- as_tibble(vapply(mf, seq_min_max, numeric(n), n = n))

    ## if there were any factors, add back the factor columns
    if (any(ff)) {
        newdata <- bind_cols(newdata, f.mf)
    }
    names(newdata) <- c(m.terms[!ff], m.terms[ff])

    newdata <- newdata[, m.terms, drop = FALSE] # re-arrange
    newdata
}
