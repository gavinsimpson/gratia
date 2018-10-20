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
##' @param unconditional logical; use smoothness selection-corrected Bayesian
##'   covariance matrix?
##' @param frequentist logical; use the frequentist covariance matrix?
##' @param offset numeric; a value to use for any offset term
##'
##' @export
##'
##' @rdname derivatives
`derivatives.gam` <- function(object, term, newdata, order = 1L,
                              type = c("forward", "backward", "central"),
                              n = 200, eps = 1e-7,
                              unconditional = FALSE, frequentist = FALSE,
                              offset = NULL, ...) {
    ## handle term
    smooth_ids <- if (!missing(term)) {
        which_smooths(object, term) # which smooths match 'term'
    } else {
        seq_len(n_smooths(object))
    }

    ## handle newdata
    if (missing(newdata)) {
        newdata <- prediction_data(object, n = n, offset = offset)
    }

    ## handle order
    if (!order %in% c(1L, 2L)) {
        stop("Only 1st or 2nd derivatives are supported: `order %in% c(1,2)`")
    }

    ## handle type
    type <- match.arg(type)

    fd <- finite_diff_lpmatrix(object, type = type, order = order,
                               newdata = newdata, h = eps)
    xf <- fd[["xf"]]
    xb <- fd[["xb"]]
    X  <- (xf - xb) / eps

    Vb <- get_vcov(object, unconditional = unconditional,
                   frequentist = frequentist)
    betas <- coef(object)

    ## how many smooths need working on
    ns <- length(smooth_ids)
    result <- vector(mode = "list", length = ns)

    for (i in smooth_ids) {
        result[[i]] <- finite_difference(i, lpmatrix = X, betas = betas, Vb = Vb,
                                         model = object, newdata = newdata)
    }
    result <- do.call("bind_rows", result)

    class(result) <- c("derivatives", class(result))
    result
}

##' @importFrom tibble data_frame
`finite_difference` <- function(id, lpmatrix, betas, Vb, model, newdata) {
    sm <- get_smooths_by_id(model, id)[[1L]]
    sm_var <- smooth_variable(sm)
    sm_lab <- smooth_label(sm)
    want <- grep(sm_lab, colnames(lpmatrix), fixed = TRUE)
    Xi <- lpmatrix * 0                  # zero out the Xp matrix
    Xi[, want] <- lpmatrix[, want]      # copy bits of Xp we need
    d <- drop(Xi %*% betas)             # estimate derivative
    se <- rowSums(Xi %*% Vb * Xi)^0.5   # standard errors
    data_frame(smooth = rep(sm_lab, length(d)),
               var = rep(sm_var, length(d)),
               data = newdata[[sm_var]],
               derivative = d, se = se)
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
    stop("2nd derivatives not yet implemented.")
}

`backward_finite_diff2` <- function(model, newdata, h = 1e-7) {
    stop("2nd derivatives not yet implemented.")
}

`central_finite_diff2` <- function(model, newdata, h = 1e-7) {
    stop("2nd derivatives not yet implemented.")
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
