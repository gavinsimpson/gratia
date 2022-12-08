#' @title Derivatives of estimated smooths via finite differences
#'
#' @param object an R object to compute derivatives for.
#' @param ... arguments passed to other methods.
#'
#' @author Gavin L. Simpson
#'
#' @export
`derivatives` <- function(object, ...) {
    UseMethod("derivatives")
}

#' @rdname derivatives
#' @export
`derivatives.default` <- function(object, ...) {
    ## want to bail with a useful error;
    ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
    stop("Don't know how to calculate derivatives for <",
         class(object)[[1L]], ">",
         call. = FALSE)           # don't show the call, simpler error
}

#' @rdname derivatives
#'
#' @export
`derivatives.gamm` <- function(object, ...) {
    derivatives(object[["gam"]], ...)
}

#' @param term character; vector of one or more smooth terms for which
#'   derivatives are required. If missing, derivatives for all smooth terms
#'   will be returned. Can be a partial match to a smooth term; see argument
#'   `partial_match` below.
#' @param data a data frame containing the values of the model covariates
#'   at which to evaluate the first derivatives of the smooths.
#' @param order numeric; the order of derivative.
#' @param type character; the type of finite difference used. One of
#'   `"forward"`, `"backward"`, or `"central"`.
#' @param n numeric; the number of points to evaluate the derivative at.
#' @param eps numeric; the finite difference.
#' @param interval character; the type of interval to compute. One of
#'   `"confidence"` for point-wise intervals, or `"simultaneous"` for
#'   simultaneous intervals.
#' @param n_sim integer; the number of simulations used in computing the
#'   simultaneous intervals.
#' @param level numeric; `0 < level < 1`; the confidence level of the
#'   point-wise or simultaneous interval. The default is `0.95` for a 95%
#'   interval.
#' @param unconditional logical; use smoothness selection-corrected Bayesian
#'   covariance matrix?
#' @param frequentist logical; use the frequentist covariance matrix?
#' @param offset numeric; a value to use for any offset term
#' @param ncores number of cores for generating random variables from a
#'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
#'   Parallelization will take place only if OpenMP is supported (but appears
#'   to work on Windows with current `R`).
#' @param partial_match logical; should smooths be selected by partial matches
#'   with `term`? If `TRUE`, `term` can only be a single string to match
#'   against.
#' @param newdata Deprecated: use `data` instead.
#'
#' @note `derivatives()` will ignore any random effect smooths it encounters in
#'   `object`.
#'
#' @export
#'
#' @importFrom dplyr filter relocate
#' @importFrom tidyselect last_col
#'
#' @rdname derivatives
#'
#' @return A tibble, currently with the following variables:
#' * `smooth`: the smooth each row refers to,
#' * `var`: the name of the variable involved in the smooth,
#' * `data`: values of `var` at which the derivative was evaluated,
#' * `derivative`: the estimated derivative,
#' * `se`: the standard error of the estimated derivative,
#' * `crit`: the critical value such that `derivative` ± `(crit * se)` gives
#'   the upper and lower bounds of the requested confidence or simultaneous
#'   interval (given `level`),
#' * `lower`: the lower bound of the confidence or simultaneous interval,
#' * `upper`: the upper bound of the confidence or simultaneous interval.
#'
#' @examples
#'
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 42)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' ## first derivatives of all smooths using central finite differences
#' derivatives(mod, type = "central")
#'
#' ## derivatives for a selected smooth
#' derivatives(mod, type = "central", term = "s(x1)")
#' ## or via a partial match
#' derivatives(mod, type = "central", term = "x1", partial_match = TRUE)
#' \dontshow{
#' options(op)
#' }
`derivatives.gam` <- function(object, term, data = newdata, order = 1L,
                              type = c("forward", "backward", "central"),
                              n = 200, eps = 1e-7,
                              interval = c("confidence", "simultaneous"),
                              n_sim = 10000, level = 0.95,
                              unconditional = FALSE, frequentist = FALSE,
                              offset = NULL, ncores = 1,
                              partial_match = FALSE, ..., newdata = NULL) {
    ## handle term
    smooth_ids <- if (!missing(term)) {
        ## which smooths match 'term'
        sms <- check_user_select_smooths(smooths(object), term,
                                         partial_match = partial_match)
        ## need to skip random effect smooths
        take <- vapply(object$smooth[sms], smooth_type , character(1)) %in%
          "Random effect"
        sms[take] <- FALSE
        which(sms)
    } else {
        s <- seq_len(n_smooths(object))
        ## need to skip random effect smooths
        take <- vapply(object$smooth, smooth_type,
                         character(1)) %in% "Random effect"
        s[!take]
    }

    ## handle type
    type <- match.arg(type)

    ## handle order
    if (!order %in% c(1L, 2L)) {
        stop("Only 1st or 2nd derivatives are supported: `order %in% c(1,2)`")
    }

    if (!is.null(newdata)) {
        newdata_deprecated()
    }

    ## handle data
    need_data <- FALSE
    if (is.null(data)) {
        need_data <- TRUE
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
        ## generate data if not supplied
        if (need_data) {
            newd <- derivative_data(object, id = smooth_ids[[i]], n = n,
                                    offset = offset, order = order,
                                    type = type, eps = eps)
        } else {
            ## assume the data are OK - mgcv::predict will catch issues
            newd <- data
            ## ...but we need to handle factor by
            sm <- get_smooths_by_id(object, id = smooth_ids[[i]])[[1]]
            if (is_factor_by_smooth(sm)) {
                newd <- filter(newd, data[[by_variable(sm)]] == by_level(sm))
            }
        }

        # generate list of finite difference predictions for the first or second
        #   derivatives or the required type
        fd <- finite_diff_lpmatrix(object, type = type, order = order,
                                   data = newd, h = eps)

        ## compute the finite differences
        X <- finite_difference(fd, order, type, eps)

        ## compute derivatives
        d <- compute_derivative(smooth_ids[[i]], lpmatrix = X, betas = betas,
                                Vb = Vb, model = object, data = newd)

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
    result <- bind_rows(result)

    ## reorder the columns
    result <- result %>%
      relocate(any_of(c("smooth", "var", "by_var", "fs_var")), .before = 1) %>%
      relocate(all_of(c("data","derivative", "se", "crit", "lower", "upper")),
               .after = last_col())

    class(result) <- c("derivatives", class(result)) # add class
    result                                           # return
}

#' @importFrom tibble add_column
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

#' @importFrom tibble add_column
#' @importFrom stats quantile
#' @importFrom mvnfast rmvn
`derivative_simultaneous_int` <- function(x, Xi, level, Vb, n_sim, ncores) {
    ## simulate un-biased deviations given bayesian covariance matrix
    buDiff <- rmvn(n = n_sim, mu = rep(0, nrow(Vb)), sigma = Vb,
        ncores = ncores)
    # simulate deviations from expected
    simDev <- tcrossprod(Xi, buDiff) # Xi %*% t(bu)
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

#' @importFrom rlang eval_tidy parse_expr
#' @importFrom tibble tibble
`compute_derivative` <- function(id, lpmatrix, betas, Vb, model, data,
    focal = NULL) {

    sm <- get_smooths_by_id(model, id)[[1L]]
    sm_var <- smooth_variable(sm)
    if (!is.null(focal)) {
        # fix the focal variable
        sm_var <- sm_var[sm_var == focal]
    }
    by_var <- by_variable(sm)
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
                    data = eval_tidy(parse_expr(sm_var), data = data),
                    derivative = d,
                    se = se)
    fs_var <- if (is.null(fs_var)) {
        rep(NA_character_, nrow(deriv))
    } else {
        data[[fs_var]]
    }
    deriv <- add_column(deriv, fs_var = fs_var, .after = 2L)

    by_var <- if (by_var == "NA"){
        rep(NA_character_, nrow(deriv))
    } else {
        deriv <- add_column(deriv, {{ by_var }} := data[[by_var]],
                            .after = 2L)
        rep(by_var, nrow(deriv))
    }
    deriv <- add_column(deriv, by_var = by_var, .after = 2L)
    result <- list(deriv = deriv, Xi = Xi)
    result
}

`finite_diff_lpmatrix` <- function(object, type, order, data = NULL, h = 1e-7,
    focal = NULL) {
    result <- if (order == 1L) {
        switch(type,
               forward  = forward_finite_diff1(object, data, h, focal = focal),
               backward = backward_finite_diff1(object, data, h, focal = focal),
               central  = central_finite_diff1(object, data, h, focal = focal))
    } else {
        switch(type,
               forward  = forward_finite_diff2(object, data, h, focal = focal),
               backward = backward_finite_diff2(object, data, h, focal = focal),
               central  = central_finite_diff2(object, data, h, focal = focal))

    }

    result
}

`forward_finite_diff1` <- function(model, data, h = 1e-7, focal = NULL) {
    ## need to exclude anything not numeric (from R's point of view)
    ## negate result as TRUE == numeric and we want opposite
    ind <- !is_numeric_var(data) # exclude non numerics
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create data2 as data + h - negate ind as TRUE == numeric
    data2 <- shift_values(data, h = h, i = ind, FUN = `+`, focal = focal)

    ## predict for x
    x0 <- predict(model, data, type = "lpmatrix")

    ## predict for x + h
    x1 <- predict(model, data2, type = "lpmatrix")

    list(xf = x1, xb = x0)
}

`backward_finite_diff1` <- function(model, data, h = 1e-7, focal = NULL) {
    ## need to exclude anything not numeric (from R's point of view)
    ## negate result as TRUE == numeric and we want opposite
    ind <- !is_numeric_var(data) # exclude non numerics
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create data2 as data - h
    data2 <- shift_values(data, h = h, i = ind, FUN = `-`, focal = focal)

    ## predict for x
    x0 <- predict(model, data, type = "lpmatrix")

    ## predict for x - h
    x1 <- predict(model, data2, type = "lpmatrix")

    list(xf = x0, xb = x1)              # intentionally flipped order
}

`central_finite_diff1` <- function(model, data, h = 1e-7, focal = NULL) {
    ## need to exclude anything not numeric (from R's point of view)
    ## negate result as TRUE == numeric and we want opposite
    ind <- !is_numeric_var(data) # exclude non numerics
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create data as data + 0.5h
    data1 <- shift_values(data, h = h/2, i = ind, FUN = `+`, focal = focal)
    ## create data2 as data - 0.5h
    data2 <- shift_values(data, h = h/2, i = ind, FUN = `-`, focal = focal)

    ## predict for x + 0.5h
    x0 <- predict(model, data1, type = "lpmatrix")

    ## predict for x - 0.5h
    x1 <- predict(model, data2, type = "lpmatrix")

    list(xf = x0, xb = x1)
}

`forward_finite_diff2` <- function(model, data, h = 1e-7, focal = NULL) {
    ## need to exclude anything not numeric (from R's point of view)
    ## negate result as TRUE == numeric and we want opposite
    ind <- !is_numeric_var(data) # exclude non numerics
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create data as data + h
    data1 <- shift_values(data, h = h, i = ind, FUN = `+`, focal = focal)
    ## create data2 as data + 2h
    data2 <- shift_values(data, h = 2*h, i = ind, FUN = `+`, focal = focal)

    ## predict for x + h
    x0 <- predict(model, data1, type = "lpmatrix")

    ## predict for x + 2h
    x1 <- predict(model, data2, type = "lpmatrix")

    ## predict for x
    x2 <- predict(model, data, type = "lpmatrix")

    list(xf = x0, xb = x1, x = x2)
}

`backward_finite_diff2` <- function(model, data, h = 1e-7, focal = NULL) {
    ## need to exclude anything not numeric (from R's point of view)
    ## negate result as TRUE == numeric and we want opposite
    ind <- !is_numeric_var(data) # exclude non numerics
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create data as data - h
    data1 <- shift_values(data, h = h, i = ind, FUN = `-`, focal = focal)
    ## create data2 as data - 2h
    data2 <- shift_values(data, h = 2*h, i = ind, FUN = `-`, focal = focal)

    ## predict for x - h
    x0 <- predict(model, data1, type = "lpmatrix")

    ## predict for x - 2h
    x1 <- predict(model, data2, type = "lpmatrix")

    ## predict for x
    x2 <- predict(model, data, type = "lpmatrix")

    list(xf = x0, xb = x1, x = x2)
}

`central_finite_diff2` <- function(model, data, h = 1e-7, focal = NULL) {
    ## need to exclude anything not numeric (from R's point of view)
    ## negate result as TRUE == numeric and we want opposite
    ind <- !is_numeric_var(data) # exclude non numerics
    if (all(ind)) {
        stop("Can't compute finite differences for all non-numeric data.")
    }

    ## create data as data + h
    data1 <- shift_values(data, h = h, i = ind, FUN = `+`, focal = focal)
    ## create data2 as data - h
    data2 <- shift_values(data, h = h, i = ind, FUN = `-`, focal = focal)

    ## predict for x + h
    x0 <- predict(model, data1, type = "lpmatrix")

    ## predict for x - h
    x1 <- predict(model, data2, type = "lpmatrix")

    ## predict for x
    x2 <- predict(model, data, type = "lpmatrix")

    list(xf = x0, xb = x1, x = x2)
}

#' @importFrom dplyr bind_cols setdiff
#' @importFrom tibble as_tibble
#' @importFrom rlang exec !!!
#' @importFrom tidyr expand_grid
#' @importFrom stringr str_detect
`derivative_data` <- function(model, id, n, offset = NULL,
                              order = NULL, type = NULL, eps = NULL,
                              focal = NULL) {
    mf <- model.frame(model)           # model.frame used to fit model

    ## remove response
    respvar <- attr(model$terms, "response")
    if (!identical(respvar, 0)) {
        mf <- mf[, -respvar, drop = FALSE]
    }

    # remove offset() var; model.frame returns both `offset(foo(var))` & `var`,
    # so we can just remove the former, but we also want to set the offset
    # variable `var` to something constant. FIXME - think this should be 0
    if (is.null(offset)) {
        offset <- 1L
    }
    mf <- fix_offset(model, mf, offset_val = offset)
    ff <- vapply(mf, is.factor, logical(1L)) # which, if any, are factors vars

    ## need a list of terms used in current smooth
    sm <- get_smooths_by_id(model, id)[[1L]]
    smooth_vars <- unique(smooth_variable(sm))

    ## list of model terms (variable names); extract these from `var.summary`
    ## because model.frame() on a gamm() contains extraneous variables, related
    ## to the mixed model form for lme()
    all_m_vars <- m_vars <- model_vars(model)
    want <- str_detect(smooth_vars, m_vars)
    m_vars <- m_vars[want]

    # now get rid of all but the focal variable *if set* in m_vars
    if (!is.null(focal)) {
        m_vars <- m_vars[str_detect(focal, m_vars)]
    }

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
    used_vars <- c(m_vars, by_var)

    ## generate covariate values for the smooth
    ## This handles terms of the form log(conc)
    newlist <- deriv_ref_data(m_vars, model = model, n = n, order = order,
                              type = type, eps = eps)
    ## handle fs smooths? --- handled by the above now automagically

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

    data <- expand_grid(!!!{newlist}) # actually compute expand.grid-alike

    ## need to provide single values for all other covariates in data
    unused_vars <- dplyr::setdiff(all_m_vars, used_vars)
    ## only processed unusaed_vars if length() > 0L
    if (length(unused_vars) > 0L) {
        unused_summ <- model[["var.summary"]][unused_vars]
        # FIXME: put this in utils.R with a better name!
        # this basically just reps the data (scalar) for the closest observation
        # to the median over all observations
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
        n_new <- NROW(data)
        unused_data <- as_tibble(lapply(unused_summ, FUN = rep_fun, n = n_new))
        ## add unnused_data to data so we're ready to predict
        data <- bind_cols(data, unused_data)
    }

    data <- data[, all_m_vars, drop = FALSE] # re-arrange
    data
}

`deriv_ref_data` <- function(vars, model, n, order, type, eps) {
    var_sum <- model[["var.summary"]][vars]
    l <- map(var_sum, expand_ref_data,
             n = n, order = order, type = type, eps = eps)
    l
}

`expand_ref_data` <- function(x, n, order, type, eps = 0) {
    if (is.factor(x) | is.character(x)) {
        out <- factor(levels(x), levels = levels(x))
    } else {
        out <- seq_min_max_eps(x[c(1,3)],
                               n = n, eps = eps, order = order, type = type)
    }
    out
}

#' @title Partial derivatives of estimated multivariate smooths via finite
#' differences
#'
#' @param object an R object to compute derivatives for.
#' @param ... arguments passed to other methods.
#'
#' @author Gavin L. Simpson
#'
#' @export
`partial_derivatives` <- function(object, ...) {
    UseMethod("partial_derivatives")
}

#' @rdname partial_derivatives
#' @export
`partial_derivatives.default` <- function(object, ...) {
    ## want to bail with a useful error;
    ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
    stop("Don't know how to calculate partial derivatives for <",
         class(object)[[1L]], ">",
         call. = FALSE)           # don't show the call, simpler error
}

#' @rdname partial_derivatives
#'
#' @export
`partial_derivatives.gamm` <- function(object, ...) {
    partial_derivatives(object[["gam"]], ...)
}

#' @param term character; vector of one or more smooth terms for which
#'   derivatives are required. If missing, derivatives for all smooth terms
#'   will be returned. Can be a partial match to a smooth term; see argument
#'   `partial_match` below.
#' @param focal character; name of the focal variable. The partial derivative
#'   of the estimated smooth with respect to this variable will be returned.
#'   All other variables involved in the smooth will be held at constant. This
#'   can be missing if supplying `data`, in which case, the focal variable will
#'   be identified as the one variable that is not constant.
#' @param data a data frame containing the values of the model covariates
#'   at which to evaluate the first derivatives of the smooths. If supplied,
#'   all but one variable must be held at a constant value.
#' @param order numeric; the order of derivative.
#' @param type character; the type of finite difference used. One of
#'   `"forward"`, `"backward"`, or `"central"`.
#' @param n numeric; the number of points to evaluate the derivative at.
#' @param eps numeric; the finite difference.
#' @param interval character; the type of interval to compute. One of
#'   `"confidence"` for point-wise intervals, or `"simultaneous"` for
#'   simultaneous intervals.
#' @param n_sim integer; the number of simulations used in computing the
#'   simultaneous intervals.
#' @param level numeric; `0 < level < 1`; the confidence level of the
#'   point-wise or simultaneous interval. The default is `0.95` for a 95%
#'   interval.
#' @param unconditional logical; use smoothness selection-corrected Bayesian
#'   covariance matrix?
#' @param frequentist logical; use the frequentist covariance matrix?
#' @param offset numeric; a value to use for any offset term
#' @param ncores number of cores for generating random variables from a
#'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
#'   Parallelization will take place only if OpenMP is supported (but appears
#'   to work on Windows with current `R`).
#' @param partial_match logical; should smooths be selected by partial matches
#'   with `term`? If `TRUE`, `term` can only be a single string to match
#'   against.
#' @param newdata Deprecated: use `data` instead.
#'
#' @note `partial_derivatives()` will ignore any random effect smooths it
#'   encounters in `object`.
#'
#' @export
#'
#' @importFrom dplyr filter relocate
#' @importFrom tidyselect last_col
#'
#' @rdname partial_derivatives
#'
#' @return A tibble, currently with the following variables:
#' * `smooth`: the smooth each row refers to,
#' * `var`: the name of the variable for which the partial derivative was
#'     evaluated,
#' * `data`: values of `var` at which the derivative was evaluated,
#' * `partial_deriv`: the estimated partial derivative,
#' * `se`: the standard error of the estimated partial derivative,
#' * `crit`: the critical value such that `derivative` ± `(crit * se)` gives
#'   the upper and lower bounds of the requested confidence or simultaneous
#'   interval (given `level`),
#' * `lower`: the lower bound of the confidence or simultaneous interval,
#' * `upper`: the upper bound of the confidence or simultaneous interval.
#'
#' @examples
#'
#' library("ggplot2")
#' library("patchwork")
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' df <- data_sim("eg2", n = 2000, dist = "normal", scale = 0.5, seed = 42)
#'
#' # fit the GAM (note: for execution time reasons, k is set articifially low)
#' m <- gam(y ~ te(x, z, k = c(5, 5)), data = df, method = "REML")
#'
#' # data slice through te(x,z) holding z == 0.4
#' ds <- data_slice(m, x = evenly(x, n = 100), z = 0.4)
#'
#' # evaluate te(x,z) at values of x & z
#' sm <- smooth_estimates(m, smooth = "te(x,z)", data = ds) |>
#'     add_confint()
#'
#' # partial derivatives
#' pd_x <- partial_derivatives(m, data = ds, type = "central", focal = "x")
#'
#' # draw te(x,z)
#' p1 <- draw(m, rug = FALSE) &
#'     geom_hline(yintercept = 0.4, size = 1)
#' p1
#'
#' # draw te(x,z) along slice
#' cap <- expression(z == 0.4)
#' p2 <- sm |>
#'     ggplot(aes(x = x, y = est)) +
#'     geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
#'     geom_line() +
#'     labs(x = "x", y = "Partial effect", title = "te(x,z)",
#'         caption = cap)
#' p2
#'
#' # draw partial derivs
#' p3 <- pd_x |>
#'     draw() +
#'     labs(caption = cap)
#' p3
#'
#' # draw all three panels
#' p1 + p2 + p3 + plot_layout(ncol = 3)
#' \dontshow{
#' options(op)
#' }
`partial_derivatives.gam` <- function(object, term, focal = NULL,
    data = newdata,
    order = 1L,
    type = c("forward", "backward", "central"),
    n = 200, eps = 1e-7,
    interval = c("confidence", "simultaneous"),
    n_sim = 10000, level = 0.95,
    unconditional = FALSE, frequentist = FALSE,
    offset = NULL, ncores = 1,
    partial_match = FALSE, ..., newdata = NULL) {

    ## handle term
    smooth_ids <- if (!missing(term)) {
        ## which smooths match 'term'
        sms <- check_user_select_smooths(smooths(object), term,
            partial_match = partial_match)
        ## need to skip random effect smooths
        take <- vapply(object$smooth[sms], smooth_type, character(1)) %in%
            "Random effect"
        # need to check that the selected smooths are multivariate
        mv_sm <- vapply(object$smooth[sms], smooth_dim, integer(1)) < 2L
        sms[take | mv_sm] <- FALSE
        which(sms)
    } else {
        s <- seq_len(n_smooths(object))
        ## need to skip random effect smooths
        take <- vapply(object$smooth, smooth_type,
            character(1)) %in% "Random effect"
        # need to check that the selected smooths are multivariate
        mv_sm <- vapply(object$smooth, smooth_dim, integer(1)) < 2L
        s[!(take | mv_sm)]
    }

    # handle focal - it should be a vector as long as the number of smooths
    # we are handling. If it is NULL, then we loop over the smooths, extract
    # the names of the variables and set the first to be the focal variable
    if (is.null(focal)) {
        focal <- vapply(object[["smooth"]],
            function(s) {
                smooth_variable(s)[1L]
            }, character(1L))
    } else {
        # if not NULL then we should check that it is of the same length as the
        # smooths we are evaluating
        n_focal <- length(focal)
        n_sm <- length(smooth_ids)
        if (isFALSE(identical(n_focal, n_sm))) {
            sm_names <- smooths(object)[smooth_ids]
            msg <- paste(sm_names, collapse = ", ")
            stop("'focal' should be the same length as the number of smooths",
                "for which partial derivatives are to be evaluated.",
                "\nThe relevant smooths are: ", msg,
            "\nThe supplied 'focal' should be length ", )
        }
    }

    ## handle type
    type <- match.arg(type)

    ## handle order
    if (!order %in% c(1L, 2L)) {
        stop("Only 1st or 2nd order partial derivatives are supported: ",
            "`order %in% c(1,2)`")
    }

    if (!is.null(newdata)) {
        newdata_deprecated()
    }

    ## handle data
    need_data <- is.null(data)

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
        ## generate data if not supplied
        if (need_data) {
            newd <- derivative_data(object, id = smooth_ids[[i]], n = n,
                offset = offset, order = order,
                type = type, eps = eps, focal = focal)
        } else {
            ## assume the data are OK - mgcv::predict will catch issues
            newd <- data
            ## ...but we need to handle factor by
            sm <- get_smooths_by_id(object, id = smooth_ids[[i]])[[1]]
            if (is_factor_by_smooth(sm)) {
                newd <- filter(newd, data[[by_variable(sm)]] == by_level(sm))
            }
            # and we need to identify which variable is the focal one
            n_unique <- vapply(newd, function(x) length(unique(x)),
                integer(1L))
            if (is.null(focal)) {
                focal <- names(newd)[which(n_unique > 1L)]
            } else {
                # and that the others are not varying
                bad <- n_unique[setdiff(names(newd), focal)] > 1L
                if (any(bad)) {
                    stop("For partial derivatives only 'focal' can be varying ",
                        "in 'data'. Problematic variables:",
                        paste(n_unique[bad], collapse = ", "))
                }
            }
        }

        # generate list of finite difference predictions for the first or second
        #   derivatives or the required type
        fd <- finite_diff_lpmatrix(object, type = type, order = order,
            data = newd, h = eps, focal = focal)

        ## compute the finite differences
        X <- finite_difference(fd, order, type, eps)

        ## compute derivatives
        d <- compute_derivative(smooth_ids[[i]], lpmatrix = X, betas = betas,
            Vb = Vb, model = object, data = newd, focal = focal)

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
    result <- bind_rows(result)

    ## reorder the columns
    result <- result %>%
        rename(partial_deriv = "derivative")%>%
        relocate(any_of(c("smooth", "var", "by_var", "fs_var")),
            .before = 1) %>%
        relocate(all_of(c("data", "partial_deriv", "se", "crit", "lower",
            "upper")), .after = last_col())

    class(result) <- c("partial_derivatives", "derivatives",
        class(result)) # add class
    result                                           # return
}
