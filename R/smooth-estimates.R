#' Evaluate smooths at covariate values
#'
#' Evaluate a smooth at a grid of evenly spaced value over the range of the
#' covariate associated with the smooth. Alternatively, a set of points at which
#' the smooth should be evaluated can be supplied. `smooth_estimates()` is a new
#' implementation of [evaluate_smooth()], and should be used instead of that
#' other function.
#'
#' @param object an object of class `"gam"` or `"gamm"`.
#' @param smooth character; a single smooth to evaluate.
#' @param n numeric; the number of points over the range of the covariate at
#'   which to evaluate the smooth.
#' @param n_3d,n_4d numeric; the number of points over the range of last
#'   covariate in a 3D or 4D smooth. The default is `NULL` which achieves the
#'   standard behaviour of using `n` points over the range of all covariate,
#'   resulting in `n^d` evaluation pointsm, where `d` is the dimension of the
#'   smooth. For `d > 2` this can result in very many evaluation points and slow
#'   performance. For smooths of `d > 4`, the value of `n_4d` will be used for
#'   all dimensions `> 4`, unless this is `NULL`, in which case the default
#'   behaviour (using `n` for all dimensions) will be observed.
#' @param data a data frame of covariate values at which to evaluate the
#'   smooth.
#' @param unconditional logical; should confidence intervals include the
#'   uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian
#'   covariance matrix will be used.
#' @param overall_uncertainty logical; should the uncertainty in the model
#'  constant term be included in the standard error of the evaluate values of
#'  the smooth?
#' @param dist numeric; if greater than 0, this is used to determine when
#'   a location is too far from data to be plotted when plotting 2-D smooths.
#'   The data are scaled into the unit square before deciding what to exclude,
#'   and `dist` is a distance within the unit square. See
#'   [mgcv::exclude.too.far()] for further details.
#' @param unnest logical; unnest the smooth objects?
#' @param partial_match logical; in the case of character `select`, should
#'   `select` match partially against `smooths`? If `partial_match = TRUE`,
#'   `select` must only be a single string, a character vector of length 1.
#' @param ... arguments passed to other methods.
#'
#' @return A data frame (tibble), which is of class `"smooth_estimates"`.
#'
#' @export
#'
#' @rdname smooth_estimates
#'
#' @examples
#' load_mgcv()
#' \dontshow{op <- options(cli.unicode = FALSE, pillar.sigfig = 6)}
#' dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' ## evaluate all smooths
#' smooth_estimates(m1)
#'
#' ## or selected smooths
#' smooth_estimates(m1, smooth = c("s(x0)", "s(x1)"))
#' \dontshow{options(op)}
`smooth_estimates` <- function(object, ...) {
    UseMethod("smooth_estimates")
}

#' @export
#' @rdname smooth_estimates
#' @importFrom dplyr bind_rows all_of
#' @importFrom tidyr unnest
#' @importFrom rlang expr_label
`smooth_estimates.gam` <- function(object,
                                   smooth = NULL,
                                   n = 100,
                                   n_3d = 16,
                                   n_4d = 4,
                                   data = NULL,
                                   unconditional = FALSE,
                                   overall_uncertainty = TRUE,
                                   dist = NULL,
                                   unnest = TRUE,
                                   partial_match = FALSE,
                                   ...) {
    model_name <- expr_label(substitute(object))
    ## if particular smooths selected
    S <- smooths(object) # vector of smooth labels - "s(x)"

    # select smooths
    select <-
        check_user_select_smooths(smooths = S, select = smooth,
                                  partial_match = partial_match,
                                  model_name = model_name)
    smooth_ids <- which(select)

    ## extract the mgcv.smooth objects
    smooths <- get_smooths_by_id(object, smooth_ids)

    ## loop over the smooths and evaluate them
    sm_list <- vector(mode = "list", length = length(smooths))

    ## if user data supplied, check for and remove response
    if (!is.null(data)) {
        if (!is.data.frame(data)) {
            stop("'data', if supplied, must be a numeric vector or data frame.",
                 call. = FALSE)
        }
        check_all_vars(object, data = data, smooths = smooths)
        data <- delete_response(object, data = data)
    }

    # # fix up the n, n_3d, n_4d. If `n_3d` is `NULL` set `n_3d <- n`
    # if (is.null(n_3d)) {
    #     n_3d <- n
    # }
    # # likewise fix up n_4d; set it to `n` if `n_4d` is NULL
    # if (is.null(n_4d)) {
    #     n_4d <- n
    # }

    for (i in seq_along(sm_list)) {
        sm_list[[i]] <- eval_smooth(smooths[[i]],
                                    model = object,
                                    n = n,
                                    n_3d = n_3d,
                                    n_4d = n_4d,
                                    data = data,
                                    unconditional = unconditional,
                                    overall_uncertainty = overall_uncertainty,
                                    dist = dist)
    }

    # see if we have any tensor term orders to collect & apply
    tensor_term_order <- lapply(sm_list, attr, "tensor_term_order")
    ## create a single df of all the smooths
    sm_list <- bind_rows(sm_list)

    ## need to unnest the `data` column?
    if (isTRUE(unnest)) {
        sm_list <- unnest(sm_list, all_of('data'))
    }

    # add back any special attributes
    attr(sm_list, "tensor_term_order") <- do.call("c", tensor_term_order)

    ## add a class
    class(sm_list) <- c("smooth_estimates", class(sm_list))

    ## return
    sm_list
}

#' @export
`smooth_estimates.gamm` <- function(object, ...) {
    smooth_estimates(object[["gam"]], ...)
}

#' @export
`smooth_estimates.scam` <- function(object, ...) {
    # scam has too many smooth types to write methods for all of them
    # this just adds on some classes that allows gratia to dispatch special
    # methods for their peculiarities
    object$smooth <- lapply(object$smooth, reclass_scam_smooth)
    # now just call the "gam" method
    smooth_estimates.gam(object, ...)
}

# gamm4 method
#' @export
`smooth_estimates.list` <- function(object, ...) {
    if (!is_gamm4(object)) {
        stop("'smooth_estimates()' not available for a generic list")
    }
    smooth_estimates(object[["gam"]], ...)
}

#' Check user-supplied data for suitability
#'
#' @param data a data frame of variables to be checked.
#' @param vars character; vector of terms.
#'
#' @importFrom tibble tibble
#' @importFrom rlang := !!
#'
#' @keywords internal
#' @noRd
`check_user_data` <- function(data, vars) {
    if (is.data.frame(data)) {
        smooth_vars <- vars %in% names(data)
        if (!all(smooth_vars)) {
            stop(paste("Variable(s)",
                       paste(paste0("'", vars[!smooth_vars], "'"),
                             collapse = ", "),
                       "not found in 'data'."),
                 call. = FALSE)
        }
    } else if (is.numeric(data)) {   # vector; coerce to data frame
        if (length(vars) > 1L) {
            stop("'smooth' requires multiple data vectors but only 1 provided.",
                 call. = FALSE)
        }
        data <- tibble(!!(vars) := data)
    } else {                            # object we can't handle; bail out
        stop("'data', if supplied, must be a numeric vector or a data frame.",
             call. = FALSE)
    }
    data
}

#' @keywords internal
#' @noRd
`check_all_vars` <- function(model, data, smooths = NULL) {
    ## if we don't pass something to smooths get names of all variables used in
    ## the model
    vars <- if (is.null(smooths)) {
        term_names(model)
    } else {
        ## something passed to smooths; bail if not a list or numeric id vector
        if (!(is.list(smooths) || is.numeric(smooths))) {
            stop("Do not know how to handle supplied `smooths`.\n",
                 "Must be a vector of smooth indices or a list of objects ",
                 "that inherit from class `mgcv.smooth`.",
                 call. = FALSE)
        }
        ## if a numeric vector of smoth indices, then extract those smooths
        ## and continue
        if (is.numeric(smooths)) {
            smooths <- get_smooths_by_id(model, smooths)
        }
        ## do all elements of smooths now inherit from mgcv_smooth?
        sms <- vapply(smooths, FUN = inherits, logical(1L), "mgcv.smooth")
        ## if they don't bail out with a helpful error
        if (!all(sms)) {
            stop("Elements ", paste(which(!sms), collapse = ", "),
                 " of `smooths` do not inherit from class `mgcv.smooth`.",
                 call. = FALSE)
        }
        ## if they do all inherit from the correct class, then run term_names
        ## on each element and combine - returns $term and $by from each smooth
        unlist(sapply(smooths, FUN = term_names))
    }

    ## check that the vars we need are in data
    smooth_vars <- vars %in% names(data)
    if (!all(smooth_vars)) {
        stop(paste("Variable(s)",
                   paste(paste0("'", vars[!smooth_vars], "'"),
                         collapse = ", "),
                   "not found in 'data'."),
             call. = FALSE)
    }

    ## if we get here then everything must be OK so return the required variable
    ## names invisibly in case it is useful
    invisible(vars)
}

#' Evaluate estimated spline values
#'
#' @param smooth currently an object that inherits from class `mgcv.smooth`.
#' @param model a fitted model; currently only [mgcv::gam()] and [mgcv::bam()]
#'   models are suported.
#' @param data an optional data frame of values to evaluate `smooth` at.
#'
#' @inheritParams eval_smooth
#'
#' @importFrom tibble tibble add_column
#' @importFrom rlang := !!
#' @importFrom dplyr pull
#' @importFrom tidyselect all_of
#' @importFrom tidyr nest unnest
#' @importFrom mgcv PredictMat
#'
#' @keywords internal
#' @noRd
`spline_values2` <- function(smooth, data, model, unconditional,
                             overall_uncertainty = TRUE,
                             frequentist = FALSE) {
    X <- PredictMat(smooth, data)   # prediction matrix
    start <- smooth[["first.para"]]
    end <- smooth[["last.para"]]
    para.seq <- start:end
    coefs <- coef(model)[para.seq]

    fit <- drop(X %*% coefs)

    label <- smooth_label(smooth)

    ## want full vcov for component-wise CI
    V <- get_vcov(model, unconditional = unconditional)

    ## variables for component-wise CIs for smooths
    column_means <- model[["cmX"]]
    lcms <- length(column_means)
    nc <- ncol(V)
    meanL1 <- smooth[["meanL1"]]
    eta_idx <- lss_eta_index(model)

    if (isTRUE(overall_uncertainty) && attr(smooth, "nCons") > 0L) {
        if (lcms < nc) {
            column_means <- c(column_means, rep(0, nc - lcms))
        }
        Xcm <- matrix(column_means, nrow = nrow(X), ncol = nc, byrow = TRUE)
        if (!is.null(meanL1)) {
            Xcm <- Xcm / meanL1
        }
        Xcm[, para.seq] <- X
        # only apply the uncertainty from linear predictors of which this smooth
        # is a part of
        idx <- vapply(eta_idx, function(i, beta) any(beta %in% i),
            FUN.VALUE = logical(1L), beta = para.seq)
        idx <- unlist(eta_idx[idx])
        rs <- rowSums((Xcm[, idx, drop = FALSE] %*%
            V[idx, idx, drop = FALSE]) * Xcm[, idx, drop = FALSE])
    } else {
        rs <- rowSums((X %*% V[para.seq, para.seq, drop = FALSE]) * X)
    }

    ## standard error of the estimate
    se.fit <- sqrt(pmax(0, rs))

    ## identify which vars are needed for this smooth...
    keep_vars <- terms_in_smooth(smooth)
    ## ... then keep only those vars
    data <- select(data, all_of(keep_vars))

    ## Return object
    tbl <- tibble(.smooth = rep(label, nrow(X)), .estimate = fit, .se = se.fit)
    ## bind on the data
    tbl <- bind_cols(tbl, data)
    ## nest all columns with varying data
    tbl <- nest(tbl, data = all_of(c(".estimate", ".se", names(data))))

    tbl
}

`smooth_values` <- function(smooth, ...) {
    UseMethod("smooth_values")
}

#' @export
`smooth_values.univariate_scam_smooth` <- function(smooth, data, model, V,
    ...) {
    
    # get values of smooth 
    X <- PredictMat(smooth, data)   # prediction matrix
    off <- attr(X, "offset") # offset, if any
    if (is.null(off)) {
        off <- 0
    }
    start <- smooth[["first.para"]]
    end <- smooth[["last.para"]]
    para_seq <- smooth_coef_indices(smooth) # start:end
    coefs <- coef(model, parametrized = FALSE)[para_seq]

    # scam smooths work quite differently to mgcv smooths as X can contain
    # constant terms, need exponentiating etc
    which_exp <- which_exp_scam_coefs(model)[para_seq] # which betas need exp
    idx <- seq_along(coefs)[which_exp]
    # which exp function are we using?
    exp_fn <- exp_fun(model)
    # exponentiate any coefs that need it
    coefs[idx] <- exp_fn(coefs[idx])
    # coefs need reprarameterizing in some smooth type or padding with a 0
    stats <- scam_beta_se(smooth, beta = coefs, X = X, ndata = nrow(data),
        V = V)
    coefs <- stats$betas
    se_fit <- stats$se
    fit <- drop(X %*% coefs) + off
    list(fit = fit, se = se_fit)
}

`spline_values_scam` <- function(smooth, data, model,
    overall_uncertainty = TRUE, frequentist = FALSE) {
    # reclass the smooth to add classes needed for gratia's S3 methods to work
    smooth <- reclass_scam_smooth(smooth)

    ## want full vcov for component-wise CI
    V <- vcov(model, freq = frequentist, parametrized = TRUE)

    # get values of smooth & std errs, modified as needed for scam smooths
    sv <- smooth_values(smooth = smooth, data = data, model = model, V = V)

    fit <- sv$fit # fitted value at data
    se_fit <- sv$se # sqrt(pmax(0, sv$se)) # std err of fitted value

    label <- smooth_label(smooth)

    ## identify which vars are needed for this smooth...
    keep_vars <- terms_in_smooth(smooth)
    ## ... then keep only those vars
    data <- select(data, all_of(keep_vars))

    ## Return object
    tbl <- tibble(.smooth = rep(label, nrow(data)), .estimate = fit,
        .se = se_fit)
    ## bind on the data
    tbl <- bind_cols(tbl, data)
    ## nest all columns with varying data
    tbl <- nest(tbl, data = all_of(c(".estimate", ".se", names(data))))

    tbl
}

#' S3 methods to evaluate individual smooths
#'
#' @param smooth currently an object that inherits from class `mgcv.smooth`.
#' @param model a fitted model; currently only [mgcv::gam()] and [mgcv::bam()]
#'   models are suported.
#' @param data an optional data frame of values to evaluate `smooth` at.
#' @param ... arguments assed to other methods
#'
#' @inheritParams smooth_estimates
#'
#' @export
`eval_smooth` <- function(smooth, ...) {
    UseMethod("eval_smooth")
}

#' @rdname eval_smooth
#' @importFrom tibble add_column
#' @export
`eval_smooth.mgcv.smooth` <- function(smooth, model,
                                      n = 100,
                                      n_3d = NULL,
                                      n_4d = NULL,
                                      data = NULL,
                                      unconditional = FALSE,
                                      overall_uncertainty = TRUE,
                                      dist = NULL,
                                      ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

    ## deal with data if supplied
    data <- process_user_data_for_eval(data = data, model = model,
        n = n, n_3d = n_3d, n_4d = n_4d,
        id = which_smooth(model,
            smooth_label(smooth)))

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
        unconditional = unconditional,
        model = model,
        overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
    ## add on spline type info
    eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

    # set some values to NA if too far from the data
    if (smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
        eval_sm <- too_far_to_na(smooth,
            input = eval_sm,
            reference = model[["model"]],
            cols = c(".estimate", ".se"),
            dist = dist)
    }
    ## return
    eval_sm
}

#' @rdname eval_smooth
#' @importFrom tibble add_column
#' @export
`eval_smooth.scam_smooth` <- function(smooth, model,
                                      n = 100,
                                      n_3d = NULL,
                                      n_4d = NULL,
                                      data = NULL,
                                      unconditional = FALSE,
                                      overall_uncertainty = TRUE,
                                      dist = NULL,
                                      ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

    ## deal with data if supplied
    data <- process_user_data_for_eval(data = data, model = model,
        n = n, n_3d = n_3d, n_4d = n_4d,
        id = which_smooth(model, smooth_label(smooth)))

    ## values of spline at data
    eval_sm <- spline_values_scam(smooth, data = data, model = model,
        overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
    ## add on spline type info
    eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

    # set some values to NA if too far from the data
    if (smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
        eval_sm <- too_far_to_na(smooth,
            input = eval_sm,
            reference = model[["model"]],
            cols = c(".estimate", ".se"),
            dist = dist)
    }
    ## return
    eval_sm
}

#' Wrapper to `gratia::smooth_data()` and `gratia:::check_user_data()` for use
#' with [gratia::eval_smooth()] methods
#'
#' @param data an optional data frame of values for the smooth
#' @param model a fitted model
#' @param n numeric; the number of new observations to generate. Passed to
#'   [gratia::smooth_data()].
#' @param n_3d numeric; the number of new observations to generate for the third
#'   dimension of a 3D smooth. Passed to [gratia::smooth_data()].
#' @param n_4d numeric; the number of new observations to generate for the
#'   fourth (or higher) dimension(s) of a *k*D smooth (*k* >= 4). Passed to
#'   [gratia::smooth_data()].
#' @param id the number ID of the smooth within `model` to process.
#'
#' @keywords internal
#' @noRd
#' @importFrom rlang .data
`process_user_data_for_eval` <- function(data, model, n, n_3d, n_4d, id,
    var_order = NULL) {
    if (is.null(data)) {
        data <- smooth_data(model = model,
            n = n,
            n_3d = n_3d,
            n_4d = n_4d,
            id = id,
            var_order = var_order)
   } else {
        smooth <- get_smooths_by_id(model, id)[[1L]]
        vars <- smooth_variable(smooth)
        by_var <- by_variable(smooth)
        if (!identical(by_var, "NA")) {
            vars <- append(vars, by_var)
        }
        ## if this is a by variable, filter the by variable for the required
        ## level now
        if (is_factor_by_smooth(smooth)) {
            data <- data %>% filter(.data[[by_var]] == by_level(smooth))
        }
    }
    data
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.fs.interaction` <- function(smooth, model, n = 100, data = NULL,
                                         unconditional = FALSE,
                                         overall_uncertainty = TRUE,
                                         ...) {

    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

    ## deal with data if supplied
    id <- which_smooth(model, smooth_label(smooth))
    data <- process_user_data_for_eval(data = data, model = model,
                                       n = n, n_3d = NULL, n_4d = NULL,
                                       id = id)

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
        unconditional = unconditional,
        model = model,
        overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
    ## add on spline type info
    eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

    ## return
    eval_sm
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.sz.interaction` <- function(smooth, model, n = 100, data = NULL,
                                         unconditional = FALSE,
                                         overall_uncertainty = TRUE,
                                         ...) {

    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

    ## deal with data if supplied
    id <- which_smooth(model, smooth_label(smooth))
    data <- process_user_data_for_eval(data = data, model = model,
        n = n, n_3d = NULL, n_4d = NULL,
        id = id)

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
        unconditional = unconditional,
        model = model,
        overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
    ## add on spline type info
    eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))
    ## return
    eval_sm
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.random.effect` <- function(smooth, model, n = 100, data = NULL,
                                        unconditional = FALSE,
                                        overall_uncertainty = TRUE,
                                        ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

    ## deal with data if supplied
    id <- which_smooth(model, smooth_label(smooth))
    data <- process_user_data_for_eval(data = data, model = model,
                                       n = n, n_3d = NULL, n_4d = NULL,
                                       id = id)

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
                              unconditional = unconditional,
                              model = model,
                              overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
    ## add on spline type info
    eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))
    ## return
    eval_sm
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.mrf.smooth` <- function(smooth, model, n = 100, data = NULL,
                                     unconditional = FALSE,
                                     overall_uncertainty = TRUE,
                                     ...) {
    .NotYetImplemented()
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.t2.smooth` <- function(smooth, model,
                                    n = 100,
                                    n_3d = NULL,
                                    n_4d = NULL,
                                    data = NULL,
                                    unconditional = FALSE,
                                    overall_uncertainty = TRUE,
                                    dist = NULL,
                                    ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

    # order of variables
    var_order <- reorder_tensor_smooth_terms(smooth)

    ## deal with data if supplied
    id <- which_smooth(model, smooth_label(smooth))
    data <- process_user_data_for_eval(data = data, model = model,
        n = n, n_3d = n_3d, n_4d = n_4d,
        id = id, var_order = var_order)

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
        unconditional = unconditional,
        model = model,
        overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
    ## add on spline type info
    eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

    # set some values to NA if too far from the data
    if (smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
        eval_sm <- too_far_to_na(smooth,
            input = eval_sm,
            reference = model[["model"]],
            cols = c(".estimate", ".se"),
            dist = dist)
    }

    tensor_term_order <- list(var_order) |>
        setNames(smooth_label(smooth))
    attr(eval_sm, "tensor_term_order") <- tensor_term_order

    ## return
    class(eval_sm) <- append(class(eval_sm), c("tensor_eval_sm", "eval_sm"),
        after = 0L)
    eval_sm
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.tensor.smooth` <- function(smooth, model,
                                        n = 100,
                                        n_3d = NULL,
                                        n_4d = NULL,
                                        data = NULL,
                                        unconditional = FALSE,
                                        overall_uncertainty = TRUE,
                                        dist = NULL,
                                        ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

    # order of variables
    var_order <- reorder_tensor_smooth_terms(smooth)

    # deal with data if supplied
    id <- which_smooth(model, smooth_label(smooth))
    data <- process_user_data_for_eval(data = data, model = model,
        n = n, n_3d = n_3d, n_4d = n_4d,
        id = id, var_order = var_order)

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
        unconditional = unconditional,
        model = model,
        overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
    ## add on spline type info
    eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

    # set some values to NA if too far from the data
    if (smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
        eval_sm <- too_far_to_na(smooth,
            input = eval_sm,
            reference = model[["model"]],
            cols = c(".estimate", ".se"),
            dist = dist)
    }

    tensor_term_order <- list(var_order) |>
        setNames(smooth_label(smooth))
    attr(eval_sm, "tensor_term_order") <- tensor_term_order

    ## return
    class(eval_sm) <- append(class(eval_sm), c("tensor_eval_sm", "eval_sm"),
        after = 0L)
    eval_sm
}

#' Plot the result of a call to `smooth_estimates()`
#'
#' @param decrease_col,increase_col colour specifications to use for
#'   indicating periods of change. `col_change` is used when
#'   `change_type = "change"`, while `col_decrease` and `col_increase` are used
#'   when `change_type = "sizer"``.
#' @param change_lwd numeric; the value to set the `linewidth` to in
#'   [ggplot2::geom_line()], used to represent the perdios of change.
#' @param ylim numeric; vector of y axis limits to use all *all* panels drawn.
#'
#' @inheritParams draw.gam
#'
#' @export
#' @importFrom patchwork wrap_plots
#'
#' @examples
#' load_mgcv()
#' # example data
#' df <- data_sim("eg1", seed = 21)
#' # fit GAM
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#' # plot all of the estimated smooths
#' sm <- smooth_estimates(m)
#' draw(sm)
#' # evaluate smooth of `x2`
#' sm <- smooth_estimates(m, smooth = "s(x2)")
#' # plot it
#' draw(sm)
#'
#' # customising some plot elements
#' draw(sm, ci_col = "steelblue", smooth_col = "forestgreen", ci_alpha = 0.3)
#'
#' # Add a constant to the plotted smooth
#' draw(sm, constant = coef(m)[1])
#'
#' # Adding change indicators to smooths based on derivatives of the smooth
#' d <- derivatives(m, n = 100) # n to match smooth_estimates()
#'
#' smooth_estimates(m) |>
#'     add_sizer(derivatives = d, type = "sizer") |>
#'     draw()
`draw.smooth_estimates` <- function(object,
                                    constant = NULL,
                                    fun = NULL,
                                    contour = TRUE,
                                    grouped_by = FALSE,
                                    contour_col = "black",
                                    n_contour = NULL,
                                    ci_alpha = 0.2,
                                    ci_col = "black",
                                    smooth_col = "black",
                                    resid_col = "steelblue3",
                                    decrease_col = "#56B4E9",
                                    increase_col = "#E69F00",
                                    change_lwd = 1.75,
                                    partial_match = FALSE,
                                    discrete_colour = NULL,
                                    discrete_fill = NULL,
                                    continuous_colour = NULL,
                                    continuous_fill = NULL,
                                    angle = NULL,
                                    ylim = NULL,
                                    projection = "orthographic",
                                    orientation = NULL,
                                    ...) {
    # add confidence intervals if they don't already exist
    if (!all(c(".lower_ci", ".upper_ci") %in% names(object))) {
        object <- object |> add_confint()
    }

    # grab tensor term order if present, if not present it is NULL & that's OK
    tensor_term_order <- attr(object, "tensor_term_order")

    # draw smooths
    # the factor in group_split is to reorder to way the smooths entered the
    # model
    sm_levs <- unique(object$.smooth)

    sm_l <- if (isTRUE(grouped_by)) {
        # need the order of the smooths, I think
        levs <- unique(str_split_fixed(object$.smooth, ":", n = 2)[, 1])
        # nest the object so we can reuse the code/ideas from draw.gam
        object |>
            nest(data = !all_of(c(".smooth", ".type", ".by"))) |>
            mutate(.smooth = factor(.data$.smooth, levels = sm_levs),
                .term = str_split_fixed(.data$.smooth, ":", n = 2)[, 1]) |>
            arrange(.data$.smooth) |>
            relocate(".term", .before = 1L)|>
            unnest(all_of("data")) |>
            group_split(factor(.data$.term, levels = levs), .data$.by)
    } else {
        # the factor is to reorder to way the smooths entered the model
        group_split(object, factor(object$.smooth, levels = sm_levs))
    }
    ## sm_l <- group_split(object, factor(object$smooth, levels = sm_levs))
    plts <- map(sm_l,
        draw_smooth_estimates,
        constant = constant,
        fun = fun,
        contour = contour,
        contour_col = contour_col,
        n_contour = n_contour,
        ci_alpha = ci_alpha,
        ci_col = ci_col,
        smooth_col = smooth_col,
        increase_col = increase_col,
        decrease_col = decrease_col,
        change_lwd = change_lwd,
        partial_match = partial_match,
        discrete_colour = discrete_colour,
        discrete_fill = discrete_fill,
        continuous_colour = continuous_colour,
        continuous_fill = continuous_fill,
        angle = angle,
        ylim = ylim,
        projection = projection,
        orientation = orientation,
        tensor_term_order = tensor_term_order, # pass on tensor order info
        ...)

    wrap_plots(plts)
}

#' @importFrom tidyr unnest
#' @importFrom tidyselect any_of
`draw_smooth_estimates` <- function(object,
                                    constant = NULL,
                                    fun = NULL,
                                    contour = TRUE,
                                    contour_col = "black",
                                    n_contour = NULL,
                                    ci_alpha = 0.2,
                                    ci_col = "black",
                                    smooth_col = "black",
                                    resid_col = "steelblue3",
                                    decrease_col = "#56B4E9",
                                    increase_col = "#E69F00",
                                    change_lwd = 1.75,
                                    partial_match = FALSE,
                                    discrete_colour = NULL,
                                    discrete_fill = NULL,
                                    continuous_colour = NULL,
                                    continuous_fill = NULL,
                                    angle = NULL,
                                    ylim = NULL,
                                    projection = "orthographic",
                                    orientation = NULL,
                                    tensor_term_order = NULL,
                                    ...) {
    sm_vars <- tensor_term_order[[unique(object$.smooth)]]
    if (is.null(sm_vars)) {
        sm_vars <- if (".term" %in% names(object)) {
            vars_from_label(unique(object[[".term"]]))
        } else {
            vars_from_label(unique(object[[".smooth"]]))
        }
    }
    sm_dim <- length(sm_vars)
    sm_type <- unique(object[[".type"]])

    # set some values to NULL in case these components don't exist
    rug_data <- NULL
    p_residuals <- NULL
    ## unnest object if it has a list column 'data'
    if ((! is.null(object[["data"]]) && is.list(object$data))) {
        obj_nms <- names(object)
        ## preserve partial residuals and rug data if present
        if ("rug_data" %in% obj_nms) {
            rug_data <- object[["rug_data"]][[1L]]
        }
        if ("partial_residual" %in% obj_nms) {
            p_residuals <- object[["partial_residual"]][[1L]]
        }
        ## remove partial residuals and rug data from object
        object <- select(object, ! any_of(c("partial_residual", "rug_data")))
        ## finally unnest
        object <- unnest(object, cols = "data")
    }

    if (sm_dim == 1L &&
        sm_type %in% c("TPRS", "TPRS (shrink)", "CRS", "CRS (shrink)",
                       "Cyclic CRS", "P spline", "B spline", "Duchon spline",
                       "GP",
                       "Mono inc P spline",
                       "Mono dec P spline",
                       "Convex P spline",
                       "Concave P spline",
                       "Mono dec conv P spline",
                       "Mono dec conc P spline",
                       "Mono inc conv P spline",
                       "Mono inc conc P spline",
                       "Mono inc 0 start P spline",
                       "Mono inc 0 start P spline")) {
        class(object) <- c("mgcv_smooth", class(object))
    } else if (sm_type == "Random effect") {
        class(object) <- append(class(object),
                                c("random_effect", "mgcv_smooth"),
                                after = 0)
    } else if (sm_type == "Factor smooth") {
        class(object) <- append(class(object),
                                c("factor_smooth", "mgcv_smooth"),
                                after = 0)
    } else if (sm_type == "Constr. factor smooth") {
        class(object) <- append(class(object),
                                c("sz_factor_smooth", "mgcv_smooth"),
                                after = 0)
    } else if (sm_type == "SOS") {
        class(object) <- append(class(object),
                                c("sos", "mgcv_smooth"),
                                after = 0)
    } else if (sm_dim == 2L) {
        # all 2D smooths get these classes
        class(object) <- append(class(object),
                                c("bivariate_smooth", "mgcv_smooth"),
                                after = 0)
        # but TPRS smooths are isotropic so need special plotting
        # see issue #81. Duchon splines are a more general TPRS so
        # need to be handled the same way
        if(sm_type %in% c("TPRS (2d)", "TPRS (shrink) (2d)",
                          "Duchon spline (2d)")) {
            class(object) <- append(class(object), "isotropic_smooth",
                                    after = 0)
        }
    } else if (sm_dim == 3L) {
        # all 3D smooths get these classes
        class(object) <- append(class(object),
                                c("trivariate_smooth", "mgcv_smooth"),
                                after = 0)
        # but TPRS smooths are isotropic so need special plotting
        # see issue #81. Duchon splines are a more general TPRS so
        # need to be handled the same way, but we don't need to handle
        # this as a special method, so add after the trivariate_smooth
        # class
        if (sm_type %in% c("TPRS (3d)", "TPRS (shrink) (3d)",
            "Duchon spline (3d)")) {
            class(object) <- append(class(object), "isotropic_smooth",
                after = 1L)
        }
    } else if (sm_dim == 4L) {
        # all 2D smooths get these classes
        class(object) <- append(class(object),
                                c("quadvariate_smooth", "mgcv_smooth"),
                                after = 0)
        # but TPRS smooths are isotropic so need special plotting
        # see issue #81. Duchon splines are a more general TPRS so
        # need to be handled the same way, but we don't need to handle
        # this as a special method, so add after the trivariate_smooth
        # class
        if(sm_type %in% c("TPRS (4d)", "TPRS (shrink) (4d)",
                          "Duchon spline (4d)")) {
            class(object) <- append(class(object), "isotropic_smooth",
                                    after = 1L)
        }
    } else {
        return(NULL)
    }

    plot_smooth(object,
                variables = sm_vars,
                rug = rug_data,
                partial_residuals = p_residuals,
                constant = constant,
                fun = fun,
                contour = contour,
                contour_col = contour_col,
                n_contour = n_contour,
                ci_alpha = ci_alpha,
                ci_col = ci_col,
                smooth_col = smooth_col,
                resid_col = resid_col,
                increase_col = increase_col,
                decrease_col = decrease_col,
                change_lwd = change_lwd,
                partial_match = partial_match,
                discrete_colour = discrete_colour,
                discrete_fill = discrete_fill,
                continuous_colour = continuous_colour,
                continuous_fill = continuous_fill,
                angle = angle,
                ylim = ylim,
                projection = projection,
                orientation = orientation,
                ...)
}

`plot_smooth` <- function(object, ...) {
    UseMethod("plot_smooth")
}

#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_point geom_rug geom_abline
#'   expand_limits labs geom_line geom_ribbon aes guides guide_axis
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
`plot_smooth.mgcv_smooth` <- function (object,
                                       variables = NULL,
                                       rug = NULL,
                                       ci_level = 0.95,
                                       constant = NULL,
                                       fun = NULL,
                                       ci_alpha = 0.2,
                                       ci_col = "black",
                                       smooth_col = "black",
                                       resid_col = "steelblue3",
                                       decrease_col = "#56B4E9",
                                       increase_col = "#E69F00",
                                       change_lwd = 1.75,
                                       angle = NULL,
                                       xlab = NULL,
                                       ylab = NULL,
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = NULL,
                                       partial_residuals = NULL,
                                       ylim = NULL,
                                       ...) {
    # do we have a grouped factor by?
    grouped_by <- FALSE
    if (".term" %in% names(object) && !all(is.na(object[[".by"]]))) {
        if (is.null(variables)) {
            variables <- vars_from_label(unique(object[[".term"]]))
        }
        grouped_by <- TRUE
    } else {
        if (is.null(variables)) {
            variables <- vars_from_label(unique(object[[".smooth"]]))
        }
    }

    # If constant supplied apply it to `.estimate`
    object <- add_constant(object, constant = constant)

    # If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    # base plot - need as.name to handle none standard names, like log2(x)
    plt <- if (grouped_by) {
        by_var <- unique(object$.by)
        ggplot(object, aes(x = .data[[variables]], y = .data$.estimate,
            colour = .data[[by_var]], group = .data[[by_var]])) +
            guides(x = guide_axis(angle = angle))
    } else {
        ggplot(object, aes(x = .data[[variables]], y = .data$.estimate)) +
            guides(x = guide_axis(angle = angle))
    }

    # do we want partial residuals? Only for univariate smooths without by vars
    if (!is.null(partial_residuals)) {
        plt <- plt + geom_point(data = partial_residuals,
                                aes(x = .data[[variables]],
                                    y = .data[["partial_residual"]]),
                                inherit.aes = FALSE,
                                colour = resid_col, alpha = 0.5)
    }

    # plot the confidence interval and smooth line
    sizer_cols <- c(".change", ".increase", ".decrease")
    do_sizer <- sizer_cols %in% names(object)
    if (grouped_by) {
        plt <- plt +
        geom_ribbon(mapping = aes(ymin = .data[[".lower_ci"]],
                                  ymax = .data[[".upper_ci"]],
                                  fill = .data[[by_var]]),
                    alpha = ci_alpha, colour = NA) +
        geom_line(aes(colour = .data[[by_var]])) +
            scale_colour_okabe_ito() +
            scale_fill_okabe_ito()
        if (any(do_sizer)) {
            plt <- if (do_sizer[[1]]) {
                plt + geom_line(aes(y = .data[[".change"]],
                    colour = .data[[by_var]]), linewidth = change_lwd,
                    na.rm = TRUE)
            } else {
                plt + geom_line(aes(y = .data[[".increase"]],
                    colour = .data[[by_var]]), linewidth = change_lwd,
                    na.rm = TRUE,
                    show.legend = FALSE) +
                    geom_line(aes(y = .data[[".decrease"]],
                    colour = .data[[by_var]]), linewidth = change_lwd,
                    na.rm = TRUE,
                    show.legend = FALSE)
            }
        }
    } else {
        plt <- plt +
        geom_ribbon(mapping = aes(ymin = .data[[".lower_ci"]],
                                  ymax = .data[[".upper_ci"]]),
                    alpha = ci_alpha, colour = NA, fill = ci_col) +
        geom_line(colour = smooth_col)
        if (any(do_sizer)) {
            plt <- if (do_sizer[[1]]) {
                plt + geom_line(aes(y = .data[[".change"]]),
                    colour = smooth_col, linewidth = change_lwd, na.rm = TRUE,
                    show.legend = FALSE)
            } else {
                plt + geom_line(aes(y = .data[[".increase"]]),
                    colour = increase_col, linewidth = change_lwd,
                    na.rm = TRUE, show.legend = FALSE) +
                    geom_line(aes(y = .data[[".decrease"]]),
                        colour = decrease_col, linewidth = change_lwd,
                        na.rm = TRUE, show.legend = FALSE)
            }
        }
    }

    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- variables
    }
    if (is.null(ylab)) {
        ylab <- "Partial effect"
    }
    if (is.null(title)) {
        title <- ifelse(grouped_by, unique(object$.term),
            as.character(unique(object$.smooth)))
    }
    if (is.null(caption)) {
        caption <- paste("Basis:", object[[".type"]])
    }
    if (all(!is.na(object[[".by"]]))) {
        if (grouped_by) {
            if (is.null(subtitle)) {
                subtitle <- paste0("By: ", by_var)
            }
        } else {
            # is the by variable a factor or a numeric
            by_class <- data_class(object)[[object[[".by"]][[1L]]]]
            by_var <- as.character(unique(object[[".by"]]))
            spl <- strsplit(title, split = ":")
            title <- spl[[1L]][[1L]]
            if (is.null(subtitle)) {
                subtitle <- if (by_class != "factor") {
                    paste0("By: ", by_var) # continuous by
                } else {
                    paste0("By: ", by_var, "; ", unique(object[[by_var]]))
                }
            }
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## add rug?
    if (!is.null(rug)) {
        plt <- plt +
            geom_rug(data = rug,
                     mapping = aes(x = .data[[variables]]),
                     inherit.aes = FALSE, sides = "b", alpha = 0.5)
    }

    # fix the yaxis limits?
    if (!is.null(ylim)) {
        plt <- plt + expand_limits(y = ylim)
    }

    plt
}

#' @importFrom ggplot2 ggplot geom_point geom_raster geom_contour
#'   expand_limits labs guides guide_colourbar theme guide_axis
#' @importFrom grid unit
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
`plot_smooth.bivariate_smooth` <- function(object,
                                           variables = NULL,
                                           rug = NULL,
                                           show = c("estimate", "se"),
                                           contour = TRUE,
                                           contour_col = "black",
                                           n_contour = NULL,
                                           constant = NULL,
                                           fun = NULL,
                                           xlab = NULL,
                                           ylab = NULL,
                                           title = NULL,
                                           subtitle = NULL,
                                           caption = NULL,
                                           ylim = NULL,
                                           continuous_fill = NULL,
                                           angle = NULL,
                                           ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[[".smooth"]]))
    }

    if (is.null(continuous_fill)) {
        continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
    }

    ## If constant supplied apply it to `.estimate`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    show <- match.arg(show)
    if (isTRUE(identical(show, "estimate"))) {
        guide_title <- "Partial\neffect"
        plot_var <- ".estimate"
        guide_limits <- if (is.null(ylim)) {
            c(-1, 1) * max(abs(object[[plot_var]]), na.rm = TRUE)
        } else {
            ylim
        }
    } else {
        guide_title <- "Std. err."
        plot_var <- ".se"
        guide_limits <- range(object[[".se"]])
    }

    plt <- ggplot(object, aes(x = .data[[variables[1]]],
                              y = .data[[variables[2]]])) +
        geom_raster(mapping = aes(fill = .data[[plot_var]]))

    if (isTRUE(contour)) {
        plt <- plt + geom_contour(mapping = aes(z = .data[[plot_var]]),
                                  colour = contour_col,
                                  bins = n_contour,
                                  na.rm = TRUE)
    }

    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- variables[1L]
    }
    if (is.null(ylab)) {
        ylab <- variables[2L]
    }
    if (is.null(title)) {
        title <- unique(object[[".smooth"]])
    }
    if (is.null(caption)) {
        caption <- paste("Basis:", object[[".type"]])
    }

    if (all(!is.na(object[[".by"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[[".by"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## Set the palette
    plt <- plt + continuous_fill

    ## Set the limits for the fill
    plt <- plt + expand_limits(fill = guide_limits)

    ## add guide
    plt <- plt +
        guides(fill = guide_colourbar(title = guide_title,
            direction = "vertical",
            barheight = grid::unit(0.25, "npc")),
        x = guide_axis(angle = angle))

    ## position legend at the
    plt <- plt + theme(legend.position = "right")

    ## add rug?
    if (!is.null(rug)) {
        plt <- plt +
          geom_point(data = rug,
                     mapping = aes(x = .data[[variables[1]]],
                                   y = .data[[variables[2]]]),
                     inherit.aes = FALSE, alpha = 0.1)
    }

    plt
}

#' @importFrom ggplot2 ggplot geom_point geom_raster geom_contour aes
#'   expand_limits labs guides guide_colourbar theme facet_wrap
#' @importFrom grid unit
#' @keywords internal
#' @noRd
`plot_smooth.trivariate_smooth` <- function(object,
                                            variables = NULL,
                                            rug = NULL,
                                            show = c("estimate","se"),
                                            contour = TRUE,
                                            contour_col = "black",
                                            n_contour = NULL,
                                            constant = NULL,
                                            fun = NULL,
                                            xlab = NULL,
                                            ylab = NULL,
                                            title = NULL,
                                            subtitle = NULL,
                                            caption = NULL,
                                            ylim = NULL,
                                            continuous_fill = NULL,
                                            angle = NULL,
                                            ...) {
    if (is.null(variables)) {
        variables <- attr(object, "tensor_term_order")
        if (is.null(variables)) {
            variables <- vars_from_label(unique(object[[".smooth"]]))
        }
    }

    if (is.null(continuous_fill)) {
        continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
    }

    ## If constant supplied apply it to `estimate`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    show <- match.arg(show)
    if (isTRUE(identical(show, "estimate"))) {
        guide_title <- "Partial\neffect"
        plot_var <- ".estimate"
        guide_limits <- if (is.null(ylim)) {
            c(-1, 1) * max(abs(object[[plot_var]]), na.rm = TRUE)
        } else {
            ylim
        }
    } else {
        guide_title <- "Std. err."
        plot_var <- ".se"
        guide_limits <- range(object[[".se"]])
    }

    plt <- ggplot(object, aes(x = .data[[variables[1]]],
                              y = .data[[variables[2]]])) +
        geom_raster(mapping = aes(fill = .data[[plot_var]])) +
            facet_wrap(vars(.data[[variables[3]]]))

    if (isTRUE(contour)) {
        plt <- plt + geom_contour(mapping = aes(z = .data[[plot_var]]),
                                  colour = contour_col,
                                  bins = n_contour,
                                  na.rm = TRUE)
    }

    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- variables[1L]
    }
    if (is.null(ylab)) {
        ylab <- variables[2L]
    }
    if (is.null(title)) {
        title <- unique(object[[".smooth"]])
    }
    if (is.null(caption)) {
        caption <- paste("Facets:", variables[3], "; Basis:", object[[".type"]])
    }

    if (all(!is.na(object[[".by"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[[".by"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## Set the palette
    plt <- plt + continuous_fill

    ## Set the limits for the fill
    plt <- plt + expand_limits(fill = guide_limits)

    ## add guide
    plt <- plt +
        guides(fill = guide_colourbar(title = guide_title,
                                      direction = "vertical",
                                      barheight = grid::unit(0.25, "npc")),
        x = guide_axis(angle = angle))

    ## position legend at the
    plt <- plt + theme(legend.position = "right")

    ## add rug? -- not yet. Need a better way to select smooth_data for 3 and 4D
    ## smooths. At the moment, we are taking a few values over the range of the
    ## 3 or 4 d variables (only, 1 and 2 dim still get n values). But we don't
    ## have data at those 3/4d coordinates. When we plot with a rug, we end up
    ## introducing nrow(orig_data) new values into the object that gets plotted
    ## and this messes up the facets at draw time.
    ##
    ## What we want here perhaps is to bin the data into the groups formed by
    ## the cut points of the data that we're plottign at and only modify the
    ## rug data so that we group the data by the cuts we're facetting by and
    ## modify the 3/4d variable(s) to be these unique values that we're
    ## plotting as facets.
    # if (!is.null(rug)) {
    #     plt <- plt +
    #       geom_point(data = rug,
    #                  mapping = aes(x = .data[[variables[1]]],
    #                                y = .data[[variables[2]]]),
    #                  inherit.aes = FALSE, alpha = 0.1)
    # }

    if (inherits(object, "isotropic_smooth")) {
        plt <- plt + coord_equal()
    }

    plt
}


#' @importFrom ggplot2 ggplot geom_point geom_raster geom_contour
#'   expand_limits labs guides guide_colourbar theme facet_grid
#' @importFrom dplyr vars
#' @importFrom grid unit
#' @keywords internal
#' @noRd
`plot_smooth.quadvariate_smooth` <- function(object,
                                             variables = NULL,
                                             rug = NULL,
                                             show = c("estimate","se"),
                                             contour = TRUE,
                                             contour_col = "black",
                                             n_contour = NULL,
                                             constant = NULL,
                                             fun = NULL,
                                             xlab = NULL,
                                             ylab = NULL,
                                             title = NULL,
                                             subtitle = NULL,
                                             caption = NULL,
                                             ylim = NULL,
                                             continuous_fill = NULL,
                                             angle = NULL,
                                             ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[[".smooth"]]))
    }

    if (is.null(continuous_fill)) {
        continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
    }

    ## If constant supplied apply it to `estimate`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    show <- match.arg(show)
    if (isTRUE(identical(show, "estimate"))) {
        guide_title <- "Partial\neffect"
        plot_var <- ".estimate"
        guide_limits <- if (is.null(ylim)) {
            c(-1, 1) * max(abs(object[[plot_var]]), na.rm = TRUE)
        } else {
            ylim
        }
    } else {
        guide_title <- "Std. err."
        plot_var <- ".se"
        guide_limits <- range(object[[".se"]])
    }

    plt <- ggplot(object, aes(x = .data[[variables[1]]],
                              y = .data[[variables[2]]])) +
        geom_raster(mapping = aes(fill = .data[[plot_var]])) +
        facet_grid(rows = vars(.data[[variables[3]]]),
                   cols = vars(.data[[variables[4]]]),
                   as.table = FALSE)

    if (isTRUE(contour)) {
        plt <- plt + geom_contour(mapping = aes(z = .data[[plot_var]]),
                                  colour = contour_col,
                                  bins = n_contour,
                                  na.rm = TRUE)
    }

    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- variables[1L]
    }
    if (is.null(ylab)) {
        ylab <- variables[2L]
    }
    if (is.null(title)) {
        title <- unique(object[[".smooth"]])
    }
    if (is.null(caption)) {
        caption <- paste("Facet rows:", variables[3],
            "; columns:", variables[4],
            "; Basis:", object[[".type"]])
    }

    if (all(!is.na(object[[".by"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[[".by"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## Set the palette
    plt <- plt + continuous_fill

    ## Set the limits for the fill
    plt <- plt + expand_limits(fill = guide_limits)

    ## add guide
    plt <- plt +
        guides(fill = guide_colourbar(title = guide_title,
                                      direction = "vertical",
                                      barheight = grid::unit(0.25, "npc")),
        x = guide_axis(angle = angle))

    ## position legend at the
    plt <- plt + theme(legend.position = "right")

    ## add rug? -- not yet. Need a better way to select smooth_data for 3 and 4D
    ## smooths. At the moment, we are taking a few values over the range of the
    ## 3 or 4 d variables (only, 1 and 2 dim still get n values). But we don't
    ## have data at those 3/4d coordinates. When we plot with a rug, we end up
    ## introducing nrow(orig_data) new values into the object that gets plotted
    ## and this messes up the facets at draw time.
    ##
    ## What we want here perhaps is to bin the data into the groups formed by
    ## the cut points of the data that we're plotting at and only modify the
    ## rug data so that we group the data by the cuts we're faceting by and
    ## modify the 3/4d variable(s) to be these unique values that we're
    ## plotting as facets.
    # if (!is.null(rug)) {
    #     plt <- plt +
    #       geom_point(data = rug,
    #                  mapping = aes(x = .data[[variables[1]]],
    #                                y = .data[[variables[2]]]),
    #                  inherit.aes = FALSE, alpha = 0.1)
    # }

    if (inherits(object, "isotropic_smooth")) {
        plt <- plt + coord_equal()
    }

    plt
}

#' @importFrom ggplot2 coord_equal
`plot_smooth.isotropic_smooth` <- function(object, ...) {
    # plot as per a bivariate smooth
    plt <- plot_smooth.bivariate_smooth(object, ...)

    # but set the x/y coordinates to have aspect ratio = 1
    plt <- plt + coord_equal(ratio = 1)

    plt # return
}

#' @importFrom ggplot2 ggplot geom_point geom_abline expand_limits
#'   labs
#' @keywords internal
#' @noRd
`plot_smooth.random_effect` <- function(object,
                                        variables = NULL,
                                        qq_line = TRUE,
                                        constant = NULL,
                                        fun = NULL,
                                        xlab = NULL,
                                        ylab = NULL,
                                        title = NULL,
                                        subtitle = NULL,
                                        caption = NULL,
                                        ylim = NULL,
                                        angle = NULL,
                                        ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[[".smooth"]]))
    }

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    ## base plot with computed QQs
    plt <- ggplot(object, aes(sample = .data[[".estimate"]])) +
        geom_point(stat = "qq") +
        guides(x = guide_axis(angle = angle))

    ## add a QQ reference line
    if (isTRUE(qq_line)) {
        sampq <- quantile(object[[".estimate"]], c(0.25, 0.75))
        gaussq <- qnorm(c(0.25, 0.75))
        slope <- diff(sampq) / diff(gaussq)
        intercept <- sampq[1L] - slope * gaussq[1L]

        plt <- plt + geom_abline(slope = slope, intercept = intercept)
    }

    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- "Gaussian quantiles"
    }
    if (is.null(ylab)) {
        ylab <- "Partial effects"
    }
    if(is.null(title)) {
        title <- variables
    }
    if (is.null(caption)) {
        caption <- paste("Basis:", object[[".type"]])
    }

    if (all(!is.na(object[[".by"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[[".by"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## fixing the y axis limits?
    if (!is.null(ylim)) {
        plt <- plt + expand_limits(y = ylim)
    }

    plt
}

#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_point geom_line expand_limits theme aes
#'   labs
#' @keywords internal
#' @noRd
`plot_smooth.factor_smooth` <- function(object,
                                        variables = NULL,
                                        rug = NULL,
                                        constant = NULL,
                                        fun = NULL,
                                        xlab = NULL, 
                                        ylab = NULL,
                                        title = NULL,
                                        subtitle = NULL,
                                        caption = NULL,
                                        ylim = NULL,
                                        discrete_colour = NULL,
                                        angle = NULL,
                                       ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[[".smooth"]]))
    }

    if (is.null(discrete_colour)) {
        discrete_colour <- scale_colour_discrete()
    }

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    plt <- ggplot(object, aes(x = .data[[variables[1]]],
                              y = .data[[".estimate"]],
                              colour = .data[[variables[2]]])) +
        geom_line() +
        discrete_colour +
        theme(legend.position = "none") +
        guides(x = guide_axis(angle = angle))

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- variables[1]
    }
    if (missing(ylab)) {
        ylab <- "Partial effect"
    }
    if (is.null(title)) {
        title <- unique(object[[".smooth"]])
    }
    if (is.null(caption)) {
        caption <- paste("Basis:", object[[".type"]])
    }

    if (all(!is.na(object[[".by"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[[".by"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## add rug?
    if (!is.null(rug)) {
        plt <- plt + geom_rug(data = rug,
                              mapping = aes(x = .data[[variables[1]]]),
                              inherit.aes = FALSE,
                              sides = "b", alpha = 0.5)
    }

    ## fixing the y axis limits?
    if (!is.null(ylim)) {
        plt <- plt + expand_limits(y = ylim)
    }

    plt
}

#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_point geom_line expand_limits theme aes
#'   labs scale_fill_hue scale_colour_hue
#' @importFrom ggokabeito scale_colour_okabe_ito scale_fill_okabe_ito
#' @keywords internal
#' @noRd
`plot_smooth.sz_factor_smooth` <- function(object,
                                           variables = NULL,
                                           rug = NULL,
                                           constant = NULL,
                                           fun = NULL,
                                           ci_alpha = 0.2,
                                           xlab = NULL,
                                           ylab = NULL,
                                           title = NULL,
                                           subtitle = NULL,
                                           caption = NULL,
                                           ylim = NULL,
                                           discrete_colour = NULL,
                                           discrete_fill = NULL,
                                           angle = NULL,
                                           ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[[".smooth"]]))
    }

    # variables will likely be length two, but it could be >2 if there are
    # multivariate factors
    fs <- vapply(object[variables], is.factor, logical(1L))
    if (length(variables) > 2L) {
        object <- mutate(object,
            ".sz_var" = interaction(object[variables[fs]], sep = ":",
            lex.order = TRUE))
        fac_var <- ".sz_var"
        fac_var_lab <- paste(variables[fs], sep = ":")
        x_var <- variables[!fs]

        # need to repeat for the rug
        if (!is.null(rug)) {
            rug <- mutate(rug,
                ".sz_var" = interaction(object[variables[fs]], sep = ":",
            lex.order = TRUE))
        }

        if (length(x_var) > 1L) {
            # this is a bivariate sz factor smooth, which we can't handle yet
            return(NULL)
        }
    } else {
        # which is the factor?
        if (fs[1L]) {
            x_var <- variables[2]
            fac_var <- fac_var_lab <- variables[1]
        } else {
            x_var <- variables[1]
            fac_var <- fac_var_lab <- variables[2]
        }
    }

    # how many levels? can't have more than 9 for okabeito
    n_levs <- nlevels(object[[fac_var]])
    if (is.null(discrete_colour)) {
        discrete_colour <- if (n_levs > 9L) {
            scale_colour_hue()
        } else {
            scale_colour_okabe_ito()
        }
    }

    if (is.null(discrete_fill)) {
        discrete_fill <- if (n_levs > 9L) {
            scale_fill_hue()
        } else {
            scale_fill_okabe_ito()
        }
    }

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    # plot
    plt <- ggplot(object, aes(x = .data[[x_var]],
                              y = .data[[".estimate"]],
                              colour = .data[[fac_var]])) +
        geom_ribbon(mapping = aes(ymin = .data[[".lower_ci"]],
                                  ymax = .data[[".upper_ci"]],
                                  fill = .data[[fac_var]],
                                  colour = NULL),
                    alpha = ci_alpha) +
        geom_line() +
        discrete_colour +
        discrete_fill +
        guides(x = guide_axis(angle = angle))

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- x_var
    }
    if (missing(ylab)) {
        ylab <- "Partial effect"
    }
    if (is.null(title)) {
        title <- unique(object[[".smooth"]])
    }
    if (is.null(caption)) {
        caption <- paste("Basis:", object[[".type"]])
    }

    if (all(!is.na(object[[".by"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[[".by"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
        caption = caption, colour = fac_var_lab, fill = fac_var_lab)

    ## add rug?
    if (!is.null(rug)) {
        plt <- plt + geom_rug(data = rug,
                              mapping = aes(x = .data[[x_var]],
                              colour = .data[[fac_var]]),
                              inherit.aes = FALSE,
                              sides = "b", alpha = 0.5)
    }

    ## fixing the y axis limits?
    if (!is.null(ylim)) {
        plt <- plt + expand_limits(y = ylim)
    }

    plt
}

#' @importFrom ggplot2 coord_map geom_tile guide_colourbar geom_contour aes
`plot_smooth.sos` <- function(object,
                              variables = NULL,
                              rug = NULL,
                              show = c("estimate", "se"),
                              contour = TRUE,
                              contour_col = "black",
                              n_contour = NULL,
                              constant = NULL,
                              fun = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              ylim = NULL,
                              continuous_fill = NULL,
                              projection = "orthographic",
                              orientation = NULL,
                              angle = NULL,
                              ...) {
    # handle splines on the sphere

    # this currently needs the mapproj pkg for coord_map()
    if (!requireNamespace("mapproj", quietly = TRUE)) {
        message("\nPlotting SOS smooths uses `ggplot2::coord_map()`.\n",
            "This requires that the {mapproj} package be installed.\n",
            "Run: `install.packages(\"mapproj\")`\n")
        stop("Package {mapproj} is not available.")
    }

    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[[".smooth"]]))
    }

    if (is.null(continuous_fill)) {
        continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
    }

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    show <- match.arg(show)
    if (isTRUE(identical(show, "estimate"))) {
        guide_title <- "Partial\neffect"
        plot_var <- ".estimate"
        guide_limits <- if (is.null(ylim)) {
            c(-1, 1) * max(abs(object[[plot_var]]), na.rm = TRUE)
        } else {
            ylim
        }
    } else {
        guide_title <- "Std. err."
        plot_var <- ".se"
        guide_limits <- range(object[[".se"]])
    }

    # if orientation is not specified, use c(20, 0, mean(range(longitude)))
    if (is.null(orientation)) {
        orientation <- c(20, 0, mean(range(object[[variables[2]]])))
    }

    # base plot
    # Simon parameterises the SOS with first argument latitude and second
    #  argument longitude, so we need to reverse that here
    plt <- ggplot(object, aes(x = .data[[variables[2]]],
                              y = .data[[variables[1]]])) +
        geom_tile(mapping = aes(fill = .data[[plot_var]])) +
        coord_map(projection = projection,
                  orientation = orientation)

    if (isTRUE(contour)) {
        plt <- plt + geom_contour(mapping = aes(z = .data[[plot_var]]),
                                  colour = contour_col,
                                  bins = n_contour,
                                  na.rm = TRUE)
    }

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- variables[2] ## yes, the smooth is s(lat, lon) !
    }

    if (missing(ylab)) {
        ylab <- variables[1] ## yes, the smooth is s(lat, lon) !
    }

    if (is.null(title)) {
        title <- unique(object[[".smooth"]])
    }

    if (is.null(caption)) {
        caption <- paste("Basis:", object[[".type"]])
    }

    if (all(!is.na(object[[".by"]]))) {
        # is the by variable a factor or a numeric
        by_class <- data_class(object)[[object[[".by"]][[1L]]]]
        by_var <- as.character(unique(object[[".by"]]))
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            subtitle <- if (by_class != "factor") {
                paste0("By: ", by_var) # continuous by
            } else {
                paste0("By: ", by_var, "; ", unique(object[[by_var]]))
            }
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## Set the palette
    plt <- plt + continuous_fill

    ## Set the limits for the fill
    plt <- plt + expand_limits(fill = guide_limits)

    ## add guide
    plt <- plt +
        guides(fill = guide_colourbar(title = guide_title,
            direction = "vertical",
            barheight = grid::unit(0.25, "npc")),
            x = guide_axis(angle = angle))

    ## position legend at the
    plt <- plt + theme(legend.position = "right")

    ## add rug?
    if (!is.null(rug)) {
        plt <- plt +
          geom_point(data = rug, ## yes, the smooth is s(lat, lon) !
                     mapping = aes(x = .data[[variables[2]]],
                                   y = .data[[variables[1]]]),
                     inherit.aes = FALSE, alpha = 0.1)
    }

    plt
}
