##' New `evaluate_smooth()` alike
##'
##' @param object an object of class `"gam"` or `"gamm"`.
##' @param smooth character; a single smooth to evaluate.
##' @param n numeric; the number of points over the range of the covariate at
##'   which to evaluate the smooth.
##' @param data a vector or data frame of points at which to evaluate the
##'   smooth.
##' @param unconditional logical; should confidence intervals include the
##'   uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian
##'   covariance matrix will be used.
##' @param overall_uncertainty logical; should the uncertainty in the model
##'  constant term be included in the standard error of the evaluate values of
##'  the smooth?
##' @param dist numeric; if greater than 0, this is used to determine when
##'   a location is too far from data to be plotted when plotting 2-D smooths.
##'   The data are scaled into the unit square before deciding what to exclude,
##'   and `dist` is a distance within the unit square. See
##'   [mgcv::exclude.too.far()] for further details.
##' @param ... arguments passed to other methods.
##'
##' @return A data frame (tibble), which is of class `"smooth_estimates"`.
##' 
##' @export
##' 
##' @rdname smooth_estimates
##'
##' @examples
##' load_mgcv()
##' \dontshow{
##' set.seed(2)
##' op <- options(cli.unicode = FALSE, digits = 6)
##' }
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' ## evaluate all smooths
##' smooth_estimates(m1)
##'
##' ## or selected smooths
##' smooth_estimates(m1, smooth = c("s(x0)", "s(x1)"))
##' \dontshow{options(op)}
`smooth_estimates` <- function(object, ...) {
    UseMethod("smooth_estimates")
}

##' @export
##' @rdname smooth_estimates
##' @importFrom dplyr bind_rows all_of
##' @importFrom tidyr unnest
`smooth_estimates.gam` <- function(object,
                                   smooth = NULL,
                                   n = 100,
                                   data = NULL,
                                   unconditional = FALSE,
                                   overall_uncertainty = TRUE,
                                   dist = 0.1, ...) {
    ## if particular smooths selected
    smooth_ids <- if (!is.null(smooth)) {
         which_smooths(object, smooth) # which smooths match 'smooth'
    } else {
        seq_len(n_smooths(object))
    }

    ## extract the mgcv.smooth objects
    smooths <- get_smooths_by_id(object, smooth_ids)

    ## loop over the smooths and evaluate them
    sm_list <- vector(mode = "list", length = length(smooths))

    for (i in seq_along(sm_list)) {
        sm_list[[i]] <- eval_smooth(smooths[[i]],
                                    model = object,
                                    n = n,
                                    data = data,
                                    unconditional = unconditional,
                                    overall_uncertainty = overall_uncertainty)
    }

    ## create a single df of all the smooths
    sm_list <- bind_rows(sm_list)
    ## need to unnest the `data` column
    sm_list <- unnest(sm_list, all_of('data'))

    ## add a class
    class(sm_list) <- c("smooth_estimates", class(sm_list))

    ## return
    sm_list
}

##' @export
`smooth_estimates.gamm` <- function(object, ...) {
    smooth_estimates(object[["gam"]], ...)
}

##' Determine the type of smooth and return it n a human readble form
##'
##' @param smooth an object inheriting from class `mgcv.smooth`.
##'
##' @keywords internal
##' @noRd
`smooth_type` <- function(smooth) {
    sm_type <- if (inherits(smooth, "tprs.smooth")) {
        "TPRS"
    } else if (inherits(smooth, "ts.smooth")) {
        "TPRS (shrink)"
    } else if (inherits(smooth, "cr.smooth")) {
        "CRS"
    } else if (inherits(smooth, "cs.smooth")) {
        "CRS (shrink)"
    } else if (inherits(smooth, "cyclic.smooth")) {
        "Cyclic CRS"
    } else if (inherits(smooth, "pspline.smooth")) {
        "P spline"
    } else if (inherits(smooth, "cp.smooth")) {
        "Cyclic P spline"
    } else if (inherits(smooth, "Bspline")) {
        "B spline"
    } else if (inherits(smooth, "duchon.spline")) {
        "Duchon"
    } else if (inherits(smooth, "fs.interaction")) {
        "Factor smooth"
    } else if (inherits(smooth, "gp.smooth")) {
        "GP"
    } else if (inherits(smooth, "mrf.smooth")) {
        "MRF"
    } else if (inherits(smooth, "random.effect")) {
        "Random effect"
    } else if (inherits(smooth, "sw")) {
        "Soap (wiggly)"
    } else if (inherits(smooth, "sf")) {
        "Soap (boundary)"
    } else if (inherits(smooth, "soap.film")) {
        "Soap"
    } else if (inherits(smooth, "t2.smooth")) {
        "Tensor (T2)"
    } else if (inherits(smooth, "sos.smooth")) {
        "SOS"
    } else if (inherits(smooth, "tensor.smooth")) {
        "Tensor"
    } else {
        stop("Uknown type of smooth")
    }

    sm_type
}

##' Check user-supplied data for suitability
##'
##' @param data a data frame of variables to be checked.
##' @param vars character; vector of terms.
##'
##' @importFrom tibble tibble
##' @importFrom rlang := !!
##'
##' @keywords internal
##' @noRd
`check_user_data` <- function(data, vars) {
    if (is.data.frame(data)) {
        smooth_vars <- vars %in% names(data)
        if (!all(vars %in% names(data))) {
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

##' Evaluate estimated spline values
##'
##' @param smooth currently an object that inherits from class `mgcv.smooth`.
##' @param model a fitted model; currently only [mgcv::gam()] and [mgcv::bam()]
##'   models are suported.
##' @param data an optional data frame of values to evaluate `smooth` at.
##'
##' @inheritParams eval_smooth
##' 
##' @importFrom tibble tibble add_column
##' @importFrom rlang := !!
##' @importFrom dplyr pull all_of
##' @importFrom tidyr nest unnest
##' @importFrom mgcv PredictMat
##'
##' @keywords internal
##' @noRd
`spline_values2` <- function(smooth, data, model, unconditional,
                             overall_uncertainty = TRUE) {
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

    if (isTRUE(overall_uncertainty) && attr(smooth, "nCons") > 0L) {
        if (lcms < nc) {
            column_means <- c(column_means, rep(0, nc - lcms))
        }
        Xcm <- matrix(column_means, nrow = nrow(X), ncol = nc, byrow = TRUE)
        if (!is.null(meanL1)) {
            Xcm <- Xcm / meanL1
        }
        Xcm[, para.seq] <- X
        rs <- rowSums((Xcm %*% V) * Xcm)
    } else {
        rs <- rowSums((X %*% V[para.seq, para.seq]) * X)
    }

    se.fit <- sqrt(pmax(0, rs))

    d <- smooth_dim(smooth)
    sm_var <- smooth_variable(smooth)

    ## Return object
    out <- if (d == 1L) {
        if (is_fs_smooth(smooth)) {
            tbl <- tibble(smooth = rep(label, nrow(X)),
                          est = fit, se = se.fit)
            tbl <- bind_cols(tbl, data)
            nest(tbl, data = all_of(names(data)))
        } else {
            tbl <- tibble(smooth = rep(label, nrow(X)),
                          est = fit, se = se.fit,
                          !!(sm_var) := pull(data, sm_var))
            nest(tbl, data = !!(sm_var))
        }
     } else {
        tbl <- bind_cols(tibble(smooth = rep(label, nrow(X)),
                                est = fit, se = se.fit),
                         data)
        nest(tbl, data = all_of(names(data)))
    }

    nr <- nrow(out)
    ## out <- unnest(out, all_of("data")) # not here
    out
}

##' S3 methods to evaluate individual smooths
##' 
##' @param smooth currently an object that inherits from class `mgcv.smooth`.
##' @param model a fitted model; currently only [mgcv::gam()] and [mgcv::bam()]
##'   models are suported.
##' @param data an optional data frame of values to evaluate `smooth` at.
##' @param ... arguments assed to other methods
##'
##' @inheritParams smooth_estimates
##' 
##' @export
`eval_smooth` <- function(smooth, ...) {
    UseMethod("eval_smooth")
}

##' @rdname eval_smooth
##' @importFrom tibble add_column
##' @export
`eval_smooth.mgcv.smooth` <- function(smooth, model, n = 100, data = NULL,
                                      unconditional = FALSE,
                                      overall_uncertainty = TRUE,
                                      ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later

    ## deal with data if supplied
    data <- process_user_data_for_eval(data = data, model = model, n = n,
                                       id = which_smooth(model,
                                                         smooth_label(smooth)))

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
                              unconditional = unconditional,
                              model = model,
                              overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    nr <- nrow(eval_sm)
    eval_sm <- add_column(eval_sm, by = rep(by_var, nr),
                          .after = 1L)
    ## add on spline type info
    sm_type <- smooth_type(smooth)
    eval_sm <- add_column(eval_sm, type = rep(sm_type, nr),
                          .after = 1L)
    ## return
    eval_sm
}

##' Wrapper to `gratia::smooth_data()` and `gratia:::check_user_data()` for use
##' with [gratia::eval_smooth()] methods
##'
##' @param data an optional data frame of values for the smooth
##' @param model a fitted model
##' @param n numeric; the number of new observations to generate. Passed to
##'   [gratia::smooth_data()].
##' @param id the number ID of the smooth within `model` to process.
##'
##' @keywords internal
##' @noRd
`process_user_data_for_eval` <- function(data, model, n, id) {
    data <- if (is.null(data)) {
        smooth_data(model = model, n = n, id = id)
   } else {
        smooth <- get_smooths_by_id(model, id)[[1L]]
        vars <- smooth_variable(smooth)
        by_var <- by_variable(smooth)
        if (!identical(by_var, "NA")) {
            vars <- append(vars, by_var)
        }
        check_user_data(data, vars)
    }
    data
}

##' @rdname eval_smooth
##' @export
##' @importFrom tibble add_column
`eval_smooth.fs.interaction` <- function(smooth, model, n = 100, data = NULL,
                                         unconditional = FALSE,
                                         overall_uncertainty = TRUE,
                                         ...) {
    
    by_var <- by_variable(smooth) # even if not a by as we want NA later

    ## deal with data if supplied
    data <- process_user_data_for_eval(data = data, model = model, n = n,
                                       id = which_smooth(model,
                                                         smooth_label(smooth)))

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
                              unconditional = unconditional,
                              model = model,
                              overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    nr <- nrow(eval_sm)
    eval_sm <- add_column(eval_sm, by = rep(by_var, nr),
                          .after = 1L)
    ## add on spline type info
    sm_type <- smooth_type(smooth)
    eval_sm <- add_column(eval_sm, type = rep(sm_type, nr),
                          .after = 1L)
    ## return
    eval_sm
}

##' @rdname eval_smooth
##' @export
##' @importFrom tibble add_column
`eval_smooth.random.effect` <- function(smooth, model, n = 100, data = NULL,
                                        unconditional = FALSE,
                                        overall_uncertainty = TRUE,
                                        ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later

    ## deal with data if supplied
    data <- process_user_data_for_eval(data = data, model = model, n = n,
                                       id = which_smooth(model,
                                                         smooth_label(smooth)))

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
                              unconditional = unconditional,
                              model = model,
                              overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    nr <- nrow(eval_sm)
    eval_sm <- add_column(eval_sm, by = rep(by_var, nr),
                          .after = 1L)
    ## add on spline type info
    sm_type <- smooth_type(smooth)
    eval_sm <- add_column(eval_sm, type = rep(sm_type, nr),
                          .after = 1L)
    ## return
    eval_sm
}

##' @rdname eval_smooth
##' @export
##' @importFrom tibble add_column
`eval_smooth.mrf.smooth` <- function(smooth, model, n = 100, data = NULL,
                                     unconditional = FALSE,
                                     overall_uncertainty = TRUE,
                                     ...) {
    .NotYetImplemented()
}

##' @rdname eval_smooth
##' @export
##' @importFrom tibble add_column
`eval_smooth.t2.smooth` <- function(smooth, model, n = 100, data = NULL,
                                    unconditional = FALSE,
                                    overall_uncertainty = TRUE,
                                    ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later

    ## deal with data if supplied
    data <- process_user_data_for_eval(data = data, model = model, n = n,
                                       id = which_smooth(model,
                                                         smooth_label(smooth)))

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
                              unconditional = unconditional,
                              model = model,
                              overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    nr <- nrow(eval_sm)
    eval_sm <- add_column(eval_sm, by = rep(by_var, nr), .after = 1L)
    ## add on spline type info
    sm_type <- smooth_type(smooth)
    eval_sm <- add_column(eval_sm, type = rep(sm_type, nr), .after = 1L)
    ## return
    eval_sm
}

##' @rdname eval_smooth
##' @export
##' @importFrom tibble add_column
`eval_smooth.tensor.smooth` <- function(smooth, model, n = 100, data = NULL,
                                        unconditional = FALSE,
                                        overall_uncertainty = TRUE,
                                        ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later

    ## deal with data if supplied
    data <- process_user_data_for_eval(data = data, model = model, n = n,
                                       id = which_smooth(model,
                                                         smooth_label(smooth)))

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
                              unconditional = unconditional,
                              model = model,
                              overall_uncertainty = overall_uncertainty)

    ## add on info regarding by variable
    nr <- nrow(eval_sm)
    eval_sm <- add_column(eval_sm, by = rep(by_var, nr),
                          .after = 1L)
    ## add on spline type info
    sm_type <- smooth_type(smooth)
    eval_sm <- add_column(eval_sm, type = rep(sm_type, nr),
                          .after = 1L)
    ## return
    eval_sm
}
