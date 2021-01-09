##' New `evaluate_smooth()` alike
##'
##' @param object an object of class `"gam"` or `"gamm"`.
##' @param smooth character; a single smooth to evaluate.
##' @param n numeric; the number of points over the range of the covariate at
##'   which to evaluate the smooth.
##' @param newdata a vector or data frame of points at which to evaluate the
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
##' @return A data frame, which is of class `"evaluated_1d_smooth"` or
##'   `evaluated_2d_smooth`, which inherit from classes `"evaluated_smooth"`
##'   and `"data.frame"`.
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
`smooth_estimates.gam` <- function(object, smooth = NULL, n = 100, newdata = NULL,
                                   unconditional = FALSE, overall_uncertainty = TRUE,
                                   dist = 0.1, ...) {
    ## if particular smooths selected
    smooth_ids <- if (!is.null(smooth)) {
         which_smooths(object, smooth) # which smooths match 'smooth'
    } else {
        seq_len(n_smooths(object))
    }

    smooths <- get_smooths_by_id(object, smooth_ids) # extract the mgcv.smooth objects

    ## loop over the smooths and evaluate them
    sm_list <- vector(mode = "list", length = length(smooths))

    for (i in seq_along(sm_list)) {
        sm_list[[i]] <- eval_smooth(smooths[[i]],
                                    model = object,
                                    n = n,
                                    data = newdata,
                                    unconditional = unconditional,
                                    overall_uncertainty = overall_uncertainty)
    }

    sm_list <- bind_rows(sm_list)
    sm_list <- unnest(sm_list, all_of('data'))
    sm_list
}

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
        "FS"
    } else if (inherits(smooth, "gp.smooth")) {
        "GP"
    } else if (inherits(smooth, "mrf.smooth")) {
        "MRF"
    } else if (inherits(smooth, "random.effect")) {
        "Ranef"
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

##' @importFrom tibble tibble
##' @importFrom rlang := !!
`check_user_data` <- function(data, vars) {
    if (is.data.frame(data)) {
        smooth_vars <- vars %in% names(data)
        if (!all(vars %in% names(data))) {
            stop(paste("Variable(s)",
                       paste(vars[!smooth_vars], collapse = ', '),
                       "not found in 'data'."))
        }
    } else if (is.numeric(data)) {   # vector; coerce to data frame
        if (length(vars) > 1L) {
            stop("'smooth' requires multiple data vectors but only 1 provided.")
        }
        data <- tibble(!!(vars) := data)
    } else {                            # object we can't handle; bail out
        stop("'data', if supplied, must be a numeric vector or a data frame.")
    }
    data
}

## Returns the type of smoother as far as mgcv is concerned
`mgcv_type` <- function(smooth) {
    stopifnot(is_mgcv_smooth(smooth))
    cls <- class(smooth)[1L]
    cls <- sub("\\.smooth$", "", cls)
    cls
}

##' @importFrom tibble tibble add_column
##' @importFrom rlang := !!
##' @importFrom dplyr pull all_of
##' @importFrom tidyr nest unnest
`spline_values2` <- function(smooth, data, model, unconditional,
                             overall_uncertainty = TRUE, term) {
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
            stop("Not yet implemented.")
            tibble(smooth = rep(label, nrow(X)),
                   est = fit, se = se.fit,
                   x = data[, 1L], f = data[, 2L])
        } else {
            tbl <- tibble(smooth = rep(label, nrow(X)),
                          est = fit, se = se.fit,
                          !!(sm_var) := pull(data, sm_var))
            nest(tbl, data = !!(sm_var))
        }
     } else {
        ## need to adapt this to n dimensions!       
        tbl <- tibble(smooth = rep(label, nrow(X)),
                      est = fit, se = se.fit,
                      !!(sm_var[1L]) := pull(data, sm_var[1L]),
                      !!(sm_var[2L]) := pull(data, sm_var[2L]))
        nest(tbl, data = c(!!(sm_var[1L]), !!(sm_var[2L])))
    }

    nr <- nrow(out)
    ## out <- unnest(out, all_of("data")) # not here
    out
}

##' S3 methods to evaulate individual smooths
##' 
##' @param smooth currently an object that inherits from class `mgcv_smooth`
##' @param ... arguments assed to other methods
##' 
##' @export
`eval_smooth` <- function(smooth, ...) {
    UseMethod("eval_smooth")
}
##' @importFrom tibble add_column
##' @export
`eval_smooth.mgcv.smooth` <- function(smooth, model, n = 100, data = NULL,
                                      unconditional = FALSE,
                                      overall_uncertainty = TRUE,
                                      ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later

    sm_var <- smooth_variable(smooth)

    ## deal with data if supplied
    data <- process_user_data_for_eval(data = data, model = model, n = n,
                                       id = which_smooth(model,
                                                         smooth_label(smooth)))

    ## values of spline at data
    eval_sm <- spline_values2(smooth, data = data,
                              unconditional = unconditional,
                              model = model,
                              overall_uncertainty = overall_uncertainty,
                              term = sm_var)

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


`process_user_data_for_eval` <- function(data, model, n, id) {
    data <- if (is.null(data)) {
        smooth_data(model = model, n = n, id = id)
   } else {
        smooth <- get_smooths_by_id(id)        
        vars <- smooth_variable(smooth)
        by_var <- by_variable(smooth)
        if (!is.na(by_var)) {
            vars <- append(vars, by_var)
        }
        check_user_data(data, vars)
    }
    data
}

##' @importFrom tibble add_column
`eval_smooth.fs.interaction` <- function(smooth, model, n = 100, data = NULL,
                                         unconditional = FALSE,
                                         overall_uncertainty = TRUE,
                                         ...) {
    .NotYetImplemented()
}

##' @importFrom tibble add_column
`eval_smooth.random.effect` <- function(smooth, model, n = 100, data = NULL,
                                        unconditional = FALSE,
                                        overall_uncertainty = TRUE,
                                        ...) {
    .NotYetImplemented()
    is_by <- is_by_smooth(smooth)
    by_var <- by_variable(smooth) # get even if not a by as we want NA later
}

##' @importFrom tibble add_column
`eval_smooth.mrf.smooth` <- function(smooth, model, n = 100, data = NULL,
                                     unconditional = FALSE,
                                     overall_uncertainty = TRUE,
                                     ...) {
    .NotYetImplemented()
}

##' @importFrom tibble add_column
`eval_smooth.t2.smooth` <- function(smooth, model, n = 100, data = NULL,
                                    unconditional = FALSE,
                                    overall_uncertainty = TRUE,
                                    ...) {
    .NotYetImplemented()
}

##' @importFrom tibble add_column
`eval_smooth.tensor.smooth` <- function(smooth, model, n = 100, data = NULL,
                                        unconditional = FALSE,
                                        overall_uncertainty = TRUE,
                                        ...) {
    .NotYetImplemented()
}
