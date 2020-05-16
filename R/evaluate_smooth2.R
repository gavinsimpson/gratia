`evaluate_smooth2` <- function(object, ...) {
    UseMethod("evaluate_smooth2")
}

##' @export
##' @rdname evaluate_smooth2
`evaluate_smooth2.gam` <- function(object, smooth = NULL, n = 100, newdata = NULL,
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
        sm_type <- smooth_type(smooths[[i]])
        sm_list[[i]] <-
            switch(sm_type,
                   `1d_smooth`  = eval_1d_smooth(smooths[[i]],
                                               n = n,
                                               newdata = newdata,
                                               unconditional = unconditional,
                                               overall_uncertainty = overall_uncertainty),
                   `2d_smooth`  = eval_2d_smooth(smooths[[i]],
                                                 n = n,
                                                 newdata = newdata,
                                                 unconditional = unconditional,
                                                 overall_uncertainty = overall_uncertainty),
                   re_smooth  = eval_re_smooth(smooths[[i]],
                                               newdata = newdata,
                                               unconditional = unconditional),
                   fs_smooth  = eval_fs_smooth(smooths[[i]],
                                               n = n,
                                               newdata = newdata,
                                               unconditional = unconditional,
                                               overall_uncertainty = overall_uncertainty),
                   mrf_smooth = eval_mrf_smooth(smooths[[i]],
                                                newdata = newdata,
                                                unconditional = unconditional,
                                                overall_uncertainty = overall_uncertainty),
                   tensor_smooth = eval_tensor_smooth(smooths[[i]],
                                                      n = n,
                                                      newdata = newdata,
                                                      unconditional = unconditional,
                                                      overall_uncertainty = overall_uncertainty)
                   )
    }
}

`smooth_type` <- function(smooth) {
    sm_type <- if (is_1d_smooth(smooth)) {
        "1d_smooth"
    } else if (is_2d_smooth(smooth)) {
        "2d smooth"
    } else if (is_re_smooth(smooth)) {
        "re_smooth"
    } else if (is_fs_smooth(smooth)) {
        "fs_smooth"
    } else if (is_mrf_smooth(smooth)) {
        "mrf_smooth"
    } else if (is_general_tensor_smooth(smooth)) {
        "tensor_smooth"
    } else {
        stop("Uknown type of smooth")
    }

    sm_type
}

##' @importFrom tibble
##' @importFrom rlang := !!
`check_user_data` <- function(data, vars) {
    if (is.data.frame(data)) {
        if (!all(vars %in% names(data))) {
            stop(paste("Variable", smooth_var, "not found in 'data'."))
        }
    } else if (is.numeric(newdata)) {   # vector; coerce to data frame
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
    ## Return
    out <- if (d == 1L) {
        if (is_fs_smooth(smooth)) {
            stop("Not yet implemented.")
            tibble(smooth = rep(label, nrow(X)),
                   x = newdata[, 1L], f = newdata[, 2L],
                   est = fit, se = se.fit)
        } else {
            tibble(smooth = rep(label, nrow(X)),
                   !!(sm_var) := pull(data, sm_var),
                   est = fit, se = se.fit)
        }
    } else {
        tibble(smooth = rep(label, nrow(X)),
               !!(sm_var[1L]) := pull(data, sm_var[1L]),
               !!(sm_var[2L]) := pull(data, sm_var[2L]),
               est = fit, se = se.fit)
    }

    nr <- nrow(out)
    
    out <- add_column(out, type = rep(mgcv_type(smooth), nr),
                      .after = 1L)
    out
}

##' @importFrom tibble add_column
`eval_1d_smooth` <- function(smooth, model, n = 100, data = NULL,
                             unconditional = FALSE,
                             overall_uncertainty = TRUE) {
    is_by <- is_by_smooth(smooth)
    by_var <- by_variable(smooth) # get even if not a by as we want NA later

    sm_var <- smooth_variable(smooth)

    ## deal with data if supplied
    data <- if (is.null(data)) {
        smooth_data(model = model, n = n, smooth = smooth)
    } else {
        vars <- sm_var
        if (is.na(by_var)) {
            vars <- append(vars, by_var)
        }
        check_user_data(data, vars)
    }
    
    eval_sm <- spline_values2(smooth, data = data,
                              unconditional = unconditional,
                              model = model,
                              overall_uncertainty = overall_uncertainty,
                              term = sm_var)

    ## add on info regarding by variable
    eval_sm <- add_column(eval_sm, by = rep(by_var, nrow(eval_sm)),
                          .after = 2L)

    eval_sm
}
