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
##' op <- options(cli.unicode = FALSE, digits = 6)
##' }
##' dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
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
                                    overall_uncertainty = overall_uncertainty,
                                    dist = dist)
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
    } else if (inherits(smooth, "Bspline.smooth")) {
        "B spline"
    } else if (inherits(smooth, "duchon.spline")) {
        "Duchon spline"
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
        stop("Unknown type of smooth")
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
                                    dist = 0.1, ...) {
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

##' @export
##' @importFrom patchwork wrap_plots
`draw.smooth_estimates` <- function(object,
                                    ...) {
    smth_est <- split(object, f = object[["smooth"]])
    plts <- vector(mode = "list", length = length(smth_est))
    for (i in seq_along(smth_est)) {
        plts[[i]] <- draw_smooth_estimates(smth_est[[i]], ...)
    }

    wrap_plots(plts)
}

`draw_smooth_estimates` <- function(object, ...) {
    sm_vars <- vars_from_label(unique(object[["smooth"]]))
    sm_dim <- length(sm_vars)
    sm_type <- unique(object[["type"]])

    if (sm_dim == 1L &&
        sm_type %in% c("TPRS", "TPRS (shrink)", "CRS", "CRS (shrink)",
                       "Cyclic CRS", "P spline", "B spline", "Duchon spline",
                       "GP")) {
        class(object) <- c("mgcv_smooth", class(object))
    } else {
        stop("Unknown type")
    }

    plot_smooth_estimates(object, variables = sm_vars, ...)
}

`plot_smooth_estimates` <- function(object, ...) {
    UseMethod("plot_smooth_estimates")
}

##' @importFrom dplyr mutate
`plot_smooth_estimates.mgcv_smooth` <- function (object,
                                                 variables = NULL,
                                                 rug = NULL,
                                                 ci_level = 0.95,
                                                 alpha = 0.3,
                                                 constant= NULL,
                                                 fun = NULL,
                                                 xlab = NULL,
                                                 ylab = NULL,
                                                 title = NULL,
                                                 subtitle = NULL,
                                                 caption = NULL,
                                                 partial_residuals = NULL,
                                                 ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[["smooth"]]))
    }

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## Add confidence interval
    crit <- qnorm((1 - ci_level) / 2, lower.tail = FALSE)
    object <- mutate(object,
                     upper = .data$est + (crit * .data$se),
                     lower = .data$est - (crit * .data$se))

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    ## base plot
    plt <- ggplot(object, aes_string(x = variables, y = "est"))

    ## do we want partial residuals? Only for univariate smooths without by vars
    if (!is.null(partial_residuals)) {
        plt <- plt + geom_point(data = partial_residuals,
                                aes_string(x = "..orig_x", y = "..p_resid"),
                                inherit.aes = FALSE,
                                colour = "steelblue3", alpha = 0.5)
    }

    ## plot the confidence interval
    plt <- plt +
        geom_ribbon(mapping = aes_string(ymin = "lower", ymax = "upper"),
                    alpha = alpha) +
        geom_line()

    ## default axis labels if none supplied
    if (missing(xlab) || is.null(xlab)) {
        xlab <- variables
    }
    if (missing(ylab) || is.null(ylab)) {
        ylab <- "Effect"
    }
    if (is.null(title)) {
        title <- unique(object[["smooth"]])
    }
    if (all(is.na(object[["by"]]))) {
        spl <- strsplit(title, split = ":")
        title <- spl[[1L]][[1L]]
        if (is.null(subtitle)) {
            by_var <- as.character(unique(object[["by"]]))
            subtitle <- paste0("By: ", by_var, "; ", unique(object[[by_var]]))
        }
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## add rug?
    if (!is.null(rug)) {
        plt <- plt +
            geom_rug(data = data.frame(x = rug), mapping = aes_string(x = 'x'),
                     inherit.aes = FALSE, sides = 'b', alpha = 0.5)
    }
    
    plt
}

`draw2` <- function(object, ...) {
    UseMethod("draw2")
}

`draw2.gam()` <- function(object, 
                       parametric = NULL,
                       select = NULL,
                       residuals = FALSE,
                       scales = c("free", "fixed"),
                       ci_level = 0.95,
                       n = 100,
                       unconditional = FALSE,
                       overall_uncertainty = TRUE,
                       constant = NULL,
                       fun = NULL,
                       dist = 0.1,
                       rug = TRUE,
                       contour = TRUE,
                       contour_col = "black",
                       n_contour = NULL,
                       partial_match = FALSE,
                       discrete_colour = NULL,
                       continuous_colour = NULL,
                       continuous_fill = NULL,
                       ncol = NULL, nrow = NULL,
                       guides = "keep",
                       ...) {
    ## fixed or free?
    scales <- match.arg(scales)

    ## fix up default scales
    if (is.null(discrete_colour)) {
        discrete_colour <- scale_colour_discrete()
    }
    if (is.null(continuous_colour)) {
        continuous_colour <- scale_colour_continuous()
    }
    if (is.null(continuous_fill)) {
        continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
    }

    ## if not using select, set parametric TRUE if not set to FALSE
    if (!is.null(select)) {
        if (is.null(parametric)) {
            parametric <- FALSE
        }
    } else {
        if (is.null(parametric)) {
            parametric <- TRUE
        }
    }
    
    S <- smooths(object)                # vector of smooth labels - "s(x)"

    ## select smooths
    select <- check_user_select_smooths(smooths = S, select = select,
                                        partial_match = partial_match)

    ## evaluate all requested smooths
    sm_eval <- smooth_estimates(object, smooth = S[select], n = n, data = data,
                                unconditional = unconditional,
                                overall_uncertainty = overall_uncertainty,
                                dist = dist)
    ## add confidence interval
    sm_eval <- add_confint(sm_eval, coverage = ci_level)

    ## get the terms predictions if we need partial residuals
    if (isTRUE(residuals)) {
        if (is.null(object$residuals) || is.null(object$weights)) {
            residuals <- FALSE
        } else {
            pred_terms <- predict(object, type = "terms")
            w_resid <- object$residuals * sqrt(object$weights)
            pred_terms <- pred_terms + w_resid
        }
    }
    
    ## need to figure out scales if "fixed"
    ylims <- NULL
    ## crit <- qnorm((1 - ci_level) / 2, lower.tail = FALSE) # not needed now
    if (isTRUE(identical(scales, "fixed"))) {
        ## not needed as we do add_confint above
        ## wrapper <- function(x, var, crit) {
        ##     range(x[[var]] + (crit * x[["se"]]),
        ##           x[[var]] - (crit * x[["se"]]))
        ## }

        if (isTRUE(residuals)) {
            want_presids <- function(x) {
                sm <- get_smooth(object, term = x)
                (! is_by_smooth(sm)) && smooth_dim(sm) == 1L
            }
            take_presids <- vapply(colnames(pred_terms), want_presids, logical(1L))
            p_resids_lims <- if (NCOL(pred_terms[, take_presids]) > 0) {
                range(pred_terms[, take_presids])
            }
        } else {
            p_resids_lims <- rep(0, 2)
        }

        ## this just needs to range est, upper, lower now
        ylims <- range(c(unlist(lapply(l, wrapper, var = "est", crit = crit)),
                         p_resids_lims))
        
        if (isTRUE(parametric)) {
            ## fix this as for the above for smooths once I figure out how to
            ## process the parametric terms in this new version
            ylims <- range(ylims,
                           unlist(lapply(p, wrapper, var = "partial", crit = crit)))
        }
    }
    
}
