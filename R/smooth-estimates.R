#' New `evaluate_smooth()` alike
#'
#' @param object an object of class `"gam"` or `"gamm"`.
#' @param smooth character; a single smooth to evaluate.
#' @param n numeric; the number of points over the range of the covariate at
#'   which to evaluate the smooth.
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
#' \dontshow{
#' op <- options(cli.unicode = FALSE, digits = 6)
#' }
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
                                   data = NULL,
                                   unconditional = FALSE,
                                   overall_uncertainty = TRUE,
                                   dist = 0.1,
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
            stop("'data', if supplied, must be a numeric vector or a data frame.",
                 call. = FALSE)
        }
        check_all_vars(object, data = data, smooths = smooths)
        data <- delete_response(object, data = data)
     }

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
    if (isTRUE(unnest)) {
        sm_list <- unnest(sm_list, all_of('data'))
    }

    ## add a class
    class(sm_list) <- c("smooth_estimates", class(sm_list))

    ## return
    sm_list
}

#' @export
`smooth_estimates.gamm` <- function(object, ...) {
    smooth_estimates(object[["gam"]], ...)
}

#' Determine the type of smooth and return it n a human readble form
#'
#' @param smooth an object inheriting from class `mgcv.smooth`.
#'
#' @keywords internal
#' @noRd
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

    sm_dim <- smooth_dim(smooth)
    if (sm_dim > 1L &&
          sm_type %in% c("TPRS", "TPRS (shrink)", "Duchon spline")) {
        sm_type <- paste0(sm_type, " (", sm_dim, "d)")
    }

    sm_type
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

    ## standard error of the estimate
    se.fit <- sqrt(pmax(0, rs))

    ## identify which vars are needed for this smooth...
    keep_vars <- terms_in_smooth(smooth)
    ## ... then keep only those vars
    data <- select(data, all_of(keep_vars))

    ## Return object
    tbl <- tibble(smooth = rep(label, nrow(X)), est = fit, se = se.fit)
    ## bind on the data
    tbl <- bind_cols(tbl, data)
    ## nest all columns with varying data
    tbl <- nest(tbl, data = all_of(c("est", "se", names(data))))

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
`eval_smooth.mgcv.smooth` <- function(smooth, model, n = 100, data = NULL,
                                      unconditional = FALSE,
                                      overall_uncertainty = TRUE,
                                      ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

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

#' Wrapper to `gratia::smooth_data()` and `gratia:::check_user_data()` for use
#' with [gratia::eval_smooth()] methods
#'
#' @param data an optional data frame of values for the smooth
#' @param model a fitted model
#' @param n numeric; the number of new observations to generate. Passed to
#'   [gratia::smooth_data()].
#' @param id the number ID of the smooth within `model` to process.
#'
#' @keywords internal
#' @noRd
#' @importFrom rlang .data
`process_user_data_for_eval` <- function(data, model, n, id) {
    if (is.null(data)) {
        data <- smooth_data(model = model, n = n, id = id)
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
`eval_smooth.t2.smooth` <- function(smooth, model, n = 100, data = NULL,
                                    unconditional = FALSE,
                                    overall_uncertainty = TRUE,
                                    dist = 0.1, ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

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

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.tensor.smooth` <- function(smooth, model, n = 100, data = NULL,
                                        unconditional = FALSE,
                                        overall_uncertainty = TRUE,
                                        ...) {
    by_var <- by_variable(smooth) # even if not a by as we want NA later
    if (by_var == "NA") {
        by_var <- NA_character_
    }

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

#' @export
#' @importFrom patchwork wrap_plots
`draw.smooth_estimates` <- function(object,
                                    ...) {
    smth_est <- split(object, f = object[["smooth"]])
    plts <- vector(mode = "list", length = length(smth_est))
    for (i in seq_along(smth_est)) {
        ## add on confint
        smth_est[[i]] <- add_confint(smth_est[[i]])
        plts[[i]] <- draw_smooth_estimates(smth_est[[i]], ...)
    }

    wrap_plots(plts)
}

#' @importFrom tidyr unnest
#' @importFrom tidyselect any_of
`draw_smooth_estimates` <- function(object, ...) {
    sm_vars <- vars_from_label(unique(object[["smooth"]]))
    sm_dim <- length(sm_vars)
    sm_type <- unique(object[["type"]])

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
                       "GP")) {
        class(object) <- c("mgcv_smooth", class(object))
    } else if (sm_type == "Random effect") {
        class(object) <- append(class(object),
                                c("random_effect", "mgcv_smooth"),
                                after = 0)
    } else if (sm_type == "Factor smooth") {
        class(object) <- append(class(object),
                                c("factor_smooth", "mgcv_smooth"),
                                after = 0)
    } else if (sm_dim == 2L &&
                sm_type %in% c("TPRS (2d)", "TPRS (shrink) (2d)",
                               "Duchon spline (2d)",
                               "Tensor", "Tensor (T2)")) {
        class(object) <- append(class(object),
                                c("bivariate_smooth", "mgcv_smooth"),
                                after = 0)
    } else {
        stop("Unknown type")
    }

    plot_smooth(object, variables = sm_vars, rug = rug_data,
                partial_residuals = p_residuals, ...)
}

`plot_smooth` <- function(object, ...) {
    UseMethod("plot_smooth")
}

#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_point aes_ aes_string geom_rug geom_abline
#'   expand_limits labs geom_line geom_ribbon
#' @keywords internal
#' @noRd
`plot_smooth.mgcv_smooth` <- function (object,
                                       variables = NULL,
                                       rug = NULL,
                                       ci_level = 0.95,
                                       alpha = 0.3,
                                       constant = NULL,
                                       fun = NULL,
                                       xlab = NULL,
                                       ylab = NULL,
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = NULL,
                                       partial_residuals = NULL,
                                       ylim = NULL,
                                       ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[["smooth"]]))
    }

    # If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    # If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    # base plot - need as.name to handle none standard names, like log2(x)
    plt <- ggplot(object, aes_(x = as.name(variables), y =  ~ est))

    # do we want partial residuals? Only for univariate smooths without by vars
    if (!is.null(partial_residuals)) {
        plt <- plt + geom_point(data = partial_residuals,
                                aes_(x = as.name(variables),
                                     y = ~partial_residual),
                                inherit.aes = FALSE,
                                colour = "steelblue3", alpha = 0.5)
    }

    # plot the confidence interval
    plt <- plt +
        geom_ribbon(mapping = aes_string(ymin = "lower_ci", ymax = "upper_ci"),
                    alpha = alpha) +
        geom_line()

    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- variables
    }
    if (is.null(ylab)) {
        ylab <- "Effect"
    }
    if (is.null(title)) {
        title <- unique(object[["smooth"]])
    }
    if (all(!is.na(object[["by"]]))) {
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
            geom_rug(data = rug,
                     mapping = aes_(x = as.name(variables)),
                     inherit.aes = FALSE, sides = 'b', alpha = 0.5)
    }

    # fix the yaxis limits?
    if (!is.null(ylim)) {
        plt <- plt + expand_limits(y = ylim)
    }

    plt
}

#' @importFrom ggplot2 ggplot geom_point geom_raster geom_contour aes_
#'   expand_limits labs guides guide_colourbar theme
#' @importFrom grid unit
#' @keywords internal
#' @noRd
`plot_smooth.bivariate_smooth` <- function(object,
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
                                           ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[["smooth"]]))
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
        guide_title <- "Effect"
        plot_var <- "est"
        guide_limits <- if (is.null(ylim)) {
            c(-1, 1) * max(abs(object[[plot_var]]))
        } else {
            ylim
        }
    } else {
        guide_title <- "Std. err."
        plot_var <- "se"
        guide_limits <- range(object[["se"]])
    }

    plt <- ggplot(object, aes_(x = as.name(variables[1]),
                               y = as.name(variables[2]))) +
        geom_raster(mapping = aes_(fill = as.name(plot_var)))

    if (isTRUE(contour)) {
        plt <- plt + geom_contour(mapping = aes_(z = as.name(plot_var)),
                                  colour = contour_col,
                                  bins = n_contour)
    }

    ## default axis labels if none supplied
    if (is.null(xlab)) {
        xlab <- variables[1L]
    }
    if (is.null(ylab)) {
        ylab <- variables[2L]
    }
    if (is.null(title)) {
        title <- unique(object[["smooth"]])
    }

    if (all(!is.na(object[["by"]]))) {
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

    ## Set the palette
    plt <- plt + continuous_fill

    ## Set the limits for the fill
    plt <- plt + expand_limits(fill = guide_limits)

    ## add guide
    plt <- plt +
        guides(fill = guide_colourbar(title = guide_title,
                                      direction = "vertical",
                                      barheight = grid::unit(0.25, "npc")))

    ## position legend at the
    plt <- plt + theme(legend.position = "right")

    ## add rug?
    if (!is.null(rug)) {
        plt <- plt +
          geom_point(data = rug,
                     mapping = aes_(x = as.name(variables[1]),
                                    y = as.name(variables[2])),
                     inherit.aes = FALSE, alpha = 0.1)
    }

    plt
}

#' @importFrom ggplot2 ggplot geom_point aes_string geom_abline expand_limits
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
                                        ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[["smooth"]]))
    }

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    ## base plot with computed QQs
    plt <- ggplot(object, aes_string(sample = "est")) +
        geom_point(stat = "qq")

    ## add a QQ reference line
    if (isTRUE(qq_line)) {
        sampq <- quantile(object[["est"]], c(0.25, 0.75))
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
        ylab <- "Effects"
    }
    if(is.null(title)) {
        title <- variables
    }
    if (all(!is.na(object[["by"]]))) {
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

    ## fixing the y axis limits?
    if (!is.null(ylim)) {
        plt <- plt + expand_limits(y = ylim)
    }

    plt
}

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
                                       ...) {
    if (is.null(variables)) {
        variables <- vars_from_label(unique(object[["smooth"]]))
    }

    if (is.null(discrete_colour)) {
        discrete_colour <- scale_colour_discrete()
    }

    #smooth_var <- names(object)[3L]
    #smooth_fac <- names(object)[4L]

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    plt <- ggplot(object, aes_(x = as.name(variables[1]), y = ~ est,
                               colour = as.name(variables[2]))) +
        geom_line() +
        discrete_colour +
        theme(legend.position = "none")

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- variables[1]
    }
    if (missing(ylab)) {
        ylab <- "Effect"
    }
    if (is.null(title)) {
        title <- unique(object[["smooth"]])
    }
    if (all(!is.na(object[["by"]]))) {
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
        plt <- plt + geom_rug(data = rug,
                              mapping = aes_(x = as.name(variables[1])),
                              inherit.aes = FALSE,
                              sides = 'b', alpha = 0.5)
    }

    ## fixing the y axis limits?
    if (!is.null(ylim)) {
        plt <- plt + expand_limits(y = ylim)
    }

    plt
}