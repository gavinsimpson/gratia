#' Evaluate a smooth
#'
#' @description
#' `r lifecycle::badge('deprecated')` Evaluate a smooth at a grid of evenly
#' spaced value over the range of the covariate associated with the smooth.
#' Alternatively, a set of points at which the smooth should be evaluated can be
#' supplied.
#'
#' @details
#' `r lifecycle::badge('deprecated')` `evaluate_smooth()` is deprecated in
#' favour of [smooth_estimates()], which provides a cleaner way to evaluate a
#' smooth over a range of covariate values. [smooth_estimates()] can handle a
#' much wider range of models than `evaluate_smooth()` is capable of and
#' [smooth_estimates()] is much easier to extend to handle new smooth types.
#' 
#' Most code that uses `evaluate_smooth()` should work simply by changing the
#' function call to [smooth_estimates()]. However, there are some differences:
#' 
#' * the `newdata` argument becomes `data`
#' 
#' Consider `evaluate_smooth()` to be *soft*-deprecated; its use is discouraged
#' and it may be removed at a later date if it becomes difficult to maintain
#' the current functionality, but there are no intentions of removing it from
#' gratia unless that situation arises.
#'
#' @param object an object of class `"gam"` or `"gamm"`.
#' @param smooth character; a single smooth to evaluate.
#' @param n numeric; the number of points over the range of the covariate at
#'   which to evaluate the smooth.
#' @param newdata a vector or data frame of points at which to evaluate the
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
#' @param ... arguments passed to other methods.
#'
#' @return A data frame, which is of class `"evaluated_1d_smooth"` or
#'   `evaluated_2d_smooth`, which inherit from classes `"evaluated_smooth"`
#'   and `"data.frame"`.
#'
#' @importFrom mgcv PredictMat exclude.too.far
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 2)
#' }
#' dat <- data_sim("eg1", n = 500, dist = "normal", scale = 1, seed = 2)
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' evaluate_smooth(m1, "s(x1)")
#'
#' ## 2d example
#' dat <- data_sim("eg2", n = 1000, dist = "normal", scale = 1, seed = 2)
#' m2 <- gam(y ~ s(x, z, k = 30), data = dat, method = "REML")
#'
#' evaluate_smooth(m2, "s(x,z)", n = 50)
#' \dontshow{options(op)}
`evaluate_smooth` <- function(object, ...) {
    lifecycle::deprecate_warn("0.7.0",
                              "evaluate_smooth()",
                              "smooth_estimates()")
    UseMethod("evaluate_smooth")
}

#' @export
#' @rdname evaluate_smooth
`evaluate_smooth.gam` <- function(object, smooth, n = 100, newdata = NULL,
                                  unconditional = FALSE,
                                  overall_uncertainty = TRUE,
                                  dist = 0.1, ...) {
    ## to keep this simple, only evaluate a single smooth at a time
    if (length(smooth) > 1L) {
        message("Supplied more than 1 'smooth'; using only the first")
        smooth <- smooth[1L]
    }
    smooth_ids <- old_which_smooth(object, smooth)
    if (identical(length(smooth_ids), 0L)) {
        stop("Requested smooth '", smooth, "' not found", call. = FALSE)
    }
    smooth_labels <- select_smooth(object, smooth)

    ## Need to handle by smooths here
    ## use get_smooth() on each smooth
    ## SMOOTHS <- object[["smooth"]][smooth_ids]  # take matched smooths
    SMOOTHS <- get_smooths_by_id(object, smooth_ids) # extract the mgcv.smooth object

    ## choose how to evaluate the smooth
    if (inherits(SMOOTHS[[1]], "random.effect")) { # FIXME: bs = "re" can also have `by`
        evaluated <- evaluate_re_smooth(SMOOTHS, model = object,
                                        newdata = newdata,
                                        unconditional = unconditional)
    } else if (inherits(SMOOTHS[[1]], "fs.interaction")) {
        evaluated <- evaluate_fs_smooth(SMOOTHS, n = n, model = object,
                                        newdata = newdata,
                                        unconditional = unconditional)
    } else if (smooth_dim(SMOOTHS[[1]]) == 1L) { # if 2d smooth, call separate fun
        evaluated <- evaluate_1d_smooth(SMOOTHS, n = n, model = object,
                                        newdata = newdata,
                                        overall_uncertainty = overall_uncertainty,
                                        unconditional = unconditional)
    } else if (smooth_dim(SMOOTHS[[1]]) == 2L) {
        evaluated <- evaluate_2d_smooth(SMOOTHS, n = n, model = object,
                                        newdata = newdata,
                                        overall_uncertainty = overall_uncertainty,
                                        unconditional = unconditional,
                                        dist = dist)
    } else {
        stop("Only univariate and bivariate smooths are currently supported.")
    }

    evaluated
}

#' @export
#' @rdname evaluate_smooth
`evaluate_smooth.gamm` <- function(object, ...) {
    evaluate_smooth(object[["gam"]], ...)
}

#' @export
#' @rdname evaluate_smooth
`evaluate_smooth.list` <- function(object, ...) {
    ## Is this list likely to be a gamm4 list?
    if (! is_gamm4(object)) {
        stop("`object` does not appear to a `gamm4` model object", call. = FALSE)
    }
    evaluate_smooth(object[["gam"]], ...)
}

## Random effect smooth
#' @importFrom tibble add_column tibble
`evaluate_re_smooth` <- function(object, model = NULL, newdata = NULL,
                                 unconditional = FALSE) {
    ## is this a by smooth
    is.by <- vapply(object, FUN = is_by_smooth, FUN.VALUE = logical(1L))
    ## 're' smooths can have multiple terms, by the time we're here the
    ## fun should just work and we don't need this check
    ## if (length(object) > 1L) {
    ##     if (!all(is.by)) {
    ##         vars <- vapply(object, smooth_variable, character(1L))
    ##         if (length(unique(vars)) > 1L) {
    ##             stop(by_smooth_failure(object))
    ##         }
    ##     }
    ## }

    ## get by variable info
    by_var <- unique(vapply(object, FUN = by_variable, FUN.VALUE = character(1)))

    if (!is.null(newdata)) {
        stop("Not yet implemented: user-supplied data in 're' smooth")
    }

    ## get variable for this smooth
    smooth_var    <- unique(unlist(lapply(object, FUN = smooth_variable)))
    smooth_labels <- vapply(object, FUN = smooth_label, FUN.VALUE = character(1))
    ## need `drop = FALSE` (the default) here because by default mgcv doesn't
    ## drop the unused levels; hence we get a coef for all combinations of
    ## vars in the ranef smooth
    var_types <- attr(terms(model), "dataClasses")[smooth_var]
    levs <- if (all(var_types %in% c("factor", "ordered"))) {
        levels(interaction(model[["model"]][smooth_var], drop = FALSE))
    } else {
        take <- smooth_var[which(var_types %in% c("factor", "ordered"))]
        levels(interaction(model[["model"]][take]))
    }

    ## if we have a by variable
    is.factor.by <- vapply(object, FUN = is_factor_by_smooth,
                           FUN.VALUE = logical(1L))

    evaluated <- vector("list", length(object))
    for (i in seq_along(evaluated)) {
        start <- object[[i]][["first.para"]]
        end   <- object[[i]][["last.para"]]
        para_seq <- seq(from = start, to = end, by = 1L)
        coefs <- coef(model)[para_seq]
        se <- diag(vcov(model, unconditional = unconditional))[para_seq]
        evaluated[[i]] <- tibble(smooth = rep(smooth_labels[i], length(coefs)),
                                 ..var  = levs,
                                 est = coefs,
                                 se = se)
    }

    evaluated <- do.call("rbind", evaluated)

    if (any(is.factor.by)) {
        na_by <- by_var == "NA"         # a non-by global smooth is "NA"
        evaluated <- add_by_var_info_to_smooth(evaluated,
                                               by_name = by_var,
                                               by_data = model[["model"]][[by_var[!na_by]]],
                                               n = length(levs))
    } else {
        evaluated <- add_missing_by_info_to_smooth(evaluated)
    }

    names(evaluated)[3] <- paste(smooth_var, collapse = ", ")
    class(evaluated) <- c("evaluated_re_smooth", "evaluated_smooth", class(evaluated))

    evaluated
}

#' @importFrom tibble add_column
#' @importFrom dplyr bind_rows
`evaluate_1d_smooth` <- function(object, n = NULL, model = NULL, newdata = NULL,
                                 unconditional = FALSE,
                                 overall_uncertainty = TRUE) {
    ## If more than one smooth, these should be by variables smooths
    ## of a global plus by variable smooth
    is.by <- vapply(object, FUN = is_by_smooth, FUN.VALUE = logical(1L))
    if (length(object) > 1L) {
        if (!all(is.by)) {
            vars <- vapply(object, smooth_variable, character(1L))
            if (length(unique(vars)) > 1L) {
                stop(by_smooth_failure(object))
            }
        }
    }

    ## get by variable info
    by_var <- unique(vapply(object, FUN = by_variable, FUN.VALUE = character(1)))

    ## get variable for this smooth
    smooth_var <- unique(vapply(object, FUN = smooth_variable,
                                FUN.VALUE = character(1)))

    newx <- if (is.null(newdata)) {
                setNames(datagen(object[[1]], n = n,
                                 data = model[["model"]])[, "x", drop = FALSE],
                         smooth_var)
    } else if (is.data.frame(newdata)) { # data frame; select out smooth
        if (!smooth_var %in% names(newdata)) {
            stop(paste("Variable", smooth_var, "not found in 'newdata'."))
        }
        newdata[, smooth_var, drop = FALSE]
    } else if (is.numeric(newdata)) {   # vector; coerce to data frame
        setNames(data.frame(newdata), smooth_var)
    } else {                            # object we can't handle; bail out
        stop("'newdata', if supplied, must be a numeric vector or a data frame.")
    }

    ## if we have a by variable, repeat newx for each level of that variable
    is.factor.by     <- vapply(object, FUN = is_factor_by_smooth,     FUN.VALUE = logical(1L))
    is.continuous.by <- vapply(object, FUN = is_continuous_by_smooth, FUN.VALUE = logical(1L))
    if (any(is.by)) {
        na_by <- by_var == "NA"         # a non-by global smooth is "NA"
        if (any(is.factor.by)) {
            if (any(na_by)) { # if we have a global by need to add some NAs for factor
                onewx <- cbind(newx, .by_var = NA)
            }
            ## repeat levels of factor, all has to be done excluding the global smooth
            ## if present
            levs <- levels(model[["model"]][[by_var[!na_by]]])
            newx <- cbind(newx, .by_var = rep(levs, each = n))
            if (any(na_by)) {
                levs <- c("NA", levs) # extend levels if a global smoother (for later)...
                newx <- rbind(onewx, newx)
                ## ...but convet to factor ignoring this extra level
                newx[[".by_var"]] <- factor(newx[[".by_var"]], levels = levs[-1L])
            } else {
                newx[[".by_var"]] <- factor(newx[[".by_var"]], levels = levs)
            }
        } else {                        # continuous by
            if (any(na_by)) {
                onewx <- cbind(newx, .ba_var = NA)
            }
            newx <- cbind(newx, .by_var = mean(model[["model"]][[by_var[!na_by]]]))
            if (any(na_by)) {
                newx <- rbind(onewx, newx)
            }
        }
        names(newx)[NCOL(newx)] <- by_var[!na_by]
    }

    evaluated <- vector("list", length(object))
    for (i in seq_along(evaluated)) {
        ind <- seq_len(NROW(newx))
        if (any(is.by)) { # need to differentiate between global and factor by smooths
            if (is.factor.by[[i]]) {
                ind <- newx[, by_var[!na_by]] == levs[i]
                ind[is.na(ind)] <- FALSE
            } else {                    # continous by or a global smooth
                is_na <- is.na(newx[, by_var[!na_by]])
                if (any(is_na)) {       # a global smooth
                    ind <- is_na
                }
            }
        }
        evaluated[[i]] <- spline_values(object[[i]],
                                        newdata = newx[ind, , drop = FALSE],
                                        unconditional = unconditional,
                                        model = model,
                                        overall_uncertainty = overall_uncertainty,
                                        term = smooth_var)
    }

    evaluated <- do.call("bind_rows", evaluated)

    if (any(is.factor.by)) {
        evaluated <- add_by_var_info_to_smooth(evaluated,
                                               by_name = by_var,
                                               by_data = model[["model"]][[by_var[!na_by]]],
                                               n = n)
    } else {
        evaluated <- add_missing_by_info_to_smooth(evaluated)
    }

    names(evaluated)[3] <- smooth_var
    class(evaluated) <- c("evaluated_1d_smooth", "evaluated_smooth", class(evaluated))

    evaluated
}

#' @importFrom tibble add_column
#' @importFrom mgcv exclude.too.far
`evaluate_2d_smooth` <- function(object, n = NULL, model = NULL, newdata = NULL,
                                 unconditional = FALSE,
                                 overall_uncertainty = TRUE, dist = 0.1) {
    ## If more than one smooth, these should be by variables smooths
    ## of a global plus by variable smooth
    is.by <- vapply(object, FUN = is_by_smooth, FUN.VALUE = logical(1L))
    if (length(object) > 1L) {
        if (!all(is.by)) {
            vars <- vapply(object, smooth_variable, character(2L))
            if (length(unique(as.vector(vars))) > 2L) {
                stop(by_smooth_failure(object))
            }
        }
    }

    ## get by variable info
    by_var <- unique(vapply(object, FUN = by_variable, FUN.VALUE = character(1)))

    ## get variables for this smooth
    smooth_var <- unique(vapply(object, FUN = smooth_variable, FUN.VALUE = character(2L))[,1])

    newx <- if (is.null(newdata)) {
                setNames(datagen(object[[1]], n = n,
                                 data = model[["model"]])[, c("x1", "x2"), drop = FALSE],
                         smooth_var)
    } else if (is.data.frame(newdata)) { # data frame; select out smooth
        if (!all(smooth_var %in% names(newdata))) {
            stop(paste("Variable", smooth_var, "not found in 'newdata'."))
        }
        newdata[, smooth_var, drop = FALSE]
    } else if (is.numeric(newdata)) {   # vector; coerce to data frame
        setNames(data.frame(newdata), smooth_var)
    } else {                            # object we can't handle; bail out
        stop("'newdata', if supplied, must be a numeric vector or a data frame.")
    }

    ## if we have a by variable, repeat newx for each level of that variable
    is.factor.by     <- vapply(object, FUN = is_factor_by_smooth,     FUN.VALUE = logical(1L))
    is.continuous.by <- vapply(object, FUN = is_continuous_by_smooth, FUN.VALUE = logical(1L))
    if (any(is.by)) {
        na_by <- by_var == "NA"         # a non-by global smooth is "NA"
        if (any(is.factor.by)) {
            if (any(na_by)) { # if we have a global by need to add some NAs for factor
                onewx <- cbind(newx, .by_var = NA)
            }
            ## repeat levels of factor, all has to be done excluding the global smooth
            ## if present
            levs <- levels(model[["model"]][[by_var[!na_by]]])
            newx <- cbind(newx, .by_var = rep(levs, each = n*n))
            if (any(na_by)) {
                levs <- c("NA", levs) # extend levels if a global smoother (for later)...
                newx <- rbind(onewx, newx)
                ## ...but convet to factor ignoring this extra level
                newx[[".by_var"]] <- factor(newx[[".by_var"]], levels = levs[-1L])
            } else {
                newx[[".by_var"]] <- factor(newx[[".by_var"]], levels = levs)
            }
        } else {                        # continuous by
            if (any(na_by)) {
                onewx <- cbind(newx, .ba_var = NA)
            }
            newx <- cbind(newx, .by_var = mean(model[["model"]][[by_var[!na_by]]]))
            if (any(na_by)) {
                newx <- rbind(onewx, newx)
            }
        }
        names(newx)[NCOL(newx)] <- by_var[!na_by]
    }

    evaluated <- vector("list", length(object))
    for (i in seq_along(evaluated)) {
        ind <- seq_len(NROW(newx))
        if (any(is.by)) { # need to differentiate between global and factor by smooths
            if (is.factor.by[[i]]) {
                ind <- newx[, by_var[!na_by]] == levs[i]
                ind[is.na(ind)] <- FALSE
            } else {                    # continous by or a global smooth
                is_na <- is.na(newx[, by_var[!na_by]])
                if (any(is_na)) {       # a global smooth
                    ind <- is_na
                }
            }
        }
        evaluated[[i]] <- spline_values(object[[i]],
                                        newdata = newx[ind, , drop = FALSE],
                                        unconditional = unconditional,
                                        model = model,
                                        overall_uncertainty = overall_uncertainty,
                                        term = smooth_var)
    }

    evaluated <- do.call("rbind", evaluated)

    if (any(is.factor.by)) {
        evaluated <- add_by_var_info_to_smooth(evaluated,
                                               by_name = by_var,
                                               by_data = model[["model"]][[by_var[!na_by]]],
                                               n = n*n)
    } else {
        evaluated <- add_missing_by_info_to_smooth(evaluated)
    }

    ## exclude values too far from data
    if (dist > 0) {
        ind <- mgcv::exclude.too.far(newx[, smooth_var[1L]],
                               newx[, smooth_var[2L]],
                               model[["model"]][, smooth_var[1L]],
                               model[["model"]][, smooth_var[2L]],
                               dist = dist)
        if (any(ind)) {
            evaluated[ind, c("est", "se")] <- NA
        }
    }

    names(evaluated)[3:4] <- smooth_var
    class(evaluated) <- c("evaluated_2d_smooth", "evaluated_smooth", class(evaluated))

    ## return
    evaluated
}

#' @importFrom tibble add_column
`evaluate_fs_smooth` <- function(object, n = NULL, model = NULL, newdata = NULL,
                                 unconditional = FALSE,
                                 overall_uncertainty = TRUE) {
    ## If more than one smooth, these should be by variables smooths
    ## of a global plus by variable smooth
    is.by <- vapply(object, FUN = is_by_smooth, FUN.VALUE = logical(1L))
    if (length(object) > 1L) {
        if (!all(is.by)) {
            vars <- vapply(object, smooth_variable, character(1L))
            if (length(unique(vars)) > 1L) {
                stop(by_smooth_failure(object))
            }
        }
    }

    ## get by variable info
    by_var <- unique(vapply(object, FUN = by_variable, FUN.VALUE = character(1)))

    ## get variable for this smooth
    smooth_var <- unique(vapply(object, FUN = smooth_variable, FUN.VALUE = character(2L)))
    smooth_fac <- unique(vapply(object, FUN = smooth_factor_variable,
                                FUN.VALUE = character(1L)))
    smooth_var <- smooth_var[smooth_var != smooth_fac]

    newx <- if (is.null(newdata)) {
        ## no need to setNames here as we know these are right from datagen
        datagen(object[[1]], n = n, data = model[["model"]])[, c(smooth_var, smooth_fac),
                                                             drop = FALSE]
    } else if (is.data.frame(newdata)) { # data frame; select out smooth
        if (!smooth_var %in% names(newdata)) {
            stop(paste("Variable", smooth_var, "not found in 'newdata'."))
        }
        newdata[, c(smooth_var, smooth_fac), drop = FALSE]
    } else {                            # object we can't handle; bail out
        stop("'newdata', if supplied, must be a data frame.")
    }

    ## n is now n * nlevels(factor)
    n <- nrow(newx)

    ## if we have a by variable, repeat newx for each level of that variable
    is.factor.by     <- vapply(object, FUN = is_factor_by_smooth,     FUN.VALUE = logical(1L))
    is.continuous.by <- vapply(object, FUN = is_continuous_by_smooth, FUN.VALUE = logical(1L))
    if (any(is.by)) {
        na_by <- by_var == "NA"         # a non-by global smooth is "NA"
        if (any(is.factor.by)) {
            if (any(na_by)) { # if we have a global by need to add some NAs for factor
                onewx <- cbind(newx, .by_var = NA)
            }
            ## repeat levels of factor, all has to be done excluding the global smooth
            ## if present
            levs <- levels(model[["model"]][[by_var[!na_by]]])
            newx <- cbind(newx, .by_var = rep(levs, each = n))
            if (any(na_by)) {
                levs <- c("NA", levs) # extend levels if a global smoother (for later)...
                newx <- rbind(onewx, newx)
                ## ...but convet to factor ignoring this extra level
                newx[[".by_var"]] <- factor(newx[[".by_var"]], levels = levs[-1L])
            } else {
                newx[[".by_var"]] <- factor(newx[[".by_var"]], levels = levs)
            }
        } else {                        # continuous by
            if (any(na_by)) {
                onewx <- cbind(newx, .ba_var = NA)
            }
            newx <- cbind(newx, .by_var = mean(model[["model"]][[by_var[!na_by]]]))
            if (any(na_by)) {
                newx <- rbind(onewx, newx)
            }
        }
        names(newx)[NCOL(newx)] <- by_var[!na_by]
    }

    evaluated <- vector("list", length(object))
    for (i in seq_along(evaluated)) {
        ind <- seq_len(NROW(newx))
        if (any(is.by)) { # need to differentiate between global and factor by smooths
            if (is.factor.by[[i]]) {
                ind <- newx[, by_var[!na_by]] == levs[i]
                ind[is.na(ind)] <- FALSE
            } else {                    # continous by or a global smooth
                is_na <- is.na(newx[, by_var[!na_by]])
                if (any(is_na)) {       # a global smooth
                    ind <- is_na
                }
            }
        }
        evaluated[[i]] <- spline_values(object[[i]],
                                        newdata = newx[ind, , drop = FALSE],
                                        unconditional = unconditional,
                                        model = model,
                                        overall_uncertainty = overall_uncertainty,
                                        term = smooth_var)
    }

    evaluated <- do.call("rbind", evaluated)

    if (any(is.factor.by)) {
        evaluated <- add_by_var_info_to_smooth(evaluated,
                                               by_name = by_var,
                                               by_data = model[["model"]][[by_var[!na_by]]],
                                               n = n)
    } else {
        evaluated <- add_missing_by_info_to_smooth(evaluated)
    }

    names(evaluated)[3L] <- smooth_var
    names(evaluated)[4L] <- smooth_fac
    class(evaluated) <- c("evaluated_fs_smooth", "evaluated_smooth",
                          class(evaluated))

    evaluated
}

#' Evaluate parametric model terms
#' 
#' @description
#' `r lifecycle::badge('deprecated')` Returns values of parametric model terms
#' at values of factor terms and over a grid of covariate values for linear
#' parametric terms. This function is now deprecated in favour of
#' [parametric_effects()].
#'
#' @export
`evaluate_parametric_term` <- function(object, ...) {
    UseMethod("evaluate_parametric_term")
}

#' @param term character; which parametric term whose effects are evaluated
#'
#' @inheritParams evaluate_smooth
#' @rdname evaluate_parametric_term
#'
#' @importFrom stats delete.response
#' @importFrom tibble as_tibble add_column
#' @importFrom rlang .data
#' @importFrom dplyr mutate bind_cols bind_rows
#'
#' @export
`evaluate_parametric_term.gam` <- function(object, term, unconditional = FALSE,
                                           ...) {
    tt <- object$pterms       # get parametric terms
    tt <- delete.response(tt) # remove response so easier to work with
    vars <- parametric_terms(object)
    mgcv_names <- names(vars) # this is how mgcv refers to the terms

    if (length(term) > 1L) {
        term <- term[1L]
        warning(sprintf("More than one `term` requested; using the first <%s>",
                        term))
    }
    if (isFALSE(term %in% vars)) {
        stop(sprintf("Term is not in the parametric part of model: <%s>",
                     term))
    }

    mf <- model.frame(object)  # data used to fit model
    
    ## is_fac <- is.factor(mf[[term]]) # is term a factor?
    is_fac <- is_factor_term(tt, term)

    ## match the specific term, with term names mgcv actually uses
    ## for example in a model with multiple linear predictors, terms in
    ## nth linear predictor (for n > 1) get appended .{n-1}  
    ind <- match(term, vars)
    
    if (is_fac) {
        ## check order of term; if > 1 interaction and not handled
        ord <- attr(tt, "order")[match(term, attr(tt, "term.labels"))]
        if (ord > 1) {
            stop("Interaction terms are not currently supported.")
        }
        ## facs <- attr(tt, 'factors')[, term]
        newd <- unique(mf[, term, drop = FALSE])
        ## ##fac_vars <- rownames(facs)
        ## fac_vars <- names(facs)[as.logical(facs)]
        ## facs <- attr(tt, 'factors')[, term]
        ## newd <- unique(mf[, names(facs)[as.logical(facs)], drop = FALSE])
        ## ##fac_vars <- rownames(facs)
        ## fac_vars <- names(facs)[as.logical(facs)]
        ## ##newd <- unique(mf[, fac_vars, drop = FALSE])
        other_vars <- setdiff(names(mf), term)
        other_data <- as_tibble(lapply(mf[other_vars], value_closest_to_median))
        pred_data <- exec(expand_grid, !!!list(newd, other_data))
        evaluated <- as.data.frame(predict(object, newdata = pred_data,
                             type = 'terms',
                             terms = term, se = TRUE,
                             unconditional = unconditional,
                             newdata.guaranteed = FALSE))
        evaluated <- setNames(evaluated, c("partial", "se"))
        evaluated <- as_tibble(evaluated)
        nr <- NROW(evaluated)
        newd <- setNames(newd, "value")
        evaluated <- bind_cols(term = rep(term, nr),
                               type = rep("factor", nr),
                               newd, evaluated)
    } else {
        ## take the actual mgcv version of the names for the `terms` argument
        evaluated <- as.data.frame(predict(object, newdata = mf, type = 'terms',
                                           terms = mgcv_names[ind], se = TRUE,
                                           unconditional = unconditional))
        evaluated <- setNames(evaluated, c("partial", "se"))
        evaluated <- as_tibble(evaluated)
        nr <- NROW(evaluated)
        evaluated <- bind_cols(term = rep(term, nr),
                               type = rep("numeric", nr),
                               value = mf[[term]],
                               evaluated)
    }

    ## add confidence interval -- be consistent and don't add this, but could?
    ## evaluated <- mutate(evaluated,
    ##                     upper = .data$partial + (2 * .data$se),
    ##                     lower = .data$partial - (2 * .data$se))

    class(evaluated) <- c("evaluated_parametric_term", class(evaluated))
    evaluated                           # return
}

## loop over smooths and predict
#' @importFrom tibble tibble
`spline_values` <- function(smooth, newdata, model, unconditional,
                            overall_uncertainty = TRUE, term) {
    X <- PredictMat(smooth, newdata)   # prediction matrix
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
    ## Return
    out <- if (d == 1L) {
               if (is_fs_smooth(smooth)) {
                   tibble(smooth = rep(label, nrow(X)),
                          x = newdata[, 1L], f = newdata[, 2L],
                          est = fit, se = se.fit)
               } else {
                   tibble(smooth = rep(label, nrow(X)),
                          x = newdata[, 1L],
                          est = fit, se = se.fit)
               }
           } else {
               tibble(smooth = rep(label, nrow(X)),
                      x1 = newdata[, 1L], x2 = newdata[, 2L],
                      est = fit, se = se.fit)
           }
    out
}
