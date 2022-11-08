## smooth_terms should be removed
`smooth_terms` <- function(obj, ...) {
    UseMethod("smooth_terms")
}

`smooth_terms.gam` <- function(object, ...) {
    lapply(object[["smooth"]], `[[`, "term")
}

`smooth_terms.gamm` <- function(object, ...) {
    smooth_terms(object[["gam"]], ...)
}

`smooth_terms.mgcv.smooth` <- function(object, ...) {
    object[["term"]]
}

`smooth_terms.fs.interaction` <- function(object, ...) {
    object[["term"]]
}

#' Dimension of a smooth
#'
#' Extracts the dimension of an estimated smooth.
#'
#' This is a generic function with methods for objects of class
#'   `"gam"`, `"gamm"`, and `"mgcv.smooth"`.
##
#' @param object an R object. See Details for list of supported objects.
#'
#' @return A numeric vector of dimensions for each smooth.
#'
#' @author Gavin L. Simpson
#'
#' @rdname smooth_dim
#' @export
`smooth_dim` <- function(object) {
    UseMethod("smooth_dim")
}

#' @rdname smooth_dim
#' @export
`smooth_dim.gam` <- function(object) {
    vapply(object[["smooth"]], FUN = `[[`, FUN.VALUE = integer(1), "dim")
}

#' @rdname smooth_dim
#' @export
`smooth_dim.gamm` <- function(object) {
    smooth_dim(object[["gam"]])
}

#' @rdname smooth_dim
#' @export
`smooth_dim.mgcv.smooth` <- function(object) {
    object[["dim"]]
}

`select_terms` <- function(object, terms) {
    TERMS <- unlist(smooth_terms(object))
    terms <- if (missing(terms)) {
                 TERMS
             } else {
                 want <- terms %in% TERMS
                 if (any(!want)) {
                     msg <- paste("Terms:",
                                  paste(terms[!want], collapse = ", "),
                                  "not found in `object`")
                     message(msg)
                 }
                 terms[want]
             }
    terms
}

`select_smooth` <- function(object, smooth) {
    SMOOTHS <- smooths(object)
    if (missing(smooth)) {
        stop("'smooth' must be supplied; eg. `smooth = 's(x2)'`")
    }
    if (length(smooth) > 1L) {
        message(paste("Multiple smooths supplied. Using only first:", smooth[1]))
        smooth <- smooth[1]
    }
    want <- grep(smooth, SMOOTHS, fixed = TRUE)
    SMOOTHS[want]
}

#' Names of smooths in a GAM
#'
#' @param object a fitted GAM or related model. Typically the result of a call
#'   to [mgcv::gam()], [mgcv::bam()], or [mgcv::gamm()].
#'
#' @export
`smooths` <- function(object) {
    vapply(object[["smooth"]], FUN  = `[[`, FUN.VALUE = character(1), "label")
}

`smooth_variable` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    smooth[["term"]]
}

`smooth_factor_variable` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    smooth[["fterm"]]
}

`smooth_label` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    smooth[["label"]]
}

#' @title Check if objects are smooths or are a particular type of smooth
#'
#' @param smooth an R object, typically a list
#'
#' @details Check if a smooth inherits from class `"mgcv.smooth"`.
#'   `stop_if_not_mgcv_smooth()` is a wrapper around `is_mgcv_smooth()`, useful
#'   when programming for checking if the supplied object is one of mgcv's
#'   smooths, and throwing a consistent error if not.
#'   `check_is_mgcv_smooth()` is similar to `stop_if_not_mgcv_smooth()` but
#'   returns the result of `is_mgcv_smooth()` invisibly.
#'
#' @export
#' @rdname is_mgcv_smooth
`is_mgcv_smooth` <- function(smooth) {
    inherits(smooth, "mgcv.smooth")
}

#' @export
#' @rdname is_mgcv_smooth
stop_if_not_mgcv_smooth <- function(smooth) {
    out <- is_mgcv_smooth(smooth)
    if (!out) {
        stop("'smooth' is not an 'mgcv.smooth'.")
    }
}

#' @export
#' @rdname is_mgcv_smooth
`check_is_mgcv_smooth` <- function(smooth) {
    out <- is_mgcv_smooth(smooth)
    if (identical(out, FALSE)) {
        stop("'smooth' is not an 'mgcv.smooth'")
    }
    invisible(out)
}

#' @export
#' @rdname is_mgcv_smooth
`is_mrf_smooth` <- function(smooth) {
  inherits(smooth, what = "mrf.smooth")
}

`is.gamm` <- function(object) {
    inherits(object, "gamm")
}

`is.gamm4` <- function(object) {
    is.list(object) & (!is.null(object[["gam"]]))
}

`is.gam` <- function(object) {
    inherits(object, "gam")
}

`is.bam` <- function(object) {
    inherits(object, "bam")
}

#' @title Extract an mgcv smooth by name
#'
#' @param object a fitted GAM model object.
#' @param term character; the name of a smooth term to extract
#'
#' @return A single smooth object, or a list of smooths if several match the
#'   named term.
#'
#' @export
`get_smooth` <- function(object, term) {
    if (is.gamm(object) || is.gamm4(object)) {
        object <- object[["gam"]]
    }
    smooth <- object[["smooth"]][which_smooth(object, term)]
    if (identical(length(smooth), 1L)) {
        smooth <- smooth[[1L]]
    }
    smooth
}
`old_get_smooth` <- function(object, term) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    smooth <- object[["smooth"]][old_which_smooth(object, term)]
    if (identical(length(smooth), 1L)) {
        smooth <- smooth[[1L]]
    }
    smooth
}

#' @title Extract an mgcv smooth given its position in the model object
#'
#' @param object a fitted GAM model object.
#' @param id numeric; the position of the smooth in the model object.
#'
#' @export
`get_smooths_by_id` <- function(object, id) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    object[["smooth"]][id]
}

#' @title Extract an factor-by smooth by name
#'
#' @param object a fitted GAM model object.
#' @param term character; the name of a smooth term to extract.
#' @param level character; which level of the factor to exrtact the smooth
#'   for.
#'
#' @return A single smooth object, or a list of smooths if several match the
#'   named term.
#'
#' @export
`get_by_smooth` <- function(object, term, level) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }

    ## which smooth match the term?
    take <- old_which_smooth(object, term)
    S <- object[["smooth"]][take]

    ## if there are multiple, then suggests a factor by smooth
    is_by <- vapply(S, is_factor_by_smooth, logical(1L))

    ## if any are factor by variable smooths, get the levels
    if (any(is_by)) {
        if (missing(level)) {
            stop("No value provided for argument 'level':\n  Getting a factor by-variable smooth requires a 'level' be supplied.")
        }
        level <- as.character(level)    # explicit coerce to character for later comparison
        levs <- vapply(S, `[[`, character(1L), "by.level")
        take <- match(level, levs)
        if (is.na(take)) {
            msg <- paste0("Invalid 'level' for smooth '", term, "'. Possible levels are:\n")
            msg <- paste(msg, paste(strwrap(paste0(shQuote(levs), collapse = ", "),
                                            prefix = " ", initial = ""),
                                    collapse = "\n"))
            stop(msg)
        }

        S <- S[[take]]
    } else {
        stop("The requested smooth '", term, "' is not a by smooth.")
    }

    ## return a single smooth object
    S
}

#' @title Identify a smooth term by its label
#'
#' @param object a fitted GAM.
#' @param terms character; one or more (partial) term labels with which to identify
#'   required smooths.
#' @param ... arguments passed to other methods.
#'
#' @export
`which_smooths` <- function(object, ...) {
    UseMethod("which_smooths")
}

#' @export
#' @rdname which_smooths
`which_smooths.default` <- function(object, ...) {
    stop("Don't know how to identify smooths for <", class(object)[[1L]], ">",
         call. = FALSE)           # don't show the call, simpler error
}

#' @export
#' @rdname which_smooths
`which_smooths.gam` <- function(object, terms, ...) {
    ids <- unique(unlist(lapply(terms, function(x, object) { which_smooth(object, x) },
                                object = object)))
    if (identical(length(ids), 0L)) {
        stop("None of the terms matched a smooth.")
    }

    ids
}

#' @export
#' @rdname which_smooths
`which_smooths.bam` <- function(object, terms, ...) {
    ids <- unique(unlist(lapply(terms, function(x, object) { which_smooth(object, x) },
                                object = object)))
    if (identical(length(ids), 0L)) {
        stop("None of the terms matched a smooth.")
    }

    ids
}

#' @export
#' @rdname which_smooths
`which_smooths.gamm` <- function(object, terms, ...) {
    ids <- unique(unlist(lapply(terms, function(x, object) { which_smooth(object, x) },
                                object = object[["gam"]])))
    if (identical(length(ids), 0L)) {
        stop("None of the terms matched a smooth.")
    }

    ids
}

`which_smooth` <- function(object, term) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    smooths <- smooths(object)
    #grep(term, smooths, fixed = TRUE)
    which(term == smooths)
}

# Needed for evaluate smooth
`old_which_smooth` <- function(object, term) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    smooths <- smooths(object)
    grep(term, smooths, fixed = TRUE)
}

#' How many smooths in a fitted model
#'
#' @inheritParams smooths
#'
#' @export
`n_smooths` <- function(object) {
    UseMethod("n_smooths")
}

#' @export
#' @rdname n_smooths
`n_smooths.default` <- function(object) {
    if (!is.null(object[["smooth"]])) {
        return(length(object[["smooth"]]))
    }

    stop("Don't know how to identify smooths for <", class(object)[[1L]], ">",
         call. = FALSE)           # don't show the call, simpler error
}

#' @export
#' @rdname n_smooths
`n_smooths.gam` <- function(object) {
    length(object[["smooth"]])
}

#' @export
#' @rdname n_smooths
`n_smooths.gamm` <- function(object) {
    length(object[["gam"]][["smooth"]])
}

#' @export
#' @rdname n_smooths
`n_smooths.bam` <- function(object) {
    length(object[["smooth"]])
}

`get_vcov` <- function(object, unconditional = FALSE, frequentist = FALSE,
                       term = NULL, by_level = NULL) {
    V <- if (frequentist) {
        object$Ve
    } else if (unconditional) {
        if (is.null(object$Vc)) {
            warning("Covariance corrected for smoothness uncertainty not available.\nUsing uncorrected covariance.")
            object$Vp     # Bayesian vcov of parameters
        } else {
            object$Vc     # Corrected Bayesian vcov of parameters
        }
    } else {
        object$Vp         # Bayesian vcov of parameters
    }

    ## extract selected term if requested
    if (!is.null(term)) {
        ## to keep this simple, only evaluate a single term
        if (length(term) > 1L) {
            message("Supplied more than 1 'term'; using only the first")
            term <- term[1L]
        }
        term <- select_smooth(object, term)
        smooth <- get_smooth(object, term)
        start <- smooth$first.para
        end <- smooth$last.para
        para.seq <- start:end
        V <- V[para.seq, para.seq, drop = FALSE]
    }
    V
}

`has_s` <- function(terms) {
    grepl("^s\\(.+\\)$", terms)
}

`add_s` <- function(terms) {
    take <- ! has_s(terms)
    terms[take] <- paste("s(", terms[take], ")", sep = "")
    terms
}

`is_re_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    inherits(smooth, "random.effect")
}

`is_fs_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    inherits(smooth, "fs.interaction")
}

#' Fix the names of a data frame containing an offset variable.
#'
#' Identifies which variable, if any, is the model offset, and fixed the name
#'   such that `offset(foo(var))` is converted to `var`, and possibly sets the
#'   values of that variable to `offset_val`.
##
#' @param model a fitted GAM.
#'
#' @param newdata data frame; new values at which to predict at.
#'
#' @param offset_val numeric, optional; if provided, then the offset variable
#'   in `newdata` is set to this constant value before returning `newdata`
#'
#' @return The original `newdata` is returned with fixed names and possibly
#'   modified offset variable.
#'
#' @author Gavin L. Simpson
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{set.seed(2)}
#' df <- data_sim("eg1", n = 400, dist = "normal", seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + offset(x2), data = df, method = "REML")
#' names(model.frame(m))
#' names(fix_offset(m, model.frame(m), offset_val = 1L))
`fix_offset` <- function(model, newdata, offset_val = NULL) {
    m.terms <- names(newdata)
    p.terms <- if (inherits(model, "scam") &&
        is.null(model[["pred.formula"]])) {
        attr(model[["terms"]], "term.labels")
    } else {
        attr(terms(model[["pred.formula"]]), "term.labels")
    }


    ## remove repsonse from m.terms if it is in there
    tt <- terms(model)
    resp <- names(attr(tt, "dataClasses"))[attr(tt, "response")]
    Y <- m.terms == resp
    if (any(Y)) {
        m.terms <- m.terms[!Y]
    }

    ## is there an offset?
    off <- is_offset(m.terms)
    if (any(off)) {
        ## which cleaned terms not in model terms
        ind <- m.terms %in% p.terms
        ## for the cleaned terms not in model terms, match with the offset
        off_var <- grep(p.terms[!ind], m.terms[off])
        if (any(off_var)) {
            take <- which(names(newdata) %in% m.terms)
            names(newdata)[take][off] <- p.terms[!ind][off_var]
        }

        ## change offset?
        if (!is.null(offset_val)) {
            newdata[, p.terms[!ind][off_var]] <- offset_val
        }
    }

    newdata                        # return
}

#' Is a model term an offset?
#'
#' Given a character vector of model terms, checks to see which, if any, is the
#'   model offset.
#'
#' @param terms character vector of model terms.
#'
#' @return A logical vector of the same length as `terms`.
#'
#' @author Gavin L. Simpson
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' df <- data_sim("eg1", n = 400, dist = "normal")
#' m <- gam(y ~ s(x0) + s(x1) + offset(x0), data = df, method = "REML")
#' nm <- names(model.frame(m))
#' nm
#' is_offset(nm)
`is_offset` <- function(terms) {
    grepl("offset\\(", terms)
}

#' Names of any parametric terms in a GAM
#'
#' @param model a fitted model.
#' @param ... arguments passed to other methods.
#'
#' @export
`parametric_terms` <- function(model, ...) {
    UseMethod("parametric_terms")
}

#' @export
#' @rdname parametric_terms
`parametric_terms.default` <- function(model, ...) {
    stop("Don't know how to identify parametric terms from <",
         class(model)[[1L]], ">", call. = FALSE)
}

#' @export
#' @rdname parametric_terms
`parametric_terms.gam` <- function(model, ...) {
    tt <- model$pterms        # get parametric terms
    if (is.list(tt)) {
        ## If a list, we have multiple linear predictors. For terms in the
        ## nth linear predictor (for n > 1) the covariate gets appended '.{n-1}'
        ## so store the mgcv names as the names of the labels returned
        labs <- unlist(lapply(tt, function(x) labels(delete.response(x))))
        names(labs) <- unlist(lapply(seq_along(labs),
                                     function(i, labs) {
                                         if (i > 1L) {
                                             paste0(labs[[i]], ".", i-1)
                                         } else {
                                             labs[[i]]}
                                     }, labs))
        labs
    } else {
        if (length(attr(tt, "term.labels") > 0L)) {
            tt <- delete.response(tt) # remove response so easier to work with
            labs <- labels(tt)        # names of all parametric terms
            names(labs) <- labs
        } else {
            labs <- character(0)
        }
    }
    labs
}

## Internal functions
`by_smooth_failure` <- function(object) {
    msg <- paste("Hmm, something went wrong identifying the requested smooth. Found:\n",
                 paste(vapply(object, FUN = smooth_label,
                              FUN.VALUE = character(1L)),
                       collapse = ', '),
                 "\nNot all of these are 'by' variable smooths. Contact Maintainer.")
    msg
}

#' @title Repeat the first level of a factor n times
#'
#' @description Function to repeat the first level of a factor n times and
#'   return this vector as a factor with the original levels intact
#'
#' @param f a factor
#' @param n numeric; the number of times to repeat the first level of `f`
#'
#' @return A factor of length `n` with the levels of `f`, but whose elements
#'   are all the first level of `f`.
`rep_first_factor_value` <- function(f, n) {
    stopifnot(is.factor(f))
    levs <- levels(f)
    factor(rep(levs[1L], length.out = n), levels = levs)
}

#' Vectorized version of `data.class`
#'
#' @param df a data frame or tibble.
#' @return A named character vector of data classes.
#'
#' @seealso The underlying functionality is provided by [data.class()].
#'
#' @noRd
`data_class` <- function(df) {
    vapply(df, data.class, character(1L))
}

#' Names of any factor variables in model data
#'
#' @param df a data frame or tibble
#'
#' @noRd
`factor_var_names` <- function(df) {
    ind <- is_factor_var(df)
    result <- if (any(ind)) {
        names(df)[ind]
    } else {
        NULL
    }
    result
}

#' Vectorised checks for variable types
#'
#' @param df a data frame or tibble
#'
#' @noRd
`is_factor_var` <- function(df) {
    result <- vapply(df, is.factor, logical(1L))
    result
}

#' @rdname is_factor_var
#'
#' @noRd
`is_numeric_var` <- function(df) {
    result <- vapply(df, is.numeric, logical(1L))
    result
}

#' Shift numeric values in a data frame by an amount `eps`
#'
#' @param df a data frame or tibble.
#' @param h numeric; the amount to shift values in `df` by.
#' @param i logical; a vector indexing columns of `df` that should not be
#'   included in the shift.
#' @param FUN function; a function to applut the shift. Typically `+` or `-`.
#' @param focal character; the focal variable when computing partial
#'   derivatives. This allows shifting only the focal variable by `eps`.
`shift_values` <- function(df, h, i, FUN = `+`, focal = NULL) {
    FUN <- match.fun(FUN)
    result <- df
    if (!is.null(focal)) {
        take <- names(df) %in% focal
        i <- i | !take
    }
    if (any(i)) {
        result[, !i] <- FUN(result[, !i], h)
    } else {
        result <- FUN(result, h)
    }
    result
}

#' @importFrom stats qnorm
`coverage_normal` <- function(level) {
    if (level <= 0 || level >= 1) {
         stop("Invalid 'level': must be 0 < level < 1", call. = FALSE)
     }
     qnorm((1 - level) / 2, lower.tail = FALSE)
}

#' @importFrom stats qt
`coverage_t` <- function(level, df) {
    if (level <= 0 || level >= 1 ) {
         stop("Invalid 'level': must be 0 < level < 1", call. = FALSE)
     }
     qt((1 - level) / 2, df = df, lower.tail = FALSE)
}

#' @importFrom mgcv fix.family.rd
`get_family_rd` <- function(object) {
    if (inherits(object, "glm")) {
        fam <- family(object)           # extract family
    } else {
        fam <- object[["family"]]
    }
    ## mgcv stores data simulation funs in `rd`
    fam <- fix.family.rd(fam)
    if (is.null(fam[["rd"]])) {
        stop("Don't yet know how to simulate from family <",
             fam[["family"]], ">", call. = FALSE)
    }
    fam[["rd"]]
}

#' @title Select smooths based on user's choices
#'
#' @description Given a vector indexing the smooths of a GAM, returns a logical
#'   vector selecting the requested smooths.
#'
#' @param smooths character; a vector of smooth labels.
#' @param select numeric, logical, or character vector of selected smooths.
#' @param partial_match logical; in the case of character `select`, should
#'   `select` match partially against `smooths`? If `partial_match = TRUE`,
#'   `select` must only be a single string, a character vector of length 1.
#' @param model_name character; a model name that will be used in error
#'   messages.
#'
#' @return A logical vector the same length as `length(smooths)` indicating
#'   which smooths have been selected.
#'
#' @author Gavin L. Simpson
`check_user_select_smooths` <- function(smooths, select = NULL,
                                        partial_match = FALSE,
                                        model_name = NULL) {
    lenSmo <- length(smooths)
    select <- if (!is.null(select)) {
        lenSel <- length(select)
        if (is.numeric(select)) {
            if (lenSmo < lenSel) {
                stop("Trying to select more smooths than are in the model.")
            }
            if (any(select > lenSmo)) {
                stop("One or more indices in 'select' > than the number of smooths in the model.")
            }
            l <- rep(FALSE, lenSmo)
            l[select] <- TRUE
            l
        } else if (is.character(select)) {
            take <- if (isTRUE(partial_match)) {
                if (length(select) != 1L) {
                    stop("When 'partial_match' is 'TRUE', 'select' must be a single string")
                }
                grepl(select, smooths, fixed = TRUE)
            } else {
                smooths %in% select
            }
            # did we fail to match?
            if (sum(take) < length(select)) {
                # must have failed to match at least one of `smooth`
                if (all(!take)) {
                    stop("Failed to match any smooths in model",
                        ifelse(is.null(model_name), "",
                            paste0(" ", model_name)),
                        ".\nTry with 'partial_match = TRUE'?",
                        call. = FALSE)
                } else {
                    stop("Some smooths in 'select' were not found in model ",
                        ifelse(is.null(model_name), "", model_name),
                        ":\n\t",
                        paste(select[!select %in% smooths], collapse = ", "),
                        call. = FALSE)
                }
            }
            take
        } else if (is.logical(select)) {
            if (lenSmo != lenSel) {
                stop("When 'select' is a logical vector, 'length(select)' must equal\nthe number of smooths in the model.")
            }
            select
        } else {
            stop("'select' is not numeric, character, or logical.")
        }
    } else {
        rep(TRUE, lenSmo)
    }

    select
}

#' Load mgcv quietly
#'
#' Simple function that loads the *mgcv* package whilst suppressing the startup
#' messages that it prints to the console.
#'
#' @return Returns a logical vectors invisibly, indicating whether the package
#'   was loaded or not.
#'
#' @export
`load_mgcv` <- function() {
    res <- suppressWarnings(requireNamespace("mgcv", quietly = TRUE))
    if (!res) {
        stop("Unable to load mgcv. Is it installed?", .call = FALSE)
    }
    ## mgcv could be attached already an we don't want to attach again
    ## as that raises an error
    attached <- "package:mgcv" %in% search()
    if(!attached) {
        suppressPackageStartupMessages(attachNamespace("mgcv"))
    }
    invisible(res)
}

## check if a model is a gamm4 object
`is_gamm4` <- function(object) {
    out <- FALSE
    ## is object a list?
    if (!inherits(object, "list")) {
        return(out)
    }
    nms <- names(object)
    if (! all(c("gam","mer") %in% nms)) {
        return(out)
    }
    if (! (inherits(object[["mer"]], "lmerMod") &&
           inherits(object[["gam"]], "gam"))) {
        return(out)
    }
    out <- TRUE
    out
}

## check is a model is a gamm object
`is_gamm` <- function(object) {
    inherits(object, "gamm")
}

#' Is a model term a factor (categorical)?
#'
#' Given the name (a term label) of a term in a model, identify if the term is a
#' factor term or numeric. This is useful when considering interactions, where
#' terms like `fac1:fac2` or `num1:fac1` may be requested by the user. Only for
#' terms of the type `fac1:fac2` will this function return `TRUE`.
#'
#' @param object an R object on which method dispatch is performed
#' @param term character; the name of a model term, in the sense of
#'   `attr(terms(object), "term.labels")`. Currently not checked to see if the
#'   term exists in the model.
#' @param ... arguments passed to other methods.
#'
#' @return A logical: `TRUE` if and only if all variables involved in the term
#'   are factors, otherwise `FALSE`.
#'
#' @export
`is_factor_term` <- function(object, term, ...) {
    UseMethod("is_factor_term", object)
}

#' @rdname is_factor_term
#' @export
`is_factor_term.terms` <- function(object, term, ...) {
    if (missing(term)) {
        stop("Argument 'term' must be provided.")
    }
    facs <- attr(object, "factors")
    out <- if (term %in% colnames(facs)) {
        facs <- facs[, term, drop = FALSE]
        take <- rownames(facs)[as.logical(facs)]
        data_types <- attr(object, 'dataClasses')[take]
        all(data_types %in% c("factor", "character"))
    } else {
        NULL
    }
    out
}

#' @rdname is_factor_term
#' @export
`is_factor_term.gam` <- function(object, term, ...) {
    object <- terms(object)
    is_factor_term(object, term, ...)
}

#' @rdname is_factor_term
#' @export
`is_factor_term.bam` <- function(object, term, ...) {
    object <- terms(object)
    is_factor_term(object, term, ...)
}

#' @rdname is_factor_term
#' @export
`is_factor_term.gamm` <- function(object, term, ...) {
    object <- terms(object$gam)
    is_factor_term(object, term, ...)
}

#' @rdname is_factor_term
#' @export
`is_factor_term.list` <- function(object, term, ...) {
    if (!is_gamm4(object)) {
        if (all(vapply(object, inherits, logical(1), "terms"))) {
            out <- any(unlist(lapply(object, is_factor_term, term)))
        } else {
            stop("Don't know how to handle generic list objects.")
        }
    } else {
        object <- terms(object$gam)
        out <- is_factor_term(object, term, ...)
    }
    out
}

#' Names of variables involved in a specified model term
#'
#' Given the name (a term label) of a term in a model, returns the names
#' of the variables involved in the term.
#'
#' @param object an R object on which method dispatch is performed
#' @param term character; the name of a model term, in the sense of
#'   `attr(terms(object), "term.labels")`. Currently not checked to see if the
#'   term exists in the model.
#' @param ... arguments passed to other methods.
#'
#' @return A character vector of variable names.
#'
#' @export
`term_variables` <- function(object, term, ...) {
    UseMethod("term_variables")
}

#' @rdname term_variables
#' @export
`term_variables.terms` <- function(object, term, ...) {
    if (missing(term)) {
        stop("'term' must be supplied.")
    }
    facs <- attr(object, "factors")[ , term]
    names(facs)[as.logical(facs)]
}

#' @rdname term_variables
#' @export
`term_variables.gam` <- function(object, term, ...) {
    object <- terms(object)
    term_variables(object, term = term, ...)
}

#' @rdname term_variables
#' @export
`term_variables.bam` <- function(object, term, ...) {
    object <- terms(object)
    term_variables(object, term, ...)
}

## Create *mgcv*-alike labels for by smooths
## FIXME: should make this work for continuous by too
`mgcv_by_smooth_labels` <- function(smooth, by_var, level) {
    paste0(smooth, ":", by_var, level)
}

#' Returns names of variables from a smooth label
#'
#' @param label character; a length 1 character vector containing the label of
#'   a smooth.
#'
#' @export
#'
#' @importFrom vctrs vec_c
#'
#' @examples
#'
#' vars_from_label("s(x1)")
#' vars_from_label("t2(x1,x2,x3)")
vars_from_label <- function(label) {
    if (length(label) > 1) {
        label <- rep(label, length.out = 1)
        warning("'label' must be a length 1 vector; using 'label[1]' only.")
    }
    # matches 1 or 2 letters or numbers for s, te, t2, etc,
    # then zero or 1 periods `.` and zero or more numbers for s.1() in LSS mods
    # then an opening paranethesis \\(
    # start a group to match 1 or more letter, numbers, punc
    #  ===> this is the variable or variables
    # end the group,
    # then a closing parenthesis \\)
    # zero or 1 : for the start of the by var info
    # finally zero or more of any characters for the factor level combo
    vars <- gsub("^[[:alnum:]]{1,2}\\.?[[:digit:]]*\\(([[:graph:]]+)\\):?(.*)$",
                 "\\1",
                 label)
    vec_c(strsplit(vars, ",")[[1L]])
}

#' Transform estimated values and confidence intervals by applying a function
#'
#' @param object an object to apply the transform function to.
#' @param fun the function to apply.
#' @param ... additional arguments passed to methods.
#' @param column character; for the `"tbl_df"` method, which column to
#'   transform.
#'
#' @return Returns `object` but with the estimate and upper and lower values
#'   of the confidence interval transformed via the function.
#'
#' @export
#' @author Gavin L. Simpson
`transform_fun` <- function(object, fun = NULL , ...) {
    UseMethod("transform_fun")
}

#' @rdname transform_fun
#' @export
`transform_fun.evaluated_smooth` <- function(object, fun = NULL, ...) {
    ## If fun supplied, use it to transform est and the upper and lower interval
    if (!is.null(fun)) {
        fun <- match.fun(fun)
        object[["est"]] <- fun(object[["est"]])
        if (!is.null(object[["upper"]])) {
            object[["upper"]] <- fun(object[["upper"]])
        }
        if (!is.null(object[["lower"]])) {
            object[["lower"]] <- fun(object[["lower"]])
        }
    }

    object
}

#' @rdname transform_fun
#' @export
`transform_fun.smooth_estimates` <- function(object, fun = NULL, ...) {
    ## If fun supplied, use it to transform est and the upper and lower interval
    if (!is.null(fun)) {
        fun <- match.fun(fun)
        object[["est"]] <- fun(object[["est"]])
        if (!is.null(object[["upper"]])) {
            object[["upper"]] <- fun(object[["upper"]])
        }
        if (!is.null(object[["lower"]])) {
            object[["lower"]] <- fun(object[["lower"]])
        }
    }

    object
}

#' @rdname transform_fun
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
`transform_fun.mgcv_smooth` <- function(object, fun = NULL, ...) {
    if (!is.null(fun)) {
        fun <- match.fun(fun)
        object <- mutate(object,
                         across(all_of(c("est", "lower_ci", "upper_ci")),
                                .fns = fun))
    }

    object
}

#' @rdname transform_fun
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
`transform_fun.evaluated_parametric_term` <- function(object, fun = NULL, ...) {
    ## If fun supplied, use it to transform est and the upper and lower interval
    if (!is.null(fun)) {
        fun <- match.fun(fun)
        object <- mutate(object,
                         across(all_of(c("est", "lower", "upper")),
                                .fns = fun))
    }

    object
}

#' @rdname transform_fun
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
`transform_fun.parametric_effects` <- function(object, fun = NULL, ...) {
    ## If fun supplied, use it to transform est and the upper and lower interval
    if (!is.null(fun)) {
        fun <- match.fun(fun)
        object <- mutate(object,
                         across(any_of(c("partial", "lower", "upper")),
                                .fns = fun))
    }

    object
}

#' @rdname transform_fun
#' @export
#' @importFrom dplyr mutate across
`transform_fun.tbl_df` <- function(object, fun = NULL, column = NULL, ...) {
    if (is.null(column)) {
        stop("'column' to modify must be supplied.")
    }
    ## If fun supplied, use it to transform est and the upper and lower interval
    if (!is.null(fun)) {
        fun <- match.fun(fun)
        object <- mutate(object,
                         across(all_of(column), .fns = fun))
    }

    object
}

## Normalize a vector to range -1 -- 1
`norm_minus_one_to_one` <- function(x) {
    abs_x <- abs(x)
    sign_x <- sign(x)
    minx <- 0
    maxx <- max(abs_x, na.rm = TRUE)
    abs_x <- (abs_x - 0) / (maxx - 0)
    abs_x * sign_x
}

#' Delete response from user-supplied data
#'
#' @param model a fitted model from which a `terms` object can be extracted.
#' @param data a data frame containing variables in the formula of `model`.
#' @param model_frame logical; if `TRUE`, return as a `model.frame` object with
#'   a `"terms"` attribute. If `FALSE`, return a data frame with the `"terms"`
#'   attribute removed.
#'
#' @keywords internal
#' @noRd
`delete_response` <- function(model, data = NULL, model_frame = TRUE) {
    if (is.null(data)) {
        if (is.null(model[["model"]])) {
            stop("`data` must be supplied if not available from 'model'")
        } else {
            data <- model[["model"]]
        }
    }

    tt <- terms(model[["pred.formula"]])
    tt <- delete.response(tt)
    out <- model.frame(tt, data = data)

    if(identical(model_frame, FALSE)) {
        attr(out, "terms") <- NULL
    }

    out
}

#' Extract names of all variables needed to fit a GAM or a smooth
#'
#' @param object a fitted GAM object or an {mgcv} smooth object
#' @param ... arguments passed to other methods. Not currently used.
#'
#' @return A vector of variable names required for terms in the model
#'
#' @export
`term_names` <- function(object, ...) {
    UseMethod("term_names")
}

#' @rdname term_names
#' @export
`term_names.gam` <- function(object, ...) {
    tt <- object[["pred.formula"]]
    if (is.null(tt)) {
        stop("`object` does not contain `pred.formula`; is this is fitted GAM?",
             call. = FALSE)
    }
    tt <- terms(tt)
    attr(tt, "term.labels")
}

#' @rdname term_names
#' @export
`term_names.mgcv.smooth` <- function(object, ...) {
    tt <- object[["term"]]
    if (is.null(tt)) {
        stop("`object` does not contain `term`; is this is an {mgcv} smooth?",
             call. = FALSE)
    }
    if (is_by_smooth(object)) {
        tt <- append(tt, by_variable(object))
    }
    tt
}

#' @rdname term_names
#' @export
`term_names.gamm` <- function(object, ...) {
    object <- object[["gam"]]
    term_names(object)
}

#' Identify all terms that are involved in a smooth
#' @noRd
#' @keywords internal
`terms_in_smooth` <- function(smooth) {
    ## make sure we're using an actual smooth
    check_is_mgcv_smooth(smooth)

    ## take the term component which has the main terms involved in the smooth
    sm_terms <- smooth[["term"]]

    ## extract any by variable - could name a factor, a continuous var,
    ## a matrix. `"NA"` signals missing - yes, really
    sm_by <- by_variable(smooth)
    if (sm_by == "NA") { # if `"NA"` set this to NULL
        sm_by <-  NULL
    }
    ## combine the elements. If `sm_by` is `NULL` it isn't included
    c(sm_terms, sm_by)
}

# does a tensor smooth involve a random effect marginal
`involves_ranef_smooth` <- function(smooth) {
    ## make sure we're using an actual smooth
    check_is_mgcv_smooth(smooth)

    out <- FALSE # return FALSE unless...

    # check if this is a tensor product smooth
    if (inherits(smooth, what = c("tensor.smooth", "t2.smooth"))) {
        # check if any of the marginals inherit from the "random.effect" class
        ranefs <- vapply(smooth[["margin"]], FUN = inherits,
                         FUN.VALUE = logical(1L), what = "random.effect")
        # return TRUE if any marginal is a random effect
        out <- any(ranefs)
    }

    out
}

# is a smooth isotropic?
`is_isotropic_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)

    out <- FALSE
    cls <- c("tprs.smooth", "duchon.spline", "gp.smooth",
             "sos.smooth") # others?
    if (inherits(smooth, what = cls)) {
        out <- TRUE
    }
    out
}

#' List the variables involved in a model fitted with a formula
#' 
#' @param model a fitted model object with a `$pred.formula`, `$terms`
#'   component or a `"terms"` attribute
#' @param ... Arguments passed to other methods. Currently ignored.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' 
#' # simulate some Gaussian data
#' df <- data_sim("eg1", n = 50, seed = 2)
#' 
#' # fit a GAM with 1 smooth and 1 linear term
#' m1 <- gam(y ~ s(x2, k = 7) + x1, data = df, method = "REML")
#' model_vars(m1)
#' 
#' # fit a lm with two linear terms
#' m2 <- lm(y ~ x2 + x1, data = df)
#' model_vars(m2)
`model_vars` <- function(model, ...) {
    UseMethod("model_vars")
}

#' @export
#' @rdname model_vars
`model_vars.gam` <- function(model, ...){
    # want a vector of variables involved in the model formula.
    # Don't want this `attr(terms(model), "term.labels")` ! as this returns
    # model terms not variable names. Use all.vars() on `pred.formula` for
    # a GAM(M) model
    all.vars(model$pred.formula)
}

#' @export
#' @rdname model_vars
`model_vars.default` <- function(model, ...){
    # want a vector of variables involved in the model formula
    tt <- terms(model)
    if (is.null(tt)) {
        stop("`terms()` not available for `model`.")
    }
    tt <- delete.response(tt) 
    all.vars(attr(tt, "variables"))
}

#' @export
#' @rdname model_vars
`model_vars.bam` <- function(model, ...){
    # want a vector of variables involved in the model formula.
    # Don't want this `attr(terms(model), "term.labels")` ! as this returns
    # model terms not variable names. Use all.vars() on `pred.formula` for
    # a GAM(M) model
    all.vars(model$pred.formula)
}

#' @export
#' @rdname model_vars
`model_vars.gamm` <- function(model, ...){
    # want a vector of variables involved in the model formula.
    # Don't want this `attr(terms(model), "term.labels")` ! as this returns
    # model terms not variable names. Use all.vars() on `pred.formula` for
    # a GAM(M) model
    model_vars(model[["gam"]]$pred.formula)
}

#' @export
#' @rdname model_vars
`model_vars.gamm4` <- function(model, ...){
    # this is here for when Simon actually classes gamm4 objects
    # want a vector of variables involved in the model formula.
    # Don't want this `attr(terms(model), "term.labels")` ! as this returns
    # model terms not variable names. Use all.vars() on `pred.formula` for
    # a GAM(M) model
    model_vars(model[["gam"]]$pred.formula)
}

#' @export
#' @rdname model_vars
`model_vars.list` <- function(model, ...){
    # want a vector of variables involved in the model formula.
    # Don't want this `attr(terms(model), "term.labels")` ! as this returns
    # model terms not variable names. Use all.vars() on `pred.formula` for
    # a GAM(M) model
    if (!is_gamm4(model)) {
        stop("Don't know how to handle generic list objects.")
    }
    model_vars(model[["gam"]]$pred.formula)
}

`newdata_deprecated` <- function() {
    message("Use of the `newdata` argument is deprecated.\n",
        "Instead, use the data argument `data`.\n")
}

# Indices of which coefs are in which linear predictors
# returns a list of length equal to the number of linear predictors
`lss_eta_index` <- function(object) {
    lpi <- attr(formula(object), "lpi")
    if (is.null(lpi)) {
        lpi <- list(seq_along(coef(object)))
    }
    # get rid of the shared information as I don't think this is needed
    attr(lpi, "overlap") <- NULL
    # return
    lpi
}
