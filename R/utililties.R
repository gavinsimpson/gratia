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

##' Dimension of a smooth
##'
##' Extracts the dimension of an estimated smooth.
##'
##' This is a generic function with methods for objects of class
##'   \code{"gam"}, \code{"gamm"}, and \code{"mgcv.smooth"}.
##
##' @param object an R object. See Details for list of supported objects.
##'
##' @return A numeric vector of dimensions for each smooth.
##'
##' @author Gavin L. Simpson
##'
##' @rdname smooth_dim
##' @export
`smooth_dim` <- function(object) {
    UseMethod("smooth_dim")
}

##' @rdname smooth_dim
##' @export
`smooth_dim.gam` <- function(object) {
    vapply(object[["smooth"]], FUN = `[[`, FUN.VALUE = integer(1), "dim")
}

##' @rdname smooth_dim
##' @export
`smooth_dim.gamm` <- function(object) {
    smooth_dim(object[["gam"]])
}

##' @rdname smooth_dim
##' @export
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

`is_mgcv_smooth` <- function(smooth) {
    inherits(smooth, "mgcv.smooth")
}

`check_is_mgcv_smooth` <- function(smooth) {
    out <- is_mgcv_smooth(smooth)
    if (identical(out, FALSE)) {
        stop("Object passed to 'smooth' is not a 'mgcv.smooth'.")
    }
    invisible(out)
}

`is.gamm` <- function(object) {
    inherits(object, "gamm")
}

`is.gam` <- function(object) {
    inherits(object, "gam")
}

`get_smooth` <- function(object, term) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    smooth <- object[["smooth"]][which_smooth(object, term)]
    if (identical(length(smooth), 1L)) {
        smooth <- smooth[[1L]]
    }
    smooth
}

`get_smooths_by_id` <- function(id, object) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    object[["smooth"]][id]
}

`get_by_smooth` <- function(object, term, level) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }

    ## which smooth match the term?
    take <- which_smooth(object, term)
    S <- object[["smooth"]][take]

    ## if there are multiple, then suggests a factor by smooth
    is_by <- vapply(S, is_factor_by_smooth, logical(1L))

    ## if any are factor by variable smooths, get the levels
    if (any(is_by)) {
        if (missing(by_level)) {
            stop("No value provided for argument 'level':\n  Getting a factor by-variable smooth requires a 'level' be supplied.")
        }
        level <- as.character(level)    # explicit coerce to character for later comparison
        levs <- vapply(S, level, character(1L))
        take <- match(level, levs)
        if (is.na(take)) {
            msg <- paste0("Invalid 'level' for smooth '", term, "'. Possible levels are:\n")
            msg <- paste(msg, paste(strwrap(paste0(shQuote(levs), collapse = ", "),
                                            prefix = " ", initial = ""),
                                    collapse = "\n"))
            stop(msg)
        }

        S <- S[[take]]
    }

    ## return a single smooth object
    S
}

`which_smooth` <- function(object, term) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    smooths <- smooths(object)
    grep(term, smooths, fixed = TRUE)
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

##' Fix the names of a data frame containing an offset variable.
##'
##' Identifies which variable, if any, is the model offset, and fixed the name
##' such that `"offset(foo(var))" is converted `"var"`, and possibly sets that
##' data to `offset_value`.
##
##' @param model a fitted GAM.
##'
##' @param newdata data frame; new values at which to predict at.
##'
##' @param offset_value numeric, optional; if provided, then the offset variable in `newdata` is set to this constant value before returning `newdata`
##'
##' @return The original `newdata` is returned with fixed names and possibly modified offset variable.
##'
##' @author Gavin L. Simpson
##'
##' @export
##'
##' @examples
##' ##\testonly{set.seed(2)}
##' library("mgcv")
##' set.seed(2)
##' df <- gamSim(1, n = 400, dist = "normal")
##' m <- gam(y ~ s(x0) + s(x1) + offset(x0), data = df, method = "REML")
##' names(model.frame(m))
##' names(fix_offset(m, model.frame(m), offset_value = 1L))
`fix_offset` <- function(model, newdata, offset_value = NULL) {
    m.terms <- names(newdata)
    p.terms <- attr(terms(model[["pred.formula"]]), "term.labels")

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
        off_var <- grepl(p.terms[!ind], m.terms[off])
        if (any(off_var)) {
            names(newdata)[off] <- p.terms[!ind][off_var]
        }
    }

    ## change offset?
    if (!is.null(offset_value)) {
        newdata[, off] <- offset_value
    }

    newdata                        # return
}

##' Is a model term an offset?
##'
##' Given a character vector of model terms, checks to see which, if any, is the model offset.
##'
##' @param terms character vector of model terms.
##'
##' @return A logical vector of the same length as `terms`.
##'
##' @author Gavin L. Simpson
##'
##' @export
##'
##' @examples
##' library("mgcv")
##' df <- gamSim(1, n = 400, dist = "normal")
##' m <- gam(y ~ s(x0) + s(x1) + offset(x0), data = df, method = "REML")
##' nm <- names(model.frame(m))
##' nm
##' is_offset(nm)
`is_offset` <- function(terms) {
    grepl("offset\\(", terms)
}
