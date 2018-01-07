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

##' Extracts the dimension of an estimated smooth.
##'
##' This is a generic function with methods for objects of class
##'   \code{"gam"}, \code{"gamm"}, and \code{"mgcv.smooth"}.
##' @title Dimension of a smooth
##' @param object an R object. See Details for list of supported objects.
##' @return A numeric vector of dimensions for each smooth.
##' @author Gavin L. Simpson
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

##' Tests for by variable smooths
##'
##' Functions to check if a smooth is a by-variable one and to test of the type
##' of by-variable smooth is a factor-smooth or a continous-smooth interaction.
##'
##' @param smooth an object of class `"mgcv.smooth"`
##'
##' @return A logical vector.
##'
##' @author Gavin L. Simpson
##'
##' @export
##' @rdname is_by_smooth
`is_by_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    is_factor_by_smooth(smooth) | is_continuous_by_smooth(smooth)
}

##' @export
##' @rdname is_by_smooth
`is_factor_by_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    by.level <- smooth[["by.level"]]
    !is.null(by.level)
}

##' @export
##' @rdname is_by_smooth
`is_continuous_by_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    by.level <- smooth[["by.level"]]
    by.var <- smooth[["by"]]
    !(is.null(by.level) & by.var == "NA")
}

`by_variable` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    as.character(smooth[["by"]])
}

`by_level` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    smooth[["by.level"]]
}

`smooth_variable` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    smooth[["term"]]
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
    object[["smooth"]][[which_smooth(object, term)]]
}

`get_smooths_by_id` <- function(id, object) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    object[["smooth"]][id]
}

`which_smooth` <- function(object, term) {
    if (is.gamm(object)) {
        object <- object[["gam"]]
    }
    smooths <- smooths(object)
    grep(term, smooths, fixed = TRUE)
}

`get_vcov` <- function(object, unconditional = FALSE, frequentist = FALSE,
                       term = NULL) {
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

`add_s` <- function(terms) {
    take <- grepl("^s\\(.+\\)$", terms)
    terms[take] <- paste("s(", terms[take], ")", sep = "")
    terms
}
