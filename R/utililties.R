`smooth_terms` <- function(obj, ...) {
    UseMethod("smooth_terms")
}

`smooth_terms.gam` <- function(obj, ...) {
    lapply(obj[["smooth"]], `[[`, "term")
}

`smooth_terms.gamm` <- function(obj, ...) {
    smooth_terms(obj[["gam"]], ...)
}

`smooth_terms.mgcv.smooth` <- function(obj, ...) {
    obj[["term"]]
}

`smooth_dim` <- function(obj) {
    UseMethod("smooth_dim")
}

`smooth_dim.gam` <- function(obj) {
    vapply(obj[["smooth"]], FUN = `[[`, FUN.VALUE = integer(1), "dim")
}

`smooth_dim.gamm` <- function(obj) {
    smooth_dim(obj[["gam"]])
}

`smooth_dim.mgcv.smooth` <- function(obj) {
    obj[["dim"]]
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

`is_by_smooth` <- function(smooth) {
    check_is_mgcv_smooth(smooth)
    !is.null(smooth[["by.level"]])
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
