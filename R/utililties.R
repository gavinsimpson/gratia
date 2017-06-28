`smooth_terms` <- function(obj, ...) {
    UseMethod("smooth_terms")
}

`smooth_terms.gam` <- function(obj, ...) {
    lapply(obj$smooth, `[[`, "term")
}

`smooth_terms.gamm` <- function(obj, ...) {
    smooth_terms(obj$gam, ...)
}

`smooth_terms.mgcv.smooth` <- function(obj, ...) {
    obj[["term"]]
}

`smooth_dim` <- function(obj) {
    UseMethod("smooth_dim")
}

`smooth_dim.gam` <- function(obj) {
    vapply(obj$smooth, FUN = `[[`, FUN.VALUE = integer(1), "dim")
}

`smooth_dim.gamm` <- function(obj) {
    smooth_dim(obj$gam)
}

`smooth_dim.mgcv.smooth` <- function(obj) {
    obj[["dim"]]
}

`select_terms` <- function(object, terms) {
    TERMS <- unlist(smooth_terms(object))
    terms <- if (missing(terms)) {
                 terms <- TERMS
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

`is.gamm` <- function(object) {
    inherits(object, "gamm")
}

`is.gam` <- function(object) {
    inherits(object, "gam")
}

`get_smooth` <- function(object, term) {
    if (is.gamm(object)) {
        object <- object$gam
    }
    object$smooth[[which_smooth(object, term)]]
}

`which_smooth` <- function(object, term) {
    if (is.gamm(object)) {
        object <- object$gam
    }
    terms <- unlist(smooth_terms(object))
    which(terms %in% term)
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
        term <- select_terms(object, term)
        smooth <- get_smooth(object, term)
        start <- smooth$first.para
        end <- smooth$last.para
        para.seq <- start:end
        V <- V[para.seq, para.seq, drop = FALSE]
    }
    V
}
