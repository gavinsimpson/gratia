#' @export
`vcov.scam` <- function (object, freq = FALSE, dispersion = NULL,
                         parametrized = TRUE, ...)  {
    if (freq) {
        vc <- if (parametrized) {
            object$Ve.t
        } else {
            object$Ve
        }
    } else {
        vc <- if (parametrized) {
            object$Vp.t
        } else {
            object$Vp
        }
    }
    if (!is.null(dispersion)) {
        vc <- dispersion * vc/object$sig2
    }
    name <- names(object$edf)
    dimnames(vc) <- list(name, name)
    vc
}

#' Extract coefficients from a fitted `scam` model.
#'
#' @param object a model object fitted by `scam()`
#' @param parametrized logical; extract parametrized coefficients, which respect the linear inequality constraints of the model.
#' @param ... other arguments.
#'
#' @export
`coef.scam` <- function(object, parametrized = TRUE, ...) {
    coefs <- if (parametrized) {
        object$coefficients.t
    } else {
        object$coefficients
    }
    coefs
}
