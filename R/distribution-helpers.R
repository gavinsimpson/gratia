## helpers for working with mgcv's families

#' Negative binomial parameter theta
#'
#' @param model a fitted model.
#'
#' @return A numeric vector of length 1 containing the estimated value of
#'   theta.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' df <- data_sim("eg1", n = 500, dist = "poisson", scale = 0.1, seed = 6)
#'
#' m <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") +
#'          s(x3, bs = "cr"), family = nb, data = df, method = "REML")
#' ## IGNORE_RDIFF_BEGIN
#' nb_theta(m)
#' ## IGNORE_RDIFF_END
`nb_theta` <- function(model) {
    UseMethod("nb_theta")
}

#' @export
#' @importFrom stringr str_detect
#' @describeIn nb_theta Method for class `"gam"`
`nb_theta.gam` <- function(model) {
    supported <- str_detect(family_name(model),
                            c("Negative Binomial", "negative binomial"))
    if (!any(supported)) {
        stop("Only negative binomial models are supported.")
    }

    ## how mgcv stores theta depends on which family was used, and this also
    ## affects the actual function stored in the family. Need theta on natural
    ## scale so we transform this if family is nb()
    fam <- family(model)
    theta <- if (inherits(fam, "extended.family")) {
        fam$getTheta(trans = TRUE)
    } else {
        fam$getTheta()
    }

    ## return
    theta
}

#' General extractor for additional parameters in mgcv models
#'
#' @param object a fitted model
#' @param transform logical; transform to the natural scale of the parameter
#' @param ... arguments passed to other methods.
#'
#' @export
#'
#' @return Returns a numeric vector of additional parameters
#'
#' @examples
#' load_mgcv()
#' df <- data_sim("eg1", dist = "poisson", seed = 42, scale = 1/5)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML",
#'          family = nb())
#' p <- theta(m)
`theta` <- function(object, ...) {
    UseMethod("theta")
}

#' @export
#' @rdname theta
`theta.gam` <- function(object, transform = TRUE, ...) {
    theta_fun <- family(object)$getTheta
    if (is.null(theta_fun)) {
        stop("No additional parameters available for this model")
    }
    theta_fun(trans = transform)
}

#' Are additional parameters available for a GAM?
#'
#' @param object an R object, either a [family()] object or an object whose
#'   class has a [family()] method.
#'
#' @return A logical; `TRUE` if additional parameters available, `FALSE`
#'   otherwise.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' df <- data_sim("eg1", dist = "poisson", seed = 42, scale = 1/5)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML",
#'          family = nb())
#' has_theta(m)
#' p <- theta(m)
`has_theta` <- function(object) {
    theta_fun <- if (inherits(object, "family")) {
        object$getTheta
    } else {
        family(object)$getTheta
    }
    !is.null(theta_fun)
}
