##' Extract link and inverse link functions from models
##'
##' Returns the link or its inverse from an estimated model, and provides a
##' simple way to extract rhese functions from complex models with multiple
##' links, such as location scale models.
##'
##' @param object a family object or a fitted model from which to extract the
##'   family object.
##' @param parameter character; which parameter of the distribution. Usually
##'   `"location"` but `"scale"` and `"shape"` may be provided for location
##'   scale models.
##' @param ... arguments passed to other methods.
##'
##' @author Gavin L. Simpson
##' 
##' @export
##'
##' @examples
##' load_mgcv()
##' 
##' link(gaussian())
##' link(nb())
##'
##' inv_link(nb())
##'
##' \dontshow{
##' set.seed(4234)
##' }
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
##' mod <- gam(list(y ~ s(x0) + s(x1) + s(x2) + s(x3), ~ 1), data = dat,
##'            family = gaulss)
##'
##' link(mod, parameter = "scale")
##' inv_link(mod, parameter = "scale")
`link` <- function(object, ...) {
    UseMethod("link")
}

##' @rdname link
##' @export
`link.family` <- function(object, parameter = c("location", "scale", "shape"), ...) {
    parameter <- match.arg(parameter)
    linfo <- object[["linfo"]]
    distr <- object[["family"]] # name of the the family

    ## process distr for some families
    if (grepl("^Negative Binomial", distr)) {
        distr <- "nb"
    }
    if (grepl("negative binomial", distr)) {
        distr <- "nb"
    }
    if (grepl("^Tweedie", distr)) {
        distr <- "tweedie"
    }
    if (identical(distr, "Beta regression")) {
        distr <- "beta"
    }
    if (identical(distr, "scaled t")) {
        distr <- "scaled_t"
    }
    if (identical(distr, "Ordered Categorical")) {
        distr <- "ocat"
    }
    if (identical(distr, "zero inflated Poisson")) {
        distr <- "zip"
    }
    if (identical(distr, "Cox PH")) {
        distr <- "cox_ph"
    }

    ## which link function
    lfun <- switch(distr,
                   gaussian = gaussian_link(object, parameter, inverse = FALSE),
                   poisson = poisson_link(object, parameter, inverse = FALSE),
                   binomial = binomial_link(object, parameter, inverse = FALSE),
                   Gamma = gamma_link(object, parameter, inverse = FALSE),
                   inverse.gaussian = inverse_gaussian_link(object, parameter,
                                                            inverse = FALSE),
                   quasi = quasi_link(object, parameter, inverse = FALSE),
                   quasipoisson = quasi_poisson_link(object, parameter,
                                                     inverse = FALSE),
                   quasibinomial = quasi_binomial_link(object, parameter,
                                                       inverse = FALSE),
                   nb = nb_link(object, parameter, inverse = FALSE),
                   tweedie = tw_link(object, parameter, inverse = FALSE),
                   beta = beta_link(object, parameter, inverse = FALSE),
                   scaled_t = scaled_t_link(object, parameter, inverse = FALSE),
                   ocat = ocat_link(object, parameter, inverse = FALSE),
                   zip = zip_link(object, parameter, inverse = FALSE),
                   cox_ph = cox_ph_link(object, parameter, inverse = FALSE),
                   gaulss = gaulss_link(object, parameter, inverse = FALSE)
                   )
    
    ## return
    lfun
}

##' @rdname link
##' @export
##' @importFrom stats family
`link.gam` <- function(object, parameter = c("location", "scale", "shape"), ...) {
    link(family(object), parameter = parameter, ...)
}

##' @rdname link
##' @export
`link.bam` <- function(object, parameter = c("location", "scale", "shape"),
                       ...) {
    NextMethod()
}

##' @rdname link
##' @export
`link.gamm` <- function(object, ...) {
    link(object[["gam"]])
}

##' @rdname link
##' @export
`inv_link` <- function(object, ...) {
    UseMethod("inv_link")
}

##' @rdname link
##' @export
`inv_link.family` <- function(object, parameter = c("location", "scale", "shape"),
                              ...) {
    parameter <- match.arg(parameter)
    linfo <- object[["linfo"]]
    distr <- object[["family"]] # name of the the family

    ## process distr for some families
    if (grepl("^Negative Binomial", distr)) {
        distr <- "nb"
    }
    if (grepl("^negative binomial", distr)) {
        distr <- "nb"
    }
    if (grepl("^Tweedie", distr)) {
        distr <- "tweedie"
    }
    if (identical(distr, "Beta regression")) {
        distr <- "beta"
    }
    if (identical(distr, "scaled t")) {
        distr <- "scaled_t"
    }
    if (identical(distr, "Ordered Categorical")) {
        distr <- "ocat"
    }
    if (identical(distr, "zero inflated Poisson")) {
        distr <- "zip"
    }
    if (identical(distr, "Cox PH")) {
        distr <- "cox_ph"
    }

    ## which link function
    lfun <- switch(distr,
                   gaussian = gaussian_link(object, parameter, inverse = TRUE),
                   poisson = poisson_link(object, parameter, inverse = TRUE),
                   binomial = binomial_link(object, parameter, inverse = TRUE),
                   Gamma = gamma_link(object, parameter, inverse = TRUE),
                   inverse.gaussian = inverse_gaussian_link(object, parameter,
                                                            inverse = TRUE),
                   quasi = quasi_link(object, parameter, inverse = TRUE),
                   quasipoisson = quasi_poisson_link(object, parameter,
                                                     inverse = TRUE),
                   quasibinomial = quasi_binomial_link(object, parameter,
                                                       inverse = TRUE),
                   nb = nb_link(object, parameter, inverse = TRUE),
                   tweedie = tw_link(object, parameter, inverse = TRUE),
                   beta = beta_link(object, parameter, inverse = TRUE),
                   scaled_t = scaled_t_link(object, parameter, inverse = TRUE),
                   ocat = ocat_link(object, parameter, inverse = TRUE),
                   zip = zip_link(object, parameter, inverse = TRUE),
                   cox_ph = cox_ph_link(object, parameter, inverse = TRUE),
                   gaulss = gaulss_link(object, parameter, inverse = TRUE)
                   )
    
    ## return
    lfun
}

##' @rdname link
##' @export
##' @importFrom stats family
`inv_link.gam` <- function(object, parameter = c("location", "scale", "shape"), ...) {
    inv_link(family(object), parameter = parameter, ...)
}

##' @rdname link
##' @export
`inv_link.bam` <- function(object, parameter = c("location", "scale", "shape"),
                       ...) {
    NextMethod()
}

##' @rdname link
##' @export
`inv_link.gamm` <- function(object, ...) {
    inv_link(object[["gam"]])
}

## Internal link extractor functions

`gaussian_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "gaussian")) {
        stop("'family' is not '\"gaussian\"'", call. = FALSE)
    }

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`poisson_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "poisson")) {
        stop("'family' is not '\"poisson\"'", call. = FALSE)
    }

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`binomial_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "binomial")) {
        stop("'family' is not '\"binomial\"'", call. = FALSE)
    }

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`gamma_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "Gamma")) {
        stop("'family' is not '\"Gamma\"'", call. = FALSE)
    }

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`inverse_gaussian_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "inverse.gaussian")) {
        stop("'family' is not '\"inverse.gaussian\"'", call. = FALSE)
    }

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`quasi_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "quasi")) {
        stop("'family' is not '\"quasi\"'", call. = FALSE)
    }

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`quasi_poisson_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "quasipoisson")) {
        stop("'family' is not '\"quasipoisson\"'", call. = FALSE)
    }

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`quasi_binomial_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "quasibinomial")) {
        stop("'family' is not '\"quasibinomial\"'", call. = FALSE)
    }

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`nb_link` <- function(family, parameter = c("location", "mu"),
                      inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    distr <- family[["family"]]
    if (!(grepl("negative binomial", distr) ||
          grepl("^Negative Binomial", distr))) {
        stop("'family' is not a negative binomial family", call. = FALSE)
    }
    
    parameter <- match.arg(parameter)
    
    extract_link(family, inverse = inverse)
}

`tw_link` <- function(family, parameter = c("location", "mu"),
                      inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!grepl("^Tweedie", family[["family"]])) {
        stop("'family' is not a Tweedie family", call. = FALSE)
    }
    
    parameter <- match.arg(parameter)
    
    extract_link(family, inverse = inverse)
}

`beta_link` <- function(family, parameter = c("location", "mu"),
                        inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "Beta regression")) {
        stop("'family' is not '\"Beta regression\"'", call. = FALSE)
    }
    
    parameter <- match.arg(parameter)
    
    extract_link(family, inverse = inverse)
}

`scaled_t_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "scaled t")) {
        stop("'family' is not '\"scaled t\"'", call. = FALSE)
    }
    
    parameter <- match.arg(parameter)
    
    extract_link(family, inverse = inverse)
}

`ocat_link` <- function(family, parameter = c("location", "mu"),
                                       inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "Ordered Categorical")) {
        stop("'family' is not '\"Ordered Categorical\"'", call. = FALSE)
    }
    
    parameter <- match.arg(parameter)
    
    extract_link(family, inverse = inverse)
}

`zip_link` <- function(family, parameter = c("location", "mu"),
                       inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "zero inflated Poisson")) {
        stop("'family' is not '\"zero inflated Poisson\"'", call. = FALSE)
    }
    
    parameter <- match.arg(parameter)
    
    extract_link(family, inverse = inverse)
}

`cox_ph_link` <- function(family, parameter = c("location", "mu"),
                       inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "Cox PH")) {
        stop("'family' is not '\"Cox PH\"'", call. = FALSE)
    }
    
    parameter <- match.arg(parameter)
    
    extract_link(family, inverse = inverse)
}

##' @rdname link
##' @export
`extract_link` <- function(family, ...) {
    UseMethod("extract_link")
}

##' @rdname link
##' 
##' @param family a family object.
##' @param inverse logical; if `TRUE` return the link function or if `FALSE`
##'   return the inverse of the link function.
##' @export
`extract_link.family` <- function(family, inverse = FALSE, ...) {
    fun <- if (isTRUE(inverse)) {
               family[["linkinv"]]
           } else {
               family[["linkfun"]]
           }

    fun # return
}

`gaulss_link` <- function(family, parameter = c("location", "scale", "mu", "sigma"),
                          inverse = FALSE) {
    if (!inherits(family, "family")) {
        stop("'family' is not a family object", call. = FALSE)
    }
    if (!identical(family[["family"]], "gaulss")) {
        stop("'family' is not '\"gaulss\"'", call. = FALSE)
    }

    parameter <- match.arg(parameter)

    lobj <- switch(parameter,
                   location = family[["linfo"]][[1L]],
                   mu       = family[["linfo"]][[1L]],
                   scale    = family[["linfo"]][[2L]],
                   sigma    = family[["linfo"]][[2L]])

    fun <- if (isTRUE(inverse)) {
               lobj[["linkinv"]]
           } else {
               lobj[["linkfun"]]
           }

    fun # return
}

##' Family method for GAM objects
##'
##' Provides a [stats::family()] method for a range of GAM objects.
##'
##' @param object a fitted model. Models fitted by [mgcv::gam()], [mgcv::bam()],
##'   and [mgcv::gamm()].
##' @param ... arguments passed to other methods.
##'
##' @export
`family.gam` <- function(object, ...) {
    object[["family"]]
}

##' @export
##' @rdname family.gam
`family.gamm` <- function(object, ...) {
    family(object[["gam"]])
}

##' @export
##' @rdname family.gam
`family.bam` <- function(object, ...) {
    object[["family"]]
}
