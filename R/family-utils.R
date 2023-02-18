#' Extract link and inverse link functions from models
#'
#' Returns the link or its inverse from an estimated model, and provides a
#' simple way to extract these functions from complex models with multiple
#' links, such as location scale models.
#'
#' @param object a family object or a fitted model from which to extract the
#'   family object.  Models fitted by [stats::glm()], [mgcv::gam()],
#'   [mgcv::bam()], [mgcv::gamm()], and [gamm4::gamm4()] are currently
#'   supported.
#' @param parameter character; which parameter of the distribution. Usually
#'   `"location"` but `"scale"` and `"shape"` may be provided for location
#'   scale models. Other options include `"mu"` as a synonym for `"location"`,
#'   `"sigma"` for the scale parameter in [mgcv::gaulss()], `"pi"` for the
#'   zero-inflation term in [mgcv::ziplss()], `"power"` for the
#'   [mgcv::twlss()] power parameter, `"xi"`, the shape parameter for
#'   [mgcv::gevlss()], `"epsilon"` or `"skewness"` for the skewness and
#'   `"delta"` or `"kurtosis"` for the kurtosis parameter for
#'   [mgcv::shash()], or `"theta"` for the scale parameter of [mgcv::gammals()].
#' @param which_eta numeric; the linear predictor to extract for families
#'   [mgcv::mvn()] and [mgcv::multinom()].
#' @param ... arguments passed to other methods.
#'
#' @author Gavin L. Simpson
#'
#' @export
#'
#' @examples
#' load_mgcv()
#'
#' link(gaussian())
#' link(nb())
#'
#' inv_link(nb())
#'
#' dat <- data_sim("eg1", seed = 4234)
#' mod <- gam(list(y ~ s(x0) + s(x1) + s(x2) + s(x3), ~ 1), data = dat,
#'            family = gaulss)
#'
#' link(mod, parameter = "scale")
#' inv_link(mod, parameter = "scale")
#'
#' ## Works with `family` objects too
#' link(shash(), parameter = "skewness")
`link` <- function(object, ...) {
    UseMethod("link")
}

#' @rdname link
#' @export
`link.family` <- function(object, parameter = NULL, which_eta = NULL, ...) {
    ## extract the link function
    lfun <- get_link_function(object, parameter = parameter, inverse = FALSE,
                              which_eta = which_eta, ...)
    ## return
    lfun
}

#' @rdname link
#' @export
#' @importFrom stats family
`link.gam` <- function(object, parameter = NULL, which_eta = NULL, ...) {
    link(family(object), parameter = parameter, which_eta = which_eta, ...)
}

#' @rdname link
#' @export
`link.bam` <- function(object, parameter = NULL, which_eta = NULL, ...) {
    NextMethod()
}

#' @rdname link
#' @export
`link.gamm` <- function(object, ...) {
    link(object[["gam"]])
}

#' @rdname link
#' @export
#' @importFrom stats family
`link.glm` <- function(object, ...) {
    link(family(object), ...)
}

#' @rdname link
#' @export
#' @importFrom stats family
`link.list` <- function(object, ...) {
    if (!is_gamm4(object)) {
        stop("`object` does not appear to a `gamm4` model object",
             call. = FALSE)
    }
    link(family(object[["gam"]], ...))
}

#' @rdname link
#' @export
`inv_link` <- function(object, ...) {
    UseMethod("inv_link")
}

#' @rdname link
#' @export
`inv_link.family` <- function(object, parameter = NULL, which_eta = NULL, ...) {
    ## extract the link function
    lfun <- get_link_function(object, parameter = parameter, inverse = TRUE,
                              which_eta = which_eta, ...)

    ## return
    lfun
}

#' @rdname link
#' @export
#' @importFrom stats family
`inv_link.gam` <- function(object, parameter = NULL, which_eta = NULL, ...) {
    inv_link(family(object), parameter = parameter, which_eta = which_eta, ...)
}

#' @rdname link
#' @export
`inv_link.bam` <- function(object, parameter = NULL, which_eta = NULL,
                           ...) {
    NextMethod()
}

#' @rdname link
#' @export
`inv_link.gamm` <- function(object, ...) {
    inv_link(object[["gam"]])
}

#' @rdname link
#' @export
#' @importFrom stats family
`inv_link.list` <- function(object, ...) {
    if (!is_gamm4(object)) {
        stop("`object` does not appear to a `gamm4` model object",
             call. = FALSE)
    }
    inv_link(family(object[["gam"]], ...))
}

#' @rdname link
#' @export
#' @importFrom stats family
`inv_link.glm` <- function(object, ...) {
    inv_link(family(object), ...)
}

#' Extract family objects from models
#'
#' Provides a [stats::family()] method for a range of GAM objects.
#'
#' @param object a fitted model. Models fitted by [mgcv::gam()], [mgcv::bam()],
#'   [mgcv::gamm()], and [gamm4::gamm4()] are currently supported.
#' @param ... arguments passed to other methods.
#'
#' @export
`family.gam` <- function(object, ...) {
    object[["family"]]
}

#' @export
#' @rdname family.gam
`family.gamm` <- function(object, ...) {
    family(object[["gam"]])
}

#' @export
#' @rdname family.gam
`family.bam` <- function(object, ...) {
    object[["family"]]
}

#' @export
#' @rdname family.gam
`family.list` <- function(object, ...) {
    if (!is_gamm4(object)) {
        stop("`object` does not appear to a `gamm4` model object",
             call. = FALSE)
    }
    family(object[["gam"]])
}

#' Extracts the type of family in a consistent way
#'
#' @param object an R object. Currently [family()] objects and anything with a
#'   [family()] method.
#' @param ... arguments passed to other methods.
#' @export
`family_type` <- function(object, ...) {
    UseMethod("family_type")
}

#' @export
#' @rdname family_type
family_type.family <- function(object, ...) {
    fn <- family_name(object)
    fn <- tolower(gsub("\\([[:alnum:]\\.,]+\\)", "", fn))
    fn <- gsub("\\s", "_", fn)
    fn
}

#' @export
#' @rdname family_type
`family_type.default` <- function(object, ...) {
    family_type(family(object))
}

## Extracts the link or inverse link function from a family object
#' @export
#' @rdname link
`extract_link` <- function(family, ...) {
    UseMethod("extract_link")
}

#' @export
#' @rdname link
#'
#' @param family a family object, the result of a call to [family()].
#' @param inverse logical; return the inverse of the link function?
`extract_link.family` <- function(family, inverse = FALSE, ...) {
    fun <- if (isTRUE(inverse)) {
        family[["linkinv"]]
    } else {
        family[["linkfun"]]
    }

    fun # return
}

#' @export
#' @rdname link
`extract_link.general.family` <- function(family, parameter, inverse = FALSE,
                                          which_eta = NULL, ...) {
    ## check `family`
    ## Note: don't pass a `type` here as we only want a check for being a
    ##       family object
    stop_if_not_family(family)

    linfo <- family[["linfo"]] # pull out linfo for easy access

    ## some general families don't have $linfo
    if (is.null(linfo)) {
        fun <- extract_link.family(family, inverse = inverse)
    } else if (family[["family"]] %in% c("Multivariate normal", "multinom")) {
        if (is.null(which_eta)) {
            stop("Which linear predictor not specified; see 'which_eta'",
                 .call. = FALSE)
        }
        len_linfo <- length(linfo)
        if (which_eta > len_linfo || which_eta < 1) {
            stop("Invalid 'which_eta': must be between 1 and ", len_linfo, ".",
                 call. = FALSE)
        }
        if (length(which_eta) > 1L) {
            which_eta <- rep(which_eta, length.out = 1L)
            warning("Multiple values passed to 'which_eta';",
                    " using only the first.")
        }
        lobj <- linfo[[which_eta]]
        fun <- if (isTRUE(inverse)) {
            lobj[["linkinv"]]
        } else {
            lobj[["linkfun"]]
        }
    } else {
        # linfo is ordered; 1: location; 2: scale or sigma, 3: shape, power, etc
        # (check pi is right greek letter for zero-inflation!)
        lobj <- switch(parameter,
                       location  = linfo[[1L]],
                       mu        = linfo[[1L]],
                       scale     = linfo[[2L]],
                       sigma     = linfo[[2L]],
                       theta     = linfo[[2L]], # scale parameter for gammals()
                       shape     = linfo[[3L]],
                       power     = linfo[[3L]], # power for twlss()
                       xi        = linfo[[3L]], # xi for gevlss()
                       pi        = linfo[[2L]], # pi for zero-inflation
                       epsilon   = linfo[[3L]], # skewness for shash
                       skewness  = linfo[[3L]], # skewness for shash
                       delta     = linfo[[4L]], # kurtosis for shash
                       kurtosis  = linfo[[4L]]  # kurtosis for shash
                       )

        fun <- if (isTRUE(inverse)) {
            lobj[["linkinv"]]
        } else {
            lobj[["linkfun"]]
        }
    }
    fun # return
}

## Other internal functions ---------------------------------------------------

## Workhorse link extractor
`get_link_function` <- function(object, parameter = "location",
                                inverse = FALSE, which_eta = NULL) {
    inverse <- as.logical(inverse)
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
    if (grepl("^scaled t", distr, ignore.case = TRUE)) {
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
    lfun <-
      switch(distr,
             gaussian = gaussian_link(object, parameter, inverse = inverse),
             poisson = poisson_link(object, parameter, inverse = inverse),
             binomial = binomial_link(object, parameter, inverse = inverse),
             Gamma = gamma_link(object, parameter, inverse = inverse),
             inverse.gaussian = inverse_gaussian_link(object, parameter,
                                                      inverse = inverse),
             quasi = quasi_link(object, parameter, inverse = inverse),
             quasipoisson = quasi_poisson_link(object, parameter,
                                               inverse = inverse),
             quasibinomial = quasi_binomial_link(object, parameter,
                                                 inverse = inverse),
             nb = nb_link(object, parameter, inverse = inverse),
             tweedie = tw_link(object, parameter, inverse = inverse),
             beta = beta_link(object, parameter, inverse = inverse),
             scaled_t = scaled_t_link(object, parameter, inverse = inverse),
             ocat = ocat_link(object, parameter, inverse = inverse),
             zip = zip_link(object, parameter, inverse = inverse),
             cox_ph = cox_ph_link(object, parameter, inverse = inverse),
             gaulss = gaulss_link(object, parameter, inverse = inverse),
             twlss = twlss_link(object, parameter, inverse = inverse),
             gevlss = gevlss_link(object, parameter, inverse = inverse),
             gammals = gammals_link(object, parameter, inverse = inverse),
             gumbls = gumbls_link(object, parameter, inverse = inverse),
             ziplss = ziplss_link(object, parameter, inverse = inverse),
             mvn = mvn_link(object, parameter, inverse = inverse,
                            which_eta = which_eta),
             multinom = multinom_link(object, parameter, inverse = inverse,
                                      which_eta = which_eta),
             shash = shash_link(object, parameter, inverse = inverse)
             )

    ## return
    lfun
}

## Internal link extractor functions

`gaussian_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    stop_if_not_family(family, type = "gaussian")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`poisson_link` <- function(family, parameter = c("location", "mu"),
                           inverse = FALSE) {
    stop_if_not_family(family, type = "poisson")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`binomial_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    stop_if_not_family(family, type = "binomial")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`gamma_link` <- function(family, parameter = c("location", "mu"),
                         inverse = FALSE) {
    stop_if_not_family(family, type = "Gamma")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`inverse_gaussian_link` <- function(family, parameter = c("location", "mu"),
                                    inverse = FALSE) {
    stop_if_not_family(family, type = "inverse.gaussian")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`quasi_link` <- function(family, parameter = c("location", "mu"),
                         inverse = FALSE) {
    stop_if_not_family(family, type = "quasi")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`quasi_poisson_link` <- function(family, parameter = c("location", "mu"),
                                 inverse = FALSE) {
    stop_if_not_family(family, type = "quasipoisson")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`quasi_binomial_link` <- function(family, parameter = c("location", "mu"),
                                  inverse = FALSE) {
    stop_if_not_family(family, type = "quasibinomial")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`nb_link` <- function(family, parameter = c("location", "mu"),
                      inverse = FALSE) {
    stop_if_not_family(family, type = "Negative Binomial")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`tw_link` <- function(family, parameter = c("location", "mu"),
                      inverse = FALSE) {
    stop_if_not_family(family, type = "Tweedie")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`beta_link` <- function(family, parameter = c("location", "mu"),
                        inverse = FALSE) {
    stop_if_not_family(family, type = "Beta regression")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`scaled_t_link` <- function(family, parameter = c("location", "mu"),
                            inverse = FALSE) {
    stop_if_not_family(family, type = "scaled t")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`ocat_link` <- function(family, parameter = c("location", "mu"),
                                       inverse = FALSE) {
    stop_if_not_family(family, type = "Ordered Categorical")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`zip_link` <- function(family, parameter = c("location", "mu"),
                       inverse = FALSE) {
    stop_if_not_family(family, type = "zero inflated Poisson")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

`cox_ph_link` <- function(family, parameter = c("location", "mu"),
                       inverse = FALSE) {
    stop_if_not_family(family, type = "Cox PH")

    parameter <- match.arg(parameter)

    extract_link(family, inverse = inverse)
}

## Location scale shape families -----------------------------------------------

`gaulss_link` <- function(family,
                          parameter = c("location", "scale", "mu", "sigma"),
                          inverse = FALSE) {
    stop_if_not_family(family, type = "gaulss")

    parameter <- match.arg(parameter)

    fun <- extract_link(family, parameter = parameter, inverse = inverse)
    fun # return
}

`twlss_link` <- function(family,
                         parameter = c("location", "scale",
                                       "mu", "sigma", "power"),
                         inverse = FALSE) {
    stop_if_not_family(family, type = "twlss")

    parameter <- match.arg(parameter)

    fun <- extract_link(family, parameter = parameter, inverse = inverse)
    fun # return
}

`gevlss_link` <- function(family,
                         parameter = c("location", "scale", "shape",
                                       "mu", "sigma", "xi"),
                         inverse = FALSE) {
    stop_if_not_family(family, type = "gevlss")

    parameter <- match.arg(parameter)

    fun <- extract_link(family, parameter = parameter, inverse = inverse)
    fun # return
}

`gammals_link` <- function(family,
                           parameter = c("location", "scale", "mu", "theta"),
                           inverse = FALSE) {
    stop_if_not_family(family, type = "gammals")

    parameter <- match.arg(parameter)

    fun <- extract_link(family, parameter = parameter, inverse = inverse)
    fun # return
}

`gumbls_link` <- function(family,
                          parameter = c("location", "scale", "mu"),
                          inverse = FALSE) {
    stop_if_not_family(family, type = "gumbls")

    parameter <- match.arg(parameter)

    fun <- extract_link(family, parameter = parameter, inverse = inverse)
    fun # return
}

`ziplss_link` <- function(family,
                          parameter = c("location", "scale", "mu", "pi"),
                          inverse = FALSE) {
    stop_if_not_family(family, type = "ziplss")

    parameter <- match.arg(parameter)

    fun <- extract_link(family, parameter = parameter, inverse = inverse)
    fun # return
}

`mvn_link` <- function(family, parameter = "location", inverse = FALSE,
                       which_eta = NULL) {
    stop_if_not_family(family, type = "Multivariate normal")

    parameter <- match.arg(parameter)

    fun <- extract_link(family, parameter = parameter, inverse = inverse,
                        which_eta = which_eta)
    fun # return
}

`multinom_link` <- function(family, parameter = "location", inverse = FALSE,
                            which_eta = NULL) {
    stop_if_not_family(family, type = "multinom")

    parameter <- match.arg(parameter)

    fun <- extract_link(family, parameter = parameter, inverse = inverse,
                        which_eta = which_eta)
    fun # return
}

`shash_link` <- function(family,
                         parameter = c("location", "scale", "skewness",
                                       "kurtosis", "mu", "sigma", "epsilon",
                                       "delta"),
                         inverse = FALSE) {
    stop_if_not_family(family, type = "shash")

    parameter <- match.arg(parameter)

    fun <- extract_link(family, parameter = parameter, inverse = inverse)
    fun # return
}

## Utility function for consistent checks and errors
##
## - only checks type if `type` is not NULL
`stop_if_not_family` <- function(object, type = NULL) {
    ## check if object is a family; throw error if not
    if (!inherits(object, c("family", "extended.family", "general.family"))) {
        stop("'family' is not a family object", call. = FALSE)
    }

    if (! is.null(type)) {
        fam <- object[["family"]]
        ## check that family is of the correct type
        ##  - need to handle a couple of special types
        special <- c("Tweedie", "Negative Binomial", "negative binomial",
                     "Scaled t", "scaled t")
        if (type %in% special) {
            if (!grepl(type, fam, ignore.case = TRUE)) {
                stop("'family' is not of type '\"", type, "\"'", call. = FALSE)
            }
        } else {
            if (!identical(fam, type)) {
                stop("'family' is not of type '\"", type, "\"'", call. = FALSE)
            }
        }
    }

    TRUE
}

#' Name of family used to fit model
#'
#' Extracts the name of the family used to fit the supplied model.
#'
#' @param object an R object.
#' @param ... arguments passed to other methods.
#'
#' @return A character vector containing the family name.
#'
#' @export
`family_name` <- function(object, ...) {
    UseMethod("family_name")
}

#' @export
`family_name.glm` <- function(object, ...) {
    family(object)[["family"]]
}

#' @export
`family_name.gam` <- function(object, ...) {
    family(object)[["family"]]
}

#' @export
`family_name.gamm` <- function(object, ...) {
    family(object)[["family"]]
}

#' @export
`family_name.family` <- function(object, ...) {
    object[["family"]]
}

#' @export
#' @importFrom stats family
`family_name.list` <- function(object, ...) {
    if (!is_gamm4(object)) {
        stop("`object` does not appear to a `gamm4` model object",
             call. = FALSE)
    }
    family_name(object[["gam"]], ...)
}
