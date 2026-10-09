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
#'   [mgcv::shash()], or `"phi"` for the scale parameter of [mgcv::gammals()] &
#'   [mgcv::twlss()]. `NULL` selects the first parameter. Only parameters and
#'   aliases supported by the supplied family are accepted. For [mgcv::twlss()],
#'   `"power"` selects the second linear predictor and `"scale"`, `"sigma"`,
#'   and `"phi"` select the third.
#' @param which_eta numeric; the integer index of the linear predictor to
#'   extract for families [mgcv::mvn()] and [mgcv::multinom()]. Defaults to
#'   the first linear predictor. If multiple indices are supplied, the first
#'   is used with a warning.
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
#' mod <- gam(list(y ~ s(x0) + s(x1) + s(x2) + s(x3), ~1),
#'   data = dat,
#'   family = gaulss
#' )
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
  extract_link(object,
    parameter = parameter, inverse = FALSE,
    which_eta = which_eta, ...
  )
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
  link(object[["gam"]], ...)
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
      call. = FALSE
    )
  }
  link(family(object[["gam"]]), ...)
}

#' @rdname link
#' @export
`inv_link` <- function(object, ...) {
  UseMethod("inv_link")
}

#' @rdname link
#' @export
`inv_link.family` <- function(object, parameter = NULL, which_eta = NULL, ...) {
  extract_link(object,
    parameter = parameter, inverse = TRUE,
    which_eta = which_eta, ...
  )
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
  inv_link(object[["gam"]], ...)
}

#' @rdname link
#' @export
#' @importFrom stats family
`inv_link.list` <- function(object, ...) {
  if (!is_gamm4(object)) {
    stop("`object` does not appear to a `gamm4` model object",
      call. = FALSE
    )
  }
  inv_link(family(object[["gam"]]), ...)
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
      call. = FALSE
    )
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
  fn <- tolower(gsub("\\([[:alnum:]\\.,=]+\\)", "", fn))
  fn <- gsub("\\s", "_", fn)
  fn <- gsub("\\.", "_", fn)
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
`extract_link.family` <- function(family, inverse = FALSE, parameter = NULL,
                                  which_eta = NULL, ...) {
  stop_if_not_family(family)
  linfo <- family[["linfo"]]

  if (is.null(linfo)) {
    parameter <- match.arg(parameter, c("location", "mu"))
    lobj <- family
  } else if (family_name(family) %in% c("Multivariate normal", "multinom")) {
    parameter <- match.arg(parameter, c("location", "mu"))
    if (is.null(which_eta)) {
      which_eta <- 1L
    }
    if (is.numeric(which_eta) && length(which_eta) > 1L) {
      warning("Multiple values passed to 'which_eta'; using only the first.",
        call. = FALSE
      )
      which_eta <- which_eta[[1L]]
    }
    if (!is.numeric(which_eta) || length(which_eta) != 1L ||
        !is.finite(which_eta) || which_eta != trunc(which_eta) ||
        which_eta < 1L || which_eta > length(linfo)) {
      stop("Invalid 'which_eta': must be an integer between 1 and ",
        length(linfo), ".", call. = FALSE
      )
    }
    lobj <- linfo[[which_eta]]
  } else {
    parameters <- link_parameter_map(family)
    parameter <- match.arg(parameter, names(parameters))
    index <- parameters[[parameter]]
    if (index > length(linfo)) {
      stop("No link information for parameter '", parameter, "'.",
        call. = FALSE
      )
    }
    lobj <- linfo[[index]]
  }

  component <- if (isTRUE(inverse)) "linkinv" else "linkfun"
  fun <- lobj[[component]]
  if (!is.function(fun)) {
    stop("Family does not supply a '", component, "' function",
      if (!is.null(linfo)) paste0(" for parameter '", parameter, "'") else "",
      ".", call. = FALSE
    )
  }
  fun
}

#' @export
#' @rdname link
`extract_link.general.family` <- function(family, parameter = NULL,
                                          inverse = FALSE, which_eta = NULL,
                                          ...) {
  extract_link.family(family, inverse = inverse, parameter = parameter,
    which_eta = which_eta, ...
  )
}

## Parameter aliases and their linear predictor indices for multi-link families.
## Single-link families use their supplied functions without a name whitelist.
link_parameter_map <- function(family) {
  parameters <- switch(family_name(family),
    gaulss = c(location = 1L, mu = 1L, scale = 2L, sigma = 2L),
    gammals = c(location = 1L, mu = 1L, scale = 2L, theta = 2L, phi = 2L),
    gumbls = c(location = 1L, mu = 1L, scale = 2L),
    gevlss = c(location = 1L, mu = 1L, scale = 2L, sigma = 2L,
      shape = 3L, xi = 3L),
    shash = c(location = 1L, mu = 1L, scale = 2L, sigma = 2L,
      skewness = 3L, epsilon = 3L, kurtosis = 4L, delta = 4L),
    ziplss = c(location = 1L, mu = 1L, scale = 2L, pi = 2L),
    twlss = c(location = 1L, mu = 1L, power = 2L,
      scale = 3L, sigma = 3L, phi = 3L)
  )
  if (is.null(parameters)) {
    stop("Unknown parameter mapping for multi-link family '",
      family_name(family), "'.", call. = FALSE
    )
  }
  parameters
}

## Utility function for consistent checks and errors.
`stop_if_not_family` <- function(object) {
  if (!is_mgcv_family(object)) {
    stop("'family' is not a family object", call. = FALSE)
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
`family_name.lm` <- function(object, ...) {
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
      call. = FALSE
    )
  }
  family_name(object[["gam"]], ...)
}
