#' Find the names of model terms
#'
#' Returns the names of any terms in a model, without needing to call
#' `summary()`. The list of model terms is especially useful when predicting
#' from a [mgcv::gam()] model using the `exclude` or `terms` argument of
#' [mgcv::predict.gam()] or [mgcv::predict.bam()].
#'
#' From the point of view of *gratia*, models contain two types of term:
#' 1. parametric terms, and
#' 2. smooth terms.
#'
#' If we consider the formula `y ~ fac + s(x2, by = fac) + s(x0)`, for a factor
#' `fac` with three levels, there are seven terms in the model:
#' 1. the model constant term, with name `"(Intercept)"`,
#' 2. the parametric factor term, with names
#'     * `fac2`,
#'     * `fac3`,
#' 3. the univariate smooth of `x0`, named `"s(x0)"`, and
#' 4. the three factor-by smooths with names
#'     * `"s(x2):fac1"`,
#'     * `"s(x2):fac2"`, and
#'     * `"s(x2):fac3"`.`
#'
#' `model_terms()` will return a vector of those names.
#'
#' @param object a fitted model.
#' @param ... arguments to be passed to other methods; not currently used.
#'
#' @export
#' @return A character vector of model terms.
#'
#' @examples
#' load_mgcv()
#'
#' m <- gam(y ~ fac + s(x2, by = fac) + s(x0),
#'   data = su_eg4, method = "REML")
#'
#' # return the names of terms in this model
#' model_terms(m)
`model_terms` <- function(object, ...) {
  UseMethod("model_terms")
}

#' @export
#' @rdname model_terms
#' @importFrom stats coef
`model_terms.gam` <- function(object, ...) {
  coefs <- names(coef(object))
  para_terms <- coefs[!match_smoother(coefs)]
  sm_terms <- smooths(object)
  c(para_terms, sm_terms)
}

#' @export
#' @rdname model_terms
`model_terms.gamm` <- function(object, ...) {
  model_terms(object$gam)
}

#' @export
#' @rdname model_terms
`model_terms.gamm4` <- function(object, ...) {
  model_terms(object$gam)
}

#' @export
#' @rdname model_terms
`model_terms.lm` <- function(object, ...) {
  names(coef(object))
}

#' Extract the model constant term
#'
#' `r lifecycle::badge("experimental")` Extracts the model constant term(s),
#' the model intercept, from a fitted model object.
#'
#' @param model a fitted model for which a `coef()` method exists.
#' @param lp numeric; which linear predictors to extract constant terms for.
#' @param ... arguments passed to other methods.
#'
#' @export
#' @importFrom stats coef
#' @examples
#' \dontshow{
#' op <- options(digits = 4)
#' }
#' load_mgcv()
#'
#' # simulate a small example
#' df <- data_sim("eg1", seed = 42)
#'
#' # fit the GAM
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' # extract the estimate of the constant term
#' model_constant(m)
#' # same as coef(m)[1L]
#' coef(m)[1L]
#'
#' \dontshow{
#' options(op)
#' }
#' @export
`model_constant` <- function(model, ...) {
  UseMethod("model_constant")
}

#' @export
#' @rdname model_constant
`model_constant.gam` <- function(model, lp = NULL, ...) {
  lss_idx <- lss_eta_index(model)
  n_lp <- n_eta(model)
  if (!is.null(lp)) {
    if (any(lp > n_lp)) {
      stop("invalid linear predictor")
    }
    lp <- lp[lp %in% seq_len(n_lp)]
  } else {
    lp <- seq_len(n_lp)
  }
  lss_idx <- lss_idx[lp]
  idx <- vapply(lss_idx, FUN = `[`, FUN.VALUE = numeric(1), 1)
  b <- coef(model)[idx] |>
    unname()
  attr(b, "par_names") <- lss_parameters(model)
  b
}

#' @export
#' @rdname model_constant
`model_constant.gamlss` <- function(model, ...) {
  .NotYetImplemented()
}

#' @export
#' @rdname model_constant
#' @importFrom stats coef
`model_constant.glm` <- function(model, ...) {
  coef(model)[1L]
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
`model_vars.gam` <- function(model, ...) {
  # want a vector of variables involved in the model formula.
  # Don't want this `attr(terms(model), "term.labels")` ! as this returns
  # model terms not variable names. Use all.vars() on `pred.formula` for
  # a GAM(M) model
  all.vars(model$pred.formula)
}

#' @export
#' @rdname model_vars
`model_vars.default` <- function(model, ...) {
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
`model_vars.bam` <- function(model, ...) {
  # want a vector of variables involved in the model formula.
  # Don't want this `attr(terms(model), "term.labels")` ! as this returns
  # model terms not variable names. Use all.vars() on `pred.formula` for
  # a GAM(M) model
  all.vars(model$pred.formula)
}

#' @export
#' @rdname model_vars
`model_vars.gamm` <- function(model, ...) {
  # want a vector of variables involved in the model formula.
  # Don't want this `attr(terms(model), "term.labels")` ! as this returns
  # model terms not variable names. Use all.vars() on `pred.formula` for
  # a GAM(M) model
  model_vars(model[["gam"]]$pred.formula)
}

#' @export
#' @rdname model_vars
`model_vars.gamm4` <- function(model, ...) {
  # this is here for when Simon actually classes gamm4 objects
  # want a vector of variables involved in the model formula.
  # Don't want this `attr(terms(model), "term.labels")` ! as this returns
  # model terms not variable names. Use all.vars() on `pred.formula` for
  # a GAM(M) model
  model_vars(model[["gam"]]$pred.formula)
}

#' @export
#' @rdname model_vars
`model_vars.list` <- function(model, ...) {
  # want a vector of variables involved in the model formula.
  # Don't want this `attr(terms(model), "term.labels")` ! as this returns
  # model terms not variable names. Use all.vars() on `pred.formula` for
  # a GAM(M) model
  if (!is_gamm4(model)) {
    stop("Don't know how to handle generic list objects.")
  }
  model_vars(model[["gam"]]$pred.formula)
}
