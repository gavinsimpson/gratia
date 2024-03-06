#' Evaluate a smooth
#'
#' @description
#' `r lifecycle::badge('deprecated')` Evaluate a smooth at a grid of evenly
#' spaced value over the range of the covariate associated with the smooth.
#' Alternatively, a set of points at which the smooth should be evaluated can be
#' supplied.
#'
#' @details
#' `r lifecycle::badge('deprecated')` `evaluate_smooth()` is deprecated in
#' favour of [smooth_estimates()], which provides a cleaner way to evaluate a
#' smooth over a range of covariate values. [smooth_estimates()] can handle a
#' much wider range of models than `evaluate_smooth()` is capable of and
#' [smooth_estimates()] is much easier to extend to handle new smooth types.
#'
#' Most code that uses `evaluate_smooth()` should work simply by changing the
#' function call to [smooth_estimates()]. However, there are some differences:
#'
#' * the `newdata` argument becomes `data`
#'
#' @param object an object of class `"gam"` or `"gamm"`.
#' @param ... arguments passed to other methods.
#'
#' @return A data frame, which is of class `"evaluated_1d_smooth"` or
#'   `evaluated_2d_smooth`, which inherit from classes `"evaluated_smooth"`
#'   and `"data.frame"`.
#'
#' @importFrom mgcv PredictMat exclude.too.far
#' @importFrom stats setNames
#'
#' @export
`evaluate_smooth` <- function(object, ...) {
  lifecycle::deprecate_stop(
    "0.7.0",
    "evaluate_smooth()",
    "smooth_estimates()"
  )
}

#' Evaluate parametric model terms
#'
#' @description
#' `r lifecycle::badge('deprecated')` Returns values of parametric model terms
#' at values of factor terms and over a grid of covariate values for linear
#' parametric terms. This function is now deprecated in favour of
#' [parametric_effects()].
#'
#' @export
`evaluate_parametric_term` <- function(object, ...) {
  lifecycle::deprecate_warn(
    "0.9.0",
    "evaluate_parametric_term()",
    "parametric_estimates()"
  )
  UseMethod("evaluate_parametric_term")
}

#' @param term character; which parametric term whose effects are evaluated
#'
#' @param unconditional logical; should confidence intervals include the
#'   uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian
#'   covariance matrix will be used.
#'
#' @inheritParams evaluate_smooth
#' @rdname evaluate_parametric_term
#'
#' @importFrom stats delete.response
#' @importFrom tibble as_tibble add_column
#' @importFrom rlang .data
#' @importFrom dplyr mutate bind_cols bind_rows
#'
#' @export
`evaluate_parametric_term.gam` <- function(object, term, unconditional = FALSE,
                                           ...) {
  tt <- object$pterms # get parametric terms
  tt <- delete.response(tt) # remove response so easier to work with
  vars <- parametric_terms(object)
  mgcv_names <- names(vars) # this is how mgcv refers to the terms

  if (length(term) > 1L) {
    term <- term[1L]
    warning(sprintf(
      "More than one `term` requested; using the first <%s>",
      term
    ))
  }
  if (isFALSE(term %in% vars)) {
    stop(sprintf(
      "Term is not in the parametric part of model: <%s>",
      term
    ))
  }

  mf <- model.frame(object) # data used to fit model

  ## is_fac <- is.factor(mf[[term]]) # is term a factor?
  is_fac <- is_factor_term(tt, term)

  ## match the specific term, with term names mgcv actually uses
  ## for example in a model with multiple linear predictors, terms in
  ## nth linear predictor (for n > 1) get appended .{n-1}
  ind <- match(term, vars)

  if (is_fac) {
    ## check order of term; if > 1 interaction and not handled
    ord <- attr(tt, "order")[match(term, attr(tt, "term.labels"))]
    if (ord > 1) {
      stop("Interaction terms are not currently supported.")
    }
    ## facs <- attr(tt, 'factors')[, term]
    newd <- unique(mf[, term, drop = FALSE])
    ## ##fac_vars <- rownames(facs)
    ## fac_vars <- names(facs)[as.logical(facs)]
    ## facs <- attr(tt, 'factors')[, term]
    ## newd <- unique(mf[, names(facs)[as.logical(facs)], drop = FALSE])
    ## ##fac_vars <- rownames(facs)
    ## fac_vars <- names(facs)[as.logical(facs)]
    ## ##newd <- unique(mf[, fac_vars, drop = FALSE])
    other_vars <- setdiff(names(mf), term)
    other_data <- as_tibble(lapply(mf[other_vars], value_closest_to_median))
    pred_data <- exec(expand_grid, !!!list(newd, other_data))
    evaluated <- as.data.frame(predict(object,
      newdata = pred_data,
      type = "terms",
      terms = term, se = TRUE,
      unconditional = unconditional,
      newdata.guaranteed = FALSE
    ))
    evaluated <- setNames(evaluated, c("partial", "se"))
    evaluated <- as_tibble(evaluated)
    nr <- NROW(evaluated)
    newd <- setNames(newd, "value")
    evaluated <- bind_cols(
      term = rep(term, nr),
      type = rep("factor", nr),
      newd, evaluated
    )
  } else {
    ## take the actual mgcv version of the names for the `terms` argument
    evaluated <- as.data.frame(predict(object,
      newdata = mf, type = "terms",
      terms = mgcv_names[ind], se = TRUE,
      unconditional = unconditional
    ))
    evaluated <- setNames(evaluated, c("partial", "se"))
    evaluated <- as_tibble(evaluated)
    nr <- NROW(evaluated)
    evaluated <- bind_cols(
      term = rep(term, nr),
      type = rep("numeric", nr),
      value = mf[[term]],
      evaluated
    )
  }

  ## add confidence interval -- be consistent and don't add this, but could?
  ## evaluated <- mutate(evaluated,
  ##                     upper = .data$partial + (2 * .data$se),
  ##                     lower = .data$partial - (2 * .data$se))

  class(evaluated) <- c("evaluated_parametric_term", class(evaluated))
  evaluated # return
}
