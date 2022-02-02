#' Estimated values for parametric model terms
#'
#' @param object a fitted model object.
#' @param term character; which parametric term whose effects are evaluated.
#' @param ci_level numeric; the coverage required for the confidence interval.
#'   Currently ignored.
#' @param ... arguments passed to other methods.
#'
#' @importFrom stats delete.response
#' @importFrom tibble as_tibble add_column
#' @importFrom rlang .data
#' @importFrom dplyr mutate bind_cols bind_rows
#'
#' @export
`parametric_effects` <- function(object, ...) {
    UseMethod("parametric_effects")
}

#' @export
`parametric_effects.gam` <- function(object, term = NULL, n = 100,
                                     data = NULL,
                                     unconditional = FALSE,
                                     unnest = TRUE,
                                     partial_match = FALSE,
                                     ci_level = 0.95, ...) {
    tt <- object$pterms       # get model terms object
    tt <- delete.response(tt) # remove response so easier to work with
    vars <- parametric_terms(object) # vector of names of model terms
    mgcv_names <- names(vars) # this is how mgcv refers to the terms

    if (!any(valid_term <- term %in% vars)) {
        stop(sprintf("Term is not in the parametric part of model: <%s>",
                     term))
    }

    mf <- model.frame(object)  # data used to fit model

    is_fac <- is_factor_term(tt, term)

    ## match the specific term, with term names mgcv actually uses
    ## for example in a model with multiple linear predictors, terms in
    ## nth linear predictor (for n > 1) get appended ".{n-1}""
    ind <- match(term, vars)

    if (is_fac) {
        ## check order of term; if > 1 interaction and not handled
        ord <- attr(tt, "order")[match(term, attr(tt, "term.labels"))]
        if (ord > 1) {
            stop("Interaction terms are not currently supported.")
        }
        ## facs <- attr(tt, 'factors')[, term]
        newd <- unique(mf[, term, drop = FALSE])
        other_vars <- setdiff(names(mf), term)
        other_data <- as_tibble(lapply(mf[other_vars], value_closest_to_median))
        pred_data <- exec(expand_grid, !!!list(newd, other_data))
        effects <- as.data.frame(predict(object, newdata = pred_data,
                             type = 'terms',
                             terms = term, se = TRUE,
                             unconditional = unconditional,
                             newdata.guaranteed = FALSE))
        effects <- setNames(effects, c("partial", "se"))
        effects <- as_tibble(effects)
        nr <- NROW(effects)
        newd <- setNames(newd, "value")
        effects <- bind_cols(term = rep(term, nr),
                               type = rep("factor", nr),
                               newd, effects)
    } else {
        ## take the actual mgcv version of the names for the `terms` argument
        effects <- as.data.frame(predict(object, newdata = mf, type = 'terms',
                                           terms = mgcv_names[ind], se = TRUE,
                                           unconditional = unconditional))
        effects <- setNames(effects, c("partial", "se"))
        effects <- as_tibble(effects)
        nr <- NROW(effects)
        effects <- bind_cols(term = rep(term, nr),
                               type = rep("numeric", nr),
                               value = mf[[term]],
                               effects)
    }

    ## add confidence interval -- be consistent and don't add this, but could?
    ## effects <- mutate(effects,
    ##                     upper = .data$partial + (2 * .data$se),
    ##                     lower = .data$partial - (2 * .data$se))

    class(effects) <- c("parametric_effects", class(effects))
    effects                           # return
}
