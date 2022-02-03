#' Estimated values for parametric model terms
#'
#' @param object a fitted model object.
#' @param term character; which parametric term whose effects are evaluated.
#' @param ci_level numeric; the coverage required for the confidence interval.
#'   Currently ignored.
#' @param ... arguments passed to other methods.
#'
#' @export
`parametric_effects` <- function(object, ...) {
    UseMethod("parametric_effects")
}

#' @export
#'
#' @importFrom stats delete.response
#' @importFrom tibble as_tibble add_column
#' @importFrom rlang .data
#' @importFrom purrr map_df
#' @importFrom dplyr mutate bind_cols bind_rows distinct %>%
`parametric_effects.gam` <- function(object, term = NULL,
                                     n = 100,
                                     data = NULL,
                                     unconditional = FALSE,
                                     unnest = TRUE,
                                     partial_match = FALSE,
                                     ci_level = 0.95, ...) {
    tt <- object$pterms       # get model terms object
    tt <- delete.response(tt) # remove response so easier to work with
    vars <- parametric_terms(object) # vector of names of model terms
    mgcv_names <- names(vars) # this is how mgcv refers to the terms

    # user supplied term? if provided check the stated terms are actually model
    # parametric terms
    valid_terms <- if (is.null(term)) {
        mgcv_names
    } else {
        if (!any(valid_terms <- term %in% vars)) {
            stop(sprintf("Term is not in the parametric part of model: <%s>",
                         term))
        }
        valid_terms
    }

    # check order of terms; if > 1 interaction and not handled
    ord <- attr(tt, "order")[match(valid_terms, attr(tt, "term.labels"))]
    if (any(int <- ord > 1)) {
        message("Interaction terms are not currently supported.")
        valid_terms <- valid_terms[! (ord > 1) ]
    }

    # data combinations to get all parametric terms inc factor level combos
    tbl <- data_combos(object, complete = FALSE)
    # predict model contributions for the parametric terms, using only
    # the factor combos in the data and typical values of all other terms
    # and exclude the effects of smooths as they don't change anything in
    # terms
    pred <- predict(object, type = "terms", newdata = tbl,
                    exclude = smooths(m_para_sm), se.fit = TRUE)

    # loop over the valid_terms and prepare the parametric effects
    fun <- function(term, data, pred) {
        out <- bind_cols(level = data[[term]],
                         partial = pred[["fit"]][, term],
                         se = pred[["se.fit"]][, term]) %>%
          distinct()
        nr <- nrow(out)
        is_fac <- is.factor(out$level)
        if (!is_fac) {
            out <- out %>%
              mutate(level = rep(NA_character_, times = nr))
        }
        out <- out %>%
          add_column(term = rep(term, times = nr),
                     type = rep(if_else(is_fac, "factor", "numeric"),
                                        times = nr),
                     .before = 1L)
        out
    }

    effects <- map_df(valid_terms, .f = fun, data = tbl, pred = pred)


    # mf <- model.frame(object)  # data used to fit model
# 
    # is_fac <- is_factor_term(tt, term)
# 
    # match the specific term, with term names mgcv actually uses
    # for example in a model with multiple linear predictors, terms in
    # nth linear predictor (for n > 1) get appended ".{n-1}""
    # ind <- match(term, vars)
# 
    # if (is_fac) {
        # check order of term; if > 1 interaction and not handled
        # ord <- attr(tt, "order")[match(term, attr(tt, "term.labels"))]
        # if (ord > 1) {
            # stop("Interaction terms are not currently supported.")
        # }
        # facs <- attr(tt, 'factors')[, term]
        # newd <- unique(mf[, term, drop = FALSE])
        # other_vars <- setdiff(names(mf), term)
        # other_data <- as_tibble(lapply(mf[other_vars], value_closest_to_median))
        # pred_data <- exec(expand_grid, !!!list(newd, other_data))
        # effects <- as.data.frame(predict(object, newdata = pred_data,
                            #  type = 'terms',
                            #  terms = term, se = TRUE,
                            #  unconditional = unconditional,
                            #  newdata.guaranteed = FALSE))
        # effects <- setNames(effects, c("partial", "se"))
        # effects <- as_tibble(effects)
        # nr <- NROW(effects)
        # newd <- setNames(newd, "value")
        # effects <- bind_cols(term = rep(term, nr),
                            #    type = rep("factor", nr),
                            #    newd, effects)
    # } else {
        # take the actual mgcv version of the names for the `terms` argument
        # effects <- as.data.frame(predict(object, newdata = mf, type = 'terms',
                                        #    terms = mgcv_names[ind], se = TRUE,
                                        #    unconditional = unconditional))
        # effects <- setNames(effects, c("partial", "se"))
        # effects <- as_tibble(effects)
        # nr <- NROW(effects)
        # effects <- bind_cols(term = rep(term, nr),
                            #    type = rep("numeric", nr),
                            #    value = mf[[term]],
                            #    effects)
    # }

    ## add confidence interval -- be consistent and don't add this, but could?
    ## effects <- mutate(effects,
    ##                     upper = .data$partial + (2 * .data$se),
    ##                     lower = .data$partial - (2 * .data$se))

    class(effects) <- c("parametric_effects", class(effects))
    effects                           # return
}

draw.parametric_effects <- function(object,
                                    ci_level = 0.95,
                                    constant = NULL,
                                    fun = NULL,
                                    xlab, ylab,
                                    title = NULL, subtitle = NULL,
                                    caption = NULL,
                                    rug = TRUE,
                                    position = "identity",
                                    response_range = NULL,
                                    ...) {
   # something
}

draw_paramteric_effect <- function(object, 
                                    ci_level = 0.95,
                                    constant = NULL,
                                    fun = NULL,
                                    xlab, ylab,
                                    title = NULL, subtitle = NULL,
                                    caption = NULL,
                                    rug = TRUE,
                                    position = "identity",
                                    response_range = NULL,
                                    ...) {
    # plot
    is_fac <- unique(object[["type"]]) == "factor"
    term_label <- unique(object[["term"]])

    ## If constant supplied apply it to `est`
    object <- add_constant(object, constant = constant)

    ## add a CI
    crit <- coverage_normal(ci_level)
    object <- mutate(object,
                     lower = .data$partial - (crit * .data$se),
                     upper = .data$partial + (crit * .data$se))

    ## If fun supplied, use it to transform est and the upper and lower interval
    object <- transform_fun(object, fun = fun)

    plt <- ggplot(object, aes_string(x = "level", y = "partial"))

    if (is_fac) {
        plt <- plt + geom_pointrange(aes_string(ymin = "lower", ymax = "upper"))
    } else {
        if (isTRUE(rug)) {
            plt <- plt + geom_rug(sides = "b", position = position, alpha = 0.5)
        }
        plt <- plt + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"),
                                 alpha = 0.3) +
            geom_line()
    }

    ## default axis labels if none supplied
    if (missing(xlab)) {
        xlab <- term_label
    }
    if (missing(ylab)) {
        ylab <- sprintf("Partial effect of %s", term_label)
    }

    ## add labelling to plot
    plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                      caption = caption)

    ## fixing the y axis limits?
    if (!is.null(response_range)) {
        plt <- plt + expand_limits(y = response_range)
    }

    plt
}