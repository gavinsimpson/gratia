#' Partial residuals in nested form
#'
#' Computes partial residuals for smooth terms, formats them in long/tidy
#' format, then nests the `partial_residual` column such that the result
#' is a nested data frame with one row per smooth.
#'
#' @param object a fitted GAM model
#' @param terms a vector of terms to include partial residuals for. Passed to
#'   argument `terms` of [mgcv::predict.gam()]].
#' @param data optional data frame
#'
#' @return A nested tibble (data frame) with one row per smooth term. Contains
#'   two columns:
#'   * `smooth` - a label indicating the smooth term
#'   * `partial_residual` - a list column containing a tibble (data frame) with
#'     1 column `partial_residual` containing the requested partial residuals
#'     for the indicated smooth.
#'
#' @keywords internal
#' @importFrom tidyr pivot_longer nest
#' @importFrom tidyselect everything
#' @importFrom dplyr %>% mutate rowwise
`nested_partial_residuals` <- function(object, terms = NULL, data = NULL) {
    # compute partial residuals
    p_resids <- compute_partial_residuals(object,
                                          terms = terms,
                                          data = data)

    # it not supplied data then recover it from the model object
    if (is.null(data)) {
        data <- model.frame(object)
    }

    ## tidy partial residuals to long format
    p_resids <- pivot_longer(p_resids, everything(),
                             names_to = "smooth",
                             values_to = "partial_residual")

    # nest this long format so we 1 row per smooth, and partial_residual is now
    # a list column
    p_resids <- nest(p_resids, partial_residual = c("partial_residual"))

    # ====> Might be making this too complicated !?
    # row-wise (so per smooth), bind on the data used to create the partial
    # residuals. Need to pull out the variables from the label in the `$smooth`
    # column for each row and use that to index into `data`
    #
    # ====> Am envisaging a situation where we have smooths with more than one
    # covariate, or a smooth with a variable appearing in more than one smooth
    # so we can't simply gather up the `data` and `p_resids` separately and
    # expect them to have the same length when gathered
    #
    # This is now done in separate function that we reuse elsewhere
    # and it filters out the data for the correct level of a by variable

    p_resids <- p_resids %>%
      rowwise() %>%
      mutate(partial_residual =
        list(get_obs_data_for_smooth(.data$smooth,
                                     object, data,
                                     extra = .data$partial_residual))) %>%
      ungroup()


    p_resids
}

#' Values for rug plot in nested form
#'
#' Extracts original data for smooth terms, formats them in long/tidy
#' format, then nests the data column(s) such that the result
#' is a nested data frame with one row per smooth.
#'
#' @param object a fitted GAM model
#' @param terms a vector of terms to include original data for. Passed to
#'   argument `terms` of [mgcv::predict.gam()]].
#' @param data optional data frame
#'
#' @return A nested tibble (data frame) with one row per smooth term.
#'
#' @keywords internal
#' @importFrom tibble tibble
#' @importFrom dplyr %>% rowwise mutate ungroup
`nested_rug_values` <- function(object, terms = NULL, data = NULL) {

    # if not supplied data then recover it from the model object
    if (is.null(data)) {
        data <- object$model # don't need -> model.frame(object)
    }

    # if no terms, extract all smooth labels
    if (is.null(terms)) {
        terms <- smooths(object)
    }

    rug_vals <- tibble(smooth = terms) %>%
        rowwise() %>%
        mutate(rug_data =
                 list(get_obs_data_for_smooth(.data$smooth, object, data))) %>%
        ungroup()

    rug_vals
}

#' @importFrom dplyr select bind_cols filter %>%
#' @importFrom tidyselect all_of
#' @importFrom tibble as_tibble
#' @noRd
#' @keywords internal
`get_obs_data_for_smooth` <- function(smooth, model, data, extra = NULL) {
    sm <- get_smooth(model, smooth)
    sm_var <- smooth_variable(sm)
    # ====> FIXME <==== what about continuous by?
    # we don't need the by variable in that case. Must check this in that
    # situation - seems OK as long as we don't filter
    by_var <- by_variable(sm)
    if (by_var == "NA") {
        by_var <- NULL
    }
    by_lev <- by_level(sm)

    # retain only those columns we want
    df <- data %>%
        select(all_of(c(sm_var, by_var))) %>% as_tibble()

    # handle matrix covariates, which will get stacked by as.numeric
    df <- unpack_matrix_cols(df) %>% as_tibble()

    # do this first so we bind on all the data, then filter
    if (!is.null(extra)) {
        df <- bind_cols(extra, df)
    }

    # if we have a by variable, filter out only those rows that pertain to
    # this smooth. Only for factor by smooths
    if (is_factor_by_smooth(sm)) {
        df <- df %>%
            filter(.data[[by_var]] == by_lev) %>%
            as_tibble()
    }

    # return
    df
}

#' @importFrom dplyr bind_cols
`unpack_matrix_cols` <- function(x) {
    # what data types are we working with
    dcs <- data_class(x)

    # do something if we have matrix columns
    if (any(mcols <- dcs == "matrix")) {
        # assume that any matrix columns are of the same number of cols
        nc <- ncol(x[mcols][[1L]])
        out <- lapply(x[mcols], as.vector)
        other <- lapply(x[!mcols], function(y, nc) rep(y, times = nc), nc = nc)
        out <- bind_cols(out, other)
        # put order back as it was
        out <- out[names(x)]
    } else {
        return(x)
    }

    out
}
