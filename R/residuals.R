#' Partial residuals
#'
#' @param object an R object, typically a model. Currently only objects of
#'   class `"gam"` (or that inherit from that class) are supported.
#' @param ... arguments passed to other methods.
#'
#' @export
`partial_residuals` <- function(object, ...) {
    UseMethod("partial_residuals")
}

#' @param select character, logical, or numeric; which smooths to plot. If
#'   `NULL`, the default, then all model smooths are drawn. Numeric `select`
#'   indexes the smooths in the order they are specified in the formula and
#'   stored in `object`. Character `select` matches the labels for smooths
#'   as shown for example in the output from `summary(object)`. Logical
#'   `select` operates as per numeric `select` in the order that smooths are
#'   stored.
#' @param partial_match logical; should smooths be selected by partial matches
#'   with `select`? If `TRUE`, `select` can only be a single string to match
#'   against.
#'
#' @rdname partial_residuals
#'
#' @export
#' 
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols arrange
#'
#' @examples
#' \dontshow{
#' set.seed(1)
#' op <- options(digits = 4, cli.unicode = FALSE)
#' }
#' ## load mgcv
#' load_mgcv()
#'
#' ## example data - Gu & Wabha four term model
#' df <- data_sim("eg1", n = 400, seed = 42)
#' ## fit the model
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = 'REML')
#'
#' ## extract partial residuals
#' partial_residuals(m)
#'
#' ## and for a select term
#' partial_residuals(m, select = "s(x2)")
#'
#' ## or with partial matching
#' partial_residuals(m, select = "x", partial_match = TRUE) # returns all
#' \dontshow{
#' options(op)
#' }
`partial_residuals.gam` <- function(object, select = NULL, partial_match = FALSE,
                                    ...) {
    ## get a vector of labels for smooths
    sms <- smooths(object)
    ## which were selected; select = NULL -> all selected
    take <- check_user_select_smooths(sms, select = select,
                                      partial_match = partial_match)
    if (!any(take)) {
        stop("No smooth label matched 'select'. Try with 'partial_match = TRUE'?",
             call. = FALSE)
    }
    sms <- sms[take] # subset to selected smooths

    ## compute partial resids
    p_resids <- compute_partial_residuals(object, terms = sms)

    ## cast as a tibble --- do something with the column names?
    ##  - they are non-standard: `s(x)` for example
    p_resids <- tibble::as_tibble(p_resids)

    p_resids
}

#' @export
`partial_residuals.gamm` <- function(object, ...) {
    partial_residuals(object[["gam"]], ...)
}

#' @export
`partial_residuals.list` <- function(object, ...) {
    if (! is_gamm4(object)) {
        stop("'object' is not a `gamm4()` fit. Can't handle general lists.")
    }
    partial_residuals(object[["gam"]], ...)
}

## Internal function to compute weighted residuals for use by other functions
#' @importFrom stats residuals weights
#' @importFrom tibble as_tibble
`compute_partial_residuals` <- function(object, terms = NULL, data = NULL) {
    ## weight residuals...
    ## need as.numeric for gamm() objects
    w_resid <- as.numeric(residuals(object)) * sqrt(weights(object))

    ## if data is null, just grab the $model out of object
    if (is.null(data)) {
        data <- object[["model"]]
    } else {
        ## check size of data
        if (nrow(data) != length(w_resid)) {
            stop("Length of model residuals not equal to number of rows in 'data'",
                 call. = FALSE)
        }
    }
    ## get the contributions for each selected smooth
    p_terms <- if (is.null(terms)) {
        predict(object, type = "terms", newdata = data)
    } else {
        predict(object, type = "terms", terms = terms, newdata = data)
    }
    attr(p_terms, "constant") <- NULL # remove intercept attribute
    ## and compute partial residuals
    p_resids <- p_terms + w_resid

    as_tibble(p_resids)
}

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
#' @importFrom dplyr %>% bind_cols ungroup
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
#' @keywords internal
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr %>% ungroup
`nested_rug_values` <- function(object, terms = NULL, data = NULL) {

    # if not supplied data then recover it from the model object
    if (is.null(data)) {
        data <- model.frame(object)
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
        select(all_of(c(sm_var, by_var)))

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