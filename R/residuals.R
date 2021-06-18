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
`partial_residuals.gam` <- function(object,
                                    select = NULL,
                                    partial_match = FALSE,
                                    ...) {
    model_name <- expr_label(substitute(object))
    ## get a vector of labels for smooths
    sms <- smooths(object)
    ## which were selected; select = NULL -> all selected
    take <- check_user_select_smooths(sms, select = select,
                                      partial_match = partial_match,
                                      model_name = model_name)
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
