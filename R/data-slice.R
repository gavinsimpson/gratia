##' Prepare a data slice through covariates
##'
##' @param object an R model object.
##' @param ... arguments passed to other methods.
##'
##' @export
`data_slice` <- function(object, ...) {
    UseMethod("data_slice")
}

##' @export
##' @rdname data_slice
`data_slice.default` <- function(object, ...) {
    stop("Don't know how to create a data slice from <", class(object)[[1L]],
         ">", call. = FALSE)
}

##' @param var1 character;
##' @param var2 character;
##' @param var3 character; ignored currently.
##' @param var4 character; ignored currently.
##' @param data a 1-row data frame or tibble containing values for variables in
##'   the fitted model that are not varying in the slice.
##' @param n numeric; the number of values to create for each of `var1` and
##'   `var2`in the slice.
##' @param offset numeric; value to use for an offset term in the model.
##'
##' @export
##' @rdname data_slice
##'
##' @importFrom tidyr crossing
##' @importFrom tibble as_tibble
##' @importFrom stats model.frame
`data_slice.gam` <- function(object, var1, var2, var3 = NULL, var4 = NULL,
                             data = NULL, n = 50, offset = NULL, ...) {
    ## we need the model frame to get data from
    mf <- model.frame(object)
    ## remove response
    respvar <- attr(object$terms, "response")
    if (!identical(respvar, 0)) {
        mf <- mf[, -respvar, drop = FALSE]
    }

    ## remove offset() var; model.frame returns both `offset(foo(var))` and `var`,
    ## so we can just remove the former, but we also want to set the offset
    ## variable `var` to something constant. FIXME
    if (is.null(offset)) {
        offset <- 1L
    }
    mf <- fix_offset(object, mf, offset_val = offset)

    data1 <- process_slice_var(x = var1, data = mf, n = n)
    data2 <- process_slice_var(x = var2, data = mf, n = n)

    ## if no data, set to a 0-row tibble; if data supplied, check it:
    ##   - single row df or list of length-1 elements; only variables in mf
    data <- process_slice_data(data)

    ## for all other variables in mf not yet supplied, we want the observation
    ##   from mf that is closest to the median
    terms_given <- c(var1, var2, var3, var4, names(data))
    ind <- names(mf) %in% terms_given
    if (any(!ind)) {
        other_data <- as_tibble(lapply(mf[!ind], value_closest_to_median))
    }

    ## expand.grid-alike
    result <- crossing(data1, data2, data, other_data)
    names(result)[1:2] <- c(var1, var2)

    result
}

##' @importFrom stats median
`value_closest_to_median` <- function(x) {
    ## only work on numeric or factor variables
    is_fac <- is.factor(x)
    is_num <- is.numeric(x)

    ## if supplied something other than numeric or factor, bail
    if(!is_fac && !is_num) {
        stop("'x' must be a factor or numeric vector. Supplied <",
             class(x)[[1L]], ">", call. = FALSE)
    }

    ## if x is a factor, return the modal value as a factor with original
    ##   levels
    if (is_fac) {
        tab <- tabulate(x)
        levs <- levels(x)
        result <- levs[which.max(tab)]
        result <- factor(result, levels = levs)
    }

    ## if x is numeric, return the median value
    if (is_num) {
        med <- median(x, na.rm = TRUE)      # median
        dif <- abs(x - med)
        result <- x[which.min(dif)]
    }

    result
}

## if no data, set to a 0-row tibble; if data supplied, check it:
##   - single row df or list of length-1 elements; only variables in mf
##' @importFrom tibble is_tibble data_frame
`process_slice_data` <- function(data) {
    ## if NULL, bail early; return a 0-row tibble
    if (is.null(data)) {
        return(data_frame())
    }

    ## we were given something
    is_tib <- is_tibble(data)
    is_df <- is.data.frame(data)
    is_list <- is.list(data)

    if (!any(is_tib, is_df, is_list)) {
        stop("'data' should be a tibble, data frame, or list. Supplied <",
             class(data)[[1L]], ">", call. = FALSE)
    }

    if (is_tib || is_df) {
        nr  <- NROW(data)
        if (nr != 1L) {
            stop("'data' should have 1 row only. Supplied <",
                 nr, ">", call. = FALSE)
        }
    }

    if (is_list) {
        if (!all(lengths(data) == 1L)) {
            stop("'data' should be a list of length-1 vectors")
        }
    }

    as_tibble(data)
}

`process_slice_var` <- function(x, data, n) {
    ## if x is NULL bail quickly
    if (is.null(x)) {
        return(x)
    }

    ## x should be a character, bail otherwise
    if (!is.character(x)) {
        stop("Supplied 'x' is not character.")
    }

    ## x should be a named variable in data
    if (!x %in% names(data)) {
        stop("Variable <", x, "> not found in data.", call. = FALSE)
    }

    values <- data[[x]]
    is_fac <- is.factor(values)
    is_num <- is.numeric(values)

    ## if supplied something other than numeric or factor, bail
    if(!is_fac && !is_num) {
        stop("Variable <", x, "> must be a factor or numeric vector. Found <",
             class(x)[[1L]], ">", call. = FALSE)
    }

    if (isTRUE(is_fac)) {
        values <- levels(values)
    }

    if (isTRUE(is_num)) {
        values <- seq_min_max(values, n)
    }

    values
}
