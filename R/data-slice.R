#' Prepare a data slice through model covariates
#'
#' @param object an R model object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> User supplied variables
#'   defining the data slice. Arguments passed via `...` need to *named*
#'
#' @export
`data_slice` <- function(object, ...) {
    UseMethod("data_slice")
}

#' @export
#' @rdname data_slice
`data_slice.default` <- function(object, ...) {
    stop("Don't know how to create a data slice from <", class(object)[[1L]],
         ">", call. = FALSE)
}

#' @param data an alternative data frame of values containing all the variables
#'   needed to fit the model. If `NULL`, the default, the data used to fit the
#'   model will be recovered using `model.frame`. User-supplied expressions
#'   passed in `...` will be evaluated in `data`.
#' @param offset numeric; value to use for an offset term in the model.
#'
#' @export
#' @rdname data_slice
#' @importFrom tidyr expand_grid
#' @importFrom rlang enquos eval_tidy
#' @importFrom stats model.frame
#'
#' @examples
#' load_mgcv()
#'
#' # simulate some Gaussian data
#' df <- data_sim("eg1", n = 50, seed = 2)
#'
#' # fit a GAM with 1 smooth and 1 linear term
#' m1 <- gam(y ~ s(x2, k = 7) + x1, data = df, method = "REML")
#'
#' # Want to predict over f(x2) while holding `x1` at some value.
#' # Default will use the observation closest to the median for unspecified
#' # variables.
#' ds <- data_slice(m1, x2 = evenly(x2, n = 50))
#'
#' # for full control, specify the values you want
#' ds <- data_slice(m1, x2 = evenly(x2, n = 50), x1 = 0.3)
#'
#' # or provide an expression (function call) which will be evaluated in the
#' # data frame passed to `data` or `model.frame(object)`
#' ds <- data_slice(m1, x2 = evenly(x2, n = 50), x1 = mean(x1))
`data_slice.gam` <- function(object, ..., data = NULL, offset = NULL) {

    if (is.null(data)) {
        data <- delete_response(object) %>% as_tibble()
    }

    # deal with ...
    ##ellipsis::check_dots_unnamed()
    exprs <- rlang::enquos(...)
    slice_vars <- purrr::map(exprs, rlang::eval_tidy, data = data)

    # check now if there are elements of slice_vars that aren't in the model
    vars <- model_vars(object)
    nms <- names(slice_vars)
    if (any(i <- ! nms %in% vars)) {
        message("Some specified variable(s) not used in model:\n",
                paste(" * ", nms[i], collapse = "\n", sep = ""),
                "\n")
    }

    # typical values, only needed ones that aren't
    need_tv <- setdiff(vars, names(slice_vars))
    if (length(need_tv) > 0L) {
        tv <- typical_values(object)
        slice_vars <- append(slice_vars, tv[need_tv])
    }

    expand_grid(!!!{slice_vars})
}

#' @export
#' @rdname data_slice
`data_slice.gamm` <- function(object, ...) { # for gamm() models
    data_slice(object[["gam"]], ...)
}

#' @export
#' @rdname data_slice
`data_slice.list` <- function(object, ...) { # for gamm4 lists only
    ## Is this list likely to be a gamm4 list?
    if (! is_gamm4(object)) {
        stop("`object` does not appear to a `gamm4` model object",
             call. = FALSE)
    }
    data_slice(object[["gam"]], ...)
}



#' @importFrom stats median quantile
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

    ## if x is numeric, return the observation closest to median value
    if (is_num) {
        # mgcv prefers this to `median()` as it is a data point
        med <- quantile(x, na.rm = TRUE, prob = 0.5, type = 3)
        # and as a result we don't need to find the value closest to med
        # as that's what `type` does
        result <- unname(med)
    }

    result
}

## if no data, set to a 0-row tibble; if data supplied, check it:
##   - single row df or list of length-1 elements; only variables in mf
#' @importFrom tibble is_tibble tibble
`process_slice_data` <- function(data) {
    ## if NULL, bail early; return a 0-row tibble
    if (is.null(data)) {
        return(NULL)
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
            stop("If 'data' is a list, it should be a list of length-1 vectors")
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

#' Typical values of model covariates
#'
#' @param object a fitted GAM(M) model.
#' @param ... arguments passed to other methods.
#'
#' @export
`typical_values` <- function(object, ...) {
    UseMethod("typical_values")
}

#' @rdname typical_values
#' @param vars terms to include or exclude from the returned object. Uses
#'   tidyselect principles.
#' @export
#' @importFrom rlang enquo
#' @importFrom tidyselect eval_select
`typical_values.gam` <- function(object, vars = everything(), ...) {
    # extract the summary from the fitted GAM
    # summ is a named list
    summ <- object[["var.summary"]]

    # include/exclude any terms?
    expr <- rlang::enquo(vars)
    pos <- eval_select(expr, data = summ)
    summ <- summ[pos]

    # for numeric variables summ is a vector with 3 elements, we want element 2
    # which contains the value of the observation closest to the median
    # probably need to handle matrix covariates here separately from numerics
    dc <- data_class(summ)
    i <-  dc == "numeric" & lengths(summ) == 3L
    summ[i] <- lapply(summ[i], `[`, 2)

    # return
    as_tibble(summ)
}

#' All combinations of factor levels
#'
#' @param object a fitted model object.
#' @param vars terms to include or exclude from the returned object. Uses
#'   tidyselect principles.
#' @param complete logical; should all combinations of factor levels be
#'   returned? If `FALSE`, only those combinations of levels observed in the
#'   model are retained.
#' @param ... arguments passed to methods.
#'
#' @export
`factor_combos` <- function(object, ...) {
    UseMethod("factor_combos")
}

#' @export
#' @importFrom purrr cross_df
#' @importFrom rlang enquo
#' @importFrom tidyr nesting expand
#' @rdname factor_combos
`factor_combos.gam` <- function(object, vars = everything(),
                                complete = TRUE, ...) {
    # extract the summary from the fitted GAM
    # summ is a named list
    summ <- object[["var.summary"]]

    # which are factors?
    is_fac <- vapply(summ, is.factor, logical(1L))
    if (!any(is_fac)) {
        message("Model contains no factor terms")
        return(NULL)
    } else {
        summ <- summ[is_fac]
    }

    # include/exclude any terms?
    expr <- rlang::enquo(vars)
    pos <- eval_select(expr, data = summ)
    summ <- summ[pos]

    f <- lapply(summ, function(x) factor(levels(x), levels = levels(x)))
    f <- purrr::cross_df(f)
    if (isFALSE(complete)) {
        mf <- model.frame(object)[names(summ)]
        f <- expand(f, nesting(mf))
    }
    f
}

#' All combinations of factor levels plus typical values of continuous variables
#'
#' @inheritParams factor_combos
#' @export
`data_combos` <- function(object, ...) {
    UseMethod("data_combos")
}

#' @inheritParams factor_combos
#' @export
#' @rdname data_combos
`data_combos.gam` <- function(object, vars = everything(),
                              complete = TRUE, ...) {
    tv <- typical_values(object)
    is_fac <- vapply(tv, is.factor, logical(1L))
    if (any(is_fac)) { # drop factor from typical values
        tv <- tv[, !is_fac]
    }
    fc <- factor_combos(object)
    tbl <- expand_grid(fc, tv)

    # include/exclude any terms?
    expr <- rlang::enquo(vars)
    pos <- eval_select(expr, data = tbl)
    tbl <- tbl[pos]
    tbl
}
