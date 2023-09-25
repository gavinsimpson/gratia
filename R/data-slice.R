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

#' @export
#' @rdname data_slice
#' @importFrom tidyr expand_grid
#' @importFrom rlang enquos eval_tidy
`data_slice.data.frame` <- function(object, ...) {
    # deal with ...
    exprs <- rlang::enquos(...)
    slice_vars <- purrr::map(exprs, rlang::eval_tidy, data = object)

    # check now if there are elements of slice_vars that aren't in the object
    vars <- names(object)
    nms <- names(slice_vars)
    if (any(i <- ! nms %in% vars)) {
        message("Some specified variable(s) not used in `object``:\n",
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

#' @param data an alternative data frame of values containing all the variables
#'   needed to fit the model. If `NULL`, the default, the data used to fit the
#'   model will be recovered using `model.frame`. User-supplied expressions
#'   passed in `...` will be evaluated in `data`.
#' @param envir the environment within which to recreate the data used to fit
#'   `object`.
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
#' m <- gam(y ~ s(x2, k = 7) + x1, data = df, method = "REML")
#'
#' # Want to predict over f(x2) while holding `x1` at some value.
#' # Default will use the observation closest to the median for unspecified
#' # variables.
#' ds <- data_slice(m, x2 = evenly(x2, n = 50))
#'
#' # for full control, specify the values you want
#' ds <- data_slice(m, x2 = evenly(x2, n = 50), x1 = 0.3)
#'
#' # or provide an expression (function call) which will be evaluated in the
#' # data frame passed to `data` or `model.frame(object)`
#' ds <- data_slice(m, x2 = evenly(x2, n = 50), x1 = mean(x1))
`data_slice.gam` <- function(object, ..., data = NULL,
                             envir = environment(formula(object))) {
    # prep data
    odata <- data
    data <- data_slice_data(object, data = data)

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
        tv <- typical_values(object, data = odata, envir = envir)
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

`data_slice_data` <- function(object, data = NULL) {
    is_mf <- FALSE # is a data a model frame
    if (is.null(data)) {
        # get data from object$model
        data <- object[["model"]]
        is_mf <- TRUE
    } else {
        if (!is.null(attr(data, "terms"))) {
            is_mf <- TRUE
        }
    }

    # find the response if there
    tt <- terms(object)
    resp_i <- attr(tt, "response")
    y_var <- names(attr(tt, "dataClasses"))[resp_i]
    all_vars <- all.vars(tt)
    data_names <- names(data)
    # if the response variable is named in data, delete it
    y_in_data <- data_names %in% y_var
    if (any(y_in_data)) {
        data <- data[!y_in_data]
    }

    # handle offsets; if we generated the data from the model, then set
    # offset variable(s) to 1
    # But note that this is only for the data object that we'll eval into
    # The offset will get set to whatever is the typical value for those offset
    # variable(s). As with any other variable you'll need to provide a value
    # for each offset if you want to use that value
    if (is_mf) {
        # are there any offsets
        offsets <- attr(tt, "offset")
        if (length(offsets)) {
            # when selected from data, remember we deleted the response already
            # offsets will be shifted 1 col to left
            data[, offsets - 1] <- 1
            names(data)[offsets - 1] <- all_vars[offsets]
        }
    }

    data
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
#' @param envir the environment within which to recreate the data used to fit
#'   `object`.
#' @param data an optional data frame of data used to fit the mdoel if
#'   reconstruction of the data from the model doesn't work.
#' 
#' @export
#' @importFrom rlang enquo
#' @importFrom tidyselect eval_select
#' @importFrom stats model.frame formula
`typical_values.gam` <- function(object, vars = everything(),
    envir = environment(formula(object)), data = NULL, ...) {
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
    # logical values get stored as numeric in the summary
    # dc <- data_class(summ) # mgcv doesn't store logicals as logicals
    # so we need to extract the data classes ourselves
    # try to recover the data
    mf <- model.frame(object)
    if (is.null(data)) {
        data <- eval(object$call$data, envir)
    }
    if (is.null(data)) {
        data <- mf
    }
    data <- data[names(summ)] # take only vars mgcv thinks we need
    dc <- data_class(data)

    # if any logicals extract them as per numeric (2nd value) and convert to
    # logical. do this before extracting the numerics
    is_log <- dc == "logical"
    if (any(is_log)) {
        summ[is_log] <- lapply(summ[is_log], \(x) as.logical(x[2]))
    }

    # now process the numerics
    dc <- data_class(summ)
    i <-  dc == "numeric" & lengths(summ) == 3L
    summ[i] <- lapply(summ[i], `[`, 2)

    # return
    as_tibble(summ)
}

#' @export
#' @rdname typical_values
#' @importFrom tidyselect everything
#' @importFrom dplyr summarise across
#' @importFrom tibble as_tibble
`typical_values.data.frame` <- function(object, vars = everything(), ...) {
    # include/exclude any terms?
    expr <- rlang::enquo(vars)
    pos <- eval_select(expr, data = object)
    object <- object[pos]

    df <- object |>
        summarise(across(everything(), .fns = value_closest_to_median))

    # return
    as_tibble(df)
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
#' @importFrom rlang enquo !!! exec
#' @importFrom tidyr nesting expand expand_grid
#' @importFrom tidyselect eval_select
#' @rdname factor_combos
`factor_combos.gam` <- function(object, vars = everything(),
                                complete = TRUE, ...) {
    # extract the summary from the fitted GAM
    # summ is a named list
    summ <- object[["var.summary"]]

    # which are factors?
    is_fac <- vapply(summ, is.factor, logical(1L))
    if (!any(is_fac)) {
        # message("Model contains no factor terms")
        return(NULL)
    } else {
        summ <- summ[is_fac]
    }

    # include/exclude any terms?
    expr <- rlang::enquo(vars)
    pos <- eval_select(expr, data = summ)
    summ <- summ[pos]

    f <- lapply(summ, function(x) factor(levels(x), levels = levels(x)))
    f <- exec("expand_grid", !!!f) # f <- purrr::cross_df(f)
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

#' @param envir the environment within which to recreate the data used to fit
#'   `object`.
#' @param data an optional data frame of data used to fit the mdoel if
#'   reconstruction of the data from the model doesn't work.
#'
#' @inheritParams factor_combos
#' @export
#' @rdname data_combos
`data_combos.gam` <- function(object, vars = everything(),
                              complete = TRUE,
                              envir = environment(formula(object)),
                              data = NULL, ...) {
    tv <- typical_values(object, envir = envir, data = data)
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
