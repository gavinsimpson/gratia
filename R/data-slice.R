#' Prepare a data slice through model covariates
#'
#' @details A data slice is the data set that results where one (or more
#' covariates) is varied systematically over some or all of its (their) range or
#' at a specified subset of values of interest, while any remaining covariates
#' in the model are held at fixed, representative values. This is known as a
#' *reference grid* in package **emmeans** and a *data grid* in the
#' **marginaleffects** package.
#'
#' For GAMs, any covariates not specified via `...` will take representative
#' values determined from the data used to fit the model as follows:
#'
#' * for numeric covariates, the value in the fitting data that is closest to
#'   the median value is used,
#' * for factor covariates, the modal (most frequently observed) level is used,
#'   or the first level (sorted as per the vector returned by [base::levels()]
#'   if several levels are observed the same number of times.
#'
#' These values are already computed when calling `gam()` or `bam()` for example
#' and can be found in the `var.summary` component of the fitted model. Function
#' [typical_values()] will extract these values for you if you are interested.
#'
#' Convenience functions [evenly()], [ref_level()], and [level()] are provided
#' to help users specify data slices. [ref_level()], and [level()] also ensure
#' that factor covariates have the correct levels, as needed by
#' [mgcv::predict.gam()] for example.
#'
#' For an extended discussion of [data_slice()] and further examples, see
#' \code{vignette("data-slices", package = "gratia")}.
#'
#' @param object an R model object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> User supplied variables
#'   defining the data slice. Arguments passed via `...` need to be *named*.
#' @param .observed_only logical or character; should the data slice be trimmed
#'   to those combinations of the variables specified that are observed in
#'   `object`. If `TRUE`, the observed combinations of variables mentioned in
#'   `...` are matched against those in `object` and filtered to return only
#'   those combinations. If `FALSE`, no filtering is done. If `.observed_only`
#'   is a character vector, on those variables named in the vector are used to
#'   in the comparison with the combinations in `object`.
#'
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional grouping
#'   variables. With `.by = NULL` (the default), expressions are evaluated over
#'   the full data set. Otherwise, expressions are evaluated separately within
#'   each observed group and the resulting grids are combined. Grouping variables
#'   are included automatically; expressions for these variables in `...` select
#'   groups and are evaluated once over the full data set. Unobserved combinations
#'   are omitted, and selecting no observed groups is an error.
#'
#' @details With `.by`, unspecified covariates retain their overall representative
#' values. Specify an expression such as `z = mean(z)` to use a group-specific
#' value instead. Factor levels and ordered status are preserved, and the result
#' is an ungrouped tibble. `.observed_only` filters exact matches within each
#' group; interpolated values need not match observed values.
#'
#' Grouping limits the default range of `evenly()` to each group's marginal
#' range. It does not ensure joint support for multiple continuous covariates.
#' Explicit values or `lower` and `upper` bounds can still request extrapolation.
#'
#' @seealso The convenience functions [evenly()], [ref_level()], and [level()].
#' [typical_values()] for extracting the representative values used for
#' covariates in the model but not named in the slice.
#'
#' @export
`data_slice` <- function(object, ...) {
  UseMethod("data_slice")
}

#' @export
#' @rdname data_slice
`data_slice.default` <- function(object, ...) {
  stop("Don't know how to create a data slice from <", class(object)[[1L]],
    ">",
    call. = FALSE
  )
}

#' @export
#' @rdname data_slice
#' @importFrom tidyr expand_grid
#' @importFrom rlang enquos eval_tidy
#' @importFrom dplyr semi_join
`data_slice.data.frame` <- function(
  object,
  ...,
  .observed_only = FALSE,
  .by = NULL
) {
  exprs <- rlang::enquos(...)
  by <- data_slice_by(rlang::enquo(.by), object)
  vars <- names(object)
  need_tv <- setdiff(vars, union(names(exprs), by))
  data_slice_grid(object, exprs, vars, by, .observed_only,
    typical_values(object, vars = all_of(need_tv)),
    source = "`object`"
  )
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
#' @importFrom dplyr semi_join
#'
#' @examples
#' \dontshow{
#' op <- options(pillar.sigfig = 3)
#' }
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
#' ds
#'
#' # for full control, specify the values you want
#' ds <- data_slice(m, x2 = evenly(x2, n = 50), x1 = 0.3)
#'
#' # or provide an expression (function call) which will be evaluated in the
#' # data frame passed to `data` or `model.frame(object)`
#' ds <- data_slice(m, x2 = evenly(x2, n = 50), x1 = mean(x1))
`data_slice.gam` <- function(object, ..., data = NULL,
                             envir = NULL,
                             .observed_only = FALSE, .by = NULL) {
  # Share environment resolution with expression evaluation and data recovery.
  object <- with_model_envir(object, envir)
  envir <- model_envir(object)
  # Only recover raw inputs used by the slice expressions. A constant offset
  # supplied by the caller must not require its original training vector.
  odata <- data
  exprs <- rlang::enquos(...)
  vars <- model_vars(object)
  # Stored summaries provide names and classes without recovering unused inputs.
  by <- data_slice_by(rlang::enquo(.by),
    if (is.null(data)) object[["var.summary"]] else data
  )
  needed <- unique(unlist(lapply(exprs, function(x) all.vars(rlang::get_expr(x)))))
  needed <- union(intersect(needed, vars), by)
  if (is.character(.observed_only)) needed <- union(needed, .observed_only)
  if (isTRUE(.observed_only)) needed <- union(needed, names(exprs))
  data <- data_slice_data(object, data = data, envir = envir, vars = needed)

  data_slice_grid(data, exprs, vars, by, .observed_only,
    typical_values(object, data = odata), source = "model"
  )
}

# Resolve grouping selections before recovering the data needed by GAM slices.
data_slice_by <- function(expr, data) {
  pos <- tidyselect::eval_select(expr, data = data, allow_rename = FALSE)
  names(pos)
}

# Evaluate and expand each group separately; the ungrouped path uses the same
# builder and retains the original expression and column ordering.
data_slice_grid <- function(data, exprs, vars, by, observed_only, tv, source) {
  nms <- names(exprs)
  if (any(i <- !nms %in% vars)) {
    message(
      "Some specified variable(s) not used in ", source, ":\n",
      paste(" * ", nms[i], collapse = "\n", sep = ""), "\n"
    )
  }
  need_tv <- setdiff(vars, union(nms, by))
  typical <- if (length(need_tv)) as.list(tv[need_tv]) else list()
  filter_vars <- if (isTRUE(observed_only)) nms else observed_only
  filter_observed <- isTRUE(observed_only) || is.character(observed_only)

  build <- function(group, key = list()) {
    values <- purrr::map(exprs[!nms %in% by], rlang::eval_tidy,
      data = group
    )
    values <- c(values, as.list(key))
    if (length(by)) values <- values[union(nms, by)]
    out <- tidyr::expand_grid(!!!c(values, typical))
    if (filter_observed) {
      out <- dplyr::semi_join(out, group, by = union(by, filter_vars))
    }
    out
  }
  if (!length(by)) return(build(data))

  # Group expressions select levels once in the full data mask. Never evaluate
  # evenly(fac) inside a group: it intentionally returns all declared levels.
  selected <- purrr::map(exprs[intersect(nms, by)], rlang::eval_tidy,
    data = data
  )
  rows <- rep(TRUE, nrow(data))
  for (nm in names(selected)) rows <- rows & data[[nm]] %in% selected[[nm]]
  data <- dplyr::ungroup(data)[rows, , drop = FALSE]
  if (!nrow(data)) {
    cli::cli_abort("No observed groups remain for {.arg .by}.")
  }
  groups <- vctrs::vec_group_loc(data[by])
  out <- lapply(seq_len(nrow(groups)), function(i) {
    key <- groups$key[i, , drop = FALSE]
    group <- data[groups$loc[[i]], , drop = FALSE]
    tryCatch(build(group, key), error = function(err) {
      label <- paste(paste(by, vapply(key, as.character, character(1)),
        sep = " = "), collapse = ", ")
      cli::cli_abort("Cannot create data slice for group {.val {label}}.",
        parent = err
      )
    })
  })
  dplyr::bind_rows(out)
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
  if (!is_gamm4(object)) {
    stop("`object` does not appear to a `gamm4` model object",
      call. = FALSE
    )
  }
  data_slice(object[["gam"]], ...)
}

#' @export
#' @rdname data_slice
`data_slice.scam` <- function(object, ...) {
  data_slice.gam(object, ...)
}

# Return raw covariates for user slice expressions, never renamed transforms.
`data_slice_data` <- function(object, data = NULL, envir = NULL, vars = model_vars(object)) {
  recover_raw_data(object, data, envir, vars)
}

#' @importFrom stats median quantile
`value_closest_to_median` <- function(x) {
  ## only work on numeric or factor variables
  is_fac <- is.factor(x)
  is_num <- is.numeric(x)

  ## if supplied something other than numeric or factor, bail
  if (!is_fac && !is_num) {
    stop("'x' must be a factor or numeric vector. Supplied <",
      class(x)[[1L]], ">",
      call. = FALSE
    )
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
      class(data)[[1L]], ">",
      call. = FALSE
    )
  }

  if (is_tib || is_df) {
    nr <- NROW(data)
    if (nr != 1L) {
      stop("'data' should have 1 row only. Supplied <",
        nr, ">",
        call. = FALSE
      )
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
  if (!is_fac && !is_num) {
    stop("Variable <", x, "> must be a factor or numeric vector. Found <",
      class(x)[[1L]], ">",
      call. = FALSE
    )
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
#' @param data an optional data frame supplying covariate classes. By default,
#'   these are taken from the stored model frame; typical values come from
#'   the fitted model's covariate summaries.
#'
#' @export
#' @importFrom rlang enquo
#' @importFrom tidyselect eval_select
#' @importFrom stats model.frame formula
`typical_values.gam` <- function(
    object, vars = everything(), data = NULL, ...) {
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
  # Stored summaries supply values; raw data supply their actual classes.
  if (is.null(data)) data <- model.frame(object)
  # Numeric summaries remain usable when only a transformed column survived.
  # Stored raw columns retain logical and factor classes where available.
  dc <- data_class(summ)
  available <- intersect(names(summ), names(data))
  dc[available] <- data_class(data[available])

  # if any logicals extract them as per numeric (2nd value) and convert to
  # logical. do this before extracting the numerics
  is_log <- dc == "logical"
  if (any(is_log)) {
    summ[is_log] <- lapply(summ[is_log], \(x) as.logical(x[2]))
  }

  # now process the numerics
  dc <- data_class(summ)
  i <- dc == "numeric" & lengths(summ) == 3L
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

#' @export
`typical_values.scam` <- function(object, ...) {
  typical_values.gam(object, ...)
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

#' @export
`factor_combos.scam` <- function(object, ...) {
  factor_combos.gam(object, ...)
}

#' All combinations of factor levels plus typical values of continuous variables
#'
#' @inheritParams factor_combos
#' @export
`data_combos` <- function(object, ...) {
  UseMethod("data_combos")
}

#' @inheritParams typical_values
#'
#' @inheritParams factor_combos
#' @export
#' @rdname data_combos
`data_combos.gam` <- function(object, vars = everything(),
                              complete = TRUE,
                              data = NULL, ...) {
  tv <- typical_values(object, data = data)
  is_fac <- vapply(tv, is.factor, logical(1L))
  if (any(is_fac)) { # drop factor from typical values
    tv <- tv[, !is_fac]
  }
  fc <- factor_combos(object, complete = complete)
  tbl <- expand_grid(fc, tv)

  # include/exclude any terms?
  expr <- rlang::enquo(vars)
  pos <- eval_select(expr, data = tbl)
  tbl <- tbl[pos]
  tbl
}

#' @export
`data_combos.scam` <- function(object, ...) {
  data_combos.gam(object, ...)
}
