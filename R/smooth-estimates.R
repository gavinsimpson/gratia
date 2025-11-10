#' Evaluate smooths at covariate values
#'
#' Evaluate a smooth at a grid of evenly spaced value over the range of the
#' covariate associated with the smooth. Alternatively, a set of points at which
#' the smooth should be evaluated can be supplied. `smooth_estimates()` is a new
#' implementation of `evaluate_smooth()`, and replaces that function, which has
#' been removed from the package.
#'
#' @param object an object of class `"gam"` or `"gamm"`.
#' @param select character; select which smooth's posterior to draw from.
#'   The default (`NULL`) means the posteriors of all smooths in `model`
#'   will be sampled from. If supplied, a character vector of requested terms.
#' @param smooth `r lifecycle::badge("deprecated")` Use `select` instead.
#' @param n numeric; the number of points over the range of the covariate at
#'   which to evaluate the smooth.
#' @param n_3d,n_4d numeric; the number of points over the range of last
#'   covariate in a 3D or 4D smooth. The default is `NULL` which achieves the
#'   standard behaviour of using `n` points over the range of all covariate,
#'   resulting in `n^d` evaluation points, where `d` is the dimension of the
#'   smooth. For `d > 2` this can result in very many evaluation points and slow
#'   performance. For smooths of `d > 4`, the value of `n_4d` will be used for
#'   all dimensions `> 4`, unless this is `NULL`, in which case the default
#'   behaviour (using `n` for all dimensions) will be observed.
#' @param data a data frame of covariate values at which to evaluate the
#'   smooth.
#' @param unconditional logical; should confidence intervals include the
#'   uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian
#'   covariance matrix will be used.
#' @param overall_uncertainty logical; should the uncertainty in the model
#'  constant term be included in the standard error of the evaluate values of
#'  the smooth?
#' @param dist numeric; if greater than 0, this is used to determine when
#'   a location is too far from data to be plotted when plotting 2-D smooths.
#'   The data are scaled into the unit square before deciding what to exclude,
#'   and `dist` is a distance within the unit square. See
#'   [mgcv::exclude.too.far()] for further details.
#' @param unnest logical; unnest the smooth objects?
#' @param partial_match logical; in the case of character `select`, should
#'   `select` match partially against `smooths`? If `partial_match = TRUE`,
#'   `select` must only be a single string, a character vector of length 1.
#' @param clip logical; should evaluation points be clipped to the boundary of
#'   a soap film smooth? The default is `FALSE`, which will return `NA` for any
#'   point that is deemed to lie outside the boundary of the soap film.
#' @param ... arguments passed to other methods.
#'
#' @return A data frame (tibble), which is of class `"smooth_estimates"`.
#'
#' @export
#'
#' @rdname smooth_estimates
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 6)
#' }
#' dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' ## evaluate all smooths
#' smooth_estimates(m1)
#'
#' ## or selected smooths
#' smooth_estimates(m1, select = c("s(x0)", "s(x1)"))
#'
#' # parallel processing of smooths
#' if (requireNamespace("mirai") && requireNamespace("carrier")) {
#'   library("mirai")
#'   daemons(2)                          # only low for CRAN requirements
#'   smooth_estimates(m1)
#' }
#' \dontshow{
#' options(op)
#' }
`smooth_estimates` <- function(object, ...) {
  UseMethod("smooth_estimates")
}

#' @export
#' @rdname smooth_estimates
#' @importFrom dplyr bind_rows all_of
#' @importFrom tidyr unnest
#' @importFrom rlang expr_label
#' @importFrom lifecycle deprecated is_present
#' @importFrom purrr in_parallel map
#' @importFrom mirai daemons_set
`smooth_estimates.gam` <- function(object,
    select = NULL,
    smooth = deprecated(),
    n = 100,
    n_3d = 16,
    n_4d = 4,
    data = NULL,
    unconditional = FALSE,
    overall_uncertainty = TRUE,
    dist = NULL,
    unnest = TRUE,
    partial_match = FALSE,
    clip = FALSE,
    ...) {
  if (lifecycle::is_present(smooth)) {
    lifecycle::deprecate_warn("0.8.9.9", "smooth_estimates(smooth)",
      "smooth_estimates(select)")
    select <- smooth
  }
  model_name <- expr_label(substitute(object))
  ## if particular smooths selected
  S <- smooths(object) # vector of smooth labels - "s(x)"

  # select smooths
  select <-
    check_user_select_smooths(
      smooths = S, select = select,
      partial_match = partial_match,
      model_name = model_name
    )
  smooth_ids <- which(select)

  ## extract the mgcv.smooth objects
  smooths <- get_smooths_by_id(object, smooth_ids)

  ## loop over the smooths and evaluate them
  sm_list <- vector(mode = "list", length = length(smooths))

  ## if user data supplied, check for and remove response
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data', if supplied, must be a numeric vector or data frame.",
        call. = FALSE
      )
    }
    check_all_vars(object, data = data, smooths = smooths)
    data <- delete_response(object, data = data)
  }

  # # fix up the n, n_3d, n_4d. If `n_3d` is `NULL` set `n_3d <- n`
  # if (is.null(n_3d)) {
  #     n_3d <- n
  # }
  # # likewise fix up n_4d; set it to `n` if `n_4d` is NULL
  # if (is.null(n_4d)) {
  #     n_4d <- n
  # }

  # loop over selected smooths and evaluate them, now with parallel processing
  if (isFALSE(mirai::daemons_set())) {
    sm_list <- map(
      smooths,
      eval_smooth,
      model = object, n = n, n_3d = n_3d, n_4d = n_4d, data = data,
      unconditional = unconditional,
      overall_uncertainty = overall_uncertainty, dist = dist, clip = clip
    )
  } else {
    sm_list <- map(
      smooths,
      in_parallel(
        \(sm) eval_smooth(
          sm, model = object, n = n, n_3d = n_3d, n_4d = n_4d,
          data = data, unconditional = unconditional,
          overall_uncertainty = overall_uncertainty, dist = dist
        ),
        object = object, n = n, n_3d = n_3d, n_4d = n_4d, data = data,
        unconditional = unconditional,
        overall_uncertainty = overall_uncertainty, dist = dist, clip = clip,
        eval_smooth = gratia::eval_smooth
      )
    )
  }

  # see if we have any tensor term orders to collect & apply
  tensor_term_order <- lapply(sm_list, attr, "tensor_term_order")
  ## create a single df of all the smooths
  sm_list <- bind_rows(sm_list)

  ## need to unnest the `data` column?
  if (isTRUE(unnest)) {
    sm_list <- unnest(sm_list, all_of("data"))
  }

  # add back any special attributes
  attr(sm_list, "tensor_term_order") <- do.call("c", tensor_term_order)

  ## add a class
  class(sm_list) <- c("smooth_estimates", class(sm_list))

  ## return
  sm_list
}

#' @export
`smooth_estimates.gamm` <- function(object, ...) {
  smooth_estimates(object[["gam"]], ...)
}

#' @export
`smooth_estimates.scam` <- function(object, ...) {
  # scam has too many smooth types to write methods for all of them
  # this just adds on some classes that allows gratia to dispatch special
  # methods for their peculiarities
  object$smooth <- lapply(object$smooth, reclass_scam_smooth)
  # now just call the "gam" method
  smooth_estimates.gam(object, ...)
}

# gamm4 method
#' @export
`smooth_estimates.list` <- function(object, ...) {
  if (!is_gamm4(object)) {
    stop("'smooth_estimates()' not available for a generic list")
  }
  smooth_estimates(object[["gam"]], ...)
}

#' Check user-supplied data for suitability
#'
#' @param data a data frame of variables to be checked.
#' @param vars character; vector of terms.
#'
#' @importFrom tibble tibble
#' @importFrom rlang := !!
#'
#' @keywords internal
#' @noRd
`check_user_data` <- function(data, vars) {
  if (is.data.frame(data)) {
    smooth_vars <- vars %in% names(data)
    if (!all(smooth_vars)) {
      stop(
        paste(
          "Variable(s)",
          paste(paste0("'", vars[!smooth_vars], "'"),
            collapse = ", "
          ),
          "not found in 'data'."
        ),
        call. = FALSE
      )
    }
  } else if (is.numeric(data)) { # vector; coerce to data frame
    if (length(vars) > 1L) {
      stop("'smooth' requires multiple data vectors but only 1 provided.",
        call. = FALSE
      )
    }
    data <- tibble(!!(vars) := data)
  } else { # object we can't handle; bail out
    stop("'data', if supplied, must be a numeric vector or a data frame.",
      call. = FALSE
    )
  }
  data
}

#' @keywords internal
#' @noRd
`check_all_vars` <- function(model, data, smooths = NULL) {
  ## if we don't pass something to smooths get names of all variables used in
  ## the model
  vars <- if (is.null(smooths)) {
    term_names(model)
  } else {
    ## something passed to smooths; bail if not a list or numeric id vector
    if (!(is.list(smooths) || is.numeric(smooths))) {
      stop("Do not know how to handle supplied `smooths`.\n",
        "Must be a vector of smooth indices or a list of objects ",
        "that inherit from class `mgcv.smooth`.",
        call. = FALSE
      )
    }
    ## if a numeric vector of smoth indices, then extract those smooths
    ## and continue
    if (is.numeric(smooths)) {
      smooths <- get_smooths_by_id(model, smooths)
    }
    ## do all elements of smooths now inherit from mgcv_smooth?
    sms <- vapply(smooths, FUN = inherits, logical(1L), "mgcv.smooth")
    ## if they don't bail out with a helpful error
    if (!all(sms)) {
      stop("Elements ", paste(which(!sms), collapse = ", "),
        " of `smooths` do not inherit from class `mgcv.smooth`.",
        call. = FALSE
      )
    }
    ## if they do all inherit from the correct class, then run term_names
    ## on each element and combine - returns $term and $by from each smooth
    unlist(lapply(smooths, FUN = term_names))
  }

  ## check that the vars we need are in data
  smooth_vars <- vars %in% names(data)
  if (!all(smooth_vars)) {
    stop(
      paste(
        "Variable(s)",
        paste(paste0("'", vars[!smooth_vars], "'"),
          collapse = ", "
        ),
        "not found in 'data'."
      ),
      call. = FALSE
    )
  }

  ## if we get here then everything must be OK so return the required variable
  ## names invisibly in case it is useful
  invisible(vars)
}

#' Evaluate a spline at provided covariate values
#'
#' @param smooth currently an object that inherits from class `mgcv.smooth`.
#' @param model a fitted model; currently only [mgcv::gam()] and [mgcv::bam()]
#'   models are supported.
#' @param data a data frame of values to evaluate `smooth` at.
#' @param frequentist logical; use the frequentist covariance matrix?
#'
#' @inheritParams eval_smooth
#'
#' @importFrom tibble tibble add_column
#' @importFrom rlang := !!
#' @importFrom dplyr pull
#' @importFrom tidyselect all_of
#' @importFrom tidyr nest unnest
#' @importFrom mgcv PredictMat inSide
#' @export
`spline_values` <- function(
    smooth, data, model, unconditional,
    overall_uncertainty = TRUE, frequentist = FALSE) {
  is_soap <- is_soap_film(smooth)
  X <- PredictMat(smooth, data) # prediction matrix
  offset <- attr(X, "offset")
  start <- smooth[["first.para"]]
  end <- smooth[["last.para"]]
  para.seq <- start:end
  coefs <- coef(model)[para.seq]

  fit <- if (is_soap && !is.null(offset)) {
    drop(X %*% coefs) + attr(X, "offset")
  } else {
    drop(X %*% coefs)
  }

  label <- smooth_label(smooth)

  ## want full vcov for component-wise CI
  V <- get_vcov(model, unconditional = unconditional)

  ## variables for component-wise CIs for smooths
  column_means <- model[["cmX"]]
  lcms <- length(column_means)
  nc <- ncol(V)
  meanL1 <- smooth[["meanL1"]]
  eta_idx <- lss_eta_index(model)

  n_cons <- attr(smooth, "nCons")
  if (isTRUE(overall_uncertainty) && !is.null(n_cons) && n_cons > 0L) {
    if (lcms < nc) {
      column_means <- c(column_means, rep(0, nc - lcms))
    }
    Xcm <- matrix(column_means, nrow = nrow(X), ncol = nc, byrow = TRUE)
    if (!is.null(meanL1)) {
      Xcm <- Xcm / meanL1
    }
    Xcm[, para.seq] <- X
    # only apply the uncertainty from linear predictors of which this smooth
    # is a part of
    idx <- vapply(eta_idx, function(i, beta) any(beta %in% i),
      FUN.VALUE = logical(1L), beta = para.seq
    )
    idx <- unlist(eta_idx[idx])
    rs <- rowSums((Xcm[, idx, drop = FALSE] %*%
      V[idx, idx, drop = FALSE]) * Xcm[, idx, drop = FALSE])
  } else {
    rs <- rowSums((X %*% V[para.seq, para.seq, drop = FALSE]) * X)
  }

  ## standard error of the estimate
  se.fit <- sqrt(pmax(0, rs))

  ## identify which vars are needed for this smooth...
  keep_vars <- terms_in_smooth(smooth)
  ## ... then keep only those vars
  data <- select(data, all_of(keep_vars))

  ## Return object
  tbl <- tibble(.smooth = rep(label, nrow(X)), .estimate = fit, .se = se.fit)
  ## bind on the data
  tbl <- bind_cols(tbl, data)
  ## nest all columns with varying data
  tbl <- nest(tbl, data = all_of(c(".estimate", ".se", names(data))))

  tbl
}

#' Evaluate a spline at provided covariate values
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' The function `spline_values2()` has been renamed to `spline_values()` as of
#' version 0.9.0. This was allowed following the removal of `evaluate_smooth()`,
#' which was the only function using `spline_values()`. So `spline_values2()`
#' has been renamed to `spline_values()`.
#'
#' @param smooth currently an object that inherits from class `mgcv.smooth`.
#' @param model a fitted model; currently only [mgcv::gam()] and [mgcv::bam()]
#'   models are supported.
#' @param data an optional data frame of values to evaluate `smooth` at.
#' @param frequentist logical; use the frequentist covariance matrix?
#'
#' @inheritParams eval_smooth
#'
#' @keywords internal
#' @export
`spline_values2` <- function(
    smooth, data, model, unconditional,
    overall_uncertainty = TRUE, frequentist = FALSE) {
  spline_values(
    smooth = smooth, data = data, model = model,
    unconditional = unconditional, frequentist = frequentist
  )
}

`smooth_values` <- function(smooth, ...) {
  UseMethod("smooth_values")
}

#' @export
`smooth_values.univariate_scam_smooth` <- function(
    smooth, data, model, V,
    ...) {
  # get values of smooth
  X <- PredictMat(smooth, data) # prediction matrix
  off <- attr(X, "offset") # offset, if any
  if (is.null(off)) {
    off <- 0
  }
  para_seq <- smooth_coef_indices(smooth) # start:end
  coefs <- coef(model, parametrized = FALSE)[para_seq]

  # scam smooths work quite differently to mgcv smooths as X can contain
  # constant terms, need exponentiating etc
  which_exp <- which_exp_scam_coefs(model)[para_seq] # which betas need exp
  idx <- seq_along(coefs)[which_exp]
  # which exp function are we using?
  exp_fn <- exp_fun(model)
  # exponentiate any coefs that need it
  coefs[idx] <- exp_fn(coefs[idx])
  # coefs need reprarameterizing in some smooth type or padding with a 0
  stats <- scam_beta_se(smooth,
    beta = coefs, X = X, ndata = nrow(data),
    V = V
  )
  coefs <- stats$betas
  se_fit <- stats$se
  fit <- drop(X %*% coefs) + off
  list(fit = fit, se = se_fit)
}

`spline_values_scam` <- function(
    smooth, data, model,
    overall_uncertainty = TRUE, frequentist = FALSE) {
  # reclass the smooth to add classes needed for gratia's S3 methods to work
  smooth <- reclass_scam_smooth(smooth)

  ## want full vcov for component-wise CI
  V <- vcov(model, freq = frequentist, parametrized = TRUE)

  # get values of smooth & std errs, modified as needed for scam smooths
  sv <- smooth_values(smooth = smooth, data = data, model = model, V = V)

  fit <- sv$fit # fitted value at data
  se_fit <- sv$se # sqrt(pmax(0, sv$se)) # std err of fitted value

  label <- smooth_label(smooth)

  ## identify which vars are needed for this smooth...
  keep_vars <- terms_in_smooth(smooth)
  ## ... then keep only those vars
  data <- select(data, all_of(keep_vars))

  ## Return object
  tbl <- tibble(
    .smooth = rep(label, nrow(data)), .estimate = fit,
    .se = se_fit
  )
  ## bind on the data
  tbl <- bind_cols(tbl, data)
  ## nest all columns with varying data
  tbl <- nest(tbl, data = all_of(c(".estimate", ".se", names(data))))

  tbl
}

#' S3 methods to evaluate individual smooths
#'
#' @param smooth currently an object that inherits from class `mgcv.smooth`.
#' @param model a fitted model; currently only [mgcv::gam()] and [mgcv::bam()]
#'   models are supported.
#' @param data an optional data frame of values to evaluate `smooth` at.
#' @param ... arguments passed to other methods
#'
#' @inheritParams smooth_estimates
#'
#' @export
`eval_smooth` <- function(smooth, ...) {
  UseMethod("eval_smooth")
}

#' @rdname eval_smooth
#' @importFrom tibble add_column
#' @export
`eval_smooth.mgcv.smooth` <- function(smooth, model,
                                      n = 100,
                                      n_3d = NULL,
                                      n_4d = NULL,
                                      data = NULL,
                                      unconditional = FALSE,
                                      overall_uncertainty = TRUE,
                                      dist = NULL,
                                      ...) {
  by_var <- by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  ## deal with data if supplied
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = n_3d, n_4d = n_4d,
    id = which_smooth(
      model,
      smooth_label(smooth)
    )
  )

  ## values of spline at data
  eval_sm <- spline_values(smooth,
    data = data,
    unconditional = unconditional,
    model = model,
    overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

  # set some values to NA if too far from the data
  if (smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
    eval_sm <- too_far_to_na(smooth,
      input = eval_sm,
      reference = model[["model"]],
      cols = c(".estimate", ".se"),
      dist = dist
    )
  }
  ## return
  eval_sm
}

#' @rdname eval_smooth
#' @importFrom dplyr n mutate relocate bind_rows
#' @importFrom tidyselect all_of
#' @export
`eval_smooth.soap.film` <- function(
  smooth,
  model,
  n = 100,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  clip = TRUE, # ?hmm thinking
  ...
) {
  by_var <- by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  # Deal with data if supplied
  # As a special case, if no `data`, then we should generate some data here for
  # soap film from the boundary
  #if (is.null(data)) {
  #  data <- soap_film_data(smooth, n = n, n_3d = n_3d, n_4d = n_4d)
  #}
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = n_3d, n_4d = n_4d,
    id = which_smooth(
      model,
      smooth_label(smooth)
    )
  )

  # handle soap film smooths
  # can use this if Simon accepts the proposed changes to inSide()
  is_soap <- is_soap_film(smooth)
  if (is_soap) {
    bnd <- boundary(smooth) # smooth$xt$bnd
    # in_side <- inSide(bnd, x = data[[smooth$vn[[1]]]],
    #  y = data[[smooth$vn[[1]]]],
    # xname = "v", yname = "w") # needs fixed inSide
    # use in_side to filter the data before we evaluate the spline
    # any point outside the domain is NA anyway
    if (isTRUE(clip)) {
      is_inside <- inside(
        data,
        bnd,
        x_var = smooth$vn[[1]],
        y_var = smooth$vn[[2]]
      )

      data <- filter(data, is_inside)
    }
  }

  ## values of spline at data
  eval_sm <- spline_values(smooth,
    data = data,
    unconditional = unconditional,
    model = model,
    overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

  ## add on the boundary info
  if (is_soap) {
    # how many points on each boundary?
    pts <- vapply(bnd, \(x) length(x[[1]]), integer(1))
    # capture the boundary as a tibble
    bndry <- dplyr::bind_rows(bnd) |>
      mutate(.smooth = rep(smooth_label(smooth), times = n()),
        .estimate = rep(NA_real_, times = n()),
        .se = rep(NA_real_, times = n()),
        .bndry = rep(TRUE, times = n()),
        .loop = rep(seq_along(pts), times = pts)) |>
      relocate(all_of(c(".smooth", ".estimate", ".se", ".bndry", ".loop")),
        .before = 1L)
    bndry <- add_by_var_column(bndry, by_var = by_var)
    bndry <- add_smooth_type_column(bndry, sm_type = smooth_type(smooth))

    eval_sm <- eval_sm |>
      unnest(cols = "data") |>
      mutate(
        .bndry = rep(FALSE, times = n()),
        .loop = rep(NA_integer_, times = n())
      ) |>
      relocate(all_of(c(".bndry", ".loop")), .after = 5L) |>
      bind_rows(bndry) |>
      nest(data = !matches(c(".smooth", ".type", ".by")))
  }

  ## return
  eval_sm
}

#' @rdname eval_smooth
#' @importFrom tibble add_column
#' @export
`eval_smooth.scam_smooth` <- function(smooth, model,
                                      n = 100,
                                      n_3d = NULL,
                                      n_4d = NULL,
                                      data = NULL,
                                      unconditional = FALSE,
                                      overall_uncertainty = TRUE,
                                      dist = NULL,
                                      ...) {
  by_var <- by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  ## deal with data if supplied
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = n_3d, n_4d = n_4d,
    id = which_smooth(model, smooth_label(smooth))
  )

  ## values of spline at data
  eval_sm <- spline_values_scam(smooth,
    data = data, model = model,
    overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

  # set some values to NA if too far from the data
  if (smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
    eval_sm <- too_far_to_na(smooth,
      input = eval_sm,
      reference = model[["model"]],
      cols = c(".estimate", ".se"),
      dist = dist
    )
  }
  ## return
  eval_sm
}

#' Wrapper to `gratia::smooth_data()` and `gratia:::check_user_data()` for use
#' with [gratia::eval_smooth()] methods
#'
#' @param data an optional data frame of values for the smooth
#' @param model a fitted model
#' @param n numeric; the number of new observations to generate. Passed to
#'   [gratia::smooth_data()].
#' @param n_3d numeric; the number of new observations to generate for the third
#'   dimension of a 3D smooth. Passed to [gratia::smooth_data()].
#' @param n_4d numeric; the number of new observations to generate for the
#'   fourth (or higher) dimension(s) of a *k*D smooth (*k* >= 4). Passed to
#'   [gratia::smooth_data()].
#' @param id the number ID of the smooth within `model` to process.
#'
#' @keywords internal
#' @noRd
#' @importFrom rlang .data
#' @importFrom vctrs vec_slice
`process_user_data_for_eval` <- function(
    data, model, n, n_3d, n_4d, id,
    var_order = NULL) {
  if (is.null(data)) {
    data <- smooth_data(
      model = model,
      n = n,
      n_3d = n_3d,
      n_4d = n_4d,
      id = id,
      var_order = var_order
    )
  } else {
    smooth <- get_smooths_by_id(model, id)[[1L]]
    vars <- smooth_variable(smooth)
    by_var <- by_variable(smooth)
    if (!identical(by_var, "NA")) {
      vars <- append(vars, by_var)
    }
    ## if this is a by variable, filter the by variable for the required
    ## level now
    if (is_factor_by_smooth(smooth)) {
      data <- vec_slice(data, data[[by_var]] == by_level(smooth))
    }
  }
  data
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.fs.interaction` <- function(smooth, model, n = 100, data = NULL,
                                         unconditional = FALSE,
                                         overall_uncertainty = TRUE,
                                         ...) {
  by_var <- by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  # order of variables - we can specify fs smooths with the factor term
  # anywhere in the smooth definition:
  #   s(f, x1, x2, bs = "fs", xt = list(bs = "ds"))
  # and the code to evaluate fs smooths works if the terms are evaluated in
  # with the factor last. So use the reordering functionality in place for
  # >=3D tensor product smooths
  var_order <- reorder_tensor_smooth_terms(smooth)

  ## deal with data if supplied
  id <- which_smooth(model, smooth_label(smooth))
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = NULL, n_4d = NULL, id = id, var_order = var_order
  )

  ## values of spline at data
  eval_sm <- spline_values(smooth,
    data = data,
    unconditional = unconditional,
    model = model,
    overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

  ## return
  eval_sm
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.sz.interaction` <- function(smooth, model, n = 100, data = NULL,
                                         unconditional = FALSE,
                                         overall_uncertainty = TRUE,
                                         ...) {
  by_var <- by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  ## deal with data if supplied
  id <- which_smooth(model, smooth_label(smooth))
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = NULL, n_4d = NULL,
    id = id
  )

  ## values of spline at data
  eval_sm <- spline_values(smooth,
    data = data,
    unconditional = unconditional,
    model = model,
    overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))
  ## return
  eval_sm
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.random.effect` <- function(smooth, model, n = 100, data = NULL,
                                        unconditional = FALSE,
                                        overall_uncertainty = TRUE,
                                        ...) {
  by_var <- by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  ## deal with data if supplied
  id <- which_smooth(model, smooth_label(smooth))
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = NULL, n_4d = NULL,
    id = id
  )

  ## values of spline at data
  eval_sm <- spline_values(smooth,
    data = data,
    unconditional = unconditional,
    model = model,
    overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))
  ## return
  eval_sm
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.mrf.smooth` <- function(smooth, model, n = 100, data = NULL,
                                     unconditional = FALSE,
                                     overall_uncertainty = TRUE,
                                     ...) {
  .NotYetImplemented()
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.t2.smooth` <- function(smooth, model,
                                    n = 100,
                                    n_3d = NULL,
                                    n_4d = NULL,
                                    data = NULL,
                                    unconditional = FALSE,
                                    overall_uncertainty = TRUE,
                                    dist = NULL,
                                    ...) {
  by_var <- by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  # order of variables
  var_order <- reorder_tensor_smooth_terms(smooth)

  ## deal with data if supplied
  id <- which_smooth(model, smooth_label(smooth))
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = n_3d, n_4d = n_4d,
    id = id, var_order = var_order
  )

  ## values of spline at data
  eval_sm <- spline_values(smooth,
    data = data,
    unconditional = unconditional,
    model = model,
    overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

  # set some values to NA if too far from the data
  if (smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
    eval_sm <- too_far_to_na(smooth,
      input = eval_sm,
      reference = model[["model"]],
      cols = c(".estimate", ".se"),
      dist = dist
    )
  }

  tensor_term_order <- list(var_order) |>
    setNames(smooth_label(smooth))
  attr(eval_sm, "tensor_term_order") <- tensor_term_order

  ## return
  class(eval_sm) <- append(class(eval_sm), c("tensor_eval_sm", "eval_sm"),
    after = 0L
  )
  eval_sm
}

#' @rdname eval_smooth
#' @export
#' @importFrom tibble add_column
`eval_smooth.tensor.smooth` <- function(smooth, model,
                                        n = 100,
                                        n_3d = NULL,
                                        n_4d = NULL,
                                        data = NULL,
                                        unconditional = FALSE,
                                        overall_uncertainty = TRUE,
                                        dist = NULL,
                                        ...) {
  by_var <- by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  # order of variables
  var_order <- reorder_tensor_smooth_terms(smooth)

  # deal with data if supplied
  id <- which_smooth(model, smooth_label(smooth))
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = n_3d, n_4d = n_4d,
    id = id, var_order = var_order
  )

  ## values of spline at data
  eval_sm <- spline_values(smooth,
    data = data,
    unconditional = unconditional,
    model = model,
    overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))

  # set some values to NA if too far from the data
  if (smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
    eval_sm <- too_far_to_na(smooth,
      input = eval_sm,
      reference = model[["model"]],
      cols = c(".estimate", ".se"),
      dist = dist
    )
  }

  tensor_term_order <- list(var_order) |>
    setNames(smooth_label(smooth))
  attr(eval_sm, "tensor_term_order") <- tensor_term_order

  ## return
  class(eval_sm) <- append(class(eval_sm), c("tensor_eval_sm", "eval_sm"),
    after = 0L
  )
  eval_sm
}

#' Plot the result of a call to `smooth_estimates()`
#'
#' @param decrease_col,increase_col colour specifications to use for
#'   indicating periods of change. `col_change` is used when
#'   `change_type = "change"`, while `col_decrease` and `col_increase` are used
#'   when `change_type = "sizer"``.
#' @param change_lwd numeric; the value to set the `linewidth` to in
#'   [ggplot2::geom_line()], used to represent the periods of change.
#' @param ylim numeric; vector of y axis limits to use all *all* panels drawn.
#'
#' @inheritParams draw.gam
#'
#' @export
#' @importFrom patchwork wrap_plots
#'
#' @examples
#' load_mgcv()
#' # example data
#' df <- data_sim("eg1", seed = 21)
#' # fit GAM
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#' # plot all of the estimated smooths
#' sm <- smooth_estimates(m)
#' draw(sm)
#' # evaluate smooth of `x2`
#' sm <- smooth_estimates(m, select = "s(x2)")
#' # plot it
#' draw(sm)
#'
#' # customising some plot elements
#' draw(sm, ci_col = "steelblue", smooth_col = "forestgreen", ci_alpha = 0.3)
#'
#' # Add a constant to the plotted smooth
#' draw(sm, constant = coef(m)[1])
#'
#' # Adding change indicators to smooths based on derivatives of the smooth
#' d <- derivatives(m, n = 100) # n to match smooth_estimates()
#'
#' smooth_estimates(m) |>
#'   add_sizer(derivatives = d, type = "sizer") |>
#'   draw()
`draw.smooth_estimates` <- function(object,
                                    constant = NULL,
                                    fun = NULL,
                                    contour = TRUE,
                                    grouped_by = FALSE,
                                    contour_col = "black",
                                    n_contour = NULL,
                                    ci_alpha = 0.2,
                                    ci_col = "black",
                                    smooth_col = "black",
                                    resid_col = "steelblue3",
                                    decrease_col = "#56B4E9",
                                    increase_col = "#E69F00",
                                    change_lwd = 1.75,
                                    partial_match = FALSE,
                                    discrete_colour = NULL,
                                    discrete_fill = NULL,
                                    continuous_colour = NULL,
                                    continuous_fill = NULL,
                                    angle = NULL,
                                    ylim = NULL,
                                    crs = NULL,
                                    default_crs = NULL,
                                    lims_method = "cross",
                                    caption = TRUE,
                                    ...) {
  # add confidence intervals if they don't already exist
  if (!all(c(".lower_ci", ".upper_ci") %in% names(object))) {
    object <- object |> add_confint()
  }

  # grab tensor term order if present, if not present it is NULL & that's OK
  tensor_term_order <- attr(object, "tensor_term_order")

  # draw smooths
  # the factor in group_split is to reorder to way the smooths entered the
  # model
  sm_levs <- unique(object$.smooth)

  sm_l <- if (isTRUE(grouped_by)) {
    levs <- unique(str_split_fixed(object$.smooth, ":", n = 2)[, 1])
    # nest the object so we can reuse the code/ideas from draw.gam
    sm_l <- object |>
      nest(data = !all_of(c(".smooth", ".type", ".by"))) |>
        mutate(
          ..smooth.. = factor(.data$.smooth, levels = sm_levs),
          .term = str_split_fixed(.data$.smooth, ":", n = 2)[, 1],
          ..by.. = if_else(is.na(.data$.by), "..no_level..", .data$.by)
        ) |>
        relocate(".term", .before = 1L)
    grp_by_levs <- unique(sm_l$"..by..")
    sm_l |>
      group_split(factor(.data$.term, levels = sm_levs),
        factor(.data$"..by..", levels = grp_by_levs))
  } else {
    # the factor is to reorder to way the smooths entered the model
    group_split(object, factor(object$.smooth, levels = sm_levs))
  }
  ## plot
  plts <- map(sm_l,
    draw_smooth_estimates,
    constant = constant,
    fun = fun,
    contour = contour,
    contour_col = contour_col,
    n_contour = n_contour,
    ci_alpha = ci_alpha,
    ci_col = ci_col,
    smooth_col = smooth_col,
    increase_col = increase_col,
    decrease_col = decrease_col,
    change_lwd = change_lwd,
    partial_match = partial_match,
    discrete_colour = discrete_colour,
    discrete_fill = discrete_fill,
    continuous_colour = continuous_colour,
    continuous_fill = continuous_fill,
    angle = angle,
    ylim = ylim,
    crs = crs,
    default_crs = default_crs,
    lims_method = lims_method,
    tensor_term_order = tensor_term_order, # pass on tensor order info,
    caption = caption,
    ...
  )

  wrap_plots(plts)
}

#' @importFrom tidyr unnest
#' @importFrom tidyselect any_of
`draw_smooth_estimates` <- function(object,
                                    constant = NULL,
                                    fun = NULL,
                                    contour = TRUE,
                                    contour_col = "black",
                                    n_contour = NULL,
                                    ci_alpha = 0.2,
                                    ci_col = "black",
                                    smooth_col = "black",
                                    resid_col = "steelblue3",
                                    decrease_col = "#56B4E9",
                                    increase_col = "#E69F00",
                                    change_lwd = 1.75,
                                    partial_match = FALSE,
                                    discrete_colour = NULL,
                                    discrete_fill = NULL,
                                    continuous_colour = NULL,
                                    continuous_fill = NULL,
                                    angle = NULL,
                                    ylim = NULL,
                                    crs = NULL,
                                    default_crs = NULL,
                                    lims_method = "cross",
                                    tensor_term_order = NULL,
                                    caption = NULL,
                                    ...) {
  sm_vars <- tensor_term_order[[unique(object$.smooth)]]
  if (is.null(sm_vars)) {
    sm_vars <- if (".term" %in% names(object)) {
      vars_from_label(unique(object[[".term"]]))
    } else {
      vars_from_label(unique(object[[".smooth"]]))
    }
  }
  sm_dim <- length(sm_vars)
  sm_type <- unique(object[[".type"]])

  # set some values to NULL in case these components don't exist
  rug_data <- NULL
  p_residuals <- NULL
  ## unnest object if it has a list column 'data'
  if ((!is.null(object[["data"]]) && is.list(object$data))) {
    obj_nms <- names(object)
    ## preserve partial residuals and rug data if present
    if ("rug_data" %in% obj_nms) {
      rug_data <- object[["rug_data"]][[1L]]
    }
    if ("partial_residual" %in% obj_nms) {
      p_residuals <- object[["partial_residual"]][[1L]]
    }
    ## remove partial residuals and rug data from object
    object <- select(object, !any_of(c("partial_residual", "rug_data")))
    ## finally unnest
    object <- unnest(object, cols = "data")
  }

  if (sm_dim == 1L &&
    sm_type %in% c(
      "TPRS", "TPRS (shrink)", "CRS", "CRS (shrink)",
      "Cyclic CRS", "Cyclic P spline", "P spline", "B spline", "Duchon spline",
      "GP",
      "Mono inc P spline",
      "Mono dec P spline",
      "Convex P spline",
      "Concave P spline",
      "Mono dec conv P spline",
      "Mono dec conc P spline",
      "Mono inc conv P spline",
      "Mono inc conc P spline",
      "Mono inc 0 start P spline",
      "Mono inc 0 start P spline"
    )) {
    class(object) <- append(class(object), "mgcv_smooth", after = 0L)
  } else if (grepl("1d Tensor product", sm_type, fixed = TRUE)) {
    class(object) <- append(class(object), "mgcv_smooth", after = 0L)
  } else if (sm_type == "Random effect") {
    class(object) <- append(class(object),
      c("random_effect", "mgcv_smooth"),
      after = 0
    )
  } else if (sm_type == "Factor smooth") {
    class(object) <- append(class(object),
      c("factor_smooth", "mgcv_smooth"),
      after = 0
    )
  } else if (sm_type == "Constr. factor smooth") {
    class(object) <- append(class(object),
      c("sz_factor_smooth", "mgcv_smooth"),
      after = 0
    )
  } else if (sm_type == "SOS") {
    class(object) <- append(class(object),
      c("sos", "mgcv_smooth"),
      after = 0
    )
  } else if (sm_dim == 2L) {
    # all 2D smooths get these classes
    class(object) <- append(class(object),
      c("bivariate_smooth", "mgcv_smooth"),
      after = 0
    )
    # but TPRS smooths are isotropic so need special plotting
    # see issue #81. Duchon splines are a more general TPRS so
    # need to be handled the same way
    if (sm_type %in% c(
      "TPRS (2d)", "TPRS (shrink) (2d)",
      "Duchon spline (2d)"
    )) {
      class(object) <- append(class(object), "isotropic_smooth",
        after = 0
      )
    } else if (sm_type %in% "Soap film") {
      class(object) <- append(class(object), "soap_film", after = 0
      )
    }
  } else if (sm_dim == 3L) {
    # all 3D smooths get these classes
    class(object) <- append(class(object),
      c("trivariate_smooth", "mgcv_smooth"),
      after = 0
    )
    # but TPRS smooths are isotropic so need special plotting
    # see issue #81. Duchon splines are a more general TPRS so
    # need to be handled the same way, but we don't need to handle
    # this as a special method, so add after the trivariate_smooth
    # class
    if (sm_type %in% c(
      "TPRS (3d)", "TPRS (shrink) (3d)",
      "Duchon spline (3d)"
    )) {
      class(object) <- append(class(object), "isotropic_smooth",
        after = 1L
      )
    }
  } else if (sm_dim == 4L) {
    # all 2D smooths get these classes
    class(object) <- append(class(object),
      c("quadvariate_smooth", "mgcv_smooth"),
      after = 0
    )
    # but TPRS smooths are isotropic so need special plotting
    # see issue #81. Duchon splines are a more general TPRS so
    # need to be handled the same way, but we don't need to handle
    # this as a special method, so add after the trivariate_smooth
    # class
    if (sm_type %in% c(
      "TPRS (4d)", "TPRS (shrink) (4d)",
      "Duchon spline (4d)"
    )) {
      class(object) <- append(class(object), "isotropic_smooth",
        after = 1L
      )
    }
  } else {
    return(NULL)
  }

  plot_smooth(
    object,
    variables = sm_vars,
    rug = rug_data,
    partial_residuals = p_residuals,
    constant = constant,
    fun = fun,
    contour = contour,
    contour_col = contour_col,
    n_contour = n_contour,
    ci_alpha = ci_alpha,
    ci_col = ci_col,
    smooth_col = smooth_col,
    resid_col = resid_col,
    increase_col = increase_col,
    decrease_col = decrease_col,
    change_lwd = change_lwd,
    partial_match = partial_match,
    discrete_colour = discrete_colour,
    discrete_fill = discrete_fill,
    continuous_colour = continuous_colour,
    continuous_fill = continuous_fill,
    angle = angle,
    ylim = ylim,
    crs = crs,
    default_crs = default_crs,
    lims_method = lims_method,
    caption = caption,
    ...
  )
}
