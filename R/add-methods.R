#' Add fitted values from a model to a data frame
#'
#' @param data a data frame containing values for the variables used to fit the
#'   model. Passed to [stats::predict()] as `newdata`.
#' @param model a fitted model for which a [stats::predict()] method is
#'   available. S3 method dispatch is performed on the `model` argument.
#' @param value character; the name of the variable in which model predictions
#'   will be stored.
#' @param ... additional arguments passed to methods.
#'
#' @return A data frame (tibble) formed from `data` and fitted values from
#'   `model`.
#'
#' @export
`add_fitted` <- function(data, model, value = ".value", ...) {
  UseMethod("add_fitted", model)
}

#' Add fitted values from a GAM to a data frame
#'
#' @param type character; the type of predictions to return. See
#'   [mgcv::predict.gam()] for options.
#' @param ... additional arguments passed to [mgcv::predict.gam()].
#'
#' @return A data frame (tibble) formed from `data` and predictions from
#'   `model`.
#'
#' @inheritParams add_fitted
#'
#' @importFrom dplyr bind_cols
#' @importFrom stats predict
#' @importFrom rlang set_names !! :=
#' @importFrom tibble as_tibble add_column
#'
#' @export
#'
#' @examples
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' load_mgcv()
#' df <- data_sim("eg1", seed = 1)
#' df <- df[, c("y", "x0", "x1", "x2", "x3")]
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' # add fitted values to our data
#' add_fitted(df, m)
#'
#' # with type = "terms" or "iterms"
#' add_fitted(df, m, type = "terms")
#' \dontshow{
#' options(op)
#' }
`add_fitted.gam` <- function(data, model, value = ".fitted", type = "response",
                             ...) {
  ## coerce to tibble
  data <- as_tibble(data)

  ## predict using the predict method
  pred_vals <- predict(model, newdata = data, type = type, ...)

  ## check if pred_vals is a list
  if (is.list(pred_vals)) {
    pred_vals <- pred_vals[["fit"]]
  }

  ## having pruned off any standard errors, process the result
  if (is.array(pred_vals) && length(dim(pred_vals)) > 1L) {
    pred_vals <- as_tibble(pred_vals)
    if (type %in% c("terms", "iterms")) {
      pred_vals <-
        add_column(pred_vals,
          !!(".constant") := coef(model)[1],
          .before = 1L
        )
    }
    data <- bind_cols(data, pred_vals)
  } else {
    data <- add_column(data, !!value := as.numeric(pred_vals),
      .after = ncol(data)
    )
  }

  data
}

#' Add residuals from a model to a data frame
#'
#' @param data a data frame containing values for the variables used to fit the
#'   model. Passed to [stats::residuals()] as `newdata`.
#' @param model a fitted model for which a [stats::residuals()] method is
#'   available. S3 method dispatch is performed on the `model` argument.
#' @param value character; the name of the variable in which model residuals
#'   will be stored.
#' @param ... additional arguments passed to methods.
#'
#' @return A data frame (tibble) formed from `data` and residuals from `model`.
#'
#' @export
`add_residuals` <- function(data, model, value = ".residual", ...) {
  UseMethod("add_residuals", model)
}

#' Add residuals from a GAM to a data frame
#'
#' @param type character; the type of residuals to return. See
#'   [mgcv::residuals.gam()] for options.
#' @param ... additional arguments passed to [mgcv::residuals.gam()].
#'
#' @return A data frame (tibble) formed from `data` and residuals from `model`.
#'
#' @inheritParams add_fitted
#'
#' @importFrom dplyr bind_cols
#' @importFrom stats residuals
#' @importFrom rlang set_names !! :=
#' @importFrom tibble as_tibble add_column
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' df <- data_sim("eg1", seed = 1)
#' df <- df[, c("y", "x0", "x1", "x2", "x3")]
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' ##
#' add_residuals(df, m)
#' \dontshow{
#' options(op)
#' }
`add_residuals.gam` <- function(data, model, value = ".residual",
                                type = "deviance", ...) {
  ## coerce to tibble
  data <- as_tibble(data)

  ## predict using the predict method
  resid_vals <- residuals(model, type = type, ...)

  ## check that the number of data rows equals length of residuals
  if (nrow(data) != length(resid_vals)) {
    stop("Length of model residuals does not equal number of rows in 'data'",
      call. = FALSE
    )
  }

  data <- add_column(data, !!value := drop(resid_vals), .after = ncol(data))

  data
}

#' Add partial residuals
#'
#' @param data a data frame containing values for the variables used to fit the
#'   model. Passed to [stats::residuals()] as `newdata`.
#' @param model a fitted model for which a [stats::residuals()] method is
#'   available. S3 method dispatch is performed on the `model` argument.
#' @param ... arguments passed to other methods.
#'
#' @export
`add_partial_residuals` <- function(data, model, ...) {
  UseMethod("add_partial_residuals", model)
}

#' @rdname add_partial_residuals
#'
#' @inheritParams partial_residuals.gam
#'
#' @export
#'
#' @importFrom tibble is_tibble as_tibble
#' @importFrom dplyr bind_cols
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' df <- data_sim("eg1", seed = 1)
#' df <- df[, c("y", "x0", "x1", "x2", "x3")]
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' ## add partial residuals
#' add_partial_residuals(df, m)
#'
#' ## add partial residuals for selected smooths
#' add_partial_residuals(df, m, select = "s(x0)")
#' \dontshow{
#' options(op)
#' }
`add_partial_residuals.gam` <- function(data, model, select = NULL,
                                        partial_match = FALSE,
                                        ...) {
  ## coerce data to tibble
  if (!is_tibble(data)) {
    data <- as_tibble(data)
  }
  ## get a vector of labels for smooths
  sms <- smooths(model)
  ## which were selected; select = NULL -> all selected
  take <- check_user_select_smooths(sms,
    select = select,
    partial_match = partial_match
  )
  if (!any(take)) {
    stop("No smooth label matched 'select'. Try 'partial_match = TRUE'?",
      call. = FALSE
    )
  }
  sms <- sms[take] # subset to selected smooths

  ## compute partial resids
  p_resids <- compute_partial_residuals(model, terms = sms, data = data)

  ## bind partial residuals to data
  data <- bind_cols(data, as_tibble(p_resids))

  data
}

#' Add a constant to estimated values
#'
#' @param object a object to add a constant to.
#' @param constant the constant to add.
#' @param ... additional arguments passed to methods.
#' @param column character; for the `"tbl_df"` method, which column to add the
#'   constant too.
#'
#' @return Returns `object` but with the estimate shifted by the addition of
#'   the supplied constant.
#'
#' @author Gavin L. Simpson
#' @export
`add_constant` <- function(object, constant = NULL, ...) {
  UseMethod("add_constant")
}

#' @rdname add_constant
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect any_of
`add_constant.smooth_estimates` <- function(object, constant = NULL, ...) {
  ## If constant supplied, add it to `est`
  if (!is.null(constant)) {
    if (!is.numeric(constant)) {
      stop("'constant' must be numeric: supplied <", constant, ">",
        call. = FALSE
      )
    }
    object <- mutate(
      object,
      across(
        any_of(c(
          "est", "lower_ci", "upper_ci", ".estimate",
          ".lower_ci", ".upper_ci"
        )),
        .fns = ~ .x + constant
      )
    )
  }

  object
}

#' @rdname add_constant
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect any_of
`add_constant.smooth_samples` <- function(object, constant = NULL, ...) {
  ## If constant supplied, add it to `est`
  if (!is.null(constant)) {
    if (!is.numeric(constant)) {
      stop("'constant' must be numeric: supplied <", constant, ">",
        call. = FALSE
      )
    }
    object <- mutate(
      object,
      across(
        any_of(c(
          "value", "lower", "upper",
          ".value", ".lower_ci", ".upper_ci"
        )),
        .fns = ~ .x + constant
      )
    )
  }

  object
}

#' @rdname add_constant
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect any_of
`add_constant.mgcv_smooth` <- function(object, constant = NULL, ...) {
  ## If constant supplied, add it to `est`
  if (!is.null(constant)) {
    if (!is.numeric(constant)) {
      stop("'constant' must be numeric: supplied <", constant, ">",
        call. = FALSE
      )
    }
    object <- mutate(
      object,
      across(
        any_of(c(
          "est", "lower_ci", "upper_ci", ".estimate",
          ".lower_ci", ".upper_ci"
        )),
        .fns = ~ .x + constant
      )
    )
  }

  object
}

#' @rdname add_constant
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect any_of
`add_constant.parametric_effects` <- function(object, constant = NULL,
                                              ...) {
  ## If constant supplied, add it to `est`
  if (!is.null(constant)) {
    if (!is.numeric(constant)) {
      stop("'constant' must be numeric: supplied <", constant,
        ">",
        call. = FALSE
      )
    }
    object <- mutate(
      object,
      across(
        any_of(c(
          "partial", "lower", "upper", ".estimate",
          ".lower_ci", ".upper_ci"
        )),
        .fns = ~ .x + constant
      )
    )
  }

  object
}

#' @rdname add_constant
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
`add_constant.tbl_df` <- function(object, constant = NULL, column = NULL,
                                  ...) {
  if (is.null(column)) {
    stop("'column' to modify must be supplied.")
  }
  ## If constant supplied, add it to `est`
  if (!is.null(constant)) {
    if (!is.numeric(constant)) {
      stop("'constant' must be numeric: supplied <", constant,
        ">",
        call. = FALSE
      )
    }
    object <- mutate(
      object,
      across(all_of(column),
        .fns = ~ .x + constant
      )
    )
  }

  object
}

#' @rdname add_constant
#' @export
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
`add_constant.evaluated_parametric_term` <- function(object, constant = NULL,
                                                     ...) {
  ## If constant supplied, add it to `est`
  if (!is.null(constant)) {
    if (!is.numeric(constant)) {
      stop("'constant' must be numeric: supplied <", constant,
        ">",
        call. = FALSE
      )
    }
    object <- mutate(
      object,
      across(all_of(c("partial")),
        .fns = ~ .x + constant
      )
    )
  }

  object
}


#' Add a confidence interval to an existing object
#'
#' @param object a R object.
#' @param coverage numeric; the coverage for the interval. Must be in the range
#'   0 < `coverage` < 1.
#' @param ... arguments passed to other methods.
#'
#' @export
`add_confint` <- function(object, coverage = 0.95, ...) {
  UseMethod("add_confint")
}

#' @rdname add_confint
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#'
#' @export
`add_confint.smooth_estimates` <- function(object, coverage = 0.95, ...) {
  # check if this is nested
  nms <- names(object)

  if (!all(c(".estimate", ".se") %in% nms)) {
    stop(
      "'object' does not contain one or both of ",
      "'.estimate', '.se'.",
      "\n  Did you use `smooth_estimates(..., unnest = FALSE)`?"
    )
  }

  ## compute the critical value
  crit <- coverage_normal(coverage)

  ## add the frequentist confidence interval
  object <- mutate(object,
    .lower_ci = .data$.estimate - (crit * .data$.se),
    .upper_ci = .data$.estimate + (crit * .data$.se)
  )

  ## return
  object
}

#' @rdname add_confint
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#'
#' @export
`add_confint.parametric_effects` <- function(object, coverage = 0.95, ...) {
  # check if this is nested
  nms <- names(object)

  if (!all(c(".partial", ".se") %in% nms)) {
    stop(
      "'object' does not contain one or both of ",
      "'.partial', '.se'.",
      "\n  Did you use `partial_effects(..., unnest = FALSE)`?"
    )
  }

  ## compute the critical value
  crit <- coverage_normal(coverage)

  ## add the frequentist confidence interval
  object <- mutate(object,
    .lower_ci = .data$.partial - (crit * .data$.se),
    .upper_ci = .data$.partial + (crit * .data$.se)
  )

  ## return
  object
}

#' @rdname add_confint
#' @importFrom rlang .data
#' @importFrom dplyr mutate relocate
#'
#' @export
`add_confint.default` <- function(object, coverage = 0.95, ...) {
  nms <- names(object)

  if (!all(c(".estimate", ".se") %in% nms)) {
    stop(
      "'object' does not contain one or both of ",
      "'.estimate' or '.se'."
    )
  }

  ## compute the critical value
  crit <- coverage_normal(coverage)

  ## add the frequentist confidence interval
  object <- mutate(object,
    .lower_ci = .data$.estimate - (crit * .data$.se),
    .upper_ci = .data$.estimate + (crit * .data$.se)
  ) |>
    relocate(all_of(c(".lower_ci", ".upper_ci")),
      .after = all_of(".se")
    )

  ## return
  object
}

#' Add indicators of significant change after SiZeR
#'
#' @param object an R object. Currently supported methods are for classes
#'   `"derivatives"`.
#' @param type character; `"change"` adds a single variable to `object`
#'   indicating where the credible interval on the derivative excludes 0.
#'   `"sizer"` adds two variables indicating whether the derivative is postive
#'   or negative.
#' @param ... arguments passed to other methods
#'
#' @export
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' df <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 42)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' ## first derivatives of all smooths using central finite differences
#' d <- derivatives(m, type = "central") |>
#'   add_sizer()
#'
#' # default adds a .change column
#' names(d)
`add_sizer` <- function(object, type = c("change", "sizer"), ...) {
  UseMethod("add_sizer")
}

#' @export
#' @importFrom dplyr mutate case_when relocate
#' @importFrom tidyselect any_of
#' @importFrom rlang .data
#' @rdname add_sizer
`add_sizer.derivatives` <- function(object, type = c("change", "sizer"), ...) {
  # match the type argument
  type <- match.arg(type)

  # if just doing a change indicator
  object <- if (isTRUE(identical(type, "change"))) {
    object |>
      mutate(
        .change =
          case_when(.data$.lower_ci > 0 |
            .data$.upper_ci < 0 ~ .data$.derivative,
          .default = NA_real_
          )
      )
  } else { # other wise we are adding a sizer-like indicator
    object |>
      mutate(
        .decrease = case_when(.data$.upper_ci < 0 ~ .data$.derivative,
          .default = NA_real_
        ),
        .increase = case_when(.data$.lower_ci > 0 ~ .data$.derivative,
          .default = NA_real_
        )
      )
  }
  object <- object |>
    relocate(any_of(c(
      ".smooth", ".by", ".fs", ".derivative",
      ".se", ".crit", ".lower_ci", ".upper_ci", ".change", ".decrease",
      ".increase"
    )), .before = 1L)
  object
}

#' @param derivatives an object of class `"derivatives"`, resulting from a call
#'   to [derivatives()].
#' @export
#' @importFrom dplyr mutate case_when bind_cols
#' @importFrom rlang .data
#' @rdname add_sizer
`add_sizer.smooth_estimates` <- function(
    object, type = c("change", "sizer"),
    derivatives = NULL, ...) {
  # match the type argument
  type <- match.arg(type)

  # must supply derivatives
  if (is.null(derivatives)) {
    stop("An object of class 'derivatives' must be supplied.")
  }
  # must be of the correct class
  if (!inherits(derivatives, "derivatives")) {
    stop("Supplied 'derivatives' is not of class 'derivatives'")
  }
  # if we get this far, derivatives must be checked to see that it matches
  # object. Right now this isn't easy as derivatives is older and stores
  # all covariate information in column `data`, so for now, just leave this
  # up to the user to ensure
  if (nrow(object) != nrow(derivatives)) {
    stop("Number of rows in 'object' and 'derivatives' are not equal.")
  }

  # if just doing a change indicator
  object <- if (isTRUE(identical(type, "change"))) {
    #derivatives <- derivatives |>
    object |>
      mutate(
        .change =
          case_when(derivatives$.lower_ci > 0 |
            derivatives$.upper_ci < 0 ~ .data$.estimate,
          .default = NA_real_
          )
      )
  } else { # otherwise we are adding a sizer-like indicator
    #derivatives <- derivatives |>
    object |>
      mutate(
        .decrease = case_when(derivatives$.upper_ci < 0 ~ .data$.estimate,
          .default = NA_real_
        ),
        .increase = case_when(derivatives$.lower_ci > 0 ~ .data$.estimate,
          .default = NA_real_
        )
      )
  }
  object <- object |>
    relocate(any_of(c(
      ".smooth", ".type", ".by", ".fs", ".estimate",
      ".se", ".lower_ci", ".upper_ci", ".change", ".decrease",
      ".increase"
    )), .before = 1L)
  object
}

#' Add posterior draws from a model to a data object
#'
#' Adds draws from the posterior distribution of `model` to the data `object`
#' using one of [fitted_samples()], [predicted_samples()], or
#' [posterior_samples()].
#'
#' @param object a data frame or tibble to which the posterior draws will be
#'   added.
#' @param model a fitted GAM (or GAM-like) object for which a posterior draw
#'   method exists.
#' @param n integer; the number of posterior draws to add.
#' @param seed numeric; a value to seed the random number generator.
#' @param select character; select which smooth's posterior to draw from. The
#'   default, `NULL`, means the posteriors of all smooths in model wil be
#'   sampled from individually. If supplied, a character vector of requested
#'   smooth terms.
#' @param ... arguments are passed to the posterior draw function, currently one
#'   of [fitted_samples()], [predicted_samples()], or [posterior_samples()]. `n`
#'   and `seed` are already specified here as arguments and are also passed on
#'   to the posterior sampling function.
#'
#' @importFrom dplyr mutate row_number left_join join_by
#' @export
#'
#' @examples
#' \dontshow{
#' op <- options(pillar.sigfig = 3)
#' }
#' load_mgcv()
#'
#' df <- data_sim("eg1", n = 400, seed = 42)
#'
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' # add fitted samples (posterior draws of the expected value of the response)
#' # note that there are 800 rows in the output: 400 data by `n = 2` samples.
#' df |>
#'   add_fitted_samples(m, n = 2, seed = 84)
#'
#' # add posterior draws from smooth s(x2)
#' df |>
#'   add_smooth_samples(m, n = 2, seed = 2, select= "s(x2)")
#' \dontshow{
#'   options(op)
#' }
`add_fitted_samples` <- function(object, model, n = 1, seed = NULL, ...) {
  # compute fitted samples
  fs <- fitted_samples(model, data = object, n = n, seed = seed, ...)
  # need to have .row in object so we can join with the fitted samples
  object <- object |>
    mutate(.row = row_number())
  object |>
    left_join(fs, by = join_by(".row" == ".row"))
}

#' @export
#' @importFrom dplyr mutate row_number left_join join_by
#' @rdname add_fitted_samples
`add_predicted_samples` <- function(object, model, n = 1, seed = NULL, ...) {
  # compute predicted samples
  ps <- predicted_samples(model, data = object, n = n, seed = seed, ...)
  # need to have .row in object so we can join with the fitted samples
  object <- object |>
    mutate(.row = row_number())
  object |>
    left_join(ps, by = join_by(".row" == ".row"))
}

#' @export
#' @importFrom dplyr mutate row_number left_join join_by
#' @rdname add_fitted_samples
`add_posterior_samples` <- function(object, model, n = 1, seed = NULL, ...) {
  # compute posterior samples
  ps <- posterior_samples(model, data = object, n = n, seed = seed, ...)
  # need to have .row in object so we can join with the fitted samples
  object <- object |>
    mutate(.row = row_number())
  object |>
    left_join(ps, by = join_by(".row" == ".row"))
}


#' @export
#' @importFrom dplyr mutate row_number left_join join_by
#' @rdname add_fitted_samples
`add_smooth_samples` <- function(
  object,
  model,
  n = 1,
  seed = NULL,
  select = NULL,
  ...
) {
  # compute posterior samples
  ss <- smooth_samples(
    model, data = object, n = n, seed = seed, select = select, ...
  )
  ss <- ss[c(".smooth", ".term", ".row", ".draw", ".value")]
  # need to have .row in object so we can join with the fitted samples
  object <- object |>
    mutate(.row = row_number())
  object |>
    left_join(ss, by = join_by(".row" == ".row"))
}
