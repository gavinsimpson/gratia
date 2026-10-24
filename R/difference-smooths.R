#' Differences of factor smooth interactions
#'
#' Estimates pairwise differences (comparisons) between factor smooth
#' interactions (smooths with a factor `by` argument) for pairs of groups
#' defined by the factor. The group means can be optionally included in the
#' difference. First and second derivatives of these differences, including
#' partial derivatives across smooth surfaces, can also be estimated.
#'
#' @param model A fitted model.
#' @param select character, logical, or numeric; which smooths to compare. If
#'   `NULL`, the default, then all model smooths are factor-smooth interactions
#'   are compared. Numeric `select` indexes the smooths in the order they are
#'   specified in the formula and stored in `object`. Character `select` matches
#'   the labels for smooths as shown for example in the output from
#'   `summary(object)`. Logical `select` operates as per numeric `select` in the
#'   order that smooths are stored. Careful selection is needed because it is
#'   not allowed to compare smooths of different covariates or of different
#'   factor-by variables.
#'
#'   For character `select`, specific named smooths cane be provided, in which
#'   case, the exact names of the smooths (as given by [smooths()], for example,
#'   can be specified, and `partial_match` must be set to `FALSE`.
#' @param smooth `r lifecycle::badge("deprecated")` Use `select` instead.
#' @param n numeric; the number of points at which to evaluate the difference
#'   between pairs of smooths.
#' @param n_2d numeric; the number of points along each of the first two axes
#'   of a smooth surface, including surface panels of higher-dimensional smooths.
#'   The default is 50 in plotting and plot-preparation functions. If `NULL`,
#'   use `n` instead. Ignored when evaluation `data` are supplied. Factor levels
#'   are retained, and curves with only one continuous covariate use `n`.
#' @param n_3d,n_4d numeric; the number of conditioning values for smooths of
#'   three or four covariates. The defaults are 16 and 4, respectively. If
#'   `NULL`, use `n`. Ignored when evaluation `data` are supplied.
#' @param ci_level numeric between 0 and 1; the coverage of credible interval.
#' @param data data frame of locations at which to evaluate the difference
#'   between smooths.
#' @param group_means logical; should the group means be included in the
#'   difference? Must be `FALSE` when `order > 0`.
#' @param order numeric; `0` (the default) estimates smooth differences;
#'   `1` and `2` estimate their first and second derivatives.
#' @param focal character; one differentiation coordinate shared by all
#'   compared smooths. Inferred for univariate smooths; required for
#'   multivariate smooths when `order > 0`.
#' @param type character; finite-difference scheme for derivatives. One of
#'   `"central"` (the default), `"forward"`, or `"backward"`.
#' @inheritParams derivatives eps wrt
#' @param partial_match logical; should `smooth` match partially against
#'   `smooths`? If `partial_match = TRUE`, `smooth` must only be a single
#'   string, a character vector of length 1. Unlike similar functions, the
#'   default here is `TRUE` because the intention is that users will be matching
#'   against factor-by smooth labels.
#' @param unconditional logical; account for smoothness selection in the model?
#' @param frequentist logical; use the frequentist covariance matrix?
#' @param interval character; `"confidence"` (the default) gives pointwise
#'   intervals. `"simultaneous"` gives simultaneous intervals for differences
#'   of smooths at the supplied covariate combinations, separately for each
#'   pair of factor levels.
#' @inheritParams fitted_values
#' @param ... arguments passed to other methods. Not currently used.
#'
#' @inheritParams smooth_estimates
#'
#' @inheritSection derivatives Parallel calculation
#'
#' @details
#' With the default simulation calibration, simultaneous intervals cover smooth differences at
#' the evaluated covariate combinations for each pair of factor levels, with
#' approximate posterior probability `ci_level` when using the Bayesian
#' covariance. They do not provide joint coverage across all pairs, outside
#' the evaluation set, or between its points. With `data = NULL`, the evaluation
#' set is generated using `n`, `n_2d`, `n_3d`, and `n_4d`. Differences and interval
#' limits are on the linear predictor scale, including group means when requested.
#' For supported univariate cubic splines, `simultaneous_method = "tube"`
#' instead covers a continuous interval, including between returned points;
#' see [simultaneous_bands]. Coverage remains separate for each pair.
#'
#' `variables` determines the order used to generate and display the evaluation
#' grid. The first two covariates are the x and y axes. The third covariate
#' defines facets for three-dimensional smooths, or facet rows for
#' four-dimensional smooths; the fourth defines facet columns. Use `data` to
#' choose particular conditioning values. The resolved order is saved with the
#' estimates for use by [draw()].
#'
#' The intervals use the joint coefficient covariance selected by
#' `unconditional` and `frequentist`, retaining covariance between the two
#' smooths. The covariance must be positive semidefinite. With
#' `frequentist = TRUE`, simulation instead uses the frequentist covariance
#' of the coefficient estimators. `n_sim`, `n_cores`, and `seed` are ignored for
#' pointwise intervals. An explicit seed scopes the entire call: each pair uses
#' a new batch of coefficient draws, and the caller's random number state is
#' restored on exit. With `seed = NULL`, the current random number state is
#' used and advanced.
#'
#' With `order > 0`, the estimate is the difference between the derivatives
#' of the selected smooths, on the linear predictor scale. Equivalently, it is
#' the derivative of their difference. The estimate and its uncertainty are
#' computed from the difference of the derivative matrices, retaining covariance
#' between the smooths. Group means are excluded, and tube intervals are
#' unavailable for derivatives; use simulation for simultaneous intervals.
#'
#' For a multivariate smooth, `focal` identifies the coordinate to differentiate.
#' All other independent coordinates are fixed at their value in each evaluation
#' row. They can vary between rows, so `data` may describe a full surface or
#' several slices. `order = 2` differentiates twice with respect to `focal`;
#' mixed partial derivatives are not supported. Generated grids adjust the
#' focal range for the finite-difference scheme; supplied evaluation locations
#' are preserved.
#'
#' With `wrt = "covariate"`, model expressions are reevaluated after changing
#' the raw focal covariate. Generated raw grids use [smooth_data()] and require
#' each axis to depend on one distinct original covariate. Supply raw `data`
#' for other transformations. With `wrt = "smooth"`, only the named smooth
#' coordinate is changed. Derivative controls are ignored when `order = 0`.
#' Raw-coordinate comparisons match the same original covariates, excluding
#' the comparison factor. Evaluated smooth coordinates may differ between
#' factor levels when a transformation involves that factor. The finite-difference
#' step must fit within the fitted focal range for generated grids, including
#' grids with only one focal value.
#'
#' @return A tibble of class `"difference_smooth"`. Columns `.smooth`, `.by`,
#'   `.level_1`, and `.level_2` identify each comparison; `.diff` contains the
#'   estimate for level 1 minus level 2, with `.se`, `.lower_ci`, `.upper_ci`,
#'   and evaluation covariates. For `order > 0`, `.diff` contains the derivative
#'   difference and `.focal` names its coordinate. The `derivative_info`
#'   attribute records `order`, `focal`, `wrt`, `type`, and the resolved step
#'   `eps`. The `plot_variables` attribute records the plotting axes.
#'
#' @seealso [derivatives()], [partial_derivatives()]
#'
#' @export
#' @examples
#' load_mgcv()
#' # Simulate group-specific curves with Gaussian noise and fit factor-by smooths
#' df <- data_sim("eg4", n = 300, seed = 42)
#' m <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")
#'
#' # Compare the centred smooth shapes for each pair of factor levels
#' sm_dif <- difference_smooths(m, select = "s(x2)")
#' sm_dif
#' draw(sm_dif)
#'
#' # Compare rates of change instead of smooth values, using the same fit
#' slope_dif <- difference_smooths(m, select = "s(x2)", order = 1)
#' slope_dif
`difference_smooths` <- function(model, ...) {
  UseMethod("difference_smooths")
}

#' @export
#'
#' @importFrom purrr pmap
#' @importFrom dplyr bind_rows
#' @importFrom tibble add_column as_tibble
#' @importFrom stats qnorm coef
#' @importFrom utils combn
#' @importFrom lifecycle deprecated is_present
#' @importFrom stringr str_detect str_replace_all
#'
#' @rdname difference_smooths
`difference_smooths.gam` <- function(model,
  select = NULL,
  smooth = deprecated(),
  n = 100,
  n_2d = 50,
  ci_level = 0.95,
  data = NULL,
  group_means = FALSE,
  partial_match = TRUE,
  unconditional = FALSE,
  frequentist = FALSE,
  envir = NULL,
  ...,
  interval = c("confidence", "simultaneous"),
  n_sim = 10000,
  n_cores = 1,
  seed = NULL,
  variables = NULL,
  n_3d = 16,
  n_4d = 4,
  simultaneous_method = c('simulation', 'tube'),
  simultaneous_domain = NULL, tube_control = list(),
  order = 0L, focal = NULL,
  type = c("central", "forward", "backward"), eps = NULL,
  wrt = c("smooth", "covariate")
) {
  if (!is.numeric(order) || length(order) != 1L || is.na(order) ||
      !order %in% 0:2) {
    cli::cli_abort("{.arg order} must be 0, 1, or 2.")
  }
  if (order > 0L && !identical(group_means, FALSE)) {
    cli::cli_abort("Derivatives of smooth differences require {.code group_means = FALSE}.")
  }
  interval <- match.arg(interval)
  simultaneous_method <- resolve_simultaneous_method(simultaneous_method, interval,
    simultaneous_domain, tube_control)
  if (order > 0L && simultaneous_method == "tube") {
    cli::cli_abort("Tube intervals are not available for derivatives of smooth differences; use simulation.")
  }
  if (simultaneous_method == 'tube') tube_check_model(model)
  if (interval == "simultaneous" && simultaneous_method == 'simulation') {
    validate_smooth_interval(interval, ci_level, n_sim, n_cores, seed)
  }
  if (simultaneous_method == 'simulation' && identical(interval, "simultaneous") && !is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
       abs(seed) > .Machine$integer.max || seed != trunc(seed))) {
    stop("`seed` must be NULL or a single integer.", call. = FALSE)
  }
  model <- with_model_envir(model, envir)
  if (lifecycle::is_present(smooth)) {
    lifecycle::deprecate_warn("0.8.9.9", "difference_smooths(smooth)",
      "difference_smooths(select)")
    select <- smooth
  }
  if (is.null(select)) {
    stop("Must specify a smooth to difference via 'select'.")
  }

  selection <- difference_smooth_selection(model, select, partial_match)
  sm_ids <- selection$ids
  smooths <- selection$smooths
  if (order > 0L && length(smooths) < 2L) {
    cli::cli_abort("Select at least two factor-by smooths to compare their derivatives.")
  }
  model <- plan_smooth_variables(model, sm_ids, variables = variables)
  if (order > 0L) {
    type <- match.arg(type)
    wrt <- match.arg(wrt)
    return(difference_smooth_derivatives(model, select, sm_ids, smooths,
      data, n, n_2d, n_3d, n_4d, focal, wrt, type, order, eps,
      unconditional, frequentist, interval, ci_level, n_sim, n_cores, seed))
  }
  if (is.null(data)) {
    sm_data <- map(sm_ids, function(id) smooth_data(model, id,
      n = n, n_2d = n_2d, n_3d = n_3d, n_4d = n_4d,
      var_order = smooth_plot_variables(model, id), include_all = TRUE))
    data <- bind_rows(sm_data)
  } else {
    data <- as_tibble(data)
    for (sm in smooths) data <- prepare_smooth_data(model, sm, data)
    # Expressions such as `group = levels(group)` supply character values.
    # Retain the fitted factor order when constructing their comparisons.
    for (sm in smooths) {
      by <- by_variable(sm)
      if (is.character(data[[by]])) {
        template <- prepare_smooth_data(model, sm)[[by]]
        data[[by]] <- factor(data[[by]], levels = levels(template),
          ordered = is.ordered(template))
      }
    }
  }
  comparison <- difference_smooth_pairs(smooths, select, data)
  by_var <- comparison$by
  smooth_var <- comparison$variables
  pairs <- comparison$pairs

  Xp <- predict_model(model, newdata = data, type = "lpmatrix")
  V <- get_vcov(model,
    unconditional = unconditional,
    frequentist = frequentist
  )
  coefs <- coef(model)

  calculate <- function() {
    tasks <- pmap(pairs, prepare_difference,
      select = select, by_var = by_var,
      smooth_var = smooth_var, data = data, Xp = Xp, V = V,
      coefs = coefs, group_means = group_means,
      interval = interval, ci_level = ci_level, n_sim = n_sim, n_cores = n_cores)
    result <- smooth_task_map(tasks, "calculate_difference")
    if (interval == 'simultaneous' && simultaneous_method == 'tube') {
      result <- tube_difference_intervals(result, tasks, model, smooths, data,
        by_var, ci_level, simultaneous_domain, tube_control)
      records <- attr(result, 'tube_records')
      result <- bind_rows(result)
      attr(result, 'interval_info') <- c(list(interval = interval, level = ci_level,
        method = 'gaussian'), tube_info(records, 'comparison', unconditional, frequentist))
      return(result)
    }
    if (interval == "simultaneous") {
      columns <- lapply(tasks, function(x) match(names(x$coefs), names(coefs)))
      result <- smooth_interval_map(result, columns, V, ci_level, n_sim, n_cores,
        estimate = ".diff", critical = FALSE)
    }
    bind_rows(result)
  }
  # Scope the seed once: do not restart the same draw sequence for every pair.
  result <- if (simultaneous_method == 'simulation' && identical(interval, "simultaneous") && !is.null(seed)) {
    withr::with_seed(seed, calculate())
  } else {
    calculate()
  }
  attr(result, "plot_variables") <- setNames(
    rep(list(smooth_plot_variables(model, sm_ids[[1L]])),
      length(unique(result$.smooth))), unique(result$.smooth))
  result
}

# A shared smooth can accompany factor-by terms in a decomposed model.
difference_smooth_selection <- function(model, select, partial_match) {
  take <- check_user_select_smooths(smooths(model), select,
    partial_match = partial_match)
  ids <- which(take)
  smooths <- get_smooths_by_id(model, ids)
  is_by <- vapply(smooths, is_factor_by_smooth, logical(1L))
  list(ids = ids[is_by], smooths = smooths[is_by])
}

difference_smooth_pairs <- function(smooths, select, data, derivative = FALSE) {
  if (length(select) == 1L && !derivative) {
    by_var <- by_variable(smooths[[1L]])
    smooth_var <- smooth_variable(smooths[[1L]])
    pairs <- as_tibble(as.data.frame(t(combn(levels(data[[by_var]]), 2)),
      stringsAsFactor = FALSE
    ))
  } else {
    # Read factor levels from smooth metadata; function calls can contain
    # parentheses and punctuation that cannot be parsed as simple labels.
    # get the smooth variable part
    smooth_var <- lapply(
      smooths,
      \(x) paste(smooth_variable(x), collapse = ",")
    ) |> unique()
    if (length(smooth_var) > 1L) {
      stop("Can't currently compare across smooths of different variables.")
    } else {
      # now reset smooth_var so it can be found later
      smooth_var <- smooth_variable(smooths[[1L]])
    }
    # get the by var part
    by_var <- vapply(smooths, by_variable, character(1L)) |> unique()
    if (length(unique(by_var)) > 1L) {
      stop(
        "Can't currently compare factor-by smooths with different `by`",
        "variables.")
    }

    lvls <- unique(vapply(smooths, by_level, character(1L)))
    if (derivative) {
      if (length(lvls) != length(smooths)) {
        cli::cli_abort("Select one smooth for each compared factor level.")
      }
      if (length(select) == 1L) {
        lvls <- intersect(levels(data[[by_var]]), lvls)
      }
      if (length(lvls) < 2L) {
        cli::cli_abort("At least two selected factor levels are required.")
      }
    }
    # finally compute the pairs
    pairs <- as_tibble(as.data.frame(t(combn(lvls, 2)),
      stringsAsFactor = FALSE
    ))
  }
  names(pairs) <- paste0("f", 1:2)
  list(pairs = pairs, by = by_var, variables = smooth_var)
}

#' @export
`difference_smooths.bam` <- function(model, ...) {
  NextMethod()
}

#' @export
`difference_smooths.gamm` <- function(model, ...) {
  difference_smooths(model[["gam"]], ...)
}

#' @export
`difference_smooths.gamm4` <- function(model, ...) {
  difference_smooths(gamm4_gam(model), ...)
}

#' @importFrom tibble new_tibble
#' @importFrom dplyr bind_cols
#' @importFrom stringr str_extract
`prepare_difference` <- function(f1, f2, select, by_var, smooth_var, data, Xp, V,
                              coefs, group_means = FALSE,
                              interval = "confidence", ci_level = 0.95,
                              n_sim = 10000, n_cores = 1) {
  ## make sure f1 and f2 are characters
  f1 <- as.character(f1)
  f2 <- as.character(f2)
  cnames <- colnames(Xp)

  # what are we keeping?
  keep <- if (isTRUE(group_means)) {
    # still need to fix this to accept length(select) > 1L - Done, but not clean
    if (length(select) > 1L) {
      rg1 <- select[str_detect(select, paste0(":", by_var, f1))]
      rg2 <- select[str_detect(select, paste0(":", by_var, f2))]
    } else {
      # columns of Xp associated with pair of smooths, including parametric
      # terms for the group means
      rg1 <- paste0(by_var, f1)
      rg2 <- paste0(by_var, f2)
    }
    # coefs for smooths to be included
    c1 <- str_detect(cnames, fixed(rg1))
    c2 <- str_detect(cnames, fixed(rg2))
    # group means could also be added via a random effect smooth
    # check for s(by_var).1 in colnames of Xp
    cg <- str_detect(
      cnames,
      paste0("^(s|[t][ei2])\\(", by_var, "\\)\\.{1}\\d+$")
    )
    # set the intercept to be included also
    c0 <- str_detect(cnames, fixed("(Intercept)"))
    (c0 | c1 | c2 | cg)
  } else {
    # columns of Xp associated with the pair of smooths, but not group means
    if (length(select) > 1L) {
      rg1 <- select[str_detect(select, paste0(":", by_var, f1))]
      rg2 <- select[str_detect(select, paste0(":", by_var, f2))]
    } else {
      rg1 <- mgcv_by_smooth_labels(select, by_var, f1)
      rg2 <- mgcv_by_smooth_labels(select, by_var, f2)
    }
    c1 <- str_detect(cnames, fixed(rg1))
    c2 <- str_detect(cnames, fixed(rg2))
    (c1 | c2)
  }

  rows <- difference_smooth_rows(data, by_var, f1, f2, smooth_var)
  r1 <- rows$first
  r2 <- rows$second

  # Send only matched rows and active coefficient columns for this pair.
  list(first = Xp[r1, keep, drop = FALSE], second = Xp[r2, keep, drop = FALSE],
    V = V[keep, keep, drop = FALSE], coefs = coefs[keep],
    data = smooth_task_data(data[r1, smooth_var, drop = FALSE]),
    select = select, by_var = by_var, f1 = f1, f2 = f2,
    interval = interval, ci_level = ci_level)
}

# Match covariate combinations before differencing the factor levels.
difference_smooth_rows <- function(data, by_var, f1, f2, variables) {
  r1 <- which(data[[by_var]] == f1)
  r2 <- which(data[[by_var]] == f2)
  keys1 <- data[r1, variables, drop = FALSE]
  keys2 <- data[r2, variables, drop = FALSE]
  if (anyDuplicated(keys1) || anyDuplicated(keys2)) {
    stop("Factor levels '", f1, "' and '", f2,
      "' must have unique covariate combinations for the selected smooth.",
      call. = FALSE)
  }
  matched <- vctrs::vec_match(keys1, keys2)
  if (!length(r1) || length(r1) != length(r2) || anyNA(matched)) {
    stop("Factor levels '", f1, "' and '", f2,
      "' must use the same prediction grid for the selected smooth.",
      call. = FALSE)
  }
  list(first = r1, second = r2[matched])
}

calculate_difference <- function(first, second, V, coefs, data, select,
                                 by_var, f1, f2, interval, ci_level,
                                 offset = NULL) {
  X <- first - second
  ## compute difference
  sm_diff <- drop(X %*% coefs)
  if (!is.null(offset)) sm_diff <- sm_diff + offset
  se <- sqrt(rowSums((X %*% V) * X))
  nr <- NROW(X)
  out <- list(
    .smooth = if(length(select) >1L) {
      str_extract(select[[1]], "^(s|[t][ei2])\\([\\w\\.\\_,]*\\)") |>
        rep(nr)
    } else {
      rep(select, nr)
    },
    .by = rep(by_var, nr),
    .level_1 = rep(f1, nr),
    .level_2 = rep(f2, nr),
    .diff = sm_diff, .se = se
  )
  out <- new_tibble(out, nrow = NROW(X), class = "difference_smooth")
  ## Only need rows associated with one of the levels
  out <- bind_cols(out, data)

  if (identical(interval, "simultaneous")) {
    return(list(data = out, X = X, V = V))
  } else {
    crit <- coverage_normal(ci_level)
    lower <- sm_diff - crit * se
    upper <- sm_diff + crit * se
  }
  add_column(out, .lower_ci = lower, .upper_ci = upper, .after = 6L)
}
