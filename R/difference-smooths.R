#' Differences of factor smooth interactions
#'
#' Estimates pairwise differences (comparisons) between factor smooth
#' interactions (smooths with a factor `by` argument) for pairs of groups
#' defined by the factor. The group means can be optionally included in the
#' difference.
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
#'   difference?
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
#' @export
#' @examples
#'
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' df <- data_sim("eg4", seed = 42)
#' m <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")
#'
#' sm_dif <- difference_smooths(m, select = "s(x2)")
#' sm_dif
#'
#' draw(sm_dif)
#'
#' # include the groups means for `fac` in the difference
#' sm_dif2 <- difference_smooths(m, select = "s(x2)", group_means = TRUE)
#' draw(sm_dif2)
#'
#' # simultaneous intervals, separately for each pair of factor levels
#' sm_sim <- difference_smooths(m, select = "s(x2)",
#'   interval = "simultaneous", n_sim = 1000, seed = 42)
#' draw(sm_sim)
#'
#' # compare specific smooths
#' sm_dif3 <- difference_smooths(m,
#'   select = c("s(x2):fac1", "s(x2):fac2"), partial_match = FALSE
#' )
#'
#' # Choose two axes and condition on the third covariate
#' m3 <- gam(y ~ fac + te(x0, x1, x2, by = fac, k = c(3, 3, 3)),
#'   data = df, method = "REML")
#' sm_dif_3d <- difference_smooths(m3, select = "te(x0,x1,x2)",
#'   variables = c("x1", "x2", "x0"), n_2d = 20, n_3d = 3)
#' draw(sm_dif_3d)
#' # Redraw the evaluated points with the axes exchanged
#' draw(sm_dif_3d, variables = c("x2", "x1", "x0"))
#'
#' # Supply particular conditioning values using a prediction grid
#' grid <- data_slice(m3, x1 = evenly(x1, n = 20),
#'   x2 = evenly(x2, n = 20), x0 = c(0.2, 0.8), fac = levels(fac))
#' sm_dif_at <- difference_smooths(m3, select = "te(x0,x1,x2)",
#'   data = grid, variables = c("x1", "x2", "x0"))
#' draw(sm_dif_at)
#' \dontshow{
#' options(op)
#' }
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
  simultaneous_domain = NULL, tube_control = list()
) {
  interval <- match.arg(interval)
  simultaneous_method <- resolve_simultaneous_method(simultaneous_method, interval,
    simultaneous_domain, tube_control)
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

  # smooths in model
  S <- smooths(model) # vector of smooth labels - "s(x)"
  # select smooths
  take <-
    check_user_select_smooths(
      smooths = S, select = select,
      partial_match = partial_match,
      model_name = expr_label(substitute(object))
    )
  sm_ids <- which(take)
  smooths <- get_smooths_by_id(model, sm_ids)
  # take only factor by smooths, as we could have a decomposed model with a
  # main effect smooth and an interaction by smooth
  is_by <- vapply(smooths, is_factor_by_smooth, logical(1L))
  if (any(!is_by)) {
    smooths <- smooths[is_by]
    sm_ids <- sm_ids[is_by]
  }
  model <- plan_smooth_variables(model, sm_ids, variables = variables)
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
  if (length(select) == 1L) {
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
    # finally compute the pairs
    pairs <- as_tibble(as.data.frame(t(combn(lvls, 2)),
      stringsAsFactor = FALSE
    ))
  }
  names(pairs) <- paste0("f", 1:2)

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

  ## Match covariate combinations before differencing the factor levels.
  r1 <- which(data[[by_var]] == f1)
  r2 <- which(data[[by_var]] == f2)
  keys1 <- data[r1, smooth_var, drop = FALSE]
  keys2 <- data[r2, smooth_var, drop = FALSE]
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
  r2 <- r2[matched]

  # Send only matched rows and active coefficient columns for this pair.
  list(first = Xp[r1, keep, drop = FALSE], second = Xp[r2, keep, drop = FALSE],
    V = V[keep, keep, drop = FALSE], coefs = coefs[keep],
    data = smooth_task_data(data[r1, smooth_var, drop = FALSE]),
    select = select, by_var = by_var, f1 = f1, f2 = f2,
    interval = interval, ci_level = ci_level)
}

calculate_difference <- function(first, second, V, coefs, data, select,
                                 by_var, f1, f2, interval, ci_level) {
  X <- first - second
  ## compute difference
  sm_diff <- drop(X %*% coefs)
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
