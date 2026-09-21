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
#' @details
#' Simultaneous intervals jointly cover the underlying smooth differences at
#' the evaluated covariate combinations for each pair of factor levels, with
#' approximate posterior probability `ci_level` when using the Bayesian
#' covariance. They do not provide joint coverage across all pairs, outside
#' the evaluation set, or between its points. With `data = NULL`, the evaluation
#' set is generated using `n`. Differences and interval limits are on the
#' linear predictor scale, including group means when requested.
#'
#' The intervals use the joint coefficient covariance selected by
#' `unconditional` and `frequentist`, retaining covariance between the two
#' smooths. The simulation covariance must be positive definite. With
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
  seed = NULL
) {
  interval <- match.arg(interval)
  if (identical(interval, "simultaneous") && !is.null(seed) &&
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
  if (is.null(data)) {
    sm_data <- map(sm_ids, smooth_data,
      model = model, n = n, include_all = TRUE
    )
    data <- bind_rows(sm_data)
  } else {
    data <- as_tibble(data)
    for (sm in smooths) data <- prepare_smooth_data(model, sm, data)
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
    pmap(pairs, calc_difference,
      select = select, by_var = by_var,
      smooth_var = smooth_var, data = data, Xp = Xp, V = V,
      coefs = coefs, group_means = group_means,
      interval = interval, ci_level = ci_level, n_sim = n_sim, n_cores = n_cores
    ) |> bind_rows()
  }
  # Scope the seed once: do not restart the same draw sequence for every pair.
  if (identical(interval, "simultaneous") && !is.null(seed)) {
    withr::with_seed(seed, calculate())
  } else {
    calculate()
  }
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
`difference_smooths.list` <- function(model, ...) {
  if (!is_gamm4(model)) {
    stop("'object' is not a `gamm4()` fit. Can't handle general lists.")
  }
  difference_smooths(model$gam, ...)
}

#' @importFrom tibble new_tibble
#' @importFrom dplyr bind_cols
#' @importFrom stringr str_extract
#' @importsFrom vctrs vec_match
`calc_difference` <- function(f1, f2, select, by_var, smooth_var, data, Xp, V,
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

  ## Keep matrix dimensions for comparisons at a single covariate location.
  X <- Xp[r1, , drop = FALSE] - Xp[r2, , drop = FALSE]

  ## zero the cols related to other splines and covariates
  X[, !keep] <- 0

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
  out <- bind_cols(out, data[r1, smooth_var, drop = FALSE])

  if (identical(interval, "simultaneous")) {
    bounds <- simultaneous_intervals(
      estimate = sm_diff, se = se, X = X, V = V,
      level = ci_level, n_sim = n_sim, n_cores = n_cores
    )
    lower <- bounds$lower
    upper <- bounds$upper
  } else {
    crit <- coverage_normal(ci_level)
    lower <- sm_diff - crit * se
    upper <- sm_diff + crit * se
  }
  add_column(out, .lower_ci = lower, .upper_ci = upper, .after = 6L)
}
