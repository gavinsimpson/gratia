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
#'   are compared. Numeric `select`
#'   indexes the smooths in the order they are specified in the formula and
#'   stored in `object`. Character `select` matches the labels for smooths
#'   as shown for example in the output from `summary(object)`. Logical
#'   `select` operates as per numeric `select` in the order that smooths are
#'   stored.
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
#' @param ... arguments passed to other methods. Not currently used.
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
  ...
) {
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
  }
  if (length(select) == 1L) {
    by_var <- by_variable(smooths[[1L]])
    smooth_var <- smooth_variable(smooths[[1L]])
    pairs <- as_tibble(as.data.frame(t(combn(levels(data[[by_var]]), 2)),
      stringsAsFactor = FALSE
    ))
  } else {
    # check if the `select` are of the correct form
    rg <- "^(s|[t][ei2])\\([\\w\\.\\_,]*\\)(?=:)([\\w\\.\\_]*)"
    rg_test <- stringr::str_detect(select, rg)
    if (!all(rg_test)) {
      stop(
        "If naming specific factor-by smooths, all smooths in `select` must be",
        " for the same factor-by smooth"
      )
    }
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

    # now reuse the regexp to grab the bit after the :,
    # and then remove the :,
    # and then remove the name of the factor by bit
    lvls <- stringr::str_replace_all(select, rg, "\\2") |>
      stringr::str_replace_all(":", "") |>
      stringr::str_replace_all(by_var, "")
    # finally compute the pairs
    pairs <- as_tibble(as.data.frame(t(combn(lvls, 2)),
      stringsAsFactor = FALSE
    ))
  }
  names(pairs) <- paste0("f", 1:2)

  Xp <- predict(model, newdata = data, type = "lpmatrix")
  V <- get_vcov(model,
    unconditional = unconditional,
    frequentist = frequentist
  )
  coefs <- coef(model)

  out <- pmap(pairs, calc_difference,
    select = select, by_var = by_var,
    smooth_var = smooth_var, data = data, Xp = Xp, V = V,
    coefs = coefs, group_means = group_means
  )
  out <- bind_rows(out)
  crit <- coverage_normal(ci_level)
  out <- add_column(out,
    .lower_ci = out$.diff - (crit * out$.se),
    .upper_ci = out$.diff + (crit * out$.se),
    .after = 6L
  )
  out
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
`calc_difference` <- function(f1, f2, select, by_var, smooth_var, data, Xp, V,
                              coefs, group_means = FALSE) {
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

  ## rows of Xp associated with pair of smooths
  r1 <- data[[by_var]] == f1
  r2 <- data[[by_var]] == f2

  ## difference rows of Xp for pair of smooths
  X <- Xp[r1, ] - Xp[r2, ]

  ## zero the cols related to other splines and covariates
  X[, !keep] <- 0

  ## compute difference
  sm_diff <- drop(X %*% coefs)
  se <- sqrt(rowSums((X %*% V) * X))
  nr <- NROW(X)
  out <- list(
    .smooth = if(length(select) >1L) {
      stringr::str_extract(select[[1]], "^(s|[t][ei2])\\([\\w\\.\\_,]*\\)") |>
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
  out <- bind_cols(out, data[r1, smooth_var])

  out
}
