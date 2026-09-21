#' Generate fitted values from a estimated GAM
#'
#' @param object a fitted model. Currently only models fitted by [mgcv::gam()]
#'   and [mgcv::bam()] are supported.
#' @param data optional data frame of covariate values for which fitted values
#'   are to be returned.
#' @param scale character; what scale should the fitted values be returned on?
#'   `"linear predictor"` is a synonym for `"link"` if you prefer that
#'   terminology.
#' @param ci_level numeric; a value between 0 and 1 indicating the coverage of
#'   the credible interval.
#' @param interval character; `"confidence"` (the default) gives pointwise
#'   intervals. `"simultaneous"` gives simultaneous intervals for predictions
#'   at the supplied covariate combinations.
#' @param n_sim positive integer; number of coefficient draws used for
#'   simultaneous intervals. Ignored for pointwise intervals.
#' @param n_cores positive integer; number of cores used by [mvnfast::rmvn()]
#'   for simultaneous intervals. Parallel execution requires OpenMP support.
#' @param seed integer or `NULL`; optional random seed for simultaneous
#'   intervals. An explicit seed preserves the caller's random number state.
#'   With `NULL`, the current random number state is used and advanced.
#' @param unconditional logical; include smoothing parameter uncertainty in
#'   the Bayesian covariance matrix, if available. Otherwise a warning is
#'   issued and the uncorrected covariance is used.
#' @param ... arguments passed to [mgcv::predict.gam()]. Note that `type`,
#'   `newdata`, and `se.fit` are already used and passed on to
#'   [mgcv::predict.gam()].
#'
#' @details
#' With `data = NULL`, results follow the model's `na.action`: `na.exclude`
#' restores excluded observations as `NA` fitted values, standard errors and
#' interval bounds; `na.omit` returns only retained observations. Covariates
#' unavailable in the stored model frame are restored as typed `NA` values.
#' `.row` indexes the fitting data after any `subset` was applied.
#'
#' Supplying `data` requests predictions at those rows, independently of training
#' exclusions. Missing responses do not prevent prediction. Missing required
#' predictors produce `NA` results by default. An explicit `na.action` passed
#' through `...` is honoured; with `na.omit`, `.row` retains the positions in the
#' supplied data, rather than numbering the remaining rows consecutively.
#'
#' Simultaneous intervals jointly cover the model's underlying expected
#' responses, or linear predictors on the link scale, at all valid covariate
#' combinations evaluated in this call, with approximate posterior probability
#' `ci_level`. The fitted values are estimates of these quantities. The
#' combinations may include factor levels and need not form an ordered sequence,
#' curve, or grid. This interpretation also applies to two predictions.
#' With `data = NULL`, the simultaneous set comprises the retained fitting
#' covariate combinations. Missing predictions do not enter this set. Coverage
#' is not asserted outside this set or between its points. Separate calls
#' define separate simultaneous sets. These are uncertainty intervals for model
#' predictions, not prediction intervals for future observations.
#'
#' Simultaneous intervals use joint Gaussian coefficient draws and a critical
#' value based on the maximum absolute standardized deviation over the supplied
#' rows. Their limits are computed on the link scale and transformed if
#' `scale = "response"`; `.se` remains on the link scale. The simulation
#' covariance must be positive definite. Supported families are Gaussian,
#' Poisson, binomial, Gamma, inverse Gaussian, quasi families, negative binomial,
#' Tweedie, beta regression, and scaled t, fitted by `gam()` or `bam()` (including
#' the `gam` component of a `gamm()` fit). Other families and `scam` models
#' currently support pointwise intervals only.
#'
#' Response-scale simultaneous intervals support the identity, log, logit,
#' probit, cloglog, cauchit, square-root, inverse, and inverse-squared links.
#' Intervals crossing an inverse-link domain boundary are rejected; use
#' `scale = "link"` for these intervals or for other links.
#'
#' `terms` and `exclude` in `...` retain [mgcv::predict.gam()] semantics and
#' apply to both prediction and interval calculation. If terms are selected or
#' excluded, intervals concern that selected quantity, not necessarily the full
#' expected response. Use the link scale for sums of smooth contributions, and
#' consider the intercept and offset treatment when selecting terms.
#'
#' @note For most families, regardless of the scale on which the fitted values
#'   are returned, the `se` component of the returned object is on the *link*
#'   (*linear predictor*) scale, not the response scale. An exception is the
#'   `mgcv::ocat()` family, for which the `se` is on the response scale if
#'   `scale = "response"`.
#'
#' @return A tibble (data frame) whose first *m* columns contain either the data
#'   used to fit the model (if `data` was `NULL`), or the variables supplied to
#'  `data`. Four further columns are added:
#'
#' * `.fitted`: the fitted values on the specified scale,
#' * `.se`: the standard error of the fitted values (always on the *link* scale),
#' * `.lower_ci`, `.upper_ci`: the limits of the credible interval on the fitted values,
#'     on the specified scale.
#'
#' Models fitted with certain families will include additional variables
#'
#' * `mgcv::ocat()` models: when `scale = "response"`, the returned object will
#'   contain a `row` column and a `category` column, which indicate to which row
#'   of the `data` each row of the returned object belongs. Additionally, there
#'   will be `nrow(data) * n_categories` rows in the returned object; each row
#'   is the predicted probability for a single category of the response.
#'
#' @inheritParams smooth_estimates
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 6)
#' }
#' sim_df <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = sim_df, method = "REML")
#' fv <- fitted_values(m)
#' fv
#'
#' # Simultaneous intervals for two arbitrary covariate combinations
#' fitted_values(m, data = sim_df[c(1, 20), ], interval = "simultaneous",
#'   n_sim = 1000, seed = 42)
#'
#' # A selected sum of smooth contributions on the link scale
#' fitted_values(m, data = sim_df[c(1, 20), ], scale = "link",
#'   terms = c("s(x2)", "s(x3)"), interval = "simultaneous",
#'   n_sim = 1000, seed = 42)
#' \dontshow{
#' options(op)
#' }
`fitted_values` <- function(object, ...) {
  UseMethod("fitted_values")
}

#' @export
#' @rdname fitted_values
`fitted_values.gam` <- function(object,
                                data = NULL,
                                scale = c(
                                  "response",
                                  "link",
                                  "linear predictor"
                                ),
                                ci_level = 0.95, envir = NULL, ...,
                                interval = c("confidence", "simultaneous"),
                                n_sim = 10000, n_cores = 1, seed = NULL,
                                unconditional = FALSE) {
  interval <- match.arg(interval)
  object <- with_model_envir(object, envir)
  # Handle everything up to and including the extended families, but not more
  fn <- family_type(object)
  if (inherits(family(object), "general.family")) {
    allowed <- c(
      "gaulss", "gammals", "gumbls", "gevlss", "shash", "ziplss",
      "twlss", "multivariate_normal", "multinom"
    )
    if (!fn %in% allowed) {
      stop("General likelihood GAMs not yet supported.")
    }
  }
  scale <- match.arg(scale)

  layout <- prediction_layout(object, data, na.action = list(...)$na.action %||% stats::na.pass)
  object <- model_used_rows(object)
  data <- as_tibble(layout$data)

  # handle special distributions that return more than vector fit & std. err.
  # find the name of the function that produces fitted values for this family
  fit_vals_fun <- get_fit_fun(fn)
  extra_fns <- switch(fn,
    "gumbls" = post_link_funs(location = exp, scale = exp),
    "gammals" = post_link_funs(location = exp, scale = exp),
    "gevlss" = post_link_funs(scale = exp),
    "shash" = post_link_funs(scale = exp, kurtosis = exp),
    "ziplss" = post_link_funs(
      location = exp,
      pi = inv_link(binomial("cloglog"))
    ),
    "twlss" = post_link_funs(power = twlss_theta_2_power, scale = exp),
    post_link_funs()
  )
  # compute fitted values
  if (identical(interval, "simultaneous")) {
    fit <- fit_vals_simultaneous(object,
      data = data, ci_level = ci_level, scale = scale,
      n_sim = n_sim, n_cores = n_cores, seed = seed,
      unconditional = unconditional, empty = !any(!is.na(layout$map)), ...
    )
  } else {
    fit <- fit_vals_fun(object,
      data = data, ci_level = ci_level,
      scale = scale, extra_fns = extra_fns,
      unconditional = unconditional, ...
    )
  }
  if (identical(scale, "response")) {
    fit <- order_interval_bounds(fit)
  }
  fit <- restore_prediction_table(fit, layout, include_data = TRUE)
  if (layout$training) {
    f <- formula(object)
    if (!is.list(f)) f <- list(f)
    response <- vapply(f, function(x) if (length(x) == 3L) {
      if (is.symbol(x[[2L]])) as.character(x[[2L]]) else paste(deparse(x[[2L]]), collapse = "")
    } else "", character(1L))
    covariates <- as.character(attr(attr(object$model, "terms"), "variables"))[-1L]
    remove <- c(response, setdiff(names(layout$data), covariates), "(weights)", "(offset)")
    fit <- fit[, !names(fit) %in% remove, drop = FALSE]
  }
  attr(fit, "terms") <- NULL
  fit
}

#' @export
#' @rdname fitted_values
`fitted_values.gamm` <- function(object, ...) {
  fitted_values(object$gam, ...)
}

#' @export
#' @rdname fitted_values
`fitted_values.scam` <- function(object, ...) {
  fitted_values.gam(object, ...)
}

# Single-predictor adapter: the centre includes offsets, but coefficient
# deviations do not. Family-specific nonlinear predictions need other adapters.
fit_vals_simultaneous <- function(
  object, data, ci_level, scale, n_sim, n_cores, seed, unconditional,
  empty = FALSE, ...
) {
  fam <- family(object)
  supported <- c(
    "gaussian", "poisson", "binomial", "gamma", "inverse_gaussian",
    "quasi", "quasipoisson", "quasibinomial", "negative_binomial",
    "tweedie", "beta_regression", "scaled_t"
  )
  if (inherits(object, "scam") || inherits(fam, "general.family") ||
      !family_type(object) %in% supported) {
    stop("Simultaneous intervals are not supported for this model family or class.",
      call. = FALSE)
  }
  if (!is.logical(unconditional) || length(unconditional) != 1L ||
      is.na(unconditional)) {
    stop("`unconditional` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L ||
      !is.finite(seed) || abs(seed) > .Machine$integer.max || seed != trunc(seed))) {
    stop("`seed` must be NULL or a single integer.", call. = FALSE)
  }
  response <- identical(scale, "response")
  if (response && !fam$link %in% c(
    "identity", "log", "logit", "probit", "cloglog", "cauchit",
    "sqrt", "inverse", "1/mu^2"
  )) {
    stop("This inverse link is not supported for simultaneous intervals; use `scale = 'link'`.",
      call. = FALSE)
  }
  # Resolve the corrected-covariance fallback once, including its warning.
  V <- get_vcov(object, unconditional = unconditional)
  corrected <- unconditional && !is.null(object$Vc)
  if (empty) {
    estimate <- se <- rep(NA_real_, nrow(data))
    X <- matrix(NA_real_, nrow(data), ncol(V))
  } else {
    prediction <- predict_model(object, newdata = data, ...,
      type = "link", se.fit = TRUE, unconditional = corrected)
    estimate <- as.vector(prediction$fit)
    se <- as.vector(prediction$se.fit)
    X <- predict_model(object, newdata = data, ..., type = "lpmatrix")
  }
  calculate <- function() simultaneous_intervals(
    estimate, se, X, V, level = ci_level, n_sim = n_sim, n_cores = n_cores
  )
  intervals <- if (is.null(seed)) calculate() else withr::with_seed(seed, calculate())
  fit <- tibble(
    .row = seq_len(nrow(data)), .fitted = estimate, .se = se,
    .lower_ci = intervals$lower, .upper_ci = intervals$upper
  )
  if (response) {
    lower <- intervals$lower
    upper <- intervals$upper
    invalid <- switch(fam$link,
      "inverse" = lower <= 0 & upper >= 0,
      "1/mu^2" = lower <= 0,
      "sqrt" = lower < 0,
      FALSE
    )
    if (any(invalid, na.rm = TRUE)) {
      stop("Simultaneous intervals cross the inverse-link domain boundary; use `scale = 'link'`.",
        call. = FALSE)
    }
    fit <- mutate(fit, across(all_of(c(".fitted", ".lower_ci", ".upper_ci")),
      .fns = inv_link(object)))
  }
  # prediction_layout() supplies compact data without an existing .row column.
  bind_cols(data, fit) |> relocate(".row", .before = 1L)
}

#' @importFrom rlang set_names .data
#' @importFrom dplyr bind_cols mutate across
#' @importFrom tibble as_tibble is_tibble
#' @importFrom tidyselect any_of
`fit_vals_default` <- function(
    object, data, ci_level = 0.95,
    scale = "response", ...) {
  fit <- predict_model(object,
    newdata = data,
    ...,
    type = "link",
    se.fit = TRUE
  ) |>
    as.data.frame() |>
    rlang::set_names(c(".fitted", ".se")) |>
    as_tibble()
  # add .row *unless* it already exists
  if (!".row" %in% names(data)) {
    fit <- mutate(fit, .row = row_number())
  }
  fit <- bind_cols(data, fit) |>
    relocate(".row", .before = 1L)

  # create the confidence interval
  crit <- coverage_normal(ci_level)
  fit <- mutate(fit,
    ".lower_ci" = .data[[".fitted"]] - (crit * .data[[".se"]]),
    ".upper_ci" = .data[[".fitted"]] + (crit * .data[[".se"]])
  )

  # convert to the response scale if requested
  if (identical(scale, "response")) {
    fit <- fit |>
      mutate(across(all_of(c(".fitted", ".lower_ci", ".upper_ci")),
        .fns = inv_link(object)
      ))
  }

  fit
}

#' @importFrom dplyr mutate across recode_values
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble add_column
`fit_vals_general_lss` <- function(
    object, data, ci_level = 0.95,
    scale = "response", extra_fns = post_link_funs(), ...) {
  crit <- coverage_normal(ci_level)
  # get the fitted values for data
  fv <- predict_model(object,
    newdata = data, ..., type = "link",
    se.fit = TRUE
  )
  std_err <- fv[[2L]]
  fv <- fv[[1]]
  colnames(std_err) <- colnames(fv) <- lss_parameters(object)
  # convert fv to tibble then long format
  fv <- fv |>
    as_tibble() |>
    mutate(.row = row_number()) |>
    relocate(".row", .before = 1L) |>
    tidyr::pivot_longer(!matches("\\.row"),
      values_to = ".fitted",
      names_to = ".parameter"
    )
  # convert fv to tibble then long format
  std_err <- std_err |>
    as_tibble() |>
    mutate(.row = row_number()) |>
    relocate(".row", .before = 1L) |>
    tidyr::pivot_longer(!matches("\\.row"),
      values_to = ".std_err",
      names_to = ".parameter"
    )
  # bind .std_err to fv...
  fit <- fv |>
    add_column(.se = pull(std_err, ".std_err")) |>
    # ...and compute interval
    mutate(
      .lower_ci = .data$.fitted - (crit * .data$.se),
      .upper_ci = .data$.fitted + (crit * .data$.se)
    )

  # convert to the response scale if requested
  if (identical(scale, "response")) {
    il <- lss_links(object, inverse = TRUE)

    fit <- fit |>
      mutate(across(all_of(c(".fitted", ".lower_ci", ".upper_ci")),
        .fns = ~ recode_values(
          .data$.parameter,
          "location" ~ extra_fns[["location"]](il[["location"]](.x)),
          "scale" ~ extra_fns[["scale"]](il[["scale"]](.x)),
          "shape" ~ extra_fns[["shape"]](il[["shape"]](.x)),
          "skewness" ~ extra_fns[["skewness"]](il[["skewness"]](.x)),
          "kurtosis" ~ extra_fns[["kurtosis"]](il[["kurtosis"]](.x)),
          "power" ~ extra_fns[["power"]](il[["power"]](.x)),
          "pi" ~ extra_fns[["pi"]](il[["pi"]](.x))
        )
      ))
  }

  fit
}

#' @importFrom dplyr mutate across recode_values row_number
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble add_column
`fit_vals_ziplss` <- function(
    object, data, ci_level = 0.95,
    scale = "response", extra_fns = post_link_funs(), ...) {
  crit <- coverage_normal(ci_level)
  # get the fitted values for data
  fv <- predict_model(object,
    newdata = data, ..., type = "link",
    se.fit = TRUE
  )
  std_err <- fv[[2L]]
  fv <- fv[[1]]
  colnames(std_err) <- colnames(fv) <- lss_parameters(object)
  # convert fv to tibble then long format
  fv <- fv |>
    as_tibble() |>
    mutate(.row = row_number()) |>
    relocate(".row", .before = 1L) |>
    tidyr::pivot_longer(!matches("\\.row"),
      values_to = ".fitted",
      names_to = ".parameter"
    )
  # convert fv to tibble then long format
  std_err <- std_err |>
    as_tibble() |>
    tidyr::pivot_longer(everything(),
      values_to = ".std_err",
      names_to = ".parameter"
    )
  # bind .std_err to fv...
  fit <- fv |>
    add_column(.se = pull(std_err, ".std_err")) |>
    # ...and compute interval
    mutate(
      .lower_ci = .data$.fitted - (crit * .data$.se),
      .upper_ci = .data$.fitted + (crit * .data$.se)
    )

  # convert to the response scale if requested
  if (identical(scale, "response")) {
    ilink_loc <- inv_link(object, parameter = "location")
    ilink_pi <- inv_link(object, parameter = "pi")

    fit <- fit |>
      mutate(across(all_of(c(".fitted", ".lower_ci", ".upper_ci")),
        .fns = ~ recode_values(
          .data$.parameter,
          "location" ~ extra_fns[["location"]](ilink_loc(.x)),
          "pi" ~ extra_fns[["pi"]](ilink_pi(.x))
        )
      ))
  }

  fit
}

#' @importFrom dplyr mutate across recode_values row_number
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble add_column
`fit_vals_twlss` <- function(
    object, data, ci_level = 0.95,
    scale = "response", extra_fns = post_link_funs(), ...) {
  crit <- coverage_normal(ci_level)
  # get the fitted values for data
  fv <- predict_model(object,
    newdata = data, ..., type = "link",
    se.fit = TRUE
  )
  std_err <- fv[[2L]]
  fv <- fv[[1]]
  colnames(std_err) <- colnames(fv) <- lss_parameters(object)
  # convert fv to tibble then long format
  fv <- fv |>
    as_tibble() |>
    mutate(.row = row_number()) |>
    relocate(".row", .before = 1L) |>
    tidyr::pivot_longer(!matches("\\.row"),
      values_to = ".fitted",
      names_to = ".parameter"
    )
  # convert fv to tibble then long format
  std_err <- std_err |>
    as_tibble() |>
    tidyr::pivot_longer(everything(),
      values_to = ".std_err",
      names_to = ".parameter"
    )
  # bind .std_err to fv...
  fit <- fv |>
    add_column(.se = pull(std_err, ".std_err")) |>
    # ...and compute interval
    mutate(
      .lower_ci = .data$.fitted - (crit * .data$.se),
      .upper_ci = .data$.fitted + (crit * .data$.se)
    )

  # convert to the response scale if requested
  if (identical(scale, "response")) {
    il <- lss_links(object, inverse = TRUE)
    bounds <- get_twlss_bounds(object)

    fit <- fit |>
      mutate(across(all_of(c(".fitted", ".lower_ci", ".upper_ci")),
        .fns = ~ recode_values(
          .data$.parameter,
          "location" ~ extra_fns[["location"]](il[["location"]](.x)),
          "power" ~ extra_fns[["power"]](il[["power"]](.x),
            a = bounds[1], b = bounds[2]),
          "scale" ~ extra_fns[["scale"]](il[["scale"]](.x))
        )
      ))
  }

  fit
}

#' @importFrom dplyr bind_rows relocate
#' @importFrom tidyr expand_grid
#' @importFrom tibble tibble
`fit_vals_ocat` <- function(
    object, data, ci_level = 0.95, scale = "response",
    ...) {
  # if link (linear predictor) scale, we can just use `fit_vals_fun()`
  if (scale %in% c("link", "linear predictor")) {
    fit <- fit_vals_default(object,
      data = data, ci_level = ci_level,
      scale = "link", ...
    )
  } else {
    # predict, needs to be response scale for ocat!
    fv <- predict_model(object,
      newdata = data, ..., type = "response",
      se.fit = TRUE
    )
    crit <- coverage_normal(ci_level)

    # extract information on how many thresholds, categories in the model
    theta <- theta(object) # the estimated thresholds, first is always -1
    n_cut <- length(theta) # how many thresholds...
    n_cat <- n_cut + 1 # ...which implies this many categories
    n_data <- NROW(data) # how many data are we predicting for

    # \hat{pi} is the estimated probability of each class for each data
    # \hat{pi} is given by fv$fit
    # std. err. of \hat{pi} is given by fv$se.fit

    # compute standard error of logit(\hat{pi}) via delta method
    # this comes from Christensen RHB (2022), a vignette of ordinal
    # package:
    # https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf
    #
    # se(logit(pi)) = se(pi) / (pi * (1 - pi))
    se_lp <- fv$se.fit / (fv$fit * (1 - fv$fit))

    # grab slightly better versions of plogis() & qlogis() from the binomial
    # family
    bin_fam <- binomial()
    lfun <- link(bin_fam)
    ifun <- inv_link(bin_fam)

    # convert \hat{pi} to logit scale and form a Wald interval, then back
    # transform the interval only (we already have \hat{pi})
    fit_lp <- lfun(fv$fit) # logit(\hat{pi})
    fit_lwr <- ifun(fit_lp - (crit * se_lp))
    fit_upr <- ifun(fit_lp + (crit * se_lp))

    # create the return object
    fit <- tibble(
      .row = rep(seq_len(n_data), times = n_cat),
      .category = factor(rep(seq_len(n_cat), each = n_data)),
      .fitted = as.numeric(fv$fit),
      .se = as.numeric(fv$se.fit),
      .lower_ci = as.numeric(fit_lwr),
      .upper_ci = as.numeric(fit_upr)
    )

    # expand data so it is replicated once per category & add to the fitted
    # values
    fit <- expand_grid(category = seq_len(n_cat), data) |>
      select(-c("category")) |>
      bind_cols(fit) |>
      relocate(".row", .before = 1)
  }
  fit
}

`fit_vals_mvn` <- function(
  object, data, ci_level = 0.95, scale = "response", ...
) {
  # scale is ignored as these are correlated Gaussians
  # but we have multiple columns in the predictions, 1 per Y
  # predict, can predict on link scale as link is identity
  fv <- predict(
    object, newdata = data, ..., type = "link", se.fit = TRUE
  )
  crit <- coverage_normal(ci_level)
  n_data <- NROW(data)
  n_y <- n_eta(object)

  # fv$fit and fv$se.fit are matrices - need to stack everything
  fit_lp  <- as.vector(fv$fit)
  fit_se  <- as.vector(fv$se)
  fit_lwr <- fit_lp - (crit * fit_se)
  fit_upr <- fit_lp + (crit * fit_se)

  # pull everything into a tibble for return
  fit <- tibble(
    .row = rep(seq_len(n_data), times = n_y),
    .y = paste0("response", rep(seq_len(n_y), each = n_data)),
    .fitted = as.numeric(fit_lp),
    .se = as.numeric(fit_se),
    .lower_ci = as.numeric(fit_lwr),
    .upper_ci = as.numeric(fit_upr)
  )

  # expand data so it is replicated once per response & add to fitted values
  fit <- expand_grid(..response = seq_len(n_y), data) |>
    select(-c("..response")) |>
    bind_cols(fit) |>
    relocate(".row", .before = 1)

  # return
  fit
}

`fit_vals_multinom` <- function(
  object, data, ci_level = 0.95, scale = "response", ...
) {
  if (scale %in% c("link", "linear predictor")) {
    stop("link scale predictions not support for `multinom()`")
  }
  # there are only n_lp columns in these; Y_categories - 1
  # predict on response scale
  fv <- predict(
    object, newdata = data, ..., type = "response", se.fit = TRUE
  )
  crit <- coverage_normal(ci_level)
  n_data <- NROW(data)
  n_lp <- n_eta(object) # number of linear predictors
  n_y <- n_lp + 1

  # as with ocat() we'll transform from response to a logit scale
  # form se on this link scale, then back transform
  se_lp <- fv$se.fit / (fv$fit * (1 - fv$fit))

  # grab slightly better versions of plogis() & qlogis() from the binomial
  # family
  bin_fam <- binomial()
  lfun <- link(bin_fam)
  ifun <- inv_link(bin_fam)

  # convert \hat{pi} to logit scale and form a Wald interval, then back
  # transform the interval only (we already have \hat{pi})
  fit_lp <- lfun(fv$fit) # logit(\hat{pi})
  fit_lwr <- ifun(fit_lp - (crit * se_lp))
  fit_upr <- ifun(fit_lp + (crit * se_lp))

  # create the return object
  fit <- tibble(
    .row = rep(seq_len(n_data), times = n_y),
    .category = factor(
      rep(seq_len(n_y), each = n_data)
    ),
    .fitted = as.vector(fv$fit),
    .se = as.vector(fv$se.fit),
    .lower_ci = as.vector(fit_lwr),
    .upper_ci = as.vector(fit_upr)
  )

  # expand data so it is replicated once per response & add to fitted values
  fit <- expand_grid(..response = seq_len(n_y), data) |>
    select(-c("..response")) |>
    bind_cols(fit) |>
    relocate(".row", .before = 1)

  # return
  fit
}

`fit_vals_zip` <- function(
  object, data, ci_level = 0.95, scale = "response", ...
) {
  fv <- predict(
    object, newdata = data, ..., type = "link", se.fit = TRUE
  )
  crit <- coverage_normal(ci_level)
  fam <- family(object)

  # use fam$predict() to do the conversion to response scale
  # fit_lp  <- as.vector(fv$fit)
  fit_se  <- as.vector(fv$se)
  fit_lwr <- as.vector(as.vector(fv$fit) - (crit * fit_se))
  fit_upr <- as.vector(as.vector(fv$fit) + (crit * fit_se))

  # pull everything into a tibble for return
  fit <- tibble(
    .row = seq_len(NROW(data)),
    .fitted = as.vector(fv$fit),
    .se = as.numeric(fit_se),
    .lower_ci = as.numeric(fit_lwr),
    .upper_ci = as.numeric(fit_upr)
  )

  # convert to the response scale if requested
  if (identical(scale, "response")) {
    fit <- fit |>
      mutate(across(all_of(c(".fitted", ".lower_ci", ".upper_ci")),
        .fns = \(eta) fam$predict(fam, eta = eta)[[1]] |> as.vector()
      ))
  }

  # return
  fit
}

# Dave has added a clognorm family to mgcvUtils with a bespoke fitted_values
# clone. Only difference with fit_vals_default is a pre-processing of fit
# by `exp()`. Might be worth adding a pre_processing step if there are more
# families like this?

#' @importFrom rlang set_names .data
#' @importFrom dplyr bind_cols mutate across
#' @importFrom tibble as_tibble is_tibble
#' @importFrom tidyselect any_of
`fit_vals_clognorm` <- function(
  object,
  data,
  ci_level = 0.95,
  scale = "response",
  ...
) {
  # get the transformation used
  base <- attr(family(object), "base")
  if(base == "e") { 
    base <- exp(1)
    trans <- exp
  } else {
    trans <- function(x) base^x
  }

  fit <- predict_model(object,
    newdata = data,
    ...,
    type = "response",
    se.fit = TRUE
  ) |>
    as.data.frame() |>
    rlang::set_names(c(".fitted", ".se")) |>
    as_tibble()|>
    mutate(
      .fitted = logb(.data[[".fitted"]], base = base)
    )
  # add .row *unless* it already exists
  if (!".row" %in% names(data)) {
    fit <- mutate(fit, .row = row_number())
  }
  fit <- bind_cols(data, fit) |>
    relocate(".row", .before = 1L)

  # create the confidence interval
  crit <- coverage_normal(ci_level)
  fit <- mutate(fit,
    ".lower_ci" = .data[[".fitted"]] - (crit * .data[[".se"]]),
    ".upper_ci" = .data[[".fitted"]] + (crit * .data[[".se"]])
  )

  # convert to the response scale if requested
  if (identical(scale, "response")) {
    fit <- fit |>
      mutate(across(all_of(c(".fitted", ".lower_ci", ".upper_ci")),
        .fns = trans
      ))
  }

  fit
}

#' @importFrom dplyr case_when
`get_fit_fun` <- function(fam) {
  fam <- case_when(
    grepl("^ordered_categorical", fam, ignore.case = TRUE) == TRUE ~ "ocat",
    grepl("^multivariate_normal", fam, ignore.case = TRUE) == TRUE ~ "mvn",
    grepl("^multinom", fam, ignore.case = TRUE) == TRUE ~ "multinom",
    grepl("^zero_inflated_poisson\\(", fam, ignore.case = TRUE) == TRUE ~ "zip",
    grepl("^clog(e|[[:digit:]]+)norm", fam, ignore.case = TRUE) == TRUE ~ "clognorm",
    fam == "gaulss" ~ "general_lss",
    fam == "gammals" ~ "general_lss",
    fam == "gumbls" ~ "general_lss",
    fam == "gevlss" ~ "general_lss",
    fam == "shash" ~ "general_lss",
    fam == "ziplss" ~ "ziplss",
    fam == "twlss" ~ "twlss",
    .default = "default"
  )
  get(paste0("fit_vals_", fam), mode = "function")
}

## my original code trying to follow Simon's ocat

# lp <- as.numeric(fv$fit)
# se <- as.numeric(fv$se.fit)
# upr <- lp + (crit * se)
# lwr <- lp - (crit * se)

# theta <- theta(object)
# n_cut <- length(theta)
# n_cat <- n_cut + 1
# n_data <- NROW(data)
# p_fit <- p_lwr <- p_upr <- matrix(0, nrow = n_data,
#     ncol = n_cut + 2)
# # cumulative probability should sum to 1 over the latent
# # fill final column with 1 to reflect that
# p_fit[, n_cut + 2] <- p_lwr[, n_cut + 2] <- p_upr[, n_cut + 2] <- 1

# # function to give probability from latent
# `ocat_prob` <- function(lp, theta) {
#     p <- theta - lp
#     i <- p > 0
#     p[i] <- 1 / (1 + exp(-p[i]))
#     p[!i] <- exp(p[!i]) / (1 + exp(p[!i]))
#     p
# }

# # fill in the matrix of cumulative probability
# for (j in seq_along(theta)) {
#     p_fit[, j + 1] <- ocat_prob(lp, theta[j])
#     p_lwr[, j + 1] <- ocat_prob(lwr, theta[j])
#     p_upr[, j + 1] <- ocat_prob(upr, theta[j])
# }
# #browser()
# p_fit <- as.numeric(t(diff(t(p_fit))))
# p_lwr <- as.numeric(t(diff(t(p_lwr))))
# p_upr <- as.numeric(t(diff(t(p_upr))))

# fit <- tibble(row = rep(seq_len(n_data), times = n_cat),
#     category = factor(rep(seq_len(n_cat), each = n_data)),
#     fitted = p_fit,
#     lower = p_lwr,
#     upper = p_upr)
# # expand data so it is replicated once per category
# fit <- expand_grid(category = seq_len(n_cat), data) |>
#     select(-c("category")) |>
#     bind_cols(fit) |>
#     relocate(row, .before = 1)
# fit

# ## trying to do somethign for multinom
# fv <- predict(
#   m_multinom, newdata = head(multinom_df), se.fit = TRUE,
#   type = "response"
# )

# se_lp <- fv$se.fit / (fv$fit * (1 - fv$fit))

# # grab slightly better versions of plogis() & qlogis() from the binomial
# # family
# bin_fam <- binomial()
# lfun <- link(bin_fam)
# ifun <- inv_link(bin_fam)

# # convert \hat{pi} to logit scale and form a Wald interval, then back
# # transform the interval only (we already have \hat{pi})
# fit_lp <- lfun(fv$fit) # logit(\hat{pi})
# fit_lwr <- ifun(fit_lp - (crit * se_lp))
# fit_upr <- ifun(fit_lp + (crit * se_lp))

# # create the return object
# fit <- tibble(
#   .row = rep(seq_len(nrow(head(multinom_df))), times = n_eta(m_multinom) + 1),
#   .category = factor(
#     rep(seq_len(n_eta(m_multinom) + 1), each = nrow(head(multinom_df)))
#   ),
#   .fitted = as.vector(fv$fit),
#   .se = as.vector(fv$se.fit),
#   .lower_ci = as.vector(fit_lwr),
#   .upper_ci = as.vector(fit_upr)
  # )
