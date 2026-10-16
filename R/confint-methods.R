#' Point-wise and simultaneous confidence intervals for derivatives of smooths
#'
#' Calculates point-wise confidence or simultaneous intervals for the first
#'   derivatives of smooth terms in a fitted GAM.
#'
#' @param object an object of class `"fderiv"` containing the estimated
#'   derivatives.
#' @param parm which parameters (smooth terms) are to be given intervals as a
#'   vector of terms. If missing, all parameters are considered.
#' @param level numeric, `0 < level < 1`; the confidence level of the
#'   point-wise or simultaneous interval. The default is `0.95` for a 95%
#'   interval.
#' @param type character; the type of interval to compute. One of `"confidence"`
#'   for point-wise intervals, or `"simultaneous"` for simultaneous intervals.
#' @param nsim integer; the number of simulations used in computing the
#'   simultaneous intervals.
#' @param ncores number of cores for generating random variables from a
#'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
#'   Parallelization will take place only if OpenMP is supported (but appears
#'   to work on Windows with current `R`).
#' @param ... additional arguments for methods
#'
#' @return a data frame with components:
#' 1. `term`; factor indicating to which term each row relates,
#' 2. `lower`; lower limit of the confidence or simultaneous interval,
#' 3. `est`; estimated derivative
#' 4. `upper`; upper limit of the confidence or simultaneous interval.
#'
#' @author Gavin L. Simpson
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(
#'   pillar.sigfig = 2, cli.unicode = FALSE,
#'   lifecycle_verbosity = "quiet"
#' )
#' }
#' dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' # new data to evaluate the derivatives at, say over the middle 50% of range
#' # of each covariate
#' middle <- function(x, n = 25, coverage = 0.5) {
#'   v <- (1 - coverage) / 2
#'   q <- quantile(x, prob = c(0 + v, 1 - v), type = 8)
#'   seq(q[1], q[2], length = n)
#' }
#' new_data <- sapply(dat[c("x0", "x1", "x2", "x3")], middle)
#' new_data <- data.frame(new_data)
#' ## first derivatives of all smooths...
#' fd <- fderiv(mod, newdata = new_data)
#'
#' ## point-wise interval
#' ci <- confint(fd, type = "confidence")
#' ci
#'
#' ## simultaneous interval for smooth term of x2
#' \dontshow{
#' set.seed(42)
#' }
#' x2_sint <- confint(fd,
#'   parm = "x2", type = "simultaneous",
#'   nsim = 10000, ncores = 2
#' )
#' \donttest{
#' x2_sint
#' }
#' \dontshow{
#' options(op)
#' }
`confint.fderiv` <- function(object, parm, level = 0.95,
                             type = c("confidence", "simultaneous"),
                             nsim = 10000,
                             ncores = 1L, ...) {
  ## Process arguments
  ## parm is one of the terms in object
  parm <- if (missing(parm)) {
    object$terms
  } else {
    parm <- add_s(parm)
    terms <- object$terms
    want <- parm %in% terms
    if (any(!want)) {
      msg <- paste(
        "Terms:", paste(parm[!want], collapse = ", "),
        "not found in `object`"
      )
      stop(msg)
    }
    parm[want]
  }
  ## parm <- add_s(parm)

  ## level should be length 1, numeric and 0 < level < 1
  if ((ll <- length(level)) > 1L) {
    warning(paste(
      "`level` should be length 1, but supplied length: ",
      ll, ". Using the first only."
    ))
    level <- rep(level, length.out = 1L)
  }
  if (!is.numeric(level)) {
    stop(paste("`level` should be numeric, but supplied:", level))
  }
  if (!(0 < level) && (level < 1)) {
    stop(paste(
      "`level` should lie in interval [0,1], but supplied:",
      level
    ))
  }

  ## which type of interval is required
  type <- match.arg(type)

  ## generate intervals
  interval <- if (type == "confidence") {
    confidence(object, terms = parm, level = level)
  } else {
    simultaneous(object,
      terms = parm, level = level, nsim = nsim,
      ncores = ncores
    )
  }

  interval <- as_tibble(interval)

  class(interval) <- c("confint.fderiv", class(interval))

  ## return
  interval
}

#' @importFrom stats quantile vcov
#' @importFrom mvnfast rmvn
`simultaneous` <- function(x, terms, level, nsim, ncores) {
  ## wrapper the computes each interval
  `simInt` <- function(x, Vb, bu, level, nsim) {
    Xi <- x[["Xi"]] # derivative Lp, zeroed except for this term
    se <- x[["se.deriv"]] # std err of deriv for current term
    d <- x[["deriv"]] # deriv for current term
    simDev <- Xi %*% t(bu) # simulate deviations from expected
    absDev <- abs(sweep(simDev, 1, se, FUN = "/")) # absolute deviations
    masd <- apply(absDev, 2L, max) # & maxabs deviation per sim
    ## simultaneous interval critical value
    crit <- quantile(masd, prob = level, type = 8)
    ## return as data frame
    data.frame(lower = d - (crit * se), est = d, upper = d + (crit * se))
  }

  ## bayesian covar matrix, possibly accounting for estimating smooth pars
  Vb <- vcov(x[["model"]], unconditional = x$unconditional)
  ## simulate un-biased deviations given bayesian covar matrix
  buDiff <- mvnfast::rmvn(
    n = nsim, mu = rep(0, nrow(Vb)), sigma = Vb,
    ncores = ncores
  )
  ## apply wrapper to compute simultaneous interval critical value and
  ## corresponding simultaneous interval for each term
  res <- lapply(x[["derivatives"]][terms],
    FUN = simInt,
    Vb = Vb, bu = buDiff, level = level, nsim = nsim
  )
  ## how many values per term - currently all equal
  lens <- vapply(res, FUN = NROW, FUN.VALUE = integer(1))
  res <- do.call("rbind", res) # row-bind each component of res
  res <- cbind(term = rep(terms, times = lens), res) # add on term ID
  rownames(res) <- NULL # tidy up
  res # return
}

#' @importFrom stats qnorm
`confidence` <- function(x, terms, level) {
  ## wrapper the computes each interval
  `confInt` <- function(x, level) {
    se <- x[["se.deriv"]] # std err of deriv for current term
    d <- x[["deriv"]] # deriv for current term
    ## confidence interval critical value
    crit <- qnorm(1 - ((1 - level) / 2))
    ## return as data frame
    data.frame(lower = d - (crit * se), est = d, upper = d + (crit * se))
  }

  ## apply wrapper to compute confidence interval critical value and
  ## corresponding confidence interval for each term
  res <- lapply(x[["derivatives"]][terms], FUN = confInt, level = level)
  ## how many values per term - currently all equal
  lens <- vapply(res, FUN = NROW, FUN.VALUE = integer(1))
  res <- do.call("rbind", res) # row-bind each component of res
  res <- cbind(term = rep(terms, times = lens), res) # add on term ID
  rownames(res) <- NULL # tidy up
  res # return
}

#' Point-wise and simultaneous confidence intervals for smooths
#'
#' Calculates point-wise confidence or simultaneous intervals for the smooth
#' terms of a fitted GAM.
#'
#' @param object an object of class `"gam"` or `"gamm"`.
#' @param parm which parameters (smooth terms) are to be given intervals as a
#'   vector of terms. If missing, all parameters are considered, although this
#'   is not currently implemented.
#' @param level numeric, `0 < level < 1`; the confidence level of the point-wise
#'   or simultaneous interval. The default is `0.95` for a 95% interval.
#' @param data data frame; new values of the covariates used in the model fit.
#'   The selected smooth(s) will be evaluated at the supplied values.
#' @param n numeric; the number of points to evaluate smooths at.
#' @param type character; the type of interval to compute. One of `"confidence"`
#'   for point-wise intervals, or `"simultaneous"` for simultaneous intervals.
#'   `"credible"` aliases `"confidence"`, for approximate Bayesian credible
#'   intervals using the default covariance.
#' @param nsim integer; the number of simulations used in computing the
#'   simultaneous intervals.
#' @param shift logical; should the model intercept be added to the smooth?
#'   Only the intercept is added; other parametric effects, including factor
#'   effects associated with a factor `by` smooth, are excluded. For full fitted
#'   values and their confidence intervals, use [fitted_values()].
#' @param transform logical; should the smooth be evaluated on a transformed
#'   scale? For generalised models, this involves applying the inverse of the
#'   link function used to fit the model. Alternatively, the name of, or an
#'   actual, function can be supplied to transform the smooth and it's
#'   confidence interval.
#' @param ncores number of cores for generating random variables from a
#'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
#'   Parallelization will take place only if OpenMP is supported (but appears
#'   to work on Windows with current `R`).
#' @param partial_match logical; should matching `parm` use a partial match or
#'   an exact match? Can only be used if `length(parm)` is `1`.
#' @param ... additional arguments passed to [smooth_estimates()], including
#'   `interval_method` to select Gaussian or stored scasm bootstrap intervals.
#' @param newdata DEPRECATED! data frame; containing new values of the
#'   covariates used in the model fit. The selected smooth(s) will be evaluated
#'   at the supplied values.
#'
#' @inheritParams get_vcov
#' @inheritParams smooth_estimates
#' 
#' @note `overall_uncertainty` applies to both pointwise and simultaneous
#'   intervals. Each simultaneous band covers the evaluation points of one
#'   fitted smooth; separate smooths do not form a joint coverage set.
#'   Native scasm fits default to `overall_uncertainty = FALSE` and use stored
#'   bootstrap draws for pointwise intervals when available, as in
#'   [smooth_estimates()]. Shifting and transforming a smooth preserves its
#'   interval limits, including asymmetric bootstrap limits.
#' 
#' @return a tibble with components:
#' 1. `.smooth`; character indicating to which term each row relates,
#' 2. `.type`; the type of smooth,
#' 3. `.by` the name of the by variable if a by smooth, `NA` otherwise,
#' 4. one or more vectors of values at which the smooth was evaluated, named as
#'    per the variables in the smooth,
#' 5. zero or more variables containing values of the by variable,
#' 6. `.estimate`; estimated value of the smooth,
#' 7. `.se`; standard error of the estimated value of the smooth,
#' 8. `.crit`; critical value for the `100 * level`% confidence interval;
#'    `NA` for bootstrap limits.
#' 9. `.lower_ci`; lower limit of the confidence or simultaneous interval,
#' 10. `.upper_ci`; upper limit of the confidence or simultaneous interval,
#'
#' @author Gavin L. Simpson
#'
#' @importFrom stats family qnorm
#' @importFrom mgcv PredictMat
#' @importFrom stats quantile vcov setNames
#' @importFrom dplyr bind_rows
#' @importFrom tibble add_column
#' @importFrom mvnfast rmvn
#' @importFrom tidyselect all_of last_col
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 2, cli.unicode = FALSE)
#' }
#' dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
#' mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' # new data to evaluate the smooths at, say over the middle 50% of range
#' # of each covariate
#' middle <- function(x, n = 50, coverage = 0.5) {
#'   v <- (1 - coverage) / 2
#'   q <- quantile(x, prob = c(0 + v, 1 - v), type = 8)
#'   seq(q[1], q[2], length = n)
#' }
#' new_data <- sapply(dat[c("x0", "x1", "x2", "x3")], middle)
#' new_data <- data.frame(new_data)
#'
#' ## point-wise interval for smooth of x2
#' ci <- confint(mod, parm = "s(x2)", type = "confidence", data = new_data)
#' ci
#' \dontshow{
#' options(op)
#' }
`confint.gam` <- function(
  object,
  parm,
  level = 0.95,
  data = newdata,
  n = 100,
  type = c("confidence", "simultaneous"),
  nsim = 10000,
  shift = FALSE,
  transform = FALSE,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  ncores = 1,
  partial_match = FALSE,
  ...,
  newdata = NULL
) {
  if (missing(overall_uncertainty) && is_scasm_model(object)) {
    overall_uncertainty <- FALSE
  }
  S <- smooths(object)
  ## select smooths
  select <- check_user_select_smooths(
    smooths = S,
    select = parm,
    partial_match = partial_match
  )
  ## did it match anything?
  if (!any(select)) {
    stop("No smooths matched <", paste(parm, collapse = ", "),
      ">. Try adding `partial_match = TRUE`?",
      call. = FALSE
    )
  }
  if (!is.null(newdata)) newdata_deprecated()
  type <- normalize_smooth_interval(type)
  out <- list(smooth_estimates(object,
    select = which(select), n = n, data = data,
    partial_match = partial_match, unconditional = unconditional,
    frequentist = frequentist, overall_uncertainty = overall_uncertainty,
    interval = type, ci_level = level, n_sim = nsim, n_cores = ncores, ...))

  ilink <- if (is.logical(transform)) {
    if (isTRUE(transform)) family(object)$linkinv else identity
  } else if (!is.null(transform)) match.fun(transform)

  if (shift) {
    const <- coef(object)
    nms <- names(const)
    test <- grep("Intercept", nms)
    const <- ifelse(length(test) == 0L, 0, const[test])
  } else {
    const <- 0
  }

  interval_info <- attr(out[[1L]], "interval_info")

  ## simplify to a data frame for return
  metadata <- lapply(out, attr, "coordinate_info")
  smooth_variables <- do.call("c", lapply(metadata, attr, "smooth_variables"))
  coordinate_info <- do.call("c", metadata)
  if (!is.null(coordinate_info)) {
    coordinate_info <- coordinate_info[!duplicated(names(coordinate_info))]
    attr(coordinate_info, "smooth_variables") <-
      smooth_variables[!duplicated(names(smooth_variables))]
  }
  tensor_term_order <- do.call("c", lapply(out, attr, "tensor_term_order"))
  out <- bind_rows(out)
  attr(out, "coordinate_info") <- coordinate_info
  attr(out, "tensor_term_order") <- tensor_term_order

  # This was needed with `[.evaluated_smooth` before switching to
  # NextMethod() to call the next S3 `[` method.
  # See: https://github.com/tidyverse/tibble/issues/511#issuecomment-431225229
  # Note needed, it seems now that NextMethod() is used but extending tibbles
  # is not currently well documented.
  # class(out) <- class(out)[-(1:2)]

  # Shift the stored limits directly: bootstrap limits need not be symmetric.
  out <- mutate(out,
    across(all_of(c(".estimate", ".lower_ci", ".upper_ci")), ~ .x + const)
  )

  ## transform
  out <- out |>
    mutate(across(all_of(c(".estimate", ".lower_ci", ".upper_ci")),
      .fns = ilink
    )) |>
    order_interval_bounds()

  # smooth_estimates has columns in different places, relocate them to match
  # old output as much as possible
  out <- relocate(out, all_of(c(
    ".estimate", ".se", ".crit", ".lower_ci", ".upper_ci"
  )), .after = last_col())

  ## prepare for return
  attr(out, "interval_info") <- interval_info
  class(out) <- c("confint.gam", class(out))
  out # return
}

#' @rdname confint.gam
#'
#' @importFrom stats confint
#'
#' @export
`confint.gamm` <- function(object, ...) {
  confint(object[["gam"]], ...)
}

#' @rdname confint.gam
#'
#' @importFrom stats confint
#'
#' @export
`confint.gamm4` <- function(object, ...) {
  confint(gamm4_gam(object), ...)
}
