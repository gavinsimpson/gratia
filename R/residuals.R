#' Partial residuals
#'
#' @param object an R object, typically a model. Currently only objects of
#'   class `"gam"` (or that inherit from that class) are supported.
#' @param ... arguments passed to other methods.
#'
#' @export
`partial_residuals` <- function(object, ...) {
  UseMethod("partial_residuals")
}

#' @param select character, logical, or numeric; which smooths to plot. If
#'   `NULL`, the default, then all model smooths are drawn. Numeric `select`
#'   indexes the smooths in the order they are specified in the formula and
#'   stored in `object`. Character `select` matches the labels for smooths
#'   as shown for example in the output from `summary(object)`. Logical
#'   `select` operates as per numeric `select` in the order that smooths are
#'   stored.
#' @param partial_match logical; should smooths be selected by partial matches
#'   with `select`? If `TRUE`, `select` can only be a single string to match
#'   against.
#'
#' @rdname partial_residuals
#'
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols arrange
#' @importFrom rlang expr_label
#'
#' @examples
#' \dontshow{
#' op <- options(pillar.sigfig = 4, cli.unicode = FALSE)
#' }
#' ## load mgcv
#' load_mgcv()
#'
#' ## example data - Gu & Wahba four term model
#' df <- data_sim("eg1", n = 400, seed = 42)
#' ## fit the model
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' ## extract partial residuals
#' partial_residuals(m)
#'
#' ## and for a select term
#' partial_residuals(m, select = "s(x2)")
#'
#' ## or with partial matching
#' partial_residuals(m, select = "x", partial_match = TRUE) # returns all
#' \dontshow{
#' options(op)
#' }
`partial_residuals.gam` <- function(object,
                                    select = NULL,
                                    partial_match = FALSE,
                                    ...) {
  model_name <- expr_label(substitute(object))
  ## get a vector of labels for smooths
  sms <- smooths(object)
  ## which were selected; select = NULL -> all selected
  take <- check_user_select_smooths(sms,
    select = select,
    partial_match = partial_match,
    model_name = model_name
  )
  if (!any(take)) {
    stop("No smooth label matched 'select'. Try with 'partial_match = TRUE'?",
      call. = FALSE
    )
  }
  sms <- sms[take] # subset to selected smooths

  ## compute partial resids
  p_resids <- compute_partial_residuals(object, terms = sms)

  ## cast as a tibble --- do something with the column names?
  ##  - they are non-standard: `s(x)` for example
  p_resids <- tibble::as_tibble(p_resids)

  p_resids
}

#' @export
`partial_residuals.gamm` <- function(object, ...) {
  partial_residuals(object[["gam"]], ...)
}

#' @export
`partial_residuals.list` <- function(object, ...) {
  if (!is_gamm4(object)) {
    stop("'object' is not a `gamm4()` fit. Can't handle general lists.")
  }
  partial_residuals(object[["gam"]], ...)
}

## Internal function to compute weighted residuals for use by other functions
#' @importFrom stats residuals weights
#' @importFrom tibble as_tibble
`compute_partial_residuals` <- function(object, terms = NULL, data = NULL) {
  ## weighted working residuals..., see #273
  ## need the working weights too, see #273 for further discussion
  w <- weights(object, type = "working")
  ## need as.numeric for gamm() objects
  w_resid <- as.numeric(residuals(object, "working")) * sqrt(w)

  ## if data is null, just grab the $model out of object
  if (is.null(data)) {
    data <- object[["model"]]
  } else {
    ## check size of data
    if (nrow(data) != length(w_resid)) {
      stop("Length of model residuals not equal to number of rows in 'data'",
        call. = FALSE
      )
    }
  }
  ## get the contributions for each selected smooth
  p_terms <- if (is.null(terms)) {
    predict(object, type = "terms", newdata = data)
  } else {
    predict(object, type = "terms", terms = terms, newdata = data)
  }
  attr(p_terms, "constant") <- NULL # remove intercept attribute
  ## and compute partial residuals
  p_resids <- p_terms + w_resid

  as_tibble(p_resids)
}

# -- quantile residuals -------------------------------------------------------
#' Randomised residuals
#'
#' @param model a fitted model object.
#' @param type character; which type of randomised residual to return
#' @param seed integer; the random seed to use when generating randomised
#'   residuals. Can be missing, in which case the current state residuals are
#'   computed using the current state of the random number generator.
#' @param ... arguments passed to other methods.
#' @details
#' With `na.exclude`, excluded observations are restored as `NA` residuals;
#' with `na.omit`, only model-used observations are returned.
#'
#' For `mgcv::cnorm()`, `mgcv::clog()`, and `mgcv::cpois()` models, censored
#' observations have PIT residuals sampled uniformly between `F(l)` and `F(u)`,
#' where `l` and `u` bound the censoring interval and `F` is the fitted latent
#' response CDF. Left and right censoring use probabilities zero and one,
#' respectively, for the unbounded end. Quantile residuals apply `qnorm()` to
#' these PIT values. Uncensored continuous observations use `F(y)`; uncensored
#' Poisson observations are randomized between `F(y - 1)` and `F(y)`.
#' Use non-integer censoring limits for `cpois()`, as recommended by mgcv.
#'
#' For CDF helpers with native log-tail support, quantile residuals are computed
#' directly from log probabilities in the smaller tail. This avoids infinite
#' residuals caused by rounding a probability to zero or one. No probability
#' clipping is applied: genuine zero-probability tails still give infinite
#' residuals. PIT residuals are returned as ordinary probabilities and may still
#' round to zero or one in extreme tails, so applying `qnorm()` to the returned
#' PIT values can be less accurate than requesting quantile residuals directly.
#' Native log tails are available for Poisson, negative binomial, binomial,
#' Gaussian, Gamma, `cnorm()`, `clog()`, `gaulss()`, `gammals()`, `scat()`, and
#' `betar()` families. Other CDF helpers retain ordinary-probability evaluation.
#'
#' @export
`quantile_residuals` <- function(
  model, type = c("pit", "quantile"), seed = NULL, ...
) {
  UseMethod("quantile_residuals")
}

#' @importFrom stats qnorm
#' @export
#' @rdname quantile_residuals
`quantile_residuals.gam` <- function(
  model, type = c("pit", "quantile"), seed = NULL, ...
) {
  # random seed stuff
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  r <- do_quantile_residuals(
    y = model$y,
    fv = model$fitted.values,
    wt = model$prior.weights,
    scale = model$sig2,
    fam = family(model),
    type = type
  )

  # return
  stats::naresid(stats::na.action(model), r)
}

#' @export
#' @rdname quantile_residuals
`quantile_residuals.glm` <- function(
  model, type = c("pit", "quantile"), seed = NULL, ...
) {
  # random seed stuff
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  r <- do_quantile_residuals(
    y = model$y,
    fv = model$fitted.values,
    wt = model$prior.weights,
    scale = summary(model)$dispersion,
    fam = family(model),
    type = type
  )

  stats::naresid(stats::na.action(model), r)
}

`do_quantile_residuals` <- function(
  y,     # observed response
  fv,    # fitted values
  wt,    # prior weights
  scale, # dispersion or scale
  fam,   # family
  type = c("pit", "quantile") # type of residual
) {
  # start randomised residuals code
  type <- match.arg(type)
  if (is.null(fam$cdf)) {
    fam <- fix_family_cdf(fam)
  }
  # is it still NULL?
  if (is.null(fam$cdf)) {
    stop("Quantile residuals are not available for this family.")
  }

  ft <- family_type(fam)
  if (ft %in% c("cnorm", "clog", "cpois")) {
    bounds <- censored_response_bounds(y, discrete = ft == "cpois")
  } else {
    lower <- y
    if (ft %in% c("poisson", "negative_binomial", "ziplss")) {
      lower <- y - 1
    } else if (ft == "binomial") {
      # The fitted response is a proportion; subtract one success, not one
      # whole proportion. Zero-weight rows have a degenerate distribution.
      lower <- y - 1 / pmax(wt, 1)
    }
    bounds <- list(lower = lower, upper = y)
  }

  if ("lower_tail" %in% names(formals(fam$cdf))) {
    return(log_tail_residuals(bounds, fv, wt, scale, fam$cdf, type))
  }

  # Preserve support for CDFs without native log-tail evaluation (including
  # user-supplied CDFs). Their accuracy remains limited by ordinary probabilities.
  r <- fam$cdf(bounds$upper, mu = fv, wt = wt, scale = scale, log_p = FALSE)
  random <- which(bounds$lower != bounds$upper)
  if (length(random)) {
    r0 <- fam$cdf(bounds$lower, mu = fv, wt = wt, scale = scale, log_p = FALSE)
    r[random] <- runif(length(random), min = r0[random], max = r[random])
  }
  if (type == "quantile") {
    r <- qnorm(r)
  }
  r
}

# Compute both tails directly, retaining tiny probabilities through the normal
# quantile transformation. The same uniform draw must be used for both tails.
log_tail_residuals <- function(bounds, mu, wt, scale, cdf, type) {
  log_u <- cdf(bounds$upper, mu, wt, scale, log_p = TRUE, lower_tail = TRUE)
  log_s <- cdf(bounds$upper, mu, wt, scale, log_p = TRUE, lower_tail = FALSE)
  random <- which(bounds$lower != bounds$upper)
  if (length(random)) {
    # runif() historically returned unnamed randomized residuals.
    log_u <- unname(log_u)
    log_s <- unname(log_s)
    log_f0 <- cdf(bounds$lower, mu, wt, scale, log_p = TRUE, lower_tail = TRUE)
    log_s0 <- cdf(bounds$lower, mu, wt, scale, log_p = TRUE, lower_tail = FALSE)
    v <- runif(length(random))
    log_v <- log(v)
    log_1mv <- log1p(-v)
    log_u[random] <- logspace_add(
      log_1mv + log_f0[random], log_v + log_u[random]
    )
    log_s[random] <- logspace_add(
      log_1mv + log_s0[random], log_v + log_s[random]
    )
  }

  # Choose the smaller tail, which avoids loss of precision near probability 1.
  lower <- log_u <= log_s
  if (type == "quantile") {
    r <- log_u
    r[] <- NA_real_
    idx <- which(lower)
    r[idx] <- qnorm(log_u[idx], log.p = TRUE)
    upper <- which(!lower)
    r[upper] <- qnorm(log_s[upper], lower.tail = FALSE, log.p = TRUE)
  } else {
    r <- exp(log_u)
    upper <- which(!lower)
    r[upper] <- -expm1(log_s[upper])
  }
  r
}

# Vectorized log(exp(a) + exp(b)), including two zero probabilities.
logspace_add <- function(a, b) {
  m <- pmax(a, b)
  out <- m + log1p(exp(pmin(a, b) - m))
  out[which(m == -Inf)] <- -Inf
  out
}

# Convert mgcv's response encoding to bounds for CDF evaluation. The fitted
# response is a vector with a "censor" attribute; the original is a matrix.
censored_response_bounds <- function(y, discrete = FALSE) {
  censor <- attr(y, "censor")
  if (is.matrix(y)) {
    censor <- y[, 2L]
    y <- y[, 1L]
  }
  y <- as.numeric(y)
  if (is.null(censor)) {
    censor <- y
  }

  lower <- pmin(y, censor)
  upper <- pmax(y, censor)
  # An exact count occupies the CDF jump (F(y - 1), F(y)). Censoring
  # intervals instead use F(l) and F(u), with non-integer limits for cpois.
  exact <- censor == y
  if (discrete) {
    lower[exact] <- y[exact] - 1
  }
  list(lower = lower, upper = upper)
}
