#' Partial residuals
#'
#' @param object an R object, typically a model. Currently only objects of
#'   class `"gam"` (or that inherit from that class) are supported.
#' @param ... arguments passed to other methods.
#'
#' @details
#' With `na.exclude`, excluded observations are restored as `NA` partial
#' residuals, in fitting order after any `subset`. With `na.omit`, only retained
#' observations are returned. Computation uses aligned working residuals,
#' working weights and term predictions before restoring excluded positions.
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
  p_resids <- restore_model_rows(
    compute_partial_residuals(object, terms = sms), object
  )

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
`partial_residuals.gamm4` <- function(object, ...) {
  partial_residuals(gamm4_gam(object), ...)
}

## Internal function to compute weighted residuals for use by other functions
#' @importFrom stats residuals weights
#' @importFrom tibble as_tibble
`compute_partial_residuals` <- function(object, terms = NULL, data = NULL) {
  used <- model_used_rows(object)
  w <- weights(used, type = "working")
  w_resid <- as.numeric(residuals(used, "working")) * sqrt(w)
  # Evaluate terms on the same retained observations as the residuals.
  p_terms <- predict(used, type = "terms", terms = terms,
                     newdata = used$model)
  attr(p_terms, "constant") <- NULL
  p_resids <- p_terms + w_resid
  if (!is.null(data)) {
    p_resids <- residual_rows(p_resids, object, data, partial = TRUE)
  }

  as_tibble(p_resids)
}

# -- quantile residuals -------------------------------------------------------
#' PIT and randomised quantile residuals
#'
#' Map observations to probabilities under the fitted response distribution.
#' These probability integral transform (PIT) residuals have a uniform reference
#' distribution under a correctly specified continuous model with known
#' parameters. Quantile residuals transform them to standard-normal scores.
#' Discrete observations require randomisation over their probability mass.
#'
#' @param model a fitted model object.
#' @param type character; `"pit"` returns probabilities and `"quantile"` returns
#'   standard-normal scores.
#' @param seed integer or `NULL`; random seed for randomised residuals. With
#'   `NULL`, calculations use the current random-number state. An explicit seed
#'   preserves the caller's state, including when none was previously initialised.
#' @param ... arguments passed to other methods.
#' @param conditioning character or `NULL`; hold fitted random effects fixed
#'   (`"conditional"`), average over new effects (`"marginal"`), or use the
#'   extracted GAM (`"gam_component"`), where supported. See [diagnostic_support].
#' @param component character; which delta sdmTMB component to check; see
#'   [appraise()].
#' @details
#' With `na.exclude`, excluded observations are restored as `NA` residuals;
#' with `na.omit`, only observations used for fitting are returned.
#'
#' For `mgcv::cnorm()`, `mgcv::clog()` and `mgcv::cpois()` models, censored
#' observations have PIT residuals sampled uniformly between `F(l)` and `F(u)`.
#' Here `l` and `u` bound the censoring interval and `F` is the fitted cumulative
#' distribution function (CDF) of the underlying response. Left and right
#' censoring use probabilities zero and one for the unbounded end, respectively.
#' Quantile residuals apply `qnorm()` to these PIT values. Uncensored continuous
#' observations use `F(y)`; uncensored Poisson observations are randomised between
#' `F(y - 1)` and `F(y)`. Use non-integer censoring limits for `cpois()`, as
#' recommended by mgcv.
#'
#' For grouped-family GAMs fitted with [mgcv::gfam()], each observation uses its
#' component's CDF, fitted nuisance parameters, dispersion and weights. Residuals
#' are returned in fitting order. All active components must supply supported
#' CDFs and probability limits for discrete or censored observations. Unsupported
#' components give an error; [appraise()] can instead select deviance residuals
#' automatically. See [appraise()] for supported families. Tweedie zeros have
#' positive probability mass: their PITs are randomised between `F(0-) = 0` and
#' `F(0)`. Positive observations use `F(y)`.
#'
#' When a CDF can calculate log probabilities in both tails, quantile residuals
#' use the log probability in the smaller tail directly. This avoids infinities
#' caused by rounding probabilities to zero or one. Probabilities are not
#' clipped, so true zero-probability tails still give infinite residuals for
#' ordinary models; grouped diagnostics report non-finite residuals as errors.
#' PITs are returned as ordinary probabilities and may still round to zero or
#' one in extreme tails. Applying `qnorm()` to returned PITs can therefore be
#' less accurate than requesting quantile residuals directly. CDFs without log
#' probabilities retain ordinary-probability calculations.
#'
#' Marginal checks may calculate residuals from ranks in joint response
#' simulations when no analytic marginal CDF is available. See
#' [diagnostic_support] for the rank definition, conditioning choices and
#' limitations after fitting.
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
quantile_residuals.gam <- function(model, type = c("pit", "quantile"),
                                    seed = NULL, conditioning = NULL, ...) {
  check_distribution_conditioning(conditioning)
  type <- match.arg(type)
  used <- model_used_rows(model)
  if (is_gfam_family(stats::family(used))) {
    r <- diagnostic_seed(seed, gfam_quantile_residuals(gfam_diagnostic_context(used), type))
  } else {
    r <- distribution_quantile_residuals(response_distribution(used), used$y,
      type = type, seed = seed)
  }
  restore_model_rows(r, model)
}

#' @export
#' @rdname quantile_residuals
quantile_residuals.glm <- function(model, type = c("pit", "quantile"),
                                    seed = NULL, ...) {
  quantile_residuals.gam(model, type = type, seed = seed, ...)
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
    } else if (ft == "tweedie") {
      # Compound Poisson Tweedie responses have a probability atom at zero.
      # Its left limit is F(0-) = 0, rather than F(0) used for positive values.
      lower[which(y == 0)] <- -Inf
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

# Compute both tails directly, retaining tiny probabilities through the chosen
# quantile transformation. The same uniform draw must be used for both tails.
log_tail_residuals <- function(bounds, mu, wt, scale, cdf, type,
                               quantile_function = stats::qnorm) {
  log_u <- if (is.function(bounds$cdf_upper))
    bounds$cdf_upper(log_p = TRUE, lower_tail = TRUE) else
    cdf(bounds$upper, mu, wt, scale, log_p = TRUE, lower_tail = TRUE)
  log_s <- if (is.function(bounds$cdf_upper))
    bounds$cdf_upper(log_p = TRUE, lower_tail = FALSE) else
    cdf(bounds$upper, mu, wt, scale, log_p = TRUE, lower_tail = FALSE)
  random <- which(bounds$lower != bounds$upper | (bounds$left_limit %||% FALSE))
  if (length(random)) {
    # runif() historically returned unnamed randomized residuals.
    log_u <- unname(log_u)
    log_s <- unname(log_s)
    log_f0 <- if (is.function(bounds$cdf_lower))
      bounds$cdf_lower(log_p = TRUE, lower_tail = TRUE) else
      cdf(bounds$lower, mu, wt, scale, log_p = TRUE, lower_tail = TRUE)
    log_s0 <- if (is.function(bounds$cdf_lower))
      bounds$cdf_lower(log_p = TRUE, lower_tail = FALSE) else
      cdf(bounds$lower, mu, wt, scale, log_p = TRUE, lower_tail = FALSE)
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
    r[idx] <- quantile_function(log_u[idx], log.p = TRUE)
    upper <- which(!lower)
    r[upper] <- quantile_function(log_s[upper], lower.tail = FALSE, log.p = TRUE)
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


#' @export
#' @rdname quantile_residuals
quantile_residuals.lm <- function(model, type = c("pit", "quantile"), seed = NULL, ...) {
  quantile_residuals.gam(prepare_lm_diagnostics(model), type, seed, ...)
}

#' @export
#' @rdname quantile_residuals
quantile_residuals.default <- function(model, type = c("pit", "quantile"),
                                       seed = NULL, conditioning = NULL, ...) {
  type <- match.arg(type)
  source <- diagnostic_data(model, type = "response", conditioning = conditioning)$source
  r <- distribution_quantile_residuals(response_distribution(source), source$y,
    type = type, seed = seed)
  restore_model_rows(r, source)
}

#' @export
#' @rdname quantile_residuals
quantile_residuals.glmmTMB <- function(model, type = c("pit", "quantile"),
                                       seed = NULL, conditioning = NULL, ...) {
  type <- match.arg(type)
  source <- glmmtmb_diagnostic_model(model, type = type, conditioning = conditioning)
  r <- distribution_quantile_residuals(response_distribution(source), source$y,
    type = type, seed = seed)
  restore_model_rows(r, source)
}

#' @export
#' @rdname quantile_residuals
quantile_residuals.gratia_glmmtmb_diagnostic <- function(model,
    type = c("pit", "quantile"), seed = NULL, conditioning = NULL, ...) {
  type <- match.arg(type)
  model <- diagnostic_plot_source(model, type, seed, conditioning, ...)
  r <- distribution_quantile_residuals(response_distribution(model), model$y,
    type = type, seed = seed)
  restore_model_rows(r, model)
}
