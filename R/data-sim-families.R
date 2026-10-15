#' Generating responses with mgcv family objects
#'
#' [data_sim()] accepts instantiated standard and mgcv family objects. The
#' example template supplies covariates and true functions; `predictors`
#' supplies the linear predictors; `parameters` supplies generating values
#' that are not themselves modelled by a predictor. The `"truth"` attribute
#' records all predictors, distribution parameters and complete responses.
#'
#' @section Scalar families:
#' The parameter names below belong inside `parameters = list(...)`.
#' Parameters are scalar unless explicitly described as observation-level.
#' Beta draws use the constructor's `eps` endpoint adjustment, matching mgcv's
#' generator. This adjustment can matter when the beta law places substantial
#' probability very close to zero or one; `eps` is recorded in the truth.
#'
#' | Family | Generating parameters |
#' |:-------|:----------------------|
#' | `gaussian()` | `dispersion = 1`, the non-negative variance |
#' | `poisson()`, `cpois()` | None |
#' | `binomial()` | `trials = 1`, positive integer trial counts |
#' | `Gamma()`, `inverse.gaussian()` | `dispersion = 1` |
#' | `nb()`, `negbin()` | `theta`, positive size; `Inf` gives the Poisson limit |
#' | `tw()`, `Tweedie()` | `power` in (1, 2), and `dispersion = 1` |
#' | `betar()` | Positive `precision` |
#' | `scat()` | `df` above the family's `min.df`, and positive t scale `sigma` |
#' | `ocat()` | Finite increasing `cuts`, one fewer than the categories |
#' | `ziP()` | `intercept` and natural-scale `slope` above the constructor's `b` |
#' | `cnorm()`, `clog()` | Positive `sigma`, Gaussian SD or logistic scale |
#' | `bcg()` | `lambda = 0` and positive log-response SD `sigma` |
#'
#' Dispersion and trials can have length one or `n`. All other scalar-family
#' parameters are constant across observations. Fixed constructor parameters
#' are used when available; conflicting explicit parameters are errors.
#' Otherwise the corresponding generating values are required, including
#' cut points for `ocat(R = ...)`. For example, both
#' `family = mgcv::nb(theta = 3)` and `family = mgcv::nb(),
#' parameters = list(theta = 3)` specify size 3. Constructor starting values
#' for an estimated parameter are not used as generating truth. A size range
#' in `negbin(c(1, 10))` also requires an explicit generating `theta`.
#'
#' For `ziP()`, `lambda = exp(eta)` and
#' `P(Y > 0) = 1 - exp(-exp(intercept + slope * eta))`.
#' Positive observations follow a Poisson distribution conditioned on being
#' positive. The natural `slope` includes the constructor's lower bound `b`;
#' the second entry of `ziP(theta = ...)` is instead a log excess above `b`.
#' For fixed ordinal spacings, mgcv fixes the first cut at -1; for example
#' `ocat(theta = c(1, 5))` supplies cuts `c(-1, 0, 5)`.
#' For free cuts whose first value is `a`, mgcv's fixed first cut of -1
#' represents the same probabilities after shifting the predictor by `-(a + 1)`.
#' Include the corresponding component intercept in grouped fitting recipes.
#'
#' Quasi families do not define a complete sampling distribution and are
#' rejected. Nonzero Box-Cox powers are currently rejected: some Gaussian
#' transformed responses would lie outside the inverse transformation's domain.
#'
#' @section Families with several linear predictors:
#' The columns returned by `predictors` follow mgcv's order and links, including
#' their constructor-specific bounds. These are link-scale predictors, not
#' the `mu` arguments of mgcv's random-number generators.
#'
#' | Family | Predictor order and natural parameters recorded in truth |
#' |:-------|:-------------------------------------------------------|
#' | `gaulss()` | Mean, SD predictor; records `mu`, `sigma` |
#' | `gammals()` | Log mean, log-dispersion predictor; records `mu`, `dispersion` |
#' | `gumbls()` | Location, log-scale predictor; records `location`, `mu` (mean), `sigma` |
#' | `gevlss()` | Location, log scale, shape predictor; records `location`, `sigma`, `shape` |
#' | `shash()` | Location, scale predictor, skewness, log tail; records `location`, `sigma`, `skewness`, `tail` |
#' | `twlss()` | Mean predictor, power predictor, log dispersion; records `mu`, `power`, `dispersion` |
#' | `ziplss()` | Log Poisson intensity, complementary log-log presence predictor; records `lambda`, `presence` |
#' | `multinom(K)` | K logits relative to category 0; records category `probabilities` |
#' | `mvn(d)` | d response means; `parameters$covariance` defaults to the identity matrix |
#'
#' These families need no scalar generating parameters except `mvn()`'s
#' optional positive definite covariance matrix. In `shash()`, the transformed
#' normal is multiplied by `tail * sigma`, following mgcv's parameterisation.
#' In `gumbls()`, the mean is location plus Euler's constant times `sigma`;
#' the first predictor specifies the location. For `gevlss()`, the default
#' shape link restricts shape to (-1, 0.5); the identity link allows any finite
#' shape. In `ziplss()`, positive counts follow a zero-truncated Poisson distribution.
#'
#' @section Default predictors:
#' Let `g` be the template's `f` before distribution transformations. Scalar
#' Gaussian, t, Gaussian-censoring, logistic-censoring and Box-Cox families use
#' natural location `g`. Binomial and beta means are `plogis((g - 5) / 5)`;
#' ordinal location is `g - mean(g)`; the `ziP()` predictor is `g / 5`.
#' Other scalar means are `exp(g / 5)`. Each mean is transformed with the
#' supplied family's link. If it is outside that link's domain, supply custom
#' predictors rather than relying on a default.
#'
#' For general families, default predictors are: `(g, 0)` for `gaulss()` and
#' `gumbls()`; `(g / 5, 0)` for `gammals()` and `ziplss()`;
#' `(g, 0, linkfun(0))` for `gevlss()` (zero shape under the supplied shape link);
#' `(g, 0, 0, 0)` for `shash()`; and `(g / 5, 0, 0)` for `twlss()`.
#' Multivariate normal means repeat `g`. Multinomial logit j is
#' `(g - mean(g)) / 5 * j / K`. The `sc_location_scale` template instead
#' supplies its own log mean and log dispersion for `gammals()`.
#'
#' @section Censoring and survival:
#' For `cnorm()`, `clog()`, `cpois()` and `bcg()`, `observation` can contain
#' finite `left` and `right` detection limits and a positive `interval_width`.
#' Responses outside detection limits are left or right censored. Other
#' responses are placed in rounding intervals wholly inside those limits;
#' values in intervals crossing a limit are retained exactly. Omitting
#' `observation` returns complete responses.
#'
#' Matrix responses use equal entries for exact observations, `(limit, -Inf)`
#' for left censoring, `(limit, Inf)` for right censoring, and `(lower, upper)`
#' for intervals. `bcg()` instead uses zero in the second entry for left
#' censoring; its limits must be positive. Poisson detection limits must be
#' non-integer, and interval widths must be integer numbers of counts. Its
#' intervals use half-integer endpoints to avoid ambiguity at count values.
#'
#' `cox.ph()` uses Weibull cumulative baseline hazard
#' `H0(t) = (t / baseline_scale)^baseline_shape`, with both baseline parameters
#' defaulting to 1. Event times have survival `exp(-H0(t) * exp(eta))`.
#' Independent exponential censoring has `censor_rate = 0.2`; zero disables
#' censoring. Default `eta` is `(g - mean(g)) / 5`. Fit using `weights = status`.
#' True event and censoring times are retained in the truth parameter list.
#'
#' @section Grouped families:
#' Supply `family = mgcv::gfam(list(...))`, optionally with
#' `parameters = list(components = list(...), index = ...)`. `components`
#' contains one parameter list per family-list position. `index` is one
#' integer position per row (a scalar selects the same family for every row).
#' Without `index`, every component is represented at least once and remaining
#' positions are sampled uniformly; `n` must be at least the number of families.
#' Repeated family types retain separate indices and parameters.
#'
#' The default shared predictor is `g / 5`; custom predictors return one column.
#' Scalar response families are supported; general families are rejected.
#' Censored-family components return complete, uncensored responses. Fit using
#' `cbind(y, index)` as the response and `weights = trials` if a component is
#' binomial with more than one trial. Row-level dispersion and trial vectors
#' in component parameter lists align with the full generated data, or can
#' have the component's row count. Truth parameters record each component's rows.
#'
#' @name data_sim_families
#' @seealso [data_sim()], [data_sim_shapes], [mgcv::family.mgcv()]
#' @examples
#' # A beta response: precision is explicitly chosen, rather than estimated.
#' d <- data_sim(family = mgcv::betar(theta = 12), seed = 2)
#' attr(d, "truth")$parameters$precision
#'
#' # Binomial proportions: use trials as the fitting weights.
#' d <- data_sim(family = binomial(), parameters = list(trials = 10), seed = 2)
#'
#' # Mean and standard deviation have separate predictors.
#' d <- data_sim("gwf2", family = mgcv::gaulss(), seed = 2,
#'   predictors = function(d) cbind(d$f, log(0.2 + d$x)))
#'
#' # Fit a censored normal model to the matrix response.
#' d <- data_sim("sc_increasing", family = mgcv::cnorm(theta = 0.2), seed = 2,
#'   observation = list(left = 0.1, right = 0.9))
#' \donttest{
#' m <- mgcv::gam(y ~ s(x), family = mgcv::cnorm(), data = d, method = "REML")
#' }
NULL

# Validate scalar or observation-level values without silently recycling them.
sim_number <- function(x, name, lower = 0, upper = Inf, integer = FALSE,
                       inclusive = FALSE, n = 1L) {
  if (!is.numeric(x) || !is.null(dim(x)) || !length(x) ||
      !length(x) %in% c(1L, n) || any(!is.finite(x)) ||
      any(if (inclusive) x < lower else x <= lower) || any(x >= upper) ||
      (integer && any(x != floor(x)))) {
    stop("'", name, "' must contain ", if (integer) "integer " else "numeric ",
      "values ", if (inclusive) ">= " else "> ", lower, " and < ", upper,
      "; supply one value", if (n > 1L) paste0(" or ", n, " values") else "", ".", call. = FALSE)
  }
  rep_len(x, n)
}

sim_validate_cuts <- function(cuts, n_cat = length(cuts) + 1L) {
  if (!is.numeric(n_cat) || length(n_cat) != 1L || !is.finite(n_cat) ||
      n_cat < 2 || n_cat != floor(n_cat)) {
    stop("'n_cat' must be a single integer of at least 2.", call. = FALSE)
  }
  if (!is.numeric(cuts) || !length(cuts) || any(!is.finite(cuts)) || any(diff(cuts) <= 0)) {
    stop("'cuts' must be finite numeric values in strictly increasing order.", call. = FALSE)
  }
  if (length(cuts) != n_cat - 1L) stop("Number of cut points not equal to ", n_cat, "-1.", call. = FALSE)
  cuts
}

# The registry describes the generating interface, not fitting starting values.
sim_family_registry <- function() {
  list(
    gaussian = c("dispersion"), poisson = character(), binomial = "trials",
    gamma = "dispersion", inverse_gaussian = "dispersion",
    negative_binomial = "theta", tweedie = c("power", "dispersion"),
    beta_regression = "precision", scaled_t = c("df", "sigma"),
    ordered_categorical = "cuts", zero_inflated_poisson = c("intercept", "slope"),
    cnorm = "sigma", clog = "sigma", cpois = character(), bcg = c("lambda", "sigma"),
    gaulss = character(), gammals = character(), gumbls = character(),
    gevlss = character(), shash = character(), twlss = character(),
    ziplss = character(), multinom = character(), multivariate_normal = "covariance",
    cox_ph = c("baseline_shape", "baseline_scale", "censor_rate")
  )
}

sim_parameters <- function(parameters, allowed) {
  if (!is.list(parameters) || (length(parameters) &&
      (is.null(names(parameters)) || anyNA(names(parameters)) ||
       any(!nzchar(names(parameters))) || anyDuplicated(names(parameters))))) {
    stop("'parameters' must be a uniquely named list.", call. = FALSE)
  }
  unknown <- setdiff(names(parameters), allowed)
  if (length(unknown)) stop("Unknown generating parameter(s): ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  if (any(vapply(parameters, is.null, logical(1)))) stop("Generating parameters cannot be NULL.", call. = FALSE)
}

sim_family_key <- function(family) {
  if (!inherits(family, "family") || !is.character(family$family)) {
    stop("'family' must be an instantiated family object, such as poisson().", call. = FALSE)
  }
  type_family <- family
  type_family$family <- sub("\\([^)]*\\)", "", family$family)
  key <- unique(family_type(type_family))
  if (length(key) != 1L || is.na(key)) stop("The family must specify one distribution type.", call. = FALSE)
  if (startsWith(key, "quasi")) stop("Quasi families do not define a sampling distribution; choose a generating family.", call. = FALSE)
  if (!key %in% names(sim_family_registry())) stop("No data generator for family '", family$family, "'.", call. = FALSE)
  key
}

# Fixed constructor parameters can be read, but estimated initial values are
# never treated as the user's chosen generating truth.
sim_auxiliary <- function(family, parameters, names, fixed = NULL) {
  if (is.null(fixed) && is.function(family$getTheta) && (isTRUE(family$n.theta == 0) || !inherits(family, "extended.family"))) {
    fixed <- if (identical(family_type(family), "bcg")) {
      value <- theta(family, transform = FALSE)
      c(value[1L], exp(value[length(value)]))
    } else theta(family)
  }
  # negbin() also accepts a range of sizes to search during fitting.
  # Such a range does not fix a generating size.
  if (identical(sim_family_key(family), "negative_binomial") && length(fixed) > 1L) fixed <- NULL
  out <- vector("list", length(names)); names(out) <- names
  for (i in seq_along(names)) {
    nm <- names[i]
    if (!is.null(parameters[[nm]])) {
      if (!is.null(fixed) && (length(fixed) < i ||
          !isTRUE(all.equal(parameters[[nm]], fixed[[i]], check.attributes = FALSE)))) {
        stop("Generating parameter '", nm, "' conflicts with the fixed family parameter.", call. = FALSE)
      }
      out[[i]] <- parameters[[nm]]
    } else if (!is.null(fixed) && length(fixed) >= i) out[[i]] <- fixed[[i]] else {
      stop("Supply 'parameters$", nm, "' or fix it in the family constructor; fitting starting values are not generating parameters.", call. = FALSE)
    }
  }
  out
}

sim_family_nlp <- function(family) {
  if (inherits(family, "general.family")) family$nlp %||% 1L else 1L
}

# Default predictors are calibrated examples. User predictors always take
# precedence and are already on the family's linear predictor scale.
sim_default_predictors <- function(data, family, key) {
  g <- data$f
  n <- nrow(data)
  if (key == "multivariate_normal") return(matrix(rep(g, family$nlp), nrow = n))
  if (key == "multinom") return(outer((g - mean(g)) / 5, seq_len(family$nlp) / family$nlp))
  if (inherits(family, "general.family")) {
    eta <- switch(key,
      gaulss = cbind(g, rep(0, n)),
      gammals = cbind(g / 5, rep(0, n)),
      gumbls = cbind(g, rep(0, n)),
      gevlss = cbind(g, rep(0, n), rep(family$linfo[[3L]]$linkfun(0), n)),
      shash = cbind(g, rep(0, n), rep(0, n), rep(0, n)),
      twlss = cbind(g / 5, rep(0, n), rep(0, n)),
      ziplss = cbind(g / 5, rep(0, n)), cox_ph = matrix((g - mean(g)) / 5, ncol = 1L))
    if ("f_scale" %in% names(data) && key == "gammals") {
      eta[, 1L] <- data$f
      eta[, 2L] <- family$linfo[[2L]]$linkfun(data$f_scale)
    }
    return(eta)
  }
  mu <- switch(key,
    gaussian =, scaled_t =, cnorm =, clog =, bcg = g,
    binomial =, beta_regression = stats::plogis((g - 5) / 5),
    ordered_categorical = g - mean(g), zero_inflated_poisson = g / 5,
    exp(g / 5))
  eta <- family$linkfun(mu)
  if (any(!is.finite(eta))) stop("Default predictors are outside this link's domain; supply 'predictors'.", call. = FALSE)
  matrix(eta, ncol = 1L)
}

sim_predictors <- function(data, family, predictors) {
  key <- sim_family_key(family)
  if (!is.null(predictors) && !is.function(predictors)) stop("'predictors' must be a function of the generated data.", call. = FALSE)
  eta <- if (is.null(predictors)) sim_default_predictors(data, family, key) else predictors(data)
  if (!is.numeric(eta) || !(is.null(dim(eta)) || is.matrix(eta))) stop("'predictors' must return a numeric vector or matrix.", call. = FALSE)
  if (is.null(dim(eta))) eta <- matrix(eta, ncol = 1L)
  if (nrow(eta) != nrow(data) || ncol(eta) != sim_family_nlp(family) || any(!is.finite(eta))) {
    stop("Predictors must have one finite row per observation and one column per family linear predictor.", call. = FALSE)
  }
  eta
}

data_sim_family <- function(model, n, family, predictors, parameters, observation) {
  if (!inherits(family, "family")) stop("'family' must be an instantiated family object.", call. = FALSE)
  if (is_gfam_family(family)) return(data_sim_grouped_family(model, n, family, predictors, parameters, observation))
  key <- sim_family_key(family)
  if (model == "gfam") stop("'model = \"gfam\"' requires a grouped family object.", call. = FALSE)
  data <- data_sim_template(model, n)
  raw <- data[setdiff(names(data), "y")]
  eta <- sim_predictors(data, family, predictors)
  response <- sim_family_response(family, eta, parameters)
  complete <- if (key == "cox_ph") response$parameters$event_time else response$y
  data$y <- sim_observe(response$y, observation, key)
  data$f <- eta[, 1L]
  if (!is.null(response$trials)) data$trials <- response$trials
  if (!is.null(response$status)) data$status <- response$status
  attr(data, "truth") <- list(eta = eta, parameters = response$parameters,
    functions = raw, response = complete, family = family$family)
  data
}

# Infinite negative binomial size is the Poisson limit supported by R.
sim_nb_size <- function(x, n = 1L) {
  if (!is.numeric(x) || !is.null(dim(x)) || !length(x) ||
      !length(x) %in% c(1L, n) || anyNA(x) || any(x <= 0)) {
    stop("'theta' must contain positive sizes (Inf gives the Poisson limit), with one value or one per observation.", call. = FALSE)
  }
  rep_len(x, n)
}

# Convert each family's predictors to the inputs its native RNG expects.
# Native constructors are read only; auxiliary parameters are sampled directly.
sim_family_response <- function(family, eta, parameters = list()) {
  key <- sim_family_key(family)
  sim_parameters(parameters, sim_family_registry()[[key]])
  n <- nrow(eta)
  pars <- parameters
  result <- list()
  phi <- function() sim_number(parameters$dispersion %||% 1, "dispersion",
    inclusive = key == "gaussian", n = n)
  if (inherits(family, "general.family") && !key %in% c("multinom", "multivariate_normal", "cox_ph")) {
    mu <- matrix(vapply(seq_len(ncol(eta)), function(j) family$linfo[[j]]$linkinv(eta[, j]), numeric(n)), nrow = n)
    if (key == "gammals") mu[, 1L] <- exp(mu[, 1L])
    if (any(!is.finite(mu))) stop("Predictors produce non-finite distribution parameters.", call. = FALSE)
    pars <- switch(key,
      gaulss = list(mu = mu[, 1L], sigma = 1 / mu[, 2L]),
      gammals = list(mu = mu[, 1L], dispersion = exp(mu[, 2L])),
      gumbls = list(location = mu[, 1L],
        mu = mu[, 1L] + 0.577215664901533 * exp(mu[, 2L]), sigma = exp(mu[, 2L])),
      gevlss = list(location = mu[, 1L], sigma = exp(mu[, 2L]), shape = mu[, 3L]),
      shash = list(location = mu[, 1L], sigma = exp(mu[, 2L]), skewness = mu[, 3L], tail = exp(mu[, 4L])),
      twlss = {ab <- get_tw_params(family); list(mu = mu[, 1L], power = theta_2_power(mu[, 2L], ab[1L], ab[2L]), dispersion = exp(mu[, 3L]))},
      ziplss = list(lambda = exp(mu[, 1L]), presence = -expm1(-exp(mu[, 2L]))))
    if (!is.null(pars$sigma)) sim_number(pars$sigma, "sigma", n = n)
    if (!is.null(pars$tail)) sim_number(pars$tail, "tail", n = n)
    if (key %in% c("gammals", "twlss")) {
      sim_number(pars$mu, "mu", n = n); sim_number(pars$dispersion, "dispersion", n = n)
    }
    if (key == "gevlss") {
      sim_number(pars$sigma, "sigma", n = n)
      u <- stats::runif(n); z <- -log(u); xi <- pars$shape
      shift <- -log(z)
      nonzero <- which(xi != 0)
      shift[nonzero] <- expm1(-xi[nonzero] * log(z[nonzero])) / xi[nonzero]
      y <- pars$location + pars$sigma * shift
    } else if (key == "gumbls") {
      y <- pars$location - pars$sigma * log(-log(stats::runif(n)))
    } else if (key == "twlss") {
      y <- rtw(pars$mu, p = pars$power, phi = pars$dispersion)
    } else if (key == "ziplss") {
      y <- sim_hurdle_poisson(pars$lambda, pars$presence)
    } else y <- family$rd(mu = mu, wt = rep(1, n), scale = 1)
  } else if (key == "multivariate_normal") {
    d <- ncol(eta)
    covariance <- parameters$covariance %||% diag(d)
    if (!is.matrix(covariance) || !is.numeric(covariance) ||
        !identical(dim(covariance), c(d, d)) || any(!is.finite(covariance)) ||
        !isSymmetric(covariance) || inherits(try(chol(covariance), silent = TRUE), "try-error")) {
      stop("'covariance' must be a finite, symmetric positive definite matrix matching the response dimension.", call. = FALSE)
    }
    y <- mgcv::rmvn(n, mu = eta, V = covariance)
    pars <- list(mu = eta, covariance = covariance)
  } else if (key == "multinom") {
    logits <- cbind(0, eta)
    probabilities <- exp(logits - apply(logits, 1L, max))
    probabilities <- probabilities / rowSums(probabilities)
    u <- stats::runif(n)
    y <- vapply(seq_len(n), function(i) {
      cumulative <- cumsum(probabilities[i, ])
      cumulative[length(cumulative)] <- 1
      which(cumulative >= u[i])[1L] - 1L
    }, integer(1))
    pars <- list(probabilities = probabilities)
  } else if (key == "cox_ph") {
    shape <- sim_number(parameters$baseline_shape %||% 1, "baseline_shape")
    scale <- sim_number(parameters$baseline_scale %||% 1, "baseline_scale")
    rate <- sim_number(parameters$censor_rate %||% 0.2, "censor_rate", inclusive = TRUE)
    event <- scale * exp((log(-log(stats::runif(n))) - eta[, 1L]) / shape)
    censor <- if (rate == 0) rep(Inf, n) else stats::rexp(n, rate)
    y <- pmin(event, censor)
    result$status <- as.integer(event <= censor)
    pars <- list(log_hazard_ratio = eta[, 1L], baseline_shape = shape, baseline_scale = scale,
      censor_rate = rate, event_time = event, censor_time = censor)
  } else {
    mu <- family$linkinv(eta[, 1L])
    if (any(!is.finite(mu)) || (is.function(family$validmu) && !isTRUE(family$validmu(mu)))) {
      stop("Predictors produce invalid distribution parameters for this family.", call. = FALSE)
    }
    pars <- list(mu = mu)
    if (key %in% c("gaussian", "gamma", "inverse_gaussian", "tweedie")) pars$dispersion <- phi()
    if (key == "gaussian") y <- rd_gaussian(mu, 1, pars$dispersion)
    if (key %in% c("poisson", "cpois")) y <- rd_poisson(mu, 1, 1)
    if (key == "gamma") y <- rd_gamma(mu, 1, pars$dispersion)
    if (key == "inverse_gaussian") y <- rd_invgaussian(mu, 1, pars$dispersion)
    if (key == "binomial") {
      trials <- sim_number(parameters$trials %||% 1, "trials", integer = TRUE, n = n)
      if (any(mu < 0 | mu > 1)) stop("Binomial probabilities must lie in [0, 1].", call. = FALSE)
      y <- rd_binomial(mu, trials, 1); pars$trials <- result$trials <- trials
    }
    if (key == "negative_binomial") {
      fixed <- if (!inherits(family, "extended.family")) theta(normalize_native_nb_parameters(family)) else NULL
      extra <- sim_auxiliary(family, parameters, "theta", fixed)
      sim_nb_size(extra$theta)
      y <- stats::rnbinom(n, mu = mu, size = extra$theta); pars <- c(pars, extra)
    }
    if (key == "tweedie") {
      fixed <- if (is.null(family$getTheta)) get_tw_params(family) else NULL
      extra <- sim_auxiliary(family, parameters, "power", fixed)
      sim_number(extra$power, "power", lower = 1, upper = 2)
      y <- rtw(mu, p = extra$power, phi = pars$dispersion); pars <- c(pars, extra)
    }
    if (key == "beta_regression") {
      extra <- sim_auxiliary(family, parameters, "precision")
      sim_number(extra$precision, "precision")
      if (any(mu <= 0 | mu >= 1)) stop("Beta means must lie strictly between zero and one.", call. = FALSE)
      # Match mgcv's endpoint treatment, retaining the theoretical beta parameters.
      eps <- get(".betarEps", envir = environment(family$rd))
      y <- pmax(eps, pmin(1 - eps, stats::rbeta(n, mu * extra$precision, (1 - mu) * extra$precision)))
      pars <- c(pars, extra, list(eps = eps))
    }
    if (key == "scaled_t") {
      extra <- sim_auxiliary(family, parameters, c("df", "sigma"))
      minimum <- get(".min.df", envir = environment(family$getTheta))
      sim_number(extra$df, "df", lower = minimum); sim_number(extra$sigma, "sigma")
      y <- mu + extra$sigma * stats::rt(n, df = extra$df); pars <- c(pars, extra)
    }
    if (key == "ordered_categorical") {
      fixed <- if (isTRUE(family$n.theta == 0)) theta(family) else NULL
      cuts <- parameters$cuts %||% fixed
      if (is.null(cuts)) stop("Supply 'parameters$cuts' or fixed category spacings in the family constructor.", call. = FALSE)
      cuts <- sim_validate_cuts(cuts)
      if (length(cuts) != length(theta(family))) stop("Cut points must match the family's number of categories.", call. = FALSE)
      if (!is.null(fixed) && !isTRUE(all.equal(cuts, fixed))) stop("Cut points conflict with the fixed family parameters.", call. = FALSE)
      latent <- mu + stats::rlogis(n)
      y <- findInterval(latent, c(-Inf, cuts, Inf), left.open = TRUE)
      pars <- c(pars, list(cuts = cuts, latent = latent))
    }
    if (key == "zero_inflated_poisson") {
      extra <- sim_auxiliary(family, parameters, c("intercept", "slope"))
      sim_number(extra$intercept, "intercept", lower = -Inf)
      lower <- get(".b", envir = environment(family$getTheta))
      sim_number(extra$slope, "slope", lower = lower)
      lambda <- exp(mu); presence <- -expm1(-exp(extra$intercept + extra$slope * mu))
      y <- sim_hurdle_poisson(lambda, presence)
      pars <- c(pars, extra, list(lambda = lambda, presence = presence))
    }
    if (key %in% c("cnorm", "clog", "bcg")) {
      extra <- sim_auxiliary(family, parameters, if (key == "bcg") c("lambda", "sigma") else "sigma")
      sim_number(extra$sigma, "sigma")
      if (key == "bcg" && (!is.numeric(extra$lambda) || length(extra$lambda) != 1L ||
          !is.finite(extra$lambda) || extra$lambda != 0)) {
        stop("bcg() generation currently requires Box-Cox 'lambda = 0'.", call. = FALSE)
      }
      y <- if (key == "clog") rd_logistic(mu, 1, extra$sigma) else rd_gaussian(mu, 1, extra$sigma^2)
      if (key == "bcg") y <- exp(y)
      pars <- c(pars, extra)
    }
  }
  if (!is.numeric(y) || NROW(y) != n || any(!is.finite(y))) {
    stop("The supplied predictors or parameters produced non-finite responses.", call. = FALSE)
  }
  result$y <- y; result$parameters <- pars
  result
}

# Inverting upper-tail Poisson probabilities avoids rounding P(Y=0) to one
# when lambda is small. Positive draws are from Poisson conditional on Y > 0.
sim_hurdle_poisson <- function(lambda, presence) {
  n <- length(lambda)
  sim_number(lambda, "lambda", n = n)
  if (any(!is.finite(presence)) || any(presence < 0 | presence > 1)) stop("Invalid presence probabilities.", call. = FALSE)
  y <- numeric(n)
  rows <- which(stats::runif(n) < presence)
  if (length(rows)) y[rows] <- stats::qpois(log(stats::runif(length(rows))) +
    log(-expm1(-lambda[rows])), lambda[rows], lower.tail = FALSE, log.p = TRUE)
  y
}

# Observation rules are separate from the complete response law. Limits are
# scalar or row-aligned; interval_width describes rounding intervals.
sim_observe <- function(y, observation, key) {
  if (is.null(observation)) return(y)
  if (!key %in% c("cnorm", "clog", "cpois", "bcg")) stop("'observation' is supported only for censored families.", call. = FALSE)
  sim_parameters(observation, c("left", "right", "interval_width"))
  n <- length(y)
  limit <- function(x, name, default) {
    if (is.null(x)) return(rep(default, n))
    if (!is.numeric(x) || !length(x) %in% c(1L, n) || any(!is.finite(x))) {
      stop("Censoring '", name, "' must have one finite value or one per observation.", call. = FALSE)
    }
    rep_len(x, n)
  }
  left <- limit(observation$left, "left", -Inf)
  right <- limit(observation$right, "right", Inf)
  if (any(left >= right)) stop("Left censoring limits must be below right limits.", call. = FALSE)
  finite <- c(left[is.finite(left)], right[is.finite(right)])
  if (key == "bcg" && any(finite <= 0)) stop("bcg() censoring limits must be positive.", call. = FALSE)
  if (key == "cpois" && any(finite == floor(finite))) stop("Use non-integer censoring limits for cpois().", call. = FALSE)
  out <- cbind(y, y)
  if (!is.null(observation$interval_width)) {
    width <- sim_number(observation$interval_width, "interval_width", n = n)
    if (key == "cpois") {
      if (any(width != floor(width))) stop("Poisson interval widths must be integer numbers of counts.", call. = FALSE)
      lo <- floor(y / width) * width - 0.5
    } else lo <- floor(y / width) * width
    hi <- lo + width
    # Only replace an exact value by an interval wholly inside the detection
    # limits, so subsequent censoring never reveals a tighter interval.
    rows <- which(lo >= left & hi <= right)
    out[rows, ] <- cbind(lo[rows], hi[rows])
  }
  rows <- which(y <= left)
  out[rows, ] <- cbind(left[rows], if (key == "bcg") 0 else -Inf)
  rows <- which(y >= right)
  out[rows, ] <- cbind(right[rows], Inf)
  if (key == "bcg") {
    rows <- which(out[, 1L] <= 0)
    out[rows, ] <- cbind(out[rows, 2L], 0)
  }
  out
}

# Family-list positions, rather than family names, identify the components.
data_sim_grouped_family <- function(model, n, family, predictors, parameters, observation) {
  if (!is.null(observation)) stop("Grouped families do not support matrix responses for censoring.", call. = FALSE)
  sim_parameters(parameters, c("components", "index"))
  components <- family$getfl()
  nf <- length(components)
  component_parameters <- parameters$components %||% rep(list(list()), nf)
  if (!is.list(component_parameters) || length(component_parameters) != nf) {
    stop("'parameters$components' must contain one parameter list per grouped family.", call. = FALSE)
  }
  for (component in components) {
    key <- sim_family_key(component)
    if (inherits(component, "general.family")) {
      stop("Grouped generators require scalar family components.", call. = FALSE)
    }
  }
  index <- parameters$index
  if (is.null(index)) {
    if (n < nf) stop("'n' must be at least the number of grouped families, or supply 'parameters$index'.", call. = FALSE)
    index <- c(seq_len(nf), sample.int(nf, n - nf, replace = TRUE))
  } else {
    index <- sim_number(index, "index", lower = 1, upper = nf + 1,
      integer = TRUE, inclusive = TRUE, n = n)
  }
  data <- data_sim_template(if (model == "gfam") "eg1" else model, n)
  raw <- data[setdiff(names(data), "y")]
  eta <- if (is.null(predictors)) matrix(data$f / 5, ncol = 1L) else
    sim_predictors(data, stats::gaussian(), predictors)
  y <- numeric(n); trials <- rep(1, n); truth <- vector("list", nf)
  for (i in seq_len(nf)) {
    rows <- which(index == i)
    # Validate empty components too, without drawing artificial observations.
    pars <- component_parameters[[i]]
    if (length(rows)) {
      for (nm in names(pars)) if (length(pars[[nm]]) == n &&
          nm %in% c("dispersion", "trials")) pars[[nm]] <- pars[[nm]][rows]
      response <- sim_family_response(components[[i]], eta[rows, , drop = FALSE], pars)
      y[rows] <- response$y
      if (!is.null(response$trials)) trials[rows] <- response$trials
      truth[[i]] <- list(rows = rows, parameters = response$parameters)
    } else sim_parameters(pars, sim_family_registry()[[sim_family_key(components[[i]])]])
  }
  data$y <- y; data$f <- eta[, 1L]; data$index <- as.integer(index)
  data$family <- vapply(components, function(f) f$family, character(1))[index]
  if (any(vapply(components, function(f) sim_family_key(f) == "binomial", logical(1)))) data$trials <- trials
  data <- data[c("y", "index", "family", setdiff(names(data), c("y", "index", "family")))]
  attr(data, "truth") <- list(eta = eta, parameters = truth, functions = raw,
    response = y, family = family$family, index = index)
  data
}
