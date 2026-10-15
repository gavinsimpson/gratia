#' Simulate example data for fitting GAMs
#'
#' A tidy reimplementation of the functions implemented in [mgcv::gamSim()]
#' that can be used to fit GAMs. The sampling
#' distribution can be applied to all the example types.
#'
#' @details `data_sim()` can simulate data from several underlying models of
#'   known true functions. Descriptive names and their original short names
#'   are interchangeable, giving identical output for the same arguments and
#'   seed. The available options currently are:
#'   
#'   * `"additive"` or `"eg1"`: a four term additive true model. This is the
#'     classic Gu & Wahba
#'     four univariate term test model. See [`gw_functions`] for more details of
#'     the underlying four functions.
#'   * `"bivariate_surface"` or `"eg2"`: a bivariate smooth true model.
#'   * `"varying_coefficient"` or `"eg3"`: an example containing a continuous
#'     by smooth (varying
#'     coefficient) true model. The model is \eqn{\hat{y}_i = f_2(x_{2i})x_{1i}}{
#' yhat = f(x2)x1} where the function \eqn{f_2()} is \eqn{f_2(x) = 0.2 * x^{11} *
#' (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^{10}}{f(x) = 0.2 * x^11 * (10 *
#' (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10}.
#'   * `"factor_by"` or `"eg4"`: a factor by smooth true model. The true model
#'     contains a factor
#'     with 3 levels, where the response for the *n*th level follows the *n*th
#'     Gu & Wahba function (for \eqn{n \in {1, 2, 3}}{n in {1,2,3}}).
#'   * `"additive_factor"` or `"eg5"`: an additive plus factor true model. The
#'     response is a linear
#'     combination of the Gu & Wahba functions 2, 3, 4 (the latter is a null
#'     function) plus a factor term with four levels.
#'   * `"additive_group_effects"` or `"eg6"`: an additive model plus four
#'     group intercepts with fixed realised values `3, 6, 9, 12`.
#'   * `"additive_correlated"` or `"eg7"`: a version of the model in `"eg1"`,
#'     but where the covariates are
#'     correlated.
#'   * `"gu_wahba_f2"` or `"gwf2"`: a model where the response is Gu & Wahba's
#'     \eqn{f_2(x_i)}{f_2(x_i)} plus noise.
#'   * `"luo_wahba_f6"` or `"lwf6"`: a model where the response is Luo &
#'     Wahba's "example 6"
#'     function \eqn{sin(2(4x-2)) + 2 exp(-256(x-0.5)^2)}{
#'     sin(2 * ((4 * x) - 2)) + (2 * exp(-256 * (x - .5)^2))} plus noise.
#'   * `"gfam"`: simulates data for use with GAMs with
#'     `family = gfam(families)`. See example in [mgcv::gfam()]. If this model
#'     is specified then `dist` is ignored and `gfam_families` is used to
#'     specify which distributions are included in the simulated data. Can be a
#'     vector of the distributions allowed by `dist`, including repeated names.
#'     Defaults use Gaussian standard deviation 0.5, Gamma dispersion 0.5,
#'     Tweedie dispersion 1.5, negative binomial size 3 and Tweedie power 1.5.
#'     Historical grouped ordinal cuts are `c(1, 2, 2.5)`.
#'     Explicit `scale`, `theta`, `power`, `n_cat` and `cuts` override these
#'     settings. A supplied `scale` is the Gaussian standard deviation and the
#'     Gamma or Tweedie dispersion. Identity, logit and log links are used
#'     for normal, binary and positive-mean responses, respectively. Ordinal
#'     responses use a logistic latent variable.
#'
#'   The random component providing noise or sampling variation can follow one
#'   of the distributions, specified via argument `dist`
#'
#'   * `"normal"`: Gaussian,
#'   * `"poisson"`: Poisson,
#'   * `"binary"`: Bernoulli,
#'   * `"negbin"`: Negative binomial,
#'   * `"tweedie"`: Tweedie,
#'   * `"gamma"`: gamma , and
#'   * `"ordered categorical"`: ordered categorical
#'
#'   Other arguments provide the parameters for the distribution.
#'
#' @param model character; the name of a model, such as `"additive"` or its
#'   original name `"eg1"`. See Details for all names and aliases.
#' @param n numeric; the number of observations to simulate.
#' @param dist character; a sampling distribution for the response
#'   variable. `"ordered categorical"` is a synonym of `"ocat"`.
#' @param scale numeric; for normal responses, the standard deviation; for
#'   Poisson and negative binomial responses, a multiplier of the true
#'   function; for binary responses, a multiplier after subtracting 5; for
#'   Gamma and Tweedie responses, the dispersion. Ignored for ordinal
#'   responses. Legacy numeric vectors are recycled across observations.
#'   Use `parameters` with the family-object interface.
#' @param theta numeric; the dispersion parameter \eqn{\theta} to use. The
#'   default is entirely arbitrary, chosen only to provide simulated data that
#'   exhibits extra dispersion beyond that assumed by under a Poisson.
#' @param power numeric; the Tweedie power parameter.
#' @param n_cat integer; the number of categories for categorical response.
#'   Currently only used for `dist %in% c("ocat", "ordered categorical")`.
#' @param cuts numeric; vector of cut points on the latent variable, excluding
#'   the end points `-Inf` and `Inf`. Must be one fewer than the number of
#'   categories: `length(cuts) == n_cat - 1`.
#' @param seed numeric; the seed for the random number generator. Passed to
#'   [base::set.seed()].
#' @param gfam_families character; a vector of distributions to use in
#'   generating data with grouped families for use with `family = gfam()`. The
#'   allowed distributions as as per `dist`.
#'
#' @param family an instantiated family object, such as `poisson()` or
#'   `mgcv::gaulss()`. Cannot be combined with explicitly supplied `dist`,
#'   `scale`, `theta`, `power`, `n_cat`, `cuts` or `gfam_families`. See
#'   [data_sim_families] for coverage and generating parameters.
#' @param predictors `NULL` or a function of the generated data, including raw
#'   true-function columns. Return a finite numeric vector for one linear
#'   predictor or a matrix with one row per observation and one column per
#'   linear predictor, in the family's order. These values are on the link
#'   scale. `NULL` uses calibrated example predictors; see [data_sim_families].
#' @param parameters a named list of natural-scale generating parameters.
#'   Parameters fixed in the family constructor are used directly. Fitting
#'   starting values are not generating parameters: supply the corresponding
#'   entries explicitly. See [data_sim_families] for names and defaults.
#' @param observation `NULL` or a named list with `left`, `right` and/or
#'   `interval_width`, for censored families. Limits and widths have length
#'   one or `n`. Complete responses are generated before censoring. See
#'   [data_sim_families] for response encodings.
#'
#' @section Shape-constrained examples:
#' Models named `"sc_increasing"`, `"sc_decreasing"`, `"sc_convex"`,
#' `"sc_concave"`, `"sc_positive"`, `"sc_increasing_convex"`,
#' `"sc_increasing_concave"`, `"sc_decreasing_convex"` and
#' `"sc_decreasing_concave"` provide univariate reference shapes on `[0, 1]`.
#' `"sc_additive"` combines four terms; `"sc_tensor"` is increasing in two
#' coordinates; `"sc_location_scale"` has a concave log-mean and increasing
#' log dispersion. The latter defaults to `family = mgcv::gammals()`; other
#' shape templates default to Gaussian noise with standard deviation 0.2.
#' See [data_sim_shapes] for functions, derivatives and `mgcv::scasm()` recipes.
#'
#' @return A tibble containing responses, covariates and true-function columns.
#'   Legacy `f` is the predictor after its distribution-specific transformation;
#'   component columns retain their original function values. With `family`,
#'   `f` is the first linear predictor, `y` can be a matrix column, and the
#'   `"truth"` attribute contains `eta` (all linear predictors), `parameters`
#'   (natural distribution parameters), `functions` (the original template),
#'   `response` (complete responses before censoring), and `family`. Grouped
#'   families also record `index` and component-specific parameter lists.
#'   Binomial family objects return proportions and a `trials` column for
#'   fitting weights. Cox models return observed times and a `status` column.
#'   An explicit `seed` restores the caller's RNG state, including its absence;
#'   `seed = NULL` advances the caller's RNG normally.
#'
#' @seealso [data_sim_families], [data_sim_shapes]
#'
#' @references
#' Gu, C., Wahba, G., (1993). Smoothing Spline ANOVA with Component-Wise
#' Bayesian "Confidence Intervals." *J. Comput. Graph. Stat.* **2**, 97–117.
#'
#' Luo, Z., Wahba, G., (1997). Hybrid adaptive splines. *J. Am. Stat. Assoc.*
#' **92**, 107–116.
#'
#' @export
#'
#' @examples
#' \dontshow{
#' op <- options(pillar.sigfig = 5, cli.unicode = FALSE)
#' }
#' data_sim("additive", n = 100, seed = 1)
#'
#' # an ordered categorical response
#' data_sim("additive", n = 100, dist = "ocat", n_cat = 4, cuts = c(-1, 0, 5))
#' \dontshow{
#' options(op)
#' }
`data_sim` <- function(
  model = "eg1", n = 400,
  scale = NULL, theta = 3, power = 1.5,
  dist = c("normal", "poisson", "binary", "negbin", "tweedie", "gamma",
    "ocat", "ordered categorical"),
  n_cat = 4, cuts = c(-1, 0, 5), seed = NULL,
  gfam_families = c("binary", "tweedie", "normal"),
  family = NULL, predictors = NULL, parameters = list(), observation = NULL
) {
  sim_number(n, "n", integer = TRUE)
  model_aliases <- c(
    additive = "eg1", bivariate_surface = "eg2", varying_coefficient = "eg3",
    factor_by = "eg4", additive_factor = "eg5", additive_group_effects = "eg6",
    additive_correlated = "eg7", gu_wahba_f2 = "gwf2", luo_wahba_f6 = "lwf6"
  )
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !model %in% c(names(data_sim_models()), names(model_aliases),
        data_sim_shapes(), "gfam")) {
    stop("Unknown 'model'. See ?data_sim for available models.", call. = FALSE)
  }
  if (model %in% names(model_aliases)) model <- model_aliases[[model]]
  if (!is.null(seed)) sim_number(seed, "seed", lower = -.Machine$integer.max,
    upper = .Machine$integer.max + 1, inclusive = TRUE)
  if (model == "sc_location_scale" && is.null(family) && missing(dist)) {
    family <- mgcv::gammals()
  }
  if (!is.null(family)) {
    supplied <- c(dist = !missing(dist), scale = !missing(scale),
      theta = !missing(theta), power = !missing(power), n_cat = !missing(n_cat),
      cuts = !missing(cuts), gfam_families = !missing(gfam_families))
    if (any(supplied)) stop("With 'family', use 'parameters' instead of: ",
      paste(names(supplied)[supplied], collapse = ", "), ".", call. = FALSE)
    run <- function() data_sim_family(model, n, family, predictors, parameters, observation)
  } else {
    if (!is.null(predictors) || length(parameters) || !is.null(observation)) {
      stop("'predictors', 'parameters' and 'observation' require 'family'.", call. = FALSE)
    }
    # The grouped example ignores dist, including ordinal post-processing.
    if (model == "gfam") {
      group_cuts <- if (missing(cuts) && missing(n_cat)) c(1, 2, 2.5) else cuts
      run <- function() sim_gfam(n, gfam_families, scale = scale, theta = theta,
        power = power, n_cat = n_cat, cuts = group_cuts)
    } else {
      dist <- match.arg(dist)
      if (dist == "ordered categorical") dist <- "ocat"
      if (is.null(scale)) scale <- if (model == "lwf6") 0.3 else
        if (model %in% data_sim_shapes()) 0.2 else 2
      sim_number(scale, "scale", lower = if (dist %in% c("poisson", "binary", "negbin")) -Inf else 0,
        inclusive = dist %in% c("normal", "ocat"), n = length(scale))
      if (dist == "negbin") sim_nb_size(theta, n = length(theta))
      if (dist == "tweedie") sim_number(power, "power", lower = 1, upper = 2, n = length(power))
      if (dist == "ocat") sim_validate_cuts(cuts, n_cat)
      sim_fun <- switch(dist, normal = sim_normal, poisson = sim_poisson,
        binary = sim_binary, negbin = sim_nb, tweedie = sim_tweedie,
        gamma = sim_gamma, ocat = sim_normal)
      run <- function() {
        data <- data_sim_template(model, n, sim_fun, scale, theta, power)
        if (dist == "ocat") data <- post_proc_ocat(data, n_cat = n_cat, cuts = cuts)
        data
      }
    }
  }
  if (is.null(seed)) run() else withr::with_seed(seed, run())
}

# Sampling-free callback used by the example templates.
sim_signal <- function(x, ...) tibble(y = x, f = x)

data_sim_models <- function() {
  list(eg1 = four_term_additive_model, eg2 = bivariate_model,
    eg3 = continuous_by_model, eg4 = factor_by_model,
    eg5 = additive_plus_factor_model, eg6 = four_term_plus_ranef_model,
    eg7 = correlated_four_term_additive_model, gwf2 = gu_Wahba_f2,
    lwf6 = luo_Wahba_f6)
}

data_sim_template <- function(model, n, sim_fun = sim_signal, scale = 2,
                              theta = 3, power = 1.5) {
  if (model %in% data_sim_shapes()) {
    data <- data_sim_shape(model, n)
    response <- sim_fun(data$f, scale = scale, theta = theta, power = power)
    data$y <- response$y
    data$f <- response$f
    return(data)
  }
  data_sim_models()[[model]](n = n, sim_fun = sim_fun, scale = scale,
    theta = theta, power = power)
}

#' @importFrom stats rnorm
`sim_normal` <- function(x, scale = 2, ...) {
  tibble(
    y = x + rnorm(length(x), mean = 0, sd = scale),
    f = x
  )
}

#' @importFrom stats rpois
`sim_poisson` <- function(x, scale = 2, ...) {
  lam <- exp(x * scale)
  tibble(y = rpois(rep(1, length(x)), lam), f = log(lam))
}

#' @importFrom stats rbinom binomial
`sim_binary` <- function(x, scale = 2, ...) {
  ilink <- inv_link(binomial())
  x <- (x - 5) * scale
  p <- ilink(x)
  tibble(y = rbinom(p, 1, p), f = x)
}

#' @importFrom stats rnbinom
`sim_nb` <- function(x, scale = 2, theta = 3, ...) {
  lam <- exp(x * scale)
  tibble(
    y = rnbinom(rep(1, length(x)), mu = lam, size = theta),
    f = log(lam)
  )
}

#' @importFrom mgcv rTweedie
`sim_tweedie` <- function(x, scale = 2, power = 1.5, ...) {
  mu <- exp((x / 3) + 0.1)
  tibble(
    y = if (length(power) == 1L && length(scale) == 1L) {
      rTweedie(mu = mu, p = power, phi = scale)
    } else rtw(mu, p = rep_len(power, length(mu)), phi = rep_len(scale, length(mu))),
    f = log(mu)
  )
}

#' @importFrom stats rgamma
`sim_gamma` <- function(x, scale = 2, ...) {
  mu <- exp(x / 3 + 0.1)
  tibble(
    y = rgamma(length(mu), shape = 1 / scale, scale = mu * scale),
    f = log(mu)
  )
}

# post-processing

# post-process ocat - simulates normal data, but we need to convert to
# categories.
#' @importFrom dplyr mutate select left_join join_by
#' @importFrom tibble tibble
#' @importFrom rlang .data
`post_proc_ocat` <- function(x, n_cat = 4, cuts = c(-1, 0, 5), ...) {
  if (!is.numeric(n_cat) || length(n_cat) != 1L ||
      !is.finite(n_cat) || n_cat < 2 || n_cat != floor(n_cat)) {
    stop("'n_cat' must be a single integer of at least 2.")
  }
  if (!is.numeric(cuts) || any(!is.finite(cuts)) ||
      any(diff(cuts) <= 0)) {
    stop("'cuts' must be finite numeric values in strictly increasing order.")
  }
  # follows example from ?ocat
  n_cuts <- length(cuts)
  if (n_cat != n_cuts + 1L) {
    stop("Number of cut points not equal to ", n_cat, "-1.")
  }

  alpha <- c(-Inf, cuts, Inf)
  ru <- runif(nrow(x))
  x <- mutate(x,
    f = .data$f - mean(.data$f),
    latent = .data$f + log(ru / (1 - ru))
  )
  cats <- seq_len(n_cat)
  lkp_up <- tibble(
    cat = cats,
    lower = alpha[cats], upper = alpha[cats + 1]
  )
  by <- join_by("latent" > "lower", "latent" <= "upper")
  x <- left_join(x, lkp_up, by = by) |>
    mutate(y = .data$cat) |>
    select(-c("cat", "lower", "upper"))

  x
}

## Gu Wahba functions
#' Gu and Wahba test functions
#'
#' @param x numeric; vector of points to evaluate the function at, on interval
#'   (0,1)
#' @param ... arguments passed to other methods, ignored.
#'
#' @rdname gw_functions
#' @export
#' @aliases gw_functions
#'
#' @examples
#' \dontshow{
#' op <- options(digits = 4)
#' }
#' x <- seq(0, 1, length = 6)
#' gw_f0(x)
#' gw_f1(x)
#' gw_f2(x)
#' gw_f3(x) # should be constant 0
#' \dontshow{
#' options(op)
#' }
gw_f0 <- function(x, ...) {
  2 * sin(pi * x)
}

#' @rdname gw_functions
#' @export
gw_f1 <- function(x, ...) {
  exp(2 * x)
}

#' @rdname gw_functions
#' @export
gw_f2 <- function(x, ...) {
  0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10
}

#' @rdname gw_functions
#' @export
gw_f3 <- function(x, ...) { # a null function with zero effect
  0 * x
}

## bivariate function
bivariate <- function(x, z, sx = 0.3, sz = 0.4, ...) {
  (pi^sx * sz) * (1.2 * exp(-(x - 0.2)^2 / sx^2 - (z - 0.3)^2 / sz^2) +
    0.8 * exp(-(x - 0.7)^2 / sx^2 - (z - 0.8)^2 / sz^2))
}

#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_cols
#' @importFrom rlang .data
`four_term_additive_model` <- function(n, sim_fun = sim_normal, scale = 2,
                                       theta = 3, power = 1.5, ...) {
  data <- tibble(
    x0 = runif(n, 0, 1), x1 = runif(n, 0, 1),
    x2 = runif(n, 0, 1), x3 = runif(n, 0, 1)
  )
  data <- mutate(data,
    f0 = gw_f0(.data$x0), f1 = gw_f1(.data$x1),
    f2 = gw_f2(.data$x2), f3 = gw_f3(.data$x3)
  )
  data2 <- sim_fun(
    x = data$f0 + data$f1 + data$f2, scale, theta = theta,
    power = power
  )
  data <- bind_cols(data2, data)
  data[c("y", "x0", "x1", "x2", "x3", "f", "f0", "f1", "f2", "f3")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_cols
#' @importFrom rlang .data
`correlated_four_term_additive_model` <- function(n, sim_fun = sim_normal,
                                                  scale = 2, theta = 3,
                                                  power = 1.5, ...) {
  data <- tibble(x0 = runif(n, 0, 1), x2 = runif(n, 0, 1))
  data <- mutate(data,
    x1 = .data$x0 * 0.7 + runif(n, 0, 0.3),
    x3 = .data$x2 * 0.9 + runif(n, 0, 0.1)
  )
  data <- mutate(data,
    f0 = gw_f0(.data$x0), f1 = gw_f1(.data$x1),
    f2 = gw_f2(.data$x2), f3 = gw_f3(.data$x0)
  )
  data2 <- sim_fun(
    x = data$f0 + data$f1 + data$f2, scale = scale,
    theta = theta, power = power
  )
  data <- bind_cols(data2, data)
  data[c("y", "x0", "x1", "x2", "x3", "f", "f0", "f1", "f2", "f3")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
`bivariate_model` <- function(n, sim_fun = sim_normal, scale = 2, theta = 3,
                              power = 1.5, ...) {
  data <- tibble(x = runif(n), z = runif(n))
  data2 <- sim_fun(
    x = bivariate(data$x, data$z), scale = scale,
    theta = theta, power = power
  )
  data <- bind_cols(data2, data)
  data
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
`continuous_by_model` <- function(n, sim_fun = sim_normal, scale = 2,
                                  theta = 3, power = 1.5, ...) {
  data <- tibble(x1 = runif(n, 0, 1), x2 = sort(runif(n, 0, 1)))
  `f_fun` <- function(x) {
    0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10
  }
  data2 <- sim_fun(
    x = f_fun(data$x2) * data$x1, scale = scale,
    theta = theta, power = power
  )
  data <- bind_cols(data2, data)
  data[c("y", "x1", "x2", "f")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
`factor_by_model` <- function(n, sim_fun = sim_normal, scale = 2, theta = 3,
                              power = 1.5, ...) {
  data <- tibble(
    x0 = runif(n, 0, 1), x1 = runif(n, 0, 1),
    x2 = sort(runif(n, 0, 1))
  )
  data <- mutate(data,
    f1 = 2 * sin(pi * .data$x2), f2 = exp(2 * .data$x2) -
      3.75887,
    f3 = 0.2 * .data$x2^11 * (10 * (1 - .data$x2))^6 +
      10 * (10 * .data$x2)^3 * (1 - .data$x2)^10,
    fac = as.factor(sample(1:3, n, replace = TRUE))
  )
  y <- data$f1 * as.numeric(data$fac == 1) +
    data$f2 * as.numeric(data$fac == 2) +
    data$f3 * as.numeric(data$fac == 3)
  data2 <- sim_fun(y, scale = scale, theta = theta, power = power)
  data <- bind_cols(data2, data)
  data[c("y", "x0", "x1", "x2", "fac", "f", "f1", "f2", "f3")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
`additive_plus_factor_model` <- function(n, sim_fun = sim_normal, scale = 2,
                                         theta = 3, power = 1.5, ...) {
  data <- tibble(
    x0 = rep_len(1:4, n), x1 = runif(n, 0, 1),
    x2 = runif(n, 0, 1), x3 = runif(n, 0, 1)
  )
  data <- mutate(data,
    f0 = 2 * .data$x0,
    f1 = exp(2 * .data$x1),
    f2 = 0.2 * .data$x2^11 * (10 * (1 - .data$x2))^6 + 10 *
      (10 * .data$x2)^3 * (1 - .data$x2)^10,
    f3 = 0 * .data$x3
  )
  y <- data$f0 + data$f1 + data$f2
  data2 <- sim_fun(y, scale = scale, theta = theta, power = power)
  data <- mutate(data, x0 = as.factor(.data$x0))
  data <- bind_cols(data2, data)
  data[c("y", "x0", "x1", "x2", "x3", "f", "f0", "f1", "f2", "f3")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
`four_term_plus_ranef_model` <- function(n, sim_fun = sim_normal, scale = 2,
                                         theta = 3, power = 1.5, ...) {
  data <- four_term_additive_model(n = n, sim_fun = sim_signal)
  data$fac <- factor(rep_len(1:4, n), levels = 1:4)
  data$f <- data$f + as.integer(data$fac) * 3
  response <- sim_fun(data$f, scale = scale, theta = theta, power = power)
  data$y <- response$y
  data$f <- response$f
  data[c("y", "x0", "x1", "x2", "x3", "fac", "f", "f0", "f1", "f2", "f3")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_cols
#' @importFrom rlang .data
`gu_Wahba_f2` <- function(
    n, sim_fun = sim_normal, scale = 2,
    theta = 3, power = 1.5, ...) {
  data <- tibble(x = runif(n, 0, 1))
  data <- mutate(data, f2 = gw_f2(.data$x))
  data2 <- sim_fun(x = data$f2, scale = scale, theta = theta, power = power)
  data <- bind_cols(data2, data)
  data[c("y", "x", "f", "f2")]
}

#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_cols
#' @importFrom rlang .data
`luo_Wahba_f6` <- function(
    n, sim_fun = sim_normal, scale = 0.3,
    theta = 3, power = 1.5, ...) {
  f <- function(x) {
    sin(2 * ((4 * x) - 2)) + (2 * exp(-256 * (x - .5)^2))
  }
  data <- tibble(x = runif(n, 0, 1))
  data <- mutate(data, f6 = f(.data$x))
  data2 <- sim_fun(x = data$f6, scale = scale, theta = theta, power = power)
  data <- bind_cols(data2, data)
  data[c("y", "x", "f")]
}

# Keep the historical grouped example's defaults while sharing response laws.
sim_gfam <- function(n, families, scale = NULL, theta = 3, power = 1.5,
                     n_cat = 4, cuts = c(-1, 0, 5)) {
  allowed <- c("normal", "poisson", "binary", "negbin", "tweedie", "gamma", "ocat")
  if (!is.character(families) || !length(families) || anyNA(families)) {
    stop("'gfam_families' must be a nonempty vector of distribution names.", call. = FALSE)
  }
  families[families == "ordered categorical"] <- "ocat"
  if (any(!families %in% allowed)) stop("Unknown distribution in 'gfam_families'.", call. = FALSE)
  if (n < length(families)) stop("'n' must be at least the number of grouped families.", call. = FALSE)
  if ("ocat" %in% families) sim_validate_cuts(cuts, n_cat)
  if ("negbin" %in% families) sim_nb_size(theta)
  if ("tweedie" %in% families) sim_number(power, "power", lower = 1, upper = 2)
  if (!is.null(scale) && any(families %in% c("normal", "gamma", "tweedie"))) {
    sim_number(scale, "scale", inclusive = !any(families %in% c("gamma", "tweedie")),
      n = length(scale))
  }
  data <- data_sim_template("eg1", n)
  for (nm in grep("^f", names(data), value = TRUE)) data[[nm]] <- data[[nm]] / 5
  index <- c(seq_along(families), sample.int(length(families), n - length(families), replace = TRUE))
  data$y <- numeric(n)
  for (i in seq_along(families)) {
    rows <- which(index == i)
    family <- switch(families[i], normal = stats::gaussian(), poisson = stats::poisson(),
      binary = stats::binomial(), negbin = mgcv::nb(theta = theta),
      tweedie = mgcv::Tweedie(p = power, link = "log"), gamma = stats::Gamma(link = "log"),
      ocat = mgcv::ocat(R = n_cat))
    phi <- if (is.null(scale)) switch(families[i], normal = 0.25, tweedie = 1.5, 0.5) else
      if (families[i] == "normal") rep_len(scale, n)[rows]^2 else rep_len(scale, n)[rows]
    parameters <- switch(families[i], normal = list(dispersion = phi),
      gamma = list(dispersion = phi), tweedie = list(dispersion = phi),
      ocat = list(cuts = cuts), list())
    data$y[rows] <- sim_family_response(family, matrix(data$f[rows], ncol = 1L), parameters)$y
  }
  data$index <- index
  data$family <- families[index]
  data[c("y", "index", "family", setdiff(names(data), c("y", "index", "family")))]
}


#' Generate reference simulations for testing
#'
#' @param scale numeric; the noise level.
#' @param seed numeric; the seed to use for simulating data.
#'
#' @return A named list of tibbles containing
#'
#' @importFrom tidyr expand_grid
#' @importFrom purrr pmap
#' @noRd
`create_reference_simulations` <- function(scale = 2, n = 100, seed = 42,
                                           theta = 4, power = 1.5) {
  `data_sim_wrap` <- function(model, dist, scale, theta, n, seed, ...) {
    data_sim(model,
      dist = dist, scale = scale, theta = theta,
      n = n, seed = seed, ...
    )
  }
  params <- expand_grid(
    model = paste0("eg", 1:7),
    dist = c(
      "normal", "poisson", "binary",
      "negbin", "ocat", "tweedie", "gamma"
    ),
    scale = rep(scale, length.out = 1),
    n = rep(n, length.out = 1),
    seed = rep(seed, length.out = 1),
    theta = rep(theta, length.out = 1), power = rep(power, length.out = 1)
  )
  out <- pmap(params, .f = data_sim_wrap)
  nms <- unlist(pmap(params[1:2], paste, sep = "-"))
  names(out) <- nms
  out
}
