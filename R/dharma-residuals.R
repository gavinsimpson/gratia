#' Simulation-based PIT residuals
#'
#' Calculate DHARMa-style residuals using gratia's fitted response generators.
#' No DHARMa or mgcViz installation is needed.
#'
#' @param model a fitted model supported by [diagnostic_support], with a
#'   generator for comparable scalar responses.
#' @param n positive integer; number of response datasets to simulate.
#' @inheritParams quantile_residuals
#' @param ... model-specific diagnostic options, including sdmTMB's `component`
#'   and `field_method`.
#' @return A numeric vector in observation order. With `na.exclude`, excluded
#'   observations are restored as `NA`; with `na.omit`, only fitting rows remain.
#' @details
#' For each observation, calculate the fractions of simulated responses strictly
#' below it and below or equal to it. The residual is sampled uniformly between
#' these fractions when simulations tie with the observation. Without ties it
#' is the fraction strictly below the observation. This implements DHARMa's PIT
#' idea internally, using gratia's response distributions and conditioning rules.
#'
#' Residuals lie in `[0, 1]`. An observation below every simulated response has
#' residual zero; one above every simulated response has residual one. Endpoints
#' are retained. Increasing `n` improves the resolution of the empirical CDF.
#' Continuous responses without ties have residuals on the grid `0:n / n`.
#'
#' Responses are generated together to retain any modelled dependence. When
#' only marginal response draws are available, these residuals describe the
#' marginal distributions. Censored observations and multivariate responses
#' are unsupported. Binomial successes and failures are compared as proportions
#' with the model's fitting trial counts. A response CDF and finite response
#' moments are not required.
#'
#' Use `type = "dharma"` and `n_dharma` in diagnostic plots. QQ and worm plots
#' use uniform reference quantiles. Independent responses have approximate
#' pointwise order-statistic bands: the finite simulation grid means the
#' continuous uniform reference is approximate. Bands are omitted for dependent
#' responses because independence-based intervals do not describe their order
#' statistics. These checks use fitted parameters without refitting the model.
#'
#' An explicit `seed` preserves the caller's random-number state. With
#' `seed = NULL`, simulation and tie randomisation advance the current state.
#' @seealso [appraise()], [qq_plot()], [worm_plot()], [quantile_residuals()]
#' @references
#' Hartig, F. DHARMa: Residual Diagnostics for Hierarchical (Multi-Level / Mixed)
#' Regression Models. \doi{10.32614/CRAN.package.DHARMa}.
#' @examples
#' fit <- mgcv::gam(count ~ spray, family = poisson(),
#'   data = InsectSprays, method = "REML")
#' head(dharma_residuals(fit, n = 250, seed = 42))
#' appraise(fit, type = "dharma", n_dharma = 250, seed = 42)
#' @export
dharma_residuals <- function(model, n = 250L, seed = NULL,
                             conditioning = NULL, ...) {
  n <- validate_diagnostic_bins(n, "n")
  d <- diagnostic_seed(seed, prepare_diagnostic_data(model, type = "dharma",
    seed = seed, conditioning = conditioning, n_dharma = n, ...))
  unname(restore_model_rows(d$data$residuals, d$source))
}

# The default count is shared by extraction and plotting. Internal calls that
# consume an already prepared vector retain its count when none is supplied.
dharma_simulation_count <- function(model, n = NULL) {
  cached <- attr(model, "gratia.diagnostics")
  n <- n %||% if (identical(cached$type, "dharma")) cached$n_dharma else 250L
  validate_diagnostic_bins(n, "n_dharma")
}

cache_dharma_residuals <- function(model, n = NULL, seed = NULL) {
  n <- dharma_simulation_count(model, n)
  r <- diagnostic_residuals(model_used_rows(model), "dharma", seed, n_dharma = n)
  attr(model, "gratia.diagnostics") <- list(type = "dharma", residuals = r,
    n_dharma = n, seed = seed)
  model
}

# Accumulate two counts without retaining a matrix of simulation results.
# Randomise after all draws, so the result also matches a matrix calculation
# using those same draws and the same random-number state.
distribution_dharma_residuals <- function(distribution, y, n, seed = NULL) {
  n <- validate_diagnostic_bins(n, "n_dharma")
  if (!is.numeric(y) || !is.null(dim(y)) || !length(y) || any(!is.finite(y)))
    stop("DHARMa-style residuals require finite scalar observations.", call. = FALSE)
  draw <- distribution$rd %||% distribution$marginal_rd
  if (!is.function(draw)) stop("DHARMa-style residuals are unavailable: no response generator.",
    call. = FALSE)
  diagnostic_seed(seed, {
    below <- equal <- numeric(length(y))
    for (i in seq_len(n)) {
      value <- draw()
      if (!is.numeric(value) || !is.null(dim(value)) || length(value) != length(y))
        stop("Simulated responses must have one numeric value per fitting observation.",
          call. = FALSE)
      if (any(!is.finite(value))) stop("Simulated responses must be finite.", call. = FALSE)
      below <- below + (value < y)
      equal <- equal + (value == y)
    }
    lower <- below / n
    upper <- (below + equal) / n
    if (any(equal > 0)) {
      randomized <- stats::runif(length(y), lower, upper)
      lower[equal > 0] <- randomized[equal > 0]
    }
    unname(lower)
  })
}

model_dharma_residuals <- function(model, n, seed = NULL) {
  f <- stats::family(model)
  grouped <- is_gfam_family(f)
  if (grouped) {
    context <- gfam_diagnostic_context(model)
    active <- sort(unique(context$index))
    for (i in active) {
      component <- context$components[[i]]$family
      label <- paste0(i, " <", component$family, ">")
      if (family_type(component) %in% c("cnorm", "clog", "cpois"))
        dharma_exact_censored_response(slice_observations(context$y,
          which(context$index == i)), paste0("grouped component ", label))
      if (!is.function(fix_family_rd(component)$rd))
        stop("No response generator for grouped component ", label, ".", call. = FALSE)
    }
  }
  y <- model$y %||% stats::model.response(stats::model.frame(model))
  if (family_type(f) %in% c("cnorm", "clog", "cpois") || !is.null(attr(y, "censor")))
    y <- dharma_exact_censored_response(y)
  if (is.matrix(y)) {
    if (!identical(family_type(f), "binomial"))
      stop("DHARMa-style residuals do not support multivariate responses.", call. = FALSE)
    y <- y[, 1L] / pmax(rowSums(y), 1)
  }
  if (is.factor(y)) y <- as.numeric(y != levels(y)[1L])
  distribution <- response_distribution(model)
  distribution_dharma_residuals(distribution, y, n, seed)
}

# Censoring families can also be fitted to entirely uncensored observations.
# Only equal response bounds can be compared directly with their latent draws.
dharma_exact_censored_response <- function(y, label = "observations") {
  bounds <- censored_response_bounds(y)
  if (anyNA(bounds$lower) || anyNA(bounds$upper) ||
      any(bounds$lower != bounds$upper))
    stop("DHARMa-style residuals do not support censored ", label, ".", call. = FALSE)
  bounds$upper
}

# Label this calculation consistently, including its acronym's capitalisation.
diagnostic_residual_label <- function(type) {
  if (identical(type, "dharma")) "DHARMa-style residuals" else
    paste(stringr::str_to_title(type), "residuals")
}

dharma_qq_reference <- function(diagnostics, level, by_family) {
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) stop("'level' must be between zero and one.", call. = FALSE)
  distribution <- response_distribution(diagnostics$source)
  independent <- isTRUE(distribution$independent)
  dat <- diagnostics$data[c("residuals", intersect(".family", names(diagnostics$data)))]
  dat <- dat[is.finite(dat$residuals), , drop = FALSE]
  if (!nrow(dat)) stop("No finite DHARMa-style residuals are available.", call. = FALSE)
  parts <- if (by_family && ".family" %in% names(dat))
    split(dat, dat$.family, drop = TRUE) else list(dat)
  df <- dplyr::bind_rows(lapply(parts, function(part) {
    part <- part[order(part$residuals), , drop = FALSE]
    nr <- nrow(part); i <- seq_len(nr); alpha <- (1 - level) / 2
    part$theoretical <- stats::ppoints(nr)
    if (independent) {
      part$lower <- stats::qbeta(alpha, i, nr + 1 - i)
      part$upper <- stats::qbeta(1 - alpha, i, nr + 1 - i)
    }
    part
  }))
  subtitle <- if (independent)
    "Uniform reference; approximate pointwise intervals (finite response simulations)" else
    paste0("Marginal uniform reference; bands omitted: independence-based intervals do not apply\n",
      distribution$dependence_reason %||% "Dependent responses")
  subtitle <- paste0(subtitle, "; ", diagnostics$n_dharma, " response simulations")
  list(data = df, method = "uniform reference", intercept = 0, slope = 1,
    center = df$theoretical, by_family = by_family,
    colour_family = ".family" %in% names(dat) && !by_family,
    subtitle = diagnostic_reference_description(distribution, subtitle))
}
