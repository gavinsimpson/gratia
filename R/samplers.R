#' Low-level Functions to generate draws from the posterior distribution of
#' model coefficients
#'
#' @param model a fitted R model. Currently only models fitted by `mgcv::gam()`
#'   or `mgcv::bam()`, or return an object that *inherits* from such objects are
#'   supported. Here, "inherits" is used in a loose fashion; models fitted by
#'   `scam::scam()` are support even though those models don't strictly inherit
#'   from class `"gam"` as far as `inherits()` is concerned.
#' @param n numeric; the number of posterior draws to take.
#' @param method character; which algorithm to use to sample from the posterior.
#'   Currently implemented methods are: `"gaussian"` and `"mh"`. `"gaussian"`
#'   calls `gaussian_draws()` which uses a Gaussian approximation to the
#'   posterior distribution. `"mh"` uses a simple Metropolis Hasting sampler
#'   which alternates static proposals based on a Gaussian approximation to the
#'   posterior, with random walk proposals. Note, setting `t_df` to a low value
#'   will result in heavier-tailed statistic proposals. See `mgcv::gam.mh()`
#'   for more details.
#' @param mu numeric; user-supplied mean vector (vector of model coefficients).
#'   Currently ignored.
#' @param sigma matrix; user-supplied covariance matrix for `mu`. Currently
#'  ignored.
#' @param n_cores integer; number of CPU cores to use when generating
#'   multivariate normal distributed random values. Only used if
#'   `mvn_method = "mvnfast"` **and** `method = "gaussian"`.
#' @param burnin numeric; the length of any initial burn in period to discard.
#'   See `mgcv::gam.mh()`.
#' @param thin numeric; retain only `thin` samples. See `mgcv::gam.mh()`.
#' @param t_df numeric; degrees of freedom for static multivariate *t* proposal.
#'   See `mgcv::gam.mh()`.
#' @param rw_scale numeric; factor by which to scale posterior covariance
#'   matrix when generating random walk proposals. See `mgcv::gam.mh()`.
#' @param index numeric; vector of indices of coefficients to use. Can be used
#'   to subset the mean vector and covariance matrix extracted from `model`.
#' @param frequentist logical; if `TRUE`, the frequentist covariance matrix of
#'   the parameter estimates is used. If `FALSE`, the Bayesian posterior
#'   covariance matrix of the parameters is used. See `mgcv::vcov.gam()`.
#' @param unconditional logical; if `TRUE`  the Bayesian smoothing parameter
#'   uncertainty corrected covariance matrix is used, *if available* for
#'   `model`. See `mgcv::vcov.gam()`.
#' @param parametrized logical; use parametrized coefficients and covariance
#'   matrix, which respect the linear inequality constraints of the model. Only
#'   for `scam::scam()` model fits.
#' @param mvn_method character; one of `"mvnfast"` or `"mgcv"`. The default is
#'   uses `mvnfast::rmvn()`, which can be considerably faster at generate large
#'   numbers of MVN random values than `mgcv::rmvn()`, but which might not work
#'   for some marginal fits, such as those where the covariance matrix is close
#'   to singular.
#' @param draws matrix; user supplied posterior draws to be used when
#'   `method = "user"`.
#' @param seed numeric; the random seed to use. If `NULL`, a random seed will
#'   be generated without affecting the current state of R's RNG.
#' @param ... arguments passed to methods.
#'
#' @export
`post_draws` <- function(model, ...) {
  UseMethod("post_draws")
}

#' @export
#' @rdname post_draws
#' @importFrom withr with_seed with_preserve_seed
`post_draws.default` <- function(
    model, n,
    method = c("gaussian", "mh", "inla", "user"), mu = NULL, sigma = NULL,
    n_cores = 1L, burnin = 1000, thin = 1, t_df = 40, rw_scale = 0.25,
    index = NULL, frequentist = FALSE, unconditional = FALSE,
    parametrized = TRUE, mvn_method = c("mvnfast", "mgcv"), draws = NULL,
    seed = NULL, ...) {
  if (is.null(seed)) {
    seed <- with_preserve_seed(runif(1))
  }
  # what posterior sampling are we using
  method <- match.arg(method)
  mvn_method <- match.arg(mvn_method)
  betas <- switch(method,
    "gaussian" = with_seed(seed, gaussian_draws(
      model = model, n = n,
      n_cores = n_cores, index = index, frequentist = frequentist,
      unconditional = unconditional, parametrized = parametrized,
      mvn_method = mvn_method, ...
    )),
    "mh" = with_seed(seed, mh_draws(
      n = n, model = model, burnin = burnin,
      thin = thin, t_df = t_df, rw_scale = rw_scale, index = index, ...
    )),
    "inla" = stop("'method = \"inla\"' is not yet implemented.",
    call. = FALSE),
    "user" = user_draws(model = model, draws = draws, ...)
  )
  betas
}
#' Generate posterior draws from a fitted model
#'
#' @export
#' @rdname post_draws
`generate_draws` <- function(model, ...) {
  UseMethod("generate_draws")
}

#' @export
#' @rdname post_draws
#' @importFrom withr with_seed with_preserve_seed
`generate_draws.gam` <- function(
    model, n, method = c("gaussian", "mh", "inla"),
    mu = NULL, sigma = NULL, n_cores = 1L, burnin = 1000, thin = 1, t_df = 40,
    rw_scale = 0.25, index = NULL, frequentist = FALSE, unconditional = FALSE,
    mvn_method = c("mvnfast", "mgcv"), seed = NULL, ...) {
  if (is.null(seed)) {
    seed <- with_preserve_seed(runif(1))
  }
  # what posterior sampling are we using
  method <- match.arg(method)
  mvn_method <- match.arg(mvn_method)
  betas <- switch(method,
    "gaussian" = with_seed(seed, gaussian_draws(
      model = model, n = n,
      n_cores = n_cores, index = index, frequentist = frequentist,
      unconditional = unconditional, mvn_method = mvn_method, ...
    )),
    "mh" = with_seed(seed, mh_draws(
      n = n, model = model, burnin = burnin,
      thin = thin, t_df = t_df, rw_scale = rw_scale, index = index, ...
    )),
    "inla" = stop("'method = \"inla\"' is not yet implemented.",
    call. = FALSE)
  )
  betas
}

#' Posterior samples using a simple Metropolis Hastings sampler
#'
#' @inheritParams post_draws
#'
#' @export
`gaussian_draws` <- function(model, ...) {
  UseMethod("gaussian_draws")
}

#' @importFrom mvnfast rmvn
#' @export
#' @rdname gaussian_draws
`gaussian_draws.gam` <- function(
    model, n, n_cores = 1L, index = NULL,
    frequentist = FALSE, unconditional = FALSE, mvn_method = "mvnfast", ...) {
  mu <- coef(model)
  sigma <- get_vcov(model,
    frequentist = frequentist,
    unconditional = unconditional
  )
  if (!is.null(index)) {
    mu <- mu[index]
    sigma <- sigma[index, index, drop = FALSE]
  }
  betas <- if (isTRUE(identical(mvn_method, "mvnfast"))) {
    mvnfast::rmvn(n = n, mu = mu, sigma = sigma, ncores = n_cores)
  } else {
    mgcv::rmvn(n = n, mu = mu, V = sigma)
  }
  # if we ask for n=1 samples, we need to get back a matrix
  if (!is.matrix(betas)) {
    betas <- matrix(betas, nrow = n, byrow = TRUE)
  }

  betas
}

#' @importFrom mvnfast rmvn
#' @export
#' @rdname gaussian_draws
`gaussian_draws.scam` <- function(
    model, n, n_cores = 1L, index = NULL,
    frequentist = FALSE, parametrized = TRUE, mvn_method = "mvnfast", ...) {
  mu <- coef(model, parametrized = parametrized)
  sigma <- vcov(model, freq = frequentist, parametrized = parametrized)
  if (!is.null(index)) {
    mu <- mu[index]
    sigma <- sigma[index, index, drop = FALSE]
  }
  betas <- if (isTRUE(identical(mvn_method, "mvnfast"))) {
    mvnfast::rmvn(n = n, mu = mu, sigma = sigma, ncores = n_cores)
  } else {
    mgcv::rmvn(n = n, mu = mu, V = sigma)
  }
  # if we ask for n=1 samples, we need to get back a matrix
  if (!is.matrix(betas)) {
    betas <- matrix(betas, nrow = n, byrow = TRUE)
  }
  betas
}

#' Posterior samples using a Gaussian approximation to the posterior
#' distribution
#'
#' @inheritParams post_draws
#'
#' @export
#' @rdname mh_draws
`mh_draws` <- function(model, ...) {
  UseMethod("mh_draws")
}

#' @importFrom mgcv gam.mh
#' @rdname mh_draws
#' @export
`mh_draws.gam` <- function(
    model, n, burnin = 1000, thin = 1,
    t_df = 40, rw_scale = 0.25, index = NULL, ...) {
  capture.output(betas <- mgcv::gam.mh(
    b = model, ns = n * thin,
    burn = burnin, thin = thin, t.df = t_df, rw.scale = rw_scale
  ))
  rw_acceptance <- attr(betas, "rw_acceptance")
  fixed_acceptance <- attr(betas, "fixed_acceptance")
  betas <- betas[["bs"]]
  if (!is.null(index)) {
    betas <- betas[, index, drop = FALSE]
  }
  attr(betas, "fixed_acceptance") <- fixed_acceptance
  attr(betas, "rw_acceptance") <- rw_acceptance
  betas
}

#' Handle user-supplied posterior draws
#'
#' @details The supplied `draws` must be a matrix (currently), with 1 column per
#'   model coefficient, and 1 row per posterior draw. The `"gam"` method has
#'   argument `index`, which can be used to subset (select) coefficients
#'   (columns) of `draws`. `index` can be any valid way of selecting (indexing)
#'   columns of a matrix. `index` is useful if you have a set of posterior draws
#'   for the entire model (say from [mgcv::gam.mh()]) and you wish to use those
#'   draws for an individual smooth, via [smooth_samples()].
#'
#' @inheritParams post_draws
#' @export
`user_draws` <- function(model, draws, ...) {
  UseMethod("user_draws")
}

#' @param index a vector to index (subset) the columns of `draws`.
#'
#' @export
#' @rdname user_draws
`user_draws.gam` <- function(model, draws, index = NULL, ...) {
  # draws must be a matrix
  if (!is.matrix(draws)) {
    stop("Supplied 'draws' is not a matrix of coefficients.",
      call. = FALSE
    )
  }

  # draws must have as many columns as model coefficients
  n_coef <- length(coef(model))
  n_col <- ncol(draws)
  if (isFALSE(identical(n_col, n_coef))) {
    stop("Supplied 'draws' doesn't match number of model coefficients.\n",
      "Number of model coefs: ", n_coef, "\n",
      "Number of columns in 'draws': ", n_col, "\n",
      call. = FALSE
    )
  }

  # if index provided, subset the draws
  if (!is.null(index)) {
    fx_a <- attr(draws, "fixed_acceptance")
    rw_a <- attr(draws, "rw_acceptance")
    draws <- draws[, index, drop = FALSE]
    if (!is.null(fx_a)) {
      attr(draws, "fixed_acceptance") <- fx_a
      attr(draws, "rw_acceptance") <- rw_a
    }
  }

  # return the user-supplied draws
  draws
}
