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
#' @param ... arguments passed to methods.
#'
#' @export
`post_draws` <-  function(model, ...) {
    UseMethod("post_draws")
}

#' @export
#' @rdname post_draws
`post_draws.default` <- function(model,
    n,
    method = c("gaussian", "mh", "inla", "user"),
    mu = NULL,
    sigma = NULL,
    n_cores = 1L,
    burnin = 1000,
    thin = 1,
    t_df = 40,
    rw_scale = 0.25,
    index = NULL,
    frequentist = FALSE,
    unconditional  = FALSE,
    parametrized = TRUE,
    mvn_method = c("mvnfast", "mgcv"),
    draws = NULL, ...) {
    # what posterior sampling are we using
    method <- match.arg(method)
    betas <- switch(method,
        "gaussian" = gaussian_draws(model = model, n = n,
            n_cores = n_cores, index = index, frequentist = frequentist,
            unconditional = FALSE, parametrized = parametrized,
            mvn_method = mvn_method, ...),
        "mh" = mh_draws(n = n, model = model, burnin = burnin,
            thin = thin, t_df = t_df, rw_scale = rw_scale, index = index, ...),
        "inla" = .NotYetImplemented(),
        "user" = user_draws(model = model, draws = draws, ...))
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
`generate_draws.gam` <- function(model,
    n,
    method = c("gaussian", "mh", "inla"),
    mu = NULL,
    sigma = NULL,
    n_cores = 1L,
    burnin = 1000,
    thin = 1,
    t_df = 40,
    rw_scale = 0.25,
    index = NULL,
    frequentist = FALSE,
    unconditional  = FALSE,
    mvn_method = c("mvnfast", "mgcv"), ...) {
    # what posterior sampling are we using
    method <- match.arg(method)
    betas <- switch(method,
        "gaussian" = gaussian_draws(model = model, n = n,
            n_cores = n_cores, index = index, frequentist = frequentist,
            unconditional = FALSE, mvn_method = mvn_method, ...),
        "mh" = mh_draws(n = n, model = model, burnin = burnin,
            thin = thin, t_df = t_df, rw_scale = rw_scale, index = index),
        "inla" = .NotYetImplemented())
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
`gaussian_draws.gam` <- function(model, n, n_cores = 1L, index = NULL,
    frequentist = FALSE, unconditional = FALSE, mvn_method = "mvnfast", ...) {
    mu <- coef(model)
    sigma <- get_vcov(model, frequentist = frequentist,
        unconditional = unconditional)
    if (!is.null(index)) {
        mu <- mu[index]
        sigma <- sigma[index, index, drop = FALSE]
    }
    betas <- if (isTRUE(identical(mvn_method, "mvnfast"))) {
        mvnfast::rmvn(n = n, mu = mu, sigma = sigma, ncores = n_cores)
    } else {
        mgcv::rmvn(n = n, mu = mu, V = sigma)
    }
    betas
}

#' @importFrom mvnfast rmvn
#' @export
#' @rdname gaussian_draws
`gaussian_draws.scam` <- function(model, n, n_cores = 1L, index = NULL,
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
`mh_draws.gam` <- function(model, n, burnin = 1000, thin = 1,
    t_df = 40, rw_scale = 0.25, index = NULL, ...) {
    capture.output(betas <- mgcv::gam.mh(b = model, ns = n * thin,
      burn = burnin, thin = thin, t.df = t_df, rw.scale = rw_scale))
    rw_acceptance <- betas[["rw.accept"]]
    fixed_acceptance <- betas[["accept"]]
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
#' @inheritParams post_draws
#' @export
`user_draws` <- function(model, draws, ...) {
    UseMethod("user_draws")
}

#' @export
`user_draws.gam` <- function(model, draws, index = NULL, ...) {
    # draws must be a matrix
    if (!is.matrix(draws)) {
        stop("Supplied 'draws' is not a matrix of coefficients.",
            call. = FALSE)
    }

    # draws must have as many columns as model coefficients
    n_coef <- length(coef(model))
    n_col <- ncol(draws)
    if (isFALSE(identical(n_col, n_coef))) {
        stop("Supplied 'draws' doesn't match number of model coefficients.\n",
            "Number of model coefs: ", n_coef, "\n",
            "Number of columns in 'draws': ", n_col, "\n", call. = FALSE)
    }

    # if index provided, subset the draws
    if (!is.null(index)) {
        draws <- draws[, index, drop = FALSE]
    }

    # return the user-supplied draws
    draws
}
