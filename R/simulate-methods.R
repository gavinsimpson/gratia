##' Simulate from the posterior distribution of a GAM
##'
##' Simulations from the posterior distribution of a fitted GAM model involve
##'   making random draws from a multivariate normal with mean vector equal to
##'   the estimated model coefficients and covariance matrix equal to the
##'   covariance matrix of the coefficients.
##'
##' @param object a fitted GAM, typically the result of a call to [mgcv::gam]`
##'   or [mgcv::gamm()].
##' @param nsim numeric; the number of posterior simulations to return.
##' @param seed numeric; a random seed for the simulations.
##' @param newdata data frame; new observations at which the posterior draws
##'   from the model should be evaluated. If not supplied, the data used to fit
##'   the model will be used for `newdata`, if available in `object`.
##' @param freq logical; `TRUE` to return the frequentist covariance matrix of
##'   the parameter estimators, `FALSE` to return the Bayesian posterior
##'   covariance matrix of the parameters.
##' @param unconditional logical; if `TRUE` (and `freq == FALSE`) then the
##'   Bayesian smoothing parameter uncertainty corrected covariance matrix is
##'   returned, if available.
##' @param weights numeric; a vector of prior weights. If `newdata` is null
##'   then defaults to `object[["prior.weights"]]`, otherwise a vector of ones.
##' @param ... arguments passed to methods
##'
##' @return (Currently) A matrix with `nsim` columns.
##'
##' @author Gavin L. Simpson
##'
##' @importFrom stats simulate runif family
##' @importFrom mvtnorm rmvnorm
##' @importFrom mgcv fix.family.rd
##'
##' @export
##'
##' @rdname simulate
##'
##' @examples
##' library("mgcv")
##' \dontshow{set.seed(2)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' sims <- simulate(m1, nsim = 5, seed = 42)
##' head(sims)
`simulate.gam` <- function(object, nsim = 1, seed = NULL, newdata = NULL,
                           freq = FALSE, unconditional = FALSE,
                           weights = NULL, ...) {
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
    ## rd function if available
    rd_fun <- get_family_rd(object)

    ## dispersion or scale variable for simulation
    scale <- object[["sig2"]]
    if (is.null(scale)) {
        scale <- summary(object)[["dispersion"]]
    }

    if (missing(newdata) || is.null(newdata)) {
        newdata <- object[["model"]]
        weights <- object[["prior.weights"]]
    } else {
        if (is.null(weights)) {
            weights <- rep(1, nrow(newdata))
        }
    }

    mu <- predict(object, newdata = newdata, type = "response")
    sims <- replicate(nsim, rd_fun(mu = mu, wt = weights, scale = scale))

    attr(sims, "seed") <- RNGstate
    sims
}

##' @rdname simulate
##'
##' @export
`simulate.gamm` <- function(object, nsim = 1, seed = NULL, newdata = NULL,
                            freq = FALSE, unconditional = FALSE, weights = NULL,
                            ...) {
    simulate(object$gam, nsim = nsim, seed = seed, newdata = newdata,
             freq = freq, unconditional = unconditional,
             weights = weights, ...)
}

##' @rdname simulate
##'
##' @importFrom mvtnorm rmvnorm
##'
##' @export
`simulate.scam` <- function(object, nsim = 1, seed = NULL, newdata = NULL,
                            freq = FALSE, weights = NULL, ...) {
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
    ## rd function if available
    rd_fun <- get_family_rd(object)

    ## dispersion or scale variable for simulation
    scale <- object[["sig2"]]
    if (is.null(scale)) {
        scale <- summary(object)[["dispersion"]]
    }

    if (missing(newdata) || is.null(newdata)) {
        newdata <- object[["model"]]
        weights <- object[["prior.weights"]]
    } else {
        if (is.null(weights)) {
            weights <- rep(1, nrow(newdata))
        }
    }

    mu <- predict(object, newdata = newdata, type = "response")

    ## sims <- mu
    sims <- replicate(nsim, rd_fun(mu = mu, wt = weights, scale = scale))

    attr(sims, "seed") <- RNGstate
    sims
}
