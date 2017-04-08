##' Simulate from the posterior distribution of a GAM
##'
##' Simulations from the posterior distribution of a fitted GAM model involve making random draws from a multivariate normal with mean vector equal to the estimated model coefficients and covariance matrix equal to the covariance matrix of the coefficients.
##'
##' @param object a fitted GAM, typically the result of a call to `gam()` or `gamm()`.
##' @param nsim numeric; the number of posterior simulations to return.
##' @param seed numeric; a random seed for the simulations.
##' @param newdata data frame; new observations at which the posterior draws from the model should be evaluated. If not supplied, the data used to fit the model will be used for `newdata`, if available in `object`.
##' @param freq logical; `TRUE` to return the frequentist covariance matrix of the parameter estimators, `FALSE` to return the Bayesian posterior covariance matrix of the parameters.
##' @param unconditional logical; if `TRUE` (and `freq == FALSE`) then the Bayesian smoothing parameter uncertainty corrected covariance matrix is returned, if available.
##' @param ... arguments passed to methods
##'
##' @return (Currently) A matrix with `nsim` columns.
##'
##' @author Gavin L. Simpson
##'
##' @importFrom stats simulate runif
##' @importFrom MASS mvrnorm
##'
##' @export
##'
##' @rdname simulate
##'
##' @examples
##' library("mgcv")
##' set.seed(2)
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' sims <- simulate(m1, nsim = 5, seed = 42)
##' head(sims)
##'
##' m2 <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' sims <- simulate(m2, nsim = 5, seed = 42)
##' head(sims)
`simulate.gam` <- function(object, nsim = 1, seed = NULL, newdata = NULL,
                           freq = FALSE, unconditional = FALSE, ...) {
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

    if (missing(newdata) || is.null(newdata)) {
        newdata <- object$model
    }

    V <- vcov(object, freq = freq, unconditional = unconditional)
    Rbeta <- mvrnorm(n = nsim, mu = coef(object), Sigma = V)
    Xp <- predict(object, newdata = newdata, type = "lpmatrix")
    sims <- Xp %*% t(Rbeta)
    attr(sims, "seed") <- RNGstate
    sims
}

##' @rdname simulate
##'
##' @export
`simulate.gamm` <- function(object, nsim = 1, seed = NULL, newdata = NULL,
                            freq = FALSE, unconditional = FALSE, ...) {
    simulate(object$gam, nsim = nsim, seed = seed, newdata = newdata,
             freq = freq, unconditional = unconditional, ...)
}
