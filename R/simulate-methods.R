#' Simulate from the posterior distribution of a GAM
#'
#' Simulations from the posterior distribution of a fitted GAM model involve
#' computing predicted values for the observation data for which simulated
#' data are required, then generating random draws from the probability
#' distribution used when fitting the model.
#'
#' For `simulate.gam()` to function, the `family` component of the fitted
#' model must contain, or be updateable to contain, the required random
#' number generator. See [mgcv::fix.family.rd()].
#'
#' @param object a fitted GAM, typically the result of a call to [mgcv::gam]`
#'   or [mgcv::gamm()].
#' @param nsim numeric; the number of posterior simulations to return.
#' @param seed numeric; a random seed for the simulations.
#' @param data data frame; new observations at which the posterior draws
#'   from the model should be evaluated. If not supplied, the data used to fit
#'   the model will be used for `newdata`, if available in `object`.
#' @param weights numeric; a vector of prior weights. If `newdata` is null
#'   then defaults to `object[["prior.weights"]]`, otherwise a vector of ones.
#' @param ... arguments passed to methods. `simulate.gam()` and
#'   `simulate.scam()` pass `...` on to `predict.gam()`. As such you can pass
#'   additional arguments such as `terms`, `exclude`, to select which model
#'   terms are included in the predictions. This may be useful, for example,
#'   for excluding the effects of random effect terms.
#' @param newdata Deprecated. Use `data` instead.
#'
#' @return (Currently) A matrix with `nsim` columns.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom stats simulate runif family
#' @importFrom mgcv fix.family.rd
#'
#' @export
#'
#' @rdname simulate
#'
#' @examples
#' load_mgcv()
#' dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' sims <- simulate(m1, nsim = 5, seed = 42)
#' head(sims)
`simulate.gam` <- function(object, nsim = 1, seed = NULL, data = newdata,
                           weights = NULL, ..., newdata = NULL) {
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

    if (!is.null(newdata)) {
        newdata_deprecated()
    }

    if (is.null(data)) {
        data <- object[["model"]]
        weights <- object[["prior.weights"]]
    } else {
        if (is.null(weights)) {
            weights <- rep(1, nrow(data))
        }
    }

    mu <- predict(object, newdata = data, type = "response", ...)
    sims <- replicate(nsim, rd_fun(mu = mu, wt = weights, scale = scale))

    attr(sims, "seed") <- RNGstate
    sims
}

#' @rdname simulate
#'
#' @export
`simulate.gamm` <- function(object, nsim = 1, seed = NULL, data = newdata,
                            weights = NULL, ..., newdata = NULL) {
    if (!is.null(newdata)) {
        newdata_deprecated()
    }

    simulate(object$gam, nsim = nsim, seed = seed, data = newdata,
             weights = weights, ...)
}

#' @rdname simulate
#'
#'
#' @export
`simulate.scam` <- function(object, nsim = 1, seed = NULL, data = newdata,
                            weights = NULL, ..., newdata = NULL) {
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

    if (!is.null(newdata)) {
        newdata_deprecated()
    }

    if (is.null(data)) {
        data <- object[["model"]]
        weights <- object[["prior.weights"]]
    } else {
        if (is.null(weights)) {
            weights <- rep(1, nrow(data))
        }
    }

    mu <- predict(object, newdata = data, type = "response", ...)

    sims <- replicate(nsim, rd_fun(mu = mu, wt = weights, scale = scale))

    attr(sims, "seed") <- RNGstate
    sims
}
