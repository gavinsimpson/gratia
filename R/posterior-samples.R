##' Draw samples from the posterior distribution of an estimated model
##'
##' @param model a fitted model of the supported types
##' @param newdata data frame; new observations at which the posterior draws
##'   from the model should be evaluated. If not supplied, the data used to fit
##'   the model will be used for `newdata`, if available in `model`.
##' @param n numeric; the number of posterior samples to return.
##' @param seed numeric; a random seed for the simulations.
##' @param scale character;
##' @param freq logical; `TRUE` to return the frequentist covariance matrix of
##'   the parameter estimators, `FALSE` to return the Bayesian posterior
##'   covariance matrix of the parameters.
##' @param unconditional logical; if `TRUE` (and `freq == FALSE`) then the
##'   Bayesian smoothing parameter uncertainty corrected covariance matrix is
##'   returned, if available.
##' @param weights numeric; a vector of prior weights. If `newdata` is null
##'   then defaults to `object[["prior.weights"]]`, otherwise a vector of ones.
##' @param ... arguments passed to other methods
##'
##' @export
`posterior_samples` <- function(model, ...) {
    UseMethod("posterior_samples")
}

##' @export
`posterior_samples.default` <- function(model, ...) {
    stop("Don't know how sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

##' @export
##' @rdname predicted_samples
`posterior_samples.gam` <- function(model, n, newdata, seed,
                                    scale = c("response","linear_predictor"),
                                    ...) {

}

##' Draw fitted values from the posterior distribution
##'
##' @inheritParams posterior_samples
##' @export
`fitted_samples` <- function(model, ...) {
    UseMethod("fitted_samples")
}

##' @export
`fitted_samples.default` <- function(model, ...) {
    stop("Don't know how sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

##' @export
##' @rdname predicted_samples
`fitted_samples.gam` <- function(model, n, newdata, seed,
                                 scale = c("response","linear_predictor"),
                                 ...) {

}

##' Draw predicted values from the posterior distribution
##'
##' Predicted values of the response drawn from the posterior distribution of
##'   fitted model, created via `simulate()` (e.g. [simulate.gam()])
##'   and returned in a tidy, long, format.
##'
##' @return A tibble (data frame) with 3 columns containing the posterior
##'   predicted values in long format. The columns are
##' * `.row` (numeric) the row of `newdata` that each posterior draw relates to,
##' * `draw` (numeric) an index, in range `1:n`, indicating which draw each row
##'     relates to,
##' * `response` (numeric) the predicted response for the indicated row of
##'     `newdata`.
##'
##' @author Gavin L. Simpson
##'
##' @inheritParams posterior_samples
##' @export
##'
##' @examples
##' library("mgcv")
##' \dontshow{set.seed(2)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' predicted_samples(m1, n = 5, seed = 42)
`predicted_samples` <- function(model, ...) {
    UseMethod("predicted_samples")
}

##' @export
`predicted_samples.default` <- function(model, ...) {
    stop("Don't know how sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

##' @export
##' @rdname predicted_samples
##' @importFrom tibble as_data_frame add_column
##' @importFrom tidyr gather
`predicted_samples.gam` <- function(model, n = 1, newdata = NULL, seed = NULL,
                                    freq = FALSE, unconditional = FALSE,
                                    weights = NULL, ...) {
    sims <- simulate(model, nsim = n, seed = seed, newdata = newdata, freq = freq,
                     unconditional = unconditional, weights = weights)
    sims <- as_data_frame(sims)
    names(sims) <- as.character(seq_len(ncol(sims)))
    sims <- add_column(sims, .row = seq_len(nrow(sims)))
    sims <- gather(sims, key = "draw", value = "response", - .row)
    sims
}
