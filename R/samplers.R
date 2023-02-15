# Functions to generate draws from the posterior distribution of model terms etc

`post_draws` <- function(n,
    method = c("gaussian", "mh", "inla", "user"),
    mu = NULL,
    sigma = NULL,
    n_cores = 1L,
    model = NULL,
    burnin = 1000,
    thin = 1,
    t_df = 40,
    rw_scale = 0.25,
    index = NULL,
    frequentist = FALSE,
    unconditional  = FALSE) {

    # what posterior sampling are we using
    method <- match.arg(method)
    betas <- switch(method,
        "gaussian" = gaussian_draws(n = n, model = model,
            n_cores = n_cores, index = index, frequentist = frequentist,
            unconditional = FALSE),
        "mh" = mh_draws(n = n, model = model, burnin = burnin,
            thin = thin, t_df = t_df, rw_scale = rw_scale, index = index),
        "inla" = .NotYetImplemented(),
        "user" = .NotYetImplemented())
    betas
}

`gaussian_draws` <- function(n, model, n_cores = 1L, index = NULL,
    frequentist = FALSE, unconditional = FALSE) {
    mu <- coef(model)
    sigma <- get_vcov(model, frequentist = frequentist,
        unconditional = unconditional)
    if (!is.null(index)) {
        mu <- mu[index]
        sigma <- sigma[index, index, drop = FALSE]
    }
    betas <- rmvn(n = n, mu = mu, sigma = sigma, ncores = n_cores)
    betas
}

#' @importFrom mgcv gam.mh
`mh_draws` <- function(n, model, burnin = 1000, thin = 1, t_df = 40,
    rw_scale = 0.25, index = NULL) {
    capture.output(betas <- gam.mh(b = model, ns = n * thin, burn = burnin,
        thin = thin, t.df = t_df, rw.scale = rw_scale))
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
