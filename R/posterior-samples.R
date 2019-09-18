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
##' @param ncores number of cores for generating random variables from a
##'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
##'   Parallelization will take place only if OpenMP is supported (but appears
##'   to work on Windows with current `R`).
##' @param ... arguments passed to other methods
##'
##' @return A tibble (data frame) with 3 columns containing the posterior
##'   predicted values in long format. The columns are
##' * `row` (integer) the row of `newdata` that each posterior draw relates to,
##' * `draw` (integer) an index, in range `1:n`, indicating which draw each row
##'     relates to,
##' * `response` (numeric) the predicted response for the indicated row of
##'     `newdata`.
##'
##' @author Gavin L. Simpson
##'
##' @export
`posterior_samples` <- function(model, ...) {
    UseMethod("posterior_samples")
}

##' @export
`posterior_samples.default` <- function(model, ...) {
    stop("Don't know how to sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

##' @export
##' @rdname posterior_samples
`posterior_samples.gam` <- function(model, n, newdata, seed,
                                    scale = c("response","linear_predictor"),
                                    freq = FALSE, unconditional = FALSE,
                                    weights = NULL, ncores = 1L, ...) {
}

##' Draw fitted values from the posterior distribution
##'
##' Expectations (fitted values) of the response drawn from the posterior
##'   distribution of fitted model, created via `simulate()` (e.g.
##'   [simulate.gam()]) and returned in a tidy, long, format.
##'
##' @return A tibble (data frame) with 3 columns containing the posterior
##'   predicted values in long format. The columns are
##' * `row` (integet) the row of `newdata` that each posterior draw relates to,
##' * `draw` (integer) an index, in range `1:n`, indicating which draw each row
##'     relates to,
##' * `response` (numeric) the predicted response for the indicated row of
##'     `newdata`.
##'
##' @author Gavin L. Simpson
##'
##' @inheritParams posterior_samples
##' @export
`fitted_samples` <- function(model, ...) {
    UseMethod("fitted_samples")
}

##' @export
`fitted_samples.default` <- function(model, ...) {
    stop("Don't know how to sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

##' @export
##' @rdname predicted_samples
##'
##' @importFrom stats vcov coef predict
##' @importFrom mvnfast rmvn
##'
##' @examples
##' suppressPackageStartupMessages(library("mgcv"))
##' \dontshow{set.seed(2)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' fitted_samples(m1, n = 5, seed = 42)
`fitted_samples.gam` <- function(model, n, newdata, seed,
                                 scale = c("response","linear_predictor"),
                                 freq = FALSE, unconditional = FALSE,
                                 ncores = 1L, ...) {
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
        newdata <- model[["model"]]
    }

    scale <- match.arg(scale)

    V <- get_vcov(model, frequentist = freq, unconditional = unconditional)
    Rbeta <- rmvn(n = n, mu = coef(model), sigma = V, ncores = ncores)
    Xp <- predict(model, newdata = newdata, type = "lpmatrix")
    sims <- Xp %*% t(Rbeta)

    if (isTRUE(identical(scale, "response"))) {
        ilink <- family(model)[["linkinv"]]
        sims <- ilink(sims)
    }

    sims <- as_data_frame(sims)
    names(sims) <- as.character(seq_len(ncol(sims)))
    sims <- add_column(sims, row = seq_len(nrow(sims)))
    sims <- gather(sims, key = "draw", value = "fitted", - row)
    sims[["draw"]] <- as.integer(sims[["draw"]])
    attr(sims, "seed") <- RNGstate
    sims
}

##' Draw predicted values from the posterior distribution
##'
##' Predicted values of the response drawn from the posterior distribution of
##'   fitted model, created via `simulate()` (e.g. [simulate.gam()])
##'   and returned in a tidy, long, format.
##'
##' @return A tibble (data frame) with 3 columns containing the posterior
##'   predicted values in long format. The columns are
##' * `row` (integet) the row of `newdata` that each posterior draw relates to,
##' * `draw` (integer) an index, in range `1:n`, indicating which draw each row
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
##' suppressPackageStartupMessages(library("mgcv"))
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
    stop("Don't know how to sample from the posterior of <",
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
    RNGstate <- attr(sims, "seed")
    sims <- as_data_frame(sims)
    names(sims) <- as.character(seq_len(ncol(sims)))
    sims <- add_column(sims, row = seq_len(nrow(sims)))
    sims <- gather(sims, key = "draw", value = "response", - row)
    sims[["draw"]] <- as.integer(sims[["draw"]])
    attr(sims, "seed") <- RNGstate
    sims
}

##' Posterior draws for individual smooths
##'
##' Returns draws from the posterior distributions of smooth functions in a GAM.
##' Useful, for example, for visualising the uncertainty in individual estimated
##' functions.
##'
##' @author Gavin L. Simpson
##'
##' @return A tibble with additional classes `"smooth_samples"` and
##'   `"posterior_samples".
##'
##'   For the `"gam"` method, the columns currently returned (not in this order)
##'   are:
##'
##' * `smooth`; character vector. Indicates the smooth function for that
##'     particular draw,
##' * `term`; character vector. Similar to `smooth`, but will contain the
##'     full label for the smooth, to differentiate factor-by smooths for
##'     example.
##' * `by_variable`; character vector. If the smooth involves a `by` term, the
##'     by variable will be named here, `NA_character_` otherwise.
##' * `row`; integer. A vector of values `seq_len(n_vals)`, repeated if
##'     `n > 1L`. Indexes the row in `newdata` for that particular draw.
##' * `draw`; integer. A vector of integer values indexing the particular
##'     posterior draw that each row belongs to.
##' * `value`; numeric. The value of smooth function for this posterior draw
##'     and covariate combination.
##' * `.xN`; numeric. A series of one or more columns containing data required
##'     for the smooth. `.x1` will always be present and contains the values of
##'     the covariate in the smooth. For example if `smooth` is `s(z)` then
##'     `.x1` will contain the values of covariate `z` at which the smooth was
##'     evaluated. Further covariates for multi-dimensional thin plate splines
##'     (e.g. `s(x, z)`) or tensor product smooths (e.g. `te(x,z,a)`) will
##'     result in variables `.x1` and `.x2`, and `.x1`, `.x2`, and `.x3`
##'     respectively, with the number (`1`, `2`, etc) representing the order
##'     in which the covariates were specified in the smooth.
##' * Additional columns will be present in the case of factor by smooths,
##'     which will contain the level for the factor named in `by_variable` for
##'     that particular posterior draw.
##'
##' @inheritParams posterior_samples
##' @export
##'
##' @examples
##' suppressPackageStartupMessages(library("mgcv"))
##' \dontshow{set.seed(2)}
##' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
##' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
##'
##' smooth_samples(m1, term = "s(x0)", n = 5, seed = 42)
##'
##' ## A factor by example (with a spurious covariate x0)
##' \dontshow{set.seed(2)}
##' dat <- gamSim(4)
##'
##' ## fit model...
##' m2 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)
##' smooth_samples(m2, n = 5, seed = 42)
`smooth_samples` <- function(model, ...) {
    UseMethod("smooth_samples")
}

##' @export
`smooth_samples.default` <- function(model, ...) {
    stop("Don't know how to sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

##' @param n_vals numeric; how many locations to evaluate the smooth at if
##'   `newdata` not supplied
##' @param term character; select which smooth's posterior to draw from.
##'   The default (`NULL`) means the posteriors of all smooths in `model`
##'   wil be sampled from. If supplied, a character vector of requested terms.
##'
##' @section Warning:
##' The set of variables returned and their order in the tibble is subject to
##' change in future versions. Don't rely on position.
##'
##' @export
##'
##' @rdname smooth_samples
##'
##' @inheritParams posterior_samples
##'
##' @importFrom mvnfast rmvn
##' @importFrom dplyr bind_rows starts_with
##' @importFrom tibble as_tibble add_column
##' @importFrom tidyr gather
##' @importFrom mgcv PredictMat
`smooth_samples.gam` <- function(model, term = NULL, n = 1, newdata = NULL,
                                 seed = NULL, freq = FALSE, unconditional = FALSE,
                                 ncores = 1L, n_vals = 200, ...) {
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

    S <- smooths(model)             # vector of smooth labels - "s(x)"

    if (!is.null(term)) {
        take <- which_smooths(model, term)
        S <- S[take]
    }

    need_newdata <- FALSE
    if (is.null(newdata)) {
        need_newdata <- TRUE
    }

    V <- get_vcov(model, frequentist = freq, unconditional = unconditional)

    ## Xp <- predict(model, newdata = newdata, type = "lpmatrix")
    coefs <- coef(model)

    sims <- data_names <- vector('list', length = length(S))
    for (i in seq_along(S)) {
        if (need_newdata) {
            newdata <- smooth_data(model, id = i, n = n_vals, offset = NULL) # FIXME: should offset be NULL?
            ## I don't think we need offset here as that really just shifts the response around
        }
        sm  <- get_smooths_by_id(model, i)[[1L]]
        idx <- smooth_coefs(sm)
        Xp <- PredictMat(sm, data = newdata)
        betas <- rmvn(n = n, mu = coefs[idx], sigma = V[idx, idx, drop=FALSE],
                      ncores = ncores)
        simu <- Xp %*% t(betas)
        colnames(simu) <- paste0("..V", seq_len(NCOL(simu)))
        simu <- as_tibble(simu)
        ## names(simu) <- paste0("..V", seq_len(NCOL(simu)))
        is_fac_by <- is_factor_by_smooth(sm)
        if (is_fac_by) {
            simu <- add_factor_by_data(simu, n = n_vals,
                                       by_name = by_variable(sm),
                                       by_data = newdata, before = 1L)
        } else {
            simu <- add_column(simu,
                               by_variable = rep(NA_character_, times = n_vals))
        }
        simu <- add_smooth_var_data(simu, smooth_variable(sm), newdata)
        sims[[i]] <- simu
        summ_names <- names(newdata[!vapply(newdata, is.factor, logical(1))])
        names(summ_names) <- paste0(".x", seq_along(summ_names))
        data_names[[i]] <- summ_names
    }

    sims <- do.call("bind_rows", sims)
    sims <- add_column(sims,
                       smooth = rep(unlist(lapply(strsplit(S, ":"), `[`, 1L)),
                                    each = n_vals),
                       .before = 1L)
    sims <- add_column(sims, term = rep(S, each = n_vals), .before = 2L)
    sims <- add_column(sims, row = rep(seq_len(nrow(newdata)), times = length(S)))
    sims <- gather(sims, key = "draw", value = "value", dplyr::starts_with("..V"))
    sims[["draw"]] <- as.integer(sub("\\.\\.V", "", sims[["draw"]]))
    attr(sims, "seed") <- RNGstate
    attr(sims, "data_names") <- setNames(data_names, nm = S)
    ## add classes
    class(sims) <- c("smooth_samples", "posterior_samples", class(sims))
    sims
}
