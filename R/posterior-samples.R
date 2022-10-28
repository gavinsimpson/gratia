#' Draw samples from the posterior distribution of an estimated model
#'
#' @param model a fitted model of the supported types
#' @param data  data frame; new observations at which the posterior draws
#'   from the model should be evaluated. If not supplied, the data used to fit
#'   the model will be used for `data`, if available in `model`.
#' @param n numeric; the number of posterior samples to return.
#' @param seed numeric; a random seed for the simulations.
#' @param scale character;
#' @param freq logical; `TRUE` to use the frequentist covariance matrix of
#'   the parameter estimators, `FALSE` to use the Bayesian posterior
#'   covariance matrix of the parameters.
#' @param unconditional logical; if `TRUE` (and `freq == FALSE`) then the
#'   Bayesian smoothing parameter uncertainty corrected covariance matrix is
#'   used, if available.
#' @param weights numeric; a vector of prior weights. If `data` is null
#'   then defaults to `object[["prior.weights"]]`, otherwise a vector of ones.
#' @param ncores number of cores for generating random variables from a
#'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
#'   Parallelization will take place only if OpenMP is supported (but appears
#'   to work on Windows with current `R`).
#' @param ... arguments passed to other methods. For `fitted_samples()`, these
#'   are passed on to `predict.gam()`.
#' @param newdata Deprecated: use `data` instead.
#'
#' @return A tibble (data frame) with 3 columns containing the posterior
#'   predicted values in long format. The columns are
#' * `row` (integer) the row of `data` that each posterior draw relates to,
#' * `draw` (integer) an index, in range `1:n`, indicating which draw each row
#'     relates to,
#' * `response` (numeric) the predicted response for the indicated row of
#'     `data`.
#'
#' @author Gavin L. Simpson
#'
#' @export
`posterior_samples` <- function(model, ...) {
    UseMethod("posterior_samples")
}

#' @export
`posterior_samples.default` <- function(model, ...) {
    stop("Don't know how to sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

#' @export
#' @rdname posterior_samples
`posterior_samples.gam` <- function(model, n, data = newdata, seed,
                                    scale = c("response","linear_predictor"),
                                    freq = FALSE, unconditional = FALSE,
                                    weights = NULL, ncores = 1L, ...,
                                    newdata = NULL) {
    .NotYetImplemented()
}

#' Draw fitted values from the posterior distribution
#'
#' Expectations (fitted values) of the response drawn from the posterior
#' distribution of fitted model using a Gaussian approximation to the
#' posterior.
#'
#' @param method character; the method used to generate samples from the
#'   posterior distribution of the model. `"gaussian"`, the default, uses a
#'   Gaussian approximation to the posterior. `"mh"` uses a simple Metropolis
#'   Hastings sampler, while `"inla"` uses a variant of Integrated Nested
#'   Laplace Approximation due to Wood (2019). Currently, the only available
#'   option is `"gaussian"`.
#'
#' @return A tibble (data frame) with 3 columns containing the posterior
#'   predicted values in long format. The columns are
#' * `row` (integer) the row of `data` that each posterior draw relates to,
#' * `draw` (integer) an index, in range `1:n`, indicating which draw each row
#'     relates to,
#' * `response` (numeric) the predicted response for the indicated row of
#'     `data`.
#'
#' @author Gavin L. Simpson
#'
#' @inheritParams posterior_samples
#'
#' @references
#'
#' Wood, S.N., (2020). Simplified integrated nested Laplace approximation.
#'   *Biometrika* **107**, 223--230. \doi{10.1093/biomet/asz044}
#'
#' @export
`fitted_samples` <- function(model, ...) {
    UseMethod("fitted_samples")
}

#' @export
`fitted_samples.default` <- function(model, ...) {
    stop("Don't know how to sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

#' @export
#'
#' @rdname fitted_samples
#'
#' @importFrom stats vcov coef predict
#' @importFrom mvnfast rmvn
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' fs <- fitted_samples(m1, n = 5, seed = 42)
#' \donttest{
#' fs
#' }
#' \dontshow{options(op)}
`fitted_samples.gam` <- function(model, n = 1, data = newdata, seed,
                                 scale = c("response", "linear_predictor"),
                                 method = c("gaussian", "mh", "inla"),
                                 freq = FALSE, unconditional = FALSE,
                                 ncores = 1L, ..., newdata = NULL) {
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

    if (!is.null(newdata)) {
        newdata_deprecated()
    }

    if (is.null(data)) {
        data <- model[["model"]]
    }

    scale <- match.arg(scale)

    method <- match.arg(method)
    if (! method %in% c("gaussian")) {
        warning("Only Gaussian approximation is currently available.")
    }

    V <- get_vcov(model, frequentist = freq, unconditional = unconditional)
    Rbeta <- rmvn(n = n, mu = coef(model), sigma = V, ncores = ncores)
    ## don't need to pass freq, unconditional here as that is done for V
    Xp <- predict(model, newdata = data, type = "lpmatrix", ...)
    sims <- Xp %*% t(Rbeta)

    if (isTRUE(identical(scale, "response"))) {
        ilink <- inv_link(model, parameter = "location")
        sims <- ilink(sims)
    }

    colnames(sims) <- paste0(".V", seq_len(NCOL(sims)))
    sims <- as_tibble(sims)
    names(sims) <- as.character(seq_len(ncol(sims)))
    sims <- add_column(sims, row = seq_len(nrow(sims)))
    sims <- gather(sims, key = "draw", value = "fitted", - row)
    sims[["draw"]] <- as.integer(sims[["draw"]])
    attr(sims, "seed") <- RNGstate
    ## add classes
    class(sims) <- c("fitted_samples", "posterior_samples", class(sims))
    sims
}

#' Draw new response values from the conditional distribution of the response
#'
#' Predicted values of the response (new response data) are drawn from the
#' fitted model, created via `simulate()` (e.g. [simulate.gam()]) and returned
#' in a tidy, long, format. These predicted values do not include the
#' uncertainty in the estimated model; they are simply draws from the
#' conditional distribution of the response.
#'
#' @return A tibble (data frame) with 3 columns containing the posterior
#'   predicted values in long format. The columns are
#' * `row` (integer) the row of `data` that each posterior draw relates to,
#' * `draw` (integer) an index, in range `1:n`, indicating which draw each row
#'     relates to,
#' * `response` (numeric) the predicted response for the indicated row of
#'     `data`.
#'
#' @author Gavin L. Simpson
#'
#' @inheritParams posterior_samples
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(pillar.sigfig = 3, cli.unicode = FALSE)
#' }
#' dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' predicted_samples(m, n = 5, seed = 42)
#'
#' ## Can pass arguments to predict.gam()
#' \dontshow{set.seed(6791)}
#' newd <- data.frame(x0 = runif(10), x1 = runif(10), x2 = runif(10),
#'                    x3 = runif(10))
#'
#' ## Exclude s(x2)
#' predicted_samples(m, n = 5, newd, exclude = "s(x2)", seed = 25)
#'
#' ## Exclude s(x1)
#' predicted_samples(m, n = 5, newd, exclude = "s(x1)", seed = 25)
#'
#' ## Select which terms --- result should be the same as previous
#' ## but note that we have to include any parametric terms, including the
#' ## constant term
#' predicted_samples(m, n = 5, newd, seed = 25,
#'                   terms = c("Intercept", "s(x0)", "s(x2)", "s(x3)"))
#' \dontshow{options(op)}
`predicted_samples` <- function(model, ...) {
    UseMethod("predicted_samples")
}

#' @export
`predicted_samples.default` <- function(model, ...) {
    stop("Don't know how to sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

#' @export
#' @rdname predicted_samples
#' @importFrom tibble as_data_frame add_column
#' @importFrom tidyr gather
`predicted_samples.gam` <- function(model, n = 1, data = newdata, seed = NULL,
                                    weights = NULL, ..., newdata = NULL) {
    if (!is.null(newdata)) {
        newdata_deprecated()
    }

    sims <- simulate(model, nsim = n, seed = seed, newdata = data,
                     weights = weights, ...)
    RNGstate <- attr(sims, "seed")
    colnames(sims) <- paste0(".V", seq_len(NCOL(sims)))
    sims <- as_tibble(sims)
    names(sims) <- as.character(seq_len(ncol(sims)))
    sims <- add_column(sims, row = seq_len(nrow(sims)))
    sims <- gather(sims, key = "draw", value = "response", - row)
    sims[["draw"]] <- as.integer(sims[["draw"]])
    attr(sims, "seed") <- RNGstate
    ## add classes
    class(sims) <- c("predicted_samples", "posterior_samples", class(sims))
    sims
}

#' Posterior draws for individual smooths
#'
#' Returns draws from the posterior distributions of smooth functions in a GAM.
#' Useful, for example, for visualising the uncertainty in individual estimated
#' functions.
#'
#' @author Gavin L. Simpson
#'
#' @return A tibble with additional classes `"smooth_samples"` and
#'   `"posterior_samples".
#'
#'   For the `"gam"` method, the columns currently returned (not in this order)
#'   are:
#'
#' * `smooth`; character vector. Indicates the smooth function for that
#'     particular draw,
#' * `term`; character vector. Similar to `smooth`, but will contain the
#'     full label for the smooth, to differentiate factor-by smooths for
#'     example.
#' * `by_variable`; character vector. If the smooth involves a `by` term, the
#'     by variable will be named here, `NA_character_` otherwise.
#' * `row`; integer. A vector of values `seq_len(n_vals)`, repeated if
#'     `n > 1L`. Indexes the row in `data` for that particular draw.
#' * `draw`; integer. A vector of integer values indexing the particular
#'     posterior draw that each row belongs to.
#' * `value`; numeric. The value of smooth function for this posterior draw
#'     and covariate combination.
#' * `.xN`; numeric. A series of one or more columns containing data required
#'     for the smooth. `.x1` will always be present and contains the values of
#'     the covariate in the smooth. For example if `smooth` is `s(z)` then
#'     `.x1` will contain the values of covariate `z` at which the smooth was
#'     evaluated. Further covariates for multi-dimensional thin plate splines
#'     (e.g. `s(x, z)`) or tensor product smooths (e.g. `te(x,z,a)`) will
#'     result in variables `.x1` and `.x2`, and `.x1`, `.x2`, and `.x3`
#'     respectively, with the number (`1`, `2`, etc) representing the order
#'     in which the covariates were specified in the smooth.
#' * Additional columns will be present in the case of factor by smooths,
#'     which will contain the level for the factor named in `by_variable` for
#'     that particular posterior draw.
#'
#' @inheritParams posterior_samples
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{op <- options(cli.unicode = FALSE, pillar.sigfig = 3)}
#' dat <- data_sim("eg1", n = 400, seed = 2)
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
#'
#' sms <- smooth_samples(m1, term = "s(x0)", n = 5, seed = 42)
#' \donttest{
#' sms
#' }
#'
#' ## A factor by example (with a spurious covariate x0)
#' dat <- data_sim("eg4", n = 1000, seed = 2)
#'
#' ## fit model...
#' m2 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)
#' sms <- smooth_samples(m2, n = 5, seed = 42)
#' draw(sms)
#' \dontshow{options(op)}
`smooth_samples` <- function(model, ...) {
    UseMethod("smooth_samples")
}

#' @export
`smooth_samples.default` <- function(model, ...) {
    stop("Don't know how to sample from the posterior of <",
         class(model)[[1L]], ">", .call = FALSE)
}

#' @param n_vals numeric; how many locations to evaluate the smooth at if
#'   `data` not supplied
#' @param term character; select which smooth's posterior to draw from.
#'   The default (`NULL`) means the posteriors of all smooths in `model`
#'   wil be sampled from. If supplied, a character vector of requested terms.
#'
#' @section Warning:
#' The set of variables returned and their order in the tibble is subject to
#' change in future versions. Don't rely on position.
#'
#' @export
#'
#' @rdname smooth_samples
#'
#' @inheritParams posterior_samples
#'
#' @importFrom mvnfast rmvn
#' @importFrom dplyr bind_rows starts_with
#' @importFrom tibble as_tibble add_column
#' @importFrom tidyr gather
#' @importFrom mgcv PredictMat
`smooth_samples.gam` <- function(model, term = NULL, n = 1, data = newdata,
                                 seed = NULL, freq = FALSE,
                                 unconditional = FALSE,
                                 ncores = 1L, n_vals = 200, ...,
                                 newdata = NULL) {
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
    take <- seq_along(S)            # in default case 1,2,3,..,n smooths
    if (!is.null(term)) {
        take <- which_smooths(model, term)
        S <- S[take]
    }

    # At least for now, don't work for random effect smooths
    # do we have ranef smooths?
    re_sms <- vapply(get_smooths_by_id(model, take), FUN = is_re_smooth,
                     FUN.VALUE = logical(1L))
    if (any(re_sms)) {
        message("\nRandom effect smooths not currently supported.\nIgnoring:",
                " <", paste(S[re_sms], collapse = ", "), ">\n")
        S <- S[!re_sms]
        take <- take[!re_sms]
    }

    # do we have any remaining terms?
    if (length(S) < 1L) {
        stop("No smooths left that can be sampled from.")
    }

    if (!is.null(newdata)) {
        newdata_deprecated()
    }

    need_data <- FALSE
    if (is.null(data)) {
        need_data <- TRUE
    }

    V <- get_vcov(model, frequentist = freq, unconditional = unconditional)

    coefs <- coef(model)

    sims <- data_names <- vector('list', length = length(S))
    for (i in seq_along(S)) {
        if (need_data) {
            # FIXME: should offset be NULL?
            data <- smooth_data(model, id = take[i], n = n_vals,
                                offset = NULL)
            ## I don't think we need offset here as that really just shifts the
            ## response around
        }
        sm  <- get_smooths_by_id(model, take[i])[[1L]]
        idx <- smooth_coef_indices(sm)
        Xp <- PredictMat(sm, data = data)
        betas <- rmvn(n = n, mu = coefs[idx], sigma = V[idx, idx, drop=FALSE],
                      ncores = ncores)
        simu <- Xp %*% t(betas)
        colnames(simu) <- paste0("..V", seq_len(NCOL(simu)))
        simu <- as_tibble(simu)
        nr_simu <- nrow(simu)
        is_fac_by <- is_factor_by_smooth(sm)
        if (is_fac_by) {
            simu <- add_factor_by_data(simu, n = n_vals,
                                       by_name = by_variable(sm),
                                       by_data = data, before = 1L)
        } else {
            simu <- add_column(simu,
                               by_variable = rep(NA_character_,
                                                 times = nr_simu))
        }
    #   # add on spline type info
        sm_type <- smooth_type(sm)
        simu <- add_column(simu, type = rep(sm_type, nr_simu),
                           .after = 1L)
        simu <- add_smooth_var_data(simu, smooth_variable(sm), data)
        sims[[i]] <- simu
        summ_names <- names(data[!vapply(data, is.factor, logical(1))])
        names(summ_names) <- paste0(".x", seq_along(summ_names))
        data_names[[i]] <- summ_names
    }

    sims <- do.call("bind_rows", sims)
    sims <- add_column(sims,
                       smooth = rep(unlist(lapply(strsplit(S, ":"), `[`, 1L)),
                                    each = nr_simu),
                       .before = 1L)
    sims <- add_column(sims, term = rep(S, each = nr_simu), .before = 2L)
    sims <- add_column(sims, row = rep(seq_len(nrow(data)),
                       times = length(S)))
    sims <- gather(sims, key = "draw", value = "value",
                   dplyr::starts_with("..V"))
    sims[["draw"]] <- as.integer(sub("\\.\\.V", "", sims[["draw"]]))
    attr(sims, "seed") <- RNGstate
    attr(sims, "data_names") <- setNames(data_names, nm = S)
    ## add classes
    class(sims) <- c("smooth_samples", "posterior_samples", class(sims))
    sims
}
