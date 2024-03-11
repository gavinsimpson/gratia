#' Draw samples from the posterior distribution of an estimated model
#'
#' @param model a fitted model of the supported types
#' @param data  data frame; new observations at which the posterior draws
#'   from the model should be evaluated. If not supplied, the data used to fit
#'   the model will be used for `data`, if available in `model`.
#' @param n numeric; the number of posterior samples to return.
#' @param seed numeric; a random seed for the simulations.
#' @param freq logical; `TRUE` to use the frequentist covariance matrix of
#'   the parameter estimators, `FALSE` to use the Bayesian posterior
#'   covariance matrix of the parameters.
#' @param unconditional logical; if `TRUE` (and `freq == FALSE`) then the
#'   Bayesian smoothing parameter uncertainty corrected covariance matrix is
#'   used, if available.
#' @param weights numeric; a vector of prior weights. If `data` is null
#'   then defaults to `object[["prior.weights"]]`, otherwise a vector of ones.
#' @param n_cores number of cores for generating random variables from a
#'   multivariate normal distribution. Passed to [mvnfast::rmvn()].
#'   Parallelization will take place only if OpenMP is supported (but appears
#'   to work on Windows with current `R`).
#' @param method character; which method should be used to draw samples from
#'   the posterior distribution. `"gaussian"` uses a Gaussian (Laplace)
#'   approximation to the posterior. `"mh"` uses a Metropolis Hastings sampler
#'   that alternates t proposals with proposals based on a shrunken version of
#'   the posterior covariance matrix. `"inla"` uses a variant of Integrated
#'   Nested Laplace Approximation due to Wood (2019), (currently not
#'   implemented). `"user"` allows for user-supplied posterior draws
#'   (currently not implemented).
#' @param burnin numeric; number of samples to discard as the burnin draws.
#'   Only used with `method = "mh"`.
#' @param thin numeric; the number of samples to skip when taking `n` draws.
#'   Results in `thin * n` draws from the posterior being taken. Only used with
#'   `method = "mh"`.
#' @param t_df numeric; degrees of freedome for t distribution proposals. Only
#'   used with `method = "mh"`.
#' @param rw_scale numeric; Factor by which to scale posterior covariance
#'   matrix when generating random walk proposals. Negative or non finite to
#'   skip the random walk step. Only used with `method = "mh"`.
#' @param ... arguments passed to other methods. For `fitted_samples()`, these
#'   are passed on to [predict.gam()]. For `posterior_samples()` these are
#'   passed on to `fitted_samples()`. For `predicted_samples()` these are
#'   passed on to the relevant `simulate()` method.
#' @param newdata Deprecated: use `data` instead.
#' @param ncores Deprecated; use `n_cores` instead. The number of cores for
#'   generating random variables from a multivariate normal distribution.
#'   Passed to [mvnfast::rmvn()]. Parallelization will take place only if
#'   OpenMP is supported (but appears to work on Windows with current `R`).
#' @param draws matrix; user supplied posterior draws to be used when
#'   `method = "user"`.
#'
#' @details
#' # Note
#'
#' Models with offset terms supplied via the `offset` argument to
#' [mgcv::gam()] etc. are ignored by [mgcv::predict.gam()]. As such, this
#' kind of offset term is also ignored by `posterior_samples()`. Offset terms
#' that are included in the model formula supplied to [mgcv::gam()] etc are
#' not ignored and the posterior samples produced will reflect those offset
#' term values. This has the side effect of requiring any new data values
#' provided to `posterior_samples()` via the `data` argument must include the
#' offset variable.
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
#' @references
#'
#' Wood, S.N., (2020). Simplified integrated nested Laplace approximation.
#'   *Biometrika* **107**, 223--230. \doi{10.1093/biomet/asz044}
#'
#' @export
`posterior_samples` <- function(model, ...) {
  UseMethod("posterior_samples")
}

#' @export
`posterior_samples.default` <- function(model, ...) {
  stop("Don't know how to sample from the posterior of <",
    class(model)[[1L]], ">",
    .call = FALSE
  )
}

#' @export
#' @rdname posterior_samples
`posterior_samples.gam` <- function(
    model, n, data = newdata, seed = NULL,
    method = c("gaussian", "mh", "inla", "user"),
    n_cores = 1, burnin = 1000, thin = 1, t_df = 40, rw_scale = 0.25,
    freq = FALSE, unconditional = FALSE,
    weights = NULL, draws = NULL, ...,
    newdata = NULL,
    ncores = NULL) {
  # generate new response data from the model including the uncertainty in
  # the model.

  # start my getting draws of expectation
  sim_eta <- fitted_samples(model,
    n = n, data = data, seed = seed,
    scale = "response", method = method, n_cores = n_cores, burnin = burnin,
    thin = thin, t_df = t_df, rw_scale = rw_scale, freq = freq,
    unconditional = unconditional, weights = weights, newdata = newdata,
    ncores = ncores, draws = draws, ...
  )

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
  rd_fun <- get_family_rd(model)

  ## dispersion or scale variable for simulation
  scale <- model[["sig2"]]
  if (is.null(scale)) {
    scale <- summary(model)[["dispersion"]]
  }

  if (!is.null(newdata)) {
    newdata_deprecated()
  }

  if (is.null(data)) {
    # data <- model[["model"]]
    weights <- model[["prior.weights"]]
  } else {
    if (is.null(weights)) {
      weights <- rep(1, nrow(data))
    }
  }

  # need to extend weights by number of draws
  weights <- rep(weights, times = n)
  # scale <- rep(scale, times = n)

  # replace fitted with
  sim_eta <- sim_eta |>
    mutate(.response = rd_fun(
      mu = .data$.fitted, wt = weights,
      scale = scale
    )) |>
    select(all_of(c(".row", ".draw", ".response")))

  ## add classes
  class(sim_eta) <- c("posterior_samples", class(sim_eta))
  sim_eta
}

#' Draw fitted values from the posterior distribution
#'
#' Expectations (fitted values) of the response drawn from the posterior
#' distribution of fitted model using a Gaussian approximation to the
#' posterior or a simple Metropolis Hastings sampler.
#'
#' @param scale character; what scale should the fitted values be returned on?
#'   `"linear predictor"` is a synonym for `"link"` if you prefer that
#'   terminology.
#'
#' @details
#' # Note
#'
#' Models with offset terms supplied via the `offset` argument to
#' [mgcv::gam()] etc. are ignored by [mgcv::predict.gam()]. As such, this
#' kind of offset term is also ignored by `posterior_samples()`. Offset terms
#' that are included in the model formula supplied to [mgcv::gam()] etc are
#' not ignored and the posterior samples produced will reflect those offset
#' term values. This has the side effect of requiring any new data values
#' provided to `posterior_samples()` via the `data` argument must include the
#' offset variable.
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
    class(model)[[1L]], ">",
    .call = FALSE
  )
}

#' @export
`fitted_samples.gamm` <- function(model, ...) {
  fitted_samples(model$gam, ...)
}

#' @export
`fitted_samples.scam` <- function(model, ...) {
  fitted_samples.gam(model, ...)
}

#' @export
#'
#' @rdname fitted_samples
#'
#' @importFrom stats vcov coef predict
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
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
#'
#' # can generate own set of draws and use them
#' drws <- generate_draws(m1, n = 2, seed = 24)
#' fs2 <- fitted_samples(m1, method = "user", draws = drws)
#' \donttest{
#' fs2
#' }
#' \dontshow{
#' options(op)
#' }
`fitted_samples.gam` <- function(
    model, n = 1, data = newdata, seed = NULL,
    scale = c("response", "linear_predictor"),
    method = c("gaussian", "mh", "inla", "user"),
    n_cores = 1, burnin = 1000, thin = 1, t_df = 40, rw_scale = 0.25,
    freq = FALSE, unconditional = FALSE, draws = NULL,
    ..., newdata = NULL, ncores = NULL) {
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

  if (!is.null(ncores)) {
    message("Argument `ncores` is deprecated. Use `n_cores` instead.")
    n_cores <- ncores
  }

  if (is.null(data)) {
    data <- model[["model"]]
  }

  scale <- match.arg(scale)

  method <- match.arg(method)

  # get posterior draws
  betas <- post_draws(
    n = n, method = method,
    n_cores = n_cores, model = model,
    burnin = burnin, thin = thin, t_df = t_df, rw_scale = rw_scale,
    index = NULL, frequentist = freq, unconditional = unconditional,
    draws = draws, seed = seed
  )
  ## don't need to pass freq, unconditional here as that is done for V
  Xp <- predict(model, newdata = data, type = "lpmatrix", ...)
  sims <- Xp %*% t(betas)
  # handle the offset if present; it is an attribute on the Xp matrix
  m_offset <- attr(Xp, "model.offset")
  if (!is.null(m_offset)) {
    sims <- sims + m_offset
  }

  if (isTRUE(identical(scale, "response"))) {
    ilink <- inv_link(model, parameter = "location")
    sims <- ilink(sims)
  }

  colnames(sims) <- paste0(".V", seq_len(NCOL(sims)))
  sims <- as_tibble(sims)
  names(sims) <- as.character(seq_len(ncol(sims)))
  sims <- add_column(sims, .row = seq_len(nrow(sims)))
  sims <- gather(sims, key = ".draw", value = ".fitted", -.row) |>
    mutate(.draw = as.integer(.data$.draw))
  attr(sims, "seed") <- RNGstate
  ## add classes
  class(sims) <- c("fitted_samples", class(sims))
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
#' \dontshow{
#' set.seed(6791)
#' }
#' newd <- data.frame(
#'   x0 = runif(10), x1 = runif(10), x2 = runif(10),
#'   x3 = runif(10)
#' )
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
#' predicted_samples(m,
#'   n = 5, newd, seed = 25,
#'   terms = c("Intercept", "s(x0)", "s(x2)", "s(x3)")
#' )
#' \dontshow{
#' options(op)
#' }
`predicted_samples` <- function(model, ...) {
  UseMethod("predicted_samples")
}

#' @export
`predicted_samples.default` <- function(model, ...) {
  stop("Don't know how to sample from the posterior of <",
    class(model)[[1L]], ">",
    .call = FALSE
  )
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

  sims <- simulate(model,
    nsim = n, seed = seed, data = data,
    weights = weights, ...
  )
  RNGstate <- attr(sims, "seed")
  colnames(sims) <- paste0(".V", seq_len(NCOL(sims)))
  sims <- as_tibble(sims)
  names(sims) <- as.character(seq_len(ncol(sims)))
  sims <- add_column(sims, .row = seq_len(nrow(sims)))
  sims <- gather(sims, key = ".draw", value = ".response", -.row) |>
    mutate(.draw = as.integer(.data$.draw))
  attr(sims, "seed") <- RNGstate
  ## add classes
  class(sims) <- c("predicted_samples", class(sims))
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
#' * `.smooth`; character vector. Indicates the smooth function for that
#'     particular draw,
#' * `.term`; character vector. Similar to `smooth`, but will contain the
#'     full label for the smooth, to differentiate factor-by smooths for
#'     example.
#' * `.by`; character vector. If the smooth involves a `by` term, the
#'     by variable will be named here, `NA_character_` otherwise.
#' * `.row`; integer. A vector of values `seq_len(n_vals)`, repeated if
#'     `n > 1L`. Indexes the row in `data` for that particular draw.
#' * `.draw`; integer. A vector of integer values indexing the particular
#'     posterior draw that each row belongs to.
#' * `.value`; numeric. The value of smooth function for this posterior draw
#'     and covariate combination.
#' * `xxx`; numeric. A series of one or more columns containing data required
#'     for the smooth, named as per the variables involved in the respective
#'     smooth.
#' * Additional columns will be present in the case of factor by smooths,
#'     which will contain the level for the factor named in `by_variable` for
#'     that particular posterior draw.
#'
#' @inheritParams posterior_samples
#' @export
#'
#' @examples
#' load_mgcv()
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 3)
#' }
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
#' \dontshow{
#' options(op)
#' }
`smooth_samples` <- function(model, ...) {
  UseMethod("smooth_samples")
}

#' @export
`smooth_samples.default` <- function(model, ...) {
  stop("Don't know how to sample from the posterior of <",
    class(model)[[1L]], ">",
    .call = FALSE
  )
}

#' @param n_vals numeric; how many locations to evaluate the smooth at if
#'   `data` not supplied
#' @param term character; select which smooth's posterior to draw from.
#'   The default (`NULL`) means the posteriors of all smooths in `model`
#'   wil be sampled from. If supplied, a character vector of requested terms.
#' @param rng_per_smooth logical; if TRUE, the behaviour of gratia version
#' 0.8.1 or earlier is used, whereby a separate call the the random number
#' generator (RNG) is performed for each smooth. If FALSE, a single call to the
#' RNG is performed for all model parameters
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
#' @inheritParams draw.gam
#'
#' @importFrom mvnfast rmvn
#' @importFrom dplyr bind_rows starts_with
#' @importFrom tibble as_tibble add_column
#' @importFrom tidyr pivot_longer
#' @importFrom mgcv PredictMat
#' @importFrom tidyselect any_of
#' @importFrom lifecycle deprecated is_present
`smooth_samples.gam` <- function(
    model,
    select = NULL,
    term = deprecated(),
    n = 1,
    data = newdata,
    method = c("gaussian", "mh", "inla", "user"),
    seed = NULL,
    freq = FALSE,
    unconditional = FALSE,
    n_cores = 1L,
    n_vals = 200,
    burnin = 1000,
    thin = 1,
    t_df = 40,
    rw_scale = 0.25,
    rng_per_smooth = FALSE,
    draws = NULL,
    partial_match = NULL,
    ...,
    newdata = NULL,
    ncores = NULL) {
  if (lifecycle::is_present(na.rm)) {
    lifecycle::deprecate_warn("0.8.9.9", "smooth_samples(..., term)",
      "smooth_samples(..., select)")
    select <- term
  }
  # smooth_samples begins
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

  sms <- smooths(model) # vector of smooth labels - "s(x)"
  take <- seq_along(sms) # in default case 1,2,3,..,n smooths
  if (!is.null(select)) {
    select <- check_user_select_smooths(sms, select = select,
      partial_match = partial_match, model_name = deparse(substitute(model)))
    # take <- which_smooths(model, term)
    take <- which(select) # need this as an id vector for things later
    sms <- sms[select] # S <- S[take]
  }

  # At least for now, don't work for random effect smooths
  # do we have ranef smooths?
  re_sms <- vapply(get_smooths_by_id(model, take),
    FUN = is_re_smooth,
    FUN.VALUE = logical(1L)
  )
  if (any(re_sms)) {
    message(
      "\nRandom effect smooths not currently supported.\nIgnoring:",
      " <", paste(sms[re_sms], collapse = ", "), ">\n"
    )
    sms <- sms[!re_sms]
    take <- take[!re_sms]
  }

  # do we have any remaining terms?
  if (length(sms) < 1L) {
    stop("No smooths left that can be sampled from.")
  }

  if (!is.null(newdata)) {
    newdata_deprecated()
  }

  if (!is.null(ncores)) {
    message("Argument `ncores` is deprecated. Use `n_cores` instead.")
    n_cores <- ncores
  }

  need_data <- FALSE
  if (is.null(data)) {
    need_data <- TRUE
  }

  # what posterior sampling are we using
  method <- match.arg(method)

  # get posterior draws - call this once for all parameters
  if (isFALSE(rng_per_smooth)) {
    betas <- post_draws(
      n = n, method = method,
      n_cores = n_cores, model = model,
      burnin = burnin, thin = thin, t_df = t_df, rw_scale = rw_scale,
      index = NULL, frequentist = freq, unconditional = unconditional,
      draws = draws, seed = seed,
    )
  }

  sims <- data_names <- vector("list", length = length(S))
  for (i in seq_along(sms)) {
    if (need_data) {
      # FIXME: should offset be NULL?
      data <- smooth_data(model,
        id = take[i], n = n_vals,
        offset = NULL
      )
      # I don't think we need offset here as that really just shifts the
      # response around
    }
    sm <- get_smooths_by_id(model, take[i])[[1L]]
    idx <- smooth_coef_indices(sm)
    Xp <- PredictMat(sm, data = data)
    # get posterior draws - use old behaviour? TRUE is yes
    simu <- if (isTRUE(rng_per_smooth)) {
      betas <- post_draws(
        n = n, method = method,
        n_cores = n_cores, model = model,
        burnin = burnin, thin = thin, t_df = t_df, rw_scale = rw_scale,
        index = idx, frequentist = freq, unconditional = unconditional,
        seed = seed
      )
      Xp %*% t(betas)
    } else {
      Xp %*% t(betas[, idx, drop = FALSE])
    }
    colnames(simu) <- paste0("..V", seq_len(NCOL(simu)))
    simu <- as_tibble(simu)
    nr_simu <- nrow(simu)
    simu <- add_by_data(simu,
      by_name = by_variable(sm), by_data = data,
      before = 1L
    )
    # add on spline type info
    sm_type <- smooth_type(sm)
    simu <- add_column(simu,
      .smooth = rep(
        unlist(lapply(strsplit(sms[[i]], ":"), `[`, 1L)),
        nr_simu
      ),
      .term = rep(sms[[i]], each = nr_simu),
      .type = rep(sm_type, nr_simu),
      .row = seq_len(nr_simu),
      .after = 0L
    )
    simu <- bind_cols(simu, data[smooth_variable(sm)])
    simu <- pivot_longer(simu,
      cols = dplyr::starts_with("..V"),
      names_to = ".draw", values_to = ".value",
      names_transform = \(x) as.integer(sub("\\.\\.V", "", x))
    )
    simu <- relocate(simu, any_of(names(data)), .after = last_col()) |>
      relocate(".by", .after = 3L)
    ## nest all columns with varying data
    simu <- nest(simu, data = all_of(c(
      ".row", ".draw", ".value",
      vars_in_smooth(sm)
    )))
    sims[[i]] <- simu
  }

  sims <- bind_rows(sims)
  sims <- unnest(sims, all_of("data"))
  attr(sims, "seed") <- RNGstate
  ## add classes
  class(sims) <- c("smooth_samples", class(sims))
  sims
}

#' @export
#' @importFrom mvnfast rmvn
#' @importFrom dplyr bind_rows starts_with
#' @importFrom tibble as_tibble add_column
#' @importFrom tidyr pivot_longer
#' @importFrom mgcv PredictMat
#' @importFrom tidyselect any_of
#' @importFrom lifecycle deprecated is_present
`smooth_samples.scam` <- function(
    model,
    select = NULL,
    term = deprecated(),
    n = 1, data = newdata,
    method = c("gaussian", "mh", "inla", "user"), seed = NULL,
    freq = FALSE, unconditional = FALSE, n_cores = 1L, n_vals = 200,
    burnin = 1000, thin = 1, t_df = 40, rw_scale = 0.25, rng_per_smooth = FALSE,
    draws = NULL, mvn_method = c("mvnfast", "mgcv"),
    partial_match = NULL, ...,
    newdata = NULL, ncores = NULL) {
  if (lifecycle::is_present(na.rm)) {
    lifecycle::deprecate_warn("0.8.9.9", "smooth_samples(..., term)",
      "smooth_samples(..., select)")
    select <- term
  }
  # smooth_samples begins
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
  sms <- smooths(model) # vector of smooth labels - "s(x)"
  take <- seq_along(sms) # in default case 1,2,3,..,n smooths
  if (!is.null(select)) {
    select <- check_user_select_smooths(sms, select = select,
      partial_match = partial_match, model_name = deparse(substitute(model)))
    # take <- which_smooths(model, term)
    take <- which(select) # need this as an id vector for things later
    sms <- sms[select] # S <- S[take]
  }

  # At least for now, don't work for random effect smooths
  # do we have ranef smooths?
  re_sms <- vapply(get_smooths_by_id(model, take),
    FUN = is_re_smooth,
    FUN.VALUE = logical(1L)
  )
  if (any(re_sms)) {
    message(
      "\nRandom effect smooths not currently supported.\nIgnoring:",
      " <", paste(sms[re_sms], collapse = ", "), ">\n"
    )
    sms <- sms[!re_sms]
    take <- take[!re_sms]
  }
  # do we have any remaining terms?
  if (length(sms) < 1L) {
    stop("No smooths left that can be sampled from.")
  }
  if (!is.null(newdata)) {
    newdata_deprecated()
  }
  if (!is.null(ncores)) {
    message("Argument `ncores` is deprecated. Use `n_cores` instead.")
    n_cores <- ncores
  }
  need_data <- FALSE
  if (is.null(data)) {
    need_data <- TRUE
  }
  # what posterior sampling are we using
  method <- match.arg(method)
  mvn_method <- match.arg(mvn_method)
  # get posterior draws - call this once for all parameters
  if (isFALSE(rng_per_smooth)) {
    betas <- post_draws(
      n = n, method = method,
      n_cores = n_cores, model = model,
      burnin = burnin, thin = thin, t_df = t_df, rw_scale = rw_scale,
      index = NULL, frequentist = freq, unconditional = unconditional,
      draws = draws, mvn_method = mvn_method, seed = seed
    )
  }
  sims <- data_names <- vector("list", length = length(sms))
  for (i in seq_along(sms)) {
    if (need_data) {
      # FIXME: should offset be NULL?
      data <- smooth_data(model,
        id = take[i], n = n_vals,
        offset = NULL
      )
      # I don't think we need offset here as that really just shifts the
      # response around
    }
    sm <- get_smooths_by_id(model, take[i])[[1L]]
    idx <- smooth_coef_indices(sm)
    Xp <- PredictMat(sm, data = data)
    # get posterior draws - use old behaviour? TRUE is yes
    simu <- if (isTRUE(rng_per_smooth)) {
      betas <- post_draws(
        n = n, method = method,
        n_cores = n_cores, model = model,
        burnin = burnin, thin = thin, t_df = t_df, rw_scale = rw_scale,
        index = idx, frequentist = freq, unconditional = unconditional,
        seed = seed
      )
      Xp %*% t(betas)
    } else {
      # In a scam model, the intercept seems to be parametrised into the
      # smooth as PredictMat returns a constant column
      if (isTRUE(length(idx) < ncol(Xp))) {
        Xp[, -1, drop = FALSE] %*% t(betas[, idx, drop = FALSE])
      } else {
        Xp %*% t(betas[, idx, drop = FALSE])
      }
    }
    colnames(simu) <- paste0("..V", seq_len(NCOL(simu)))
    simu <- as_tibble(simu)
    nr_simu <- nrow(simu)
    simu <- add_by_data(simu,
      by_name = by_variable(sm), by_data = data,
      before = 1L
    )
    # add on spline type info
    sm_type <- smooth_type(sm)
    simu <- add_column(simu,
      .smooth = rep(
        unlist(lapply(strsplit(sms[[i]], ":"), `[`, 1L)),
        nr_simu
      ),
      .term = rep(sms[[i]], each = nr_simu),
      .type = rep(sm_type, nr_simu),
      .row = seq_len(nr_simu),
      .after = 0L
    )
    simu <- bind_cols(simu, data[smooth_variable(sm)])
    simu <- pivot_longer(simu,
      cols = dplyr::starts_with("..V"),
      names_to = ".draw", values_to = ".value",
      names_transform = \(x) as.integer(sub("\\.\\.V", "", x))
    )
    simu <- relocate(simu, any_of(names(data)), .after = last_col()) |>
      relocate(".by", .after = 3L)
    ## nest all columns with varying data
    simu <- nest(simu, data = all_of(c(
      ".row", ".draw", ".value",
      vars_in_smooth(sm)
    )))
    sims[[i]] <- simu
  }

  sims <- bind_rows(sims)
  sims <- unnest(sims, all_of("data"))
  attr(sims, "seed") <- RNGstate
  ## add classes
  class(sims) <- c("smooth_samples", class(sims))
  sims
}

#' @export
`smooth_samples.gamm` <- function(model, ...) {
  smooth_samples(model$gam)
}

#' @title Posterior expectations of derivatives from an estimated model
#'
#' @param object an R object to compute derivatives for
#' @param ... arguments passed to other methods and on to `fitted_samples()`
#'
#' @author Gavin L. Simpson
#'
#' @export
`derivative_samples` <- function(object, ...) {
  UseMethod("derivative_samples")
}

#' @rdname derivative_samples
#' @export
`derivative_samples.default` <- function(object, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop("Don't know how to calculate response derivatives for <",
    class(object)[[1L]], ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

#' @rdname derivative_samples
#'
#' @export
`derivative_samples.gamm` <- function(object, ...) {
  derivative_samples(object[["gam"]], ...)
}

#' @param envir the environment within which to recreate the data used to fit
#'   `object`.
#' @param draws matrix; user supplied posterior draws to be used when
#'   `method = "user"`.
#'
#' @inheritParams response_derivatives
#'
#' @export
#'
#' @rdname derivative_samples
#'
#' @return A tibble, currently with the following variables:
#' * `.derivative`: the estimated partial derivative,
#' * additional columns containing the covariate values at which the derivative
#'   was eveluated.
#'
#' @examples
#'
#' load_mgcv()
#' df <- data_sim("eg1", dist = "negbin", scale = 0.25, seed = 42)
#'
#' # fit the GAM (note: for execution time reasons using bam())
#' m <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
#'   data = df, family = nb(), method = "fREML", discrete = TRUE
#' )
#'
#' # data slice through data along x2 - all other covariates will be set to
#' # typical values (value closest to median)
#' ds <- data_slice(m, x2 = evenly(x2, n = 200))
#'
#' # samples from posterior of derivatives
#' fd_samp <- derivative_samples(m,
#'   data = ds, type = "central",
#'   focal = "x2", eps = 0.01, seed = 21, n_sim = 100
#' )
#'
#' # plot the first 20 posterior draws
#' if (requireNamespace("ggplot2") && requireNamespace("dplyr")) {
#'   library("ggplot2")
#'   fd_samp |>
#'     dplyr::filter(.draw <= 20) |>
#'     ggplot(aes(x = x2, y = .derivative, group = .draw)) +
#'     geom_line(alpha = 0.5)
#' }
`derivative_samples.gam` <- function(
    object,
    focal = NULL,
    data = NULL,
    order = 1L,
    type = c("forward", "backward", "central"),
    scale = c("response", "linear_predictor"),
    method = c("gaussian", "mh", "inla", "user"),
    n = 100,
    eps = 1e-7,
    n_sim = 10000, level = 0.95,
    seed = NULL,
    envir = environment(formula(object)),
    draws = NULL,
    ...) {
  ## handle type
  type <- match.arg(type)
  ## handle method
  method <- match.arg(method)
  ## handle scale
  scale <- match.arg(scale)

  ## handle order
  if (!order %in% c(1L, 2L)) {
    stop(
      "Only 1st or 2nd order partial derivatives are supported: ",
      "`order %in% c(1,2)`"
    )
  }

  ## handle data
  need_data <- is.null(data)

  ## handle focal
  if (is.null(focal)) {
    stop("Argument 'focal' must be supplied.")
  }

  ## sort out data
  if (need_data) {
    x <- object$var.summary[[focal]]
    x <- seq(x[1L], x[3L], length = n)
    tv <- typical_values(object,
      vars = !matches(focal), data = data,
      envir = envir
    )
    data <- expand_grid(.x = x, tv)
  } else {
    data <- data |>
      select(all_of(model_vars(object))) |>
      rename(.x = all_of({{ focal }}))
  }
  data <- data |>
    add_column(.row = seq_len(nrow(data)), .before = 1L)

  # now shift values depending on method
  fd_data <- prepare_fdiff_data(
    data = data, eps = eps, type = type,
    order = order, focal = focal
  )

  ## compute posterior draws of E(y) (on response scale)
  fs <- fitted_samples(
    model = object, n = n_sim, data = fd_data,
    method = method, seed = seed, draws = draws, ...
  )

  fs <- fs |>
    left_join(select(fd_data, all_of(c(".row", "..type", "..orig"))),
      by = ".row"
    )

  yd <- compute_y_fdiff(fs, order = order, type = type, eps = eps)

  yd <- yd |>
    left_join(data, by = join_by("..orig" == ".row")) |>
    rename("{focal}" := ".x", ".derivative" = "..fd", ".row" = "..orig") |>
    select(!matches(c("..xf", "..xb", "..x"))) |>
    add_column(.focal = rep(focal, nrow(data) * n_sim), .before = 1L) |>
    relocate(".row", .before = 1L)

  class(yd) <- append(class(yd), "derivative_samples", after = 0L)
  yd
}
