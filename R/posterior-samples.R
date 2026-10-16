#' Draw samples from the posterior distribution of an estimated model
#'
#' @details
#' With no explicit `data`, observation rows follow the fitted model's
#' `na.action`. `na.exclude` restores excluded rows as `NA` in every draw;
#' `na.omit` returns retained rows only. `.row` indexes the fitting data after
#' any `subset`. With explicit `data`, `.row` indexes that data, missing
#' responses do not prevent prediction, and missing required predictors give
#' `NA` draws. An explicit prediction `na.action` in `...` is honoured without
#' renumbering retained rows. Restoration does not consume random numbers.
#'
#' @param model a fitted model of the supported types; see [diagnostic_support].
#' @param conditioning character or `NULL`; mixed-model conditioning mode;
#'   see [diagnostic_support].
#' @param data  data frame; new observations at which the posterior draws
#'   from the model should be evaluated. If not supplied, the data used to fit
#'   the model will be used for `data`, if available in `model`.
#' @inheritParams post_draws n_draws
#' @inheritParams post_draws
#' @param seed numeric; a random seed for the simulations.
#' @param frequentist logical; `TRUE` to use the frequentist covariance matrix of
#'   the parameter estimators, `FALSE` to use the Bayesian posterior
#'   covariance matrix of the parameters.
#' @param unconditional logical; if `TRUE` (and `frequentist == FALSE`) then the
#'   Bayesian smoothing parameter uncertainty corrected covariance matrix is
#'   used, if available.
#' @param family_index numeric; optional family-list positions for grouped
#'   family ([mgcv::gfam()]) response draws, one integer per evaluation row.
#'   Membership can instead be recovered from the response-index expression
#'   in `data`; observed response values are not required.
#' @param weights numeric; response sampling weights for GAM methods. An
#'   explicit vector overrides the fitting weights; a single value applies to
#'   every observation. With `data = NULL`, the default is the fitted prior
#'   weights, except factor-valued binomial responses retain one Bernoulli trial
#'   per observation. Native `gaulss()` ignores fitting weights in its likelihood,
#'   so its default response weights are also ones; explicitly supplied sampling
#'   weights still override this default. With prediction data, weights default
#'   to a vector of ones.
#'   For ordinary Gaussian and Gamma responses, larger weights reduce response
#'   variance (precision weights). For binomial responses, weights give integer
#'   trial counts. They do not alter the fitted coefficient covariance. Include
#'   positions restored by `na.exclude` when supplying weights for fitting rows.
#' @inheritParams post_draws n_cores n_threads
#' @param method character; which method should be used to draw samples from
#'   the posterior distribution. `"gaussian"` uses a Gaussian (Laplace)
#'   approximation to the posterior. `"mh"` uses a Metropolis Hastings sampler
#'   that alternates t proposals with proposals based on a shrunken version of
#'   the posterior covariance matrix. `"inla"` uses a variant of Integrated
#'   Nested Laplace Approximation due to Wood (2019), (currently not
#'   implemented). `"user"` allows for user-supplied posterior draws
#'   as a coefficient matrix.
#' @param burnin numeric; number of samples to discard as the burnin draws.
#'   Only used with `method = "mh"`.
#' @inheritParams post_draws thin
#' @param t_df numeric; degrees of freedom for t distribution proposals. Only
#'   used with `method = "mh"`.
#' @param rw_scale numeric; Factor by which to scale posterior covariance
#'   matrix when generating random walk proposals. Negative or non finite to
#'   skip the random walk step. Only used with `method = "mh"`.
#' @param ... arguments passed to other methods. For `fitted_samples()`, these
#'   are passed on to [mgcv::predict.gam()]. For `posterior_samples()` these are
#'   passed on to `fitted_samples()`. For `predicted_samples()` these are
#'   passed on to the relevant `simulate()` method.
#' @param draws matrix; user supplied posterior draws to be used when
#'   `method = "user"`.
#' @param mvn_method character; one of `"mvnfast"` or `"mgcv"`. The default is
#'   uses `mvnfast::rmvn()`, which can be considerably faster at generate large
#'   numbers of MVN random values than `mgcv::rmvn()`, but which might not work
#'   for some marginal fits, such as those where the covariance matrix is close
#'   to singular.
#'
#' @details
#' For Gaussian identity-link `bam` and `scam` fits, first-order autoregressive (AR1) errors
#' make consecutive responses correlated. Responses are drawn together to retain
#' this fitted correlation, restarting at each `AR.start` segment. These draws
#' currently use the fitting observations only. For other families or links,
#' the AR1 correction applies to working residuals used during fitting. It does
#' not specify a joint distribution of actual responses, so sampling produces
#' an error.
#'
#' Location-scale-shape families [mgcv::gaulss()], [mgcv::gammals()],
#' [mgcv::gumbls()], [mgcv::gevlss()], [mgcv::shash()], [mgcv::twlss()] and
#' [mgcv::ziplss()] support fitting observations, subsets and new covariates.
#' Every distribution parameter is computed from the same joint coefficient
#' draw. Formula offsets and prescribed smooth contributions are retained.
#' Evaluation data need predictors and formula-offset variables, but no
#' observed response. In `gaulss()`, the second response parameter is inverse
#' standard deviation: sampling uses its reciprocal, divided by the square
#' root of the sampling weight. Other LSS generators currently ignore sampling
#' weights, following their mgcv implementations. Parameters represented by
#' coefficients vary jointly; other family parameters remain fixed.
#'
#' Cox survival models require evaluation times and an observation process and
#' do not have a response generator for this interface. Censored-family draws
#' are latent, uncensored responses; reproducing observed censoring requires
#' an observation process and remains unsupported in posterior predictive checks.
#' Grouped-family posterior response draws support fitting observations and
#' supplied data with known family membership. Component nuisance parameters
#' remain fixed. `.parameter` identifies the component used for each row.
#' Censored grouped components require an observation process; use `simulate()`
#' for their latent, uncensored responses.
#'
#' # Note
#'
#' With `data = NULL`, fitting offsets are retained, including offsets
#' supplied through the fitting function's `offset` argument. Explicit prediction
#' data follow [mgcv::predict.gam()]: offsets in formulas are evaluated from
#' the prediction data, while offsets supplied through the fitting function's
#' `offset` argument are not transferred to new observations. Include offsets
#' in the formula when predicting at new data; the required offset variables
#' must then be present in `data`.
#'
#' @return A tibble (data frame) in long format, with columns:
#' * `.row`: row of the evaluation data;
#' * `.draw`: globally unique draw identifier;
#' * `.response`: simulated response.
#' Where needed, `.parameter` identifies the response or grouped component.
#' Multiple-chain output also includes `.chain` and `.iteration`. Attached
#' coefficient diagnostics are available through [sampling_diagnostics()].
#'
#' @author Gavin L. Simpson
#'
#' @references
#'
#' Wood, S.N., (2020). Simplified integrated nested Laplace approximation.
#'   *Biometrika* **107**, 223--230. \doi{10.1093/biomet/asz044}
#'
#' @export
`posterior_samples` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  UseMethod("posterior_samples")
}

#' @export
`posterior_samples.default` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  stop(
    "Don't know how to sample from the posterior of <",
    class(model)[[1L]],
    ">",
    .call = FALSE
  )
}

#' @export
#' @rdname posterior_samples
`posterior_samples.gam` <- function(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e6,
  weights = NULL,
  family_index = NULL
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = TRUE)
  sampling_check_options(
    match.arg(method),
    names(match.call(expand.dots = FALSE))[-1L],
    mh_backend = match.arg(mh_backend),
    mvn_method = match.arg(mvn_method)
  )
  layout <- sampling_resolve(n_draws, chains, draws, method = match.arg(method))
  n_draws <- layout$n_draws
  chains <- layout$chains
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }
  n_threads <- sampling_count(n_threads, "n_threads")

  # generate new response data from the model including the uncertainty in
  # the model.

  method <- match.arg(method)
  mvn_method <- match.arg(mvn_method)
  fam_type <- family_type(model)

  do_posterior_samples(
    model = model,
    n = n_draws,
    data = data,
    seed = seed,
    method = method,
    n_threads = n_threads,
    n_cores = n_cores,
    chains = chains,
    diagnostics = diagnostics,
    mh_backend = mh_backend,
    block_size = block_size,
    dense_limit = dense_limit,
    burnin = burnin,
    thin = thin,
    t_df = t_df,
    rw_scale = rw_scale,
    freq = frequentist,
    unconditional = unconditional,
    weights = weights,
    family_index = family_index,
    draws = draws,
    mvn_method = mvn_method,
    ...
  )
}

#' @export
#' @rdname posterior_samples
`posterior_samples.scam` <- function(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e6,
  weights = NULL,
  family_index = NULL
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = TRUE)
  sampling_check_options(
    match.arg(method),
    names(match.call(expand.dots = FALSE))[-1L],
    mh_backend = match.arg(mh_backend),
    mvn_method = match.arg(mvn_method)
  )
  layout <- sampling_resolve(n_draws, chains, draws, method = match.arg(method))
  n_draws <- layout$n_draws
  chains <- layout$chains
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }
  n_threads <- sampling_count(n_threads, "n_threads")

  check_scam_unconditional(model, unconditional)
  do_posterior_samples(
    model = model,
    n = n_draws,
    data = data,
    seed = seed,
    method = method,
    n_threads = n_threads,
    n_cores = n_cores,
    chains = chains,
    diagnostics = diagnostics,
    mh_backend = mh_backend,
    block_size = block_size,
    dense_limit = dense_limit,
    burnin = burnin,
    thin = thin,
    t_df = t_df,
    rw_scale = rw_scale,
    freq = frequentist,
    unconditional = unconditional,
    weights = weights,
    family_index = family_index,
    draws = draws,
    mvn_method = mvn_method,
    ...
  )
}

`do_posterior_samples` <- function(
  n_threads = 1L,
  model,
  n = 1,
  data = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1,
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  freq = FALSE,
  unconditional = FALSE,
  weights = NULL,
  family_index = NULL,
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  envir = NULL,
  chains = 1L,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  block_size = 10000L,
  dense_limit = 5e6,
  ...
) {
  model <- with_model_envir(model, envir)
  # generate new response data from the model including the uncertainty in
  # the model.

  method <- match.arg(method)
  mvn_method <- match.arg(mvn_method)
  fam_type <- family_type(model)
  fam <- stats::family(model)
  grouped <- is_gfam_family(fam)
  if (!grouped && !is.null(family_index)) stop(
    "'family_index' is only supported for grouped-family models.", call. = FALSE)
  joint_distribution <- NULL
  correlated <- !is.null(model$AR1.rho) && model$AR1.rho != 0
  if (correlated) {
    if (!is.null(data)) {
      stop(
        "AR1 posterior response draws require fitting observations; ",
        "a joint correlation structure for new data is not defined.",
        call. = FALSE
      )
    }
    if (fam_type != "gaussian" || fam$link != "identity") {
      stop(
        "The working AR1 correction does not define a joint response simulator ",
        "for this family and link.",
        call. = FALSE
      )
    }
    joint_distribution <- response_distribution(model)
    if (!is.function(joint_distribution$rd)) {
      stop(
        "A joint response generator is required for correlated posterior draws.",
        call. = FALSE
      )
    }
  }
  if (fam_type == "cox_ph") stop(
    "Cox posterior response sampling requires an observation process and a response generator; ",
    "evaluation times and strata alone do not define response draws.", call. = FALSE)
  advanced <- !grouped &&
    (inherits(fam, "general.family") &&
      !fam_type %in% c("multivariate_normal", "multinom"))
  if (advanced && !is_lss_family(fam)) {
    if (!is.null(data)) {
      stop("Posterior response sampling at supplied data is not implemented for ",
        "family <", family_name(fam), ">; a distribution-parameter adapter is required.",
        call. = FALSE)
    }
    return(diagnostic_posterior_samples.default(
      model,
      n,
      seed,
      method = method,
      n_threads = n_threads,
      n_cores = n_cores,
      chains = chains,
      diagnostics = diagnostics,
      mh_backend = mh_backend,
      block_size = block_size,
      dense_limit = dense_limit,
      burnin = burnin,
      thin = thin,
      t_df = t_df,
      rw_scale = rw_scale,
      frequentist = freq,
      unconditional = unconditional,
      weights = weights,
      draws = draws,
      mvn_method = mvn_method,
      ...
    ))
  }
  # Seed coefficient and response draws once, so the streams are independent.
  # Restore both an existing RNG state and its absence at the public boundary.
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
    }
    on.exit(
      {
        if (had_seed) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (
          exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        ) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      },
      add = TRUE
    )
    set.seed(seed)
  }

  # start getting draws of expectation
  # - some families need linear predictor values/matrix, e.g., multinom(),
  #   but we'll handle that later by not using the family()$rd
  coefficient_seed <- if (method == "user") {
    NULL
  } else {
    sample.int(.Machine$integer.max, 1L)
  }
  if (grouped) {
    context <- grouped_posterior_context(model, data, family_index = family_index, ...)
    betas <- sampling_call(post_draws, list(model = context$model,
      n_draws = n, seed = coefficient_seed, method = method,
      n_threads = n_threads, n_cores = n_cores, chains = chains,
      diagnostics = diagnostics, mh_backend = mh_backend, block_size = block_size,
      dense_limit = dense_limit, burnin = burnin, thin = thin, t_df = t_df,
      rw_scale = rw_scale, frequentist = freq, unconditional = unconditional,
      draws = draws, mvn_method = mvn_method))
    out <- grouped_response_draws(context, betas, weights)
    class(out) <- c("posterior_samples", class(out))
    return(attach_sampling(out, betas, draw_index = TRUE))
  }
  if (is_lss_family(fam)) {
    context <- lss_prediction_context(model, data, ...)
    betas <- sampling_call(post_draws, list(model = context$model,
      n_draws = n, seed = coefficient_seed, method = method,
      n_threads = n_threads, n_cores = n_cores, chains = chains,
      diagnostics = diagnostics, mh_backend = mh_backend, block_size = block_size,
      dense_limit = dense_limit, burnin = burnin, thin = thin, t_df = t_df,
      rw_scale = rw_scale, frequentist = freq, unconditional = unconditional,
      draws = draws, mvn_method = mvn_method))
    out <- lss_response_draws(context, betas, weights)
    class(out) <- c("posterior_samples", class(out))
    return(attach_sampling(out, betas, draw_index = TRUE))
  }
  sim_eta <- sampling_call(
    fitted_samples,
    list(
      model,
      n_draws = n,
      data = data,
      seed = coefficient_seed,
      scale = "response",
      method = method,
      n_threads = n_threads,
      n_cores = n_cores,
      chains = chains,
      diagnostics = diagnostics,
      mh_backend = mh_backend,
      block_size = block_size,
      dense_limit = dense_limit,
      burnin = burnin,
      thin = thin,
      t_df = t_df,
      rw_scale = rw_scale,
      frequentist = freq,
      unconditional = unconditional,
      draws = draws,
      mvn_method = mvn_method,
      ...
    )
  )

  sampling <- attr(sim_eta, "sampling")
  ## rd function if available
  rd_fun <- missing_safe_rd(
    choose_rd_fun(model),
    n_response = if (fam_type == "multivariate_normal") n_eta(model) else 1L
  )

  ## dispersion or scale variable for simulation
  scale_p <- model[["sig2"]]
  if (is.null(scale_p)) {
    scale_p <- summary(model)[["dispersion"]]
  }

  if (is.null(data) && is.null(weights)) {
    used <- model_used_rows(model)
    weights <- restore_model_rows(
      binomial_sampling_weights(used, used[["prior.weights"]]),
      model
    )
  } else {
    if (is.null(weights)) {
      weights <- rep(1, nrow(data))
    }
  }

  n_rows <- if (is.null(data)) length(model_row_map(model)) else NROW(data)
  if (is.null(weights)) {
    weights <- rep(1, n_rows)
  }
  if (length(weights) == 1L) {
    weights <- rep(weights, n_rows)
  }
  if (length(weights) != n_rows) {
    stop("Weights must have one value per prediction row.")
  }
  if (!is.null(joint_distribution)) {
    used <- model_used_rows(model)
    used$prior.weights <- weights[which(!is.na(model_row_map(model)))]
    joint_distribution <- response_distribution(used)
  }
  # Match weights to retained input positions before extending by draws.
  row_ids <- unique(sim_eta$.row)
  weights <- rep(weights[row_ids], times = length(unique(sim_eta$.draw)))
  # scale <- rep(scale, times = n)

  # replace fitted with simulated response
  # need to handle mvn() and multinom() with multiple linear predictors that
  # are all really just location parameters
  if (!is.null(joint_distribution)) {
    sim_eta <- posterior_location_response_draws(
      sim_eta,
      joint_distribution,
      rows = which(!is.na(model_row_map(model)))
    )
  } else if (is_multivariate_y(model)) {
    sim_eta_w <- sim_eta |>
      pivot_wider(
        id_cols = all_of(c(".row", ".draw")),
        values_from = ".fitted",
        names_from = ".parameter"
      )
    # Different multivariate models need different handling because they expect
    # (intake) and return different kinds of things. multinom() for example
    # should return a predicted category for each sample, so the output looks
    # more like a normal/standard family. mvn(), however, returns 1 value per
    # response per draw
    if (identical(fam_type, "multinom")) {
      sim_eta <- sim_eta_w |>
        select(all_of(c(".row", ".draw"))) |>
        bind_cols(
          rd_fun(
            mu = sim_eta_w[, -c(1, 2)] |> as.matrix(),
            wt = weights,
            scale = scale_p
          ) |>
            as_tibble() |>
            rename(".response" = "value")
        )
    } else if (identical(fam_type, "multivariate_normal")) {
      sim_eta <- sim_eta_w |>
        select(all_of(c(".row", ".draw"))) |>
        bind_cols(
          rd_fun(
            mu = sim_eta_w[, -c(1, 2)] |> as.matrix(),
            wt = weights,
            scale = scale_p
          ) |>
            as_tibble()
        ) |>
        pivot_longer(
          cols = !c(".row", ".draw"),
          values_to = ".response",
          names_to = ".parameter"
        )
    } else {
      stop("Unexpected multivariate model; file a bug report please!")
    }
  } else {
    sim_eta <- sim_eta |>
      mutate(
        .response = rd_fun(
          mu = .data$.fitted,
          wt = weights,
          scale = scale_p
        )
      ) |>
      select(all_of(c(".row", ".draw", ".response")))
  }

  ## add classes
  class(sim_eta) <- c("posterior_samples", class(sim_eta))
  attach_sampling(sim_eta, sampling)
}

# For a location family, changing the mean shifts the response distribution.
# Draw a complete response vector to retain dependence and fitted scales, then
# shift it to the drawn mean. Match retained rows before restoring exclusions.
posterior_location_response_draws <- function(fitted, distribution, rows) {
  mean <- as.numeric(distribution$mean)
  if (length(mean) != length(rows)) {
    stop(
      "Joint response means are not aligned with the fitting observations.",
      call. = FALSE
    )
  }
  response <- rep(NA_real_, nrow(fitted))
  for (draw in unique(fitted$.draw)) {
    positions <- which(fitted$.draw == draw)
    index <- match(fitted$.row[positions], rows)
    valid <- which(!is.na(index) & !is.na(fitted$.fitted[positions]))
    values <- distribution$rd()
    if (
      !is.numeric(values) ||
        !is.null(dim(values)) ||
        length(values) != length(mean)
    ) {
      stop(
        "A joint response generator must return one value per fitting observation.",
        call. = FALSE
      )
    }
    response[positions[valid]] <- values[index[valid]] +
      fitted$.fitted[positions[valid]] -
      mean[index[valid]]
  }
  fitted$.response <- response
  fitted[, c(".row", ".draw", ".response"), drop = FALSE]
}

#' Draw fitted values from the posterior distribution
#'
#' Expectations (fitted values) of the response drawn from the posterior
#' distribution of fitted model using a Gaussian approximation to the
#' posterior or a simple Metropolis Hastings sampler.
#'
#' @param scale character; `"response"` returns the expected response;
#'   `"linear_predictor"` returns the first linear predictor. For LSS models,
#'   this is the predictor for location, which need not be the expected response.
#'   For multinomial models both scales return every category, labelled
#'   `response1`, `response2`, and so on. On the linear predictor scale,
#'   `response1` is the reference category with predictor zero; other values
#'   are log odds relative to it. Response-scale values are probabilities.
#'
#' @param family_index numeric; optional family-list positions for grouped
#'   family ([mgcv::gfam()]) response predictions, with one integer per requested
#'   row. By default membership is recovered from the response/index expression
#'   in `data`, or the fitted membership when `data` is omitted. This is useful
#'   when predicting without responses or with a complex response expression.
#'   An explicit index must agree with membership recoverable from `data`.
#'
#' @details
#' For LSS models, `scale = "response"` returns the expected response, using
#' all parameters on which it depends. This includes the mean correction for
#' Gumbel, GEV and sinh-arcsinh distributions and the hurdle probability for
#' `ziplss()`. The Gamma LSS mean is the exponential of its first predictor.
#' `scale = "linear_predictor"` returns only the first predictor (location);
#' it is not a transformation of the expectation for these multi-parameter
#' distributions. The existing `.parameter = "location"` label is retained on
#' both scales. A GEV expectation does not exist at shape >= 1: response-scale
#' fitted and derivative sampling then fail explicitly, while response draws
#' and location-predictor draws remain available.
#'
#' For grouped families, coefficient draws preserve the joint model covariance;
#' each row is then transformed using its own fitted component family. Explicit
#' prediction data must identify each row's family: no training-order fallback
#' is used. A two-column response matrix supplies its second column; a numeric
#' column named for the response supplies indices directly. For a response such
#' as `cbind(y, index)`, the `index` expression can be supplied without `y`.
#' Missing indices behave like missing required predictors. Link-scale draws
#' do not require family membership.
#'
#' Grouped response-scale draws currently support scalar fitted means, including
#' families with a scalar prediction transform such as [mgcv::ziP()]. They do
#' not support [mgcv::ocat()] category-probability output; use the link scale
#' for grouped ordinal models. Distributional nuisance parameters are held at
#' their fitted values. This does not add support for general/location-scale
#' families that `gfam()` itself rejects, nor repair upstream fitting failures
#' (including fixed-theta negative-binomial components and some `ziP()` fits
#' in some mgcv versions).
#'
#' Observation rows use the same missing-value convention as
#' [posterior_samples()]: default results restore `na.exclude` observations in
#' every draw, while explicit `data` are evaluated in their own row order.
#'
#' # Note
#'
#' With `data = NULL`, fitting offsets are retained, including offsets
#' supplied through the fitting function's `offset` argument. Explicit prediction
#' data follow [mgcv::predict.gam()]: offsets in formulas are evaluated from
#' the prediction data, while offsets supplied through the fitting function's
#' `offset` argument are not transferred to new observations. Include offsets
#' in the formula when predicting at new data; the required offset variables
#' must then be present in `data`.
#'
#' @return A tibble (data frame) in long format, with columns:
#' * `.row`: row of the evaluation data;
#' * `.draw`: globally unique draw identifier;
#' * `.parameter`: `"location"` for scalar fitted values, or a response/category
#'   label for multivariate or multinomial models;
#' * `.fitted`: expected response or linear predictor, as selected by `scale`.
#' Multiple-chain output also includes `.chain` and `.iteration`. Attached
#' coefficient diagnostics are available through [sampling_diagnostics()].
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
`fitted_samples` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  UseMethod("fitted_samples")
}

#' @export
`fitted_samples.default` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  stop(
    "Don't know how to sample from the posterior of <",
    class(model)[[1L]],
    ">",
    .call = FALSE
  )
}

#' @export
`fitted_samples.gamm` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  fitted_samples(model$gam, ...)
}

#' @export
#'
#' @rdname fitted_samples
#'
#' @importFrom stats vcov coef predict
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
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
#' fs <- fitted_samples(m1, n_draws = 5, seed = 42)
#' \donttest{
#' fs
#' }
#'
#' # can generate own set of draws and use them
#' drws <- generate_draws(m1, n_draws = 2, seed = 24)
#' fs2 <- fitted_samples(m1, method = "user", draws = drws)
#' \donttest{
#' fs2
#' }
#' \dontshow{
#' options(op)
#' }
`fitted_samples.gam` <- function(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e6,
  scale = c("response", "linear_predictor"),
  family_index = NULL
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = TRUE)
  sampling_check_options(
    match.arg(method),
    names(match.call(expand.dots = FALSE))[-1L],
    mh_backend = match.arg(mh_backend),
    mvn_method = match.arg(mvn_method)
  )
  layout <- sampling_resolve(n_draws, chains, draws, method = match.arg(method))
  n_draws <- layout$n_draws
  chains <- layout$chains
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }
  n_threads <- sampling_count(n_threads, "n_threads")

  do_fitted_samples(
    model = model,
    n = n_draws,
    data = data,
    seed = seed,
    scale = scale,
    method = method,
    n_threads = n_threads,
    n_cores = n_cores,
    chains = chains,
    diagnostics = diagnostics,
    mh_backend = mh_backend,
    block_size = block_size,
    dense_limit = dense_limit,
    burnin = burnin,
    thin = thin,
    t_df = t_df,
    rw_scale = rw_scale,
    freq = frequentist,
    unconditional = unconditional,
    draws = draws,
    mvn_method = mvn_method,
    family_index = family_index,
    ...
  )
}

#' @export
#' @rdname fitted_samples
`fitted_samples.scam` <- function(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e6,
  scale = c("response", "linear_predictor")
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = TRUE)
  sampling_check_options(
    match.arg(method),
    names(match.call(expand.dots = FALSE))[-1L],
    mh_backend = match.arg(mh_backend),
    mvn_method = match.arg(mvn_method)
  )
  layout <- sampling_resolve(n_draws, chains, draws, method = match.arg(method))
  n_draws <- layout$n_draws
  chains <- layout$chains
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }
  n_threads <- sampling_count(n_threads, "n_threads")

  check_scam_unconditional(model, unconditional)
  do_fitted_samples(
    model = model,
    n = n_draws,
    data = data,
    seed = seed,
    scale = scale,
    method = method,
    n_threads = n_threads,
    n_cores = n_cores,
    chains = chains,
    diagnostics = diagnostics,
    mh_backend = mh_backend,
    block_size = block_size,
    dense_limit = dense_limit,
    burnin = burnin,
    thin = thin,
    t_df = t_df,
    rw_scale = rw_scale,
    freq = frequentist,
    unconditional = unconditional,
    draws = draws,
    mvn_method = mvn_method,
    ...
  )
}

`do_fitted_samples` <- function(
  n_threads = 1L,
  model,
  n = 1,
  data = NULL,
  seed = NULL,
  scale = c("response", "linear_predictor"),
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1,
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  freq = FALSE,
  unconditional = FALSE,
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  envir = NULL,
  family_index = NULL,
  chains = 1L,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  block_size = 10000L,
  dense_limit = 5e6,
  ...
) {
  training <- is.null(data)
  model <- with_model_envir(model, envir)
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

  scale <- match.arg(scale)
  fn <- family_type(model)
  gen_fam <- inherits(family(model), "general.family")
  if (gen_fam && scale == "response" &&
      !is_lss_family(family(model)) && !fn %in% c("multivariate_normal", "multinom", "cox_ph")) {
    stop("Response-scale fitted samples are not supported for general family ",
      sQuote(family_name(model)), ". Use scale = 'linear_predictor' for ",
      "the first linear predictor.", call. = FALSE)
  }
  grouped <- is_gfam_family(family(model))
  if (!grouped && !is.null(family_index)) {
    stop(
      "'family_index' is only supported for grouped-family models.",
      call. = FALSE
    )
  }
  context <- if (grouped && identical(scale, "response")) {
    gfam_prediction_context(
      model,
      data,
      family_index = family_index,
      na.action = list(...)$na.action %||% stats::na.pass
    )
  } else {
    NULL
  }
  layout <- if (is.null(context)) {
    prediction_layout(
      model,
      data,
      na.action = list(...)$na.action %||% stats::na.pass
    )
  } else {
    context$layout
  }
  model <- model_used_rows(model)
  data <- layout$data
  method <- match.arg(method)
  mvn_method <- match.arg(mvn_method)

  # get posterior draws
  betas <- sampling_call(
    post_draws,
    list(
      n_draws = n,
      method = method,
      n_threads = n_threads,
      n_cores = n_cores,
      chains = chains,
      diagnostics = diagnostics,
      mh_backend = mh_backend,
      block_size = block_size,
      dense_limit = dense_limit,
      model = model,
      burnin = burnin,
      thin = thin,
      t_df = t_df,
      rw_scale = rw_scale,
      index = NULL,
      frequentist = freq,
      unconditional = unconditional,
      draws = draws,
      seed = seed,
      mvn_method = mvn_method
    )
  )
  sampling <- attr(betas, "sampling")
  if (is_lss_family(stats::family(model))) {
    context <- lss_prediction_context(model, layout = layout, ...)
    values <- lss_fitted_values(context, betas, scale)
    sims <- tibble::tibble(
      .row = rep(seq_len(NROW(values)), NROW(betas)),
      .draw = rep(seq_len(NROW(betas)), each = NROW(values)),
      .parameter = "location", .fitted = as.numeric(values))
    sims <- restore_prediction_table(sims, layout)
    attr(sims, "seed") <- RNGstate
    class(sims) <- c("fitted_samples", class(sims))
    return(attach_sampling(sims, sampling, draw_index = TRUE))
  }
  ## betas is for *all* coefs, including distributional parameters
  lss_idx <- lss_eta_index(model)
  if (gen_fam) {
    # some general families should allow all linear predictors;
    # e.g., mvn(), multinom()
    if (fn %in% c("multivariate_normal", "multinom")) {
      lss_loc <- lss_idx # for mvn we want to loop over all the linpreds
    } else if (scale == "linear_predictor") {
      lss_loc <- lss_idx[[1L]]
      lss_idx <- lss_idx[1L]
    } else {
      if (family_type(model) == "cox_ph") {
        stop("Cox survival predictions require evaluation times and strata; ",
          "a location parameter alone does not define an expected response.",
          call. = FALSE)
      }
      stop("The expected response is not implemented for family <", family_name(model),
        ">; a family-specific expectation adapter is required.", call. = FALSE)
    }
  } else {
    lss_loc <- lss_idx[[1L]]
    lss_idx <- lss_idx[1L] # keep as a list
  }
  # function to control predictions
  pred_fun <- function(Xp, betas, lss_loc, scale, lp = 1L) {
    sims <- Xp[, lss_loc, drop = FALSE] %*% t(betas[, lss_loc, drop = FALSE])
    # handle the offset if present; it is an attribute on the Xp matrix
    # it is a list - with 1 component if a normal model and `l` components if it
    # is a model with `l` linear predictors.
    m_offset <- attr(Xp, "model.offset")
    if (is.list(m_offset)) {
      m_offset <- m_offset[[lp]]
    }
    if (!is.null(m_offset)) {
      sims <- sims + m_offset
    }
    # handle transformation to response scale
    if (identical(scale, "response") && fn != "multinom") {
      if (grouped) {
        sims <- gfam_response_means(sims, context)
      } else {
        ilink <- inv_link(model, parameter = "location")
        sims <- ilink(sims)
      }
    }
    colnames(sims) <- paste0(".V", seq_len(NCOL(sims)))
    sims <- as_tibble(sims)
    names(sims) <- as.character(seq_len(ncol(sims)))
    sims
  }
  ## don't need to pass freq, unconditional here as that is done for V
  Xp <- if (grouped) {
    gfam_predict_eta(model, data, type = "lpmatrix", ...)
  } else {
    predict_model(model, newdata = data, type = "lpmatrix", ...)
  }
  attr(Xp, "model.offset") <- prediction_offsets(model, Xp)
  if (training) {
    # Native fitting predictions retain offsets supplied as fitting arguments.
    # Apply the same term exclusions to the native predictor and design matrix.
    eta <- matrix(
      predict_model(model, type = "link", ...) +
        fitting_link_correction(model),
      nrow = NROW(Xp)
    )
    indices <- attr(Xp, "lpi") %||% lss_eta_index(model)
    attr(Xp, "model.offset") <- lapply(seq_along(indices), function(i) {
      j <- indices[[i]]
      eta[, i] - drop(Xp[, j, drop = FALSE] %*% stats::coef(model)[j])
    })
  }
  ## predict step; for mvn() and multinom() models, we do want to loop over
  ##   the linear predictors
  n_eta <- length(lss_idx) # number of linear predictors to work with
  if (n_eta > 1L || fn == "multinom") {
    # this is where we need to handle non-standard predictions
    n_data <- NROW(data)
    n_lp <- n_eta(model)
    sims <- lapply(
      seq_len(n_eta),
      FUN = \(i, Xp, betas, lss_loc, scale) {
        pred_fun(Xp, betas, lss_loc[[i]], scale, lp = i)
      },
      Xp = Xp,
      betas = betas,
      lss_loc = lss_loc,
      scale = scale
    )
    sims <- switch(
      fn,
      multivariate_normal = yhat_mvn(sims, n_data, n_lp),
      multinom = yhat_multinom(sims, n_data, n_lp, fam = family(model),
        scale = scale)
    )
  } else {
    sims <- pred_fun(Xp, betas, lss_loc, scale) |>
      add_column(
        .row = seq_len(nrow(data)),
        .parameter = rep("location", nrow(data))
      )
  }
  sims <- sims |>
    pivot_longer(
      cols = !any_of(c(".row", ".parameter")),
      values_to = ".fitted",
      names_to = ".draw",
      cols_vary = "slowest",
      names_transform = list(".draw" = as.integer)
    ) |>
    relocate(c(".row", ".draw", ".parameter"), .before = 1L)
  sims <- restore_prediction_table(sims, layout)
  attr(sims, "seed") <- RNGstate
  ## add classes
  class(sims) <- c("fitted_samples", class(sims))
  attach_sampling(sims, sampling, draw_index = TRUE)
}

`yhat_mvn` <- function(sims, n_data, n_eta) {
  sims <- sims |>
    bind_rows() |>
    add_column(
      .row = rep(seq_len(n_data), times = n_eta),
      .parameter = paste0(
        "response",
        rep(seq_len(n_eta), each = n_data)
      )
    )
  sims
}

`yhat_multinom` <- function(sims, n_data, n_eta, fam, scale) {
  n_y <- n_eta + 1
  sims <- sims |>
    bind_rows()
  n_sim <- NCOL(sims)
  out <- lapply(
    sims,
    FUN = \(x, n_data, n_eta, fam, scale) {
      eta <- matrix(x, ncol = n_eta, nrow = n_data)
      r <- if (scale == "response") {
        fam$predict(fam, eta = eta)[[1L]]
      } else {
        cbind(0, eta)
      }
      r |>
        data.frame() |>
        setNames(nm = paste0("response", seq_len(n_y))) |>
        as_tibble()
    },
    n_data = n_data, n_eta = n_eta, fam = fam, scale = scale
  ) |>
    bind_rows() |>
    add_column(
      .row = rep(seq_len(n_data), times = n_sim),
      .draw = rep(seq_len(n_sim), each = n_data),
      .before = 1L
    ) |>
    pivot_longer(
      cols = -c(".row", ".draw"),
      cols_vary = "slowest",
      names_to = ".parameter",
      values_to = ".fitted"
    ) |>
    pivot_wider(
      id_cols = c(".row", ".parameter"),
      names_from = ".draw",
      values_from = ".fitted"
    )
  out
}

#' Draw new response values from the conditional distribution of the response
#'
#' Predicted values of the response (new response data) are drawn from the
#' fitted model, created via `simulate()` (e.g. [simulate.gam()]) and returned
#' in a tidy, long, format. These predicted values do not include the
#' uncertainty in the estimated model; they are simply draws from the
#' conditional distribution of the response.
#'
#' @details
#' LSS response simulation uses all fitted distribution parameters at the
#' requested data, with the same parameter transformations and offset rules as
#' [posterior_samples()]. It holds coefficients fixed, so it has no coefficient
#' chains or attached coefficient diagnostics.
#'
#' @return A tibble (data frame) with 3 columns containing the posterior
#'   predicted values in long format. The columns are
#' * `.row` (integer) the row of `data` that each posterior draw relates to,
#' * `.draw` (integer) an index, in range `1:n_draws`, indicating which draw each row
#'     relates to,
#' * `.response` (numeric) the predicted response for the indicated row of
#'     `data`.
#'
#' @author Gavin L. Simpson
#'
#' @param n_draws positive integer; number of response replicates to generate
#'   at the fitted parameters. Required; coefficient chains are not used.
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
#' predicted_samples(m, n_draws = 5, seed = 42)
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
#' predicted_samples(m, n_draws = 5, data = newd, exclude = "s(x2)", seed = 25)
#'
#' ## Exclude s(x1)
#' predicted_samples(m, n_draws = 5, data = newd, exclude = "s(x1)", seed = 25)
#'
#' ## Select which terms --- result should be the same as previous
#' ## but note that we have to include any parametric terms, including the
#' ## constant term
#' predicted_samples(m,
#'   n_draws = 5, data = newd, seed = 25,
#'   terms = c("Intercept", "s(x0)", "s(x2)", "s(x3)")
#' )
#' \dontshow{
#' options(op)
#' }
`predicted_samples` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  sampling_check_prediction(substitute(list(...)))
  UseMethod("predicted_samples")
}

#' @export
#' @rdname predicted_samples
`predicted_samples.default` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  stop(
    "Don't know how to sample from the posterior of <",
    class(model)[[1L]],
    ">",
    .call = FALSE
  )
}

#' @export
#' @rdname predicted_samples
#' @importFrom tibble as_data_frame add_column
#' @importFrom tidyr gather
`predicted_samples.gam` <- function(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  seed = NULL,
  weights = NULL
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = TRUE)
  layout <- sampling_resolve(n_draws, NULL, NULL, method = "gaussian")
  n_draws <- layout$n_draws
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }

  do_predicted_samples(
    model = model,
    n = n_draws,
    data = data,
    seed = seed,
    weights = weights,
    ...
  )
}

#' @export
#' @rdname predicted_samples
`predicted_samples.scam` <- function(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  seed = NULL,
  weights = NULL
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = TRUE)
  layout <- sampling_resolve(n_draws, NULL, NULL, method = "gaussian")
  n_draws <- layout$n_draws
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }

  check_scam_unconditional(model, list(...)$unconditional %||% FALSE)
  do_predicted_samples(
    model = model,
    n = n_draws,
    data = data,
    seed = seed,
    weights = weights,
    ...
  )
}

`do_predicted_samples` <- function(
  model,
  n = 1,
  data = NULL,
  seed = NULL,
  weights = NULL,
  envir = NULL,
  ...
) {
  model <- with_model_envir(model, envir)
  simulate_fun <- if (inherits(model, "gam")) simulate.gam else simulate
  sims <- simulate_fun(
    model, nsim = n, seed = seed, data = data, weights = weights, ...
  )
  RNGstate <- attr(sims, "seed")
  row_ids <- attr(sims, "row_ids")
  class(sims) <- class(sims)[-1] # remove the "simulate_gratia" class
  if (
    is_multivariate_y(model) &&
      identical(family_type(model), "multivariate_normal")
  ) {
    # first column is .response for truly multivariate Y models
    sims <- as_tibble(sims)
    names(sims)[-1] <- as.character(seq_len(ncol(sims) - 1))
    n_eta <- n_eta(model)
    sims <- sims |>
      add_column(.row = rep(seq_len(nrow(sims) / n_eta), times = n_eta)) |>
      pivot_longer(
        cols = !c(".row", ".yvar"),
        names_to = ".draw",
        values_to = ".response",
        names_transform = list(.draw = as.integer)
      ) |>
      rename(.parameter = ".yvar") |>
      relocate(all_of(c(".row", ".draw")), .before = 1L)
  } else {
    # rewrite this so it is more like the above with pivot_longer
    colnames(sims) <- paste0(".V", seq_len(NCOL(sims)))
    sims <- as_tibble(sims)
    names(sims) <- as.character(seq_len(ncol(sims)))
    sims <- add_column(sims, .row = row_ids %||% seq_len(nrow(sims)))
    sims <- gather(sims, key = ".draw", value = ".response", -.row) |>
      mutate(.draw = as.integer(.data$.draw))
  }

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
#' @inheritSection derivatives Parallel calculation
#'
#' @details
#' For LSS models, each smooth is a contribution to its own linear predictor.
#' Select its full smooth label to distinguish predictors, for example `s(x)`
#' and `s.1(x)`. These are component draws on the predictor scale; use
#' [fitted_samples()] for the expected response and [derivative_samples()] for
#' its derivative. Coefficients are drawn jointly across all predictors.
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
#' * `.row`; integer. A vector of values `seq_len(nrow(data))`, repeated if
#'     `n_draws > 1L`. Indexes the row in `data` for that particular draw.
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
#' sms <- smooth_samples(m1, select = "s(x0)", n_draws = 5, seed = 42)
#' \donttest{
#' sms
#' }
#'
#' ## A factor by example (with a spurious covariate x0)
#' dat <- data_sim("eg4", n = 1000, seed = 2)
#'
#' ## fit model...
#' m2 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)
#' sms <- smooth_samples(m2, n_draws = 5, seed = 42)
#' draw(sms)
#' \dontshow{
#' options(op)
#' }
`smooth_samples` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  UseMethod("smooth_samples")
}

#' @export
`smooth_samples.default` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  stop(
    "Don't know how to sample from the posterior of <",
    class(model)[[1L]],
    ">",
    .call = FALSE
  )
}

#' @param n_grid positive integer; resolution for constructing evaluation data if
#'   `data` not supplied
#' @param select character; select which smooth's posterior to draw from.
#'   The default (`NULL`) means the posteriors of all smooths in `model`
#'   will be sampled from. If supplied, a character vector of requested terms.
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
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e6,
  select = NULL,
  n_grid = 200L,
  partial_match = NULL,
  envir = NULL
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = FALSE)
  sampling_check_options(
    match.arg(method),
    names(match.call(expand.dots = FALSE))[-1L],
    mh_backend = match.arg(mh_backend),
    mvn_method = match.arg(mvn_method)
  )
  layout <- sampling_resolve(n_draws, chains, draws, method = match.arg(method))
  n_draws <- layout$n_draws
  chains <- layout$chains
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }
  n_grid <- sampling_count(n_grid, "n_grid")
  n_threads <- sampling_count(n_threads, "n_threads")

  model_name <- deparse(substitute(model))
  model <- with_model_envir(model, envir)
  # smooth_samples begins
  if (is.null(seed)) {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) runif(1)
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    withr::local_seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
  }

  sms <- smooths(model) # vector of smooth labels - "s(x)"
  take <- seq_along(sms) # in default case 1,2,3,..,n smooths
  if (!is.null(select)) {
    select <- check_user_select_smooths(
      sms,
      select = select,
      partial_match = partial_match,
      model_name = model_name
    )
    # take <- which_smooths(model, term)
    take <- which(select) # need this as an id vector for things later
    sms <- sms[select] # S <- S[take]
  }

  # At least for now, don't work for random effect smooths
  # do we have ranef smooths?
  re_sms <- vapply(
    get_smooths_by_id(model, take),
    FUN = is_re_smooth,
    FUN.VALUE = logical(1L)
  )
  if (any(re_sms)) {
    message(
      "\nRandom effect smooths not currently supported.\nIgnoring:",
      " <",
      paste(sms[re_sms], collapse = ", "),
      ">\n"
    )
    sms <- sms[!re_sms]
    take <- take[!re_sms]
  }

  # do we have any remaining terms?
  if (length(sms) < 1L) {
    stop("No smooths left that can be sampled from.")
  }

  need_data <- FALSE
  if (is.null(data)) {
    need_data <- TRUE
  }

  # what posterior sampling are we using
  method <- match.arg(method)
  mvn_method <- match.arg(mvn_method)

  # get posterior draws - call this once for all parameters
  betas <- sampling_call(
    post_draws,
    list(
      n_draws = n_draws,
      method = method,
      n_threads = n_threads,
      n_cores = n_cores,
      chains = chains,
      diagnostics = diagnostics,
      mh_backend = mh_backend,
      block_size = block_size,
      dense_limit = dense_limit,
      model = model,
      burnin = burnin,
      thin = thin,
      t_df = t_df,
      rw_scale = rw_scale,
      index = NULL,
      frequentist = frequentist,
      unconditional = unconditional,
      draws = draws,
      seed = seed,
      mvn_method = mvn_method
    )
  )

  tasks <- vector("list", length = length(sms))
  for (i in seq_along(sms)) {
    if (need_data) {
      # FIXME: should offset be NULL?
      data <- smooth_data(model, id = take[i], n = n_grid, offset = NULL)
      # I don't think we need offset here as that really just shifts the
      # response around
    }
    sm <- get_smooths_by_id(model, take[i])[[1L]]
    idx <- smooth_coef_indices(sm)
    # Resolve calls before matrix construction and output-column selection.
    data <- prepare_smooth_data(model, sm, data)
    tasks[[i]] <- list(sm = smooth_task_smooth(sm),
      data = smooth_task_data(data[, intersect(names(data), terms_in_smooth(sm)), drop = FALSE]),
      model = smooth_matrix_model(model), betas = betas[, idx, drop = FALSE],
      label = sms[[i]],
      Xp = if (smooth_task_predict_in_caller(sm, model)) {
        smooth_predict_matrix(sm, data, model)
      } else NULL)
  }
  sims <- smooth_task_map(tasks, "calculate_smooth_samples")

  sims <- bind_rows(sims)
  sims <- unnest(sims, all_of("data"))
  attr(sims, "seed") <- RNGstate
  ## add classes
  class(sims) <- c("smooth_samples", class(sims))
  attach_sampling(sims, betas, draw_index = TRUE)
}

# All terms use columns of the same joint posterior draws.
calculate_smooth_samples <- function(sm, data, model, betas, label, Xp = NULL) {
  if (is.null(Xp)) Xp <- smooth_predict_matrix(sm, data, model)
  simu <- Xp %*% t(betas)
  # A prescribed boundary contributes a fixed value to every coefficient draw.
  off <- attr(Xp, "offset", exact = TRUE)
  if (!is.null(off)) simu <- simu + off
  colnames(simu) <- paste0("..V", seq_len(NCOL(simu)))
  simu <- as_tibble(simu)
  nr_simu <- nrow(simu)
  simu <- add_by_data(
    simu,
    by_name = by_variable(sm),
    by_data = data,
    before = 1L
  )
  # add on spline type info
  sm_type <- smooth_type(sm)
  simu <- add_column(
    simu,
    .smooth = rep(
      unlist(lapply(strsplit(label, ":"), `[`, 1L)),
      nr_simu
    ),
    .term = rep(label, each = nr_simu),
    .type = rep(sm_type, nr_simu),
    .row = seq_len(nr_simu),
    .after = 0L
  )
  simu <- bind_cols(simu, data[smooth_variable(sm)])
  simu <- pivot_longer(
    simu,
    cols = dplyr::starts_with("..V"),
    names_to = ".draw",
    values_to = ".value",
    names_transform = \(x) as.integer(sub("\\.\\.V", "", x))
  )
  simu <- relocate(simu, any_of(names(data)), .after = last_col()) |>
    relocate(".by", .after = 3L)
  ## nest all columns with varying data
  simu <- nest(
    simu,
    data = all_of(c(
      ".row",
      ".draw",
      ".value",
      vars_in_smooth(sm)
    ))
  )
  simu
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
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e6,
  select = NULL,
  n_grid = 200L,
  partial_match = NULL,
  envir = NULL
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = FALSE)
  sampling_check_options(
    match.arg(method),
    names(match.call(expand.dots = FALSE))[-1L],
    mh_backend = match.arg(mh_backend),
    mvn_method = match.arg(mvn_method)
  )
  layout <- sampling_resolve(n_draws, chains, draws, method = match.arg(method))
  n_draws <- layout$n_draws
  chains <- layout$chains
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }
  n_grid <- sampling_count(n_grid, "n_grid")
  n_threads <- sampling_count(n_threads, "n_threads")

  check_scam_unconditional(model, unconditional)
  model_name <- deparse(substitute(model))
  model <- with_model_envir(model, envir)
  # smooth_samples begins
  if (is.null(seed)) {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) runif(1)
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
    withr::local_seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
  }
  sms <- smooths(model) # vector of smooth labels - "s(x)"
  take <- seq_along(sms) # in default case 1,2,3,..,n smooths
  if (!is.null(select)) {
    select <- check_user_select_smooths(
      sms,
      select = select,
      partial_match = partial_match,
      model_name = model_name
    )
    # take <- which_smooths(model, term)
    take <- which(select) # need this as an id vector for things later
    sms <- sms[select] # S <- S[take]
  }

  # At least for now, don't work for random effect smooths
  # do we have ranef smooths?
  re_sms <- vapply(
    get_smooths_by_id(model, take),
    FUN = is_re_smooth,
    FUN.VALUE = logical(1L)
  )
  if (any(re_sms)) {
    message(
      "\nRandom effect smooths not currently supported.\nIgnoring:",
      " <",
      paste(sms[re_sms], collapse = ", "),
      ">\n"
    )
    sms <- sms[!re_sms]
    take <- take[!re_sms]
  }
  # do we have any remaining terms?
  if (length(sms) < 1L) {
    stop("No smooths left that can be sampled from.")
  }
  need_data <- FALSE
  if (is.null(data)) {
    need_data <- TRUE
  }
  # what posterior sampling are we using
  method <- match.arg(method)
  mvn_method <- match.arg(mvn_method)
  # get posterior draws - call this once for all parameters
  betas <- sampling_call(
    post_draws,
    list(
      n_draws = n_draws,
      method = method,
      n_threads = n_threads,
      n_cores = n_cores,
      chains = chains,
      diagnostics = diagnostics,
      mh_backend = mh_backend,
      block_size = block_size,
      dense_limit = dense_limit,
      model = model,
      burnin = burnin,
      thin = thin,
      t_df = t_df,
      rw_scale = rw_scale,
      index = NULL,
      frequentist = frequentist,
      unconditional = unconditional,
      draws = draws,
      mvn_method = mvn_method,
      seed = seed
    )
  )

  tasks <- vector("list", length = length(sms))
  for (i in seq_along(sms)) {
    if (need_data) {
      # FIXME: should offset be NULL?
      data <- smooth_data(model, id = take[i], n = n_grid, offset = NULL)
      # I don't think we need offset here as that really just shifts the
      # response around
    }
    sm <- get_smooths_by_id(model, take[i])[[1L]]
    idx <- smooth_coef_indices(sm)
    # Resolve calls before matrix construction and output-column selection.
    data <- prepare_smooth_data(model, sm, data)
    tasks[[i]] <- list(sm = smooth_task_smooth(sm),
      data = smooth_task_data(data[, intersect(names(data), terms_in_smooth(sm)), drop = FALSE]),
      model = smooth_matrix_model(model), betas = betas[, idx, drop = FALSE],
      label = sms[[i]],
      Xp = if (smooth_task_predict_in_caller(sm, model)) {
        smooth_predict_matrix(sm, data, model)
      } else NULL)
  }
  sims <- smooth_task_map(tasks, "calculate_smooth_samples")

  sims <- bind_rows(sims)
  sims <- unnest(sims, all_of("data"))
  attr(sims, "seed") <- RNGstate
  ## add classes
  class(sims) <- c("smooth_samples", class(sims))
  attach_sampling(sims, betas, draw_index = TRUE)
}

#' @export
`smooth_samples.gamm` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  smooth_samples(model$gam, ...)
}

#' @title Posterior expectations of derivatives from an estimated model
#'
#' @details
#' For LSS models, response-scale derivatives differentiate the expected response
#' returned by [fitted_samples()], retaining the joint uncertainty in every
#' parameter that affects that expectation. Predictor-scale derivatives refer
#' to the first linear predictor. Smooth-component derivatives and derivatives
#' of the response expectation are different quantities.
#'
#' @param model a fitted model to compute derivative samples for
#' @param ... arguments passed to other methods and on to `fitted_samples()`
#'
#' @author Gavin L. Simpson
#'
#' @export
`derivative_samples` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = TRUE)

  UseMethod("derivative_samples")
}

#' @rdname derivative_samples
#' @export
`derivative_samples.default` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = TRUE)

  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Don't know how to calculate response derivatives for <",
    class(model)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

#' @rdname derivative_samples
#'
#' @export
`derivative_samples.gamm` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = TRUE)

  derivative_samples(model[["gam"]], ...)
}

#' @param envir the environment within which to recreate the data used to fit
#'   `model`.
#' @param draws matrix; user supplied posterior draws to be used when
#'   `method = "user"`.
#' @param mvn_method character; one of `"mvnfast"` or `"mgcv"`. The default is
#'   uses `mvnfast::rmvn()`, which can be considerably faster at generate large
#'   numbers of MVN random values than `mgcv::rmvn()`, but which might not work
#'   for some marginal fits, such as those where the covariance matrix is close
#'   to singular.
#'
#' @inheritParams response_derivatives focal data order type scale eps
#' @inheritParams smooth_samples n_grid
#' @inheritParams post_draws n_draws n_cores n_threads burnin thin t_df rw_scale frequentist unconditional seed method
#' @inheritParams post_draws chains diagnostics mh_backend block_size dense_limit
#'
#' @export
#'
#' @rdname derivative_samples
#'
#' @return A tibble, currently with the following variables:
#' * `.derivative`: the estimated partial derivative,
#' * additional columns containing the covariate values at which the derivative
#'   was evaluated.
#'
#' @examples
#'
#' load_mgcv()
#' df <- data_sim("eg1", dist = "negbin", scale = 0.25, seed = 42)
#'
#' # fit the GAM (note: for execution time reasons using bam())
#' m <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
#'   data = df, family = nb(), method = "fREML")
#'
#' # data slice through data along x2 - all other covariates will be set to
#' # typical values (value closest to median)
#' ds <- data_slice(m, x2 = evenly(x2, n = 200))
#'
#' # samples from posterior of derivatives
#' fd_samp <- derivative_samples(m,
#'   data = ds, type = "central",
#'   focal = "x2", eps = 0.01, seed = 21, n_draws = 100
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
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e6,
  focal = NULL,
  order = 1L,
  type = c("forward", "backward", "central"),
  scale = c("response", "linear_predictor"),
  n_grid = 200L,
  eps = NULL,
  envir = environment(formula(model))
) {
  sampling_check_names(substitute(list(...)), derivative = TRUE)
  sampling_check_extra(substitute(list(...)), prediction = TRUE)
  sampling_check_options(
    match.arg(method),
    names(match.call(expand.dots = FALSE))[-1L],
    mh_backend = match.arg(mh_backend),
    mvn_method = match.arg(mvn_method)
  )
  layout <- sampling_resolve(n_draws, chains, draws, method = match.arg(method))
  n_draws <- layout$n_draws
  chains <- layout$chains
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }
  n_grid <- sampling_count(n_grid, "n_grid")
  n_threads <- sampling_count(n_threads, "n_threads")

  do_derivative_samples(
    n_cores = n_cores,
    burnin = burnin,
    thin = thin,
    t_df = t_df,
    rw_scale = rw_scale,
    frequentist = frequentist,
    unconditional = unconditional,
    n_threads = n_threads,
    object = model,
    focal = focal,
    data = data,
    order = order,
    type = type,
    scale = scale,
    method = method,
    n = n_grid,
    eps = eps,
    n_sim = n_draws,
    seed = seed,
    envir = envir,
    draws = draws,
    mvn_method = mvn_method,
    chains = chains,
    diagnostics = diagnostics,
    mh_backend = mh_backend,
    block_size = block_size,
    dense_limit = dense_limit,
    ...
  )
}

#' @export
#' @rdname derivative_samples
`derivative_samples.scam` <- function(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e6,
  focal = NULL,
  order = 1L,
  type = c("forward", "backward", "central"),
  scale = c("response", "linear_predictor"),
  n_grid = 200L,
  eps = NULL,
  envir = environment(formula(model))
) {
  sampling_check_names(substitute(list(...)), derivative = TRUE)
  sampling_check_extra(substitute(list(...)), prediction = TRUE)
  sampling_check_options(
    match.arg(method),
    names(match.call(expand.dots = FALSE))[-1L],
    mh_backend = match.arg(mh_backend),
    mvn_method = match.arg(mvn_method)
  )
  layout <- sampling_resolve(n_draws, chains, draws, method = match.arg(method))
  n_draws <- layout$n_draws
  chains <- layout$chains
  if (!is.null(data) && !is.data.frame(data)) {
    stop(
      "'data' must be a data frame; name the draw count with n_draws = ... .",
      call. = FALSE
    )
  }
  n_grid <- sampling_count(n_grid, "n_grid")
  n_threads <- sampling_count(n_threads, "n_threads")

  do_derivative_samples(
    n_cores = n_cores,
    burnin = burnin,
    thin = thin,
    t_df = t_df,
    rw_scale = rw_scale,
    frequentist = frequentist,
    unconditional = unconditional,
    n_threads = n_threads,
    object = model,
    focal = focal,
    data = data,
    order = order,
    type = type,
    scale = scale,
    method = method,
    n = n_grid,
    eps = eps,
    n_sim = n_draws,
    seed = seed,
    envir = envir,
    draws = draws,
    mvn_method = mvn_method,
    chains = chains,
    diagnostics = diagnostics,
    mh_backend = mh_backend,
    block_size = block_size,
    dense_limit = dense_limit,
    ...
  )
}

`do_derivative_samples` <- function(
  n_threads = 1L,
  object,
  focal = NULL,
  data = NULL,
  order = 1L,
  type = c("forward", "backward", "central"),
  scale = c("response", "linear_predictor"),
  method = c("gaussian", "mh", "inla", "user"),
  n = 100,
  eps = NULL,
  n_sim = 10000,
  seed = NULL,
  envir = environment(formula(object)),
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  chains = 1L,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  block_size = 10000L,
  dense_limit = 5e6,
  ...
) {
  object <- with_model_envir(object, envir)
  ## handle type
  type <- match.arg(type)
  ## handle method
  method <- match.arg(method)
  mvn_method <- match.arg(mvn_method)
  ## handle scale
  scale <- match.arg(scale)

  grid <- prepare_response_derivative_data(
    object,
    focal,
    data,
    n,
    eps,
    order,
    type
  )
  data <- grid$data
  fd_data <- grid$fd_data
  eps <- grid$eps

  ## compute posterior draws of E(y) (on response scale)
  fs <- sampling_call(
    fitted_samples,
    list(
      n_threads = n_threads,
      model = object,
      n_draws = n_sim,
      data = fd_data,
      method = method,
      seed = seed,
      draws = draws,
      mvn_method = mvn_method,
      scale = scale,
      chains = chains,
      diagnostics = diagnostics,
      mh_backend = mh_backend,
      block_size = block_size,
      dense_limit = dense_limit,
      ...
    )
  )

  sampling <- attr(fs, "sampling")
  fs <- fs |>
    left_join(
      select(fd_data, all_of(c(".row", "..type", "..orig"))),
      by = ".row"
    )

  yd <- compute_y_fdiff(fs, order = order, type = type, eps = eps)

  yd <- yd |>
    left_join(data, by = join_by("..orig" == ".row")) |>
    rename("{focal}" := ".x", ".derivative" = "..fd", ".row" = "..orig") |>
    select(!matches(c("..xf", "..xb", "..x"))) |>
    add_column(.focal = focal, .before = 1L) |>
    relocate(".row", .before = 1L)

  class(yd) <- append(class(yd), "derivative_samples", after = 0L)
  attach_sampling(yd, sampling)
}
