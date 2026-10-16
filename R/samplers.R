#' Low-level Functions to generate draws from the posterior distribution of
#' model coefficients
#'
#' @param model a fitted model from [mgcv::gam()], [mgcv::bam()],
#'   [scam::scam()], or [gamm4::gamm4()], or a model inheriting from these
#'   classes. See [gamm4_support] for sampling from gamm4 fits.
#' @param n_draws positive integer; total retained draws across all chains.
#'   Required when generating draws. With `method = "user"`, the supplied
#'   matrix determines the count; an explicit count must agree with its rows.
#' @param chains positive integer or `NULL`; number of chains. `NULL` uses one
#'   chain, or the chain mapping in supplied draws. `n_draws` must be divisible by
#'   `chains`. Burn-in is applied separately to each MH chain.
#' @param diagnostics logical; calculate coefficient R-hat, effective sample
#'   sizes, and Monte Carlo standard errors using the optional posterior package.
#' @param mh_backend character; `"mgcv"` preserves [mgcv::gam.mh()].
#'   `"gratia"` uses compiled common-family likelihoods. `"family"` uses a
#'   compiled MH loop with callbacks to the fitted family object. Both support
#'   independent-observation bam fits. Smoothing parameters and parameters
#'   outside the fitted coefficient vector remain fixed. LSS scale and shape
#'   coefficients are sampled jointly with location. The family backend caches
#'   a full design matrix,
#'   smoothing penalty, and proposal factorisation once for all chains.
#' @param block_size positive integer; maximum observation rows per block in the
#'   gratia MH evaluator. Large designs are evaluated without storing a full matrix.
#' @param dense_limit numeric; maximum number of design-matrix elements to cache
#'   for the gratia MH backend. Use zero to force blockwise evaluation.
#' @param method character; which algorithm to use to sample from the posterior.
#'   Currently implemented methods are: `"gaussian"` and `"mh"`. `"gaussian"`
#'   calls `gaussian_draws()` which uses a Gaussian approximation to the
#'   posterior distribution. `"mh"` uses a simple Metropolis Hastings sampler
#'   which alternates static proposals based on a Gaussian approximation to the
#'   posterior, with random walk proposals. Note, setting `t_df` to a low value
#'   will result in heavier-tailed statistic proposals. See `mgcv::gam.mh()`
#'   for more details.
#' @param n_cores positive integer; maximum concurrent MH chains using mirai,
#'   capped at `chains`. Existing workers are reused; otherwise temporary workers
#'   are created and cleaned up. Only used by `method = "mh"`.
#' @param n_threads positive integer; threads within Gaussian sampling using
#'   mvnfast. Defaults to one. This is separate from concurrent MH chains.
#' @param burnin numeric; the length of any initial burn in period to discard.
#'   See `mgcv::gam.mh()`.
#' @param thin positive integer; retain every `thin` iterations after burn-in.
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
#'   SCAM models do not support this correction; use `unconditional = FALSE`.
#' @param parametrized logical; for SCAM fits, transform Gaussian draws from
#'   the unrestricted fitting coefficients using the fitted positivity transform.
#'   This preserves the smooth's constraints. With `FALSE`, return Gaussian draws
#'   in the unrestricted fitting coordinates. Only for `scam::scam()` fits.
#' @param mvn_method character; one of `"mvnfast"` or `"mgcv"`. The default is
#'   uses `mvnfast::rmvn()`, which can be considerably faster at generate large
#'   numbers of MVN random values than `mgcv::rmvn()`, but which might not work
#'   for some marginal fits, such as those where the covariance matrix is close
#'   to singular.
#' @param draws matrix; user supplied posterior draws to be used when
#'   `method = "user"`.
#' @param seed numeric or `NULL`; seed for the complete sampling operation.
#'   An explicit seed restores the caller's random-number state on exit.
#' @param ... arguments passed to methods.
#'
#' @details
#' Sampling controls must be named. Removed arguments produce migration errors;
#' see `vignette("sampling-migration")`. Explicit controls that the selected
#' method cannot use are rejected.
#'
#' For SCAM models, Gaussian draws approximate the distribution of the unrestricted
#' fitting coefficients using `Vp` (or `Ve` with `frequentist = TRUE`), then apply
#' scam's fitted exponential or `notExp` transformation. This is an approximation
#' in fitting coordinates, rather than an exact constrained posterior. Supplied
#' draws must already use the transformed coordinates in `coef(model)` order.
#' Numerically singular covariance matrices are sampled by eigen decomposition;
#' negative eigenvalues within `100 * .Machine$double.eps * ncol(V)` times the
#' largest absolute eigenvalue are treated as zero. Larger negatives are errors.
#' For weakly identified exponential coefficients, a very large fitting-coordinate
#' variance can yield non-finite transformed draws. Sampling then reports an
#' error because this Gaussian approximation cannot provide usable draws; supply
#' finite transformed draws from a suitable alternative sampler.
#'
#' For multiple chains, `n_draws` is the total retained count and must be divisible
#' by `chains`. Coefficient matrices carry a chain mapping; use
#' [as_mcmc_draws()] before ordinary matrix operations remove that mapping.
#' Higher-level sampling tables include `.chain` and `.iteration` for multiple
#' chains, with `.draw` unique across chains. Default single-chain table columns
#' are unchanged. Use [sampling_diagnostics()] to extract coefficient diagnostics
#' before response simulation adds further noise.
#'
#' The gratia MH backend supports Gaussian identity, Poisson log, binomial
#' logit, Gamma log, and negative binomial log fits. It evaluates the fitted
#' bases at original covariates, including for discrete bam fits. GAMM components, AR1 dependence,
#' `paraPen`, `H`, and fits made from pre-fit `G` objects are unsupported.
#'
#' The family MH backend calls the fitted family's `dev.resids()` and `linkinv()`
#' functions, or `ll()` for families with multiple linear predictors. This
#' retains response encodings and attributes such as censoring indicators and
#' the mappings between coefficients and predictors. It supports compatible
#' standard, extended, and general mgcv families, including multinomial, Gaussian
#' location-scale, and grouped families. Quasi families, Cox proportional hazards
#' models, GAMM components, AR1 dependence, `paraPen`, `H`, and pre-fit `G` objects
#' are unsupported. The fitted coefficient covariance must be nonsingular.
#' It always caches the full design matrix; `block_size` and `dense_limit` apply
#' only to the gratia backend and are rejected for the family backend.
#' The new MH backends reject `bam` fits with nonzero prescribed smooth
#' contributions: mgcv fitting and prediction can use different offsets for
#' those fits. Supplied coefficient draws can still be evaluated with the
#' native prediction conventions.
#'
#' All MH backends sample the conditional coefficient posterior with fitted
#' smoothing, dispersion, and additional family parameters fixed. Parameters
#' represented in the fitted coefficient vector are sampled, including scale
#' or covariance coefficients in general families. `unconditional` applies
#' to Gaussian draws only.
#'
#' GAM-class fits with linear inequality constraints, including
#'   `mgcv::scasm()` fits, store these as `Ain %*% coefficients >= bin`.
#'   Gaussian sampling uses the fitted covariance matrix without enforcing
#'   these inequalities and issues a warning. Sampled smooths can therefore
#'   violate the fitted shape constraints. Pointwise intervals computed from
#'   the same Gaussian covariance approximation also need not preserve those
#'   shapes. All MH backends are unavailable for these fits because
#'   the samplers do not enforce their inequalities.
#'
#'   If `mgcv::scasm()` was fitted with `bs > 0`, its `bs` component stores
#'   coefficient draws from the native constrained bootstrap in columns. These
#'   can be supplied explicitly as `draws = t(model$bs)` with `method = "user"`.
#'   The result then describes bootstrap variation, rather than Gaussian
#'   posterior sampling. Gratia does not automatically select these draws.
#'   The caller is responsible for the interpretation and constraint
#'   feasibility of user-supplied draws.
#'
#' @export
`post_draws` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  UseMethod("post_draws")
}

#' @export
#' @rdname post_draws
#' @importFrom withr with_seed with_preserve_seed
`post_draws.default` <- function(
  model,
  ...,
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
  index = NULL,
  parametrized = TRUE
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

  n_threads <- sampling_count(n_threads, "n_threads")
  if (!inherits(model, "scam") && !missing(parametrized)) {
    stop(
      "Argument 'parametrized' is only supported for SCAM coefficient draws.",
      call. = FALSE
    )
  }
  check_scam_unconditional(model, unconditional)
  if (is.null(seed)) {
    seed <- with_preserve_seed(sample.int(.Machine$integer.max, 1L))
  }
  # what posterior sampling are we using
  method <- match.arg(method)
  ids <- if (method == "gaussian") sampling_layout(n_draws, chains) else NULL
  mvn_method <- match.arg(mvn_method)
  betas <- switch(
    method,
    "gaussian" = with_seed(
      seed,
      sampling_call(
        gaussian_draws,
        list(
          model = model,
          n_draws = n_draws,
          n_threads = n_threads,
          index = index,
          frequentist = frequentist,
          unconditional = unconditional,
          parametrized = parametrized,
          mvn_method = mvn_method,
          ...
        )
      )
    ),
    "mh" = with_seed(
      seed,
      sampling_call(
        mh_draws,
        list(
          n_draws = n_draws,
          model = model,
          burnin = burnin,
          thin = thin,
          t_df = t_df,
          rw_scale = rw_scale,
          index = index,
          chains = chains,
          n_cores = n_cores,
          diagnostics = diagnostics,
          mh_backend = mh_backend,
          block_size = block_size,
          dense_limit = dense_limit,
          ...
        )
      )
    ),
    "inla" = stop("'method = \"inla\"' is not yet implemented.", call. = FALSE),
    "user" = sampling_call(
      user_draws,
      list(model = model, draws = draws, index = index, ...)
    )
  )
  if (method == "gaussian") {
    nm <- names(coef(model))
    if (!is.null(index)) {
      nm <- nm[index]
    }
    betas <- sampling_finish(
      betas,
      ids,
      diagnostics = diagnostics,
      coefficient_names = nm
    )
  }
  if (method == "user") {
    # A supplied matrix determines the count, including its existing chain map.
    meta <- attr(betas, "sampling")
    if (
      chains > 1L &&
        (is.null(meta) || length(unique(meta$draw_ids$.chain)) != chains)
    ) {
      stop(
        "User draws must carry a matching chain mapping from post_draws().",
        call. = FALSE
      )
    }
    if (isTRUE(diagnostics)) {
      selected <- if (is.character(index)) match(index, colnames(draws)) else index
      coefficient_names <- names(coef(model))[selected %||% seq_len(ncol(betas))]
      betas <- sampling_finish(
        betas,
        meta$draw_ids %||% sampling_layout(nrow(betas), 1L),
        info = meta$chains,
        diagnostics = TRUE,
        coefficient_names = coefficient_names
      )
    }
  }
  betas
}
#' Generate posterior draws from a fitted model
#'
#' @export
#' @rdname post_draws
`generate_draws` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  UseMethod("generate_draws")
}

#' @export
#' @rdname post_draws
#' @importFrom withr with_seed with_preserve_seed
`generate_draws.gam` <- function(
  model,
  ...,
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
  index = NULL
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
  n_threads <- sampling_count(n_threads, "n_threads")

  sampling_call(
    post_draws,
    list(
      model,
      n_draws = n_draws,
      method = match.arg(method),
      draws = draws,
      n_threads = n_threads,
      n_cores = n_cores,
      burnin = burnin,
      thin = thin,
      t_df = t_df,
      rw_scale = rw_scale,
      index = index,
      frequentist = frequentist,
      unconditional = unconditional,
      mvn_method = match.arg(mvn_method),
      seed = seed,
      chains = chains,
      diagnostics = diagnostics,
      mh_backend = mh_backend,
      block_size = block_size,
      dense_limit = dense_limit,
      ...
    )
  )
}

#' Posterior samples using a Gaussian approximation
#'
#' @inheritParams post_draws
#' @inherit post_draws details
#'
#' @export
`gaussian_draws` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE, gaussian = TRUE)

  UseMethod("gaussian_draws")
}

#' @importFrom mvnfast rmvn
#' @export
#' @rdname gaussian_draws
`gaussian_draws.gam` <- function(
  model,
  ...,
  n_draws = NULL,
  n_threads = 1L,
  frequentist = FALSE,
  unconditional = FALSE,
  mvn_method = "mvnfast",
  index = NULL,
  seed = NULL,
  chains = NULL,
  diagnostics = FALSE
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE, gaussian = TRUE)
  sampling_check_extra(substitute(list(...)), prediction = FALSE)
  sampling_check_options(
    "gaussian",
    names(match.call(expand.dots = FALSE))[-1L],
    mvn_method = match.arg(mvn_method, c("mvnfast", "mgcv"))
  )
  layout <- sampling_resolve(n_draws, chains, NULL, method = "gaussian")
  n_draws <- layout$n_draws
  chains <- layout$chains
  n_threads <- sampling_count(n_threads, "n_threads")

  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  mvn_method <- match.arg(mvn_method, c("mvnfast", "mgcv"))
  if (has_inequality_constraints(model)) {
    warning("Gaussian draws do not enforce the fitted linear inequality ",
      "constraints; sampled smooths can violate the fitted shapes.",
      call. = FALSE)
  }
  mu <- coef(model) |> as.vector() # drop the empty dim if present
  sigma <- get_vcov(
    model,
    frequentist = frequentist,
    unconditional = unconditional
  )
  if (!is.null(index)) {
    mu <- mu[index]
    sigma <- sigma[index, index, drop = FALSE]
  }
  betas <- if (isTRUE(identical(mvn_method, "mvnfast"))) {
    tryCatch(
      mvnfast::rmvn(n = n_draws, mu = mu, sigma = sigma, ncores = n_threads),
      error = function(e) {
        if (
          !grepl(
            "chol(): decomposition failed",
            conditionMessage(e),
            fixed = TRUE
          )
        ) {
          stop(e)
        }
        warning(
          "Cholesky decomposition failed; using mgcv::rmvn().",
          call. = FALSE
        )
        mgcv::rmvn(n = n_draws, mu = mu, V = sigma)
      }
    )
  } else {
    mgcv::rmvn(n = n_draws, mu = mu, V = sigma)
  }
  # if we ask for n=1 samples, we need to get back a matrix
  if (!is.matrix(betas)) {
    betas <- matrix(betas, nrow = n_draws, byrow = TRUE)
  }

  sampling_finish(
    betas,
    sampling_layout(n_draws, chains),
    diagnostics = diagnostics,
    coefficient_names = names(coef(model))[index %||% seq_len(ncol(betas))]
  )
}

#' @importFrom mvnfast rmvn
#' @export
#' @rdname gaussian_draws
`gaussian_draws.scam` <- function(
  model,
  ...,
  n_draws = NULL,
  n_threads = 1L,
  frequentist = FALSE,
  unconditional = FALSE,
  mvn_method = "mvnfast",
  index = NULL,
  parametrized = TRUE,
  seed = NULL,
  chains = NULL,
  diagnostics = FALSE
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE, gaussian = TRUE)
  sampling_check_extra(substitute(list(...)), prediction = FALSE)
  sampling_check_options(
    "gaussian",
    names(match.call(expand.dots = FALSE))[-1L],
    mvn_method = match.arg(mvn_method, c("mvnfast", "mgcv"))
  )
  layout <- sampling_resolve(n_draws, chains, NULL, method = "gaussian")
  n_draws <- layout$n_draws
  chains <- layout$chains
  n_threads <- sampling_count(n_threads, "n_threads")

  flags <- list(parametrized = parametrized, frequentist = frequentist)
  for (name in names(flags)) {
    value <- flags[[name]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) stop(
      "`", name, "` must be a single non-missing logical value.", call. = FALSE)
  }

  check_scam_unconditional(model, unconditional)
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  mvn_method <- match.arg(mvn_method, c("mvnfast", "mgcv"))
  mu <- coef.scam(model, parametrized = FALSE)
  # scam registers its own vcov.scam with a different parameterisation argument.
  # Read fitting-coordinate covariances directly so package load order cannot
  # silently select the transformed covariance.
  sigma <- if (isTRUE(frequentist)) model$Ve else model$Vp
  dimnames(sigma) <- list(names(mu), names(mu))
  constrained <- stats::setNames(model$p.ident, names(mu))
  if (!is.null(index)) {
    mu <- mu[index]
    sigma <- sigma[index, index, drop = FALSE]
  }
  if (!is.null(index)) constrained <- constrained[index]
  betas <- scam_gaussian_matrix(n_draws, mu, sigma, mvn_method, n_threads)
  if (isTRUE(parametrized) && any(constrained)) {
    transform <- if (isTRUE(model$not.exp)) {
      function(x) getFromNamespace("notExp", "scam")(x,
        b = model$control$b.notexp %||% 1,
        threshold = model$control$threshold.notexp %||% 20)
    } else exp
    betas[, constrained] <- transform(betas[, constrained, drop = FALSE])
    if (any(!is.finite(betas))) stop(
      "SCAM coefficient transformation overflowed; the Gaussian approximation produces non-finite draws.",
      call. = FALSE)
  }
  attr(betas, "scam_parameterization") <- if (isTRUE(parametrized))
    "transformed" else "unrestricted"
  sampling_finish(
    betas,
    sampling_layout(n_draws, chains),
    diagnostics = diagnostics,
    coefficient_names = names(coef(model))[index %||% seq_len(ncol(betas))]
  )
}

#' Posterior samples using Metropolis Hastings
#'
#' @inheritParams post_draws
#' @inherit post_draws details
#'
#' @export
#' @rdname mh_draws
`mh_draws` <- function(
  model,
  ...
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  UseMethod("mh_draws")
}

#' @importFrom mgcv gam.mh
#' @rdname mh_draws
#' @export
`mh_draws.gam` <- function(
  model,
  ...,
  n_draws = NULL,
  chains = NULL,
  n_cores = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  block_size = 10000L,
  dense_limit = 5e6,
  index = NULL,
  seed = NULL
) {
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }

  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = FALSE)
  sampling_check_options(
    "mh",
    names(match.call(expand.dots = FALSE))[-1L],
    mh_backend = match.arg(mh_backend)
  )
  layout <- sampling_resolve(n_draws, chains, NULL, method = "mh")
  n_draws <- layout$n_draws
  chains <- layout$chains

  if (has_inequality_constraints(model)) {
    stop("Metropolis-Hastings sampling is not supported for models with ",
      "linear inequality constraints because the MH samplers do not enforce ",
      "them. Gaussian draws are an unconstrained approximation.",
      call. = FALSE)
  }
  if (isTRUE(attr(model, "gratia.gamm4"))) {
    stop(
      "Metropolis-Hastings sampling is not supported for gamm4 models; ",
      "use Gaussian or user-supplied draws.",
      call. = FALSE
    )
  }
  ids <- sampling_layout(n_draws, chains)
  chains <- sampling_count(chains, "chains")
  n_cores <- sampling_count(n_cores, "n_cores")
  burnin <- sampling_count(burnin, "burnin", zero = TRUE)
  thin <- sampling_count(thin, "thin")
  if (n_draws * thin + burnin > .Machine$integer.max) {
    stop("Too many MH iterations.", call. = FALSE)
  }
  if (
    length(t_df) != 1L ||
      !is.finite(t_df) ||
      t_df <= 0 ||
      length(rw_scale) != 1L ||
      is.na(rw_scale)
  ) {
    stop(
      "'t_df' must be positive and finite; 'rw_scale' must be one non-missing number.",
      call. = FALSE
    )
  }
  if (
    length(diagnostics) != 1L || is.na(diagnostics) || !is.logical(diagnostics)
  ) {
    stop(
      "'diagnostics' must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  if (diagnostics) {
    sampling_require("posterior")
  }
  mh_backend <- match.arg(mh_backend)
  if (mh_backend == "mgcv" && inherits(model, "bam")) {
    stop(
      "mgcv::gam.mh() does not support bam fits; use mh_backend = 'gratia' or 'family'.",
      call. = FALSE
    )
  }
  if (length(dense_limit) != 1L || !is.finite(dense_limit) || dense_limit < 0) {
    stop(
      "'dense_limit' must be non-negative and finite.",
      call. = FALSE
    )
  }
  target <- if (mh_backend == "family") {
    sampling_family_target(model)
  } else if (mh_backend == "gratia") {
    sampling_target(
      model,
      block_size = block_size,
      dense_limit = dense_limit,
      compiled = TRUE
    )
  } else {
    NULL
  }
  # Preserve legacy single-chain random draws. Multiple chains use assigned
  # streams, independent of worker count or scheduling.
  streams <- if (chains == 1L) {
    list(NULL)
  } else {
    withr::with_preserve_seed(sampling_streams(chains))
  }
  tasks <- lapply(seq_len(chains), function(i) {
    list(chain = i, stream = streams[[i]])
  })
  args <- list(
    model = if (is.null(target)) model else NULL,
    target = target,
    n = n_draws / chains,
    burnin = burnin,
    thin = thin,
    t_df = t_df,
    rw_scale = rw_scale,
    dispersed = chains > 1L
  )
  if (chains > 1L && n_cores > 1L) {
    result <- sampling_parallel(tasks, args, min(chains, n_cores))
  } else {
    result <- do.call(
      lapply,
      c(list(X = tasks, FUN = sampling_mh_worker), args)
    )
  }
  betas <- do.call(rbind, lapply(result, `[[`, "bs"))
  info <- do.call(rbind, lapply(result, `[[`, "info"))
  betas <- sampling_finish(
    betas,
    ids,
    info = info,
    diagnostics = diagnostics,
    coefficient_names = names(coef(model))
  )
  if (!is.null(index)) {
    meta <- attr(betas, "sampling")
    betas <- betas[, index, drop = FALSE]
    attr(betas, "sampling") <- meta
  }
  attr(betas, "fixed_acceptance") <- info$fixed_acceptance
  attr(betas, "rw_acceptance") <- info$rw_acceptance
  betas
}

sampling_mh_worker <- function(
  task,
  model,
  target,
  n,
  burnin,
  thin,
  t_df,
  rw_scale,
  dispersed
) {
  run <- function() {
    started <- proc.time()[["elapsed"]]
    if (is.null(target)) {
      invisible(utils::capture.output(
        out <- mgcv::gam.mh(
          model,
          ns = max(2L, n * thin),
          burn = burnin,
          thin = thin,
          t.df = t_df,
          rw.scale = rw_scale
        )
      ))
      out$bs <- matrix(out$bs, ncol = length(coef(model)))
      if (n == 1L && burnin == 0L && thin == 1L) {
        out$bs <- out$bs[2L, , drop = FALSE]
      } else {
        out$bs <- out$bs[seq_len(n), , drop = FALSE]
      }
    } else {
      initial <- target$beta
      if (dispersed) {
        for (attempt in seq_len(100L)) {
          initial <- target$beta +
            drop(stats::rnorm(length(initial)) %*% sampling_factor(target)) * 2
          if (is.finite(sampling_target_eval(target, initial))) break
        }
      }
      out <- sampling_mh(
        target,
        n,
        burnin,
        thin,
        t_df,
        rw_scale,
        initial = initial
      )
    }
    list(
      bs = out$bs,
      info = data.frame(
        .chain = task$chain,
        fixed_acceptance = out$accept,
        rw_acceptance = out$rw.accept,
        elapsed = proc.time()[["elapsed"]] - started,
        backend = if (is.null(target)) "mgcv" else target$backend,
        retained = n,
        burnin = burnin,
        thin = thin,
        t_df = t_df,
        rw_scale = rw_scale
      )
    )
  }
  if (is.null(task$stream)) {
    run()
  } else {
    withr::with_preserve_seed({
      assign(".Random.seed", task$stream, .GlobalEnv)
      run()
    })
  }
}

#' Handle user-supplied posterior draws
#'
#' @details The supplied `draws` must be a matrix (currently), with 1 column per
#'   model coefficient, and 1 row per posterior draw. The `"gam"` method has
#'   argument `index`, which can be used to subset (select) coefficients
#'   (columns) of `draws`. `index` can be any valid way of selecting (indexing)
#'   columns of a matrix. `index` is useful if you have a set of posterior draws
#'   for the entire model (say from [mgcv::gam.mh()]) and you wish to use those
#'   draws for an individual smooth, via [smooth_samples()].
#'
#' @inheritParams post_draws
#' @export
`user_draws` <- function(
  model,
  ...,
  draws
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)

  UseMethod("user_draws")
}

#' @param index a vector to index (subset) the columns of `draws`.
#'
#' @export
#' @rdname user_draws
`user_draws.gam` <- function(
  model,
  ...,
  n_draws = NULL,
  chains = NULL,
  draws,
  index = NULL
) {
  sampling_check_names(substitute(list(...)), derivative = FALSE)
  sampling_check_extra(substitute(list(...)), prediction = FALSE)
  layout <- sampling_resolve(n_draws, chains, draws, method = "user")
  n_draws <- layout$n_draws
  chains <- layout$chains

  # draws must be a matrix
  if (!is.matrix(draws)) {
    stop("Supplied 'draws' is not a matrix of coefficients.", call. = FALSE)
  }

  # draws must have as many columns as model coefficients
  n_coef <- length(coef(model))
  n_col <- ncol(draws)
  if (isFALSE(identical(n_col, n_coef))) {
    stop(
      "Supplied 'draws' doesn't match number of model coefficients.\n",
      "Number of model coefs: ",
      n_coef,
      "\n",
      "Number of columns in 'draws': ",
      n_col,
      "\n",
      call. = FALSE
    )
  }

  # if index provided, subset the draws
  if (!is.null(index)) {
    meta <- attr(draws, "sampling")
    fx_a <- attr(draws, "fixed_acceptance")
    rw_a <- attr(draws, "rw_acceptance")
    draws <- draws[, index, drop = FALSE]
    if (!is.null(meta)) {
      attr(draws, "sampling") <- meta
    }
    if (!is.null(fx_a)) {
      attr(draws, "fixed_acceptance") <- fx_a
      attr(draws, "rw_acceptance") <- rw_a
    }
  }

  # return the user-supplied draws
  draws
}

#' @details For SCAM models, supplied draws must contain finite transformed
#'   coefficients in the order returned by `coef(model)`. If column names are
#'   supplied they must match exactly. Entries identified by `model$p.ident`
#'   must be nonnegative. Draws returned with `parametrized = FALSE` cannot be
#'   supplied directly for prediction; transform them first.
#' @export
#' @rdname user_draws
user_draws.scam <- function(
  model,
  ...,
  draws = NULL,
  n_draws = NULL,
  chains = NULL,
  index = NULL
) {
  sampling_check_names(substitute(list(...)))
  if (identical(attr(draws, "scam_parameterization"), "unrestricted")) stop(
    "SCAM user draws must use transformed coefficients; these draws use unrestricted fitting coordinates.",
    call. = FALSE)
  if (!is.matrix(draws) || !is.numeric(draws) || any(!is.finite(draws))) stop(
    "SCAM user draws must be a finite numeric matrix of transformed coefficients.",
    call. = FALSE)
  if (!is.null(colnames(draws)) &&
      !identical(colnames(draws), names(coef(model)))) stop(
    "SCAM user draw columns must match coef(model) names and order.", call. = FALSE)
  if (ncol(draws) == length(coef(model)) &&
      any(draws[, model$p.ident, drop = FALSE] < 0)) stop(
    "SCAM user draws contain negative coefficients constrained to be nonnegative.",
    call. = FALSE)
  user_draws.gam(model, ..., draws = draws, n_draws = n_draws,
    chains = chains, index = index)
}

#' @export
#' @rdname post_draws
#' @method generate_draws scam
generate_draws.scam <- post_draws

#' @export
#' @rdname mh_draws
mh_draws.scam <- function(model, ...) {
  stop("Metropolis Hastings sampling is not implemented for SCAM models; use method = 'gaussian' or transformed user draws.",
    call. = FALSE)
}

# Check the covariance before selecting a backend. Singular positive semidefinite
# matrices need an eigen factor because the Cholesky backend requires full rank.
scam_gaussian_matrix <- function(n, mu, V, method, n_threads) {
  p <- length(mu)
  if (!is.matrix(V) || !identical(dim(V), c(p, p)) ||
      any(!is.finite(V)) || any(!is.finite(mu))) stop(
    "SCAM Gaussian sampling requires finite coefficients and a matching covariance matrix.",
    call. = FALSE)
  scale <- max(abs(V))
  tolerance <- 100 * .Machine$double.eps * max(1L, p)
  if (max(abs(V - t(V))) > tolerance * scale) stop(
    "SCAM coefficient covariance is not symmetric.", call. = FALSE)
  V <- (V + t(V)) / 2
  decomposition <- eigen(V, symmetric = TRUE)
  eigen_scale <- max(abs(decomposition$values))
  cutoff <- tolerance * eigen_scale
  if (any(decomposition$values < -cutoff)) stop(
    "SCAM coefficient covariance has materially negative eigenvalues.", call. = FALSE)
  if (all(decomposition$values > cutoff)) {
    draws <- if (method == "mvnfast") {
      mvnfast::rmvn(n, mu, V, ncores = n_threads)
    } else mgcv::rmvn(n, mu, V)
    return(matrix(draws, nrow = n, ncol = p))
  }
  values <- pmax(decomposition$values, 0)
  factor <- sweep(decomposition$vectors, 2L, sqrt(values), `*`)
  z <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  sweep(z %*% t(factor), 2L, mu, `+`)
}

# Detect inequalities on the fitted coefficients, rather than a model class: one
# GAM class covers both ordinary gam() and constrained scasm() fits.
has_inequality_constraints <- function(model) {
  A <- model$Ain
  b <- model$bin
  is.matrix(A) && is.numeric(A) && nrow(A) > 0L &&
    ncol(A) == length(coef(model)) && is.numeric(b) &&
    length(b) == nrow(A) && all(is.finite(A)) && all(is.finite(b)) &&
    any(A != 0)
}
