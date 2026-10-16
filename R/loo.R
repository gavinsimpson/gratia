#' Leave-one-out validation and WAIC for GAMs
#'
#' These methods evaluate the likelihood of each fitting observation at
#' posterior coefficient draws and return the usual objects from \pkg{loo}.
#' Neither method changes [stats::logLik()] for fitted GAMs.
#'
#' @param x a GAM or BAM fitted by \pkg{mgcv}, or an independent-response
#'   SCAM fitted by \pkg{scam}.
#' @param n_draws total posterior coefficient draws across all chains. Defaults
#'   to 4000 when generating draws; supplied draws determine their own count.
#' @param seed random seed for posterior sampling.
#' @param sampling_args named list of arguments passed to the coefficient
#'   sampler, including `chains`, `diagnostics`, `method`, `mh_backend` and
#'   method-specific controls; see [log_lik()].
#' @param r_eff optional relative effective sample sizes, either one value or
#'   one per observation. Independent Gaussian draws use `1` by default.
#'   For Metropolis--Hastings draws this is estimated from the likelihoods.
#'   Supplied draws use their stored chain mapping when available; otherwise
#'   supply `r_eff` or `chain_id`.
#' @param chain_id integer chain identifier for each coefficient draw, in
#'   draw order. Identifiers must be consecutive starting at one, and chains
#'   must contain equal numbers of draws. A supplied mapping must agree with
#'   the coefficient draws' stored mapping. Stored iterations determine temporal
#'   order for relative efficiency without reordering response or likelihood rows.
#' @param save_psis retain the PSIS object in the returned LOO result.
#' @param cores number of cores used by \pkg{loo}.
#' @param ... additional arguments passed to [loo::loo()] or [loo::waic()].
#'
#' @details
#' The Gaussian posterior approximation and fitted values of additional family
#' parameters are described in [log_lik()]. PSIS diagnostics assess the
#' importance weights; they do not measure the accuracy of that approximation.
#' With random-effect smooths, leaving out individual observations predicts
#' further observations in the fitted groups, rather than new groups.
#'
#' Likelihoods are evaluated one observation at a time using the function
#' interface in \pkg{loo}. Saving PSIS weights still requires storage
#' proportional to the number of observations times the number of draws.
#'
#' @return A native `loo` or `waic` object, with attributes `gratia` describing
#'   posterior sampling assumptions and `observation` identifying fitting rows.
#' @examples
#' if (requireNamespace("loo", quietly = TRUE)) {
#'   library("mgcv")
#'   m <- gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
#'   loo::loo(m, n_draws = 500, seed = 42)
#' }
#' @name loo.gam
NULL

#' @rdname loo.gam
#' @exportS3Method loo::loo gam
loo.gam <- function(x, ..., n_draws = NULL, seed = NULL, sampling_args = list(),
                    r_eff = NULL, chain_id = NULL, save_psis = FALSE,
                    cores = 1) {
  sampling_check_names(substitute(list(...)))
  posterior_likelihood_require_loo()
  prep <- posterior_likelihood_prepare(x, n = n_draws, seed = seed,
    sampling_args = sampling_args)
  posterior_likelihood_loo(prep, r_eff = r_eff, chain_id = chain_id,
    save_psis = save_psis, cores = cores, ...)
}

#' @rdname loo.gam
#' @exportS3Method loo::loo bam
loo.bam <- loo.gam

#' @rdname loo.gam
#' @exportS3Method loo::waic gam
waic.gam <- function(x, ..., n_draws = NULL, seed = NULL, sampling_args = list()) {
  sampling_check_names(substitute(list(...)))
  posterior_likelihood_require_loo()
  prep <- posterior_likelihood_prepare(x, n = n_draws, seed = seed,
    sampling_args = sampling_args)
  if (nrow(prep$betas) < 2L) {
    stop("WAIC requires at least two posterior draws.", call. = FALSE)
  }
  result <- loo::waic(posterior_likelihood_loo_loglik,
    data = data.frame(.gratia_row = seq_along(prep$rows)), draws = prep, ...)
  posterior_likelihood_loo_attributes(result, prep)
}

#' @rdname loo.gam
#' @exportS3Method loo::waic bam
waic.bam <- waic.gam

posterior_likelihood_require_loo <- function() {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("Package 'loo' is required for this operation. Install it with ",
      "install.packages(\"loo\").", call. = FALSE)
  }
  invisible(TRUE)
}

posterior_likelihood_loo_loglik <- function(data_i, draws) {
  ll <- as.vector(posterior_likelihood_evaluate(draws, data_i$.gratia_row))
  bad <- which(!is.finite(ll))
  if (length(bad)) {
    stop("LOO and WAIC require finite log likelihoods; observation ",
      draws$rows[data_i$.gratia_row], ", draw ", bad[1L],
      " has a non-finite value.", call. = FALSE)
  }
  ll
}

posterior_likelihood_loo_likelihood <- function(data_i, draws) {
  ll <- posterior_likelihood_loo_loglik(data_i, draws)
  if (!is.null(draws$efficiency_order)) ll <- ll[draws$efficiency_order]
  # A common scale factor leaves relative efficiency unchanged and avoids
  # underflow when the observation has a very small likelihood.
  exp(ll - max(ll))
}

posterior_likelihood_loo_attributes <- function(result, prep,
                                              observations = NULL) {
  attr(result, "gratia") <- prep$metadata
  attr(result, "observation") <- if (is.null(observations)) prep$rows else
    prep$rows[observations]
  labels <- rownames(prep$X) %||% as.character(prep$rows)
  rownames(result$pointwise) <- if (is.null(observations)) labels else
    labels[observations]
  attach_sampling(result, prep$betas)
}

posterior_likelihood_loo <- function(prep, save_psis = FALSE, r_eff = NULL,
                                   chain_id = NULL, cores = 1,
                                   observations = NULL, ...) {
  posterior_likelihood_require_loo()
  n_draws <- nrow(prep$betas)
  if (is.null(observations)) observations <- seq_along(prep$rows)
  if (!is.numeric(observations) || !length(observations) ||
      anyNA(observations) || any(!is.finite(observations)) ||
      any(observations != as.integer(observations)) ||
      any(observations < 1L | observations > length(prep$rows)) ||
      anyDuplicated(observations)) {
    stop("'observations' must identify distinct retained observation positions.",
      call. = FALSE)
  }
  n_obs <- length(observations)
  data <- data.frame(.gratia_row = observations)
  if (n_draws < 2L) {
    stop("LOO requires at least two posterior draws.", call. = FALSE)
  }
  ids <- attr(prep$betas, "sampling")$draw_ids
  if (!is.null(chain_id)) {
    posterior_likelihood_validate_chains(chain_id, n_draws)
    if (!is.null(ids) && !identical(as.integer(chain_id), as.integer(ids$.chain)))
      stop("'chain_id' conflicts with the stored coefficient chain mapping.", call. = FALSE)
  }
  if (is.null(r_eff)) {
    if (identical(prep$method, "gaussian")) {
      r_eff <- 1
    } else {
      if (is.null(chain_id)) {
        if (!is.null(ids)) {
          chain_id <- ids$.chain
        } else if (identical(prep$method, "mh")) {
          chain_id <- rep.int(1L, n_draws)
        } else {
          stop("Supply 'chain_id' or 'r_eff' for user-supplied draws.",
            call. = FALSE)
        }
      }
      posterior_likelihood_validate_chains(chain_id, n_draws)
      efficiency_prep <- prep
      if (!is.null(ids)) {
        efficiency_prep$efficiency_order <- order(ids$.chain, ids$.iteration)
        chain_id <- chain_id[efficiency_prep$efficiency_order]
      }
      r_eff <- loo::relative_eff(posterior_likelihood_loo_likelihood,
        chain_id = chain_id, data = data, draws = efficiency_prep, cores = cores)
    }
  }
  if (!is.numeric(r_eff) || !length(r_eff) %in% c(1L, n_obs) ||
      anyNA(r_eff) || any(!is.finite(r_eff)) || any(r_eff <= 0)) {
    stop("'r_eff' must contain positive finite values, one or one per ",
      "observation.", call. = FALSE)
  }
  result <- loo::loo(posterior_likelihood_loo_loglik,
    data = data, draws = prep, r_eff = r_eff, save_psis = save_psis,
    cores = cores, ...)
  posterior_likelihood_loo_attributes(result, prep, observations)
}

posterior_likelihood_validate_chains <- function(chain_id, n_draws) {
  valid <- is.numeric(chain_id) && length(chain_id) == n_draws &&
    !anyNA(chain_id) && all(is.finite(chain_id)) &&
    all(chain_id >= 1) && all(chain_id == as.integer(chain_id))
  if (valid) {
    ids <- sort(unique(chain_id))
    valid <- identical(as.integer(ids), seq_along(ids)) &&
      length(unique(as.integer(table(chain_id)))) == 1L
  }
  if (!valid) {
    stop("'chain_id' must identify each draw using consecutive positive ",
      "integers starting at one, with equal numbers of draws per chain.",
      call. = FALSE)
  }
  invisible(TRUE)
}
