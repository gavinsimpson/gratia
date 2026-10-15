#' Posterior checks with bayesplot
#'
#' Compare observations with replicated responses using an optional installation
#' of bayesplot. Call `bayesplot::pp_check()` after loading gratia.
#'
#' @param object a fitted GAM, BAM, gamm, or gamm4 model.
#' @param type character; the bayesplot plot name without its `ppc_` or `ppd_`
#'   prefix. Available names come from the installed version of bayesplot.
#' @param prefix character; `"ppc"` compares responses with observations;
#'   `"ppd"` shows replicated responses alone.
#' @param n positive integer; number of coefficient and response draws. Defaults
#'   to 50 for ordinary plots and 4000 for leave-one-out plots. With supplied
#'   coefficient draws, defaults to their number of rows.
#' @param seed integer or `NULL`; seed for reproducible sampling.
#' @param group,x a variable name in the fitting model frame, or a vector in
#'   fitting-row order. Missing fitting rows are removed with the response.
#' @param response character or integer; response column or grouped-family
#'   component to check, for example `"response2"` or `"component1"`. Required
#'   when the model contains several responses or grouped components.
#' @param conditioning character or `NULL`; conditioning passed to the existing
#'   posterior predictive sampler. See [posterior_predictive_plot()].
#' @param sampling_args named list; arguments for the posterior sampler, including
#'   `method`, `draws`, `unconditional`, and `mvn_method`. Prediction at new data
#'   is not supported. Fitted binomial trials must be retained.
#'   For leave-one-out plots, supply `r_eff` or `chain_id` here when using
#'   coefficient draws supplied by the user.
#' @param ... arguments passed only to the selected bayesplot plotting function.
#'
#' @details
#' These methods require bayesplot only when called. Leave-one-out plots also
#' require loo and a likelihood supported by [log_lik()]. Their response and
#' likelihood calculations use exactly the same coefficient draws. For a
#' multivariate response, the weights leave out the entire response vector.
#' Grouped-family checks use only the selected component's observations.
#'
#' Some bayesplot plotting functions, including `error_binned` and LOO-PIT
#' plots, also require bayesplot's optional rstantools package. Install it with
#' `install.packages("rstantools")` to use those plots.
#'
#' Binomial observations and replicated responses are shown as numbers of
#' successes with fitted trial counts. Other gratia predictive plots use
#' proportions. The `error_binned` plot uses expected responses rather than
#' response simulations and requires an available finite response mean. For
#' gamm and gamm4 it currently requires `conditioning = "gam_component"`.
#' Plotting functions requiring additional inputs, such
#' as censoring indicators, receive those inputs through `...`.
#'
#' Coefficient draws use a Gaussian approximation by default; additional family
#' parameters remain at fitted values. See [log_lik()] for the assumptions of
#' approximate leave-one-out calculations. Existing conditioning and response
#' dependence restrictions of the diagnostic samplers also apply here.
#'
#' @return The object returned by the selected bayesplot function, usually a
#'   ggplot object.
#' @seealso [posterior_predictive_plot()], [log_lik()]
#' @name pp_check
#' @examples
#' if (requireNamespace("bayesplot", quietly = TRUE)) {
#'   load_mgcv()
#'   dat <- data_sim("eg1", n = 100, seed = 2)
#'   fit <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
#'   bayesplot::pp_check(fit, n = 10, seed = 42)
#' }
NULL

#' @rdname pp_check
#' @exportS3Method bayesplot::pp_check gam
pp_check.gam <- function(object, type = "dens_overlay", prefix = c("ppc", "ppd"),
    n = NULL, seed = NULL, group = NULL, x = NULL, response = NULL,
    conditioning = NULL, sampling_args = list(), ...) {
  prepared <- bayesplot_prepare(object, type, prefix, n, seed, group, x,
    response, conditioning, sampling_args, list(...))
  diagnostic_seed(seed, do.call(prepared$fun, prepared$args))
}

#' @rdname pp_check
#' @exportS3Method bayesplot::pp_check bam
pp_check.bam <- pp_check.gam

#' @rdname pp_check
#' @exportS3Method bayesplot::pp_check gamm
pp_check.gamm <- pp_check.gam

#' @rdname pp_check
#' @exportS3Method bayesplot::pp_check gamm4
pp_check.gamm4 <- pp_check.gam

bayesplot_prepare <- function(object, type, prefix, n, seed, group, x,
    response, conditioning, sampling_args, dots) {
  if (!requireNamespace("bayesplot", quietly = TRUE)) stop(
    "Package 'bayesplot' is required for posterior checks with bayesplot.", call. = FALSE)
  prefix <- match.arg(prefix, c("ppc", "ppd"))
  available <- if (prefix == "ppc") bayesplot::available_ppc() else
    bayesplot::available_ppd()
  valid <- sub(paste0("^", prefix, "_"), "", as.character(available))
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !type %in% valid) stop("Unknown bayesplot type. Available ", prefix,
    " types: ", paste(valid, collapse = ", "), ".", call. = FALSE)
  fun <- getExportedValue("bayesplot", paste0(prefix, "_", type))
  formal_names <- names(formals(fun))
  needs_loo <- any(c("lw", "psis_object") %in% formal_names)
  if (!is.list(sampling_args) || (length(sampling_args) &&
      (is.null(names(sampling_args)) || anyNA(names(sampling_args)) ||
       any(!nzchar(names(sampling_args))) || anyDuplicated(names(sampling_args)))))
    stop("'sampling_args' must be a named list with unique names.", call. = FALSE)
  n <- n %||% if (identical(sampling_args$method, "user") &&
      !is.null(sampling_args$draws)) NROW(sampling_args$draws) else
    if (needs_loo) 4000L else 50L
  diagnostic_posterior_count(n)
  if (identical(sampling_args$method, "user") &&
      !is.null(sampling_args$draws) && n != NROW(sampling_args$draws)) stop(
    "'n' must equal the number of rows in supplied coefficient draws.", call. = FALSE)
  forbidden <- intersect(names(sampling_args), c("model", "n", "seed", "data",
    "newdata", "conditioning", "weights", ".response_weights"))
  if (length(forbidden)) stop("Do not supply ", paste(forbidden, collapse = ", "),
    " in 'sampling_args': bayesplot checks use fitting observations and trial counts.",
    call. = FALSE)
  reserved <- intersect(names(dots), c("y", "yrep", "ypred", "lw", "psis_object"))
  if (length(reserved)) stop("Arguments ", paste(reserved, collapse = ", "),
    " are prepared from the fitted model and cannot be supplied in '...'.", call. = FALSE)
  if (needs_loo && !requireNamespace("loo", quietly = TRUE)) stop(
    "Package 'loo' is required for leave-one-out bayesplot checks.", call. = FALSE)
  diagnostic_seed(seed, {
    observed <- diagnostic_posterior_observed(object, conditioning = conditioning)
    parameters <- unique(observed$.parameter)
    selected <- bayesplot_response(response, parameters)
    observed <- observed[observed$.parameter == selected, , drop = FALSE]
    observed <- observed[!is.na(observed$.response), , drop = FALSE]
    if (!nrow(observed) || any(!is.finite(observed$.response))) stop(
      "The model must have finite observed responses.", call. = FALSE)
    frame_model <- if (inherits(object, c("gamm", "gamm4"))) object$gam else object
    frame <- stats::model.frame(frame_model)
    fitting_rows <- which(!is.na(model_row_map(frame_model)))
    selected_positions <- match(observed$.row, fitting_rows)
    if (anyNA(selected_positions)) stop("Cannot align fitting observations.", call. = FALSE)
    args <- list()
    if ("group" %in% formal_names) {
      if (is.null(group)) stop("'group' is required for bayesplot type '", type,
        "'.", call. = FALSE)
      args$group <- bayesplot_covariate(group, frame, fitting_rows,
        selected_positions, "group", length(model_row_map(frame_model)))
    }
    if ("x" %in% formal_names && !is.null(x))
      args$x <- bayesplot_covariate(x, frame, fitting_rows, selected_positions, "x", length(model_row_map(frame_model)))
    loo_args <- sampling_args[intersect(names(sampling_args), c("r_eff", "chain_id"))]
    sampling <- sampling_args[setdiff(names(sampling_args), names(loo_args))]
    prep <- NULL
    response_seed <- seed
    if (needs_loo) {
      check_distribution_conditioning(conditioning)
      coefficient_seed <- seed %||% sample.int(.Machine$integer.max, 1L)
      prep <- posterior_likelihood_prepare(object, n, coefficient_seed, sampling)
      response_seed <- sample.int(.Machine$integer.max, 1L)
      sampling$method <- "user"
      sampling$draws <- prep$betas
    }
    response_sampling <- sampling
    raw_trials <- bayesplot_fitted_trials(object)
    if (!is.null(raw_trials)) {
      if (inherits(object, c("gamm", "gamm4")) &&
          !identical(conditioning, "gam_component")) {
        response_sampling$.response_weights <- raw_trials
      } else {
        weights <- rep(NA_real_, length(model_row_map(frame_model)))
        weights[fitting_rows] <- raw_trials
        response_sampling$weights <- weights
      }
    }
    sims <- if (identical(type, "error_binned")) {
      bayesplot_expected_samples(object, n, seed, conditioning, sampling,
        response = selected)
    } else if (!is.null(prep) &&
        identical(family_type(prep$model), "multivariate_normal")) {
      diagnostic_seed(response_seed, bayesplot_mvn_samples(prep, n))
    } else {
      do.call(diagnostic_posterior_samples, c(list(model = object, n = n,
        seed = response_seed, conditioning = conditioning), response_sampling))
    }
    if (!".parameter" %in% names(sims)) sims$.parameter <- "response"
    sims <- sims[sims$.parameter == selected, , drop = FALSE]
    sims <- posterior_predictive_validate(sims, observed, seq_len(n))
    ordered <- order(sims$.draw, match(sims$.row, observed$.row))
    replicated <- matrix(sims$.response[ordered], nrow = n, byrow = TRUE)
    trials <- bayesplot_trials(object, selected, selected_positions)
    y <- observed$.response * trials
    replicated <- sweep(replicated, 2L, trials, `*`)
    if (prefix == "ppc") {
      args$y <- y
      args$yrep <- replicated
    } else args$ypred <- replicated
    if (needs_loo) {
      positions <- match(observed$.row, prep$rows)
      if (length(loo_args$r_eff) == length(prep$rows))
        loo_args$r_eff <- loo_args$r_eff[positions]
      result <- do.call(posterior_likelihood_loo, c(list(prep = prep,
        save_psis = TRUE, observations = positions), loo_args))
      if ("lw" %in% formal_names) args$lw <- stats::weights(result$psis_object,
        log = TRUE) else args$psis_object <- result$psis_object
    }
    list(fun = fun, args = c(args, dots), rows = observed$.row)
  })
}

bayesplot_response <- function(response, parameters) {
  if (is.null(response)) {
    if (length(parameters) != 1L) stop("Select a 'response': ",
      paste(parameters, collapse = ", "), ".", call. = FALSE)
    return(parameters[[1L]])
  }
  if (is.numeric(response) && length(response) == 1L && is.finite(response) &&
      response == floor(response) && response >= 1L) {
    prefix <- if (all(startsWith(parameters, "component"))) "component" else "response"
    candidate <- paste0(prefix, response)
    if (candidate %in% parameters) response <- candidate else
      if (identical(parameters, "response") && response == 1L) response <- "response"
  }
  if (!is.character(response) || length(response) != 1L || is.na(response) ||
      !response %in% parameters) stop("Unknown 'response'. Choose from: ",
    paste(parameters, collapse = ", "), ".", call. = FALSE)
  response
}

bayesplot_covariate <- function(value, frame, rows, positions, label, n_original) {
  if (is.character(value) && length(value) == 1L) {
    if (!value %in% names(frame)) stop("Variable '", value,
      "' is absent from the fitting model frame.", call. = FALSE)
    value <- frame[[value]]
  } else if (length(value) == n_original && length(value) != nrow(frame)) {
    value <- value[rows]
  }
  if (length(value) != nrow(frame) || !is.null(dim(value))) stop("'", label,
    "' must name a model-frame variable or contain one value per fitting row.",
    call. = FALSE)
  value <- value[positions]
  if (anyNA(value)) stop("'", label, "' must not contain missing fitting values.",
    call. = FALSE)
  value
}

bayesplot_trials <- function(object, response, positions) {
  if (inherits(object, c("gamm", "gamm4"))) {
    if (inherits(object, "gamm4")) {
      model <- object$mer
      fam <- stats::family(model)
      wt <- if (family_type(fam) == "binomial") stats::weights(model, type = "prior") else NULL
    } else {
      model <- model_used_rows(object$gam)
      fam <- stats::family(model)
      wt <- model$prior.weights
    }
  } else {
    model <- model_used_rows(object)
    fam <- stats::family(model)
    wt <- model$prior.weights
  }
  if (is_gfam_family(fam)) {
    component <- as.integer(sub("^component", "", response))
    fam <- gfam_prediction_context(model)$families[[component]]
    if (family_type(fam) != "binomial") return(rep(1, length(positions)))
    return((wt %||% rep(1, nrow(model$model)))[positions])
  }
  if (family_type(fam) != "binomial") return(rep(1, length(positions)))
  raw <- stats::model.response(stats::model.frame(model))
  if (is.matrix(raw)) return(unname(rowSums(raw)[positions]))
  binomial_sampling_weights(model, wt %||% rep(1, NROW(stats::model.frame(model))))[positions]
}

# Count responses record trials separately from any frequency fitting weights.
bayesplot_fitted_trials <- function(object) {
  model <- if (inherits(object, "gamm4")) object$mer else
    if (inherits(object, "gamm")) object$gam else object
  if (family_type(stats::family(model)) != "binomial") return(NULL)
  raw <- stats::model.response(stats::model.frame(model))
  if (!is.matrix(raw)) return(NULL)
  trials <- unname(rowSums(raw))
  if (length(trials) != NROW(raw) || any(!is.finite(trials)) ||
      any(trials < 0 | trials != floor(trials))) stop(
    "Paired binomial responses must contain nonnegative integer trial counts.",
    call. = FALSE)
  trials
}

bayesplot_expected_samples <- function(object, n, seed, conditioning, sampling_args,
                                      response = NULL) {
  if (inherits(object, c("gamm", "gamm4"))) {
    conditioning <- mixed_conditioning(object, conditioning)
    if (conditioning != "gam_component") stop("The 'error_binned' plot currently ",
      "requires conditioning = 'gam_component' for gamm and gamm4 models.", call. = FALSE)
    object <- if (inherits(object, "gamm4")) gamm4_gam(object) else object$gam
    conditioning <- NULL
  }
  check_distribution_conditioning(conditioning %||% "conditional")
  rows <- which(!is.na(model_row_map(object)))
  model <- model_used_rows(object)
  f <- stats::family(model)
  if (family_type(f) == "gumbls" && native_mgcv_hook(f$ll, mgcv::gumbls()$ll) &&
      (is.null(f$gratia_diagnostics) || is.function(f$gratia_diagnostics))) {
    factory <- f$gratia_diagnostics
    f$gratia_diagnostics <- function(mu, wt, scale, y, eta) {
      extra <- if (is.function(factory))
        factory(mu = mu, wt = wt, scale = scale, y = y, eta = eta) else list()
      # Gumbel's location differs from its expectation. Retain an explicit
      # family mean, including NULL, when its diagnostic hook supplies one.
      if (is.list(extra) && !"mean" %in% names(extra))
        extra$mean <- mu[, 1L] - digamma(1) * exp(mu[, 2L])
      extra
    }
  }
  grouped <- is_gfam_family(f)
  if (family_type(f) == "multinom" ||
      grepl("^ordered_categorical", family_type(f))) stop(
    "The 'error_binned' plot is unavailable for categorical responses.", call. = FALSE)
  X <- if (grouped) gfam_predict_eta(model, model$model, "lpmatrix") else
    predict_model(model, newdata = model$model, type = "lpmatrix")
  index <- attr(X, "lpi") %||% lss_eta_index(model)
  base <- vapply(index, function(j) drop(X[, j, drop = FALSE] %*%
    stats::coef(model)[j]), numeric(nrow(X)))
  offsets <- matrix(model$linear.predictors, nrow = nrow(X)) - base
  args <- sampling_args
  if (!is.null(args$freq)) {
    args$frequentist <- args$freq
    args$freq <- NULL
  }
  betas <- do.call(post_draws, c(list(model = model, n = n, seed = seed), args))
  context <- if (grouped) gfam_diagnostic_context(model) else NULL
  components <- if (grouped && !is.null(response))
    as.integer(sub("^component", "", response)) else if (grouped)
      seq_along(context$families) else integer()
  if (grouped && any(vapply(context$families[components], function(family)
      grepl("^ordered_categorical", family_type(family)), logical(1)))) stop(
    "The 'error_binned' plot is unavailable for categorical responses.", call. = FALSE)
  dplyr::bind_rows(lapply(seq_len(n), function(i) {
    eta <- vapply(index, function(j) drop(X[, j, drop = FALSE] %*%
      betas[i, j]), numeric(nrow(X))) + offsets
    if (grouped) {
      context$eta <- as.numeric(eta)
      for (j in components) {
        at <- which(context$index == j)
        context$mu[at] <- context$families[[j]]$linkinv(context$eta[at])
      }
      mean <- numeric(nrow(X))
      for (j in components) {
        at <- which(context$index == j)
        distribution <- gfam_component_distribution(context, j)
        mean[at] <- bayesplot_distribution_mean(distribution)
      }
      parameter <- paste0("component", context$index)
      keep <- which(context$index %in% components)
      return(diagnostic_response_table(mean[keep], i, parameter[keep], rows[keep]))
    } else {
      latent <- grepl("^ordered_categorical|^zero_inflated_poisson", family_type(f))
      mu <- if (latent) as.numeric(eta) else family_prediction_parameters(f, eta)
      distribution <- distribution_from_family(f, mu,
        binomial_sampling_weights(model, model$prior.weights), model$sig2,
        y = model$y, eta = eta)
      mean <- bayesplot_distribution_mean(distribution)
      parameter <- "response"
    }
    diagnostic_response_table(mean, i, parameter, rows)
  }))
}

bayesplot_distribution_mean <- function(distribution) {
  if (!distribution_moment_available(distribution, "mean") ||
      is.null(distribution$mean) || any(!is.finite(distribution$mean))) stop(
    "The 'error_binned' plot requires finite expected responses. ",
    distribution$moment_reason %||% "An analytic mean is unavailable for this family.",
    call. = FALSE)
  distribution$mean
}

# A multivariate normal fit stores precision parameters among its coefficients.
# LOO weights and response draws must use the same drawn precision matrix.
bayesplot_mvn_samples <- function(prep, n) {
  d <- n_eta(prep$model)
  fits <- fitted_samples(prep$model, n = n, method = "user", draws = prep$betas)
  dplyr::bind_rows(lapply(seq_len(n), function(i) {
    mu <- vapply(paste0("response", seq_len(d)), function(parameter)
      fits$.fitted[fits$.draw == i & fits$.parameter == parameter],
      numeric(length(prep$rows)))
    R <- posterior_likelihood_mvn_R(prep$betas[i, ], d)
    values <- matrix(stats::rnorm(length(mu)), nrow = nrow(mu)) %*%
      t(solve(R)) + mu
    diagnostic_response_table(values, i, rows = prep$rows)
  }))
}
