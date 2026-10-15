# Each fitter supplies observations and a response generator. Shared plotting
# and summary functions use the same four-column table for every fitter.
diagnostic_posterior_samples <- function(model, n, seed = NULL,
                                        conditioning = NULL, ...) {
  UseMethod("diagnostic_posterior_samples")
}

#' @export
diagnostic_posterior_samples.default <- function(model, n, seed = NULL,
                                                conditioning = NULL, ...) {
  diagnostic_posterior_count(n)
  dots <- list(...)
  if (!is.null(dots$data) || !is.null(dots$newdata)) stop(
    "Posterior diagnostic sampling uses fitting observations; new data are not supported.",
    call. = FALSE)
  diagnostic_seed(seed, {
    sampler <- diagnostic_posterior_sampler(model, n = n, seed = seed,
      conditioning = conditioning, ...)
    dplyr::bind_rows(lapply(seq_len(n), sampler))
  })
}

#' @export
diagnostic_posterior_samples.gam <- function(model, n, seed = NULL,
                                            conditioning = NULL, ...) {
  check_distribution_conditioning(conditioning)
  fam <- stats::family(model)
  ft <- family_type(fam)
  if (!is_gfam_family(fam) && (!inherits(fam, "general.family") ||
      ft %in% c("multivariate_normal", "multinom"))) {
    return(diagnostic_seed(seed, posterior_samples(model, n = n,
      seed = seed, ...)))
  }
  diagnostic_posterior_samples.default(model, n, seed, conditioning, ...)
}

#' @export
diagnostic_posterior_samples.scam <- diagnostic_posterior_samples.gam

diagnostic_posterior_sampler <- function(model, n, seed = NULL,
                                        conditioning = NULL, ...) {
  UseMethod("diagnostic_posterior_sampler")
}

#' @export
diagnostic_posterior_sampler.default <- function(model, n, seed = NULL,
                                                conditioning = NULL, ...) {
  stop("Posterior predictive checks are not available for <",
    class(model)[[1L]], "> models.", call. = FALSE)
}

# Draw coefficients together, then generate one response vector at a time.
# For p coefficients, n draws and N observations, memory is O(p*n + N*p);
# no N-by-n response matrix is stored. Batch size does not change draw order.
#' @export
diagnostic_posterior_sampler.gam <- function(model, n, seed = NULL,
                                            conditioning = NULL, ...) {
  check_distribution_conditioning(conditioning)
  row_map <- model_row_map(model)
  rows <- which(!is.na(row_map))
  model <- model_used_rows(model)
  fam <- stats::family(model)
  ft <- family_type(fam)
  if (ft %in% c("cnorm", "clog", "cpois")) {
    stop("Posterior predictive checks are not available for family <",
      family_name(fam), "> without a censoring observation process.", call. = FALSE)
  }
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) stats::runif(1L)
  dots <- list(...)
  if (!is.null(dots$weights)) {
    if (length(dots$weights) == 1L) dots$weights <- rep(dots$weights, length(row_map))
    if (length(dots$weights) != length(row_map)) stop(
      "Weights must have one value per fitting observation.", call. = FALSE)
    dots$weights <- dots$weights[rows]
  }
  sampling_names <- c("method", "n_cores", "burnin", "thin", "t_df",
    "rw_scale", "unconditional", "draws", "mvn_method")
  args <- dots[intersect(names(dots), sampling_names)]
  if (!is.null(dots$freq)) args$frequentist <- dots$freq
  coefficient_seed <- if (identical(args$method, "user")) NULL else
    sample.int(.Machine$integer.max, 1L)
  betas <- do.call(post_draws, c(list(model = model, n = n,
    seed = coefficient_seed), args))
  grouped <- is_gfam_family(fam)
  advanced <- grouped || (inherits(fam, "general.family") &&
    !ft %in% c("multivariate_normal", "multinom"))
  if (identical(ft, "multivariate_normal")) {
    return(diagnostic_mvn_posterior_sampler(model, betas, rows))
  }
  if (!advanced) {
    response_args <- dots[setdiff(names(dots), c(sampling_names, "freq"))]
    return(function(i) {
      out <- do.call(posterior_samples, c(list(model = model, n = 1L,
        method = "user", draws = betas[i, , drop = FALSE]), response_args))
      out$.draw <- i
      out$.row <- rows[out$.row]
      if (!".parameter" %in% names(out)) out$.parameter <- "response"
      out
    })
  }
  X <- if (grouped) gfam_predict_eta(model, model$model, type = "lpmatrix") else
    predict_model(model, newdata = model$model, type = "lpmatrix")
  idx <- attr(X, "lpi") %||% lss_eta_index(model)
  # Compare fitted linear predictors with X times the fitted coefficients to
  # recover offsets, including those supplied through fitting arguments.
  eta0 <- model$linear.predictors %||% if (grouped)
    gfam_predict_eta(model, model$model, type = "link") else stats::predict(model, type = "link")
  eta0 <- matrix(eta0, nrow = nrow(X))
  base_eta <- vapply(idx, function(j) drop(X[, j, drop = FALSE] %*%
    stats::coef(model)[j]), numeric(nrow(X)))
  offsets <- eta0 - base_eta
  wt <- dots$weights %||% binomial_sampling_weights(model,
    model$prior.weights %||% rep(1, nrow(X)))
  scale <- model$sig2 %||% summary(model)$dispersion %||% 1
  if (grouped) {
    context <- gfam_prediction_context(model)
    generators <- lapply(context$components, function(component) {
      if (family_type(component$family) %in% c("cnorm", "clog", "cpois")) {
        stop("Posterior predictive checks are not available for censored ",
          "grouped components without an observation process.", call. = FALSE)
      }
      rd <- fix_family_rd(component$family)$rd
      if (!is.function(rd)) stop("No response generator for grouped component <",
        component$family$family, ">.", call. = FALSE)
      rd
    })
    parameter <- paste0("component", context$index)
  } else {
    rd <- fix_family_rd(fam)$rd
    if (!is.function(rd)) stop("Posterior predictive checks are not available for family <",
      family_name(fam), ">: no response generator.", call. = FALSE)
    parameter <- "response"
  }
  function(i) {
    eta <- vapply(idx, function(j) drop(X[, j, drop = FALSE] %*%
      betas[i, j]), numeric(nrow(X))) + offsets
    if (grouped) {
      response <- gfam_response_draw(as.numeric(eta), wt, context, generators)
    } else {
      # Use the family's prediction function when it returns every distribution
      # parameter. If it returns only E[Y], transform each predictor using its
      # own inverse link instead (for example, ziplss).
      mu <- family_prediction_parameters(fam, eta)
      response <- rd(mu = mu, wt = wt, scale = scale)
    }
    diagnostic_response_table(response, i, parameter = parameter,
      rows = rows)
  }
}

#' @export
diagnostic_posterior_sampler.scam <- diagnostic_posterior_sampler.gam

#' @export
diagnostic_posterior_sampler.lm <- function(model, n, seed = NULL,
                                           conditioning = NULL, ...) {
  check_distribution_conditioning(conditioning)
  diagnostic_gaussian_posterior_options(list(...), class(model)[[1L]])
  used <- model_used_rows(model)
  beta <- stats::coef(used)
  keep <- is.finite(beta)
  X <- stats::model.matrix(used)[, keep, drop = FALSE]
  V <- stats::vcov(used)[keep, keep, drop = FALSE]
  draws <- diagnostic_gaussian_draws(n, mu = beta[keep], V = V)
  offset <- stats::model.offset(stats::model.frame(used)) %||% rep(0, nrow(X))
  wt <- if (inherits(model, "glm")) used$prior.weights else
    stats::model.weights(stats::model.frame(used))
  wt <- binomial_sampling_weights(used, wt %||% rep(1, nrow(X)))
  fam <- stats::family(used)
  if (startsWith(fam$family, "quasi")) stop(
    "Posterior predictive checks require a response distribution; quasi families ",
    "do not define one.", call. = FALSE)
  scale <- summary(used)$dispersion %||% summary(used)$sigma^2
  rd <- fix_family_rd(fam)$rd
  if (!is.function(rd)) stop("Posterior predictive checks are not available for family <",
    fam$family, ">: no response generator.", call. = FALSE)
  rows <- which(!is.na(model_row_map(model)))
  function(i) {
    mu <- fam$linkinv(as.numeric(X %*% draws[i, ]) + offset)
    diagnostic_response_table(rd(mu, wt, scale), i, rows = rows)
  }
}

#' @export
diagnostic_posterior_sampler.glm <- diagnostic_posterior_sampler.lm

# Format all draws in one table, whether responses come from coefficient draws
# or the fitter's simulator, and whether there is one response or several.
diagnostic_response_table <- function(response, draw, parameter = "response",
                                      rows = seq_len(NROW(response))) {
  rows <- unname(as.integer(rows))
  if (is.matrix(response)) {
    parameter <- paste0("response", seq_len(NCOL(response)))
    return(tibble::tibble(.row = rep(rows, NCOL(response)), .draw = draw,
      .parameter = rep(parameter, each = NROW(response)),
      .response = as.vector(response)))
  }
  tibble::tibble(.row = rows, .draw = draw, .parameter = parameter,
    .response = as.numeric(response))
}

diagnostic_posterior_observed <- function(model, conditioning = NULL, ...) {
  UseMethod("diagnostic_posterior_observed")
}

#' @export
diagnostic_posterior_observed.default <- function(model, conditioning = NULL, ...) {
  prepared <- diagnostic_data(model, type = "response", conditioning = conditioning)
  dat <- prepared$data
  out <- tibble::tibble(.row = dat$.row, .draw = 0L,
    .parameter = if (length(unique(dat$.response)) > 1L)
      paste0("response", dat$.response) else "response", .response = dat$observed)
  attr(out, "discrete") <- setNames(rep(FALSE, length(unique(out$.parameter))),
    unique(out$.parameter))
  out
}

#' @export
diagnostic_posterior_observed.gam <- function(model, conditioning = NULL, ...) {
  original <- model
  model <- model_used_rows(model)
  fam <- stats::family(model)
  ft <- family_type(fam)
  if (ft %in% c("cnorm", "clog", "cpois")) stop(
    "Posterior predictive checks are not available for family <", family_name(fam),
    "> without a censoring observation process.", call. = FALSE)
  y <- model$y %||% stats::model.response(stats::model.frame(model))
  grouped <- is_gfam_family(fam)
  multivariate <- identical(ft, "multivariate_normal")
  if (ft == "binomial") {
    if (is.matrix(y)) y <- y[, 1L] / (rowSums(y) + as.numeric(rowSums(y) == 0))
    if (is.factor(y)) y <- as.numeric(y != levels(y)[1L])
  }
  if (grouped) y <- if (is.matrix(y)) y[, 1L] else y
  if (!is.numeric(y) || (!multivariate && !is.null(dim(y)))) {
    stop("The model must have a numeric response on the sampling scale.", call. = FALSE)
  }
  rows <- which(!is.na(model_row_map(original)))
  parameter <- if (grouped) paste0("component", gfam_prediction_context(model)$index) else
    "response"
  out <- diagnostic_response_table(y, 0L, parameter, rows)
  types <- if (grouped) vapply(gfam_prediction_context(model)$families,
    family_type, character(1)) else ft
  discrete <- types %in% c("poisson", "negative_binomial", "binomial", "multinom") |
    grepl("^ordered_categorical|^zero_inflated_poisson", types)
  attr(out, "discrete") <- setNames(rep_len(discrete, if (grouped) length(types) else
    length(unique(out$.parameter))), if (grouped)
    paste0("component", seq_along(types)) else unique(out$.parameter))
  out
}

#' @export
diagnostic_posterior_observed.scam <- diagnostic_posterior_observed.gam
#' @export
diagnostic_posterior_observed.lm <- diagnostic_posterior_observed.gam
#' @export
diagnostic_posterior_observed.glm <- diagnostic_posterior_observed.gam

# Raw draws and diagnostic plots use the same response generators.
#' @export
#' @rdname posterior_samples
#' @details
#' The `lm`, `glm`, `glmmTMB`, `GJRM::gamlss`, `sdmTMB`, and full mixed-model
#' diagnostic samplers currently use the fitting observations only. They draw
#' coefficients together using Gaussian approximations. See
#' [posterior_predictive_plot()] for which additional distribution parameters
#' remain fixed and how random effects are treated for each fitter.
posterior_samples.lm <- function(model, n = 1L, seed = NULL, ...) {
  diagnostic_posterior_samples.default(model, n, seed, ...)
}
#' @export
#' @rdname posterior_samples
posterior_samples.glm <- posterior_samples.lm

#' @export
diagnostic_posterior_observed.glmmTMB <- function(model, conditioning = NULL, ...) {
  frame <- stats::model.frame(model)
  y <- stats::model.response(frame)
  fam <- stats::family(model)
  if (fam$family %in% c("binomial", "betabinomial", "combinomial")) {
    if (is.matrix(y)) y <- y[, 1L] / (rowSums(y) + as.numeric(rowSums(y) == 0))
    if (is.factor(y)) y <- as.numeric(y != levels(y)[1L])
  }
  out <- diagnostic_response_table(y, 0L,
    rows = which(!is.na(model_row_map(model))))
  attr(out, "discrete") <- setNames(fam$family %in% c("poisson", "binomial",
    "nbinom1", "nbinom2", "compois", "genpois", "betabinomial", "combinomial", "ordinal",
    "truncated_poisson", "truncated_nbinom1", "truncated_nbinom2"), "response")
  attr(out, "diagnostic_description") <- if (identical(conditioning, "marginal")) paste(
    "Marginal Gaussian coefficient draws and new native effects;",
    "effect covariance and other family parameters plugged in at fitted values.") else paste(
      "Joint Gaussian coefficient/fitted-effect draws marginalize covariance-parameter estimation;",
      "other family parameters plugged in at fitted values.")
  out
}

# Take the required coefficients and effects from TMB's full joint Gaussian
# approximation. Their covariance retains uncertainty from estimating parameters
# that are not returned. Generating responses at given random-effect values
# does not require their prior covariance; generating new effects uses the fitted
# prior covariance. Other family parameters remain at fitted values.
#' @export
diagnostic_posterior_sampler.glmmTMB <- function(model, n, seed = NULL,
                                                conditioning = NULL, ...) {
  diagnostic_gaussian_posterior_options(list(...), class(model)[[1L]])
  conditioning <- match.arg(conditioning %||% "conditional", c("conditional", "marginal"))
  rows <- which(!is.na(model_row_map(model)))
  model <- glmmtmb_used_fit(model)
  fam <- stats::family(model)
  # Check that responses can be generated before computing joint covariance.
  if (conditioning == "conditional") {
    distribution <- response_distribution(model, conditioning = "conditional")
    if (!is.function(distribution$rd)) stop("No response generator for glmmTMB family <",
      fam$family, ">.", call. = FALSE)
  }
  p <- model$fit$parfull
  tags <- if (conditioning == "conditional")
    c("beta", "b", "betazi", "bzi", "betadisp", "bdisp") else
    c("beta", "betazi", "betadisp")
  index <- which(names(p) %in% tags)
  draws <- diagnostic_joint_parameter_draws(p, n, model$sdr,
    report = function() TMB::sdreport(model$obj, getJointPrecision = TRUE),
    label = "glmmTMB", index = index)
  native <- model$obj$env$data
  component <- function(draw, fixed, random, x, z, offset) {
    X <- native[[x]]
    if (is.null(X) || !ncol(X)) X <- native[[paste0(x, "S")]]
    Z <- native[[z]]
    out <- rep_len(native[[offset]] %||% 0, nrow(model$frame))
    fi <- which(names(p) == fixed)
    ri <- which(names(p) == random)
    if (length(fi)) out <- out + as.numeric(X %*% draw[fi])
    if (length(ri)) out <- out + as.numeric(Z %*% draw[ri])
    out
  }
  y <- stats::model.response(model$frame)
  trials <- if (is.matrix(y)) rowSums(y) else if (is.factor(y)) rep(1, length(y)) else
    stats::model.weights(model$frame) %||% rep(1, length(y))
  power <- if (fam$family == "tweedie") unname(glmmTMB::family_params(model))[1L] else NULL
  function(i) {
    draw <- p
    draw[index] <- draws[i, ]
    if (conditioning == "marginal") {
      response <- model$obj$simulate(par = draw)$yobs
      if (fam$family %in% c("binomial", "betabinomial", "combinomial"))
        response <- response / (trials + as.numeric(trials == 0))
      return(diagnostic_response_table(response, i, rows = rows))
    }
    mu <- fam$linkinv(component(draw, "beta", "b", "X", "Z", "offset"))
    eta_disp <- component(draw, "betadisp", "bdisp", "Xdisp", "Zdisp", "dispoffset")
    # glmmTMB >= 1.1.15 models log residual SD for Gaussian responses.
    # Its Gamma dispersion predictions are 1/sqrt(shape).
    phi <- exp(eta_disp * if (fam$family == "Gamma") -0.5 else 1)
    zprob <- if (any(names(p) == "betazi") || any(names(p) == "bzi"))
      stats::plogis(component(draw, "betazi", "bzi", "Xzi", "Zzi", "zioffset")) else 0
    distribution <- glmmtmb_response_distribution(fam$family, mu, phi,
      trials = trials, power = power, zprob = zprob)
    diagnostic_response_table(distribution$rd(), i, rows = rows)
  }
}

#' @export
#' @rdname posterior_samples
posterior_samples.glmmTMB <- function(model, n = 1L, seed = NULL,
                                      conditioning = NULL, ...) {
  diagnostic_posterior_samples.default(model, n, seed, conditioning, ...)
}

# Extract fitting observations without requiring an analytic response CDF.
# Native posterior simulations can cover more families than residual diagnostics.
sdmtmb_posterior_observations <- function(model, component = "response") {
  component <- match.arg(component, c("response", "binomial", "positive"))
  delta <- isTRUE(model$family$delta)
  if (!delta && component != "response") stop(
    "Component posterior checks require a delta sdmTMB model.", call. = FALSE)
  which_model <- if (delta && component != "binomial") 2L else 1L
  fam <- if (delta) model$family[[which_model]] else stats::family(model)
  response <- as.matrix(model$response)
  y <- response[, which_model]
  if (delta && component == "response")
    y <- ifelse(response[, 1L] == 0, 0, response[, 2L])
  keep <- which(!is.na(y))
  binomial <- fam$family %in% c("binomial", "betabinomial")
  trials <- if (binomial) rep_len(unname(model$tmb_data$size), NROW(response)) else 1
  if (binomial) y <- y / (trials + as.numeric(trials == 0))
  list(y = unname(y[keep]), rows = keep, binomial = binomial,
    trials = trials, family = fam$family)
}

#' @export
diagnostic_posterior_observed.sdmTMB <- function(model, conditioning = NULL,
                                                component = "response", ...) {
  observed <- sdmtmb_posterior_observations(model, component)
  out <- diagnostic_response_table(observed$y, 0L, rows = observed$rows)
  attr(out, "discrete") <- setNames(observed$family %in% c("poisson", "binomial", "betabinomial", "nbinom1",
    "nbinom2", "truncated_poisson", "truncated_nbinom1", "truncated_nbinom2"), "response")
  attr(out, "diagnostic_description") <- if (identical(conditioning, "marginal")) paste(
    "Approximate joint parameter draws and new native spatial/temporal/group effects;",
    "smooth coefficients retain their posterior draws.") else paste(
      "Approximate joint coefficient, nuisance-parameter and fitted-field posterior draws;",
      "native observation simulation.")
  out
}

# sdmTMB's mle-mvn simulator draws random fields but keeps fixed coefficients
# at their estimates. Here, draw coefficients and effects together and pass the
# full vector to sdmTMB's simulator. This preserves their covariance and lets
# sdmTMB generate both parts of a delta response together.
#' @export
diagnostic_posterior_sampler.sdmTMB <- function(model, n, seed = NULL,
                                               conditioning = NULL,
                                               component = "response", ...) {
  if (!is.null(list(...)$field_method)) stop(
    "'field_method' selects residual diagnostics; posterior predictive draws use ",
    "their joint coefficient and field approximation.", call. = FALSE)
  diagnostic_gaussian_posterior_options(list(...), class(model)[[1L]])
  conditioning <- conditioning %||% "conditional"
  if (!conditioning %in% c("conditional", "marginal")) stop(
    "sdmTMB posterior predictive conditioning must be 'conditional' or 'marginal'.",
    call. = FALSE)
  if (conditioning == "marginal") {
    dependence <- sdmtmb_response_dependence(model, conditioning, "mle-eb",
      seq_len(NROW(model$response)))
    if (!isTRUE(dependence$joint_available)) stop(
      "Marginal posterior predictive simulation is unavailable: ",
      dependence$reason, call. = FALSE)
  }
  p <- model$last.par.best
  index <- seq_along(p)
  draws <- diagnostic_joint_parameter_draws(p, n, model$sd_report,
    report = function() {
      ns <- asNamespace("sdmTMB")
      report <- if (exists("sdreport_sdmTMB", ns, inherits = FALSE))
        get("sdreport_sdmTMB", ns) else TMB::sdreport
      report(model$tmb_obj, getJointPrecision = TRUE)
    }, label = "sdmTMB")
  observed <- sdmtmb_posterior_observations(model, component)
  delta <- isTRUE(model$family$delta)
  component <- match.arg(component, c("response", "binomial", "positive"))
  which_model <- if (component == "positive") 2L else 1L
  keep <- observed$rows
  binomial <- observed$binomial
  trials <- observed$trials
  function(i) {
    draw_seed <- sample.int(.Machine$integer.max, 1L)
    response <- withr::with_seed(draw_seed, stats::simulate(model,
      nsim = 1L, seed = draw_seed, type = "mle-eb",
      mcmc_samples = matrix(draws[i, ], ncol = 1L),
      model = if (!delta || component == "response") NA_integer_ else which_model,
      re_form = if (conditioning == "conditional") NULL else NA,
      silent = TRUE))
    response <- as.numeric(response)[keep]
    if (binomial) response <- response / (trials[keep] + as.numeric(trials[keep] == 0))
    diagnostic_response_table(response, i, rows = keep)
  }
}

#' @export
#' @rdname posterior_samples
posterior_samples.sdmTMB <- function(model, n = 1L, seed = NULL,
                                     conditioning = NULL, ...) {
  diagnostic_posterior_samples.default(model, n, seed, conditioning, ...)
}

#' @export
diagnostic_posterior_observed.gamm4 <- function(model, conditioning = NULL, ...) {
  conditioning <- mixed_conditioning(model, conditioning)
  if (conditioning == "gam_component") {
    return(diagnostic_posterior_observed.gam(gamm4_gam(model)))
  }
  mer <- model$mer
  y <- stats::model.response(stats::model.frame(mer))
  fam <- stats::family(mer)
  if (fam$family %in% c("binomial", "betabinomial", "combinomial")) {
    if (is.matrix(y)) y <- y[, 1L] / (rowSums(y) + as.numeric(rowSums(y) == 0))
    if (is.factor(y)) y <- as.numeric(y != levels(y)[1L])
  }
  out <- diagnostic_response_table(y, 0L, rows = which(!is.na(model_row_map(model$gam))))
  attr(out, "discrete") <- setNames(fam$family %in% c("poisson", "binomial"), "response")
  attr(out, "diagnostic_description") <- if (identical(conditioning, "marginal")) paste(
    "Gaussian fixed-coefficient draws and new smooth/group effects;",
    "covariance hyperparameters fixed.") else paste(
      "Approximate joint fitted fixed/random-effect draws; covariance hyperparameters fixed.")
  out
}

#' @export
diagnostic_posterior_samples.gamm4 <- function(model, n, seed = NULL,
                                              conditioning = NULL, ...) {
  conditioning <- mixed_conditioning(model, conditioning)
  if (conditioning == "gam_component") {
    return(diagnostic_posterior_samples.gam(gamm4_gam(model), n, seed, ...))
  }
  diagnostic_posterior_samples.default(model, n, seed, conditioning, ...)
}

#' @export
diagnostic_posterior_sampler.gamm4 <- function(model, n, seed = NULL,
                                              conditioning = NULL, ...) {
  conditioning <- mixed_conditioning(model, conditioning)
  if (conditioning == "gam_component") {
    return(diagnostic_posterior_sampler.gam(gamm4_gam(model), n, seed, ...))
  }
  diagnostic_gaussian_posterior_options(list(...), "gamm4")
  conditioning <- match.arg(conditioning, c("conditional", "marginal"))
  mer <- model$mer
  beta <- lme4::fixef(mer)
  rows <- which(!is.na(model_row_map(model$gam)))
  if (conditioning == "marginal") {
    draws <- diagnostic_gaussian_draws(n, beta, as.matrix(stats::vcov(mer)))
    X <- lme4::getME(mer, "X")
    ZL <- lme4::getME(mer, "Z") %*% lme4::getME(mer, "Lambda")
    offset <- stats::model.offset(stats::model.frame(mer)) %||% 0
    fam <- stats::family(mer)
    rd <- fix_family_rd(fam)$rd
    if (!is.function(rd)) stop("No gamm4 response sampler for family <", fam$family, ">.", call. = FALSE)
    wt <- list(...)$.response_weights %||% binomial_sampling_weights(mer,
      stats::weights(mer, type = "prior") %||% rep(1, nrow(X)))
    scale <- if (fam$family == "gaussian") stats::sigma(mer)^2 else 1
    return(function(i) {
      eta <- as.numeric(X %*% draws[i, ]) + offset +
        as.numeric(ZL %*% stats::rnorm(ncol(ZL), sd = sqrt(scale)))
      diagnostic_response_table(rd(fam$linkinv(eta), wt, scale), i, rows = rows)
    })
  }
  b <- lme4::getME(mer, "b")
  covariance <- tryCatch(as.matrix(stats::vcov(mer, full = TRUE)), error = function(e)
    stop("Joint random/fixed effect covariance is unavailable for gamm4: ",
      conditionMessage(e), call. = FALSE))
  mu <- c(as.numeric(b), beta)
  if (!identical(dim(covariance), c(length(mu), length(mu)))) stop(
    "This lme4 version does not provide joint random/fixed effect covariance.", call. = FALSE)
  draws <- diagnostic_gaussian_draws(n, mu, covariance)
  X <- cbind(lme4::getME(mer, "Z"), lme4::getME(mer, "X"))
  offset <- stats::model.offset(stats::model.frame(mer)) %||% 0
  fam <- stats::family(mer)
  rd <- fix_family_rd(fam)$rd
  if (!is.function(rd)) stop("No gamm4 response sampler for family <", fam$family, ">.", call. = FALSE)
  wt <- list(...)$.response_weights %||% binomial_sampling_weights(mer,
    stats::weights(mer, type = "prior") %||% rep(1, nrow(X)))
  scale <- if (fam$family == "gaussian") stats::sigma(mer)^2 else 1
  function(i) {
    mu <- fam$linkinv(as.numeric(X %*% draws[i, ]) + offset)
    diagnostic_response_table(rd(mu, wt, scale), i, rows = rows)
  }
}

#' @export
diagnostic_posterior_observed.gamlss <- function(model, conditioning = NULL, ...) {
  gjrm_response_distribution(model)
  out <- diagnostic_response_table(as.vector(model$y1), 0L)
  attr(out, "discrete") <- setNames(model$margins[1L] %in% c("P", "NBI", "NBII"), "response")
  out
}

# Use GJRM's final coefficients and covariance, rather than those of the GAMs
# used during fitting. Draw mean and scale coefficients together to retain
# their covariance.
#' @export
diagnostic_posterior_sampler.gamlss <- function(model, n, seed = NULL,
                                              conditioning = NULL, ...) {
  check_distribution_conditioning(conditioning)
  diagnostic_gaussian_posterior_options(list(...), class(model)[[1L]])
  gjrm_response_distribution(model)
  beta <- model$coefficients
  V <- model$Vb
  if (any(!is.finite(beta)) || !identical(dim(V), c(length(beta), length(beta)))) {
    stop("GJRM final coefficient covariance is unavailable.", call. = FALSE)
  }
  draws <- diagnostic_gaussian_draws(n, beta, V)
  X1 <- model$X1
  n1 <- ncol(X1)
  idx1 <- seq_len(n1)
  offset1 <- rep_len(as.vector(model$eta1), nrow(X1)) - drop(X1 %*% beta[idx1])
  idx2 <- setdiff(seq_along(beta), idx1)
  X2 <- model$X2
  if (length(idx2) && is.null(X2)) X2 <- matrix(1, nrow(X1), 1L)
  if (length(idx2) && ncol(X2) != length(idx2)) stop(
    "Cannot align GJRM distributional coefficients with fitted predictors.", call. = FALSE)
  offset2 <- if (length(idx2)) rep_len(as.vector(model$eta2), nrow(X1)) -
    drop(X2 %*% beta[idx2]) else NULL
  function(i) {
    eta <- drop(X1 %*% draws[i, idx1]) + offset1
    sigma <- if (length(idx2)) exp(drop(X2 %*% draws[i, idx2]) + offset2) else NULL
    distribution <- gjrm_response_distribution(model, eta = eta, sigma = sigma)
    diagnostic_response_table(distribution$rd(), i)
  }
}

#' @export
#' @rdname posterior_samples
posterior_samples.gamlss <- function(model, n = 1L, seed = NULL, ...) {
  diagnostic_posterior_samples.default(model, n, seed, ...)
}

#' @export
diagnostic_posterior_observed.gratia_diagnostic_proxy <- function(model, conditioning = NULL, ...) {
  args <- list(...)
  args$component <- args$component %||% model$diagnostic_component
  do.call(diagnostic_posterior_observed, c(list(model = model$original,
    conditioning = conditioning %||% model$conditioning), args))
}
#' @export
diagnostic_posterior_samples.gratia_diagnostic_proxy <- function(model, n, seed = NULL,
                                                                conditioning = NULL, ...) {
  args <- list(...)
  args$component <- args$component %||% model$diagnostic_component
  do.call(diagnostic_posterior_samples, c(list(model = model$original, n = n, seed = seed,
    conditioning = conditioning %||% model$conditioning), args))
}
#' @export
diagnostic_posterior_sampler.gratia_diagnostic_proxy <- function(model, n, seed = NULL,
                                                                conditioning = NULL, ...) {
  args <- list(...)
  args$component <- args$component %||% model$diagnostic_component
  do.call(diagnostic_posterior_sampler, c(list(model = model$original, n = n, seed = seed,
    conditioning = conditioning %||% model$conditioning), args))
}


# Use TMB/RTMB's sparse inverse covariance to avoid storing a dense covariance
# for all coefficients and random effects. Draw all parameters before selecting
# the required ones, so their uncertainty includes uncertainty in the parameters
# that are not returned. Fixing those parameters would give a different
# covariance.
diagnostic_joint_parameter_draws <- function(p, n, sdr, report, label,
                                            index = seq_along(p)) {
  fixed_only <- !is.null(sdr) && identical(names(sdr$par.fixed), names(p)) &&
    identical(dim(sdr$cov.fixed), c(length(p), length(p)))
  if (is.null(sdr) || (is.null(sdr$jointPrecision) && !fixed_only)) sdr <- report()
  if (!isTRUE(sdr$pdHess)) stop(label, " covariance is unavailable: ",
    "Hessian is not positive definite.", call. = FALSE)
  Q <- sdr$jointPrecision
  if (is.null(Q)) {
    covariance <- mixed_joint_covariance(sdr, p, index, label)
    return(diagnostic_gaussian_draws(n, p[index], covariance))
  }
  if (nrow(Q) != length(p) || (!is.null(rownames(Q)) &&
      !identical(rownames(Q), names(p)))) stop("Cannot align ", label,
    " joint precision with fitted parameters (mapped parameters are not supported).",
    call. = FALSE)
  chol <- Matrix::expand(Matrix::Cholesky(Matrix::forceSymmetric(Q),
    perm = TRUE, LDL = FALSE))
  z <- matrix(stats::rnorm(length(p) * n), nrow = length(p))
  deviations <- Matrix::t(chol$P) %*% Matrix::solve(Matrix::t(chol$L), z)
  sweep(t(as.matrix(deviations[index, , drop = FALSE])), 2L, p[index], `+`)
}


# With a matrix of means, diagnostic_gaussian_draws() uses random numbers for
# all observations and draws of one response before moving to the next response.
# Save the generator state for each response to reproduce that order while
# generating one replicated data set at a time.
diagnostic_mvn_posterior_sampler <- function(model, betas, rows) {
  n_obs <- nrow(model$model)
  n <- nrow(betas)
  p <- n_eta(model)
  covariance <- solve(crossprod(stats::family(model)$data$R))
  root <- mgcv::mroot(covariance, rank = p)
  states <- vector("list", p)
  for (j in seq_len(p)) {
    states[[j]] <- get(".Random.seed", envir = .GlobalEnv)
    remaining <- as.double(n_obs) * n
    while (remaining > 0) {
      size <- min(remaining, 100000L)
      stats::rnorm(size)
      remaining <- remaining - size
    }
  }
  function(i) {
    fits <- fitted_samples(model, n = 1L, method = "user",
      draws = betas[i, , drop = FALSE])
    mu <- vapply(paste0("response", seq_len(p)), function(parameter)
      fits$.fitted[fits$.parameter == parameter], numeric(n_obs))
    z <- vapply(seq_len(p), function(j) withr::with_preserve_seed({
      assign(".Random.seed", states[[j]], envir = .GlobalEnv)
      value <- stats::rnorm(n_obs)
      states[[j]] <<- get(".Random.seed", envir = .GlobalEnv)
      value
    }), numeric(n_obs))
    diagnostic_response_table(z %*% t(root) + mu, i, rows = rows)
  }
}


#' @export
#' @rdname posterior_samples
posterior_samples.gamm <- function(model, n = 1L, seed = NULL,
                                   conditioning = NULL, ...) {
  diagnostic_posterior_samples.default(model, n, seed, conditioning, ...)
}


# mgcv returns a vector for one multivariate Gaussian draw. Always return a
# matrix with draws in rows and parameters in columns, including for one draw.
diagnostic_gaussian_draws <- function(n, mu, V) {
  matrix(mgcv::rmvn(n, mu, V), nrow = n)
}


diagnostic_posterior_count <- function(n) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 1 ||
      n != floor(n) || n > .Machine$integer.max) {
    stop("'n' must be a positive integer.", call. = FALSE)
  }
  invisible(n)
}

#' @export
diagnostic_posterior_observed.gamm <- function(model, conditioning = NULL, ...) {
  conditioning <- mixed_conditioning(model, conditioning)
  if (conditioning == "gam_component") return(diagnostic_posterior_observed.gam(model$gam))
  proxy <- gamm_diagnostic_model(model, conditioning)
  out <- diagnostic_response_table(proxy$y, 0L,
    rows = which(!is.na(model_row_map(proxy))))
  attr(out, "discrete") <- setNames(proxy$family$family %in% c("poisson", "binomial"), "response")
  attr(out, "diagnostic_description") <- if (!is.null(model$lme$y))
    mixed_posterior_description(conditioning,
      "PQL Gaussian working-posterior and native Gaussian effect-prior approximation") else
    if (identical(conditioning, "marginal")) paste(
      "Gaussian fixed-coefficient draws and new smooth/group effects;",
      "fitted observation covariance retained.") else paste(
        "Joint Gaussian fitted coefficient/effect draws; fitted observation covariance retained.")
  out
}


# These fitters support Gaussian approximations only. Reject requests for user
# draws or other sampling algorithms instead of ignoring them.
diagnostic_gaussian_posterior_options <- function(dots, label) {
  if (!is.null(dots$method)) {
    method <- match.arg(dots$method, c("gaussian", "mh", "inla", "user"))
    if (method != "gaussian") stop(label,
      " posterior diagnostic sampling supports only method = 'gaussian'.", call. = FALSE)
  }
  if (!is.null(dots$draws)) stop("User coefficient draws are not supported by the ",
    label, " posterior diagnostic sampler.", call. = FALSE)
  if (!is.null(dots$weights)) stop("Custom response weights are not supported by the ",
    label, " posterior diagnostic sampler; fitted weights are used.", call. = FALSE)
  if (isTRUE(dots$unconditional) || isTRUE(dots$freq)) stop(
    "Alternative covariance corrections are not supported by the ", label,
    " posterior diagnostic sampler.", call. = FALSE)
  invisible(NULL)
}

# Direct mixed-model fits use the same posterior samplers as gamm/gamm4 objects.
# Model accessors receive retained rows only; the prepared diagnostic model
# restores excluded positions once, so na.exclude is not applied twice.
native_mer_diagnostic_components <- function(model) {
  gam <- mer_diagnostic_model(model, "conditional")
  mer <- model
  attr(mer@frame, "na.action") <- NULL
  list(gam = gam, mer = mer)
}

native_mixed_posterior_conditioning <- function(conditioning) {
  match.arg(conditioning %||% "conditional", c("conditional", "marginal"))
}

#' @export
diagnostic_posterior_sampler.lme <- function(model, n, seed = NULL,
    conditioning = NULL, ...) {
  diagnostic_posterior_sampler.gamm(lme_diagnostic_components(model), n,
    seed = seed, conditioning = native_mixed_posterior_conditioning(conditioning), ...)
}
#' @export
diagnostic_posterior_observed.lme <- function(model, conditioning = NULL, ...) {
  conditioning <- native_mixed_posterior_conditioning(conditioning)
  out <- diagnostic_posterior_observed.gamm(lme_diagnostic_components(model),
    conditioning = conditioning, ...)
  attr(out, "diagnostic_description") <- mixed_posterior_description(conditioning)
  out
}
#' @export
#' @rdname posterior_samples
posterior_samples.lme <- function(model, n = 1L, seed = NULL,
    conditioning = NULL, ...) {
  diagnostic_posterior_samples.default(model, n, seed,
    conditioning = native_mixed_posterior_conditioning(conditioning), ...)
}

#' @export
diagnostic_posterior_sampler.merMod <- function(model, n, seed = NULL,
    conditioning = NULL, ...) {
  diagnostic_posterior_sampler.gamm4(native_mer_diagnostic_components(model), n,
    seed = seed, conditioning = native_mixed_posterior_conditioning(conditioning), ...)
}
#' @export
diagnostic_posterior_observed.merMod <- function(model, conditioning = NULL, ...) {
  conditioning <- native_mixed_posterior_conditioning(conditioning)
  out <- diagnostic_posterior_observed.gamm4(native_mer_diagnostic_components(model),
    conditioning = conditioning, ...)
  attr(out, "diagnostic_description") <- mixed_posterior_description(conditioning)
  out
}
#' @export
#' @rdname posterior_samples
posterior_samples.merMod <- function(model, n = 1L, seed = NULL,
    conditioning = NULL, ...) {
  diagnostic_posterior_samples.default(model, n, seed,
    conditioning = native_mixed_posterior_conditioning(conditioning), ...)
}


mixed_posterior_description <- function(conditioning, approximation = NULL) {
  uncertainty <- if (conditioning == "marginal")
    "Gaussian coefficient uncertainty and newly simulated effects" else
    "joint Gaussian coefficient and fitted-effect uncertainty"
  paste(c(paste0("Conditioning: ", conditioning), uncertainty,
    "fitted covariance parameters held fixed", approximation), collapse = "; ")
}
