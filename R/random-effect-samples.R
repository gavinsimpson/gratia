#' Draw new random effects from an estimated GAM
#'
#' Sample zero-mean Gaussian random effects using the fitted smoothing
#' parameters and dispersion. These are new effects, rather than posterior
#' draws of the effects estimated for the fitting groups.
#'
#' @param model An ordinary [mgcv::gam()] or [mgcv::bam()] fit with one
#'   linear predictor.
#' @param data Evaluation data. With `NULL`, use the fitting observations.
#'   Supplied data need only contain predictors of the selected smooths.
#'   Group labels may be new; repeated labels share an effect within a draw.
#' @param select Exact smooth labels, or `NULL` for all `re` and `fs` terms.
#' @param n_draws Positive integer giving the number of draws.
#' @param seed Optional integer seed. An explicit seed preserves the caller's
#'   random-number state; `NULL` advances the current state.
#'
#' @details
#' Standard `re` random intercepts, slopes and factor interactions, and `fs`
#' random curves are supported. Each curve is drawn jointly over its evaluation
#' points, including the components penalising its constant and linear parts.
#' Custom `re` penalties supplied through `xt$S` are unsupported. Fits using
#' `min.sp` are also unsupported: mgcv does not retain the fitted lower bounds
#' needed to reconstruct their random-effect covariance.
#'
#' For stored penalties \eqn{S_j}, smoothing parameters \eqn{\lambda_j}, and
#' dispersion \eqn{\phi}, coefficient covariance is
#' \deqn{\phi(\sum_j\lambda_j S_j)^{-1}.}
#' Smoothing parameters and dispersion remain fixed. As in [mgcv::gam.vcomp()],
#' random-effect covariance uses `reml.scale` when it is finite and `sig2`
#' otherwise. Response generators retain their usual `sig2` dispersion.
#' Different smooths and
#' different groups have independent new coefficients, as specified by these
#' model terms. Covariance between fitted coefficients is not used here.
#'
#' Use `marginalise` in [fitted_values()] or [fitted_samples()] to integrate
#' response means over these distributions. Use it in [predicted_samples()],
#' [posterior_samples()] or [simulate.gam()] to simulate new-group responses.
#'
#' @return A tibble of class `random_effect_samples`, with `.row`, `.draw`,
#'   `.smooth`, `.value` (the link-scale contribution), and covariates. The
#'   `random_effect_info` attribute describes the fixed covariance estimates.
#' @export
#' @examples
#' load_mgcv()
#' m <- gam(travel ~ s(Rail, bs = "re"), data = nlme::Rail,
#'   method = "REML")
#' random_effect_samples(m, n_draws = 3, seed = 42)
#'
#' # Compare zero random effects with response means averaged over groups.
#' set.seed(2)
#' d <- expand.grid(x = seq(0, 1, length.out = 15), id = factor(1:12))
#' d$y <- rpois(nrow(d), exp(0.3 + d$x + rnorm(12, sd = 0.6)[d$id]))
#' p <- gam(y ~ x + s(id, bs = "re"), data = d, family = poisson(),
#'   method = "REML")
#' nd <- data.frame(x = c(0, 0.5, 1), id = d$id[1])
#' fitted_values(p, data = nd, exclude = "s(id)")
#' fitted_values(p, data = nd, marginalise = TRUE)
#' fitted_values(p, data = nd, marginalise = TRUE,
#'   integration = "monte_carlo", seed = 42)
#' nd$id <- c("new", "new", "other")
#' predicted_samples(p, data = nd, marginalise = TRUE, n_draws = 3, seed = 42)
random_effect_samples <- function(model, data = NULL, select = NULL,
                                 n_draws, seed = NULL) {
  n_draws <- sampling_count(n_draws, "n_draws")
  setup <- random_effect_setup(model, select)
  if (!is.null(data) && !is.data.frame(data))
    stop("'data' must be a data frame.", call. = FALSE)
  layout_model <- model
  if (!is.null(data)) {
    variables <- unique(unlist(lapply(setup$terms, function(d)
      c(d$smooth$term, setdiff(d$smooth$by, "NA")))))
    layout_model$terms <- stats::terms(stats::reformulate(variables))
  }
  layout <- prediction_layout(layout_model, data)
  blocks <- lapply(setup$terms, function(def)
    random_effect_block(def, prepare_smooth_data(model, def$smooth, layout$data)))
  generate <- function() {
    out <- lapply(seq_along(blocks), function(i) {
      values <- if (all(is.na(layout$map)))
        matrix(NA_real_, nrow(layout$data), n_draws) else
          random_effect_block_draws(blocks[[i]], n_draws)
      tab <- tibble::tibble(.row = rep(seq_len(nrow(values)), n_draws),
        .draw = rep(seq_len(n_draws), each = nrow(values)),
        .value = as.vector(values))
      tab <- restore_prediction_table(tab, layout)
      tab$.smooth <- setup$terms[[i]]$smooth$label
      tab
    })
    out <- dplyr::bind_rows(out)
    covariates <- tibble::as_tibble(layout$output_data)
    covariates <- covariates[match(out$.row, layout$rows), , drop = FALSE]
    covariates <- covariates[, !names(covariates) %in% names(out), drop = FALSE]
    out <- tibble::as_tibble(cbind(out, covariates))
    out <- dplyr::relocate(out, dplyr::all_of(c(".row", ".draw", ".smooth", ".value")))
    attr(out, "random_effect_info") <- setup$info
    class(out) <- c("random_effect_samples", class(out))
    out
  }
  random_effect_with_seed(seed, generate)
}

random_effect_with_seed <- function(seed, fun) {
  if (is.null(seed)) return(fun())
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed != trunc(seed) || abs(seed) > .Machine$integer.max)
    stop("'seed' must be NULL or a single integer.", call. = FALSE)
  withr::with_seed(seed, fun())
}

random_effect_setup <- function(model, select = NULL) {
  if (!inherits(model, "gam") || inherits(model, "scam") ||
      isTRUE(attr(model, "gratia.gamm4")) || has_inequality_constraints(model) ||
      is.list(stats::formula(model)) || n_eta(model) != 1L)
    stop("Random-effect sampling requires an ordinary gam or bam fit with one linear predictor.", call. = FALSE)
  eligible <- vapply(model$smooth, function(s)
    inherits(s, c("random.effect", "fs.interaction")), logical(1L))
  labels <- vapply(model$smooth, `[[`, character(1L), "label")
  if (is.null(select) || identical(select, TRUE)) select <- labels[eligible]
  if (!is.character(select) || !length(select) || anyNA(select) ||
      anyDuplicated(select) || !all(select %in% labels[eligible]))
    stop("Select distinct exact labels of supported 're' or 'fs' terms; none may be available.", call. = FALSE)
  # full.sp omits min.sp, whose fitted values are not stored. Evaluating the
  # original call could recover changed values rather than the fitted bounds.
  if (!is.null(model$call$min.sp) && !identical(model$call$min.sp, quote(NULL)))
    stop("Random-effect sampling and marginalisation do not support fits using min.sp; the fitted lower bounds are not stored.", call. = FALSE)
  # full.sp is in penalty order, including fixed and replicated parameters and
  # any parametric penalties preceding the smooth penalties.
  sp <- model$full.sp %||% model$sp
  counts <- vapply(model$smooth, function(s) length(s$S), integer(1L))
  offset <- length(sp) - sum(counts)
  if (offset < 0L) stop("Cannot match smoothing parameters to stored penalties.", call. = FALSE)
  ends <- offset + cumsum(counts)
  likelihood_scale <- !is.null(model$reml.scale) && is.finite(model$reml.scale)
  phi <- if (likelihood_scale) model$reml.scale else
    model$sig2 %||% summary(model)$dispersion
  if (length(phi) != 1L || !is.finite(phi) || phi <= 0)
    stop("A finite positive fitted dispersion is required.", call. = FALSE)
  terms <- lapply(match(select, labels), function(i) {
    sm <- model$smooth[[i]]
    fs <- inherits(sm, "fs.interaction")
    if (any(vapply(model$model[sm$term], is.matrix, logical(1L))))
      stop("Matrix-valued random-effect predictors are not yet supported.", call. = FALSE)
    if (!fs && !is.null(sm$xt$S))
      stop("Custom 're' penalties supplied through xt$S are unsupported.", call. = FALSE)
    groups <- if (fs) sm$fterm else sm$term[vapply(sm$term, function(nm)
      is.factor(model$model[[nm]]), logical(1L))]
    levels <- lapply(groups, function(nm) levels(model$model[[nm]]))
    names(levels) <- groups
    k <- if (fs) sm$base$bs.dim else 1L
    index <- seq.int(ends[i] - counts[i] + 1L, ends[i])
    lambda <- sp[index]
    if (length(lambda) != length(sm$S) || any(!is.finite(lambda) | lambda <= 0))
      stop("Random-effect smoothing parameters must be finite and positive.", call. = FALSE)
    Q <- Reduce(`+`, Map(function(S, a) a * S[seq_len(k), seq_len(k), drop = FALSE],
      sm$S, lambda)) / phi
    root <- tryCatch(chol(Q), error = function(e) NULL)
    if (is.null(root) || any(!is.finite(root)))
      stop("The selected penalties do not define a proper Gaussian distribution.", call. = FALSE)
    list(smooth = sm, groups = groups, levels = levels, k = k,
      covariance = chol2inv(root), lambda = unname(lambda))
  })
  names(terms) <- select
  list(terms = terms, info = list(terms = select, dispersion = phi,
    dispersion_source = if (likelihood_scale) "fitted reml.scale" else "fitted sig2",
    smoothing_parameters = lapply(terms, `[[`, "lambda"),
    coefficient_covariance = lapply(terms, `[[`, "covariance"),
    variance_parameters = "fixed at fitted estimates"))
}

# Evaluate one group's basis using fitted reference levels. The original labels
# only assign independent coefficient vectors; they never select fitted effects.
random_effect_block <- function(def, data, require_groups = TRUE) {
  patched <- as.data.frame(data)
  if (require_groups && !all(def$groups %in% names(data)))
    stop("Supply grouping columns for new random-effect draws.", call. = FALSE)
  for (nm in def$groups) patched[[nm]] <- factor(rep(def$levels[[nm]][1L],
    nrow(patched)), levels = def$levels[[nm]])
  # Group reference levels select one repeated penalty block. Apply the by
  # multiplier from the original data, even when it is itself a group.
  smooth <- def$smooth
  smooth$by <- "NA"
  B <- mgcv::PredictMat(smooth, patched)[, seq_len(def$k), drop = FALSE]
  if (def$smooth$by != "NA") {
    by <- data[[def$smooth$by]]
    if (is.null(by)) stop("Supply the selected smooth's by variable.", call. = FALSE)
    multiplier <- if (!is.null(def$smooth$by.level))
      as.numeric(as.character(by) == def$smooth$by.level) else
      as.numeric(by)
    B <- B * multiplier
  }
  group <- if (require_groups && length(def$groups))
    as.integer(vctrs::vec_group_id(data[def$groups])) else rep(1L, nrow(B))
  list(B = B, group = group, covariance = def$covariance,
    variance = rowSums((B %*% def$covariance) * B))
}

random_effect_block_draws <- function(block, n) {
  B <- block$B
  out <- matrix(0, nrow(B), n)
  for (g in unique(block$group)) {
    rows <- which(block$group == g)
    coefficients <- matrix(stats::rnorm(n * ncol(B)), n, ncol(B)) %*%
      chol(block$covariance)
    out[rows, ] <- B[rows, , drop = FALSE] %*% t(coefficients)
  }
  out
}

marginal_family <- function(model) {
  fam <- stats::family(model)
  supported <- c("gaussian", "poisson", "binomial", "gamma", "inverse_gaussian",
    "quasi", "quasipoisson", "quasibinomial", "negative_binomial", "tweedie",
    "beta_regression", "scaled_t")
  if (inherits(fam, "general.family") || !family_type(model) %in% supported)
    stop("Marginalisation requires a supported scalar response-mean family.", call. = FALSE)
  safe <- (fam$link %in% c("logit", "probit", "cloglog", "cauchit") &&
      isTRUE(fam$validmu(c(.1, .9)))) ||
    (fam$link == "log" && isTRUE(fam$validmu(c(.1, 1, 2)))) ||
    (fam$link == "identity" && isTRUE(fam$validmu(c(-1, 0, 1))))
  if (!safe) stop("This link does not support integration over unbounded Gaussian random effects.", call. = FALSE)
  if (!is.null(model$AR1.rho) && model$AR1.rho != 0)
    stop("Marginalisation of residual-correlated BAM fits is not supported.", call. = FALSE)
  fam
}

marginal_context <- function(model, data, marginalise, response = FALSE, ...) {
  setup <- random_effect_setup(model, marginalise)
  fam <- marginal_family(model)
  dots <- list(...)
  if (!is.null(dots$terms)) stop("'terms' cannot be combined with 'marginalise'.", call. = FALSE)
  if (length(intersect(dots$exclude, setup$info$terms)))
    stop("A term cannot occur in both 'exclude' and 'marginalise'.", call. = FALSE)
  original <- if (is.null(data)) model$model else as.data.frame(data)
  patched <- original
  selected <- setup$info$terms
  others <- Filter(function(s) !s$label %in% selected, model$smooth)
  used_elsewhere <- unique(c(all.vars(stats::delete.response(model$pterms)),
    unlist(lapply(others, function(s) c(s$term, setdiff(s$by, "NA")))),
    unlist(lapply(setup$terms, function(d) setdiff(d$smooth$by, "NA")))))
  groups <- unique(unlist(lapply(setup$terms, `[[`, "groups")))
  used_vars <- unique(unlist(lapply(used_elsewhere, function(nm)
    all.vars(term_expression(nm, model)))))
  exclusive <- groups[vapply(groups, function(nm)
    !length(intersect(all.vars(term_expression(nm, model)), used_vars)), logical(1L))]
  available <- groups[vapply(groups, function(nm) nm %in% names(original) ||
    all(all.vars(term_expression(nm, model)) %in% names(original)), logical(1L))]
  evaluated <- evaluate_terms(original, available, model,
    evaluated = !is.null(attr(original, "terms")))
  patched <- evaluated
  attr(patched, "gratia.evaluated") <- TRUE
  if (response && !all(groups %in% names(evaluated)))
    stop("Supply grouping columns for new-group response simulations.", call. = FALSE)
  for (def in setup$terms) for (nm in intersect(def$groups, exclusive))
    patched[[nm]] <- factor(rep(def$levels[[nm]][1L], nrow(patched)),
      levels = def$levels[[nm]])
  for (nm in setdiff(groups, exclusive)) {
    if (!nm %in% names(evaluated))
      stop("Supply grouping columns also used in retained model terms or selected by smooths.", call. = FALSE)
    known <- levels(model$model[[nm]])
    if (any(!is.na(evaluated[[nm]]) & !as.character(evaluated[[nm]]) %in% known))
      stop("New grouping levels also occur in retained model terms or selected by smooths.", call. = FALSE)
  }
  required <- if (response && length(groups)) evaluated[groups] else NULL
  layout <- if (is.null(data)) prediction_layout(model) else
    prediction_layout(model, patched, na.action = dots$na.action %||% stats::na.pass,
      required = required)
  if (is.null(data)) {
    layout$data <- patched
  } else {
    layout$output_data <- original[layout$rows, , drop = FALSE]
  }
  compact_original <- if (is.null(data)) evaluated else
    evaluated[layout$input_rows, , drop = FALSE]
  if (!nrow(compact_original)) compact_original <- model$model[1L, , drop = FALSE]
  blocks <- lapply(setup$terms, function(def) {
    d <- prepare_smooth_data(model, def$smooth, layout$data)
    if (response) for (nm in def$groups) d[[nm]] <- compact_original[[nm]]
    random_effect_block(def, d, require_groups = response)
  })
  dots$exclude <- union(dots$exclude, selected)
  context <- do.call(lss_prediction_context, c(list(model = model, layout = layout), dots))
  context$blocks <- blocks
  context$variance <- Reduce(`+`, lapply(blocks, `[[`, "variance"))
  context$random_effect_info <- setup$info
  context$family <- fam
  distribution_output(context$spec, matrix(0, ncol = 1L), "expectation", "response")
  context
}

normal_quadrature <- function(n) {
  J <- matrix(0, n, n)
  j <- seq_len(n - 1L)
  J[cbind(j, j + 1L)] <- J[cbind(j + 1L, j)] <- sqrt(j)
  e <- eigen(J, symmetric = TRUE)
  list(nodes = e$values, weights = e$vectors[1L, ]^2)
}

marginal_integrator <- function(integration, n_quad, n_integrate) {
  integration <- match.arg(integration, c("quadrature", "monte_carlo"))
  if (integration == "quadrature") {
    n_quad <- sampling_count(n_quad, "n_quad")
    if (n_quad < 2L || n_quad > 1000L)
      stop("'n_quad' must be between 2 and 1000.", call. = FALSE)
    orders <- n_quad * c(1L, 2L, 4L)
    rules <- lapply(orders, normal_quadrature)
    # An opposite-parity rule has interlaced nodes. It detects agreement
    # caused by two even rules both missing a narrow link transition.
    checks <- lapply(orders[-1L] - 1L, normal_quadrature)
  } else {
    n_integrate <- sampling_count(n_integrate, "n_integrate")
    if (n_integrate < 2L) stop("'n_integrate' must be at least two.", call. = FALSE)
    rules <- list(list(nodes = stats::rnorm(n_integrate),
      weights = rep(1 / n_integrate, n_integrate)))
  }
  list(method = integration, rules = rules, checks = if (integration == "quadrature") checks)
}

# For bounded inverse links F, E[F(eta + sd * Z)] can also be written
# as the integral of pnorm((eta - F^{-1}(u)) / sd) over uniform u.
# This independent adaptive rule resolves link transitions that widely
# spaced Hermite nodes can miss. Split around the Gaussian transition
# to retain accuracy when its probability is near zero or one.
marginal_quadrature_check <- function(eta, variance, fam) {
  sd <- sqrt(variance)
  bounds <- sort(unique(c(0, .5, 1,
    fam$linkinv(eta + sd * c(-8, -4, -1, 0, 1, 4, 8)))))
  calculate <- function(derivative) {
    pieces <- lapply(seq_len(length(bounds) - 1L), function(i)
      stats::integrate(function(u) {
        z <- (eta - fam$linkfun(u)) / sd
        if (derivative) stats::dnorm(z) / sd else stats::pnorm(z)
      }, bounds[i], bounds[i + 1L], abs.tol = 1e-11, rel.tol = 1e-8,
        subdivisions = 1000L, stop.on.error = FALSE))
    messages <- vapply(pieces, `[[`, character(1L), "message")
    if (any(messages != "OK" & !grepl("roundoff error", messages, fixed = TRUE)))
      stop("Adaptive quadrature could not estimate numerical accuracy.", call. = FALSE)
    c(value = sum(vapply(pieces, `[[`, numeric(1L), "value")),
      error = sum(vapply(pieces, `[[`, numeric(1L), "abs.error")))
  }
  list(mean = calculate(FALSE), derivative = calculate(TRUE))
}

# Only an observation-by-node matrix is formed, never a three-dimensional
# observation-by-coefficient-draw-by-integration-draw array.
marginal_integrate <- function(eta, variance, fam, integrator) {
  if (integrator$method == "quadrature" &&
      fam$link %in% c("identity", "log", "probit")) {
    variance <- pmax(0, variance)
    value <- switch(fam$link, identity = eta, log = exp(eta + variance / 2),
      probit = stats::pnorm(eta / sqrt(1 + variance)))
    derivative <- switch(fam$link, identity = rep(1, length(eta)),
      log = value, probit = stats::dnorm(eta / sqrt(1 + variance)) /
        sqrt(1 + variance))
    if (any(!is.finite(value)) || any(!is.finite(derivative)))
      stop("Non-finite response means or derivatives during integration.", call. = FALSE)
    return(list(value = value, derivative = derivative,
      error = numeric(length(eta)), count = 0L, algorithm = "closed form"))
  }
  calculate <- function(rule) {
    result <- derivative <- error <- numeric(length(eta))
    batch <- max(1L, floor(1e6 / length(rule$nodes)))
    for (start in seq.int(1L, length(eta), by = batch)) {
      i <- seq.int(start, min(length(eta), start + batch - 1L))
      z <- outer(sqrt(pmax(0, variance[i])), rule$nodes) + eta[i]
      value <- matrix(fam$linkinv(as.vector(z)), nrow(z))
      gradient <- matrix(fam$mu.eta(as.vector(z)), nrow(z))
      if (any(!is.finite(value)) || any(!is.finite(gradient)))
        stop("Non-finite response means or derivatives during integration.", call. = FALSE)
      result[i] <- drop(value %*% rule$weights)
      derivative[i] <- drop(gradient %*% rule$weights)
      if (integrator$method == "monte_carlo")
        error[i] <- sqrt(rowSums((value - result[i])^2) /
          (ncol(value) * (ncol(value) - 1L)))
    }
    list(value = result, derivative = derivative, error = error,
      count = length(rule$nodes), algorithm = if (integrator$method == "monte_carlo")
        "Monte Carlo" else "Gauss-Hermite")
  }
  previous <- calculate(integrator$rules[[1L]])
  if (integrator$method == "monte_carlo") return(previous)
  # The independent check is only needed when the Gaussian distribution
  # spreads the unit-scale link transition over widely spaced nodes.
  verify <- which(variance > 1)
  independent <- NULL
  for (i in seq_along(integrator$checks)) {
    current <- calculate(integrator$rules[[i + 1L]])
    check <- calculate(integrator$checks[[i]])
    current$error <- pmax(abs(current$value - previous$value),
      abs(current$value - check$value))
    derivative_error <- pmax(abs(current$derivative - previous$derivative),
      abs(current$derivative - check$derivative))
    agrees <- function() all(current$error <= 1e-8 + 1e-6 * abs(current$value)) &&
      all(derivative_error <= 1e-8 + 1e-6 * abs(current$derivative))
    if (agrees() && length(verify)) {
      if (is.null(independent)) independent <- tryCatch(
        lapply(verify, function(j) marginal_quadrature_check(eta[j], variance[j], fam)),
        error = function(e) stop("Gaussian quadrature did not converge during independent accuracy checking.", call. = FALSE))
      for (j in seq_along(verify)) {
        row <- verify[j]
        reference <- independent[[j]]
        current$error[row] <- max(current$error[row],
          abs(current$value[row] - reference$mean["value"]) + reference$mean["error"])
        derivative_error[row] <- max(derivative_error[row],
          abs(current$derivative[row] - reference$derivative["value"]) +
            reference$derivative["error"])
      }
      current$algorithm <- "Gauss-Hermite with adaptive accuracy check"
    }
    if (agrees()) return(current)
    previous <- current
  }
  stop("Gaussian quadrature did not converge; increase 'n_quad' or use integration = 'monte_carlo'.", call. = FALSE)
}

marginal_info <- function(context, integrator = NULL, accuracy = NULL) {
  c(context$random_effect_info, list(rows = context$layout$rows[
    match(seq_len(nrow(context$X)), context$layout$map)],
    integration = integrator$method,
    error_type = if (integrator$method == "monte_carlo")
      "Monte Carlo standard error" else "absolute integration error estimate",
    accuracy = accuracy, variance = context$variance))
}

marginal_fitted_values <- function(object, data, marginalise, quantity, scale,
    ci_level, interval, interval_method, unconditional, seed, integration,
    n_quad, n_integrate, ...) {
  if (prediction_choice(quantity[1L], scale[1L])$scale != "response")
    stop("'marginalise' requires response-scale fitted means.", call. = FALSE)
  if (interval != "confidence" || match.arg(interval_method,
      c("auto", "gaussian", "bootstrap")) == "bootstrap")
    stop("Marginal fitted values support pointwise Gaussian intervals only.", call. = FALSE)
  validate_smooth_interval("confidence", ci_level, 2L, 1L, seed)
  context <- marginal_context(object, data, marginalise, ...)
  # fitted_values() evaluates explicit rows and omits fitting-argument offsets.
  if (is.null(data)) context$offsets <- matrix(
    prediction_offsets(context$model, context$X)[[1L]], ncol = 1L)
  generate <- function() {
    integrator <- marginal_integrator(integration[1L], n_quad, n_integrate)
    eta <- as.numeric(lss_predictors(context, stats::coef(context$model)))
    fit <- marginal_integrate(eta, context$variance, context$family, integrator)
    J <- context$X * fit$derivative
    V <- get_vcov(context$model, unconditional = unconditional)
    se <- sqrt(pmax(0, rowSums((J %*% V) * J)))
    critical <- coverage_normal(ci_level)
    out <- tibble::tibble(.row = seq_along(eta),
      .parameter = if (quantity[1L] == "expectation") "expectation" else
        context$spec$parameters$parameter[context$spec$parameters$returned][1L],
      .fitted = fit$value, .se = se, .lower_ci = fit$value - critical * se,
      .upper_ci = fit$value + critical * se)
    out <- restore_prediction_table(out, context$layout)
    covariates <- tibble::as_tibble(context$layout$output_data)
    covariates <- covariates[match(out$.row, context$layout$rows), , drop = FALSE]
    covariates <- covariates[, !names(covariates) %in% names(out), drop = FALSE]
    out <- tibble::as_tibble(cbind(covariates, out))
    attr(out, "integration_info") <- marginal_info(context, integrator,
      list(error = fit$error, count = fit$count, algorithm = fit$algorithm))
    attr(out, "interval_info") <- list(method = "gaussian", interval = "confidence",
      ci_level = ci_level)
    attach_prediction_spec(out, context$spec, quantity[1L], "response", se_scale = "response")
  }
  random_effect_with_seed(seed, generate)
}

marginal_fitted_samples <- function(context, betas, quantity, integration,
    n_quad, n_integrate) {
  integrator <- marginal_integrator(integration[1L], n_quad, n_integrate)
  accuracy <- vector("list", nrow(betas))
  out <- lapply(seq_len(nrow(betas)), function(i) {
    eta <- as.numeric(lss_predictors(context, betas[i, ]))
    fit <- marginal_integrate(eta, context$variance, context$family, integrator)
    accuracy[[i]] <<- list(error = fit$error, count = fit$count, algorithm = fit$algorithm)
    value <- tibble::tibble(.row = seq_along(eta), .draw = i,
      .parameter = if (quantity == "expectation") "expectation" else
        context$spec$parameters$parameter[context$spec$parameters$returned][1L],
      .fitted = fit$value)
    restore_prediction_table(value, context$layout)
  })
  out <- dplyr::bind_rows(out)
  attr(out, "integration_info") <- marginal_info(context, integrator, accuracy)
  attach_prediction_spec(out, context$spec, quantity, "response")
}

marginal_response_values <- function(context, betas, weights = NULL) {
  n <- nrow(betas)
  if (all(is.na(context$layout$map)))
    return(matrix(NA_real_, nrow(context$X), n))
  rd <- missing_safe_rd(choose_rd_fun(context$model))
  wt <- lss_sampling_weights(context, weights)
  values <- matrix(NA_real_, nrow(context$X), n)
  # Draw one joint effect per term and group for each replicate.
  for (i in seq_len(n)) {
    eta <- as.numeric(lss_predictors(context, betas[i, ]))
    for (block in context$blocks)
      eta <- eta + as.numeric(random_effect_block_draws(block, 1L))
    values[, i] <- rd(mu = context$family$linkinv(eta), wt = wt,
      scale = context$model$sig2 %||% summary(context$model)$dispersion)
  }
  values
}

marginal_response_table <- function(context, betas, weights = NULL) {
  values <- marginal_response_values(context, betas, weights)
  out <- tibble::tibble(.row = rep(seq_len(nrow(values)), ncol(values)),
    .draw = rep(seq_len(ncol(values)), each = nrow(values)),
    .response = as.vector(values))
  out <- restore_prediction_table(out, context$layout)
  attr(out, "random_effect_info") <- context$random_effect_info
  out
}

marginal_not_supported <- function(dots, name) {
  if (!is.null(dots$marginalise) ||
      any(c("integration", "n_quad", "n_integrate") %in% names(dots)))
    stop("Marginalisation is not yet supported by ", name, ".", call. = FALSE)
}
