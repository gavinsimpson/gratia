# Each fitter prepares a diagnostic model with response parameters and
# distribution functions. Plotting and reference simulation use this prepared
# model without depending on the original fitter's class.
new_diagnostic_proxy <- function(y, mu, eta, family, distribution,
                                 weights = rep(1, length(y)), model = NULL,
                                 original = NULL, na.action = NULL,
                                 conditioning = "conditional") {
  if (is.null(mu)) mu <- rep(NA_real_, length(y))
  if (is.null(weights)) weights <- rep(1, length(y))
  if (!inherits(family, "family")) class(family) <- "family"
  if (is.null(model)) model <- data.frame(.response = y)
  if (NROW(model) != length(y)) stop("Diagnostic rows are not aligned.")
  distribution$conditioning <- conditioning
  structure(list(y = y, fitted.values = mu, linear.predictors = eta,
    prior.weights = weights, weights = weights, family = family,
    distribution = distribution, model = model, original = original,
    na.action = na.action, conditioning = conditioning, sig2 = 1),
    class = "gratia_diagnostic_proxy", gratia.default_residual_type = "quantile")
}

#' @export
model.frame.gratia_diagnostic_proxy <- function(formula, ...) formula$model
#' @export
family.gratia_diagnostic_proxy <- function(object, ...) object$family
#' @export
fitted.gratia_diagnostic_proxy <- function(object, ...) {
  stats::napredict(object$na.action, object$fitted.values)
}
#' @export
response_distribution.gratia_diagnostic_proxy <- function(model,
    conditioning = NULL, ...) {
  prepared <- diagnostic_plot_source(model, type = "response",
    conditioning = conditioning, ...)
  if (inherits(prepared, "gratia_diagnostic_proxy")) return(prepared$distribution)
  response_distribution(prepared)
}

#' @export
diagnostic_reference_residuals.gratia_diagnostic_proxy <- function(model, y,
    type) {
  check_distribution_residual_moments(model$distribution, type)
  mu <- model$fitted.values
  if (type == "deviance" && identical(model$distribution$calibration,
      "simulation ranks")) {
    stop("Canonical deviance residuals are undefined for a simulation-rank ",
      "integrated response law; use PIT, quantile, response or Pearson residuals.",
      call. = FALSE)
  }
  if (type == "deviance" && identical(model$conditioning, "marginal") &&
      identical(model$family$family, "gaussian")) {
    variance <- model$distribution$variance
    if (is.null(variance)) stop("Gaussian marginal deviance requires integrated variances.")
    return((y - mu) / sqrt(variance))
  }
  if (type == "response") return(y - mu)
  if (type == "pearson") {
    v <- model$distribution$variance
    if (is.function(v)) v <- v()
    if (is.null(v)) stop("Pearson residuals need response variances.")
    return((y - mu) / sqrt(v))
  }
  if (type == "deviance" && is.function(model$family$dev.resids)) {
    return(sign(y - mu) * sqrt(pmax(0,
      model$family$dev.resids(y, mu, model$prior.weights))))
  }
  stop("This response distribution does not define '", type,
    "' residuals.", call. = FALSE)
}

#' @export
residuals.gratia_diagnostic_proxy <- function(object, type = "deviance", ...) {
  stats::naresid(object$na.action,
    diagnostic_reference_residuals(object, object$y, type))
}

#' @export
diagnostic_data.gratia_diagnostic_proxy <- function(model, type = NULL,
    seed = NULL, conditioning = NULL, ..., n_dharma = NULL) {
  prepared <- diagnostic_plot_source(model, type = type, seed = seed,
    conditioning = conditioning, n_dharma = n_dharma, ...)
  if (!inherits(prepared, "gratia_diagnostic_proxy"))
    return(diagnostic_data(prepared, type, seed, n_dharma = n_dharma))
  if (is.null(type)) type <- "quantile"
  diagnostic_data.gam(prepared, type, seed, n_dharma = n_dharma)
}

mixed_conditioning <- function(model, conditioning = NULL) {
  if (is.null(conditioning)) {
    conditioning <- "conditional"
  }
  match.arg(conditioning, c("conditional", "marginal", "gam_component"))
}

mixed_diagnostic_model <- function(model, conditioning = NULL) {
  conditioning <- mixed_conditioning(model, conditioning)
  if (conditioning == "gam_component") {
    return(if (is_gamm4(model)) gamm4_gam(model) else model$gam)
  }
  if (is_gamm4(model)) return(gamm4_diagnostic_model(model, conditioning))
  gamm_diagnostic_model(model, conditioning)
}

# Gaussian lme response draws preserve fitted variance weights and correlation.
# Store the matrix factors used to draw correlated errors separately for each
# group, avoiding a covariance matrix covering all observations.
lme_response_error <- function(model) {
  n <- NROW(model$groups)
  ord <- do.call(order, model$groups)
  w <- model$modelStruct$varStruct
  sd <- rep(model$sigma, n)
  if (!is.null(w)) sd[ord] <- model$sigma / nlme::varWeights(w)
  cs <- model$modelStruct$corStruct
  blocks <- list()
  if (!is.null(cs)) {
    cor <- nlme::corMatrix(cs)
    if (is.matrix(cor)) cor <- list(cor)
    gr <- as.character(nlme::getGroups(cs))
    ids <- split(ord, factor(gr, levels = unique(gr)))
    if (length(ids) != length(cor)) stop("Unaligned lme correlation groups.")
    blocks <- Map(function(i, r) list(i = i, correlation = r, factor =
      diag(sd[i], length(i)) %*% t(chol(r))), ids, cor)
  }
  independent <- !length(blocks) || all(vapply(blocks, function(b) {
    r <- b$correlation
    !any(abs(r - diag(nrow(r))) > 1e-10)
  }, logical(1)))
  list(variance = sd^2, independent = independent, blocks = blocks, sd = sd, draw = function() {
    if (!length(blocks)) return(stats::rnorm(n, sd = sd))
    out <- numeric(n)
    for (b in blocks) out[b$i] <- as.vector(b$factor %*%
      stats::rnorm(length(b$i)))
    out
  })
}

lme_random_error <- function(model) {
  dat <- lme_fitting_data(model)
  terms <- lme_random_terms(model, dat)
  n <- NROW(dat)
  variance <- numeric(n)
  for (b in terms) variance <- variance + rowSums((b$z %*% b$factor)^2)
  design <- lme_random_design(terms, n)
  list(variance = variance, terms = terms, design = design,
    independent = !row_dependence_from_design(design), draw = function() {
    out <- numeric(n)
    for (b in terms) {
      groups <- unique(b$group)
      effect <- b$factor %*% matrix(stats::rnorm(ncol(b$z) *
        length(groups)), nrow = ncol(b$z))
      out <- out + rowSums(b$z * t(effect[, match(b$group, groups),
        drop = FALSE]))
    }
    out
  })
}

gamm_diagnostic_model <- function(model, conditioning = "conditional") {
  gam <- model_used_rows(model$gam)
  lme <- model$lme
  fam <- stats::family(gam)
  sampling_weights <- binomial_sampling_weights(gam, gam$prior.weights)
  gaussian <- identical(fam$family, "gaussian") && fam$link == "identity" &&
    is.null(lme$y)
  dat <- lme_fitting_data(lme)
  idx <- match(rownames(gam$model), rownames(dat))
  if (anyNA(idx)) stop("GAM and mixed-model fitting rows are not aligned.")
  lev <- if (conditioning == "marginal") 0L else ncol(lme$groups)
  eta <- as.vector(lme$fitted[, lev + 1L])[idx]
  off <- gam$offset %||% stats::model.offset(gam$model)
  if (!is.null(off)) eta <- eta + off
  mu <- fam$linkinv(eta)
  if (gaussian) {
    error <- lme_response_error(lme)
    random <- if (conditioning == "marginal") lme_random_error(lme) else NULL
    variance <- error$variance
    if (!is.null(random)) variance <- variance + random$variance
    dist <- distribution_from_parameters("gaussian", mu,
      dispersion = sqrt(variance[idx]))
    dist$variance <- variance[idx]
    dist$rd <- function() {
      value <- error$draw()
      if (!is.null(random)) value <- value + random$draw()
      mu + value[idx]
    }
    dist$independent <- error$independent &&
      (is.null(random) || random$independent)
    dist$joint_available <- TRUE
    if (!dist$independent) dist$dependence_reason <-
      "Fitted residual covariance or shared Gaussian latent effects induce dependence."
  } else {
    scale <- gam$sig2 %||% lme$sigma^2
    error <- lme_response_error(lme)
    approximation <- "PQL uses approximate fitted Gaussian random-effect priors and response-family parameters."
    if (conditioning == "conditional") {
      dist <- distribution_from_family(fam, mu, sampling_weights,
        scale = scale, y = gam$y)
      dist$variance <- scale * fam$variance(mu) / sampling_weights
      dist$independent <- error$independent
      dist$joint_available <- error$independent && is.function(dist$rd)
      if (!error$independent) {
        dist$rd <- NULL
        dist$dependence_reason <- paste(
          "PQL working correlation does not specify a joint response law;",
          "only approximate one-observation family calibration is available.")
      }
    } else if (!error$independent) {
      mu <- NULL
      dist <- new_response_distribution(NULL, family = fam, independent = FALSE)
      dist$joint_available <- FALSE
      dist$dependence_reason <- paste(
        "PQL working correlation does not specify a marginal joint response law.",
        "Marginal diagnostics require a response-level generative model.")
    } else {
      random <- lme_random_error(lme)
      mu <- NULL
      available <- is.function(fix_family_rd(fam)$rd)
      rd <- if (available) function() {
        response_mu <- fam$linkinv(eta + random$draw()[idx])
        distribution_from_family(fam, response_mu, sampling_weights,
          scale = scale, y = gam$y)$rd()
      } else NULL
      dist <- new_response_distribution(NULL, rd = rd, family = fam,
        independent = random$independent)
      dist$joint_available <- available
      if (!available) dist$dependence_reason <-
        "The fitted PQL family does not define a response generator."
      if (available && !dist$independent) dist$dependence_reason <-
        "Shared approximate PQL Gaussian latent effects induce response dependence."
    }
    dist$approximation <- approximation
  }
  dist$conditioning <- conditioning
  if (isTRUE(dist$joint_available)) dist$marginal_rd <- dist$rd
  if (is.null(dist$mean) && isTRUE(dist$joint_available)) {
    dist <- prepare_marginal_response_distribution(dist)
    mu <- dist$mean
  }
  new_diagnostic_proxy(gam$y, mu, eta, fam, dist,
    weights = gam$prior.weights, model = gam$model, original = model,
    na.action = model$gam$na.action, conditioning = conditioning)
}

gamm4_diagnostic_model <- function(model, conditioning = "conditional") {
  gam <- model_used_rows(gamm4_gam(model))
  mer_diagnostic_model(model$mer, conditioning, y = gam$y,
    frame = gam$model, na.action = model$gam$na.action, original = model)
}

mer_diagnostic_model <- function(mer, conditioning = "conditional", y = NULL,
    frame = NULL, na.action = NULL, original = mer) {
  fam <- stats::family(mer)
  native_frame <- stats::model.frame(mer)
  if (is.null(frame)) frame <- native_frame
  if (is.null(y)) y <- stats::model.response(native_frame)
  if (is.null(na.action)) na.action <- stats::na.action(mer)
  if (fam$family == "binomial") {
    if (is.matrix(y)) y <- y[, 1L] / pmax(rowSums(y), 1)
    if (is.factor(y)) y <- as.numeric(y != levels(y)[1L])
  }
  re <- if (conditioning == "marginal") NA else NULL
  eta <- as.vector(stats::predict(mer, type = "link", re.form = re))
  if (length(eta) != nrow(frame)) eta <- eta[!is.na(model_row_map(mer))]
  wt <- stats::weights(mer, type = "prior") %||% rep(1, length(y))
  if (length(wt) != length(y)) wt <- wt[!is.na(model_row_map(mer))]
  sampling_weights <- binomial_sampling_weights(mer, wt)
  scale <- if (inherits(mer, "lmerMod")) stats::sigma(mer)^2 else 1
  independent <- TRUE
  reason <- NULL
  if (conditioning == "marginal") {
    zl <- lme4::getME(mer, "Z") %*% lme4::getME(mer, "Lambda")
    independent <- !row_dependence_from_design(zl)
    if (!independent) reason <- "Shared Gaussian latent effects induce response dependence."
  }
  if (conditioning == "marginal" &&
      (fam$family != "gaussian" || fam$link != "identity")) {
    mu <- NULL
    dist <- new_response_distribution(NULL,
      rd = mer_response_generator(mer, conditioning), family = fam,
      independent = independent)
  } else {
    mu <- fam$linkinv(eta)
    dist <- distribution_from_family(fam, mu, sampling_weights, scale, y = y)
    dist$variance <- scale * fam$variance(mu) / sampling_weights
    if (conditioning == "marginal") {
      variance <- scale * (1 / wt + as.numeric(Matrix::rowSums(zl^2)))
      dist <- distribution_from_parameters("gaussian", mu,
        dispersion = sqrt(variance))
      dist$rd <- mer_response_generator(mer, conditioning)
    }
    dist$independent <- independent
  }
  dist$joint_available <- is.function(dist$rd)
  dist$dependence_reason <- reason
  dist$conditioning <- conditioning
  if (isTRUE(dist$joint_available)) dist$marginal_rd <- dist$rd
  if (is.null(dist$mean) && dist$joint_available) {
    dist <- prepare_marginal_response_distribution(dist)
    mu <- dist$mean
  }
  new_diagnostic_proxy(y, mu, eta, fam, dist, weights = wt,
    model = frame, original = original, na.action = na.action,
    conditioning = conditioning)
}

#' @export
#' @rdname quantile_residuals
quantile_residuals.gamm <- function(model, type = c("pit", "quantile"),
    seed = NULL, conditioning = NULL, ...) {
  quantile_residuals(mixed_diagnostic_model(model, conditioning),
    type = match.arg(type), seed = seed, ...)
}

#' @export
diagnostic_data.gamm <- function(model, type = NULL, seed = NULL,
    conditioning = NULL, ..., n_dharma = NULL) {
  diagnostic_data(mixed_diagnostic_model(model, conditioning), type, seed, n_dharma = n_dharma)
}

#' @export
#' @rdname appraise
appraise.gamm <- function(model, conditioning = NULL, ...) {
  appraise(mixed_diagnostic_model(model, conditioning), ...)
}
#' @export
#' @rdname qq_plot
qq_plot.gamm <- function(model, conditioning = NULL, ...) {
  qq_plot(mixed_diagnostic_model(model, conditioning), ...)
}
#' @export
#' @rdname worm_plot
worm_plot.gamm <- function(model, conditioning = NULL, ...) {
  worm_plot(mixed_diagnostic_model(model, conditioning), ...)
}

#' @export
response_distribution.gamm <- function(model, conditioning = NULL, ...) {
  response_distribution(mixed_diagnostic_model(model, conditioning))
}
#' @export
response_distribution.gamm4 <- function(model, conditioning = NULL, ...) {
  response_distribution(mixed_diagnostic_model(model, conditioning))
}

# The fitter can simulate responses without first computing means and variances
# averaged over random effects.
gamm4_response_generator <- function(model, conditioning) {
  mer_response_generator(model$mer, conditioning)
}

mer_response_generator <- function(mer, conditioning) {
  fam <- stats::family(mer)
  re <- if (conditioning == "marginal") NA else NULL
  function() {
    value <- stats::simulate(mer, nsim = 1, re.form = re)[[1L]]
    if (fam$family == "binomial") {
      if (is.matrix(value)) value <- value[, 1L] / pmax(rowSums(value), 1)
      if (is.factor(value)) value <- as.numeric(value != levels(value)[1L])
    }
    as.vector(value)
  }
}

# Called by the public simulate.gamm wrapper in simulate-methods.R.
simulate_gamm_response <- function(object, nsim = 1L, seed = NULL,
                                   conditioning = NULL) {
  nsim <- validate_diagnostic_bins(nsim, "nsim")
  if (is_gamm4(object) && identical(conditioning, "marginal")) {
    proxy <- object$gam
    dist <- list(rd = gamm4_response_generator(object, conditioning))
  } else {
    proxy <- mixed_diagnostic_model(object, conditioning)
    dist <- response_distribution(proxy)
  }
  if (!is.function(dist$rd)) stop(dist$dependence_reason %||%
    "No fitted joint response generator is available.", call. = FALSE)
  diagnostic_seed(seed, {
    value <- replicate(nsim, dist$rd(), simplify = FALSE)
    value <- as.data.frame(value)
    names(value) <- paste0("sim_", seq_len(nsim))
    value <- value[model_row_map(proxy), , drop = FALSE]
    class(value) <- c("simulate_gratia", class(value))
    attr(value, "conditioning") <- mixed_conditioning(object, conditioning)
    value
  })
}

#' @export
#' @rdname appraise
appraise.gratia_diagnostic_proxy <- function(model, type = "quantile", ...) {
  appraise.gam(model, type = type, ...)
}
#' @export
#' @rdname qq_plot
qq_plot.gratia_diagnostic_proxy <- function(model, type = "quantile", ...) {
  qq_plot.gam(model, type = type, ...)
}
#' @export
#' @rdname worm_plot
worm_plot.gratia_diagnostic_proxy <- function(model, type = "quantile", ...) {
  worm_plot.gam(model, type = type, ...)
}

#' @export
#' @rdname quantile_residuals
quantile_residuals.gratia_diagnostic_proxy <- function(model,
    type = c("pit", "quantile"), seed = NULL, conditioning = NULL, ...) {
  type <- match.arg(type)
  prepared <- diagnostic_plot_source(model, type = type, seed = seed,
    conditioning = conditioning, ...)
  if (!inherits(prepared, "gratia_diagnostic_proxy"))
    return(quantile_residuals(prepared, type = type, seed = seed))
  value <- distribution_quantile_residuals(prepared$distribution, prepared$y,
    type = type, seed = seed)
  restore_model_rows(value, prepared)
}

# For Gaussian mixed models, the penalised least-squares equations provide the
# inverse covariance of fixed and fitted random effects. Draw these effects
# together, keeping covariance parameters at their estimates. Use sparse design
# and inverse-covariance matrices instead of a dense covariance for all effects.
lme_joint_diagnostic_draws <- function(model, n) {
  dat <- lme_fitting_data(model)
  fixed <- stats::delete.response(stats::terms(model))
  frame <- stats::model.frame(fixed, data = dat, na.action = stats::na.pass)
  frame <- lme_fitted_factor_data(model, frame)
  X <- stats::model.matrix(fixed, frame,
    contrasts.arg = model$contrasts[intersect(names(model$contrasts), names(frame))])
  errors <- lme_response_error(model)
  random <- lme_random_error(model)
  designs <- penalties <- list()
  for (b in random$terms) {
    g <- match(b$group, unique(b$group))
    k <- ncol(b$z)
    designs[[length(designs) + 1L]] <- Matrix::sparseMatrix(
      i = rep(seq_len(nrow(X)), k),
      j = as.vector(outer((g - 1L) * k, seq_len(k), `+`)),
      x = as.vector(b$z), dims = c(nrow(X), max(g) * k))
    penalties <- c(penalties, rep(list(solve(b$covariance)), max(g)))
  }
  H <- do.call(cbind, c(list(Matrix::Matrix(X, sparse = TRUE)), designs))
  P <- Matrix::bdiag(c(list(Matrix::Matrix(0, ncol(X), ncol(X),
    sparse = TRUE)), penalties))
  R <- Matrix::Diagonal(nrow(X), 1 / errors$sd^2)
  for (b in errors$blocks) {
    R[b$i, b$i] <- chol2inv(t(b$factor))
  }
  precision <- Matrix::forceSymmetric(Matrix::crossprod(H, R %*% H) + P)
  L <- Matrix::expand(Matrix::Cholesky(precision, perm = FALSE,
    LDL = FALSE))$L
  perturbation <- Matrix::solve(Matrix::t(L), matrix(stats::rnorm(ncol(H) * n),
    nrow = ncol(H)))
  list(X = X, design = H, perturbation = perturbation,
    error = errors, random = random)
}

#' @export
diagnostic_posterior_sampler.gamm <- function(model, n, seed = NULL,
    conditioning = NULL, ...) {
  conditioning <- mixed_conditioning(model, conditioning)
  if (conditioning == "gam_component") {
    return(diagnostic_posterior_sampler(model$gam, n, seed = seed, ...))
  }
  diagnostic_gaussian_posterior_options(list(...), "gamm")
  pql <- !is.null(model$lme$y)
  if (pql && !lme_response_error(model$lme)$independent) {
    stop("PQL working correlation does not specify a joint response law for ",
      "posterior predictive checks.", call. = FALSE)
  }
  if (pql && !is.function(fix_family_rd(stats::family(model$gam))$rd)) {
    stop("PQL posterior predictive checks require an available response-family ",
      "generator; quasi families do not define a sampling distribution.", call. = FALSE)
  }
  proxy <- gamm_diagnostic_model(model, conditioning)
  joint <- lme_joint_diagnostic_draws(model$lme, n)
  sampling_weights <- list(...)$.response_weights %||%
    binomial_sampling_weights(proxy, proxy$prior.weights)
  idx <- match(rownames(proxy$model), rownames(lme_fitting_data(model$lme)))
  rows <- which(!is.na(model_row_map(proxy)))
  function(i) {
    if (conditioning == "conditional") {
      delta <- as.vector(joint$design %*% joint$perturbation[, i])
    } else {
      delta <- as.vector(joint$X %*%
        joint$perturbation[seq_len(ncol(joint$X)), i]) + joint$random$draw()
    }
    response <- if (pql) {
      mu <- proxy$family$linkinv(proxy$linear.predictors + delta[idx])
      distribution_from_family(proxy$family, mu, sampling_weights,
        scale = model$gam$sig2 %||% model$lme$sigma^2, y = proxy$y)$rd()
    } else proxy$fitted.values + (delta + joint$error$draw())[idx]
    diagnostic_response_table(response, i, rows = rows)
  }
}

#' @export
family_name.gratia_diagnostic_proxy <- function(object, ...) object$family$family

# Predictors formed from the same Gaussian effects are dependent when their
# covariance is nonzero. Rows with nonzero entries in shared columns are possible
# matches; check their dot products without storing covariances for every pair
# of observations.
row_dependence_from_design <- function(design, tolerance = 1e-10) {
  design <- Matrix::drop0(Matrix::Matrix(design, sparse = TRUE))
  if (nrow(design) < 2L || !ncol(design)) return(FALSE)
  nz <- Matrix::summary(design)
  if (!nrow(nz)) return(FALSE)
  col_rows <- split(nz$i, nz$j)
  row_cols <- split(nz$j, nz$i)
  variance <- as.numeric(Matrix::rowSums(design^2))
  for (row in names(row_cols)) {
    i <- as.integer(row)
    candidates <- unique(unlist(col_rows[as.character(row_cols[[row]])],
      use.names = FALSE))
    candidates <- candidates[candidates > i & variance[candidates] > 0]
    if (!length(candidates) || variance[i] == 0) next
    covariance <- as.numeric(design[candidates, , drop = FALSE] %*%
      Matrix::t(design[i, , drop = FALSE]))
    if (any(abs(covariance) > tolerance * sqrt(variance[i] * variance[candidates])))
      return(TRUE)
  }
  FALSE
}

lme_fitting_data <- function(model) {
  # getData.lme applies na.action before subset, although omission indices refer
  # to the subset. The stored data plus fitted row names preserves both orders.
  data <- model$data %||% nlme::getData(model)
  i <- match(rownames(model$groups), rownames(data))
  if (anyNA(i) || anyDuplicated(rownames(data)))
    stop("Cannot align native lme fitting rows.", call. = FALSE)
  lme_fitted_factor_data(model, data[i, , drop = FALSE])
}

lme_fitted_factor_data <- function(model, data) {
  # Subsetting a factor can leave unused levels. Restore the fitted levels and
  # contrasts before building the fixed- or random-effect design matrices.
  contrasts <- model$contrasts
  for (nm in intersect(names(contrasts), names(data))) {
    coding <- contrasts[[nm]]
    if (!is.matrix(coding) || is.null(rownames(coding))) next
    value <- as.character(data[[nm]])
    if (any(!is.na(value) & !value %in% rownames(coding)))
      stop("Cannot align native lme fitted factor levels.", call. = FALSE)
    data[[nm]] <- factor(value, levels = rownames(coding),
      ordered = is.ordered(data[[nm]]))
    stats::contrasts(data[[nm]]) <- coding
  }
  data
}

lme_random_design <- function(terms, n) {
  if (!length(terms)) return(Matrix::Matrix(0, n, 0, sparse = TRUE))
  designs <- lapply(terms, function(b) {
    g <- match(b$group, unique(b$group))
    k <- ncol(b$z)
    z <- b$z %*% b$factor
    Matrix::sparseMatrix(i = rep(seq_len(n), k),
      j = as.vector(outer((g - 1L) * k, seq_len(k), `+`)),
      x = as.vector(z), dims = c(n, max(g) * k))
  })
  do.call(cbind, designs)
}

# Direct mixed-model fits use the same preparation functions as gamm/gamm4
# objects.
lme_diagnostic_components <- function(model) {
  if (!is.null(model$y) || inherits(model, "glmmPQL")) {
    stop("A PQL working lme component is not a Gaussian response model; ",
      "diagnose the whole gamm fit instead.", call. = FALSE)
  }
  data <- lme_fitting_data(model)
  frame <- stats::model.frame(stats::formula(model), data = data,
    na.action = stats::na.pass)
  y <- stats::model.response(frame)
  gam <- new_diagnostic_proxy(y, as.vector(model$fitted[, ncol(model$fitted)]),
    as.vector(model$fitted[, ncol(model$fitted)]), stats::gaussian(), list(),
    model = frame, na.action = model$na.action)
  list(gam = gam, lme = model)
}

lme_diagnostic_model <- function(model, conditioning = NULL) {
  conditioning <- match.arg(conditioning %||% "conditional",
    c("conditional", "marginal"))
  proxy <- gamm_diagnostic_model(lme_diagnostic_components(model), conditioning)
  proxy$original <- model
  proxy
}

#' @export
diagnostic_data.lme <- function(model, type = NULL, seed = NULL,
    conditioning = NULL, ..., n_dharma = NULL) {
  diagnostic_data(lme_diagnostic_model(model, conditioning), type, seed, n_dharma = n_dharma)
}
#' @export
response_distribution.lme <- function(model, conditioning = NULL, ...) {
  lme_diagnostic_model(model, conditioning)$distribution
}
#' @export
#' @rdname quantile_residuals
quantile_residuals.lme <- function(model, type = c("pit", "quantile"),
    seed = NULL, conditioning = NULL, ...) {
  quantile_residuals(lme_diagnostic_model(model, conditioning),
    match.arg(type), seed = seed, ...)
}
#' @export
#' @rdname appraise
appraise.lme <- function(model, conditioning = NULL, ...) {
  appraise(lme_diagnostic_model(model, conditioning), ...)
}
#' @export
#' @rdname qq_plot
qq_plot.lme <- function(model, conditioning = NULL, ...) {
  qq_plot(lme_diagnostic_model(model, conditioning), ...)
}
#' @export
#' @rdname worm_plot
worm_plot.lme <- function(model, conditioning = NULL, ...) {
  worm_plot(lme_diagnostic_model(model, conditioning), ...)
}

#' @export
diagnostic_data.merMod <- function(model, type = NULL, seed = NULL,
    conditioning = NULL, ..., n_dharma = NULL) {
  diagnostic_data(mer_diagnostic_model(model,
    match.arg(conditioning %||% "conditional", c("conditional", "marginal"))),
    type, seed, n_dharma = n_dharma)
}
#' @export
response_distribution.merMod <- function(model, conditioning = NULL, ...) {
  mer_diagnostic_model(model, match.arg(conditioning %||% "conditional",
    c("conditional", "marginal")))$distribution
}
#' @export
#' @rdname quantile_residuals
quantile_residuals.merMod <- function(model, type = c("pit", "quantile"),
    seed = NULL, conditioning = NULL, ...) {
  quantile_residuals(mer_diagnostic_model(model,
    match.arg(conditioning %||% "conditional", c("conditional", "marginal"))),
    match.arg(type), seed = seed, ...)
}
#' @export
#' @rdname appraise
appraise.merMod <- function(model, conditioning = NULL, ...) {
  appraise(mer_diagnostic_model(model,
    match.arg(conditioning %||% "conditional", c("conditional", "marginal"))), ...)
}
#' @export
#' @rdname qq_plot
qq_plot.merMod <- function(model, conditioning = NULL, ...) {
  qq_plot(mer_diagnostic_model(model,
    match.arg(conditioning %||% "conditional", c("conditional", "marginal"))), ...)
}
#' @export
#' @rdname worm_plot
worm_plot.merMod <- function(model, conditioning = NULL, ...) {
  worm_plot(mer_diagnostic_model(model,
    match.arg(conditioning %||% "conditional", c("conditional", "marginal"))), ...)
}


# Use nlme's reStruct model matrices to handle multiple covariance blocks and
# preserve the fitted contrasts. Match design columns to covariance names before
# calculating variances, drawing responses or forming the inverse posterior
# covariance.
lme_random_predictors <- function(model, data = lme_fitting_data(model)) {
  rs <- model$modelStruct$reStruct
  contrasts <- model$contrasts
  direct <- intersect(names(contrasts), names(data))
  direct <- direct[vapply(data[direct], is.factor, logical(1))]
  for (nm in direct) stats::contrasts(data[[nm]]) <- contrasts[[nm]]
  remaining <- contrasts[setdiff(names(contrasts), direct)]
  design <- stats::model.matrix(rs, data = data,
    contrast = if (length(remaining)) remaining else NULL)
  counts <- attr(design, "ncols")
  labels <- attr(design, "nams")
  if (length(counts) != length(rs) || length(labels) != length(rs))
    stop("Native lme random design blocks are not aligned.", call. = FALSE)
  ends <- cumsum(counts)
  starts <- ends - counts + 1L
  out <- lapply(seq_along(rs), function(i) {
    nm <- names(rs)[i]
    z <- design[, seq.int(starts[i], ends[i]), drop = FALSE]
    colnames(z) <- labels[[i]]
    list(z = z, group = as.character(model$groups[[nm]]))
  })
  names(out) <- names(rs)
  out
}

lme_random_terms <- function(model, data = lme_fitting_data(model)) {
  rs <- model$modelStruct$reStruct
  predictors <- lme_random_predictors(model, data)
  lapply(seq_along(rs), function(i) {
    z <- predictors[[i]]$z
    covariance <- nlme::pdMatrix(rs[[i]]) * model$sigma^2
    columns <- match(colnames(covariance), colnames(z))
    if (ncol(z) != ncol(covariance) || anyNA(columns) ||
        anyDuplicated(columns)) {
      stop("Native lme random predictor columns are not aligned with their ",
        "covariance parameters.", call. = FALSE)
    }
    z <- z[, columns, drop = FALSE]
    list(z = z, covariance = covariance, factor = t(chol(covariance)),
      group = predictors[[i]]$group)
  })
}
