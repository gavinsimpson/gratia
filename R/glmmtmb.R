# glmmTMB's random-effect parameterization is kept behind this adapter. In
# particular, never use re.form = NA to obtain a smooth: that removes its
# penalized coefficients too.
glmmtmb_component <- function(component) {
  match.arg(component, c("cond", "zi", "disp"))
}

glmmtmb_smooth_info <- function(object, component) {
  info <- object$modelInfo$reTrms[[component]]$smooth_info
  info[lengths(info) > 0L]
}

# Follow mgcv's predictor order and labels, including predictors with no
# smooths. A missing zero-inflation predictor does not consume an index.
glmmtmb_components <- function(object) {
  components <- c("cond", "zi", "disp")
  fixed <- glmmTMB::fixef(object)
  active <- vapply(components, function(component) {
    length(fixed[[component]]) > 0L ||
      length(object$modelInfo$reStruc[[paste0(component, "ReStruc")]]) > 0L
  }, logical(1))
  active[1L] <- TRUE
  components[active]
}

# Constant dispersion is a scale parameter, rather than an additional
# predictor for display. Zero-inflation counts even when intercept-only.
glmmtmb_predictor_count <- function(object) {
  components <- glmmtmb_components(object)
  if (!length(all.vars(stats::formula(object, component = "disp")))) {
    components <- setdiff(components, "disp")
  }
  length(components)
}

glmmtmb_smooths <- function(object) {
  components <- glmmtmb_components(object)
  out <- list()
  for (i in seq_along(components)) {
    component <- components[i]
    sms <- lapply(glmmtmb_smooth_info(object, component), `[[`, "sm")
    for (sm in sms) {
      if (i > 1L) sm$label <- sub("(", paste0(".", i - 1L, "("), sm$label, fixed = TRUE)
      attr(sm, "gratia.component") <- component
      out[[length(out) + 1L]] <- sm
    }
  }
  out
}

#' @rdname glmmTMB-methods
#' @export
smooths.glmmTMB <- function(object, ...) {
  vapply(glmmtmb_smooths(object), `[[`, character(1), "label")
}

#' @rdname n_smooths
#' @details
#' For `glmmTMB` models, `n_smooths()` counts smooth terms across the
#' conditional, zero-inflation and dispersion predictors. Each fitted smooth
#' is counted once; native random effects are excluded. Models without smooths
#' return integer zero.
#' @export
n_smooths.glmmTMB <- function(object) {
  length(glmmtmb_smooths(object))
}

#' @rdname glmmTMB-methods
#' @export
get_smooths_by_id.glmmTMB <- function(object, id, ...) {
  glmmtmb_smooths(object)[id]
}

glmmtmb_all_smooths <- function(object, covariance) {
  components <- glmmtmb_components(object)
  models <- lapply(components, function(component)
    glmmtmb_smooth_model(object, component, covariance = FALSE))
  names(models) <- components
  mixed_all_smooths(models, glmmtmb_smooths(object), object$fit$parfull,
    if (covariance) function(index) glmmtmb_joint_covariance(object, index),
    glmmtmb_predictor_count(object))
}

# Return the covariance of selected *joint* parameters, marginalizing all the
# others. Subsetting the precision before solving would condition on them.
glmmtmb_joint_covariance <- function(object, index) {
  sdr <- object$sdr
  p <- object$fit$parfull
  fixed_only <- identical(names(sdr$par.fixed), names(p)) &&
    identical(dim(sdr$cov.fixed), c(length(p), length(p)))
  if (is.null(sdr$jointPrecision) && !fixed_only) {
    sdr <- TMB::sdreport(object$obj, getJointPrecision = TRUE)
  }
  if (!isTRUE(sdr$pdHess)) {
    stop("glmmTMB covariance is unavailable: Hessian is not positive definite.")
  }
  mixed_joint_covariance(sdr, p, index, "glmmTMB")
}

glmmtmb_smooth_model <- function(object, component = NULL, covariance = TRUE) {
  if (is.null(component)) return(glmmtmb_all_smooths(object, covariance))
  component <- glmmtmb_component(component)
  info <- object$modelInfo$reTrms[[component]]$smooth_info
  positions <- which(lengths(info) > 0L)
  tags <- switch(component, cond = c("beta", "b"),
    zi = c("betazi", "bzi"), disp = c("betadisp", "bdisp"))
  p <- object$fit$parfull
  fixed <- which(names(p) == tags[1L])
  random <- which(names(p) == tags[2L])
  blocks <- object$modelInfo$reStruc[[paste0(component, "ReStruc")]]
  if (is.null(info)) info <- vector("list", length(blocks))
  sizes <- vapply(blocks, function(x) x$blockSize * x$blockReps, numeric(1))
  if (sum(sizes) != length(random) || length(blocks) != length(info)) {
    stop("Cannot align glmmTMB smooths with random-effect blocks.")
  }
  ends <- cumsum(c(0L, sizes))
  smooths <- lapply(info[positions], `[[`, "sm")
  # Keep parametric coefficients as well, for uncertainty in the overall mean.
  null <- unlist(lapply(info[positions], function(x) x$re$beta_ind))
  parametric <- setdiff(seq_along(fixed), null)
  transforms <- vector("list", length(positions))
  indices <- fixed[parametric]
  offset <- length(parametric)
  for (j in seq_along(positions)) {
    pos <- positions[j]
    re <- info[[pos]]$re
    sm <- smooths[[j]]
    nr <- sizes[pos]
    nf <- ncol(re$Xf)
    if (length(re$rand) != 1L || ncol(re$rand[[1L]]) != nr ||
        length(re$beta_ind) != nf || any(re$beta_ind > length(fixed))) {
      stop("Unsupported or inconsistent glmmTMB smooth coefficient mapping.")
    }
    nb <- length(re$pen.ind)
    B <- mixed_smooth_transform(re, "glmmTMB")
    transforms[[j]] <- B
    indices <- c(indices, random[seq.int(ends[pos] + 1L, ends[pos + 1L])],
      fixed[re$beta_ind])
    sm$first.para <- offset + 1L
    offset <- offset + nb
    sm$last.para <- offset
    smooths[[j]] <- sm
  }
  A <- as.matrix(Matrix::bdiag(c(list(diag(length(parametric))), transforms)))
  coefficients <- drop(A %*% p[indices])
  names(coefficients) <- c(names(glmmTMB::fixef(object)[[component]])[parametric],
    unlist(lapply(smooths, function(sm) paste0(sm$label, ".", seq_len(sm$df)))))
  V <- if (covariance) A %*% glmmtmb_joint_covariance(object, indices) %*% t(A) else NULL
  ff <- stats::formula(object, component = component)
  frame <- stats::model.frame(object)
  # terms() from glmmTMB contains the parametric terms, without evaluating s().
  tt <- stats::terms(object, component = component)
  X <- stats::model.matrix(object, component = component)
  if (ncol(X) != length(fixed)) stop("Rank-deficient glmmTMB smooth models are not supported.")
  cm <- colMeans(X)[parametric]
  for (sm in smooths) cm <- c(cm, colMeans(mgcv::PredictMat(sm, frame)))
  structure(list(smooth = smooths, coefficients = coefficients, Vp = V,
    cmX = unname(cm), model = frame, terms = tt, formula = ff,
    coefficient_transform = A, joint_indices = indices,
    call = object$call, component = component, fitter = "glmmTMB",
    pterms = tt, parametric = seq_along(parametric),
    contrasts = object$modelInfo$contrasts,
    xlevels = stats::.getXlevels(tt, frame),
    pred.formula = if (length(all.vars(stats::delete.response(tt))))
      stats::reformulate(all.vars(stats::delete.response(tt)), env = environment(ff))
      else stats::as.formula("~1", env = environment(ff)),
    var.summary = lapply(frame[intersect(names(frame), all.vars(ff))],
      function(x) if (is.numeric(x))
      c(min(x), stats::median(x), max(x)) else x),
    na.action = stats::na.action(object)), class = c("gratia_glmmtmb_smooth", "gratia_mixed_smooth"),
    gratia.n_predictors = glmmtmb_predictor_count(object))
}

#' Effects and diagnostics for glmmTMB models
#'
#' Evaluate and plot the stored mgcv smooths in a glmmTMB fit. Coefficients and
#' their joint covariance are transformed back to the constrained mgcv basis.
#' Intervals use TMB's joint covariance, including fixed--random covariance;
#' they are not mgcv frequentist or smoothness-corrected intervals.
#'
#' @param object,model A fitted `glmmTMB` model.
#' @param select Smooth or native random-effect coefficient selection, across
#'   all linear predictors. Numeric indices select smooths first, then native
#'   random coefficients. Native labels have the form `(1 | g)` and `(x | g)`;
#'   later predictors append `.1`, `.2`, etc. `smooth_estimates()` and
#'   `smooth_coefs()` select smooths only.
#' @param terms Character vector selecting parametric terms, as returned by
#'   `parametric_terms()`. Later predictors append `.1`, `.2`, etc.
#' @param id Integer positions of smooths across all linear predictors.
#' @param unconditional,frequentist Must be `FALSE`; mgcv-specific covariance
#'   alternatives are not available.
#' @param parametric Include parametric effect plots? `NULL` follows
#'   `draw.gam()`: include them unless `select` is supplied. Use `terms` to
#'   choose parametric terms, and `parametric = FALSE` to omit them.
#' @param residuals Must be `FALSE`; partial residual overlays are not supported.
#' @param ranef_lines Logical; replace QQ plots for a simple native random
#'   intercept and numeric slope term, `(1 + x | g)`, with one panel of lines
#'   showing `b[0,g] + x * b[1,g]`? Defaults to `FALSE`. Both coefficients must
#'   be selected. Multiple slopes, interactions, factor slopes, transformed
#'   slopes and structured covariance terms retain separate QQ plots.
#' @param ... Arguments passed to the corresponding gratia method.
#' @param envir Optional environment for evaluating model covariates.
#' @details
#' Smooth support requires stored smooth and random-effect metadata. Unsupported
#' multi-penalty structures, mapped joint parameters and rank-deficient smooth
#' models give errors. Factor-by and tensor smooth availability is determined
#' by glmmTMB; gratia does not expand the smooth types glmmTMB can fit.
#' Use `draw(model, surface = "3d", ci_3d = TRUE)` for interval surfaces on
#' smooths and numeric parametric interactions in any linear predictor.
#' `ci_3d_alpha` and `ci_3d_col` control their opacity and fill colour.
#' Pass `cyclic = "polar"` and optional `polar_args` to `draw()` or
#' `assemble()` for polar plots of supported one-dimensional cyclic smooths.
#' See [draw.gam()] for the coordinate options.
#'
#' Parametric effects use fitted contrast coding and exclude smooth null-space
#' coefficients. Their intervals include uncertainty in the intercept of their
#' own linear predictor by default, as for GAMs. `overall_uncertainty = FALSE`
#' gives term-only intervals. Models without smooths are also supported.
#'
#' Native random effects use one Gaussian QQ panel per coefficient type and
#' grouping factor, matching mgcv random-effect smooth plots. The internal
#' random effects representing smooths are excluded from these panels.
#' With `ranef_lines = TRUE`, eligible intercept-slope pairs use a common grid
#' over the fitting range of the numeric covariate, or its supplied `data`
#' values. Lines show random contributions only, without population-level
#' terms or uncertainty bands. Different formula terms are never combined.
#'
#' Diagnostics use native Pearson residuals by default. QQ simulations hold
#' fitted smooths and grouping effects fixed and draw new responses; no refitting
#' is performed. Supported response families are Gaussian, Poisson, binomial,
#' nbinom1/2, beta, Gamma/ziGamma, Tweedie, lognormal, and zero-truncated
#' Poisson/nbinom1/2,
#' including applicable zero-inflation or hurdle mixtures and varying dispersion.
#' Truncated nbinom1 currently requires `type = "response"` because glmmTMB
#' does not supply its Pearson residuals. Other unavailable residual types fail
#' explicitly. Supported fitted response distributions also provide randomized
#' PIT/quantile residuals and inverse-CDF QQ references, including their
#' zero-inflation and truncation probabilities. Posterior predictive checks draw
#' coefficients and fitted random effects together from their approximate
#' posterior. See [posterior_predictive_plot()] for how covariance and other
#' family parameters are treated. Marginal checks simulate new effects using
#' glmmTMB's response generator. If marginal CDFs are not analytic, residual
#' checks use a shared sample of simulations to calculate moments and ranks.
#' See [diagnostic_support] for conditioning choices and remaining limitations.
#'
#' @return Smooth estimates, coefficients, or plots, as for the GAM methods.
#'   `assemble()` returns a named list of ggplot objects, of class
#'   `"assembled_plots"`. `draw()` combines these plots into a patchwork unless
#'   `wrap = FALSE`, which returns the individual plots instead.
#'   All smooths, native random effects and parametric terms are plotted by
#'   default, as for `draw.gam()`. Smooths in later
#'   linear predictors use mgcv-style labels such as `s.1(x)` and `s.2(x)`.
#'   Predictor order is conditional, zero-inflation (if present), then dispersion
#'   (if present). Use `smooths(object)` and `select` to select individual smooths.
#'   For models with multiple linear predictors, smooth and parametric subtitles
#'   identify the "Conditional model", "Zero-inflation model" or "Dispersion model",
#'   consistently with native random-effect panels. Constant dispersion alone
#'   does not trigger these subtitles.
#'   Saved [parametric_effects()] retain the labels in an optional `.model` column.
#'   `names(assemble(object, parametric = FALSE))` also lists native
#'   random-effect coefficient panels.
#' @name glmmTMB-methods
#' @examples
#' if (requireNamespace("glmmTMB", quietly = TRUE)) {
#'   load_mgcv()
#'   set.seed(42)
#'   d <- data.frame(x = runif(150))
#'   d$y <- sin(6 * d$x) + rnorm(150, sd = 0.2)
#'   m <- glmmTMB::glmmTMB(y ~ s(x, k = 8), data = d, REML = TRUE)
#'   n_smooths(m) # one smooth term
#'   draw(m)
#'   # Get individual ggplot objects for further customization
#'   panels <- assemble(m)
#'   panels[["s(x)"]]
#'   appraise(m, seed = 42)
#'
#'   # Parametric terms and native random intercepts and slopes
#'   d$z <- rnorm(150)
#'   d$g <- factor(rep(seq_len(10), each = 15))
#'   d$y <- d$y + 0.5 * d$z + rnorm(10)[d$g] + rnorm(10)[d$g] * d$z
#'   mixed <- glmmTMB::glmmTMB(y ~ z + s(x, k = 8) + (1 + z | g),
#'     data = d, REML = TRUE)
#'   draw(mixed) # separate QQ panels for intercepts and slopes
#'   draw(mixed, ranef_lines = TRUE) # random contributions only
#'   if (requireNamespace("ggcube", quietly = TRUE)) {
#'     interaction <- glmmTMB::glmmTMB(y ~ x:z, data = d)
#'     draw(interaction, surface = "3d", ci_3d = TRUE, n_2d = 15)
#'   }
#' }
NULL

glmmtmb_covariance_options <- function(unconditional, frequentist) {
  if (!identical(unconditional, FALSE) || !identical(frequentist, FALSE)) {
    stop("glmmTMB supports TMB joint covariance only; unconditional and frequentist must be FALSE.")
  }
}

#' @rdname glmmTMB-methods
#' @export
smooth_estimates.glmmTMB <- function(object, ...,
    unconditional = FALSE, frequentist = FALSE) {
  glmmtmb_covariance_options(unconditional, frequentist)
  if (!length(glmmtmb_smooths(object))) stop("No smooths in glmmTMB model.")
  smooth_estimates(glmmtmb_smooth_model(object), ...)
}

#' @rdname glmmTMB-methods
#' @export
smooth_coefs.glmmTMB <- function(object, select, ...) {
  smooth_coefs.gam(glmmtmb_smooth_model(object, covariance = FALSE), select, ...)
}

glmmtmb_plot_model <- function(object, residuals, unconditional,
    frequentist, ranef_lines) {
  glmmtmb_covariance_options(unconditional, frequentist)
  if (!identical(residuals, FALSE)) {
    stop("glmmTMB partial residual overlays are not yet supported.")
  }
  if (!is.logical(ranef_lines) || length(ranef_lines) != 1L || is.na(ranef_lines)) {
    stop("'ranef_lines' must be TRUE or FALSE.", call. = FALSE)
  }
  # ranef() also needs the report; retain a newly computed one on this local
  # copy so smooth/fixed covariance and native effects share the same report.
  if (is.null(object$sdr)) {
    object$sdr <- TMB::sdreport(object$obj, getJointPrecision = TRUE)
  }
  model <- glmmtmb_smooth_model(object)
  model$native_random_effects <- glmmtmb_random_effects(object)
  model$ranef_lines <- ranef_lines
  model
}

#' @rdname glmmTMB-methods
#' @export
assemble.glmmTMB <- function(object, ..., parametric = NULL,
    residuals = FALSE, unconditional = FALSE, frequentist = FALSE,
    ranef_lines = FALSE, envir = NULL) {
  model <- glmmtmb_plot_model(object, residuals, unconditional,
    frequentist, ranef_lines)
  assemble(model, ..., parametric = parametric, envir = envir)
}

#' @rdname glmmTMB-methods
#' @export
#' @inheritSection draw Label layout
draw.glmmTMB <- function(object, ..., parametric = NULL,
    residuals = FALSE, unconditional = FALSE, frequentist = FALSE,
    ranef_lines = FALSE, envir = NULL) {
  model <- glmmtmb_plot_model(object, residuals, unconditional,
    frequentist, ranef_lines)
  draw.gam(model, ..., parametric = parametric, envir = envir)
}

#' @export
model_context.glmmTMB <- function(object, component = "cond") {
  if (is.null(component)) component <- "cond"
  component <- glmmtmb_component(component)
  ff <- stats::formula(object, component = component)
  list(model = object, component = component, formula = ff,
    terms = stats::terms(object, component = component),
    data = stats::model.frame(object), provenance = "evaluated",
    call = object$call, envir = environment(ff),
    na.action = attr(object$frame, "na.action"))
}
