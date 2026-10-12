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
glmmtmb_smooths <- function(object) {
  components <- c("cond", "zi", "disp")
  fixed <- glmmTMB::fixef(object)
  active <- vapply(components, function(component) {
    length(fixed[[component]]) > 0L ||
      length(object$modelInfo$reStruc[[paste0(component, "ReStruc")]]) > 0L
  }, logical(1))
  active[1L] <- TRUE # The conditional predictor exists even with only an offset.
  components <- components[active]
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

#' @rdname glmmTMB-methods
#' @export
get_smooths_by_id.glmmTMB <- function(object, id, ...) {
  glmmtmb_smooths(object)[id]
}

# Combine coefficient transformations before extracting joint covariance. The
# lpi attribute keeps overall-mean uncertainty local to each linear predictor.
glmmtmb_all_smooths <- function(object, covariance) {
  sms <- glmmtmb_smooths(object)
  if (!length(sms)) stop("No smooths in glmmTMB model.")
  components <- unique(vapply(sms, attr, character(1), "gratia.component"))
  models <- lapply(components, function(component)
    glmmtmb_smooth_model(object, component, covariance = FALSE))
  names(models) <- components
  out <- models[[1L]]
  sizes <- vapply(models, function(x) length(x$coefficients), integer(1))
  starts <- cumsum(c(0L, sizes))
  smooths <- list()
  for (i in seq_along(models)) {
    for (sm in models[[i]]$smooth) {
      sm$first.para <- sm$first.para + starts[i]
      sm$last.para <- sm$last.para + starts[i]
      j <- length(smooths) + 1L
      sm$label <- sms[[j]]$label
      attr(sm, "gratia.component") <- components[i]
      smooths[[j]] <- sm
    }
  }
  indices <- unlist(lapply(models, `[[`, "joint_indices"), use.names = FALSE)
  A <- as.matrix(Matrix::bdiag(lapply(models, `[[`, "coefficient_transform")))
  out$coefficients <- drop(A %*% object$fit$parfull[indices])
  out$Vp <- if (covariance) A %*% glmmtmb_joint_covariance(object, indices) %*% t(A) else NULL
  out$cmX <- unlist(lapply(models, `[[`, "cmX"), use.names = FALSE)
  out$smooth <- smooths
  out$coefficient_transform <- A
  out$joint_indices <- indices
  out$component <- NULL
  out$contexts <- lapply(models, function(x) list(formula = x$formula, terms = x$terms))
  attr(out$formula, "lpi") <- lapply(seq_along(models), function(i)
    seq.int(starts[i] + 1L, starts[i + 1L]))
  # Coefficient names also identify the predictor, just like the smooth labels.
  names(out$coefficients) <- unlist(lapply(seq_along(models), function(i) {
    nm <- names(models[[i]]$coefficients)
    for (sm in smooths[vapply(smooths, function(x)
        attr(x, "gratia.component") == components[i], logical(1))]) {
      nm[smooth_coef_indices(sm) - starts[i]] <- paste0(sm$label, ".", seq_len(sm$df))
    }
    nm
  }), use.names = FALSE)
  out
}

# Return the covariance of selected *joint* parameters, marginalizing all the
# others. Subsetting the precision before solving would condition on them.
glmmtmb_joint_covariance <- function(object, index) {
  sdr <- object$sdr
  if (is.null(sdr$jointPrecision)) {
    sdr <- TMB::sdreport(object$obj, getJointPrecision = TRUE)
  }
  if (!isTRUE(sdr$pdHess)) {
    stop("glmmTMB covariance is unavailable: Hessian is not positive definite.")
  }
  Q <- sdr$jointPrecision
  p <- object$fit$parfull
  if (is.null(Q) || nrow(Q) != length(p) ||
      (!is.null(rownames(Q)) && !identical(rownames(Q), names(p)))) {
    stop("Cannot align glmmTMB joint precision with fitted parameters (mapped parameters are not supported).")
  }
  E <- Matrix::sparseMatrix(i = index, j = seq_along(index), x = 1,
    dims = c(nrow(Q), length(index)))
  V <- as.matrix(Matrix::solve(Q, E)[index, , drop = FALSE])
  if (any(!is.finite(V))) stop("glmmTMB covariance contains non-finite values.")
  (V + t(V)) / 2
}

glmmtmb_smooth_model <- function(object, component = NULL, covariance = TRUE) {
  if (is.null(component)) return(glmmtmb_all_smooths(object, covariance))
  component <- glmmtmb_component(component)
  info <- object$modelInfo$reTrms[[component]]$smooth_info
  positions <- which(lengths(info) > 0L)
  if (!length(positions)) stop("No smooths in glmmTMB component '", component, "'.")
  tags <- switch(component, cond = c("beta", "b"),
    zi = c("betazi", "bzi"), disp = c("betadisp", "bdisp"))
  p <- object$fit$parfull
  fixed <- which(names(p) == tags[1L])
  random <- which(names(p) == tags[2L])
  blocks <- object$modelInfo$reStruc[[paste0(component, "ReStruc")]]
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
    # Map [random coefficients, null-space coefficients] back to the original
    # smooth basis, undoing smooth2random's permutation, scaling and rotation.
    B <- matrix(0, nb, nb)
    penalized <- which(re$pen.ind != 0L)
    unpenalized <- which(re$pen.ind == 0L)
    if (length(penalized) != nr || length(unpenalized) != nf ||
        !setequal(re$rind, seq_len(nr))) {
      stop("Unsupported smooth2random permutation in glmmTMB fit.")
    }
    B[cbind(penalized, re$rind)] <- 1
    if (nf) B[cbind(unpenalized, nr + seq_len(nf))] <- 1
    B <- re$trans.D * B
    if (!is.null(re$trans.U)) B <- re$trans.U %*% B
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
    call = object$call, component = component,
    var.summary = lapply(frame, function(x) if (is.numeric(x))
      c(min(x), stats::median(x), max(x)) else x),
    na.action = stats::na.action(object)), class = "gratia_glmmtmb_smooth")
}

#' @export
coef.gratia_glmmtmb_smooth <- function(object, ...) object$coefficients
#' @export
model.frame.gratia_glmmtmb_smooth <- function(formula, ...) formula$model
#' @export
formula.gratia_glmmtmb_smooth <- function(x, ...) x$formula
#' @export
terms.gratia_glmmtmb_smooth <- function(x, ...) x$terms
#' @export
get_smooths_by_id.gratia_glmmtmb_smooth <- function(object, id, ...) object$smooth[id]
#' @export
model_context.gratia_glmmtmb_smooth <- function(object, component = NULL) {
  model_context.default(object, component = NULL)
}
#' @export
smooth_estimates.gratia_glmmtmb_smooth <- function(object, select = NULL,
    partial_match = FALSE, ...) {
  if (is.null(object$contexts)) return(smooth_estimates.gam(object,
    select = select, partial_match = partial_match, ...))
  labels <- smooths(object)
  take <- check_user_select_smooths(labels, select = select,
    partial_match = partial_match)
  selected <- object$smooth[which(take)]
  components <- unique(vapply(selected, attr, character(1), "gratia.component"))
  results <- lapply(components, function(component) {
    local <- object
    ctx <- object$contexts[[component]]
    local$formula <- ctx$formula
    attr(local$formula, "lpi") <- attr(object$formula, "lpi")
    local$terms <- ctx$terms
    want <- vapply(selected, function(sm) attr(sm, "gratia.component") == component, logical(1))
    smooth_estimates.gam(local,
      select = vapply(selected[want], `[[`, character(1), "label"), ...)
  })
  out <- dplyr::bind_rows(results)
  attr(out, "tensor_term_order") <- do.call(c, lapply(results, attr, "tensor_term_order"))
  out
}
#' @export
assemble.gratia_glmmtmb_smooth <- function(object, ...) assemble.gam(object, ...)

#' Smooths and diagnostics for glmmTMB models
#'
#' Evaluate and plot the stored mgcv smooths in a glmmTMB fit. Coefficients and
#' their joint covariance are transformed back to the constrained mgcv basis.
#' Intervals use TMB's joint covariance, including fixed--random covariance;
#' they are not mgcv frequentist or smoothness-corrected intervals.
#'
#' @param object,model A fitted `glmmTMB` model.
#' @param select Smooth selection, across all linear predictors.
#' @param id Integer positions of smooths across all linear predictors.
#' @param unconditional,frequentist Must be `FALSE`; mgcv-specific covariance
#'   alternatives are not available.
#' @param parametric,residuals Must be `FALSE`; parametric effect plots and
#'   partial residual overlays are not yet supported.
#' @param ... Arguments passed to the corresponding gratia method.
#' @param envir Optional environment for evaluating model covariates.
#' @details
#' Smooth support requires stored smooth and random-effect metadata. Unsupported
#' multi-penalty structures, mapped joint parameters and rank-deficient smooth
#' models give errors. Factor-by and tensor smooth availability is determined
#' by glmmTMB; gratia does not expand the smooth types glmmTMB can fit.
#'
#' Diagnostics use native Pearson residuals by default. QQ simulations hold
#' fitted smooths and grouping effects fixed and draw new responses; no refitting
#' is performed. Supported response families are Gaussian, Poisson, binomial,
#' nbinom1/2, beta, Gamma/ziGamma, Tweedie, and zero-truncated Poisson/nbinom1/2,
#' including applicable zero-inflation or hurdle mixtures and varying dispersion.
#' Truncated nbinom1 currently requires `type = "response"` because glmmTMB
#' does not supply its Pearson residuals. Other unavailable residual types fail
#' explicitly. PIT and quantile residuals are not supported by these methods.
#'
#' @return Smooth estimates, coefficients, or plots, as for the GAM methods.
#'   All smooths are included by default, as for `draw.gam()`. Smooths in later
#'   linear predictors use mgcv-style labels such as `s.1(x)` and `s.2(x)`.
#'   Predictor order is conditional, zero-inflation (if present), then dispersion
#'   (if present). Use `smooths(object)` and `select` to select individual smooths.
#' @name glmmTMB-methods
#' @examples
#' if (requireNamespace("glmmTMB", quietly = TRUE)) {
#'   library(mgcv)
#'   set.seed(42)
#'   d <- data.frame(x = runif(150))
#'   d$y <- sin(6 * d$x) + rnorm(150, sd = 0.2)
#'   m <- glmmTMB::glmmTMB(y ~ s(x, k = 8), data = d, REML = TRUE)
#'   draw(m)
#'   appraise(m, seed = 42)
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
  smooth_estimates(glmmtmb_smooth_model(object), ...)
}

#' @rdname glmmTMB-methods
#' @export
smooth_coefs.glmmTMB <- function(object, select, ...) {
  smooth_coefs.gam(glmmtmb_smooth_model(object, covariance = FALSE), select, ...)
}

#' @rdname glmmTMB-methods
#' @export
draw.glmmTMB <- function(object, ..., parametric = FALSE,
    residuals = FALSE, unconditional = FALSE, frequentist = FALSE, envir = NULL) {
  glmmtmb_covariance_options(unconditional, frequentist)
  if (!identical(parametric, FALSE) || !identical(residuals, FALSE)) {
    stop("glmmTMB parametric plots and partial residual overlays are not yet supported.")
  }
  draw.gam(glmmtmb_smooth_model(object), ..., parametric = FALSE,
    envir = envir)
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
