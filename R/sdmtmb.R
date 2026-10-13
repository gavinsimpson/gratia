# sdmTMB stores smooth null-space coefficients in bs and penalized weights in
# b_smooth. Delta components are columns of both matrices, not separate bases.
sdmtmb_components <- function(object) {
  if (!requireNamespace("sdmTMB", quietly = TRUE)) {
    stop("Package 'sdmTMB' is required.", call. = FALSE)
  }
  if (is.null(object$last.par.best) || is.null(object$parlist)) {
    stop("Expected a fitted sdmTMB model with stored parameters.",
      call. = FALSE)
  }
  n <- length(object$formula)
  if (!n %in% 1:2 || (n == 2L && !isTRUE(object$family$delta))) {
    stop("Only single-predictor and delta sdmTMB models are supported.",
      call. = FALSE)
  }
  as.character(seq_len(n))
}

sdmtmb_bases <- function(object) {
  stored <- object$smoothers$basis_out
  bases <- if (is.list(stored)) do.call(c, stored)
  if (isTRUE(as.logical(object$tmb_data$has_smooths)) && !length(bases)) {
    stop("sdmTMB smooth metadata is unavailable; refit with sdmTMB >= 1.1.0.",
      call. = FALSE
    )
  }
  bases
}

sdmtmb_smooths <- function(object) {
  components <- sdmtmb_components(object)
  bases <- sdmtmb_bases(object)
  out <- list()
  for (i in seq_along(components)) {
    for (sm in bases) {
      if (i > 1L) {
        sm$label <- sub("(", paste0(".", i - 1L, "("), sm$label, fixed = TRUE)
      }
      attr(sm, "gratia.component") <- components[i]
      out[[length(out) + 1L]] <- sm
    }
  }
  out
}

sdmtmb_parameter_indices <- function(object, tag, component = NULL) {
  values <- object$parlist[[tag]]
  index <- which(names(object$last.par.best) == tag)
  map <- object$tmb_map[[tag]]
  if (length(index) != length(values) ||
    (!is.null(map) && (anyNA(map) || anyDuplicated(map)))) {
    stop("Cannot align sdmTMB coefficients: mapped '", tag,
      "' parameters are not supported.",
      call. = FALSE
    )
  }
  if (!is.null(component)) {
    if (length(dim(values)) != 2L || as.integer(component) > ncol(values)) {
      stop("Cannot align sdmTMB coefficient columns for '", tag, "'.",
        call. = FALSE)
    }
    index <- matrix(index, nrow(values), ncol(values))[, as.integer(component)]
  }
  index
}

sdmtmb_joint_covariance <- function(object, index) {
  sdr <- object$sd_report
  p <- object$last.par.best
  fixed_only <- identical(names(sdr$par.fixed), names(p)) &&
    identical(dim(sdr$cov.fixed), c(length(p), length(p)))
  if (is.null(sdr) || (is.null(sdr$jointPrecision) && !fixed_only)) {
    if (is.null(object$tmb_obj)) {
      stop("sdmTMB covariance is unavailable: ",
        "refit with standard errors enabled.",
        call. = FALSE
      )
    }
    # Current development routes both TMB and RTMB through this helper. Older
    # releases have only the TMB backend. Keep that dependency boundary here.
    ns <- asNamespace("sdmTMB")
    report <- if (exists("sdreport_sdmTMB", ns, inherits = FALSE)) {
      get("sdreport_sdmTMB", ns)
    } else {
      TMB::sdreport
    }
    sdr <- report(object$tmb_obj, getJointPrecision = TRUE)
  }
  mixed_joint_covariance(sdr, p, index, "sdmTMB")
}

sdmtmb_smooth_model <- function(
  object, component = NULL, covariance = TRUE,
  envir = NULL
) {
  components <- sdmtmb_components(object)
  if (is.null(component)) {
    models <- lapply(components, function(component) {
      sdmtmb_smooth_model(object, component, covariance = FALSE, envir = envir)
    })
    names(models) <- components
    return(mixed_all_smooths(
      models, sdmtmb_smooths(object), object$last.par.best,
      if (covariance) function(index) sdmtmb_joint_covariance(object, index),
      length(components)
    ))
  }
  component <- as.character(component)
  if (length(component) != 1L || !component %in% components) {
    stop("Unknown sdmTMB component.", call. = FALSE)
  }
  if (length(object$control$profile) &&
    !identical(object$control$profile, FALSE)) {
    stop("Profiled sdmTMB coefficients are not supported; ",
      "refit with profile = FALSE.",
      call. = FALSE
    )
  }
  i <- as.integer(component)
  fixed <- sdmtmb_parameter_indices(object, if (i == 1L) "b_j" else "b_j2")
  smooths <- sdmtmb_bases(object)
  transforms <- list(diag(length(fixed)))
  indices <- fixed
  random_offset <- null_offset <- 0L
  offset <- length(fixed)
  block <- 0L
  if (length(smooths)) {
    random <- sdmtmb_parameter_indices(object, "b_smooth", component)
    null <- sdmtmb_parameter_indices(object, "bs", component)
    for (j in seq_along(smooths)) {
      sm <- smooths[[j]]
      re <- mgcv::smooth2random(sm, names(object$data), type = 2)
      dims <- vapply(re$rand, ncol, integer(1))
      nr <- sum(dims)
      nf <- ncol(re$Xf)
      # Check fitted blocks before using their ordering, including tensor
      # products with several penalties and factor-by expansions.
      positions <- seq_along(dims) + block
      fitted <- object$smoothers$Zs[positions]
      fitted_dims <- as.integer(object$smoothers$sm_dims[positions])
      if (!identical(unname(dims), fitted_dims) ||
        length(fitted) != length(dims) ||
        !all(vapply(seq_along(dims), function(k) {
          isTRUE(all.equal(as.matrix(re$rand[[k]]), as.matrix(fitted[[k]]),
            check.attributes = FALSE, tolerance = 1e-8
          ))
        }, logical(1)))) {
        stop("Cannot align sdmTMB smooth random-effect blocks.", call. = FALSE)
      }
      null_columns <- seq_len(nf) + null_offset
      if (nf && !isTRUE(all.equal(re$Xf,
        object$smoothers$Xs[, null_columns, drop = FALSE],
        check.attributes = FALSE, tolerance = 1e-8
      ))) {
        stop("Cannot align sdmTMB smooth null-space columns.", call. = FALSE)
      }
      transforms[[j + 1L]] <- mixed_smooth_transform(re, "sdmTMB")
      indices <- c(indices, random[seq_len(nr) + random_offset],
        null[null_columns])
      random_offset <- random_offset + nr
      null_offset <- null_offset + nf
      block <- block + length(dims)
      sm$first.para <- offset + 1L
      offset <- offset + length(re$pen.ind)
      sm$last.para <- offset
      attr(sm, "gratia.component") <- component
      smooths[[j]] <- sm
    }
    if (random_offset != length(random) || null_offset != length(null) ||
      block != length(object$smoothers$Zs)) {
      stop("Cannot align sdmTMB smooth coefficient dimensions.", call. = FALSE)
    }
  }
  X <- as.matrix(object$tmb_data$X_ij[[i]])
  ff <- object$formula[[i]]
  tt <- object$terms[[i]]
  if (!is.null(envir)) {
    if (!is.environment(envir)) {
      stop("'envir' must be an environment.", call. = FALSE)
    }
    environment(ff) <- environment(tt) <- envir
  }
  frame <- object$data
  if (is.null(tt) || ncol(X) != length(fixed) ||
    nrow(X) != nrow(frame) || is.null(colnames(X))) {
    stop("Cannot align sdmTMB parametric design with fitted coefficients.",
      call. = FALSE)
  }
  # Retain prediction expressions (e.g. poly's fitted basis) from the original
  # parametric terms, alongside evaluated coordinates for mgcv smooths.
  evaluated_data <- frame
  for (nm in names(evaluated_data)) {
    if (is.factor(evaluated_data[[nm]])) {
      attr(evaluated_data[[nm]], "contrasts") <- NULL
    }
  }
  evaluated <- stats::model.frame(tt, evaluated_data,
    na.action = stats::na.pass,
    xlev = object$xlevels[[i]]
  )
  for (nm in names(evaluated)) frame[[nm]] <- evaluated[[nm]]
  A <- as.matrix(Matrix::bdiag(transforms))
  coefficients <- drop(A %*% object$last.par.best[indices])
  names(coefficients) <- c(colnames(X), unlist(lapply(
    smooths,
    function(sm) paste0(sm$label, ".", seq_len(sm$df))
  )))
  V <- if (covariance) A %*% sdmtmb_joint_covariance(object, indices) %*% t(A)
  cm <- colMeans(X)
  for (sm in smooths) {
    # model_context supplies raw data; expression recovery prepares only the
    # covariates needed for this particular smooth.
    context <- structure(list(
      formula = ff, terms = tt, model = frame,
      var.summary = frame
    ), class = "gratia_mixed_smooth")
    sm_data <- prepare_smooth_data(context, sm)
    for (nm in terms_in_smooth(sm)) frame[[nm]] <- sm_data[[nm]]
    cm <- c(cm, colMeans(mgcv::PredictMat(sm, sm_data)))
  }
  labels <- stats::setNames(paste("Delta component", components), components)
  structure(
    list(
      smooth = smooths, coefficients = coefficients, Vp = V,
      cmX = unname(cm), model = frame, terms = tt, formula = ff,
      coefficient_transform = A, joint_indices = indices,
      call = object$call, component = component, fitter = "sdmTMB",
      component_labels = labels, predictor = i,
      pterms = tt, parametric = seq_along(fixed),
      contrasts = object$contrasts[[i]], xlevels = object$xlevels[[i]],
      pred.formula = if (length(all.vars(stats::delete.response(tt)))) {
        stats::reformulate(all.vars(stats::delete.response(tt)),
          env = environment(ff))
      } else {
        stats::as.formula("~1", env = environment(ff))
      },
      var.summary = lapply(
        frame[intersect(names(frame), all.vars(ff))],
        function(x) {
          if (is.numeric(x)) c(min(x), stats::median(x), max(x)) else x
        }
      ),
      na.action = object$na_action
    ),
    class = c("gratia_sdmtmb_smooth", "gratia_mixed_smooth"),
    gratia.n_predictors = length(components)
  )
}

#' Formula effects for sdmTMB models
#'
#' Evaluate and plot formula smooths and parametric effects from a fitted
#' `sdmTMB` model using the same plotting methods as mgcv smooths. Penalized
#' and unpenalized coefficients and their joint covariance are transformed
#' back to the constrained mgcv basis.
#'
#' @param object,model A fitted `sdmTMB` model.
#' @param select Smooth selection across both predictors of delta models.
#'   Numeric and logical indices follow stored smooth order, first component
#'   then
#'   second. Character selections use [smooths()] labels, such as `s(x)`
#'   and `s.1(x)` for the second component.
#' @param id Integer positions of smooths across model components.
#' @param terms Parametric terms to evaluate or draw, as returned by
#'   [parametric_terms()]. Second-component selectors have a `.1` suffix.
#' @param parametric Include parametric effects? `NULL` includes them unless
#'   smooths are selected explicitly, as for [draw.gam()].
#' @param residuals Must be `FALSE`; partial residuals are not supported.
#' @param unconditional,frequentist Must be `FALSE`. Intervals use the joint
#'   TMB or RTMB covariance; mgcv-specific alternatives are unavailable.
#' @param envir Optional environment for evaluating model covariates.
#' @param ... Arguments passed to the corresponding gratia methods, including
#'   grid, interval, transformation, surface, styling and layout controls.
#'
#' @details
#' Estimates are partial contributions on each component's link scale. Spatial,
#' spatiotemporal, IID random effects, offsets, and varying coefficients are
#' not added to the effects. Their influence on coefficient uncertainty is
#' retained through the joint covariance. Smooth intervals include uncertainty
#' in their component's overall mean by default; parametric intervals include
#' the component's intercept. Use `overall_uncertainty = FALSE` for term-only
#' intervals. `fun` transforms displayed partial effects, not full predictions.
#'
#' Delta components have separate smooth and parametric coefficients and are
#' drawn separately, with component subtitles. Upstream sdmTMB restrictions on
#' supported bases and delta formulas still apply. Tensor smooths with multiple
#' penalties are supported. Fits require stored smooth metadata; mapping or
#' profiling required coefficients and non-positive-definite Hessians
#' give errors.
#'
#' Support is for sdmTMB >= 1.1.0, including development fits using RTMB.
#' Dispersion formulas, other multi-predictor families, threshold and nonlocal
#' effects, random-effect plots, diagnostics and posterior sampling are not
#' provided by these methods.
#'
#' @return Smooth labels, smooth objects, coefficients, effect estimates, or
#'   plots, as for the corresponding GAM methods. [draw()] returns a patchwork,
#'   or named panels with `wrap = FALSE`; [assemble()] returns an
#'   `assembled_plots` list. Models without smooths can draw parametric effects.
#' @name sdmTMB-methods
#' @examples
#' if (requireNamespace("sdmTMB", quietly = TRUE)) {
#'   library("mgcv")
#'   set.seed(42)
#'   d <- data.frame(x = runif(150), z = runif(150))
#'   d$y <- 1 + sin(6 * d$x) + 0.5 * d$z + rnorm(150, sd = 0.3)
#'   m <- sdmTMB::sdmTMB(y ~ z + s(x, k = 6), data = d, spatial = "off")
#'   draw(m)
#'   smooth_estimates(m, select = "s(x)")
#'   assemble(m)
#'
#'   d$y <- ifelse(runif(150) < 0.3, 0, exp(d$y))
#'   delta <- sdmTMB::sdmTMB(y ~ z + s(x, k = 6),
#'     data = d,
#'     spatial = "off", family = sdmTMB::delta_gamma()
#'   )
#'   draw(delta)
#'   draw(delta, select = "s.1(x)", parametric = TRUE, terms = "z.1")
#' }
NULL

#' @rdname sdmTMB-methods
#' @export
smooths.sdmTMB <- function(object, ...) {
  vapply(sdmtmb_smooths(object), `[[`, character(1), "label")
}

#' @rdname sdmTMB-methods
#' @export
get_smooths_by_id.sdmTMB <- function(object, id, ...) {
  sdmtmb_smooth_model(object, covariance = FALSE)$smooth[id]
}

#' @rdname sdmTMB-methods
#' @export
smooth_coefs.sdmTMB <- function(object, select, ...) {
  smooth_coefs.gam(sdmtmb_smooth_model(object, covariance = FALSE), select, ...)
}

#' @rdname sdmTMB-methods
#' @export
smooth_estimates.sdmTMB <- function(
  object, ...,
  unconditional = FALSE, frequentist = FALSE, envir = NULL
) {
  mixed_covariance_options(unconditional, frequentist, "sdmTMB")
  if (!length(sdmtmb_smooths(object))) {
    stop("No smooths in sdmTMB model.", call. = FALSE)
  }
  smooth_estimates(sdmtmb_smooth_model(object, envir = envir),
    ..., envir = envir)
}

#' @rdname sdmTMB-methods
#' @export
parametric_terms.sdmTMB <- function(model, ...) {
  parametric_terms(sdmtmb_smooth_model(model, covariance = FALSE))
}

#' @rdname sdmTMB-methods
#' @export
parametric_effects.sdmTMB <- function(
  object, terms = NULL, ...,
  unconditional = FALSE, frequentist = FALSE, envir = NULL
) {
  mixed_covariance_options(unconditional, frequentist, "sdmTMB")
  parametric_effects(sdmtmb_smooth_model(object, envir = envir),
    terms = terms, ..., envir = envir
  )
}

sdmtmb_plot_model <- function(object, residuals, unconditional, frequentist,
    envir) {
  mixed_covariance_options(unconditional, frequentist, "sdmTMB")
  if (!identical(residuals, FALSE)) {
    stop("sdmTMB partial residual overlays are not yet supported.",
      call. = FALSE)
  }
  sdmtmb_smooth_model(object, envir = envir)
}

#' @rdname sdmTMB-methods
#' @export
assemble.sdmTMB <- function(
  object, ..., parametric = NULL, residuals = FALSE,
  unconditional = FALSE, frequentist = FALSE, envir = NULL
) {
  model <- sdmtmb_plot_model(object, residuals, unconditional, frequentist,
    envir)
  assemble(model, ..., parametric = parametric, envir = envir)
}

#' @rdname sdmTMB-methods
#' @export
draw.sdmTMB <- function(
  object, ..., parametric = NULL, residuals = FALSE,
  unconditional = FALSE, frequentist = FALSE, envir = NULL
) {
  model <- sdmtmb_plot_model(object, residuals, unconditional, frequentist,
    envir)
  draw.gam(model, ..., parametric = parametric, envir = envir)
}
