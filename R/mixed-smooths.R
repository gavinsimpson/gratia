# Shared effect evaluation and plotting for mixed-model smooth adapters.
# Fitters supply constrained mgcv bases, coefficient transforms and covariance;
# no fitted object is reclassified as a GAM.

mixed_all_smooths <- function(
  models, sms, parameters, covariance,
  n_predictors
) {
  components <- names(models)
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
  out$coefficients <- drop(A %*% parameters[indices])
  out$Vp <- if (is.function(covariance)) {
    A %*% covariance(indices) %*% t(A)
  }
  out$cmX <- unlist(lapply(models, `[[`, "cmX"), use.names = FALSE)
  out$smooth <- smooths
  out$coefficient_transform <- A
  out$joint_indices <- indices
  out$component <- NULL
  out$models <- lapply(seq_along(models), function(i) {
    local <- models[[i]]
    columns <- seq_len(sizes[i]) + starts[i]
    local$Vp <- if (!is.null(out$Vp)) {
      out$Vp[columns, columns, drop = FALSE]
    }
    local$predictor <- i
    local
  })
  names(out$models) <- components
  out$contexts <- lapply(models, function(x) {
    list(formula = x$formula, terms = x$terms)
  })
  attr(out$formula, "lpi") <- lapply(seq_along(models), function(i) {
    seq_len(sizes[i]) + starts[i]
  })
  # Coefficient names also identify the predictor, just like the smooth labels.
  names(out$coefficients) <- unlist(lapply(seq_along(models), function(i) {
    nm <- names(models[[i]]$coefficients)
    for (sm in smooths[vapply(smooths, function(x) {
      attr(x, "gratia.component") == components[i]
    }, logical(1))]) {
      nm[smooth_coef_indices(sm) - starts[i]] <-
        paste0(sm$label, ".", seq_len(sm$df))
    }
    nm
  }), use.names = FALSE)
  attr(out, "gratia.n_predictors") <- n_predictors
  out
}

mixed_joint_covariance <- function(sdr, p, index, model) {
  if (!isTRUE(sdr$pdHess)) {
    stop(model, " covariance is unavailable: Hessian is not positive definite.",
      call. = FALSE)
  }
  Q <- sdr$jointPrecision
  # With no random parameters TMB supplies cov.fixed, not jointPrecision.
  if (is.null(Q) && identical(names(sdr$par.fixed), names(p)) &&
    identical(dim(sdr$cov.fixed), c(length(p), length(p)))) {
    return(sdr$cov.fixed[index, index, drop = FALSE])
  }
  if (is.null(Q) || nrow(Q) != length(p) ||
    (!is.null(rownames(Q)) && !identical(rownames(Q), names(p)))) {
    stop("Cannot align ", model, " joint precision with fitted parameters ",
      "(mapped parameters are not supported).", call. = FALSE)
  }
  E <- Matrix::sparseMatrix(
    i = index, j = seq_along(index), x = 1,
    dims = c(nrow(Q), length(index))
  )
  V <- as.matrix(Matrix::solve(Q, E)[index, , drop = FALSE])
  if (any(!is.finite(V))) {
    stop(model, " covariance contains non-finite values.", call. = FALSE)
  }
  (V + t(V)) / 2
}

mixed_smooth_transform <- function(re, model) {
  nr <- sum(vapply(re$rand, ncol, integer(1)))
  nf <- ncol(re$Xf)
  nb <- length(re$pen.ind)
  penalized <- which(re$pen.ind != 0L)
  unpenalized <- which(re$pen.ind == 0L)
  if (length(penalized) != nr || length(unpenalized) != nf ||
    !setequal(re$rind, seq_len(nr))) {
    stop("Unsupported smooth2random permutation in ", model, " fit.",
      call. = FALSE)
  }
  B <- matrix(0, nb, nb)
  B[cbind(penalized, re$rind)] <- 1
  if (nf) B[cbind(unpenalized, nr + seq_len(nf))] <- 1
  B <- re$trans.D * B
  if (!is.null(re$trans.U)) B <- re$trans.U %*% B
  B
}

mixed_covariance_options <- function(unconditional, frequentist, model) {
  if (!identical(unconditional, FALSE) || !identical(frequentist, FALSE)) {
    stop(model, " supports joint covariance only; ",
      "unconditional and frequentist must be FALSE.",
      call. = FALSE
    )
  }
}

mixed_model_label <- function(object, component) {
  labels <- object$component_labels
  if (!is.null(labels)) {
    return(unname(labels[component]))
  }
  glmmtmb_model_label(component)
}

#' @export
coef.gratia_mixed_smooth <- function(object, ...) object$coefficients
#' @export
model.frame.gratia_mixed_smooth <- function(formula, ...) formula$model
#' @export
formula.gratia_mixed_smooth <- function(x, ...) x$formula
#' @export
terms.gratia_mixed_smooth <- function(x, ...) x$terms
#' @export
get_smooths_by_id.gratia_mixed_smooth <- function(object, id, ...) {
  object$smooth[id]
}
#' @export
model_context.gratia_mixed_smooth <- function(object, component = NULL) {
  model_context.default(object, component = NULL)
}
#' @export
smooth_estimates.gratia_mixed_smooth <- function(
  object, select = NULL,
  partial_match = FALSE, ...
) {
  if (is.null(object$contexts)) {
    return(smooth_estimates.gam(object,
      select = select, partial_match = partial_match, ...
    ))
  }
  labels <- smooths(object)
  take <- check_user_select_smooths(labels,
    select = select,
    partial_match = partial_match
  )
  selected <- object$smooth[which(take)]
  components <- unique(vapply(selected, attr, character(1), "gratia.component"))
  results <- lapply(components, function(component) {
    local <- object
    ctx <- object$contexts[[component]]
    local$formula <- ctx$formula
    attr(local$formula, "lpi") <- attr(object$formula, "lpi")
    local$terms <- ctx$terms
    want <- vapply(selected, function(sm) {
      attr(sm, "gratia.component") == component
    }, logical(1))
    smooth_estimates.gam(local,
      select = vapply(selected[want], `[[`, character(1), "label"), ...
    )
  })
  out <- dplyr::bind_rows(results)
  metadata <- c("tensor_term_order", "surface_support", "tensor_plot_info")
  for (attribute in metadata) {
    attr(out, attribute) <- do.call(c, lapply(results, attr, attribute))
  }
  coordinates <- lapply(results, attr, "coordinate_info")
  info <- do.call(c, coordinates)
  info <- info[!duplicated(names(info))]
  attr(info, "smooth_variables") <- do.call(
    c,
    lapply(coordinates, attr, "smooth_variables")
  )
  attr(out, "coordinate_info") <- info
  out
}

# Parametric components use the ordinary fixed-effect columns only. Smooth
# null-space coefficients belong to their smooth, not to a parametric term.
#' @export
parametric_terms.gratia_mixed_smooth <- function(model, ...) {
  if (!is.null(model$models)) {
    return(do.call(c, unname(lapply(model$models, parametric_terms))))
  }
  labels <- attr(model$pterms, "term.labels")
  if (!length(labels)) {
    return(stats::setNames(character(), character()))
  }
  suffix <- if (is.null(model$predictor) || model$predictor == 1L) {
    ""
  } else {
    paste0(".", model$predictor - 1L)
  }
  stats::setNames(labels, paste0(labels, suffix))
}


#' @export
predict_parametric_effects.gratia_mixed_smooth <- function(
  object, newdata,
  terms, unconditional, overall_uncertainty
) {
  mixed_covariance_options(unconditional, FALSE, object$fitter)
  tt <- stats::delete.response(object$pterms)
  # model.frame(xlev = ...) otherwise warns while dropping contrasts carried
  # by supplied factors. model.matrix below restores the fitted contrasts.
  for (nm in names(newdata)) {
    if (is.factor(newdata[[nm]])) attr(newdata[[nm]], "contrasts") <- NULL
  }
  frame <- stats::model.frame(tt, newdata,
    na.action = stats::na.pass,
    xlev = object$xlevels
  )
  X <- stats::model.matrix(tt, frame, contrasts.arg = object$contrasts)
  columns <- object$parametric
  if (!identical(colnames(X), names(coef(object))[columns])) {
    stop("Cannot align ", object$fitter,
      " parametric design with fitted coefficients.",
      call. = FALSE
    )
  }
  assignment <- attr(X, "assign")
  labels <- names(parametric_terms(object))
  fit <- se <- matrix(NA_real_, nrow(X), length(terms),
    dimnames = list(NULL, terms)
  )
  for (term in terms) {
    j <- match(term, labels)
    if (is.na(j)) stop("Unknown parametric term: ", term, call. = FALSE)
    take <- which(assignment == j)
    fit[, term] <- drop(X[, take, drop = FALSE] %*% coef(object)[columns[take]])
    if (isTRUE(overall_uncertainty)) {
      take <- union(which(assignment == 0L), take)
    }
    Xi <- X[, take, drop = FALSE]
    V <- object$Vp[columns[take], columns[take], drop = FALSE]
    se[, term] <- sqrt(pmax(0, rowSums((Xi %*% V) * Xi)))
  }
  list(fit = fit, se.fit = se)
}

#' @export
parametric_effects.gratia_mixed_smooth <- function(object, terms = NULL, ...) {
  if (is.null(object$models)) {
    return(parametric_effects.gam(object,
      terms = terms, ...
    ))
  }
  available <- names(parametric_terms(object))
  if (!length(available)) {
    warning("The model doesn't contain any parametric terms", call. = FALSE)
    return(NULL)
  }
  if (is.null(terms)) terms <- available
  if (!all(terms %in% available)) {
    stop("Term is not in the parametric part of model: ",
      paste(setdiff(terms, available), collapse = ", "),
      call. = FALSE
    )
  }
  results <- lapply(object$models, function(local) {
    want <- intersect(terms, names(parametric_terms(local)))
    if (!length(want)) {
      return(NULL)
    }
    # Parametric recovery must use its own predictor's formula/environment.
    local$formula <- stats::formula(local$pterms)
    local$terms <- local$pterms
    local <- with_model_envir(local, attr(object, "gratia.envir", exact = TRUE))
    parametric_effects.gam(local, terms = want, ...)
  })
  results <- Filter(Negate(is.null), results)
  out <- dplyr::bind_rows(results)
  for (attribute in c("factor_levels", "term_info", "axis_info")) {
    # Predictor names must not prefix the term labels used by the renderer.
    attr(out, attribute) <- do.call(c, unname(lapply(results, attr, attribute)))
  }
  attr(out, "back_transform") <- attr(results[[1L]], "back_transform")
  class(out) <- unique(c("parametric_effects", class(out)))
  out
}


# Smooth and native RE selection share one index, with smooth indices first.
# Parametric selection continues to use `terms`, exactly as in draw.gam().
#' @export
assemble.gratia_mixed_smooth <- function(
  object, select = NULL,
  parametric = NULL, partial_match = FALSE, ...
) {
  dots <- list(...)
  native <- object$native_random_effects
  S <- smooths(object)
  labels <- c(S, unlist(lapply(native, `[[`, "labels"), use.names = FALSE))
  take <- check_user_select_smooths(labels, select, partial_match)
  if (is.null(parametric)) parametric <- is.null(select)
  smooth_take <- take[seq_along(S)]
  plots <- if (any(smooth_take) ||
    (isTRUE(parametric) && length(parametric_terms(object)))) {
    do.call(assemble.gam, c(list(
      object = object,
      select = which(smooth_take), parametric = parametric
    ), dots))
  } else {
    list()
  }
  # Keep native effects between smooth and parametric panels.
  para <- intersect(names(plots), names(parametric_terms(object)))
  parametric_plots <- plots[para]
  plots <- plots[setdiff(names(plots), para)]
  native_take <- take[-seq_along(S)]
  if (!length(S)) native_take <- take
  start <- 0L
  defaults <- lapply(formals(assemble.gam), function(x) x)
  options <- c(dots, defaults[setdiff(names(defaults), names(dots))])
  # These defaults are constants (and avoid passing missing formal values).
  options <- options[intersect(names(options), c(
    "data", "n", "constant", "fun",
    "smooth_col", "rug", "caption", "angle"
  ))]
  for (term in native) {
    subtitle <- dots[["subtitle"]]
    if (is.null(subtitle) && has_multiple_predictors(object)) {
      subtitle <- mixed_model_label(object, term$component)
    }
    chosen <- native_take[seq_along(term$labels) + start]
    start <- start + length(term$labels)
    if (isTRUE(object$ranef_lines) && term$simple && all(chosen)) {
      plot <- do.call(
        glmmtmb_random_lines,
        c(list(term = term, subtitle = subtitle), options)
      )
      plots[[paste(term$labels, collapse = " + ")]] <- plot
    } else {
      for (j in which(chosen)) {
        d <- tibble::tibble(
          .smooth = term$labels[j], .type = "Random effect",
          .by = NA_character_,
          .estimate = unname(term$values[, j])
        )
        class(d) <- c("smooth_estimates", class(d))
        plots[[term$labels[j]]] <- do.call(
          plot_smooth.random_effect,
          c(
            list(
              object = d, variables = term$group,
              title = term$labels[j], subtitle = subtitle
            ),
            options[intersect(names(options), c("constant", "fun", "angle"))],
            list(caption = if (isTRUE(options$caption)) "Random effect")
          )
        )
      }
    }
  }
  plots <- append(plots, parametric_plots)
  if (!length(plots)) {
    return(invisible(NULL))
  }
  class(plots) <- c("assembled_plots", "list")
  plots
}
