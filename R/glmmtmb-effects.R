# Parametric components use the ordinary fixed-effect columns only. Smooth
# null-space coefficients belong to their smooth, not to a parametric term.
#' @export
parametric_terms.gratia_glmmtmb_smooth <- function(model, ...) {
  if (!is.null(model$models)) {
    return(do.call(c, unname(lapply(model$models, parametric_terms))))
  }
  labels <- attr(model$pterms, "term.labels")
  if (!length(labels)) return(stats::setNames(character(), character()))
  suffix <- if (is.null(model$predictor) || model$predictor == 1L) "" else
    paste0(".", model$predictor - 1L)
  stats::setNames(labels, paste0(labels, suffix))
}

#' @rdname glmmTMB-methods
#' @export
parametric_terms.glmmTMB <- function(model, ...) {
  parametric_terms(glmmtmb_smooth_model(model, covariance = FALSE))
}

#' @export
predict_parametric_effects.gratia_glmmtmb_smooth <- function(object, newdata,
    terms, unconditional, overall_uncertainty) {
  glmmtmb_covariance_options(unconditional, FALSE)
  tt <- stats::delete.response(object$pterms)
  frame <- stats::model.frame(tt, newdata, na.action = stats::na.pass,
    xlev = object$xlevels)
  X <- stats::model.matrix(tt, frame, contrasts.arg = object$contrasts)
  columns <- object$parametric
  if (!identical(colnames(X), names(coef(object))[columns])) {
    stop("Cannot align glmmTMB parametric design with fitted coefficients.",
      call. = FALSE)
  }
  assignment <- attr(X, "assign")
  labels <- names(parametric_terms(object))
  fit <- se <- matrix(NA_real_, nrow(X), length(terms),
    dimnames = list(NULL, terms))
  for (term in terms) {
    j <- match(term, labels)
    if (is.na(j)) stop("Unknown glmmTMB parametric term: ", term, call. = FALSE)
    take <- which(assignment == j)
    fit[, term] <- drop(X[, take, drop = FALSE] %*% coef(object)[columns[take]])
    if (isTRUE(overall_uncertainty)) take <- union(which(assignment == 0L), take)
    Xi <- X[, take, drop = FALSE]
    V <- object$Vp[columns[take], columns[take], drop = FALSE]
    se[, term] <- sqrt(pmax(0, rowSums((Xi %*% V) * Xi)))
  }
  list(fit = fit, se.fit = se)
}

#' @export
parametric_effects.gratia_glmmtmb_smooth <- function(object, terms = NULL, ...) {
  if (is.null(object$models)) return(parametric_effects.gam(object,
    terms = terms, ...))
  available <- names(parametric_terms(object))
  if (!length(available)) {
    warning("The model doesn't contain any parametric terms", call. = FALSE)
    return(NULL)
  }
  if (is.null(terms)) terms <- available
  if (!all(terms %in% available)) {
    stop("Term is not in the parametric part of model: ",
      paste(setdiff(terms, available), collapse = ", "), call. = FALSE)
  }
  results <- lapply(object$models, function(local) {
    want <- intersect(terms, names(parametric_terms(local)))
    if (!length(want)) return(NULL)
    # Parametric recovery must use its own predictor's formula/environment.
    local$formula <- stats::formula(local$pterms)
    local$terms <- local$pterms
    local <- with_model_envir(local, attr(object, "gratia.envir", exact = TRUE))
    parametric_effects.gam(local, terms = want, ...)
  })
  results <- Filter(Negate(is.null), results)
  out <- dplyr::bind_rows(results)
  for (attribute in c("factor_levels", "term_info", "axis_info")) {
    attr(out, attribute) <- do.call(c, lapply(results, attr, attribute))
  }
  attr(out, "back_transform") <- attr(results[[1L]], "back_transform")
  class(out) <- unique(c("parametric_effects", class(out)))
  out
}

#' @rdname glmmTMB-methods
#' @export
parametric_effects.glmmTMB <- function(object, terms = NULL, ..., unconditional = FALSE,
    frequentist = FALSE) {
  glmmtmb_covariance_options(unconditional, frequentist)
  parametric_effects(glmmtmb_smooth_model(object), terms = terms, ...)
}

# ranef() can combine several formula terms for a grouping factor. Its column
# order follows cnms; smooth_info identifies the blocks to exclude. Never infer
# which blocks are smooths from dummy grouping-factor or coefficient names.
glmmtmb_random_effects <- function(object) {
  components <- glmmtmb_components(object)
  out <- list()
  native <- NULL
  for (i in seq_along(components)) {
    component <- components[i]
    re <- object$modelInfo$reTrms[[component]]
    blocks <- object$modelInfo$reStruc[[paste0(component, "ReStruc")]]
    info <- re$smooth_info
    if (is.null(info)) info <- vector("list", length(blocks))
    positions <- which(lengths(info) == 0L)
    if (!length(positions)) next
    if (is.null(native)) native <- glmmTMB::ranef(object, condVar = TRUE)
    assignment <- attr(re$flist, "assign")
    for (j in positions) {
      group <- names(re$flist)[assignment[j]]
      previous <- seq_along(re$cnms) < j & assignment == assignment[j]
      start <- sum(lengths(re$cnms)[previous])
      columns <- seq_along(re$cnms[[j]]) + start
      values <- as.matrix(native[[component]][[group]][, columns, drop = FALSE])
      if (ncol(values) != blocks[[j]]$blockSize ||
          nrow(values) != blocks[[j]]$blockReps || any(!is.finite(values))) {
        stop("Cannot align glmmTMB native random effects with fitted blocks.",
          call. = FALSE)
      }
      names <- colnames(values)
      labels <- paste0("(", ifelse(names == "(Intercept)", "1", names),
        " | ", group, ")", if (i > 1L) paste0(".", i - 1L) else "")
      # Only a single, untransformed numeric slope paired with an intercept.
      # Factor slopes, interactions, structured covariance terms and multiple
      # slopes retain coefficient QQ plots even when ranef_lines is TRUE.
      slope <- setdiff(names, "(Intercept)")
      simple <- length(names) == 2L && "(Intercept)" %in% names &&
        length(slope) == 1L && slope %in% names(object$frame) &&
        slope %in% all.vars(stats::formula(object, component = component)) &&
        is.numeric(object$frame[[slope]]) &&
        !is.matrix(object$frame[[slope]]) &&
        blocks[[j]]$blockName %in% c("us", "diag", "homdiag")
      out[[length(out) + 1L]] <- list(values = values, labels = labels,
        component = component, group = group, simple = simple,
        slope = if (simple) slope else NULL,
        observed = if (simple) object$frame[[slope]] else NULL)
    }
  }
  # Preserve identity when separate fitted blocks have the same labels.
  if (!length(out)) return(out)
  labels <- make.unique(unlist(lapply(out, `[[`, "labels"), use.names = FALSE))
  start <- 0L
  for (i in seq_along(out)) {
    n <- length(out[[i]]$labels)
    out[[i]]$labels <- labels[seq_len(n) + start]
    start <- start + n
  }
  out
}

glmmtmb_model_label <- function(component) {
  switch(component, cond = "Conditional model", zi = "Zero-inflation model",
    disp = "Dispersion model")
}

glmmtmb_random_lines <- function(term, data = NULL, n = 100,
    constant = NULL, fun = NULL, smooth_col = "black", rug = TRUE,
    caption = TRUE, angle = NULL, ylim = NULL, ...) {
  x <- if (is.null(data)) seq_min_max(term$observed, n = n) else {
    if (!term$slope %in% names(data) || !is.numeric(data[[term$slope]])) {
      stop("Supply a numeric '", term$slope, "' column in 'data' for random-effect lines.",
        call. = FALSE)
    }
    sort(unique(data[[term$slope]][is.finite(data[[term$slope]])]))
  }
  if (!length(x)) stop("No finite covariate values for random-effect lines.")
  b <- term$values
  value <- outer(x, b[, term$slope]) +
    matrix(b[, "(Intercept)"], length(x), nrow(b), byrow = TRUE)
  d <- tibble::tibble(.x = rep(x, nrow(b)),
    .group = rep(rownames(b), each = length(x)), .estimate = as.vector(value))
  d <- add_constant(d, constant = constant, column = ".estimate")
  d <- transform_fun(d, fun = fun, column = ".estimate")
  title <- paste0("(1 + ", term$slope, " | ", term$group, ")")
  plt <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.x, y = .data$.estimate,
    group = .data$.group)) +
    ggplot2::geom_line(colour = smooth_col) +
    ggplot2::labs(x = term$slope, y = "Partial effect", title = title,
      subtitle = glmmtmb_model_label(term$component),
      caption = if (isTRUE(caption)) "Random intercept and slope contributions" else NULL) +
    ggplot2::guides(x = ggplot2::guide_axis(angle = angle))
  if (isTRUE(rug)) plt <- plt + ggplot2::geom_rug(
    data = data.frame(.x = term$observed), ggplot2::aes(x = .data$.x),
    inherit.aes = FALSE, sides = "b", alpha = 0.5)
  if (!is.null(ylim)) plt <- plt + ggplot2::expand_limits(y = ylim)
  plt
}

# Smooth and native RE selection share one index, with smooth indices first.
# Parametric selection continues to use `terms`, exactly as in draw.gam().
#' @export
assemble.gratia_glmmtmb_smooth <- function(object, select = NULL,
    parametric = NULL, partial_match = FALSE, ...) {
  dots <- list(...)
  native <- object$native_random_effects
  S <- smooths(object)
  labels <- c(S, unlist(lapply(native, `[[`, "labels"), use.names = FALSE))
  take <- check_user_select_smooths(labels, select, partial_match)
  if (is.null(parametric)) parametric <- is.null(select)
  smooth_take <- take[seq_along(S)]
  plots <- if (any(smooth_take) ||
      (isTRUE(parametric) && length(parametric_terms(object)))) {
    do.call(assemble.gam, c(list(object = object,
      select = which(smooth_take), parametric = parametric), dots))
  } else list()
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
  options <- options[intersect(names(options), c("data", "n", "constant", "fun",
    "smooth_col", "rug", "caption", "angle"))]
  for (term in native) {
    chosen <- native_take[seq_along(term$labels) + start]
    start <- start + length(term$labels)
    if (isTRUE(object$ranef_lines) && term$simple && all(chosen)) {
      plot <- do.call(glmmtmb_random_lines, c(list(term = term), options))
      plots[[paste(term$labels, collapse = " + ")]] <- plot
    } else {
      for (j in which(chosen)) {
        d <- tibble::tibble(.smooth = term$labels[j], .type = "Random effect",
          .by = NA_character_,
          .estimate = unname(term$values[, j]))
        class(d) <- c("smooth_estimates", class(d))
        plots[[term$labels[j]]] <- do.call(plot_smooth.random_effect,
          c(list(object = d, variables = term$group,
            title = term$labels[j], subtitle = glmmtmb_model_label(term$component)),
            options[intersect(names(options), c("constant", "fun", "angle"))],
            list(caption = if (isTRUE(options$caption)) "Random effect" else NULL)))
      }
    }
  }
  plots <- append(plots, parametric_plots)
  if (!length(plots)) return(invisible(NULL))
  class(plots) <- c("assembled_plots", "list")
  plots
}
