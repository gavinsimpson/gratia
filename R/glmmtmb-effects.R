#' @rdname glmmTMB-methods
#' @export
parametric_terms.glmmTMB <- function(model, ...) {
  parametric_terms(glmmtmb_smooth_model(model, covariance = FALSE))
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
    caption = TRUE, angle = NULL, ylim = NULL,
    subtitle = glmmtmb_model_label(term$component), ...) {
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
  plt <- gratia_plot(d, ggplot2::aes(x = .data$.x, y = .data$.estimate,
    group = .data$.group)) +
    geom_gratia_smooth_line(colour = smooth_col) +
    ggplot2::labs(x = term$slope, y = "Partial effect", title = title,
      subtitle = subtitle,
      caption = if (isTRUE(caption)) "Random intercept and slope contributions" else NULL) +
    ggplot2::guides(x = ggplot2::guide_axis(angle = angle))
  if (isTRUE(rug)) plt <- plt + geom_gratia_rug(
    data = data.frame(.x = term$observed), ggplot2::aes(x = .data$.x),
    inherit.aes = FALSE, sides = "b", alpha = 0.5)
  if (!is.null(ylim)) plt <- plt + ggplot2::expand_limits(y = ylim)
  plt
}
