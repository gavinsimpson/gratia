# Evaluate one multivariate formula term without adding any other components.
evaluate_parametric_component <- function(model, term, covariates, columns,
    data, supplied_data, n, n_2d, n_3d, n_4d, dist, unconditional) {
  numeric <- vapply(data[covariates], is.numeric, logical(1))
  variables <- c(covariates[numeric], covariates[!numeric])
  numeric_vars <- covariates[numeric]
  discrete_vars <- covariates[!numeric]
  if (!all(vapply(data[covariates], function(x) {
    is.numeric(x) || is.factor(x) || is.logical(x) || is.character(x)
  }, logical(1)))) {
    stop("Unsupported covariate type in parametric term: ", term, call. = FALSE)
  }
  if (any(vapply(data[covariates], is.matrix, logical(1)))) {
    stop("Matrix covariates cannot be plotted as raw axes: ", term, call. = FALSE)
  }
  resolutions <- list(n = n, n_2d = n_2d, n_3d = n_3d, n_4d = n_4d)
  resolutions <- lapply(resolutions, function(x) if (is.null(x)) n else x)
  if (!supplied_data && any(!vapply(resolutions, function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 2 && x == floor(x)
  }, logical(1)))) {
    stop("Grid resolutions must be integers of at least 2.", call. = FALSE)
  }
  if (!is.numeric(dist) || length(dist) != 1L || !is.finite(dist) || dist < 0) {
    stop("'dist' must be a non-negative finite number.", call. = FALSE)
  }
  # Observation data are kept separate from the prediction grid. If a model's
  # fitting data cannot be recovered, supplied prediction rows are not a rug.
  observed <- if (supplied_data) {
    tryCatch(recover_raw_data(model), error = function(e) NULL)
  } else data
  if (!is.null(observed)) observed <- observed[, covariates, drop = FALSE]

  if (supplied_data) {
    grid <- data
  } else {
    counts <- rep(resolutions$n, length(variables))
    if (length(numeric_vars) >= 2L) counts[1:2] <- resolutions$n_2d
    if (length(variables) == 3L) counts[3] <- resolutions$n_3d
    if (length(variables) > 3L) counts[seq.int(3L, length(variables))] <-
      resolutions$n_4d
    values <- lapply(seq_along(variables), function(i) {
      x <- data[[variables[i]]]
      if (is.logical(x) || is.character(x)) return(unique(x[!is.na(x)]))
      seq_min_max(x, n = counts[i])
    })
    grid <- tidyr::expand_grid(!!!setNames(values, variables))
    other <- setdiff(names(data), variables)
    if (length(other)) {
      typical <- typical_values(data[, other, drop = FALSE])
      grid <- dplyr::bind_cols(grid, typical[rep(1L, nrow(grid)), , drop = FALSE])
    }
  }
  # Request every parametric term: mgcv can zero later linear predictors
  # when only a suffixed term is requested. Select the desired column below.
  pred <- predict_model(model, newdata = grid, type = "terms",
    terms = names(parametric_terms(model)),
    se.fit = TRUE, unconditional = unconditional)
  out <- tibble::as_tibble(grid[, covariates, drop = FALSE])
  # Use the fitted levels, even when newdata has only a subset of levels.
  for (v in discrete_vars) {
    template <- if (!is.null(observed)) observed[[v]] else data[[v]]
    lev <- model$xlevels[[v]]
    if (is.null(lev)) lev <- if (is.factor(template)) levels(template) else
      sort(unique(template[!is.na(template)]))
    out[[v]] <- factor(out[[v]], levels = lev, ordered = is.ordered(template))
  }
  names(out) <- unname(columns[covariates])
  out$.partial <- as.numeric(pred$fit[, term])
  out$.se <- as.numeric(pred$se.fit[, term])
  out <- dplyr::distinct(out)
  if (!is.null(observed)) names(observed) <- unname(columns[covariates])
  result <- tibble::tibble(.term = term, .type = "interaction", data = list(out))
  attr(result, "term_info") <- list(
    variables = variables, columns = columns[variables],
    numeric = numeric_vars, discrete = discrete_vars,
    levels = lapply(out[unname(columns[discrete_vars])], levels),
    observed = observed, dist = dist)
  result
}

# One extra dimension wraps; two or more use rows and columns, as for smooths.
parametric_component_facets <- function(variables, columns, numeric_vars) {
  if (!length(variables)) return(NULL)
  facets <- setNames(lapply(unname(columns[variables]), rlang::sym), variables)
  label <- function(labels) {
    for (v in intersect(names(labels), numeric_vars)) {
      labels[[v]] <- format(signif(as.numeric(as.character(labels[[v]])), 3),
        trim = TRUE)
    }
    ggplot2::label_both(labels)
  }
  if (length(facets) == 1L) {
    ggplot2::facet_wrap(ggplot2::vars(!!!facets), labeller = label)
  } else {
    ggplot2::facet_grid(rows = ggplot2::vars(!!!facets[1L]),
      cols = ggplot2::vars(!!!facets[-1L]), labeller = label,
      as.table = FALSE)
  }
}

# Plot a coefficient block, not an adjusted prediction. All scale transforms
# are applied to the component and its intervals together.
draw_parametric_component <- function(object, info, ci_level, ci_col, ci_alpha,
    line_col, constant, fun, xlab, ylab, title, subtitle, caption, rug, ylim,
    angle, contour, contour_col, n_contour, geom, continuous_fill,
    discrete_colour, discrete_fill) {
  if (!all(c(".lower_ci", ".upper_ci") %in% names(object))) {
    crit <- coverage_normal(ci_level)
    object$.lower_ci <- object$.partial - crit * object$.se
    object$.upper_ci <- object$.partial + crit * object$.se
  }
  object <- add_constant(object, constant = constant,
    column = c(".partial", ".lower_ci", ".upper_ci"))
  object <- transform_fun(object, fun = fun,
    column = c(".partial", ".lower_ci", ".upper_ci"))
  cols <- info$columns
  numeric <- info$numeric
  discrete <- info$discrete
  for (v in discrete) {
    column <- cols[[v]]
    object[[column]] <- factor(object[[column]], levels = info$levels[[column]],
      ordered = is.ordered(object[[column]]))
  }
  if (is.null(title)) title <- unique(object$.term)
  if (is.null(caption)) caption <- "Parametric term contribution"
  surface <- length(numeric) >= 2L
  x <- info$variables[1L]
  if (is.null(xlab)) xlab <- x
  if (surface) {
    y <- numeric[2L]
    if (is.null(ylab)) ylab <- y
    extra <- info$variables[-c(1L, 2L)]
    observed <- info$observed
    if (info$dist > 0 && !is.null(observed) &&
        diff(range(observed[[cols[[x]]]], na.rm = TRUE)) > 0 &&
        diff(range(observed[[cols[[y]]]], na.rm = TRUE)) > 0) {
      excluded <- mgcv::exclude.too.far(object[[cols[[x]]]],
        object[[cols[[y]]]], observed[[cols[[x]]]], observed[[cols[[y]]]],
        dist = info$dist)
      object$.partial[excluded] <- NA_real_
    }
    # Treatment-coded interactions and zero-valued numeric slices can be
    # identically zero. Keep their surface, but do not ask stat_contour() to
    # contour a constant (or fully masked) panel.
    contour_data <- object |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(unname(cols[extra])))) |>
      dplyr::filter(dplyr::n_distinct(.data$.partial[is.finite(.data$.partial)]) > 1L) |>
      dplyr::ungroup()
    contour <- isTRUE(contour) && nrow(contour_data) > 0L
    return(prepare_surface_plot(object, x_var = cols[[x]], y_var = cols[[y]],
      fill_var = ".partial", fill_title = "Partial effect",
      labels = ggplot2::labs(x = xlab, y = ylab, title = title,
        subtitle = subtitle, caption = caption),
      geom = geom, continuous_fill = continuous_fill, contour = contour,
      contour_data = contour_data,
      contour_col = contour_col, n_contour = n_contour, angle = angle,
      facet = parametric_component_facets(extra, cols, numeric),
      rug = if (isTRUE(rug) && !length(extra)) observed else NULL))
  }
  group <- if (length(numeric)) discrete[1L] else discrete[2L]
  extra <- if (length(numeric)) discrete[-1L] else discrete[-c(1L, 2L)]
  group_col <- cols[[group]]
  plt <- ggplot2::ggplot(object, ggplot2::aes(
    x = .data[[cols[[x]]]], y = .data$.partial,
    colour = .data[[group_col]], group = .data[[group_col]]))
  if (length(numeric)) {
    plt <- plt + ggplot2::geom_ribbon(ggplot2::aes(
      ymin = .data$.lower_ci, ymax = .data$.upper_ci,
      fill = .data[[group_col]]), alpha = ci_alpha, colour = NA) +
      ggplot2::geom_line()
    if (isTRUE(rug) && !is.null(info$observed)) {
      plt <- plt + ggplot2::geom_rug(data = info$observed,
        mapping = ggplot2::aes(x = .data[[cols[[x]]]]),
        inherit.aes = FALSE, sides = "b", alpha = 0.5)
    }
  } else {
    plt <- plt + ggplot2::geom_pointrange(ggplot2::aes(
      ymin = .data$.lower_ci, ymax = .data$.upper_ci),
      position = ggplot2::position_dodge(width = 0.5))
  }
  if (is.null(ylab)) ylab <- "Partial effect"
  plt <- plt + parametric_component_facets(extra, cols, numeric) +
    ggplot2::labs(x = xlab, y = ylab, colour = group,
      fill = if (length(numeric)) group else NULL,
      title = title, subtitle = subtitle, caption = caption) +
    ggplot2::guides(x = ggplot2::guide_axis(angle = angle)) +
    discrete_colour + discrete_fill
  if (!is.null(ylim)) plt <- plt + ggplot2::expand_limits(y = ylim)
  plt
}
