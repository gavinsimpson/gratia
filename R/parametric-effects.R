#' Estimated values for parametric model terms
#'
#' @param object a fitted model object.
#' @param terms character; which model parametric terms should be drawn? The
#'   Default of `NULL` will plot all parametric terms that can be drawn.
#' @param data an optional data frame containing raw model covariates at which
#'   to evaluate parametric effects. By default, stored raw columns are used;
#'   missing raw inputs are recovered from the fitting data when possible.
#' @param unconditional logical; should confidence intervals include the
#'   uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian
#'   covariance matrix will be used.
#' @param overall_uncertainty logical; include uncertainty in the intercept of
#'   the term's linear predictor, including its covariance with the term?
#'   Defaults to `TRUE`. Partial effect estimates are unchanged. With no
#'   intercept, this has no effect. Set to `FALSE` for term-only uncertainty.
#' @param unnest logical; unnest the parametric effect objects?
#' @param ci_level numeric; the coverage required for the confidence interval.
#'   Currently ignored.
#' @param envir an optional environment for local functions and constants, and
#'   for recovering fitting data when required raw columns are not stored in
#'   the model. Defaults to the model's evaluation environment.
#' @inheritParams draw.gam
#' @param transform `r lifecycle::badge("deprecated")` Use
#'   `back_transform = !transform` instead.
#' @param n,n_2d,n_3d,n_4d Grid resolutions for multivariate parametric terms.
#'   Curves use `n = 100`; the first two numeric surface axes use `n_2d = 50`.
#'   The third dimension of a three-variable term uses `n_3d = 16`; dimensions
#'   beyond the second of higher-dimensional terms use `n_4d = 4`.
#'   `NULL` uses `n` instead. Factors retain their levels. Ignored when `data`
#'   is supplied. Single-variable terms retain their observed evaluation values.
#' @param dist Non-negative distance for masking surface plots far from observed
#'   covariates, as in [draw.gam()]. Applied to the first two numeric axes when
#'   drawing; returned estimates are not masked. Use zero to disable masking.
#' @param ... arguments passed to other methods.
#'
#' @details
#' Each estimate is the contribution of one formula term to its linear
#' predictor. Intercepts, main effects and other terms are not added to an
#' interaction. Components follow the model's contrast coding: with treatment
#' contrasts, a numeric-by-factor interaction represents a departure from the
#' reference-level slope, rather than a complete slope for each level.
#'
#' For sdmTMB time-varying coefficients, `.type = "time_varying"` identifies
#' coefficient trajectories. In these rows, `.value` contains time, `.partial`
#' contains the full coefficient, and `.se` includes the joint uncertainty of
#' its ordinary coefficient and temporal deviation. Default selection omits
#' ordinary terms already included in these trajectories; their usual selectors
#' remain available for explicit requests. See [sdmTMB-methods] for details.
#'
#' A multi-column term such as `poly(x, 3)` is one component, whereas `x` and
#' `I(x^2)` remain separate components. Scalar numeric transformations use model
#' coordinates by default, including each axis of an interaction. Basis
#' expansions such as `poly(x, 3)`, terms combining several inputs in one
#' expression, and interactions containing several transformations of the same
#' input retain raw axes because they have no single model coordinate per input.
#' Other covariates in generated prediction data are held at typical values
#' solely to evaluate the model matrix; their contributions are not included.
#' Rugs show fitting observations, including when prediction `data` are
#' supplied. If fitting observations cannot be recovered, these rugs are omitted.
#'
#' The drawing method uses the first two numeric covariates for surface axes,
#' grouping curves by the first factor when there is only one numeric covariate.
#' Factor-only terms use grouped points and intervals. Remaining covariates
#' define facets: one wraps, two or more use the first as rows and the rest as
#' columns. Formula order is retained within numeric and discrete covariates.
#' Logical covariates are discrete. Surface plots show estimates; uncertainty
#' is retained in the returned data rather than drawn as additional surfaces.
#'
#' @return A tibble of class `parametric_effects`, with `.term`, `.type`,
#'   `.partial` and `.se`. Single-variable terms retain `.value` or `.level`;
#'   multivariate terms use columns named for their raw covariates, containing
#'   coordinates on the selected scale. Names conflicting
#'   with reserved columns are repaired with numeric suffixes. The `term_info`
#'   attribute records multivariate column mappings, plotting order, factor
#'   levels and observation data. With `unnest = FALSE`, estimates and
#'   covariates are nested in a `data` list column. The `axis_info` attribute
#'   identifies retained raw and model coordinate columns and their labels
#'   so the drawing method
#'   can select either scale. The `back_transform` attribute records the scale
#'   of the returned coordinate columns. Auxiliary `.gratia_raw_*` and
#'   `.gratia_model_*` columns retain both scales for transformed axes.
#'
#' @examples
#' load_mgcv()
#'
#' # Simulate numeric and factor effects
#' set.seed(42)
#' d <- data.frame(
#'   x = runif(100),
#'   group = factor(rep(c("A", "B"), each = 50))
#' )
#' d$y <- with(d, 2 * x + (group == "B") + rnorm(100, sd = 0.25))
#' m <- gam(y ~ x + group, data = d, method = "REML")
#'
#' # Estimate and draw the contribution of each parametric term
#' pe <- parametric_effects(m)
#' pe
#' draw(pe)
#'
#' @export
`parametric_effects` <- function(object, ...) {
  UseMethod("parametric_effects")
}

#' @importFrom stats delete.response formula model.frame
#' @importFrom tibble as_tibble add_column
#' @importFrom rlang .data
#' @importFrom purrr map_df map
#' @importFrom dplyr mutate bind_cols bind_rows distinct relocate rename
#' @importFrom tidyr nest unnest
#' @importFrom tidyselect any_of last_col
#' @importFrom cli cli_alert_info
#'
#' @rdname parametric_effects
#' @export
`parametric_effects.gam` <- function(object, terms = NULL,
                                     data = NULL,
                                     unconditional = FALSE,
                                     unnest = TRUE,
                                     ci_level = 0.95,
                                     envir = NULL,
                                     transform = lifecycle::deprecated(),
                                     n = 100, n_2d = 50,
                                     n_3d = 16, n_4d = 4,
                                     dist = 0.1, overall_uncertainty = TRUE,
                                     back_transform = FALSE, ...) {
  back_transform <- parametric_back_transform(back_transform, transform,
    supplied = !missing(back_transform))
  # SCAM retains prediction variables in its terms, without pred.formula.
  # Data recovery and prediction must use the same variables.
  if (is.null(object$pred.formula)) {
    object$pred.formula <- stats::delete.response(stats::terms(object))
  }
  object <- with_model_envir(object, envir)
  envir <- model_envir(object)
  supplied_data <- !is.null(data)
  vars <- parametric_terms(object) # vector of names of model terms
  if (length(vars) == 0L) {
    warning("The model doesn't contain any parametric terms", call. = FALSE)
    return(NULL)
  }
  mgcv_names <- names(vars) # this is how mgcv refers to the terms

  # user supplied term? if provided check the stated terms are actually model
  # parametric terms
  valid_terms <- if (is.null(terms)) {
    mgcv_names
  } else {
    if (!any(valid_terms <- terms %in% mgcv_names)) {
      stop(sprintf(
        "Term is not in the parametric part of model: <%s>",
        terms
      ))
    }
    terms[valid_terms]
  }

  # Prefer stored raw columns. Recover the fitting data only when a required
  # input is absent, using the same checks as data_slice().
  data <- recover_raw_data(object, data = data, envir = envir)
  observed <- if (supplied_data) tryCatch(recover_raw_data(object),
    gratia_data_recovery_error = function(e) NULL) else data

  # have to do predictions *after* we reconstruct the data otherwise we get
  # problems if there were NAs in the original data.

  # Evaluate single-variable components at observed (or supplied) values.
  # Multivariate components below generate their own grids when needed.
  data <- model.frame(object$pred.formula, data = data)
  attr(data, "terms") <- NULL # squich this or predict.gam complains
  # can limit the data combinations we predict at now by taking the unique
  # data combos
  data <- distinct(data)
  # Work around a bug in predict.gam() with exclude length 0 character
  # (i.e smooths(objects) when model contains only parametric terms)
  # Predict all parametric terms before selecting: requesting only a suffixed
  # term can make mgcv zero later linear predictors in multi-predictor models.
  pred <- predict_parametric_effects(object,
    newdata = data,
    terms = mgcv_names, overall_uncertainty = overall_uncertainty,
    unconditional = unconditional
  )

  # Keep plotting metadata outside the tabular estimates. Reserved names are
  # escaped consistently across terms, including covariates such as `.partial`.
  raw_vars <- lapply(vars[valid_terms], function(x) {
    intersect(all.vars(str2lang(x)), names(data))
  })
  all_raw <- unique(unlist(raw_vars, use.names = FALSE))
  reserved <- c(".term", ".type", ".level", ".value", ".partial", ".se",
    ".lower_ci", ".upper_ci", "data")
  columns <- setNames(utils::tail(make.unique(c(reserved, all_raw)), length(all_raw)),
    all_raw)

  # loop over the valid_terms and prepare the parametric effects
  fun <- function(term, data, pred, vars) {
    # if we are handling an lss model, we need to find the right data
    covariates <- raw_vars[[term]]
    if (length(covariates) > 1L) {
      # Interactions may use a generated grid rather than these observed rows,
      # so their estimates and SEs must be recomputed on that grid together.
      return(evaluate_parametric_component(object, term, covariates, columns,
        data, supplied_data, n, n_2d, n_3d, n_4d, dist, unconditional,
        overall_uncertainty, back_transform))
    }
    vars <- vars[term]
    term_expr <- str2expression(vars)[[1L]]
    x_data <- if (length(covariates) == 1L) data[[covariates]] else
      eval(term_expr, data, enclos = envir)
    axis_columns <- stats::setNames(if (is.factor(x_data)) ".level" else ".value",
      covariates)
    axes <- parametric_axes(object, vars, data, axis_columns)

    out <- bind_cols(
      .level = x_data,
      .partial = as.numeric(pred[["fit"]][, term]),
      .se = as.numeric(pred[["se.fit"]][, term])
    )
    out <- retain_parametric_axes(out, axes) |>
      distinct(.data$.level, .keep_all = TRUE)
    nr <- nrow(out)
    is_fac <- is.factor(out$.level)
    type <- "factor"
    f_levels <- NA
    if (!is_fac) {
      type <- class(out$.level) # numeric but I presume logical too
      out <- out |>
        rename(".value" = ".level")
    } else {
      f_levels <- levels(out$.level)
      if (is.ordered(out$.level)) {
        type <- "ordered"
      }
      out <- out |>
        mutate(.level = as.character(.data$.level))
    }
    out <- out |>
      add_column(
        .term = rep(term, times = nr),
        .type = rep(type, times = nr),
        .before = 1L
      )
    coordinate_cols <- unname(unlist(lapply(axes, function(axis) {
      c(axis$raw_column, axis$model_column)
    })))
    out <- display_parametric_axes(out, axes, from_raw = TRUE,
      back_transform = back_transform)$data |>
      nest(data = any_of(c(".level", ".value", ".partial", ".se", coordinate_cols)))
    axis_metadata <- parametric_axis_info(axes)
    if (!is_fac) {
      rug_data <- NULL
      if (!is.null(observed)) {
        observed_axes <- parametric_axes(object, vars, observed, axis_columns)
        rug_data <- tibble::as_tibble(stats::setNames(
          list(observed[[covariates]]), unname(axis_columns)))
        rug_data <- retain_parametric_axes(rug_data, observed_axes) |> distinct()
      }
      attr(axis_metadata, "observations_retained") <- TRUE
      attr(axis_metadata, "observed") <- rug_data
    }
    attr(out, "axis_info") <- axis_metadata

    # set levels as an attribute
    attr(out, "factor_levels") <- list(f_levels) |>
      set_names(term)
    out
  }

  effects <- map(valid_terms,
    .f = fun, data = data, pred = pred,
    vars = vars
  )

  # capture the `factor_levels` attribute off each term's data frame
  f_levels <- lapply(effects, \(x) attr(x, "factor_levels")) |>
    unlist(recursive = FALSE) # peel off the outer layer of the list
  # now we can bind the data frames together - need to do it this way to
  # preserve the levels of all factors - see #284
  term_info <- lapply(effects, function(x) attr(x, "term_info"))
  names(term_info) <- valid_terms
  term_info <- term_info[!vapply(term_info, is.null, logical(1))]
  axis_info <- lapply(effects, function(x) attr(x, "axis_info"))
  names(axis_info) <- valid_terms
  effects <- effects |> bind_rows()

  if (unnest) {
    effects <- unnest(effects, cols = "data") |>
      relocate(c(".partial", ".se"), .after = last_col())
  }

  attr(effects, "factor_levels") <- f_levels
  attr(effects, "term_info") <- term_info
  attr(effects, "axis_info") <- axis_info
  attr(effects, "back_transform") <- back_transform
  class(effects) <- c("parametric_effects", class(effects))
  effects # return
}

#' Plot estimated effects for model parametric terms
#'
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param line_col Deprecated; use [gratia_theme_get()] theme
#'   settings instead. Colour specification used for regression lines of linear
#'   continuous terms.
#'
#' @inheritParams draw.gam
#' @param object A `parametric_effects` object, optionally nested.
#' @param scales Use a common effect-axis range for curves and points (`fixed`),
#'   or separate ranges (`free`). Surface covariate axes are not affected.
#'
#' @export
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr group_by group_split group_map
#' @importFrom purrr map
#' @importFrom tidyr nest
#' @importFrom tidyselect any_of
#' @importFrom rlang .data
draw.parametric_effects <- function(object,
                                    scales = c("free", "fixed"),
                                    ci_level = 0.95,
                                    ci_col = "black",
                                    ci_alpha = 0.2,
                                    line_col = "black",
                                    constant = NULL,
                                    fun = NULL,
                                    rug = TRUE,
                                    position = "identity",
                                    angle = NULL,
                                    ...,
                                    ncol = NULL, nrow = NULL,
                                    guides = "keep",
                                    contour = TRUE, contour_col = "black",
                                    n_contour = NULL,
                                    surface = c("raster", "tile", "3d"),
                                    surface_3d = list(),
                                    ci_3d = FALSE,
                                    ci_3d_alpha = 0.15, ci_3d_col = "grey60",
                                    continuous_fill = NULL,
                                    discrete_colour = NULL, discrete_fill = NULL,
                                    back_transform = FALSE,
                                    time_varying_style = c("ribbon", "pointrange")) {
  gratia_plot_arguments(c("ci_col", "ci_alpha", "line_col", "contour_col", "ci_3d_alpha", "ci_3d_col"), environment())
  check_back_transform(back_transform)
  time_varying_style <- match.arg(time_varying_style)
  axis_info <- attr(object, "axis_info")
  time_varying_info <- attr(object, "time_varying_info")
  from_raw <- isTRUE(attr(object, "back_transform"))
  if ("data" %in% names(object) && !".partial" %in% names(object)) {
    info <- attr(object, "term_info")
    levels <- attr(object, "factor_levels")
    object <- tidyr::unnest(object, cols = "data")
    attr(object, "term_info") <- info
    attr(object, "factor_levels") <- levels
  }
  surface <- match.arg(surface)
  validate_surface_3d(surface_3d)
  validate_ci_3d(ci_3d, ci_3d_alpha, ci_3d_col)
  check_surface_dots(match.call(expand.dots = FALSE))
  # Add CI
  crit <- coverage_normal(ci_level)
  object <- mutate(object,
    .lower_ci = .data$.partial - (crit * .data$.se),
    .upper_ci = .data$.partial + (crit * .data$.se)
  )

  # fixed or free?
  scales <- match.arg(scales)

  # need to figure out scales if "fixed"
  ylim <- NULL
  if (isTRUE(identical(scales, "fixed"))) {
    displayed <- object |>
      add_constant(constant = constant,
        column = c(".partial", ".lower_ci", ".upper_ci")) |>
      transform_fun(fun = fun,
        column = c(".partial", ".lower_ci", ".upper_ci"))
    ylim <- surface_fill_limits(c(displayed$.partial, displayed$.upper_ci,
      displayed$.lower_ci), symmetric = FALSE)
  }

  f_levels <- attr(object, "factor_levels")
  term_info <- attr(object, "term_info")

  plts <- object |>
    group_by(.data$.term) |>
    group_map(
      .keep = TRUE,
      .f = ~ draw_parametric_effect(.x,
        ci_level = ci_level,
        ci_col = ci_col,
        ci_alpha = ci_alpha,
        line_col = line_col,
        constant = constant,
        fun = fun,
        rug = rug,
        position = position,
        ylim = ylim,
        angle = angle,
        factor_levels = f_levels, term_info = term_info,
        axis_info = axis_info, from_raw = from_raw,
        time_varying_info = time_varying_info,
        time_varying_style = time_varying_style,
        back_transform = back_transform,
        contour = contour, contour_col = contour_col, n_contour = n_contour,
        surface = surface, surface_3d = surface_3d,
        ci_3d = ci_3d, ci_3d_alpha = ci_3d_alpha, ci_3d_col = ci_3d_col,
        continuous_fill = continuous_fill,
        discrete_colour = discrete_colour, discrete_fill = discrete_fill
      )
    )

  # return
  n_plots <- length(plts)
  layout <- prepare_plot_layout(n_plots, ncol = ncol, nrow = nrow)
  ncol <- layout$ncol
  nrow <- layout$nrow
  wrap_plots(plts,
    byrow = TRUE, ncol = ncol, nrow = nrow,
    guides = guides, ...
  )
}

#' Internal function to draw an individual parametric effect
#'
#' @param xlab character or expression; the label for the x axis. If not
#'   supplied, a suitable label will be generated from `object`.
#' @param ylab character or expression; the label for the y axis. If not
#'   supplied, a suitable label will be generated from `object`.
#' @param title character or expression; the title for the plot. See
#'   [ggplot2::labs()].
#' @param subtitle character or expression; the subtitle for the plot. See
#'   [ggplot2::labs()].
#' @param caption character or expression; the plot caption. See
#'   [ggplot2::labs()].
#' @param term_info Internal per-term covariate and plotting metadata.
#' @param factor_levels list; a named list of factor levels
#' @param axis_info Internal per-term mappings between model and raw coordinates.
#' @param from_raw Internal logical; are the supplied coordinates on the raw scale?
#' @param time_varying_info Internal per-term labels for time-varying coefficients.
#'
#' @inheritParams draw.gam
#'
#' @importFrom dplyr mutate if_else recode_values
#' @importFrom ggplot2 ggplot aes geom_pointrange geom_rug geom_ribbon
#'   geom_line labs expand_limits
#' @keywords internal
`draw_parametric_effect` <- function(object,
                                     ci_level = 0.95,
                                     ci_col = "black",
                                     ci_alpha = 0.2,
                                     line_col = "black",
                                     constant = NULL,
                                     fun = NULL,
                                     xlab = NULL, ylab = NULL,
                                     title = NULL, subtitle = NULL,
                                     caption = NULL,
                                     rug = TRUE,
                                     position = "identity",
                                     ylim = NULL,
                                     angle = NULL,
                                     factor_levels = NULL,
                                     term_info = NULL,
                                     contour = TRUE, contour_col = "black",
                                     n_contour = NULL, surface = "raster", surface_3d = list(),
                                     ci_3d = FALSE,
                                     ci_3d_alpha = 0.15, ci_3d_col = "grey60",
                                     continuous_fill = NULL,
                                     discrete_colour = NULL, discrete_fill = NULL,
                                     axis_info = NULL, from_raw = FALSE,
                                     back_transform = FALSE,
                                     time_varying_info = NULL,
                                     time_varying_style = c("ribbon", "pointrange"),
                                     ...) {
  term <- unique(object$.term)
  if (identical(unique(object$.type), "time_varying")) {
    return(draw_time_varying_coefficient(object,
      info = time_varying_info[[term]], style = time_varying_style,
      ci_level = ci_level, ci_col = ci_col, ci_alpha = ci_alpha,
      line_col = line_col, constant = constant, fun = fun,
      xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
      caption = caption, ylim = ylim, angle = angle))
  }
  axes <- axis_info[[term]]
  displayed <- display_parametric_axes(object, axes, from_raw, back_transform)
  object <- displayed$data
  info <- term_info[[unique(object$.term)]]
  if (!is.null(info)) {
    info$axis_labels <- displayed$labels
    info$observed <- display_parametric_axes(info$observed, axes,
      from_raw = TRUE, back_transform = back_transform)$data
    return(draw_parametric_component(object, info,
      ci_level = ci_level, ci_col = ci_col, ci_alpha = ci_alpha,
      line_col = line_col, constant = constant, fun = fun,
      xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
      caption = caption, rug = rug, ylim = ylim, angle = angle,
      contour = contour, contour_col = contour_col, n_contour = n_contour,
      surface = surface, surface_3d = surface_3d,
      ci_3d = ci_3d, ci_3d_alpha = ci_3d_alpha, ci_3d_col = ci_3d_col,
      continuous_fill = continuous_fill,
      discrete_colour = discrete_colour, discrete_fill = discrete_fill))
  }
  # plot
  type <- unique(object[[".type"]])
  is_fac <- type %in% c("ordered", "factor")
  x_val <- if_else(is_fac, ".level", ".value")
  term_label <- unique(object[[".term"]])

  # grab the factor levels
  f_levels <- factor_levels[[term_label]]
  if (is_fac && !is.null(f_levels)) {
    object <- object |>
      mutate(".level" = factor(.data[[".level"]],
        levels = f_levels,
        ordered = isTRUE(type == "ordered")
      )
    )
  }

  ## add a CI
  if (!all(c(".upper_ci", ".lower_ci") %in% names(object))) {
    crit <- coverage_normal(ci_level)
    object <- mutate(object,
      .lower_ci = .data$.partial - (crit * .data$.se),
      .upper_ci = .data$.partial + (crit * .data$.se)
    )
  }

  # Shift estimates and intervals together, after creating any missing CI.
  object <- add_constant(object,
    constant = constant,
    column = c(".partial", ".lower_ci", ".upper_ci")
  )

  ## If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object,
    fun = fun,
    column = c(".partial", ".lower_ci", ".upper_ci")
  )

  # base plot
  plt <- gratia_plot(
    object,
    aes(x = .data[[x_val]], y = .data$.partial)
  ) +
    guides(x = guide_axis(angle = angle))

  if (is_fac) {
    plt <- plt + geom_gratia_effect_pointrange(aes(
      ymin = .data$.lower_ci,
      ymax = .data$.upper_ci
    ))
  } else {
    if (isTRUE(rug)) {
      if (isTRUE(attr(axes, "observations_retained"))) {
        observed <- display_parametric_axes(attr(axes, "observed"), axes,
          from_raw = TRUE, back_transform = back_transform)$data
        if (!is.null(observed)) {
          plt <- plt + geom_gratia_rug(data = observed, inherit.aes = FALSE,
            aes(x = .data$.value), sides = "b", position = position, alpha = 0.5)
        }
      } else {
        plt <- plt + geom_gratia_rug(sides = "b", position = position, alpha = 0.5)
      }
    }
    plt <- plt + geom_gratia_interval_ribbon(
      aes(
        ymin = .data$.lower_ci,
        ymax = .data$.upper_ci
      ),
      alpha = ci_alpha, fill = ci_col, colour = NA
    ) +
      geom_gratia_effect_line(colour = line_col)
  }

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- if (length(displayed$labels)) unname(displayed$labels[[1L]]) else term_label
  }
  if (is.null(ylab)) {
    ylab <- "Partial effect"
  }
  if (is.null(title)) {
    title <- term_label
  }
  if (is.null(caption)) {
    # caption <- paste("Parametric term")
    caption <- recode_values(
      type,
      "ordered" ~ "Ordered factor",
      "factor" ~ "Factor",
      "numeric" ~ "Numeric",
      "logical" ~ "Logical",
      default = "Parametric term",
      ptype = character()
    )
  }

  ## add labelling to plot
  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

  ## fixing the y axis limits?
  if (!is.null(ylim)) {
    plt <- plt + expand_limits(y = ylim)
  }

  plt
}
