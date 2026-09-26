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
#' @param unnest logical; unnest the parametric effect objects?
#' @param ci_level numeric; the coverage required for the confidence interval.
#'   Currently ignored.
#' @param envir an optional environment for local functions and constants, and
#'   for recovering fitting data when required raw columns are not stored in
#'   the model. Defaults to the model's evaluation environment.
#' @param transform logical; if `TRUE`, the parametric effect will be plotted on
#'   its transformed scale which will result in the effect being a straight
#'   line. If FALSE, the effect will be plotted against the raw data (i.e. for
#'   `log10(x)`, or `poly(z)`, the x-axis of the plot will be `x` or `z`
#'   respectively.)
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
#' A multi-column term such as `poly(x, 3)` is one component, whereas `x` and
#' `I(x^2)` remain separate components. Multivariate terms are evaluated against
#' their raw covariates; `transform = TRUE` is not supported for these terms.
#' Other covariates in generated prediction data are held at typical values
#' solely to evaluate the model matrix; their contributions are not included.
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
#'   multivariate terms contain their raw covariate columns. Names conflicting
#'   with reserved columns are repaired with numeric suffixes. The `term_info`
#'   attribute records multivariate column mappings, plotting order, factor
#'   levels and observation data. With `unnest = FALSE`, estimates and
#'   covariates are nested in a `data` list column.
#'
#' @examples
#' load_mgcv()
#' d <- data_sim("eg1", n = 200, seed = 42)
#' d$group <- factor(rep(c("A", "B"), length.out = nrow(d)))
#' m <- gam(y ~ x0 * group + x1:x2:x3, data = d, method = "REML")
#' pe <- parametric_effects(m, n_2d = 20, n_3d = 4)
#' draw(pe)
#' draw(m, parametric = TRUE, terms = "x0:group")
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
                                     transform = FALSE,
                                     n = 100, n_2d = 50,
                                     n_3d = 16, n_4d = 4,
                                     dist = 0.1, ...) {
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
  pred <- predict_model(object,
    newdata = data, type = "terms",
    terms = mgcv_names, se.fit = TRUE,
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
  if (transform && any(lengths(raw_vars) > 1L)) {
    stop("'transform = TRUE' is not supported for multivariate parametric terms.",
      call. = FALSE)
  }

  # loop over the valid_terms and prepare the parametric effects
  fun <- function(term, data, pred, vars) {
    # if we are handling an lss model, we need to find the right data
    covariates <- raw_vars[[term]]
    if (length(covariates) > 1L) {
      return(evaluate_parametric_component(object, term, covariates, columns,
        data, supplied_data, n, n_2d, n_3d, n_4d, dist, unconditional))
    }
    vars <- vars[term]
    term_expr <- str2expression(vars)[[1L]]
    x_data <- if (length(term_expr) > 1L) {
      if (transform) {
        pred$fit[, term]
      } else {
        if (length(covariates) == 1L) data[[covariates]] else
          eval(term_expr[[2L]], data, enclos = envir)
      }
    } else {
      eval(term_expr, data, enclos = envir)
    }

    out <- bind_cols(
      .level = x_data,
      .partial = as.numeric(pred[["fit"]][, term]),
      .se = as.numeric(pred[["se.fit"]][, term])
    ) |>
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
      ) |>
      nest(data = any_of(c(".level", ".value", ".partial", ".se")))

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
  effects <- effects |> bind_rows()

  if (unnest) {
    effects <- unnest(effects, cols = "data") |>
      relocate(c(".partial", ".se"), .after = last_col())
  }

  attr(effects, "factor_levels") <- f_levels
  attr(effects, "term_info") <- term_info
  class(effects) <- c("parametric_effects", class(effects))
  effects # return
}

#' Plot estimated effects for model parametric terms
#'
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param line_col colour specification used for regression lines of linear
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
                                    n_contour = NULL, geom = c("raster", "tile"),
                                    continuous_fill = NULL,
                                    discrete_colour = NULL, discrete_fill = NULL) {
  if ("data" %in% names(object) && !".partial" %in% names(object)) {
    info <- attr(object, "term_info")
    levels <- attr(object, "factor_levels")
    object <- tidyr::unnest(object, cols = "data")
    attr(object, "term_info") <- info
    attr(object, "factor_levels") <- levels
  }
  geom <- match.arg(geom)
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
    ylim <- range(object$.partial, object$.upper_ci, object$.lower_ci)
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
        contour = contour, contour_col = contour_col, n_contour = n_contour,
        geom = geom, continuous_fill = continuous_fill,
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
                                     n_contour = NULL, geom = "raster",
                                     continuous_fill = NULL,
                                     discrete_colour = NULL, discrete_fill = NULL,
                                     ...) {
  info <- term_info[[unique(object$.term)]]
  if (!is.null(info)) {
    return(draw_parametric_component(object, info,
      ci_level = ci_level, ci_col = ci_col, ci_alpha = ci_alpha,
      line_col = line_col, constant = constant, fun = fun,
      xlab = xlab, ylab = ylab, title = title, subtitle = subtitle,
      caption = caption, rug = rug, ylim = ylim, angle = angle,
      contour = contour, contour_col = contour_col, n_contour = n_contour,
      geom = geom, continuous_fill = continuous_fill,
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
  plt <- ggplot(
    object,
    aes(x = .data[[x_val]], y = .data$.partial)
  ) +
    guides(x = guide_axis(angle = angle))

  if (is_fac) {
    plt <- plt + geom_pointrange(aes(
      ymin = .data$.lower_ci,
      ymax = .data$.upper_ci
    ))
  } else {
    if (isTRUE(rug)) {
      plt <- plt + geom_rug(sides = "b", position = position, alpha = 0.5)
    }
    plt <- plt + geom_ribbon(
      aes(
        ymin = .data$.lower_ci,
        ymax = .data$.upper_ci
      ),
      alpha = ci_alpha, fill = ci_col, colour = NA
    ) +
      geom_line(colour = line_col)
  }

  ## default axis labels if none supplied
  if (is.null(xlab)) {
    xlab <- term_label
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
