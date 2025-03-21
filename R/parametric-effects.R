#' Estimated values for parametric model terms
#'
#' @param object a fitted model object.
#' @param terms character; which model parametric terms should be drawn? The
#'   Default of `NULL` will plot all parametric terms that can be drawn.
#' @param data a optional data frame that may or may not be used? FIXME!
#' @param unconditional logical; should confidence intervals include the
#'   uncertainty due to smoothness selection? If `TRUE`, the corrected Bayesian
#'   covariance matrix will be used.
#' @param unnest logical; unnest the parametric effect objects?
#' @param ci_level numeric; the coverage required for the confidence interval.
#'   Currently ignored.
#' @param envir an environment to look up the data within.
#' @param transform logical; if `TRUE`, the parametric effect will be plotted on
#'   its transformed scale which will result in the effect being a straight
#'   line. If FALSE, the effect will be plotted against the raw data (i.e. for
#'   `log10(x)`, or `poly(z)`, the x-axis of the plot will be `x` or `z`
#'   respectively.)
#' @param ... arguments passed to other methods.
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
#'
#' @rdname parametric_effects
#' @export
`parametric_effects.gam` <- function(object, terms = NULL,
                                     data = NULL,
                                     unconditional = FALSE,
                                     unnest = TRUE,
                                     ci_level = 0.95,
                                     envir = environment(formula(object)),
                                     transform = FALSE,
                                     ...) {
  tt <- object$pterms # get model terms object
  tt <- delete.response(tt) # remove response so easier to work with
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
    if (!any(valid_terms <- terms %in% vars)) {
      stop(sprintf(
        "Term is not in the parametric part of model: <%s>",
        terms
      ))
    }
    terms[valid_terms]
  }

  # check order of terms; if > 1 interaction and not handled
  ord <- attr(tt, "order")[match(valid_terms, attr(tt, "term.labels"))]
  if (any(int <- ord > 1)) {
    message("Interaction terms are not currently supported.")
    valid_terms <- valid_terms[!(ord > 1)]
  }
  # return early if no vallid terms to
  if (length(valid_terms) == 0L) {
    message("The model doesn't contain any non-interaction parametric terms")
    return(NULL)
  }

  # try to recover the data
  mf <- model.frame(object)
  if (is.null(data)) {
    data <- eval(object$call$data, envir)
  }
  if (is.null(data)) {
    data <- mf
  }

  # have to do predictions *after* we reconstruct the data otherwise we get
  # problems if there were NAs in the original data.

  # predict model contributions for the parametric terms, using only
  # the factor combos in the data and typical values of all other terms
  # and exclude the effects of smooths as they don't change anything in
  # terms
  # get the data that we need for the model; right now `data` could contain
  # everything and be a model frame
  data <- model.frame(object$pred.formula, data = data)
  attr(data, "terms") <- NULL # squich this or predict.gam complains
  # can limit the data combinations we predict at now by taking the unique
  # data combos
  data <- distinct(data)
  # Work around a bug in predict.gam() with exclude length 0 character
  # (i.e smooths(objects) when model contains only parametric terms)
  pred <- predict(object,
    newdata = data, type = "terms",
    terms = mgcv_names, se.fit = TRUE,
    unconditional = unconditional
  )

  # loop over the valid_terms and prepare the parametric effects
  fun <- function(term, data, pred, vars) {
    # if we are handling an lss model, we need to find the right data
    vars <- vars[term]
    # term_expr <- str2expression(term)[[1L]]
    term_expr <- str2expression(vars)[[1L]]
    x_data <- if (length(term_expr) > 1L) {
      if (transform) {
        pred$fit[, term]
      } else {
        eval(term_expr[[2L]], data, enclos = envir)
      }
    } else {
      eval(term_expr, data, enclos = envir)
    }

    out <- bind_cols(
      .level = x_data,
      .partial = pred[["fit"]][, term],
      .se = pred[["se.fit"]][, term]
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

  # effects <- map_df(valid_terms,
  #  .f = fun, data = data, pred = pred,
  #  vars = vars
  # )
  effects <- map(valid_terms,
    .f = fun, data = data, pred = pred,
    vars = vars
  )

  # capture the `factor_levels` attribute off each term's data frame
  f_levels <- lapply(effects, \(x) attr(x, "factor_levels")) |>
    unlist(recursive = FALSE) # peel off the outer layer of the list
  # now we can bind the data frames together - need to do it this way to
  # preserve the levels of all factors - see #284
  effects <- effects |> bind_rows()

  if (unnest) {
    # f_levels <- attr(effects, "factor_levels")
    effects <- unnest(effects, cols = "data") |>
      relocate(c(".partial", ".se"), .after = last_col())
    attr(effects, "factor_levels") <- f_levels
  }

  ## add confidence interval -- be consistent and don't add this, but could?
  ## effects <- mutate(effects,
  ##                     upper = .data$partial + (2 * .data$se),
  ##                     lower = .data$partial - (2 * .data$se))

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
                                    guides = "keep") {
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
        factor_levels = f_levels
      )
    )

  # return
  n_plots <- length(plts)
  if (is.null(ncol) && is.null(nrow)) {
    ncol <- ceiling(sqrt(n_plots))
    nrow <- ceiling(n_plots / ncol)
  }
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
#' @param factor_levels list; a named list of factor levels
#'
#' @inheritParams draw.gam
#'
#' @importFrom dplyr mutate if_else
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
                                     ...) {
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

  ## If constant supplied apply it to `est`
  object <- add_constant(object, constant = constant, column = ".partial")

  ## add a CI
  if (!all(c(".upper_ci", ".lower_ci") %in% names(object))) {
    crit <- coverage_normal(ci_level)
    object <- mutate(object,
      .lower_ci = .data$.partial - (crit * .data$.se),
      .upper_ci = .data$.partial + (crit * .data$.se)
    )
  }

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
    caption <- case_match(type,
      "ordered" ~ "Ordered factor",
      "factor" ~ "Factor",
      "numeric" ~ "Numeric",
      "logical" ~ "Logical",
      .default = "Parametric term",
      .ptype = character()
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
