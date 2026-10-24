#' Conditional predictions from a GAM
#'
#' Generate predicted values from a GAM, conditional upon supplied values of
#' covariates. [conditional_values()] is modelled after
#' [marginaleffects::plot_predictions()], but with an intentionally simpler,
#' more restrictive functionality. The intended use case is for quickly
#' visualizing predicted values from a fitted GAM on the response scale. For
#' more complex model predictions, you are strongly encouraged to use
#' [marginaleffects::plot_predictions()].
#'
#' @param model a fitted GAM object.
#' @param condition either a character vector or a list supplying the names of
#'   covariates, and possibly their values, to condition on. The order of the
#'   values determines how these are plotted via the [draw.conditional_values()]
#'   method; the first element is mapped to the *x* channel, the second element
#'   to the *colour* channel, the third to [ggplot2::facet_wrap()] **if no
#'   fourth element is present**, if present, the fourth element is mapped to
#'   the rows and the third element is mapped to the columns of
#'   [ggplot2::facet_grid()].
#'   To request a surface, group two numeric covariates in the first element,
#'   for example `list(c("x1", "x2"), "treatment")`. The grouped covariates
#'   supply the horizontal and vertical axes, in that order. The remaining
#'   elements supply facet columns and rows; there is no categorical colour
#'   channel. Use a nested list to supply axis values or helpers, for example
#'   `list(list(x1 = c(0, 0.5, 1), x2 = "quartile"), "treatment")`.
#'   At most four covariates are allowed, counting both surface axes.
#' @param data optional data frame used to generate condition values and
#'   determine covariate classes. Explicit values in `condition` take precedence.
#'   Other covariates use the fitted model's typical values. The rows of `data`
#'   are not used directly as a prediction grid.
#' @param scale character; which scale should predictions be returned on?
#' @param n_vals positive integer or `NULL`; number of values to generate along
#'   each numeric axis. For [conditional_values()] and
#'   [conditional_differences()], `NULL` (the default) uses 100 for curves and
#'   50 per axis for surfaces. Explicit values or helpers in `condition`
#'   override generated values. Other numeric conditions use their five-number
#'   summaries unless explicit values or helpers are supplied.
#' @param ci_level numeric; a number on interval (0,1) giving the coverage for
#'   credible intervals.
#' @param complete logical; if `TRUE` (the default), retain all combinations of
#'   conditions. If `FALSE`, retain only combinations of factor levels among
#'   the variables named in `condition` that occur in the fitted model's data.
#'   Numeric conditions are not filtered, and variables absent from `condition`
#'   remain at their typical values. Supplied `data` does not change which
#'   combinations count as observed. An error is raised if no combinations
#'   remain.
#' @param ... arguments passed to [fitted_values()].
#'
#' @author Gavin L. Simpson
#'
#' @return A data frame (tibble) of class `"conditional_values"`.
#'
#' @export
#'
#' @examples
#' load_mgcv()
#' # Simulate an additive Gaussian response and fit a GAM
#' df <- data_sim("eg1", n = 200, seed = 2)
#' m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' # Vary x2 while holding the other covariates at representative values
#' cv <- conditional_values(m, condition = "x2")
#' cv
#' draw(cv)
`conditional_values` <- function(
  model,
  condition = NULL,
  data = NULL,
  scale = c("response", "link", "linear_predictor"),
  ...
) {
  UseMethod("conditional_values")
}

#' @export
#' @rdname conditional_values
#' @importFrom dplyr setdiff semi_join
`conditional_values.gam` <- function(
  model,
  condition = NULL,
  data = NULL,
  scale = c("response", "link", "linear_predictor"),
  n_vals = NULL,
  ci_level = 0.95,
  complete = TRUE,
  ...
) {
  scale <- match.arg(scale)
  grid <- conditional_prediction_grid(model, condition, data, n_vals, complete,
    surfaces = TRUE)
  pred_data <- grid$data

  pv <- fitted_values(
    model, data = pred_data, scale = scale, ci_level = ci_level, ...
  )

  # form an attributes list so we know which vars are to be encoded to which
  # channels as per condition
  attr(pv, "channels") <- grid$channels

  # add a new class as we have attributes to handle with `[`
  class(pv) <- append(class(pv), "conditional_values", after = 0L)

  # return
  pv
}

# Shared grid construction for conditional predictions and differences.
# Comparison factors are expanded separately, so they do not consume plotting
# channels or change which numeric condition is evaluated over its full range.
conditional_prediction_grid <- function(model, condition, data = NULL,
                                        n_vals = 100, complete = TRUE,
                                        by = character(), surfaces = FALSE) {
  if (!is.logical(complete) || length(complete) != 1L || is.na(complete)) {
    cli::cli_abort("{.arg complete} must be a single non-missing logical value")
  }
  if (is.null(condition) || !length(condition)) {
    cli::cli_abort("{.arg condition} must be supplied")
  }
  if (!is.null(n_vals) && (!is.numeric(n_vals) || length(n_vals) != 1L ||
      !is.finite(n_vals) || n_vals < 1 || n_vals > .Machine$integer.max ||
      n_vals != trunc(n_vals))) {
    cli::cli_abort("{.arg n_vals} must be NULL or a positive integer.")
  }
  if (is.null(data)) data <- data_slice_data(model) |> as_tibble()
  m_vars <- model_vars(model)
  comparison <- list()
  if (length(by)) {
    if (!(is.character(condition) || is.list(condition))) {
      cli::cli_abort("{.arg condition} must be a list or a character vector")
    }
    condition <- as.list(condition)
    nms <- names(condition)
    if (is.null(nms)) nms <- rep("", length(condition))
    ids <- vapply(seq_along(condition), function(i) {
      if (nzchar(nms[i])) nms[i] else {
        x <- condition[[i]]
        if (is.character(x) && length(x) == 1L) x else ""
      }
    }, character(1L))
    if (anyDuplicated(ids[nzchar(ids)])) {
      cli::cli_abort("Variables in {.arg condition} must not be repeated.")
    }
    for (nm in by) {
      lev <- levels(model$var.summary[[nm]])
      if (!nm %in% names(data)) {
        cli::cli_abort("Comparison factor {.var {nm}} is missing from {.arg data}.")
      }
      values <- as.character(data[[nm]])
      if (anyNA(values) || any(!values %in% lev)) {
        cli::cli_abort("Comparison factor {.var {nm}} has missing or unknown levels.")
      }
      available <- if (is.factor(data[[nm]])) levels(data[[nm]]) else unique(values)
      data[[nm]] <- factor(values, levels = lev[lev %in% available],
        ordered = is.ordered(model$var.summary[[nm]]))
      i <- match(nm, ids)
      selected <- if (is.na(i)) {
        evenly(data[[nm]], n = n_vals %||% 100)
      } else {
        condition_to_data(condition[[i]],
          if (nzchar(nms[i])) nm else NULL, data, m_vars, n_vals %||% 100)
      }
      if (anyNA(selected) || any(!as.character(selected) %in% lev)) {
        cli::cli_abort("Invalid comparison levels for {.var {nm}}.")
      }
      comparison[[nm]] <- factor(lev[lev %in% as.character(selected)],
        levels = lev, ordered = is.ordered(data[[nm]]))
    }
    condition <- condition[!ids %in% by]
    if (!length(condition)) {
      cli::cli_abort("{.arg condition} must include a covariate other than {.arg by}.")
    }
  }
  specification <- conditional_axes(condition, surfaces = surfaces)
  condition <- specification$condition
  n_axes <- specification$n_axes
  n_vals <- n_vals %||% if (n_axes == 2L) 50 else 100
  cond_list <- process_condition(condition, data = data, variables = m_vars,
    n_vals = n_vals, n_axes = n_axes)
  named_cond <- names(cond_list)
  if (anyDuplicated(named_cond)) {
    cli::cli_abort("Variables in {.arg condition} must not be repeated.")
  }
  if (n_axes == 2L) {
    axes <- cond_list[1:2]
    valid <- vapply(seq_along(axes), function(i) {
      nm <- names(axes)[i]
      nm %in% m_vars && !nm %in% by && is.numeric(data[[nm]]) &&
        is.numeric(axes[[i]]) && all(is.finite(axes[[i]])) &&
        length(unique(axes[[i]])) >= 2L
    }, logical(1L))
    if (!all(valid)) {
      cli::cli_abort("Surface axes must be two distinct numeric model covariates, each with at least two finite values, and cannot be comparison factors.")
    }
    cond_list[1:2] <- lapply(axes, unique)
  }
  used_vars <- c(named_cond, by)
  tv <- typical_values(model, data = data)
  tv <- tv[dplyr::setdiff(m_vars, used_vars)]
  cond_list <- c(cond_list, comparison, tv)
  if (length(by)) cond_list <- lapply(cond_list, unique)
  pred_data <- expand_grid(!!!cond_list)
  if (!complete) {
    factor_vars <- used_vars[
      vapply(cond_list[used_vars], is.factor, logical(1L))
    ]
    if (length(factor_vars)) {
      observed <- data_combos(model, vars = all_of(factor_vars),
        complete = FALSE, data = data)
      pred_data <- semi_join(pred_data, observed, by = factor_vars)
      if (!nrow(pred_data)) {
        cli::cli_abort("No observed combinations of the supplied factor conditions remain")
      }
    }
  }
  list(data = pred_data, condition = named_cond,
    channels = conditional_channels(named_cond, n_axes))
}

# A group in the first plotting position supplies both surface coordinates.
# Flatten only that group; named vectors elsewhere remain explicit values.
conditional_axes <- function(condition, surfaces = FALSE) {
  if (!(is.character(condition) || is.list(condition))) {
    cli::cli_abort("{.arg condition} must be a list or a character vector")
  }
  condition <- as.list(condition)
  nms <- names(condition)
  if (is.null(nms)) nms <- rep("", length(condition))
  if (anyNA(nms)) cli::cli_abort("Condition names must not be missing.")
  grouped <- vapply(seq_along(condition), function(i) {
    !nzchar(nms[i]) && (is.list(condition[[i]]) ||
      (is.character(condition[[i]]) && length(condition[[i]]) > 1L))
  }, logical(1L))
  n_axes <- 1L
  if (any(grouped)) {
    if (!surfaces) {
      cli::cli_abort("Grouped surface conditions are supported only by {.fn conditional_values} and {.fn conditional_differences}.")
    }
    if (!identical(which(grouped), 1L) || length(condition[[1L]]) != 2L) {
      cli::cli_abort("A surface requires exactly two covariates grouped in the first plotting position of {.arg condition}.")
    }
    axes <- as.list(condition[[1L]])
    condition <- c(axes, condition[-1L])
    n_axes <- 2L
  }
  list(condition = condition, n_axes = n_axes)
}

conditional_channels <- function(condition, n_axes = 1L) {
  channels <- setNames(rep(list(NULL), 4L), c("x", "colour", "f_col", "f_row"))
  if (n_axes == 1L) {
    channels[seq_along(condition)] <- condition
  } else {
    channels$x <- condition[1:2]
    extra <- condition[-c(1L, 2L)]
    if (length(extra)) channels[c("f_col", "f_row")[seq_along(extra)]] <- extra
  }
  channels
}

`condition_helper` <- function(helper) {
  # internal allowed functions
  allowed <- c("threenum", "fivenum", "quartile", "minmax", "decile")
  if (length(helper) > 1L) {
    return(FALSE) # return early
  }
  is_allowed <- match(helper, allowed)
  helper <- if (is.na(is_allowed)) {
    FALSE
  } else {
    match.fun(paste0("g_", helper)) # make fun g_helper
  }
  helper
}

#' @importFrom stats sd
`g_threenum` <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  c(mu - sd, mu, mu + sd)
}

#' @importFrom stats fivenum
`g_fivenum` <- function(x) {
  fivenum(x, na.rm = TRUE)
}

#' @importFrom stats quantile
`g_quartile` <- function(x) {
  quantile(x, c(1, 2, 3) / 4, na.rm = TRUE)
}

#' @importFrom stats quantile
`g_decile` <- function(x) {
  quantile(x, seq(1, 9) / 10, na.rm = TRUE)
}

#' @importFrom stats quantile
`g_minmax` <- function(x) {
  range(x, na.rm = TRUE)
}

#' @importFrom scales ordinal_format
#' @importFrom cli qty
`process_condition` <- function(condition, data, variables, n_vals = 100,
                              n_axes = 1L) {
  # condition: the thing passed as condition to conditional_values
  # data: a data frame to use for evaluating the fitted values
  # variables: the variables used in the model
  # n_vals: how many values of the x condition to create
  c_list <- is.list(condition)
  nms <- names(condition)
  nms_len <- nchar(nms)
  # check if any names are "" == 0 characters, for those elements with such
  # names, check if that element of conditions is the name of a variable in the
  # model. If it is we don't need to do anything. If it isn't, check if it is a
  # helper: if it is a helper, throw an error but prompt user to check if they
  # forgot to name that element. If it is'nt a helper, throw a less specific
  # error
  no_nms <- nms_len == 0
  if (any(no_nms)) {
    is_zero <- which(no_nms)
    # check if the unnamed element contains the name of a variable in the model
    nm_in_model <- condition[is_zero] %in% variables
    # drop any that are in models
    no_nms[is_zero[which(nm_in_model)]] <- FALSE
    is_zero <- is_zero[!nm_in_model]
    # check if any remaining unnamed elements contain helpers
    allowed <- c("threenum", "fivenum", "quartile", "minmax", "decile")
    is_allowed <- condition[is_zero] %in% allowed
    # report this problem
    msg_allowed <- NULL
    if (any(is_allowed)) {
      which_allowed <- is_zero[is_allowed]
      n_allowed <- length(which_allowed)
      str_allowed <- scales::ordinal_format()(which_allowed)
      msg_allowed <- "The {str_allowed} element{?s} of {.var condition} {?is/are} unnamed, but {?contains a/contains} function name{?s}."
    }
    # drop any that are helpers
    no_nms[is_zero] <- FALSE
    is_zero <- is_zero[!is_allowed]
    # if there are any remaining, then format a different message
    msg_wrong <- NULL
    if ((n_wrong <- length(is_zero)) > 0L) {
      str_wrong <- scales::ordinal_format()(is_zero)
      msg_wrong <- "The {str_wrong} element{?s} of {.var condition} {?is/are} unnamed but {?does/do} not refer to {?a variable/variables} in the model."
    }
    msg_qty <- length(c(msg_wrong, msg_allowed))
    msg <- c("{.strong Error in the supplied {.var condition}:}",
      "x" = msg_wrong,
      "x" = msg_allowed,
      "i" = "Did you forget to name {qty(msg_qty)} {?this/these} element{?s}?"
    )
    if (!is.null(msg_allowed) | !is.null(msg_wrong)) {
      cli::cli_abort(msg)
    }
  }
  if (is.null(nms)) {
    nms <- rep(list(NULL), 4)
  }
  c_vec <- is.character(condition)
  if (!(c_list || c_vec)) {
    cli::cli_abort("{.arg condition} must be a list or a character vector")
  }
  # if no names on the list, then unlist(condition) must be character vector
  if (c_list && is.null(nms)) {
    if (!(cl <- unlist(condition) |> is.character())) {
      cli::cli_abort("If {.arg condition} is an unnamed list, elements must be length 1
characters")
    }
  }

  # if character vector, then convert to a list
  if (c_vec) {
    condition <- condition |> as.list()
    c_list <- TRUE
  }

  # if list has names then we either have vectors of covariate values to eval
  # model at, or we have functions (or names of functions) that we can apply
  # but we should only have at most 4
  if ((lc <- length(condition)) > 4L) {
    cli::cli_abort("{.arg condition} has {lc} elements; only 4 allowed.")
  }

  # iterate over the elements of condition, creating data vectors depending on
  # what is in each element
  out <- lapply(seq_len(lc), function(i) {
    condition_to_data(condition[[i]], nms[[i]], data = data,
      variables = variables, n_vals = n_vals, summary = i > n_axes)
  })

  # get the variable names as processed
  var_nms <- vapply(out, attr, character(1L), "var_name")

  # return
  out <- out |>
    setNames(var_nms)
  out
}

`condition_to_data` <- function(
  x,
  name,
  data,
  variables,
  n_vals = 100,
  summary = FALSE
) {
  good <- FALSE # keep track of whether 'x' is valid or not

  # is x one of the internal function names we allow?
  FUN <- condition_helper(x)
  is_allowed <- is.function(FUN)

  # check if x is a function - need to skip all this if x is a name of a model
  # term
  if (any(!x %in% variables) && identical(length(x), 1L)) {
    is_fun <- try(match.fun(x), silent = TRUE)
    is_fun <- if (inherits(is_fun, "try-error")) {
      FALSE
    } else {
      FUN <- is_fun
      TRUE
    }

    if (is_allowed || is_fun) {
      good <- TRUE
      out <- FUN(data[[name]])
    }
  }

  # what is 'x' is not a function (name)?
  x_cls <- data.class(x)

  # if 'x' is character, length 1, and hasn't matched already, then lookup
  # 'x' from data and return an even spread
  # this handles factors too because evenly() handles them
  # this should only match setting where condition has no names i.e.
  #   condition = "foo", or
  #   condition = list("foo")
  if (!good && identical(x_cls, "character") &&
      (is.null(name) || identical(name, ""))
  ) {
    name <- x
    x_data <- data[[x]]
    if (is.null(x_data)) {
      cli::cli_abort("{.var {x}} is not a variable in {.arg data}.")
    }
    # if this is a faceting var, we want a small set of representative values
    # to use as facet values
    out <- if (isTRUE(summary) && !is.factor(x_data)) {
      fivenum(x_data, na.rm = TRUE)
    } else {
      evenly(x_data, n = n_vals)
    }
    good <- TRUE
  }

  # if we get here then 'x' not a function or name of variable in 'data'
  # at this point we allow numeric or factor, or character, which we should
  # convert to a factor with the right levels
  if (!good) {
    # check name is in data
    if (!(name %in% names(data))) {
      cli::cli_abort("{.var {name}} is not in {.arg data}.")
    }
    # need to check what data type we are dealing with
    x_data <- data[[name]]

    # Handle different data types
    if (is.numeric(x_data)) { # handle number x
      if (isFALSE(identical(x_cls, "numeric"))) {
        cli::cli_abort("{.var {name}} is numeric but the supplied condition is not.")
      }
      good <- TRUE
      out <- x
    }

    if (is.factor(x_data)) { # handle factor x
      out <- level(data[[name]], as.character(x))
      good <- TRUE
    }
  }

  # return NA if supplied data is not good
  if (isFALSE(good)) {
    out <- NA_integer_
  }

  # set name as an attribute that we pass back
  attr(out, "var_name") <- name
  # return
  out
}

#' Plot conditional predictions
#'
#' Curves show predictions and intervals. Grouped numeric conditions show a
#' surface, with predictions mapped to fill and, for `surface = "3d"`, height.
#' Remaining conditions create separate facets. Flat surfaces show estimates;
#' `ci_3d = TRUE` adds the returned lower and upper interval surfaces in 3D.
#'
#' @param object an object of class `"conditional_values"`, the result of a call
#'   to [conditional_values()].
#' @param facet_scales character; should facets have the same axis scales
#'   across facets? See [ggplot2::facet_wrap()] for details. Options are:
#'   `"fixed"` (default), `"free_x"`, `"free_y"`, and `"free"`.
#' @param ylab character; label for the prediction axis of a curve, or the
#'   second covariate axis of a surface.
#' @param xlab character; label for the x axis of the plot.
#' @param zlab character; label for the surface fill legend and 3D height axis.
#'   Defaults to the prediction label, or the difference and its scale for
#'   conditional differences.
#' @param surface character; `"raster"` (default), `"tile"`, or `"3d"`.
#'   Applies to grouped numeric conditions. Three-dimensional surfaces require
#'   the optional ggcube package. Irregular flat grids use rectangles bounded
#'   by adjacent coordinate midpoints.
#' @param ci_3d logical; add the returned lower and upper interval surfaces
#'   when `surface = "3d"`? Defaults to `FALSE`.
#' @param surface_3d named list of view options passed to [ggcube::coord_3d()],
#'   for example `list(yaw = 45, persp = FALSE)`. The default view uses
#'   `ggcube::light(backface_scale = 1)`. A supplied `light` replaces that
#'   lighting specification. The same view applies to all panels; gratia
#'   retains its plot theme.
#' @param contour logical; draw contour lines on surfaces? Defaults to `TRUE`.
#' @param ... additional arguments; currently unused.
#'
#' @inheritParams draw.smooth_estimates
#'
#' @importFrom rlang .data
#' @importFrom stats as.formula
#' @importFrom ggplot2 facet_grid facet_wrap ggplot geom_ribbon geom_line labs
#'   position_dodge geom_pointrange
#' @importFrom tidyselect where any_of
#' @export
#' @inheritSection draw Label layout
`draw.conditional_values` <- function(
    object,
    facet_scales = "fixed",
    discrete_colour = NULL,
    discrete_fill = NULL,
    xlab = NULL,
    ylab = NULL,
    ...,
    surface = c("raster", "tile", "3d"), surface_3d = list(),
    continuous_fill = NULL, contour = TRUE, n_contour = NULL,
    ci_3d = FALSE, zlab = NULL) {
  channels <- attr(object, "channels")
  surface <- match.arg(surface)
  if (length(channels$x) == 2L) {
    return(draw_conditional_surface(object, channels,
      facet_scales = facet_scales, xlab = xlab, ylab = ylab, zlab = zlab,
      surface = surface, surface_3d = surface_3d,
      continuous_fill = continuous_fill, contour = contour,
      n_contour = n_contour, ci_3d = ci_3d))
  }

  # if using colour channel of faceting, fix the number of signif digits
  # if colour or facet vars are not NULL and are numeric
  object <- object |>
    mutate(
      across(
        any_of(c(channels$f_col, channels$f_row, channels$colour)) &
          where(\(x) !is.null(x) & is.numeric(x)),
        .fns = \(x) format_to_signif(x, digits = 2)
      )
    )

  # base plot
  plt <- gratia_plot(data = object)

  # if x channel is a numeric we have a continuous x axis
  if (isTRUE(is.numeric(object[[channels$x]]))) {
    if (is.null(channels$colour)) {
      plt <- plt +
        geom_gratia_interval_ribbon(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted,
            ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]]),
          alpha = 0.2
        ) +
        geom_gratia_effect_line(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted)
        )
    } else {
      plt <- plt +
        geom_gratia_interval_ribbon(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted,
            ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]],
            fill = factor(.data[[channels$colour]])
          ),
          alpha = 0.2
        ) +
        geom_gratia_effect_line(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted,
            colour = factor(.data[[channels$colour]])
          )
        )
    }
  } else { # categorical x axis
    if (is.null(channels$colour)) {
      plt <- plt +
        geom_gratia_effect_pointrange(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted,
            ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]]
          )
        )
    } else {
      plt <- plt +
        geom_gratia_effect_pointrange(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted,
            ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]],
            colour = factor(.data[[channels$colour]])
          ),
          position = position_dodge(width = 0.2)
        )
    }
  }

  # colour guide labels
  if (!is.null(channels$colour)) {
    # colour scales
    # how many levels? can't have more than 9 for okabeito
    n_levs <- nlevels(object[[channels$colour]])
    if (is.null(discrete_colour)) {
      discrete_colour <- if (n_levs > 9L) {
        gratia_default_scale(scale_colour_hue())
      } else {
        gratia_default_scale(scale_colour_okabe_ito())
      }
    }

    if (is.null(discrete_fill)) {
      discrete_fill <- if (n_levs > 9L) {
        gratia_default_scale(scale_fill_hue())
      } else {
        gratia_default_scale(scale_fill_okabe_ito())
      }
    }
    plt <- plt +
      discrete_colour +
      discrete_fill
  }

  # facets - are we faceting?
  if (!is.null(channels$f_col)) {
    # 2 faceting vars so use facet_grid
    if (!is.null(channels$f_col) && !is.null(channels$f_row)) {
      fml <- paste(channels$f_row, "~", channels$f_col) |>
        as.formula()
      plt <- plt +
        facet_grid(
          fml,
          scales = facet_scales,
          labeller = label_both
        )
    } else { # one faceting variable so wrap
      fml <- paste("~", channels$f_col)
      plt <- plt +
        facet_wrap(
          fml,
          scales = facet_scales,
          labeller = label_both
        )
    }
  }

  # labels
  plt <- plt +
    labs(
      x = if (is.null(xlab)) channels$x else xlab,
      y = if (is.null(ylab)) expression(hat(y)) else ylab,
      colour = channels$colour
    )

  if (isFALSE(is.factor(object[[channels$x]]))) {
    plt <- plt + labs(fill = channels$colour)
  }

  # return
  plt
}

# Surface rendering is shared by predictions and differences. Facet variables
# are passed to the mesh builder as well as ggplot2 so panels never share faces.
draw_conditional_surface <- function(object, channels, facet_scales = "fixed",
    xlab = NULL, ylab = NULL, zlab = NULL, surface = "raster",
    surface_3d = list(), continuous_fill = NULL, contour = TRUE,
    n_contour = NULL, ci_3d = FALSE) {
  difference <- inherits(object, "conditional_differences")
  object$.estimate <- if (difference) object$.diff else object$.fitted
  value_label <- if (difference) {
    paste0("Difference (", attr(object, "scale"), " scale)")
  } else expression(hat(y))
  value_label <- zlab %||% value_label
  extra <- intersect(c(".parameter", ".category", ".y", ".family"), names(object))
  extra <- extra[vapply(object[extra], function(x) dplyr::n_distinct(x) > 1L,
    logical(1L))]
  facet_vars <- unique(c(if (difference) ".comparison", extra,
    channels$f_col, channels$f_row))
  facet <- if (difference || length(extra)) {
    ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(facet_vars)),
      scales = facet_scales,
      labeller = ggplot2::labeller(
        .comparison = ggplot2::as_labeller(
          function(x) sub("^[0-9]+[.] ", "", x),
          default = ggplot2::label_value),
        .default = ggplot2::label_both))
  } else if (!is.null(channels$f_row)) {
    ggplot2::facet_grid(rows = ggplot2::vars(!!!rlang::syms(channels$f_row)),
      cols = ggplot2::vars(!!!rlang::syms(channels$f_col)),
      scales = facet_scales, labeller = ggplot2::label_both)
  } else if (!is.null(channels$f_col)) {
    ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(channels$f_col)),
      scales = facet_scales, labeller = ggplot2::label_both)
  } else NULL
  # Constant surfaces remain useful, but have no contour lines to compute.
  contour_data <- object |>
    dplyr::group_by(across(all_of(facet_vars))) |>
    dplyr::filter(dplyr::n_distinct(.data$.estimate[is.finite(.data$.estimate)]) > 1L) |>
    dplyr::ungroup()
  fill_limits <- surface_fill_limits(object$.estimate, symmetric = difference)
  prepare_surface_plot(object,
    x_var = channels$x[1L], y_var = channels$x[2L], fill_var = ".estimate",
    fill_limits = fill_limits, fill_title = value_label,
    labels = ggplot2::labs(x = xlab %||% channels$x[1L],
      y = ylab %||% channels$x[2L]),
    surface = surface, surface_3d = surface_3d,
    continuous_fill = continuous_fill,
    contour = isTRUE(contour) && nrow(contour_data) > 0L,
    contour_data = contour_data, n_contour = n_contour,
    ci_3d = ci_3d, facet = facet, facet_vars = facet_vars,
    irregular_cells = TRUE,
    z_limits = if (difference) NULL else fill_limits)
}

# Simple formatter for numeric data that will be used for facets
# Format the numeric vector `x` such that it has ~ `digits` significant digits
`format_to_signif` <- function(x, digits = 2) {
  x <- format(x, digits = digits)
  u_vals <- unique(x) |>
    sort()
  x <- x |>
    factor(levels = u_vals)
  x
}
