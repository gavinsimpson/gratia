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
#'   covariates, and possibly their values, to condition up. The order of the
#'   values determines how these are plotted via the [draw.conditional_values()]
#'   method; the first element is mapped to the *x* channel, the second element
#'   to the *colour* channel, the third to [ggplot2::facet_wrap()] **if no
#'   fourth element is present**, if present, the fourth element is mapped to
#'   the rows and the third element is mapped to the columns of
#'   [ggplot2::facet_grid()].
#' @param data data frame of values at which to predict. If supplied overrides
#'   values supplied through `condition`.
#' @param scale character; which scale should predictions be returned on?
#' @param n_vals numeric; number of values to generate for numeric variables
#'   named in `condition`.
#' @param ci_level numeric; a number on interval (0,1) giving the coverage for
#'   credible intervals.
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
#' df <- data_sim("eg1", seed = 2)
#' m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
#'
#' # predictions conditional on values evenly spaced over x2, all other
#' # variables in model are held at representative values
#' cv <- conditional_values(
#'   m1,
#'   condition = "x2"
#' )
#' # plot
#' cv |> draw()
#'
#' # as above but condition on `x1` also. When plotted, `x1` is mapped to the
#' # colour channel, noting that it has been summarised using fivenum()
#' cv <- conditional_values(
#'   m1,
#'   condition = c("x2", "x1")
#' )
#' # plot
#' cv |> draw()
#'
#' # can pass `condition` a list, allowing for greater flexibility
#' # For example, here we condition on all four variables in the model,
#' # summarising:
#' #   * `x1` at its five number summary,
#' #   * `x0  at its quartiles
#' #   * `x3` at its mean a d mean +/- sd
#' cv <- conditional_values(
#'   m1,
#'   condition = list("x2", x1 = "fivenum", x0 = "quartile", x3 = "threenum")
#' )
#' # plot
#' cv |> draw()
#'
#' # some model terms can be exclude from the conditional predictions using the
#' # `exclude` mechanism of `predict.gam`. Here we exclude the effects of
#' # `s(x0)` and `s(x3)` from the conditional predictions. This, in effect,
#' # treats these smooths as having **0** effect on the conditional predictions
#' # of the response, even though the two smooths conditioned on (`s(x2)` and
#' # `s(x1)`) were estimated given the two excluded smooths were in the model
#' cv <- conditional_values(
#'   m1,
#'   condition = list("x2", x1 = "minmax"),
#'   exclude = c("s(x0)", "s(x3)")
#' )
#' # plot
#' cv |> draw()
#'
#' # categorical conditions are also handled
#' df <- data_sim("eg4", seed = 2)
#' m2 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")
#' cv <- conditional_values(
#'   m2,
#'   condition = list("fac", x2 = "fivenum")
#' )
#' # plot - we see a discrete x axis
#' cv |> draw()
#'
#' # in this example we condition on `x2` and `fac %in% c(2,3)`
#' cv <- conditional_values(
#'   m2,
#'   condition = list("x2", fac = 2:3)
#' )
#' # plot - smooths of `x2` for `fac == 2` and `fac == 3`
#' cv |> draw()
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
#' @importFrom dplyr setdiff
`conditional_values.gam` <- function(
  model,
  condition = NULL,
  data = NULL,
  scale = c("response", "link", "linear_predictor"),
  n_vals = 100,
  ci_level = 0.95,
  ...
) {
  scale <- match.arg(scale)
  # replace this with `process_condition` to capture all possible
  if (is.null(condition)) {
    stop("'condition' must be supplied")
  }
  c_x <- condition[1]
  c_colour <- condition[2]
  c_fcol <- condition[3]
  c_frow <- condition[4]

  c_cond <- c(c_x, c_colour, c_fcol, c_frow) |>
    setNames(c("x", "colour", "fcol", "frow"))

  c_miss <- is.na(c_cond)
  c_cond <- c_cond[!c_miss]

  data <- if (is.null(data)) {
    data <- data_slice_data(model) |> as_tibble()
  } else {
    data # probably need some checking here but for now just pass on
  }

  # the calls below shouldn't be to evenly but to a wrapper
  # that wrapper can call other functions to create the vectors of data needed
  # for the prediction set.
  m_vars <- model_vars(model)
  cond_list <- process_condition(condition, data = data, variables = m_vars,
    n_vals = n_vals)
  named_cond <- names(cond_list)

  # which model vars were *not* included in the conditions?
  used_vars <- names(cond_list)
  not_used <- dplyr::setdiff(m_vars, used_vars)
  tv <- typical_values(model, data = data)
  tv <- tv |> select(all_of(not_used))
  cond_list <- c(cond_list, tv)

  # return the data for testing
  pred_data <- expand_grid(!!!{cond_list})

  pv <- fitted_values(
    model, data = pred_data, scale = scale, ci_level = ci_level, ...
  )

  # form an attributes list so we know which vars are to be encoded to which
  # channels as per condition
  channels <- rep(list(NULL), 4) |>
    setNames(c("x", "colour", "f_col", "f_row"))
  channels[seq_along(named_cond)] <- named_cond
  attr(pv, "channels") <- channels

  # add a new class as we have attributes to handle with `[`
  class(pv) <- append(class(pv), "conditional_values", after = 0L)

  # return
  pv
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
#' @importFrom cli format_error qty
#' @importFrom rlang abort
`process_condition` <- function(condition, data, variables, n_vals = 100) {
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
      rlang::abort(format_error(msg))
    }
  }
  if (is.null(nms)) {
    nms <- rep(list(NULL), 4)
  }
  c_vec <- is.character(condition)
  if (!(c_list || c_vec)) {
    stop("'condition' must be a list or a character vector")
  }
  # if no names on the list, then unlist(condition) must be character vector
  if (c_list && is.null(nms)) {
    if (!(cl <- unlist(condition) |> is.character())) {
      stop("If 'condition' is an unnamed list, elements must be length 1
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
    stop("'condition' has '", lc, "' elements; only 4 allowed")
  }

  # iterate over the elements of condition, creating data vectors depending on
  # what is in each element
  out <- list()
  out$c1 <- condition_to_data(condition[[1]], nms[[1]], data = data,
    variables = variables, n_vals = n_vals, summary = FALSE
  )
  if (lc > 1L) {
    out$c2 <- condition_to_data(condition[[2]], nms[[2]], data = data,
      variables = variables, n_vals = n_vals, summary = TRUE
    )
  }
  if (lc > 2L) {
    out$c3 <- condition_to_data(condition[[3]], nms[[3]], data = data,
      variables = variables, n_vals = n_vals, summary = TRUE
    )
  }
  if (lc > 3L) {
    out$c4 <- condition_to_data(condition[[4]], nms[[4]], data = data,
      variables = variables, n_vals = n_vals, summary = TRUE
    )
  }

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
      stop("'", x, "' is not a variable in '", deparse(substitute(data)), "'")
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
      stop("'", name, "' is not in '", deparse(substitute(data)), "'")
    }
    # need to check what data type we are dealing with
    x_data <- data[[name]]

    # Handle different data types
    if (is.numeric(x_data)) { # handle number x
      if (isFALSE(identical(x_cls))) {
        stop("'", name, "' is numeric but supplied condition is not.")
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
#' @param object an object of class `"conditional_values"`, the result of a call
#'   to [conditional_values()].
#' @param facet_scales character; should facets have the same axis scales
#'   across facets? See [ggplot2::facet_wrap()] for details. Options are:
#'   `"fixed"` (default), `"free_x"`, `"free_y"`, and `"free"`.
#' @param ylab character; label for the y axis of the plot.
#' @param xlab character; label for the x axis of the plot.
#'
#' @inheritParams draw.smooth_estimates
#'
#' @importFrom rlang .data
#' @importFrom stats as.formula
#' @importFrom ggplot2 facet_grid facet_wrap ggplot geom_ribbon geom_line labs
#'   position_dodge geom_pointrange
#' @importFrom ggokabeito scale_colour_okabe_ito scale_fill_okabe_ito
#' @importFrom tidyselect where any_of
#' @export
`draw.conditional_values` <- function(
    object,
    facet_scales = "fixed",
    discrete_colour = NULL,
    discrete_fill = NULL,
    xlab = NULL,
    ylab = NULL,
    ...) {
  channels <- attr(object, "channels")

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
  plt <- ggplot(data = object)

  # if x channel is a numeric we have a continuous x axis
  if (isTRUE(is.numeric(object[[channels$x]]))) {
    if (is.null(channels$colour)) {
      plt <- plt +
        geom_ribbon(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted,
            ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]]),
          alpha = 0.2
        ) +
        geom_line(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted)
        )
    } else {
      plt <- plt +
        geom_ribbon(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted,
            ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]],
            fill = factor(.data[[channels$colour]])
          ),
          alpha = 0.2
        ) +
        geom_line(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted,
            colour = factor(.data[[channels$colour]])
          )
        )
    }
  } else { # categorical x axis
    if (is.null(channels$colour)) {
      plt <- plt +
        geom_pointrange(
          mapping = aes(x = .data[[channels$x]], y = .data$.fitted,
            ymin = .data[[".lower_ci"]], ymax = .data[[".upper_ci"]]
          )
        )
    } else {
      plt <- plt +
        geom_pointrange(
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
        scale_colour_hue()
      } else {
        scale_colour_okabe_ito()
      }
    }

    if (is.null(discrete_fill)) {
      discrete_fill <- if (n_levs > 9L) {
        scale_fill_hue()
      } else {
        scale_fill_okabe_ito()
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
      colour = channels$colour,
      fill = channels$colour
    )

  # return
  plt
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
