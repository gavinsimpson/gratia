#' Plot variance components and proportions of term variance
#'
#' Draw estimates and their stored confidence intervals, with components on
#' the x-axis by default. No model fitting or posterior sampling is performed.
#'
#' @param object A result from [variance_comp()].
#' @param type What to plot: `"variance"` (the default), `"std_dev"`, or
#'   `"proportion"`. Proportions require `variance_comp(..., type = "terms")`.
#' @param select Components to plot, as exact names, positive integer positions,
#'   or a logical vector with one entry per row of `object`. Components are
#'   displayed from left to right in the requested order, or from top to bottom
#'   with `flip = TRUE`. Use positions when component names are duplicated.
#' @param include_total Logical; include the total rows of term results?
#'   Defaults to `TRUE`. Selection positions refer to `object` before totals
#'   are removed.
#' @param predictor Linear predictors to plot for `type = "proportion"`, as
#'   positive integer positions or total labels (`"total"`, `"total.1"`, and
#'   so on). The default plots all predictors, with separate panels when more
#'   than one is present. Shared terms appear in each relevant panel.
#' @param ncol,nrow Number of columns or rows for the panels.
#' @param angle Numeric; angle of the component labels in degrees, between
#'   -90 and 90. Passed to [ggplot2::guide_axis()]. The default, `NULL`, uses
#'   the angle specified by the plot theme.
#' @param n_dodge Positive integer; number of rows for component labels, or
#'   columns with `flip = TRUE`.
#'   Use, for example, `n_dodge = 2` to stagger labels that would overlap.
#'   Passed to the `n.dodge` argument of [ggplot2::guide_axis()]. Defaults to 1.
#' @param flip Logical; put components on the y-axis? Defaults to `FALSE`.
#' @param ... Not used. Modify the returned plot with ggplot2 layers, scales
#'   and themes.
#'
#' @details
#' Variance plots square the stored standard-deviation interval bounds so that
#' estimates and intervals use the same scale. Standard-deviation and proportion
#' plots use their stored bounds directly. Components with unavailable intervals
#' retain their points. Intervals are drawn when both bounds are finite.
#' Undefined estimates have no point, but retain their component labels.
#'
#' Proportions can be negative or greater than one, and the plot preserves
#' those values. Each panel shows only terms belonging to that predictor and
#' its own total. A predictor with zero total variance has undefined proportions
#' and is displayed without points or intervals.
#'
#' Points use the `geom.gratia.effect` theme element and intervals use
#' `geom.gratia.interval`; see [gratia_theme_get()].
#'
#' @return A [ggplot2::ggplot()] object.
#' @seealso [variance_comp()], [gratia_theme_get()]
#' @export
#' @examples
#' load_mgcv()
#' # Simulate linear and nonlinear effects with Gaussian noise
#' set.seed(42)
#' dat <- data.frame(x = runif(120), z = runif(120))
#' dat$y <- 2 * dat$z + sin(2 * pi * dat$x) + rnorm(120, sd = 0.3)
#'
#' # Fit a linear effect of z and a smooth effect of x
#' model <- gam(y ~ z + s(x, k = 8), data = dat, method = "REML")
#'
#' # Plot the variances used in the random-effect representation of the penalties
#' components <- variance_comp(model)
#' draw(components)
#'
#' # Plot variation of fitted term contributions across the fitting observations
#' terms <- variance_comp(model, type = "terms", n_draws = 200, seed = 42)
#' draw(terms)
#' @inheritSection draw Label layout
draw.variance_comp <- function(object, type = c("variance", "std_dev", "proportion"),
    select = NULL, include_total = TRUE, predictor = NULL, ncol = NULL,
    nrow = NULL, angle = NULL, n_dodge = 1L, flip = FALSE, ...) {
  type <- match.arg(type)
  if (length(list(...))) stop("Additional arguments are not used by this method; ",
    "modify the returned ggplot instead.", call. = FALSE)
  if (!is.logical(include_total) || length(include_total) != 1L ||
      is.na(include_total)) stop("'include_total' must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(flip) || length(flip) != 1L || is.na(flip)) {
    stop("'flip' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(angle) && (!is.numeric(angle) || length(angle) != 1L ||
      !is.finite(angle) || abs(angle) > 90)) {
    stop("'angle' must be NULL or a finite number between -90 and 90.", call. = FALSE)
  }
  n_dodge <- sampling_count(n_dodge, "n_dodge")
  if (!is.null(ncol)) ncol <- sampling_count(ncol, "ncol")
  if (!is.null(nrow)) nrow <- sampling_count(nrow, "nrow")
  if (!nrow(object) || !".component" %in% names(object)) {
    stop("No variance components are available to plot.", call. = FALSE)
  }
  prop_columns <- grep("^\\.proportion(\\.[0-9]+)?$", names(object), value = TRUE)
  totals <- sub(".proportion", "total", prop_columns, fixed = TRUE)
  rows <- variance_comp_plot_select(select, object$.component, "select")
  if (!include_total) rows <- rows[!object$.component[rows] %in% totals]
  if (!length(rows)) stop("No components remain after removing totals.", call. = FALSE)
  column <- function(name) {
    if (name %in% names(object)) object[[name]] else rep(NA_real_, nrow(object))
  }
  if (type == "proportion") {
    if (!length(prop_columns)) stop("Proportions require ",
      "variance_comp(..., type = \"terms\").", call. = FALSE)
    if (is.logical(predictor)) stop("'predictor' must contain total labels or ",
      "positive integer positions.", call. = FALSE)
    predictors <- variance_comp_plot_select(predictor, totals, "predictor")
    groups <- attr(object, "term_groups")
    pieces <- lapply(predictors, function(k) {
      suffix <- sub(".proportion", "", prop_columns[k], fixed = TRUE)
      estimate <- column(prop_columns[k])
      lower <- column(paste0(".proportion_lower_ci", suffix))
      upper <- column(paste0(".proportion_upper_ci", suffix))
      if (!is.null(groups[[totals[k]]])) {
        eligible <- object$.component %in% c(groups[[totals[k]]], totals[k])
      } else {
        # Older results may not have term membership metadata. Retain rows
        # with defined estimates or bounds, or all terms for an undefined panel.
        eligible <- is.finite(estimate) | is.finite(lower) | is.finite(upper)
        if (!any(eligible)) eligible <- !object$.component %in% setdiff(totals, totals[k])
      }
      i <- rows[eligible[rows]]
      tibble::tibble(.row = as.character(i), .component = object$.component[i],
        .estimate = estimate[i], .lower = lower[i], .upper = upper[i],
        .predictor = paste("Linear predictor", k))
    })
    data <- do.call(rbind, pieces)
    if (!nrow(data)) stop("No selected components belong to the requested ",
      "linear predictors.", call. = FALSE)
    data$.predictor <- factor(data$.predictor,
      levels = paste("Linear predictor", predictors))
    ylab <- "Proportion of total variance"
  } else {
    if (!is.null(predictor)) stop("'predictor' is only used with ",
      "'type = \"proportion\"'.", call. = FALSE)
    estimate_column <- if (type == "variance") ".variance" else ".std_dev"
    if (!estimate_column %in% names(object)) stop("The result has no ",
      estimate_column, " estimates.", call. = FALSE)
    lower <- column(".lower_ci")
    upper <- column(".upper_ci")
    if (type == "variance") {
      lower <- lower^2
      upper <- upper^2
    }
    data <- tibble::tibble(.row = as.character(rows),
      .component = object$.component[rows], .estimate = object[[estimate_column]][rows],
      .lower = lower[rows], .upper = upper[rows])
    ylab <- if (type == "variance") "Variance" else "Standard deviation"
  }
  row_levels <- if (flip) rev(as.character(rows)) else as.character(rows)
  data$.row <- factor(data$.row, levels = row_levels)
  labels <- stats::setNames(object$.component[rows], as.character(rows))
  intervals <- data[is.finite(data$.lower) & is.finite(data$.upper), , drop = FALSE]
  points <- data[is.finite(data$.estimate), , drop = FALSE]
  plt <- gratia_plot(data, ggplot2::aes(x = .data$.row, y = .data$.estimate)) +
    ggplot2::geom_blank(ggplot2::aes(x = .data$.row), inherit.aes = FALSE) +
    gratia_geom("interval", ggplot2::geom_linerange(data = intervals,
      ggplot2::aes(ymin = .data$.lower, ymax = .data$.upper), orientation = "x")) +
    gratia_geom("effect", ggplot2::geom_point(data = points)) +
    ggplot2::scale_x_discrete(labels = labels,
      guide = ggplot2::guide_axis(angle = angle, n.dodge = n_dodge)) +
    ggplot2::labs(x = NULL, y = ylab)
  if (type == "proportion" && length(unique(data$.predictor)) > 1L) {
    plt <- plt + ggplot2::facet_wrap(ggplot2::vars(.data$.predictor),
      scales = if (flip) "free_y" else "free_x", ncol = ncol, nrow = nrow)
  }
  if (flip) plt <- plt + ggplot2::coord_flip()
  plt
}

variance_comp_plot_select <- function(select, labels, argument) {
  if (is.null(select)) select <- seq_along(labels)
  if (is.character(select)) {
    if (anyDuplicated(labels[labels %in% select])) stop("Names in '", argument,
      "' are ambiguous; use positive integer positions.", call. = FALSE)
    index <- match(select, labels)
    if (anyNA(index)) stop("Unknown name in '", argument, "': ",
      paste(select[is.na(index)], collapse = ", "), call. = FALSE)
  } else if (is.logical(select) && length(select) == length(labels) && !anyNA(select)) {
    index <- which(select)
  } else if (is.numeric(select) && all(is.finite(select)) &&
      all(select == floor(select) & select >= 1 & select <= length(labels))) {
    index <- as.integer(select)
  } else stop("'", argument, "' must contain names, positive integer positions, ",
    "or one non-missing logical entry per component.", call. = FALSE)
  if (!length(index) || anyDuplicated(index)) stop("'", argument,
    "' must choose at least one entry without duplicates.", call. = FALSE)
  index
}
