#' Concurvity of an estimated GAM
#'
#' @param model a fitted GAM. Currently only objects of class `"gam"` are
#'   supported
#' @param terms currently ignored
#' @param type character;
#' @param pairwise logical; extract pairwise concurvity of model terms?
#' @param ... arguents passed to other methods.
#'
#' @importFrom tibble as_tibble add_column rownames_to_column
#' @importFrom mgcv concurvity
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter bind_rows
#'
#' @export
#'
#' @examples
#' ## simulate data with concurvity...
#' library("tibble")
#' load_mgcv()
#' set.seed(8)
#' n <- 200
#' df <- tibble(
#'   t = sort(runif(n)),
#'   x = gw_f2(t) + rnorm(n) * 3,
#'   y = sin(4 * pi * t) + exp(x / 20) + rnorm(n) * 0.3
#' )
#'
#' ## fit model
#' m <- gam(y ~ s(t, k = 15) + s(x, k = 15), data = df, method = "REML")
#'
#' ## overall concurvity
#' o_conc <- concrvity(m)
#' draw(o_conc)
#'
#' ## pairwise concurvity
#' p_conc <- concrvity(m, pairwise = TRUE)
#' draw(p_conc)
`model_concurvity` <- function(model, ...) {
  UseMethod("model_concurvity")
}

#' @export
#' @rdname model_concurvity
`model_concurvity.gam` <- function(model, terms = everything(),
                                   type = c(
                                     "all", "estimate", "observed",
                                     "worst"
                                   ),
                                   pairwise = FALSE, ...) {
  type <- match.arg(type)
  con <- if (pairwise) {
    pairwise_concurvity(model, terms = terms, type = type)
  } else {
    overall_concurvity(model, terms = terms, type = type)
  }
  con
}

#' @export
#' @rdname model_concurvity
`concrvity` <- function(model, terms = everything(),
                        type = c("all", "estimate", "observed", "worst"),
                        pairwise = FALSE, ...) {
  model_concurvity(
    model = model,
    terms = terms,
    type = type,
    pairwise = pairwise, ...
  )
}

#' @importFrom rlang .data
`pairwise_concurvity` <- function(model, terms = everything(),
                                  type) {
  con <- concurvity(model, full = FALSE)
  type_nms <- names(con)
  nms <- colnames(con[[1]])
  nc <- length(nms)
  wrap_as_data_frame <- function(m) {
    df <- as.data.frame(m)
    rownames_to_column(df, ".term")
  }
  con <- lapply(con, wrap_as_data_frame)
  con <- unname(con)
  con <- bind_rows(con) |>
    as_tibble() |>
    add_column(.type = rep(type_nms, each = nc), .before = 1L)
  if (type != "all") {
    con <- con |> filter(.data[[".type"]] == type)
  }
  con <- con |>
    pivot_longer(-c(".type", ".term"),
      names_to = ".with",
      values_to = ".concurvity"
    )
  class(con) <- c("pairwise_concurvity", "concurvity", class(con))
  con
}

#' @importFrom rlang .data
`overall_concurvity` <- function(model, terms = everything(), type) {
  con <- concurvity(model, full = TRUE)
  con <- as.data.frame(con)
  con <- rownames_to_column(con, ".type")
  con <- as_tibble(con)
  if (type != "all") {
    con <- con |>
      filter(.data[[".type"]] == type)
  }
  con <- con |>
    pivot_longer(-c(".type"),
      names_to = ".term",
      values_to = ".concurvity"
    )
  class(con) <- c("overall_concurvity", "concurvity", class(con))
  con
}

#' Plot concurvity measures
#'
#' @param object An object inheriting from class `"concurvity"`, usually the
#'   result of a call to [model_concurvity()] or its abbreviated form
#'   [concrvity()].
#' @param x_lab character; the label for the x axis.
#' @param y_lab character; the label for the y axis.
#' @param title character; the plot title.
#' @param subtitle character; the plot subtitle.
#' @param caption character; the plot caption
#' @param fill_lab character; the label to use for the fill guide.
#' @param continuous_colour function; continuous colour (fill) scale to use.
#' @param bar_col colour specification for the bar colour.
#' @param bar_fill colour specification for the bar fill
#' @param ... arguments passed to other methods.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile facet_wrap coord_equal labs
#'   scale_fill_viridis_c
`draw.pairwise_concurvity` <- function(object,
                                       title = "Smooth-wise concurvity",
                                       subtitle = NULL, caption = NULL,
                                       x_lab = "Term", y_lab = "With",
                                       fill_lab = "Concurvity",
                                       continuous_colour = NULL, ...) {
  plt <- ggplot(
    object,
    aes(
      x = .data[[".term"]],
      y = .data[[".with"]],
      group = .data[[".type"]]
    )
  ) +
    geom_tile(aes(fill = .data[[".concurvity"]])) +
    facet_wrap(vars(.data[[".type"]])) +
    coord_equal()

  plt <- plt +
    labs(
      x = x_lab, y = y_lab, title = title, subtitle = subtitle,
      caption = caption, fill = fill_lab
    )

  if (is.null(continuous_colour)) {
    continuous_colour <- scale_fill_viridis_c(option = "cividis")
  }
  plt <- plt + continuous_colour

  plt
}

#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs
#' @export
#' @rdname draw.pairwise_concurvity
`draw.overall_concurvity` <- function(object, title = "Overall concurvity",
                                      subtitle = NULL, caption = NULL,
                                      y_lab = "Concurvity",
                                      x_lab = NULL,
                                      bar_col = "steelblue", bar_fill = "steelblue",
                                      ...) {
  plt <- ggplot(object, aes(
    x = .data[[".term"]],
    y = .data[[".concurvity"]],
    group = .data[[".type"]]
  )) +
    geom_col(colour = bar_col, fill = bar_fill) +
    facet_wrap(vars(.data[[".type"]])) +
    labs(
      title = title, subtitle = subtitle, caption = caption,
      x = x_lab, y = y_lab
    )

  plt
}
