#' Plot estimated parametric effects
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Plots estimated univariate and bivariate smooths using ggplot2.
#'
#' @param object an object, the result of a call to
#'   [evaluate_parametric_term()].
#' @param rug For `evaluate_parametric_terms()`, a logical to
#'   indicate if a rug plot should be drawn.
#' @param ci_level numeric between 0 and 1; the coverage of credible interval.
#' @param constant numeric; a constant to add to the estimated values of the
#'   smooth. `constant`, if supplied, will be added to the estimated value
#'   before the confidence band is computed.
#' @param fun function; a function that will be applied to the estimated values
#'   and confidence interval before plotting. Can be a function or the name of a
#'   function. Function `fun` will be applied after adding any `constant`, if
#'   provided.
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
#' @param response_range numeric; a vector of two values giving the range of
#'   response data for the guide. Used to fix plots to a common scale/range.
#'   Ignored if `show` is set to `"se"`.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param ... arguments passed to other methods.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @author Gavin L. Simpson
#'
#' @importFrom ggplot2 ggplot aes labs geom_line geom_ribbon geom_pointrange
#'   geom_rug expand_limits
#' @importFrom rlang .data
#' @importFrom grid unit
#'
#' @export
#' @name draw.evaluated_parametric_term
`draw.evaluated_parametric_term` <- function(object,
                                             ci_level = 0.95,
                                             constant = NULL,
                                             fun = NULL,
                                             xlab, ylab,
                                             title = NULL, subtitle = NULL,
                                             caption = NULL,
                                             rug = TRUE,
                                             position = "identity",
                                             response_range = NULL,
                                             ...) {
  is_fac <- object[["type"]][1L] == "factor"
  term_label <- object[["term"]][1L]

  ## If constant supplied apply it to `est`
  object <- add_constant(object, constant = constant)

  ## add a CI
  crit <- qnorm((1 - ci_level) / 2, lower.tail = FALSE)
  object <- mutate(object,
    lower = .data$partial - (crit * .data$se),
    upper = .data$partial + (crit * .data$se)
  )

  ## If fun supplied, use it to transform est and the upper and lower interval
  object <- transform_fun(object, fun = fun)

  plt <- ggplot(object, aes(x = .data$value, y = .data$partial))

  if (is_fac) {
    plt <- plt + geom_pointrange(aes(
      ymin = .data$lower,
      ymax = .data$upper
    ))
  } else {
    if (isTRUE(rug)) {
      plt <- plt + geom_rug(sides = "b", position = position, alpha = 0.5)
    }
    plt <- plt + geom_ribbon(
      aes(
        ymin = .data$lower,
        ymax = .data$upper
      ),
      alpha = 0.3
    ) +
      geom_line()
  }

  ## default axis labels if none supplied
  if (missing(xlab)) {
    xlab <- term_label
  }
  if (missing(ylab)) {
    ylab <- sprintf("Partial effect of %s", term_label)
  }

  ## add labelling to plot
  plt <- plt + labs(
    x = xlab, y = ylab, title = title, subtitle = subtitle,
    caption = caption
  )

  ## fixing the y axis limits?
  if (!is.null(response_range)) {
    plt <- plt + expand_limits(y = response_range)
  }

  plt
}
