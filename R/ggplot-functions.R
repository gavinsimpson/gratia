# Functions to make building plots easier with ggplot, including scales and
# geoms

#' Default diverging red-blue colour palette for partial effects
#'
#' @param name The name of the scale. Used as the legend title. If `NULL` the
#'   legend title will be omitted.
#' @param na.value Missing values will be replaced with this value (colour).
#' @param guide A function used to create a guide or its name. See
#'   [ggplot2::guides()] for more information.
#' @param direction Sets the order of colours in the scale. If `1`, the
#'   default, colours are as output by [RColorBrewer::brewer.pal()]. If `-1`,
#'   the order of colours is reversed.
#' @param ... arguments passed to [ggplot2::continuous_scale()].
#'
#' @export
#' @importFrom scales pal_gradient_n pal_brewer
#' @importFrom ggplot2 continuous_scale
`scale_fill_partial_effect` <- function(
  name = "Partial effect",
  ...,
  na.value = "grey50",
  guide = "colourbar",
  direction = -1
) {
  cols <- pal_brewer(
    type = "div",
    palette = "RdBu",
    direction = direction
  )
  pal <- pal_gradient_n(
    colours = cols(7), values = NULL, space = "Lab"
  )
  continuous_scale(
    aesthetics = "fill", name = name, palette = pal, na.value = na.value,
    guide = guide, ...
  )
}

# `geom_credible_interval` <- function() {
# 
# }