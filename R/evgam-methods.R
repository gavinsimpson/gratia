#' Plot smooths of a distribution extreme value GAM estimated by `evgam::evgam`
#'
#' Provides a [gratia::draw()] method for GAMLSS (distributional GAMs) fitted
#' by [evgam::evgam()].
#'
#' @param object a model, fitted by [GJRM::gamlss()]
#' @param ... arguments passed to [gratia::draw.gam()]
#'
#' @inheritParams draw.gam
#'
#' @importFrom purrr map_dbl map
#' @importFrom patchwork plot_layout
#' @export
#'
#' @note Plots of smooths are not labelled with the linear predictor to which
#'   they belong.
#'
#' @examples
#' if (require("evgam", quietly = TRUE)) {
#'     # follow example from ?evgam::evgam
#'     load_mgcv()
#'     suppressPackageStartupMessages(library("evgam"))
#'     data(fremantle, package = "evgam")
#'     fmla_gev <- list(SeaLevel ~ s(Year, k = 5, bs = "cr"), ~1, ~1)
#'     m_gev <- evgam(fmla_gev, fremantle, family = "gev")
#'
#'     # draw(m_gev)
#' }
`draw.evgam` <- function(object,
    scales = c("free", "fixed"),
    ncol = NULL, nrow = NULL, guides = "keep",
    widths = NULL, heights = NULL, ...) {

}

get_smooths_by_id.gamlist <- function(object, id) {
    get_smooths_by_id.gam(object, id)
}