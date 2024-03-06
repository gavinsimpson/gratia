# Utilities involved in draw methods

#' Reorder tensor product terms for nicer plotting
#'
#' If a tensor product smooth of 3 or more terms contains a 2d marginal smooth,
#' we will get nicer output from [smooth_estimates()] and hence a nicer plot
#' from the [draw.smooth_estimates()] method if we reorder the terms of the
#' smooth such that we vary the terms in the 2d marginal first, and any other
#' terms vary more slowly when we generate data to evaluate the smooth at. This
#' results in automatically generated data that focuses on the (or the first if
#' more than one) 2d marginal smooth, with the end result that
#' [smooth_estimates()] shows how that 2d smooth changes with the other terms
#' involved in the smooth.
#'
#' @param smooth an mgcv smooth object
#'
#' @keywords internal
`reorder_tensor_smooth_terms` <- function(smooth) {
  # ensure we're working with an mgcv smooth
  check_is_mgcv_smooth(smooth)
  # take the order as specified in smooth$term as we return this if we can't
  # improve the order
  var_order <- smooth_variable(smooth)
  # can we improve the order of terms to be plotted? We can if there is one
  # or more 2d marginal smooth in the tensor smooth?
  n_margin <- length(smooth$margin)
  sm_dim <- smooth_dim(smooth)
  # clause triggers for any m-d marginal: doesn't mean we have a 2d smooth
  # but we focus on 2d terms because of if on n_2d_margins
  # this could be improved for a 3d marginal in a 4d tensor - we could take
  # the first 2 terms in that 3d marginal and then reorder the 3rd term in
  # the marginal plus the 4th term in the tensor after the first two
  if (n_margin < sm_dim) {
    m_dims <- vapply(smooth$margin, \(x) x$dim, FUN.VALUE = integer(1))
    are_2d <- which(m_dims == 2L)
    n_2d_margins <- length(are_2d)
    m_terms <- lapply(smooth$margin, \(x) x$term)
    take <- seq_along(m_terms)
    if (n_2d_margins > 0) {
      take <- c(take[are_2d[1]], take[take != are_2d[1]])
    }
    var_order <- unlist(m_terms[take])
  }
  var_order # return
}

#' Reorder random factor smooth terms to place factor last
#'
#' @param smooth an mgcv smooth object
#'
#' @keywords internal
`reorder_fs_smooth_terms` <- function(smooth) {
  # ensure we're working with an mgcv smooth
  check_is_mgcv_smooth(smooth)
  # take the order as specified in smooth$term as we return this if we can't
  # improve the order
  var_order <- smooth_variable(smooth)
  # which term is the factor - info in smooth$base$term is the vars in the
  # smooth which will be continuous vars. The omitted term is the factor
  base_vars <- smooth$base$term
  f_var <- setdiff(var_order, base_vars)
  # reorder so  f_var is after base_vars
  var_order <- c(base_vars, f_var)
  var_order # return
}

# get limits from plot objects #################################################

#' @importFrom purrr map
`get_xlim_from_plots` <- function(plts) {
  map(plts, get_xlim_from_plot)
}

#' @importFrom ggplot2 layer_scales
`get_xlim_from_plot` <- function(p) {
  f <- layer_scales(p)[["x"]][["get_limits"]]
  f()
}

#' @importFrom purrr map
`get_ylim_from_plots` <- function(plts) {
  map(plts, get_ylim_from_plot)
}

#' @importFrom ggplot2 layer_scales
`get_ylim_from_plot` <- function(p) {
  f <- layer_scales(p)[["y"]][["get_limits"]]
  f()
}
