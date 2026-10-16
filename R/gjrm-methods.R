#' Plot smooths of a GAMLSS model estimated by `GJRM::gamlss`
#'
#' Provides a [gratia::draw()] method for GAMLSS (distributional GAMs) fitted
#' by [GJRM::gamlss()].
#'
#' @inheritParams smooth_estimates
#'
#' @param object a model, fitted by [GJRM::gamlss()]
#' @param ... arguments passed to [gratia::draw.gam()]
#' @param select Smooth selection across all equations, using the labels from
#'   [smooths()], or numeric or logical indices in equation and smooth order.
#'   Repeated labels receive suffixes such as `s.1(x)`; existing distinct labels
#'   are retained. `partial_match` uses these model-wide labels.
#' @param wrap Logical; return an assembled patchwork when `TRUE` (the default),
#'   or a flat named list of plots when `FALSE`.
#'
#' @inheritParams draw.gam
#'
#' @importFrom patchwork wrap_plots
#' @export
#'
#' @note For models with multiple linear predictors, smooth plot subtitles
#'   identify the distributional parameter using the parent model's margin and
#'   equation order, rather than the working family of the embedded GAMs.
#'   Single-predictor models omit these automatic labels. Existing smooth labels,
#'   selection, by-variable information and custom subtitles are retained.
#'   Pass `surface = "3d", ci_3d = TRUE` through `...` to add interval surfaces
#'   to smooths and numeric parametric interactions in any equation.
#'   `ci_3d_alpha` and `ci_3d_col` control their opacity and fill colour.
#'
#' @examples
#' if (suppressPackageStartupMessages(require("GJRM", quietly = TRUE))) {
#'   # follow example from ?GJRM::gamlss
#'   load_mgcv()
#'   suppressPackageStartupMessages(library("GJRM"))
#'   set.seed(0)
#'   n <- 100
#'   x1 <- round(runif(n))
#'   x2 <- runif(n)
#'   x3 <- runif(n)
#'   f1 <- function(x) cos(pi * 2 * x) + sin(pi * x)
#'   y1 <- -1.55 + 2 * x1 + f1(x2) + rnorm(n)
#'   dataSim <- data.frame(y1, x1, x2, x3)
#'
#'   eq_mu <- y1 ~ x1 + s(x2)
#'   eq_s <- ~ s(x3, k = 6)
#'   fl <- list(eq_mu, eq_s)
#'   m <- gamlss(fl, data = dataSim)
#'
#'   draw(m)
#'   if (requireNamespace("ggcube", quietly = TRUE)) {
#'     surface <- GJRM::gamlss(list(y1 ~ s(x2, x3, k = 10), ~ s(x3, k = 6)),
#'       data = dataSim)
#'     draw(surface, surface = "3d", ci_3d = TRUE, n_2d = 15)
#'   }
#' }
`draw.gamlss` <- function(object,
                          scales = c("free", "fixed"),
                          ncol = NULL, nrow = NULL, guides = "keep",
                          widths = NULL, heights = NULL, ...,
                          n = 100, envir = NULL, select = NULL,
                          parametric = NULL, terms = NULL,
                          partial_match = FALSE, wrap = TRUE) {
  scales <- match.arg(scales)
  components <- gjrm_gamlss_components(object)
  labels <- gjrm_gamlss_smooth_labels(components)
  if (anyNA(select) || (is.numeric(select) &&
      any(!is.finite(select) | select < 1 | select != floor(select)))) {
    stop("Smooth indices in 'select' must be positive integers without missing values.")
  }
  selected <- check_user_select_smooths(labels, select,
    partial_match = partial_match)
  if (is.null(parametric)) parametric <- is.null(select)
  if (isTRUE(parametric) && !is.null(terms) && !any(terms %in% unlist(lapply(components,
      parametric_terms), use.names = FALSE))) {
    stop("No parametric terms in 'terms' were found in the model.")
  }

  # Resolve selection once across the model, then use local positions in each
  # equation. An unselected equation can still supply requested parametric terms.
  plts <- list()
  start <- 0L
  for (m in components) {
    count <- length(m$smooth)
    index <- start + seq_len(count)
    local_select <- selected[index]
    local_terms <- if (is.null(terms)) NULL else intersect(terms, parametric_terms(m))
    local_parametric <- isTRUE(parametric) &&
      (is.null(terms) || length(local_terms) > 0L)
    if (any(local_select) || local_parametric) {
      panels <- draw(m, select = local_select, parametric = local_parametric,
        terms = local_terms, partial_match = FALSE, n = n, envir = envir,
        wrap = FALSE, ...)
      # Preserve familiar titles; names expose the unambiguous selection keys.
      panel_names <- names(panels)
      for (i in which(local_select)) {
        panel_names[panel_names == m$smooth[[i]]$label] <- labels[index[i]]
      }
      names(panels) <- panel_names
      plts <- c(plts, panels)
    }
    start <- start + count
  }
  if (!length(plts)) return(invisible(NULL))

  if (identical(scales, "fixed")) {
    is_3d <- vapply(plts, function(p) inherits(p$coordinates, "Coord3D"), logical(1))
    if (any(is_3d)) {
      ranges <- lapply(seq_along(plts), function(i) {
        if (is_3d[i]) {
          ggplot2::ggplot_build(plts[[i]])$plot$scales$get_scales("z")$get_limits()
        } else if (!inherits(plts[[i]]$coordinates, "CoordSf")) {
          get_ylim_from_plot(plts[[i]])
        }
      })
      effect_limits <- range(unlist(ranges), finite = TRUE)
      plts <- lapply(seq_along(plts), function(i) {
        p <- plts[[i]]
        if (is_3d[i]) {
          p + ggplot2::expand_limits(z = effect_limits)
        } else if (!inherits(p$coordinates, "CoordSf")) {
          p + ggplot2::expand_limits(y = effect_limits)
        } else p
      })
    } else {
      y_lim <- get_ylim_from_plots(plts)
      y_lim <- exec("range", !!!y_lim)
      plts <- lapply(plts, \(p) p + lims(y = y_lim))
    }
  }
  if (!wrap) return(plts)

  # Count all panels, including parametric effects, before choosing the layout.
  layout <- prepare_plot_layout(length(plts), ncol = ncol, nrow = nrow)
  if (length(plts) > 1L && is.null(widths)) widths <- 1
  wrap_plots(plts, byrow = TRUE, nrow = layout$nrow, ncol = layout$ncol,
    guides = guides, heights = heights, widths = widths)
}

# Prepare embedded GAMs with the final coefficients and covariance, in equation
# order. Their working families are retained; parent labels identify parameters.
gjrm_gamlss_components <- function(object) {
  model_names <- paste0("gam", 1:9)
  eta <- which(vapply(object[model_names], inherits, logical(1), "gam"))
  lapply(eta, function(i) modify_gamlss_gam(object, model_names[i],
    gjrm_gamlss_ind(object, i)))
}

gjrm_gamlss_smooth_labels <- function(components) {
  original <- as.character(unlist(lapply(components, smooths), use.names = FALSE))
  labels <- original
  equations <- rep(seq_along(components), lengths(lapply(components, smooths)))
  for (i in which(duplicated(original))) {
    suffix <- max(1L, equations[i] - 1L)
    repeat {
      candidate <- if (grepl("(", original[i], fixed = TRUE)) {
        sub("(", paste0(".", suffix, "("), original[i], fixed = TRUE)
      } else paste0(original[i], ".", suffix)
      if (!candidate %in% c(original, labels[seq_len(i - 1L)])) break
      suffix <- suffix + 1L
    }
    labels[i] <- candidate
  }
  labels
}

#' @export
#' @name smooths
#' @rdname smooths
#' @details For GJRM GAMLSS models, labels follow equation and smooth order.
#'   Labels repeated in later equations receive suffixes such as `s.1(x)`.
#'   These labels can be supplied to [draw()] to select smooths across equations.
smooths.gamlss <- function(object, ...) {
  gjrm_gamlss_smooth_labels(gjrm_gamlss_components(object))
}

`gjrm_gamlss_ind` <- function(object, eta) {
  # return the indices of terms from the gamlss needed depending on which
  # linear predictor indicated by eta

  # check eta is numeric
  eta <- as.integer(eta)

  # some constants
  is_ordcon <- !is.null(object$VC$K1) & is.null(object$VC$K2)
  is_ordord <- !is.null(object$VC$K1) & !is.null(object$VC$K2)
  if (is_ordcon) {
    shift1 <- object$VC$K1 - 2
    shift2 <- shift3 <- shift1 + 1
  } else if (is_ordord) {
    shift1 <- shift2 <- object$VC$K1 + object$VC$K2 - 3
    shift3 <- shift1 + 1
  } else {
    shift1 <- shift2 <- shift3 <- 0
  }
  # create indices
  ind <- switch(eta,
    eta1 = with(object, (shift1 + 1):(X1.d2 + shift3)),
    eta2 = with(object, (X1.d2 + 1 + shift2):(X1.d2 + X2.d2 + shift3)),
    eta3 = with(object, (X1.d2 + X2.d2 + 1 + shift3):(X1.d2 + X2.d2 +
      X3.d2 + shift3)),
    eta4 = with(object, (X1.d2 + X2.d2 + X3.d2 + 1 + shift3):(X1.d2 +
      X2.d2 + X3.d2 + X4.d2 + shift3)),
    eta5 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + 1 +
      shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + shift3)),
    eta6 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + 1 +
      shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 + shift3)),
    eta7 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 + 1 +
      shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 + X7.d2 +
      shift3)),
    eta8 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 +
      X7.d2 + 1 + shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 +
      X7.d2 + X8.d2 + shift3)),
    eta9 = with(object, (X1.d2 + X2.d2 + X3.d2 + X4.d2 + X5.d2 + X6.d2 +
      X7.d2 + X8.d2 + 1 + shift3):(X1.d2 + X2.d2 + X3.d2 + X4.d2 +
      X5.d2 + X6.d2 + X7.d2 + X8.d2 + X9.d2 + shift3))
  )
  ind
}

# i are the indices of the parameters etc
# object is the gamlss object
# model is the name "gamX" with X and integer 1:9
`modify_gamlss_gam` <- function(object, model, i) {
  # extract the GAM
  gam_obj <- object[[model]]

  # modify the GAM
  gam_obj$coefficients <- object$coefficients[i]
  gam_obj$Vp <- object$Vb[i, i]
  gam_obj$Vp.t <- object$Vb.t[i, i]
  gam_obj$sig2 <- 1
  gam_obj$edf <- diag(object$F)[i]
  gam_obj$scale.estimated <- FALSE
  gam_obj$call$data <- object$call$data
  attr(gam_obj, "gratia.n_predictors") <- sum(vapply(
    object[paste0("gam", 1:9)], inherits, logical(1), "gam"))
  attr(gam_obj, "gratia.model_label") <- gjrm_predictor_label(object,
    as.integer(sub("gam", "", model, fixed = TRUE)))

  # return the GAM
  gam_obj
}
