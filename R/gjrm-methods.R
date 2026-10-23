#' Plot smooths of models estimated by GJRM
#'
#' Provides [gratia::draw()] and [gratia::assemble()] methods for GAMLSS
#' and joint models fitted by [GJRM::gamlss()] or [GJRM::gjrm()]. `assemble()` returns the
#' individual plots; `draw()` arranges them using [patchwork::wrap_plots()].
#'
#' @inheritParams smooth_estimates
#'
#' @param object a model fitted by [GJRM::gamlss()] or [GJRM::gjrm()].
#' @param ... arguments passed to [gratia::assemble.gam()]
#' @param select Smooth selection across all equations, using the labels from
#'   [smooths()], or numeric or logical indices in equation and smooth order.
#'   Repeated labels receive suffixes such as `s.1(x)`; existing distinct labels
#'   are retained. `partial_match` uses these model-wide labels.
#' @param wrap Logical; return an assembled patchwork when `TRUE` (the default),
#'   or a flat named list of plots when `FALSE`.
#'
#' @inheritParams draw.gam
#'
#' @return `assemble()` and `draw(wrap = FALSE)` return a flat named list of
#'   plots, ordered by equation and then by term within each equation. Smooth
#'   names use the model-wide labels returned by [smooths()]. `draw()` returns
#'   a patchwork by default. Both methods return invisible `NULL` when there
#'   are no plots to draw.
#'
#' @importFrom patchwork wrap_plots
#' @export
#'
#' @note For models with multiple linear predictors, smooth and parametric
#'   plot subtitles identify the distributional parameter using the parent
#'   model's margin and equation order, rather than the working family of
#'   the embedded GAMs.
#'   Univariate GAMLSS models with one predictor omit these automatic labels.
#'   Joint models retain equation labels even when only dependence is estimated.
#'   Existing smooth labels, selection, by-variable information and custom
#'   subtitles are retained.
#'   Pass `surface = "3d", ci_3d = TRUE` through `...` to add interval surfaces
#'   to smooths and numeric parametric interactions in any equation.
#'   `ci_3d_alpha` and `ci_3d_col` control their opacity and fill colour.
#'   Pass `cyclic = "polar"` and optional `polar_args` through `...` for
#'   polar plots of supported one-dimensional cyclic smooths in any equation;
#'   see [draw.gam()] for the coordinate options.
#'
#'   Joint models use the final joint coefficients and covariance for every
#'   embedded GAM, including dependence equations and random-effect smooths.
#'   Parametric plot subtitles also identify their response and parameter.
#'   For both univariate and joint models, a parametric term repeated across
#'   equations has list names such as `x (equation 1)` and `x (equation 2)`;
#'   unique terms keep their original names.
#'   Grouped factor-by panels also receive an equation suffix when their
#'   base name would hide another smooth's selection label.
#'   Ordinal cutpoints are excluded from smooth and intercept uncertainty.
#'   Fixed-margin fits plot their estimated dependence equation only.
#'   Flexible survival equations fitted using SCAM are not yet supported.
#'   Working residuals from preliminary GAM fits are unavailable for joint
#'   models; requesting partial residuals warns and draws the smooths alone.
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
#'   plots <- assemble(m, parametric = FALSE)
#'   names(plots)
#'   plots[[1]]
#'   if (requireNamespace("ggcube", quietly = TRUE)) {
#'     surface <- GJRM::gamlss(list(y1 ~ s(x2, x3, k = 10), ~ s(x3, k = 6)),
#'       data = dataSim)
#'     draw(surface, surface = "3d", ci_3d = TRUE, n_2d = 15)
#'   }
#' }
#'
#' if (requireNamespace("GJRM", quietly = TRUE)) {
#'   load_mgcv()
#'   set.seed(12)
#'   d <- data.frame(x = runif(150), z = runif(150))
#'   d$y1 <- sin(2 * pi * d$x) + rnorm(150)
#'   d$y2 <- cos(2 * pi * d$z) + 0.3 * d$y1 + rnorm(150)
#'   joint <- GJRM::gjrm(list(y1 ~ s(x, k = 5), y2 ~ s(z, k = 5),
#'     ~ 1, ~ 1, ~ s(x, k = 4)), data = d, margins = c("N", "N"),
#'     model = "B")
#'   draw(joint, parametric = FALSE)
#'   smooth_estimates(joint, select = smooths(joint)[3])
#' }
`draw.gamlss` <- function(object,
                          scales = c("free", "fixed"),
                          ncol = NULL, nrow = NULL, guides = "keep",
                          widths = NULL, heights = NULL, ...,
                          n = 100, envir = NULL, select = NULL,
                          parametric = NULL, terms = NULL,
                          partial_match = FALSE, wrap = TRUE) {
  plts <- assemble.gamlss(object, scales = scales, ..., n = n, envir = envir,
    select = select, parametric = parametric, terms = terms,
    partial_match = partial_match)
  if (!length(plts)) return(invisible(NULL))
  if (!wrap) return(plts)

  # Count all panels, including parametric effects, before choosing the layout.
  layout <- prepare_plot_layout(length(plts), ncol = ncol, nrow = nrow)
  if (length(plts) > 1L && is.null(widths)) widths <- 1
  wrap_plots(plts, byrow = TRUE, nrow = layout$nrow, ncol = layout$ncol,
    guides = guides, heights = heights, widths = widths)
}

#' @rdname draw.gamlss
#' @export
assemble.gamlss <- function(object, scales = c("free", "fixed"), ..., residuals = FALSE,
                            n = 100, envir = NULL, select = NULL,
                            parametric = NULL, terms = NULL,
                            partial_match = FALSE,
                            grouped_by = deprecated(), grouped = c("sz", "fs"),
                            show_ci = c("factor-by", "sz")) {
  factor_controls <- resolve_factor_plot_controls(grouped, show_ci, grouped_by,
    "assemble.gamlss")
  scales <- match.arg(scales)
  joint <- !isTRUE(object$univar.gamlss)
  components <- gjrm_gamlss_components(object)
  labels <- gjrm_gamlss_smooth_labels(components)
  parametric_labels <- unlist(lapply(components, parametric_terms), use.names = FALSE)
  repeated_parametric <- unique(parametric_labels[duplicated(parametric_labels)])
  if (anyNA(select) || (is.numeric(select) &&
      any(!is.finite(select) | select < 1 | select != floor(select)))) {
    stop("Smooth indices in 'select' must be positive integers without missing values.")
  }
  selected <- check_user_select_smooths(labels, select,
    partial_match = partial_match)
  if (is.null(parametric)) parametric <- is.null(select)
  if (joint && isTRUE(residuals) && any(selected)) {
    first <- which(vapply(components, function(m) length(m$smooth) > 0L, logical(1)))[1L]
    check_partial_residuals(components[[first]], warn = TRUE)
    residuals <- FALSE
  }
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
    available_terms <- parametric_terms(m)
    local_terms <- if (is.null(terms)) NULL else intersect(terms, available_terms)
    local_parametric <- isTRUE(parametric) && length(available_terms) > 0L &&
      (is.null(terms) || length(local_terms) > 0L)
    if (any(local_select) || local_parametric) {
      panels <- assemble(m, select = local_select, parametric = local_parametric,
        terms = local_terms, residuals = residuals, partial_match = FALSE,
        n = n, envir = envir, grouped = factor_controls$grouped,
        show_ci = factor_controls$show_ci,
        ...)
      # Preserve familiar titles; names expose the unambiguous selection keys.
      panel_names <- names(panels)
      local_labels <- vapply(m$smooth, smooth_label, character(1))
      for (j in seq_along(panels)) {
        plotted <- unique(as.character(panels[[j]]$data[[".smooth"]]))
        positions <- match(plotted, local_labels)
        positions <- positions[!is.na(positions)]
        if (length(positions)) {
          global <- labels[index[positions]]
          if (length(global) > 1L) {
            global <- unique(vapply(global, factor_plot_term, character(1),
              by = m$smooth[[positions[1L]]]$by))
            if (length(global) == 1L && global %in% labels) {
              global <- paste0(global, " (equation ",
                attr(m, "gratia.gjrm_equation", exact = TRUE), ")")
            }
          }
          if (length(global) == 1L) {
            suffix <- if (length(positions) == 1L &&
                startsWith(panel_names[j], local_labels[positions])) {
              substring(panel_names[j], nchar(local_labels[positions]) + 1L)
            } else ""
            panel_names[j] <- paste0(global, suffix)
          }
        }
      }
      parameter_panels <- which(names(panels) %in% parametric_terms(m) &
        !vapply(panels, function(p) ".smooth" %in% names(p$data), logical(1)))
      for (j in parameter_panels) {
        if (names(panels)[j] %in% repeated_parametric) {
          panel_names[j] <- paste0(names(panels)[j], " (equation ",
            attr(m, "gratia.gjrm_equation", exact = TRUE), ")")
        }
      }
      names(panels) <- panel_names
      plts <- c(plts, panels)
    }
    start <- start + count
  }
  if (!length(plts)) return(invisible(NULL))

  if (identical(scales, "fixed")) plts <- gjrm_fixed_effect_scales(plts)
  plts
}

#' @rdname draw.gamlss
#' @export
draw.gjrm <- draw.gamlss

#' @rdname draw.gamlss
#' @export
assemble.gjrm <- assemble.gamlss


# A surface's y axis is a covariate. Share effect scales without changing it.
gjrm_fixed_effect_scales <- function(plots) {
  built <- lapply(plots, ggplot2::ggplot_build)
  effects <- lapply(built, function(b) {
    p <- b$plot
    axes <- if (inherits(p$coordinates, "Coord3D")) "z" else character()
    for (aesthetic in c("fill", "colour")) {
      scale <- p$scales$get_scales(aesthetic)
      if (is.null(scale) || scale$is_discrete()) next
      mappings <- c(list(p$mapping), lapply(p$layers, function(layer) layer$mapping))
      maps_effect <- any(vapply(mappings, function(mapping) {
        value <- mapping[[aesthetic]]
        !is.null(value) && grepl("[.]estimate|[.]partial|[.]se", rlang::as_label(value))
      }, logical(1)))
      if (maps_effect) axes <- c(axes, aesthetic)
    }
    if (!length(axes) && !inherits(p$coordinates, "CoordSf")) axes <- "y"
    axes
  })
  raw_limits <- function(scale) {
    scale$get_transformation()$inverse(scale$get_limits())
  }
  ranges <- Map(function(b, axes) lapply(axes, function(aesthetic) {
    if (aesthetic == "y") lapply(b$layout$panel_scales_y, raw_limits) else {
      raw_limits(b$plot$scales$get_scales(aesthetic))
    }
  }), built, effects)
  limits <- unlist(ranges, use.names = FALSE)
  limits <- limits[is.finite(limits)]
  if (!length(limits)) return(plots)
  limits <- range(limits)
  Map(function(p, b, axes) {
    if (!length(axes)) return(p)
    p$scales <- p$scales$clone()
    for (aesthetic in axes) {
      scale <- p$scales$get_scales(aesthetic)
      if (is.null(scale)) {
        scale <- b$plot$scales$get_scales(aesthetic)$clone()
        p$scales$add(scale)
      }
      scale$limits <- scale$transform(limits)
    }
    p
  }, plots, built, effects)
}

# Prepare embedded GAMs with the final coefficients and covariance, in equation
# order. Their working families are retained; parent labels identify parameters.
gjrm_gamlss_components <- function(object) {
  model_names <- gjrm_component_names(object)
  lapply(model_names, function(nm) {
    eta <- as.integer(sub("gam", "", nm, fixed = TRUE))
    modify_gamlss_gam(object, nm, gjrm_gamlss_ind(object, eta))
  })
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
#' @details For GJRM models, labels follow equation and smooth order.
#'   Labels repeated in later equations receive suffixes such as `s.1(x)`.
#'   These labels can be supplied to [draw()] or [smooth_estimates()] to select
#'   smooths across equations.
smooths.gamlss <- function(object, ...) {
  gjrm_gamlss_smooth_labels(gjrm_gamlss_components(object))
}


#' @export
#' @rdname smooths
smooths.gjrm <- smooths.gamlss

#' @export
#' @rdname n_smooths
n_smooths.gjrm <- function(object) length(smooths(object))

# Never silently omit an equation whose basis requires a different adapter.
gjrm_component_names <- function(object) {
  model_names <- paste0("gam", 1:9)
  components <- object[model_names]
  if (any(vapply(components, inherits, logical(1), "scam"))) {
    stop("GJRM flexible survival equations fitted using SCAM are not yet supported.",
      call. = FALSE)
  }
  found <- vapply(components, inherits, logical(1), "gam")
  if (isTRUE(object$fixed.margins) || isTRUE(object$VC$fixed.margins)) {
    found[1:4] <- FALSE
  }
  model_names[found]
}

# A zero position represents the absent intercept of an ordinal equation.
# Its coefficient and covariance are restored as zero in the prepared GAM.
gjrm_gamlss_ind <- function(object, eta) {
  if (length(eta) != 1L || is.na(eta) || !eta %in% 1:9) {
    stop("Select a GJRM equation from 1 to 9.", call. = FALSE)
  }
  dimensions <- vapply(seq_len(eta), function(j) {
    nm <- paste0("X", j, ".d2")
    value <- object[[nm]] %||% object$VC[[nm]] %||% 0L
    if (length(value) != 1L || !is.finite(value) || value < 0 || value != floor(value)) {
      stop("Cannot align GJRM equation dimensions with its coefficients.", call. = FALSE)
    }
    as.integer(value)
  }, integer(1))
  cutpoints <- if (is.null(object$VC$K1)) 0L else {
    object$VC$K1 - 1L + if (is.null(object$VC$K2)) 0L else object$VC$K2 - 1L
  }
  offset <- sum(utils::head(dimensions, -1L)) + cutpoints
  if (isTRUE(object$fixed.margins) || isTRUE(object$VC$fixed.margins)) offset <- offset - 4L
  index <- offset + seq_len(dimensions[eta])
  ordinal <- (eta == 1L && !is.null(object$VC$K1)) ||
    (eta == 2L && !is.null(object$VC$K2))
  if (ordinal && isTRUE(attr(object[[paste0("gam", eta)]]$pterms, "intercept") == 1L)) {
    index <- c(0L, index)
  }
  as.integer(index)
}

# Preserve the outer fitting data and subset rather than constructor-local names.
gjrm_original_call <- function(object, call) {
  data <- object[["mcd"]] %||% object[["VC"]][["mcd"]]
  call$data <- data %||% object$call$data
  if (!is.null(data)) call$subset <- object[["sbs"]] %||% object[["VC"]][["sbs"]]
  call
}

# Replace preliminary estimates without changing the fitted basis or data.
modify_gamlss_gam <- function(object, model, i) {
  gam_obj <- object[[model]]
  q <- ncol(gam_obj$Vp)
  if (length(q) != 1L || length(i) != q || anyNA(i) ||
      any(i < 0L | i > length(object$coefficients)) ||
      any(!is.finite(object$coefficients[i[i > 0L]]))) {
    stop("Cannot align GJRM equation coefficients with its fitted GAM basis.", call. = FALSE)
  }
  parent_q <- length(object$coefficients)
  covariance <- function(V, name, optional = FALSE) {
    if (is.null(V) && optional) return(NULL)
    if (!is.matrix(V) || !identical(dim(V), c(parent_q, parent_q)) ||
        any(!is.finite(V))) {
      stop("GJRM final ", name, " is unavailable or does not align with its coefficients.",
        call. = FALSE)
    }
    out <- matrix(0, q, q)
    positions <- which(i > 0L)
    out[positions, positions] <- V[i[positions], i[positions], drop = FALSE]
    out
  }
  beta <- numeric(q)
  beta[i > 0L] <- object$coefficients[i[i > 0L]]
  # Original GAM names remain aligned with the basis, including ordinal intercepts.
  beta_names <- colnames(gam_obj$Vp) %||% names(gam_obj$coefficients)
  if (any(i == 0L) && length(beta_names) == q - 1L) {
    beta_names <- c("(Intercept)", beta_names)
  }
  if (length(beta_names) == q) names(beta) <- beta_names
  gam_obj$coefficients <- beta
  gam_obj$Vp <- covariance(object[["Vb"]], "Bayesian covariance")
  gam_obj$Vp.t <- covariance(object[["Vb.t"]], "transformed covariance", optional = TRUE)
  gam_obj$Ve <- covariance(object[["Ve"]], "frequentist covariance")
  # Preliminary smoothing-parameter uncertainty does not describe the joint fit.
  gam_obj$Vc <- NULL
  gam_obj$sig2 <- 1
  if (!is.matrix(object[["F"]]) || !identical(dim(object[["F"]]), c(parent_q, parent_q)) ||
      any(!is.finite(object[["F"]]))) {
    stop("GJRM final effective degrees of freedom do not align with its coefficients.", call. = FALSE)
  }
  edf <- diag(object[["F"]])
  gam_obj$edf <- numeric(q)
  gam_obj$edf[i > 0L] <- edf[i[i > 0L]]
  gam_obj$scale.estimated <- FALSE
  gam_obj$call <- gjrm_original_call(object, gam_obj$call)
  if (!isTRUE(object$univar.gamlss)) {
    gam_obj$residuals <- NULL
    gam_obj$weights <- NULL
  }
  predictors <- length(gjrm_component_names(object))
  # Fixed response distributions still belong to the parent joint model.
  if (!isTRUE(object$univar.gamlss)) predictors <- max(predictors, length(object$margins))
  attr(gam_obj, "gratia.n_predictors") <- predictors
  attr(gam_obj, "gratia.model_label") <- gjrm_predictor_label(object,
    as.integer(sub("gam", "", model, fixed = TRUE)))
  attr(gam_obj, "gratia.gjrm_equation") <- as.integer(sub("gam", "", model, fixed = TRUE))
  gam_obj
}
