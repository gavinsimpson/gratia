#' Compare smooths across models
#'
#' @param model Primary model for comparison.
#' @param ... Additional models to compare smooths against those of `model`.
#' @param smooths `r lifecycle::badge("deprecated")` Use `select` instead.
#' @param select character; select which smooths to compare. The default
#'   (`NULL`) means all smooths in `model` will be compared. Numeric `select`
#'   indexes the smooths in the order they are specified in the formula and
#'   stored in `model`. Character `select` matches the labels for smooths
#'   as shown for example in the output from `summary(object)`. Logical
#'   `select` operates as per numeric `select` in the order that smooths are
#'   stored.
#' @param partial_match logical; should smooths be selected by partial matches
#'   with `select`? If `TRUE`, `select` can only be a single string to match
#'   against.
#'
#' @details
#' Two-variable smooths are evaluated at common locations. Automatic grids span
#' the intersection of the models' observed covariate ranges; factor levels must
#' overlap. Supplied `data` are evaluated as given, without filling gaps.
#' Each selected smooth label must occur exactly once in every model and use
#' the same covariates, including any `by` variable. Numeric and logical `select`
#' refer to the primary model; matching labels are then used in every model.
#'
#' Estimates are contributions of individual smooths to the linear predictor,
#' on its link scale, with the centring constraints used in each fitted model.
#' They exclude the intercept and other model terms. Different fitting data can
#' change the centring, so a surface difference can include a constant shift.
#' It is the difference between these fitted smooth contributions, rather than
#' a difference between fitted responses.
#'
#' The result retains one row per model and smooth, with estimates in the `data`
#' list column. Use [draw.compare_smooths()] for faceted surface comparisons or
#' estimated surface differences. Smooths with more than two covariates can be
#' evaluated but cannot be drawn by this method.
#'
#' @inheritParams smooth_estimates
#'
#' @export
#'
#' @importFrom rlang dots_list
#' @importFrom dplyr group_by
#' @importFrom lifecycle deprecated is_present
#'
#' @examples
#' \dontshow{
#' op <- options(cli.unicode = FALSE, pillar.sigfig = 5)
#' }
#' load_mgcv()
#' dat <- data_sim("eg1", seed = 2)
#'
#' ## models to compare smooths across - artificially create differences
#' m1 <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
#'   data = dat, method = "REML"
#' )
#' m2 <- gam(y ~ s(x0, bs = "ts") + s(x1, bs = "ts") + s(x2, bs = "ts") +
#'   s(x3, bs = "ts"), data = dat, method = "REML")
#'
#' ## build comparisons
#' comp <- compare_smooths(m1, m2)
#' comp
#' ## notice that the result is a nested tibble
#'
#' draw(comp)
#'
#' # Simulate a noisy response over two continuous covariates, x and z
#' surf <- data_sim("eg2", n = 100, seed = 7)
#' # Fit two small bases to the same surface data
#' coarse <- gam(y ~ s(x, z, k = 10), data = surf, method = "REML")
#' fine <- gam(y ~ s(x, z, k = 15), data = surf, method = "REML")
#' # Compare surfaces on shared axes and a shared colour scale
#' surfaces <- compare_smooths(coarse, fine, n_2d = 20)
#' draw(surfaces)
#' # Show coarse minus fine, with a colour scale centred on zero
#' draw(surfaces, show = "difference", pair = c("coarse", "fine"))
#' \dontshow{
#' options(op)
#' }
`compare_smooths` <- function(model, ...,
    select = NULL,
    smooths = deprecated(),
    n = 100,
    n_2d = 50,
    data = NULL,
    unconditional = FALSE,
    overall_uncertainty = TRUE,
    partial_match = FALSE, envir = NULL, dist = NULL) {
  if (lifecycle::is_present(smooths)) {
    lifecycle::deprecate_warn("0.8.9.9", "compare_smooths(smooths)",
      "compare_smooths(select)")
    select <- smooths
  }
  ## grab ...
  dots <- rlang::dots_list(..., .named = TRUE)
  model_names <- c(paste(deparse(substitute(model)), collapse = ""), names(dots))
  if (length(dots) < 1L) {
    stop("Need at least two models to compare smooths",
      call. = FALSE
    )
  }
  ## combine model and others into a list
  models <- append(list(model), dots)

  models <- lapply(models, function(m) {
    if (inherits(m, c("gamm", "gamm4"))) model_context(m)$model else m
  })
  models <- lapply(models, with_model_envir, envir = envir)
  labels <- gratia::smooths(model)
  labels <- labels[check_user_select_smooths(labels, select, partial_match)]
  if (!length(labels)) stop("No smooths selected.", call. = FALSE)
  if (anyDuplicated(model_names)) {
    stop("Model names must be unique; name the additional models in `...`.",
      call. = FALSE)
  }
  info <- vector("list", length(labels))
  names(info) <- labels
  estimates <- lapply(labels, function(label) {
    ids <- lapply(seq_along(models), function(i) {
      id <- which(gratia::smooths(models[[i]]) == label)
      if (length(id) != 1L) {
        stop("Smooth ", label, " must occur exactly once in model ",
          model_names[i], ". Select matching terms with unique labels.", call. = FALSE)
      }
      id
    })
    sms <- Map(function(m, id) get_smooths_by_id(m, id)[[1L]], models, ids)
    variables <- lapply(sms, smooth_variable)
    keys <- lapply(sms, terms_in_smooth)
    if (!all(vapply(variables, setequal, logical(1), variables[[1L]])) ||
        !all(vapply(keys, setequal, logical(1), keys[[1L]]))) {
      stop("Incompatible covariates for smooth ", label, ".", call. = FALSE)
    }
    grid <- data
    if (length(variables[[1L]]) == 2L) {
      grid <- comparison_surface_data(models, sms, ids, data, n, n_2d)
    }
    results <- Map(function(m, name) {
      x <- smooth_estimates(m, select = label, n = n, n_2d = n_2d,
        data = grid, unconditional = unconditional,
        overall_uncertainty = overall_uncertainty, unnest = FALSE,
        dist = dist, envir = envir)
      x <- add_column(x, .model = name, .before = 1L)
      x
    }, models, model_names)
    info[[label]] <<- list(variables = variables[[1L]],
      coordinate_info = attr(results[[1L]], "coordinate_info"))
    bind_rows(results)
  })
  sm_est <- bind_rows(estimates) |> arrange(.data$.smooth)
  attr(sm_est, "comparison_info") <- info
  attr(sm_est, "model_order") <- model_names
  class(sm_est) <- c("compare_smooths", class(sm_est))
  sm_est
}

#' Plot comparisons of smooths
#'
#' @param object of class `"compare_smooths"`, the result of a call to
#'   [gratia::compare_smooths()].
#' @param guides character; guide handling for the combined plot, passed to
#'   [patchwork::wrap_plots()]. The default is `"collect"`.
#' @param show character; `"models"` (default) overlays one-variable smooths
#'   and draws two-variable smooths in side-by-side model facets with fixed axes
#'   and a shared fill scale. `"difference"` draws the estimated difference for
#'   selected two-variable smooths, using a fill scale centred on zero.
#' @param pair two model names or integer indices in input order. In difference
#'   mode, subtract the second model from the first. With exactly two models,
#'   the default uses their input order; with more models, supply `pair`.
#' @param ci_level numeric; coverage for one-variable smooth intervals.
#' @param surface character; `"raster"` or `"tile"` for surface displays.
#'   Irregular numeric grids use rectangles bounded by coordinate midpoints.
#' @param select,partial_match select smooths from the comparison, using the
#'   conventions of [compare_smooths()]. Numeric and logical selections follow
#'   the order of unique smooth labels in `object`.
#' @param ... arguments passed to [patchwork::wrap_plots()].
#' @details
#' Difference plots match both coordinates and any retained conditioning
#' covariates. Locations missing from either model, and missing estimates, remain
#' missing. Multiple conditioning values are drawn in separate panels.
#' No interval is calculated for the difference: estimates from models fitted
#' to the same observations can be correlated, and their separate covariance
#' matrices do not supply the covariance between models. See [compare_smooths()]
#' for the scale and centring of the compared smooth contributions.
#'
#' @inheritParams draw.gam
#'
#' @export
#' @importFrom dplyr group_split
#' @importFrom purrr map
#' @importFrom patchwork wrap_plots
#' @inheritSection draw Label layout
`draw.compare_smooths` <- function(object,
                                   ncol = NULL, nrow = NULL,
                                   guides = "collect",
                                   select = NULL, partial_match = FALSE,
                                   show = c("models", "difference"), pair = NULL,
                                   ci_level = 0.95,
                                   surface = c("raster", "tile"),
                                   contour = TRUE, n_contour = NULL,
                                   continuous_fill = NULL,
                                   ...) {
  show <- match.arg(show)
  surface <- match.arg(surface)
  labels <- unique(object$.smooth)
  labels <- labels[check_user_select_smooths(labels, select, partial_match)]
  if (!length(labels)) stop("No smooths selected.", call. = FALSE)
  model_order <- attr(object, "model_order")
  if (is.null(model_order)) model_order <- unique(object$.model)
  if (show == "difference") pair <- comparison_model_pair(pair, model_order)
  info <- attr(object, "comparison_info")
  plts <- lapply(labels, function(label) {
    x <- object[object$.smooth == label, ]
    variables <- info[[label]]$variables
    if (is.null(variables)) variables <- vars_from_label(label)
    if (length(variables) > 2L) {
      stop("Comparisons can draw only one- or two-variable smooths; select another term.",
        call. = FALSE)
    }
    if (show == "difference" && length(variables) != 2L) {
      stop("`show = 'difference'` requires two-variable smooths; use `select` to choose surfaces.",
        call. = FALSE)
    }
    plot_comparison_of_smooths(x, coverage = ci_level, variables = variables,
      model_order = model_order, show = show, pair = pair,
      surface = surface, contour = contour, n_contour = n_contour,
      continuous_fill = continuous_fill)
  })
  layout <- prepare_plot_layout(length(plts), ncol = ncol, nrow = nrow)
  wrap_plots(plts, byrow = TRUE, ncol = layout$ncol, nrow = layout$nrow,
    guides = guides, ...)
}

# Match names or indices without silently choosing a pair from several models.
comparison_model_pair <- function(pair, models) {
  if (is.null(pair)) {
    if (length(models) != 2L) {
      stop("With more than two models, supply `pair` as two model names or indices.",
        call. = FALSE)
    }
    return(models)
  }
  if (is.numeric(pair) && length(pair) == 2L && !anyNA(pair) &&
      all(is.finite(pair) & pair == floor(pair) & pair >= 1 & pair <= length(models))) {
    pair <- models[pair]
  }
  if (!is.character(pair) || length(pair) != 2L || anyNA(pair) ||
      anyDuplicated(pair) || !all(pair %in% models)) {
    stop("`pair` must select two distinct available model names or indices.", call. = FALSE)
  }
  pair
}

# Use all retained covariates, rather than row order, to match observations.
comparison_surface_difference <- function(object, pair) {
  first <- object[object$.model == pair[1L], ]
  second <- object[object$.model == pair[2L], ]
  if (!nrow(first) || !nrow(second)) stop("Both models in `pair` must contain the selected smooth.", call. = FALSE)
  first <- unnest(first, cols = all_of("data"))
  second <- unnest(second, cols = all_of("data"))
  value_cols <- c(".estimate", ".se", ".lower_ci", ".upper_ci")
  surface_rows <- function(d) {
    if (".bndry" %in% names(d)) {
      d <- d[!d$.bndry, setdiff(names(d), c(".bndry", ".loop"))]
    }
    d
  }
  first <- surface_rows(first)
  second <- surface_rows(second)
  metadata <- c(".model", ".smooth", ".type", ".by")
  keys <- setdiff(names(first), c(value_cols, metadata))
  other_keys <- setdiff(names(second), c(value_cols, metadata))
  if (!length(keys) || !setequal(keys, other_keys)) {
    stop("Surface covariates must match in both models.", call. = FALSE)
  }
  if (anyDuplicated(first[keys]) || anyDuplicated(second[keys])) {
    stop("Surface locations must be unique within each model, including conditioning values.", call. = FALSE)
  }
  first <- first[c(keys, ".estimate")]
  second <- second[c(keys, ".estimate")]
  out <- dplyr::full_join(first, second, by = keys, suffix = c("_1", "_2"))
  out$.diff <- out$.estimate_1 - out$.estimate_2
  if (!any(is.finite(out$.diff))) {
    stop("The selected models have no overlapping surface locations with finite estimates.",
      call. = FALSE)
  }
  out[c(keys, ".diff")]
}

# Retain the nested comparison contract; only the surface drawing changes.
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @importFrom ggplot2 ggplot geom_ribbon geom_line facet_wrap facet_grid vars
`plot_comparison_of_smooths` <- function(object, coverage = 0.95,
    variables = vars_from_label(unique(object$.smooth)),
    model_order = unique(object$.model), show = "models", pair = NULL,
    surface = "raster", contour = TRUE, n_contour = NULL,
    continuous_fill = NULL, ...) {
  label <- unique(object$.smooth)
  types <- unique(object$.type)
  isotropic <- all(types %in% c("TPRS (2d)", "TPRS (shrink) (2d)",
    "Duchon spline (2d)"))
  if (length(variables) == 2L) {
    boundary <- NULL
    if (any(vapply(object$data, function(d) ".bndry" %in% names(d), logical(1)))) {
      d <- unnest(object, cols = all_of("data"))
      if (show == "difference") d <- d[d$.model %in% pair, ]
      boundary <- d[d$.bndry %in% TRUE, ]
      boundary$.boundary_group <- interaction(boundary$.model, boundary$.loop)
    }
    if (show == "difference") {
      object <- comparison_surface_difference(object, pair)
      fill_var <- ".diff"
      fill_title <- paste(pair, collapse = " - ")
      facet <- NULL
    } else {
      object <- unnest(object, cols = all_of("data"))
      if (".bndry" %in% names(object)) {
        object <- object[!object$.bndry, setdiff(names(object), c(".bndry", ".loop"))]
      }
      object$.model <- factor(object$.model, levels = model_order)
      fill_var <- ".estimate"
      fill_title <- "Partial\neffect"
      facet <- facet_wrap(vars(.data$.model), nrow = 1L)
    }
    conditioning <- setdiff(names(object), c(variables, ".model", ".smooth",
      ".type", ".by", ".estimate", ".se", ".diff", ".lower_ci", ".upper_ci"))
    conditioning <- conditioning[!startsWith(conditioning, ".gratia_raw_")]
    conditioning <- conditioning[vapply(object[conditioning],
      function(x) length(unique(x)) > 1L, logical(1))]
    if (length(conditioning)) {
      rows <- ggplot2::vars(!!!rlang::syms(conditioning))
      cols <- if (show == "models") vars(.data$.model) else vars()
      facet <- facet_grid(rows = rows, cols = cols, labeller = "label_both")
    }
    if (show == "difference" && !is.null(boundary)) boundary$.model <- NULL
    if (all(types == "SOS")) {
      # Reuse geographic cells, projection and contour paths for spherical fits.
      if (show == "difference") object$.estimate <- object$.diff
      object$.smooth <- label
      object$.by <- if (show == "models") ".model" else NA_character_
      class(object) <- c("sos", "mgcv_smooth", class(object))
      # The geographic renderer transforms these columns even without intervals.
      object$.lower_ci <- object$.upper_ci <- NA_real_
      plt <- plot_smooth.sos(object, variables = variables, title = label,
        grouped_by = show == "models", contour = contour, n_contour = n_contour,
        continuous_fill = continuous_fill, caption = FALSE, subtitle = "")
      return(plt + facet + labs(fill = fill_title) +
        guides(fill = guide_colourbar(title = fill_title, direction = "vertical")))
    }
    return(prepare_surface_plot(object,
      x_var = variables[1L], y_var = variables[2L], fill_var = fill_var,
      fill_limits = surface_fill_limits(object[[fill_var]]),
      fill_title = fill_title, labels = labs(title = label,
        x = variables[1L], y = variables[2L], fill = fill_title),
      surface = surface, facet = facet,
      coord = if (isotropic || !is.null(boundary)) coord_equal(),
      boundary = boundary, boundary_group = ".boundary_group",
      contour = contour,
      n_contour = n_contour, continuous_fill = continuous_fill,
      irregular_cells = all(vapply(object[variables], is.numeric, logical(1)))))
  }
  object <- unnest(object, cols = all_of("data"))
  object <- object |> add_confint(coverage = coverage)
  plt <- gratia_plot(object, aes(
    x = .data[[variables[1L]]], y = .data$.estimate, group = .data$.model))
  plt <- add_curve_interval(plt,
    lower_var = ".lower_ci", upper_var = ".upper_ci",
    ribbon_mapping = aes(fill = .data$.model),
    line_mapping = aes(colour = .data$.model))
  plt + labs(colour = "Model", fill = "Model", title = label, y = "Partial effect")
}

# Build one grid over the common observed ranges; explicit data are evaluated
# as supplied, including their gaps and conditioning values.
comparison_surface_data <- function(models, smooths, ids, data, n, n_2d) {
  reference <- Map(function(m, sm) {
    d <- prepare_smooth_data(m, sm)
    if (is_factor_by_smooth(sm)) {
      d <- d[!is.na(d[[by_variable(sm)]]) &
        d[[by_variable(sm)]] == by_level(sm), , drop = FALSE]
    }
    d
  }, models, smooths)
  keys <- terms_in_smooth(smooths[[1L]])
  axes <- smooth_variable(smooths[[1L]])
  common <- lapply(keys, function(key) {
    columns <- lapply(reference, `[[`, key)
    numeric <- vapply(columns, is.numeric, logical(1))
    categorical <- vapply(columns, is.factor, logical(1))
    if (all(numeric) && !any(vapply(columns, is.matrix, logical(1)))) {
      ranges <- lapply(columns, function(x) {
        x <- x[is.finite(x)]
        if (length(x)) range(x) else c(NA_real_, NA_real_)
      })
      limits <- c(max(vapply(ranges, `[`, numeric(1), 1L)),
        min(vapply(ranges, `[`, numeric(1), 2L)))
      if (any(!is.finite(limits)) || limits[1L] > limits[2L] ||
          (key %in% axes && limits[1L] == limits[2L])) {
        stop("Nonoverlapping support for covariate ", key, ".", call. = FALSE)
      }
      return(limits)
    }
    if (all(categorical)) {
      levels <- Reduce(intersect, lapply(columns, function(x) levels(droplevels(x))))
      if (!length(levels)) stop("Nonoverlapping factor levels for ", key, ".", call. = FALSE)
      return(levels)
    }
    stop("Incompatible or unsupported covariate ", key, " in surface comparison.", call. = FALSE)
  })
  names(common) <- keys
  if (!is.null(data)) return(data)
  grid <- smooth_data(models[[1L]], ids[[1L]], n = n, n_2d = n_2d)
  for (key in axes) {
    if (is.numeric(grid[[key]])) {
      values <- seq(common[[key]][1L], common[[key]][2L],
        length.out = length(unique(grid[[key]])))
      grid[[key]] <- values[match(grid[[key]], unique(grid[[key]]))]
    } else {
      grid <- grid[as.character(grid[[key]]) %in% common[[key]], , drop = FALSE]
    }
  }
  grid
}
