# Utilities involved in draw methods

validate_factor_plot_types <- function(value, argument) {
  choices <- c("factor-by", "sz", "fs")
  if (!is.character(value) || anyNA(value) || any(!value %in% choices)) {
    cli::cli_abort("{.arg {argument}} must be a character vector containing only {.val {choices}}; use character(0) to select none.")
  }
  unique(value)
}

resolve_factor_plot_controls <- function(grouped, show_ci, grouped_by,
                                         caller) {
  grouped <- validate_factor_plot_types(grouped, "grouped")
  show_ci <- validate_factor_plot_types(show_ci, "show_ci")
  if (lifecycle::is_present(grouped_by)) {
    if (!is.logical(grouped_by) || length(grouped_by) != 1L ||
        is.na(grouped_by)) {
      cli::cli_abort("`grouped_by` must be a single TRUE or FALSE.")
    }
    lifecycle::deprecate_warn("0.11.2.59", paste0(caller, "(grouped_by)"),
      paste0(caller, "(grouped)"), details =
        'Use "factor-by" in `grouped` to overlay factor-by curves, or omit it to separate them.',
      user_env = globalenv())
    grouped <- if (grouped_by) union(grouped, "factor-by") else {
      setdiff(grouped, "factor-by")
    }
  }
  list(grouped = grouped, show_ci = show_ci)
}

factor_plot_term <- function(label, by) {
  if (is.na(by)) return(label)
  suffixes <- gregexpr(paste0(":", by), label, fixed = TRUE)[[1L]]
  if (suffixes[1L] < 0L) return(label)
  # A colon inside a quoted covariate expression is not the by suffix.
  for (suffix in suffixes) {
    term <- substr(label, 1L, suffix - 1L)
    expression <- tryCatch(parse(text = term), error = function(e) NULL)
    if (length(expression) == 1L) return(term)
  }
  substr(label, 1L, suffixes[1L] - 1L)
}

# Resolve variable roles in exactly the same way for models and saved estimates.
factor_plot_spec <- function(object, tensor_term_order = NULL,
                              tensor_plot_info = NULL, plot_variables = NULL,
                              coordinate_info = NULL) {
  label <- as.character(object$.smooth[1L])
  by <- as.character(object$.by[1L])
  term <- factor_plot_term(label, by)
  values <- if ("data" %in% names(object)) {
    dplyr::bind_rows(object$data)
  } else object
  variables <- plot_variables[[label]]
  if (is.null(variables)) variables <- tensor_plot_info[[label]]$variables
  if (is.null(variables)) variables <- tensor_term_order[[label]]
  if (is.null(variables)) {
    variables <- attr(coordinate_info, "smooth_variables")[[label]]
  }
  if (is.null(variables)) variables <- vars_from_label(term)
  factors <- variables[vapply(values[variables], is.factor, logical(1))]
  numeric <- variables[vapply(values[variables], is.numeric, logical(1))]
  type <- as.character(object$.type[1L])
  category <- if (!is.na(by) && is.factor(values[[by]])) "factor-by" else {
    if (type == "Factor smooth") "fs" else {
      if (type == "Constr. factor smooth") "sz" else NA_character_
    }
  }
  curve <- length(numeric) == 1L &&
    (identical(category, "factor-by") && length(variables) == 1L ||
      category %in% c("fs", "sz") && length(factors) > 0L)
  list(category = category, curve = curve, factors = factors, term = term,
    by = by, type = type)
}

factor_panel_name <- function(object) {
  name <- attr(object, "factor_plot")$name
  if (is.null(name)) unique(object$.term) else name
}

# Plan one input per ggplot; renderers never need to return lists of plots.
prepare_factor_smooth_panels <- function(object, grouped, show_ci,
    tensor_term_order = attr(object, "tensor_term_order"),
    tensor_plot_info = attr(object, "tensor_plot_info"),
    plot_variables = attr(object, "plot_variables"),
    coordinate_info = attr(object, "coordinate_info")) {
  if (!"data" %in% names(object)) {
    object <- tidyr::nest(object,
      data = !tidyselect::any_of(c(".smooth", ".type", ".by", ".smooth_id", ".model")))
  }
  components <- split_smooth_components(object)
  specs <- lapply(components, factor_plot_spec,
    tensor_term_order = tensor_term_order, tensor_plot_info = tensor_plot_info,
    plot_variables = plot_variables, coordinate_info = coordinate_info)
  groups <- seq_along(components)
  by_ids <- which(vapply(specs, function(x) {
    identical(x$category, "factor-by") && "factor-by" %in% grouped
  }, logical(1)))
  if (length(by_ids)) {
    keys <- dplyr::bind_rows(lapply(by_ids, function(i) {
      x <- components[[i]]
      tibble::tibble(term = specs[[i]]$term, type = specs[[i]]$type,
        by = specs[[i]]$by, model = if (".model" %in% names(x)) {
          as.character(x$.model[1L])
        } else NA_character_, label = as.character(x$.smooth[1L]))
    }))
    # Repeated components with identical labels remain separate across levels.
    identities <- vctrs::vec_group_id(keys)
    keys$component <- stats::ave(seq_along(identities), identities, FUN = seq_along)
    keys$label <- NULL
    ids <- vctrs::vec_group_id(keys)
    groups[by_ids] <- by_ids[match(ids, ids)]
  }
  panels <- list()
  for (g in unique(groups)) {
    ids <- which(groups == g)
    entry <- dplyr::bind_rows(components[ids])
    spec <- specs[[ids[1L]]]
    entry$.term <- spec$term
    settings <- list(grouped_by = identical(spec$category, "factor-by") &&
        "factor-by" %in% grouped,
      interval = if (spec$curve) spec$category %in% show_ci else NULL,
      separate = spec$curve && spec$category %in% c("fs", "sz") &&
        !spec$category %in% grouped,
      subtitle = NULL, name = NULL)
    if (!settings$separate) {
      attr(entry, "factor_plot") <- settings
      panels[[length(panels) + 1L]] <- entry
      next
    }
    values <- dplyr::bind_rows(entry$data)
    states <- vctrs::vec_unique(values[spec$factors])
    states <- states[do.call(order, states), , drop = FALSE]
    matches <- vctrs::vec_match(values[spec$factors], states)
    for (i in seq_len(nrow(states))) {
      panel <- entry[1L, , drop = FALSE]
      panel$data <- list(values[matches == i, , drop = FALSE])
      for (column in intersect(c("rug_data", "partial_residual"), names(panel))) {
        observations <- dplyr::bind_rows(entry[[column]])
        if (all(spec$factors %in% names(observations))) {
          observations <- observations[
            vctrs::vec_match(observations[spec$factors], states) %in% i, , drop = FALSE]
        }
        panel[[column]] <- list(observations)
      }
      settings$subtitle <- paste(vapply(spec$factors, function(v) {
        paste0(v, " = ", encodeString(as.character(states[[v]][i]), quote = '"'))
      }, character(1)), collapse = ", ")
      settings$name <- paste0(spec$term, ": ", settings$subtitle)
      attr(panel, "factor_plot") <- settings
      panels[[length(panels) + 1L]] <- panel
    }
  }
  panels
}

# Hidden ribbons must not determine the common range of curve plots.
factor_smooth_plot_range <- function(object, show_ci, constant = NULL,
    fun = NULL, transform = FALSE) {
  specs <- split_smooth_components(object)
  ranges <- lapply(specs, function(x) {
    spec <- factor_plot_spec(x,
      tensor_term_order = attr(object, "tensor_term_order"),
      tensor_plot_info = attr(object, "tensor_plot_info"),
      plot_variables = attr(object, "plot_variables"),
      coordinate_info = attr(object, "coordinate_info"))
    d <- dplyr::bind_rows(x$data)
    columns <- if (spec$curve && !spec$category %in% show_ci) ".estimate" else {
      c(".estimate", ".lower_ci", ".upper_ci")
    }
    if (transform) {
      d <- add_constant(d, constant = constant, column = columns)
      d <- transform_fun(d, fun = fun, column = columns)
    }
    unlist(d[intersect(columns, names(d))], use.names = FALSE)
  })
  unlist(ranges, use.names = FALSE)
}

# Keep one covariate order from grid generation through saved estimates.
validate_plot_variables <- function(variables, expected) {
  if (is.null(variables)) return(expected)
  if (!is.character(variables) || !length(variables) || anyNA(variables) ||
      any(!nzchar(variables)) || anyDuplicated(variables)) {
    cli::cli_abort("`variables` must be a character vector of unique covariate names without missing values.")
  }
  if (length(variables) != length(expected) || !setequal(variables, expected)) {
    cli::cli_abort(c(
      "`variables` must contain every covariate of each selected smooth exactly once.",
      "i" = "Expected: {.val {expected}}. Use `select` to choose smooths with the same covariates."
    ))
  }
  variables
}

smooth_plot_variables <- function(model, id) {
  orders <- attr(model, "gratia.plot_variables")
  # Soap components can share a label; use the first selected component's order.
  for (i in id) {
    if (!is.null(orders[[i]])) return(orders[[i]])
  }
  NULL
}

validate_plot_variable_types <- function(variables, data) {
  factors <- vapply(data[variables], is.factor, logical(1))
  if (any(factors) && any(diff(as.integer(factors)) < 0L)) {
    cli::cli_abort("Continuous covariates must precede factor covariates in `variables`.")
  }
}

plan_smooth_variables <- function(model, ids, variables = NULL) {
  orders <- vector("list", length(model$smooth))
  for (id in ids) {
    sm <- model$smooth[[id]]
    margins <- sm[["margin"]]
    order <- if (inherits(sm, "fs.interaction")) {
      reorder_fs_smooth_terms(sm)
    } else if (!is.null(margins)) reorder_tensor_smooth_terms(sm) else {
      smooth_variable(sm)
    }
    spec <- tensor_plot_spec(model, id)
    if (!is.null(spec)) order <- spec$variables
    if (!is.null(margins) &&
        any(vapply(margins, inherits, logical(1), "random.effect"))) {
      order <- tensor_plot_variables(order, prepare_smooth_data(model, sm))
    }
    orders[[id]] <- validate_plot_variables(variables, order)
    if (!is.null(variables)) {
      validate_plot_variable_types(variables, prepare_smooth_data(model, sm))
    }
    if (inherits(sm, "sos.smooth") &&
        !identical(unname(orders[[id]]), unname(order))) {
      cli::cli_abort("`variables` cannot reorder latitude and longitude in a spline on the sphere.")
    }
  }
  attr(model, "gratia.plot_variables") <- orders
  model
}

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

# The boundary and wiggly soap components have the same mgcv smooth label.
# Use fitted indices when labels and types both collide, retaining public labels.
split_smooth_components <- function(data) {
  columns <- intersect(c(".smooth", ".type", ".model", ".smooth_id"), names(data))
  key <- vctrs::vec_group_id(data[columns])
  unname(split(data, key))
}
