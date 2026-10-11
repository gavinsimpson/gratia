# Plotting restrictions are local to the model copy passed by assemble().
# smooth_estimates() called directly never acquires these restrictions.
is_tensor_plot_type <- function(type) {
  type %in% c("Tensor product", "Tensor product int.", "Tensor product (T2)")
}

tensor_plot_variables <- function(variables, data) {
  numeric <- vapply(data[variables], is.numeric, logical(1))
  c(variables[numeric], variables[!numeric])
}

validate_tensor_plot_controls <- function(re_levels, max_re_combinations) {
  if (!is.null(re_levels) && (!is.list(re_levels) || is.null(names(re_levels)) ||
      anyNA(names(re_levels)) || any(!nzchar(names(re_levels))) ||
      anyDuplicated(names(re_levels)) || any(!vapply(re_levels, function(x) {
        is.character(x) && length(x) > 0L && !anyNA(x) && !anyDuplicated(x)
      }, logical(1))))) {
    cli::cli_abort("`re_levels` must be a named list of nonempty, unique factor level labels.")
  }
  if (!is.numeric(max_re_combinations) || length(max_re_combinations) != 1L ||
      is.na(max_re_combinations) || max_re_combinations < 1 ||
      max_re_combinations != floor(max_re_combinations)) {
    cli::cli_abort("`max_re_combinations` must be a positive integer or Inf.")
  }
}

validate_tensor_levels <- function(re_levels, templates) {
  for (v in names(re_levels)) {
    if (!v %in% names(templates) || !is.factor(templates[[v]])) {
      cli::cli_abort("`re_levels` variable {.val {v}} is not a factor in a selected tensor term.")
    }
    unknown <- setdiff(re_levels[[v]], levels(templates[[v]]))
    if (length(unknown)) cli::cli_abort("Unknown `re_levels` for {.val {v}}: {.val {unknown}}.")
  }
}

filter_tensor_levels <- function(data, re_levels) {
  for (v in intersect(names(re_levels), names(data))) {
    data <- data[as.character(data[[v]]) %in% re_levels[[v]], , drop = FALSE]
  }
  data
}

# Count conditioning states rather than multiplying marginal counts for supplied
# rows: a supplied slice need not be a complete Cartesian product.
tensor_plot_combinations <- function(data, variables) {
  numeric <- variables[vapply(data[variables], is.numeric, logical(1))]
  conditioning <- setdiff(variables, utils::head(numeric, 2L))
  if (!length(conditioning)) return(1L)
  nrow(unique(data[conditioning]))
}

tensor_plot_allowed <- function(label, count, limit) {
  if (count <= limit) return(TRUE)
  cli::cli_alert_info("Skipping {label}: {count} random-effect conditioning combinations exceed `max_re_combinations = {limit}`. Select `re_levels`, supply a smaller `data` slice, or increase the limit.")
  FALSE
}

tensor_plot_spec <- function(model, id) {
  specs <- attr(model, "gratia.tensor_plot_specs")
  if (is.null(specs) || length(id) != 1L) return(NULL)
  specs[[id]]
}

plan_tensor_plots <- function(model, ids, data, re_levels, limit,
    n, n_2d, n_3d, n_4d, back_transform) {
  validate_tensor_plot_controls(re_levels, limit)
  specs <- vector("list", length(model$smooth))
  templates <- list()
  candidates <- list()
  for (id in ids) {
    sm <- model$smooth[[id]]
    if (!is_tensor_plot_type(smooth_type(sm)) ||
        !any(vapply(sm$margin, inherits, logical(1), "random.effect"))) next
    vars <- reorder_tensor_smooth_terms(sm)
    fitted <- prepare_smooth_data(model, sm)
    factors <- vars[vapply(fitted[vars], is.factor, logical(1))]
    if (!length(factors)) next
    templates[factors] <- fitted[factors]
    candidates[[as.character(id)]] <- list(variables = tensor_plot_variables(vars, fitted),
      fitted = fitted, factors = factors)
  }
  validate_tensor_levels(re_levels, templates)
  keep <- ids
  for (key in names(candidates)) {
    id <- as.integer(key)
    sm <- model$smooth[[id]]
    candidate <- candidates[[key]]
    vars <- candidate$variables
    selected <- re_levels[intersect(names(re_levels), candidate$factors)]
    if (!is.null(data)) {
      grid <- prepare_smooth_data(model, sm, data)
      if (is_factor_by_smooth(sm)) {
        by <- by_variable(sm)
        grid <- grid[is.na(grid[[by]]) | grid[[by]] == by_level(sm), , drop = FALSE]
      }
      grid <- filter_tensor_levels(grid, selected)
      if (!nrow(grid)) cli::cli_abort("No tensor evaluation rows remain for {.val {smooth_label(sm)}} after selecting `re_levels`.")
      count <- tensor_plot_combinations(grid, vars)
    } else {
      conditioning <- setdiff(vars, utils::head(vars[vapply(candidate$fitted[vars],
        is.numeric, logical(1))], 2L))
      counts <- vapply(conditioning, function(v) {
        x <- candidate$fitted[[v]]
        if (is.factor(x)) {
          lev <- levels(x)
          if (v %in% names(selected)) lev <- intersect(lev, selected[[v]])
          return(length(lev))
        }
        resolution <- if (length(vars) == 3L) n_3d else n_4d
        if (is.null(resolution)) resolution <- n
        values <- seq_min_max(x, resolution)
        if (!back_transform) values <- round(values, 3)
        length(unique(values))
      }, integer(1))
      count <- prod(counts)
    }
    if (!tensor_plot_allowed(smooth_label(sm), count, limit)) {
      keep <- setdiff(keep, id)
    } else specs[[id]] <- list(variables = vars, levels = selected)
  }
  attr(model, "gratia.tensor_plot_specs") <- specs
  list(model = model, ids = keep)
}

validate_estimate_re_levels <- function(object, re_levels) {
  if (!length(re_levels)) return(invisible(NULL))
  data <- if ("data" %in% names(object) && !".estimate" %in% names(object)) {
    tidyr::unnest(object, cols = "data")
  } else object
  templates <- list()
  for (label in unique(data$.smooth)) {
    term <- data[data$.smooth == label, , drop = FALSE]
    info <- attr(object, "tensor_plot_info")[[label]]
    if (is.null(info) && !is_tensor_plot_type(unique(term$.type))) next
    vars <- if (!is.null(info)) info$variables else attr(object, "tensor_term_order")[[label]]
    if (is.null(vars)) vars <- vars_from_label(label)
    factors <- vars[vapply(term[vars], is.factor, logical(1))]
    templates[factors] <- term[factors]
  }
  validate_tensor_levels(re_levels, templates)
}

# Persist marginal roles independently of labels and the smooth's display type.
tensor_plot_info <- function(model, smooths) {
  info <- lapply(smooths, function(sm) {
    if (is.null(sm$margin) ||
        !any(vapply(sm$margin, inherits, logical(1), "random.effect"))) return(NULL)
    data <- prepare_smooth_data(model, sm)
    vars <- smooth_variable(sm)
    list(variables = tensor_plot_variables(vars, data),
      factors = vars[vapply(data[vars], is.factor, logical(1))])
  })
  stats::setNames(info, vapply(smooths, smooth_label, character(1)))
}
