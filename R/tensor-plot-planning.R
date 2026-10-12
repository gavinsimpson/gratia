# Explicit plotting level selections are local to the model copy passed by
# assemble(); direct smooth_estimates() uses its own grid controls.
is_tensor_plot_type <- function(type) {
  type %in% c("Tensor product", "Tensor product int.", "Tensor product (T2)")
}

tensor_plot_variables <- function(variables, data) {
  numeric <- vapply(data[variables], is.numeric, logical(1))
  c(variables[numeric], variables[!numeric])
}

validate_tensor_plot_controls <- function(re_levels) {
  if (!is.null(re_levels) && (!is.list(re_levels) || is.null(names(re_levels)) ||
      anyNA(names(re_levels)) || any(!nzchar(names(re_levels))) ||
      anyDuplicated(names(re_levels)) || any(!vapply(re_levels, function(x) {
        is.character(x) && length(x) > 0L && !anyNA(x) && !anyDuplicated(x)
      }, logical(1))))) {
    cli::cli_abort("`re_levels` must be a named list of nonempty, unique factor level labels.")
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

tensor_plot_spec <- function(model, id) {
  specs <- attr(model, "gratia.tensor_plot_specs")
  if (is.null(specs) || length(id) != 1L) return(NULL)
  specs[[id]]
}

plan_tensor_plots <- function(model, ids, data, re_levels) {
  validate_tensor_plot_controls(re_levels)
  specs <- vector("list", length(model$smooth))
  templates <- list()
  for (id in ids) {
    sm <- model$smooth[[id]]
    if (!is_tensor_plot_type(smooth_type(sm)) ||
        !any(vapply(sm$margin, inherits, logical(1), "random.effect"))) next
    fitted <- prepare_smooth_data(model, sm)
    vars <- tensor_plot_variables(reorder_tensor_smooth_terms(sm), fitted)
    factors <- vars[vapply(fitted[vars], is.factor, logical(1))]
    if (!length(factors)) next
    templates[factors] <- fitted[factors]
    selected <- re_levels[intersect(names(re_levels), factors)]
    validate_tensor_levels(selected, fitted[factors])
    if (!is.null(data)) {
      grid <- prepare_smooth_data(model, sm, data)
      if (is_factor_by_smooth(sm)) {
        by <- by_variable(sm)
        grid <- grid[is.na(grid[[by]]) | grid[[by]] == by_level(sm), , drop = FALSE]
      }
      grid <- filter_tensor_levels(grid, selected)
      if (!nrow(grid)) cli::cli_abort("No tensor evaluation rows remain for {.val {smooth_label(sm)}} after selecting `re_levels`.")
    }
    specs[[id]] <- list(variables = vars, levels = selected)
  }
  validate_tensor_levels(re_levels, templates)
  attr(model, "gratia.tensor_plot_specs") <- specs
  model
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
    vars <- reorder_tensor_smooth_terms(sm)
    list(variables = tensor_plot_variables(vars, data),
      factors = vars[vapply(data[vars], is.factor, logical(1))])
  })
  stats::setNames(info, vapply(smooths, smooth_label, character(1)))
}

# Match typed tuples, never concatenated labels: values may contain separators.
# Only evaluated categorical states may contribute rugs/facets.
tensor_plot_groups <- function(object, rug, factors, group_vars) {
  states <- vctrs::vec_unique(object[group_vars])
  labels <- lapply(group_vars, function(v) {
    paste0(v, " = ", encodeString(as.character(states[[v]]), quote = '\"'))
  })
  labels <- do.call(paste, c(labels, sep = ", "))
  as_group <- function(x) factor(vctrs::vec_match(x[group_vars], states),
    levels = seq_len(nrow(states)), labels = labels)
  if (!is.null(rug)) {
    observed <- !is.na(vctrs::vec_match(rug[factors],
      vctrs::vec_unique(object[factors])))
    rug <- rug[observed, , drop = FALSE]
  }
  list(group = as_group(object), rug = rug,
    rug_group = if (!is.null(rug)) as_group(rug) else NULL)
}
