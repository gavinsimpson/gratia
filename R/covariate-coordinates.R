# Plot coordinates are separate from transformations of the estimated effect.
check_back_transform <- function(back_transform) {
  if (!is.logical(back_transform) || length(back_transform) != 1L ||
      is.na(back_transform)) {
    cli::cli_abort("{.arg back_transform} must be TRUE or FALSE.")
  }
  back_transform
}

parametric_back_transform <- function(back_transform, transform,
    supplied = FALSE) {
  if (lifecycle::is_present(transform)) {
    lifecycle::deprecate_warn("0.11.3", "parametric_effects(transform)",
      "parametric_effects(back_transform)",
      details = "Use `back_transform = !transform` to select the covariate scale.")
    check_back_transform(transform)
    if (supplied && !identical(back_transform, !transform)) {
      cli::cli_abort("{.arg transform} and {.arg back_transform} select different covariate scales.")
    }
    back_transform <- !transform
  }
  check_back_transform(back_transform)
}

# A formula interaction has one coordinate per input, not one product axis.
# Basis expansions and expressions involving several inputs retain raw axes.
parametric_axes <- function(model, expression, data, columns) {
  split <- function(x) {
    if (is.call(x) && identical(x[[1L]], as.name(":"))) {
      c(split(x[[2L]]), split(x[[3L]]))
    } else list(x)
  }
  pieces <- split(str2lang(expression))
  evaluated <- NULL
  lapply(names(columns), function(v) {
    relevant <- Filter(function(x) v %in% all.vars(x), pieces)
    value <- data[[v]]
    label <- v
    if (length(relevant) == 1L && !is.symbol(relevant[[1L]]) &&
        length(intersect(all.vars(relevant[[1L]]), names(data))) == 1L) {
      if (is.null(evaluated)) evaluated <- evaluated_model_frame(model, data)
      candidate <- paste(deparse(relevant[[1L]], width.cutoff = 500L), collapse = "")
      fitted <- evaluated[[candidate]]
      if (is.numeric(fitted) && (!is.matrix(fitted) || ncol(fitted) == 1L)) {
        value <- as.numeric(fitted)
        label <- candidate
      }
    }
    list(column = unname(columns[[v]]), original = v, label = label,
      raw = data[[v]], model = value,
      raw_column = unname(coordinate_columns(model, names(columns), ".gratia_raw_")[v]),
      model_column = unname(coordinate_columns(model, names(columns), ".gratia_model_")[v]))
  }) |>
    stats::setNames(names(columns))
}

# Retain coordinates by row: many-to-one functions never require inversion.
coordinate_columns <- function(model, variables, prefix) {
  reserved <- unique(c(names(model$var.summary), names(model$model),
    ".smooth", ".estimate", ".se", ".partial", ".value", ".level"))
  stats::setNames(utils::tail(make.unique(c(reserved,
    paste0(prefix, variables))), length(variables)), variables)
}

retain_parametric_axes <- function(object, axes) {
  for (axis in axes) {
    if (identical(axis$label, axis$original)) next
    object[[axis$raw_column]] <- axis$raw
    object[[axis$model_column]] <- axis$model
  }
  object
}

parametric_axis_info <- function(axes) {
  lapply(axes, function(axis) axis[setdiff(names(axis), c("raw", "model"))])
}

display_parametric_axes <- function(object, axes, from_raw, back_transform) {
  labels <- character()
  if (is.null(object)) return(list(data = NULL, labels = labels))
  for (axis in axes) {
    column <- axis$column
    labels[axis$original] <- if (back_transform) axis$original else axis$label
    if (identical(axis$label, axis$original)) next
    attr(object, "gratia.coordinate_display") <- TRUE
    if (!column %in% names(object) || identical(from_raw, back_transform)) next
    source <- if (back_transform) axis$raw_column else axis$model_column
    if (!source %in% names(object)) {
      cli::cli_abort("Retained covariate coordinates are unavailable; recompute `parametric_effects()`.")
    }
    object[[column]] <- object[[source]]
  }
  list(data = object, labels = labels)
}

smooth_coordinate_info <- function(label, model = NULL, data = NULL) {
  expr <- term_expression(label, model)
  if (is.symbol(expr)) return(list(original = label, column = label))
  inputs <- all.vars(expr)
  if (!is.null(model)) inputs <- intersect(inputs, names(model$var.summary))
  if (length(inputs) != 1L) return(list(original = NULL, column = NULL))
  column <- if (is.null(model)) NULL else
    unname(coordinate_columns(model, names(model$var.summary), ".gratia_raw_")[inputs])
  list(original = inputs[[1L]], column = column)
}

retain_smooth_coordinates <- function(data, labels, model, required = FALSE) {
  for (label in labels) {
    axis <- smooth_coordinate_info(label, model)
    if (identical(axis$original, label)) next
    # A retained row coordinate takes precedence over nuisance covariates,
    # including a continuous by multiplier subsequently set to one.
    if (!is.null(axis$column) && axis$column %in% names(data)) next
    available <- !is.null(axis$original) && axis$original %in% names(data)
    # Evaluated grids may carry typical raw values for unrelated predictions.
    # Those values do not describe the rows of the transformed axis.
    if (isTRUE(attr(data, "gratia.evaluated"))) available <- FALSE
    if (available && !is.null(attr(data, "terms"))) {
      raw <- data
      attr(raw, "terms") <- NULL
      reference <- tryCatch(evaluate_terms(raw, label, model), error = function(e) NULL)
      available <- !is.null(reference) && isTRUE(all.equal(
        reference[[label]], data[[label]], check.attributes = FALSE))
    }
    if (!available) {
      if (required) cli::cli_abort(c(
        "Cannot recover an original covariate for {.val {label}}.",
        "i" = "Supply raw covariates in `data`; each coordinate must depend on one original model covariate."))
      next
    }
    data[[axis$column]] <- data[[axis$original]]
  }
  data
}

smooth_coordinate_columns <- function(model, labels, data) {
  columns <- vapply(labels, function(label) {
    axis <- smooth_coordinate_info(label, model)
    if (is.null(axis$column)) NA_character_ else axis$column
  }, character(1))
  intersect(columns, names(data))
}

back_transform_smooth_coordinates <- function(object, variables, info = NULL,
    numeric_by = character()) {
  variables <- intersect(variables, names(object))
  original <- variables
  for (i in seq_along(variables)) {
    label <- variables[[i]]
    axis <- info[[label]]
    if (is.null(axis)) axis <- smooth_coordinate_info(label)
    if (identical(axis$original, label)) next
    if (is.null(axis$original) || is.null(axis$column) ||
        !axis$column %in% names(object)) {
      cli::cli_abort(c("Original covariate coordinates for {.val {label}} are unavailable.",
        "i" = "Recompute with `smooth_estimates(model, back_transform = TRUE)`, or supply raw covariates in `data`."))
    }
    if (".bndry" %in% names(object) && any(object$.bndry &
        is.na(object[[axis$column]]) & !is.na(object[[label]]), na.rm = TRUE)) {
      cli::cli_abort(c("Original boundary coordinates for {.val {label}} are unavailable.",
        "i" = "Draw this soap film on the model scale with `back_transform = FALSE`."))
    }
    object[[label]] <- object[[axis$column]]
    original[[i]] <- axis$original
    attr(object, "gratia.coordinate_display") <- TRUE
  }
  # Numeric by values have already multiplied the estimates. The retained raw
  # axis replaces a same-named multiplier only in the drawing data.
  collisions <- intersect(original[original != variables],
    setdiff(names(object), variables))
  for (column in intersect(collisions, numeric_by)) object[[column]] <- NULL
  if (anyDuplicated(original) || any(original != variables & original %in%
      setdiff(names(object), variables))) {
    cli::cli_abort("Back-transformed coordinate names would collide.")
  }
  names(object)[match(variables, names(object))] <- original
  list(data = object, variables = original)
}
