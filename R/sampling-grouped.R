## Grouped models use one joint coefficient vector and one observation family
## per row. Reuse membership validation and native component generators rather
## than invoking the training-row-specific aggregate gfam closures.
grouped_posterior_context <- function(model, data = NULL, family_index = NULL, ...) {
  context <- gfam_prediction_context(model, data = data, family_index = family_index,
    na.action = list(...)$na.action %||% stats::na.pass)
  context$model <- model_used_rows(model)
  context$X <- gfam_predict_eta(context$model, context$layout$data,
    type = "lpmatrix", ...)
  context$offset <- prediction_offsets(context$model, context$X)[[1L]]
  if (context$layout$training) {
    eta <- predict_model(context$model, type = "link", ...) +
      fitting_link_correction(context$model)
    context$offset <- as.numeric(eta) -
      drop(context$X %*% stats::coef(context$model))
  }
  context$generators <- lapply(seq_along(context$components), function(i) {
    component <- context$components[[i]]
    if (family_type(component$family) %in% c("cnorm", "clog", "cpois") &&
        any(context$index == i, na.rm = TRUE)) {
      stop("Posterior response sampling for censored grouped component ", i,
        " requires an observation process; latent component draws remain ",
        "available through simulate().", call. = FALSE)
    }
    rd <- fix_family_rd(component$family)$rd
    if (!is.function(rd) && any(context$index == i, na.rm = TRUE)) {
      stop("No response generator for grouped component ", i, " <",
        component$family$family, ">.", call. = FALSE)
    }
    rd
  })
  context
}

grouped_sampling_weights <- function(context, weights = NULL) {
  layout <- context$layout
  if (is.null(weights) && layout$training) {
    return(context$model$prior.weights %||% rep(1, NROW(context$X)))
  }
  if (is.null(weights)) weights <- rep(1, layout$n_input)
  if (length(weights) == 1L) weights <- rep(weights, layout$n_input)
  if (length(weights) != layout$n_input) {
    stop("Weights must have one value per prediction row.", call. = FALSE)
  }
  if (all(is.na(layout$map))) return(rep(NA_real_, NROW(context$X)))
  rows <- if (layout$training) which(!is.na(layout$map)) else layout$input_rows
  weights[rows]
}

# Keep one generator call per component and draw, in component-list order.
# This preserves the RNG order of the fitting-observation diagnostic sampler.
grouped_response_generator <- function(context, betas, weights = NULL) {
  wt <- grouped_sampling_weights(context, weights)
  missing <- all(is.na(context$layout$map))
  function(i) {
    if (missing) return(rep(NA_real_, NROW(context$X)))
    eta <- drop(context$X %*% betas[i, ]) + context$offset
    gfam_response_draw(eta, wt, context, context$generators)
  }
}

grouped_response_parameters <- function(context) {
  parameter <- context$family_index[context$layout$rows]
  ifelse(is.na(parameter), NA_character_, paste0("component", parameter))
}

# Diagnostic calculations can request one response table at a time.
grouped_response_sampler <- function(context, betas, weights = NULL) {
  generate <- grouped_response_generator(context, betas, weights)
  parameter <- grouped_response_parameters(context)
  out <- function(i) {
    value <- diagnostic_response_table(generate(i), i)
    # Restore rows before attaching the row-specific component labels. Otherwise
    # the restoration helper would incorrectly expand each row into every family.
    value <- restore_prediction_table(value, context$layout)
    value$.parameter <- parameter
    value
  }
  attr(out, "sampling") <- attr(betas, "sampling")
  out
}

# Public output includes every draw. Allocate its response values once, restore
# excluded positions directly, and construct the table after drawing responses.
grouped_response_draws <- function(context, betas, weights = NULL) {
  generate <- grouped_response_generator(context, betas, weights)
  layout <- context$layout
  n <- length(layout$map)
  draws <- NROW(betas)
  values <- matrix(NA_real_, n, draws)
  for (i in seq_len(draws)) values[, i] <- generate(i)[layout$map]
  dim(values) <- NULL
  tibble::tibble(.row = rep(layout$rows, draws),
    .draw = rep(seq_len(draws), each = n),
    .parameter = rep(grouped_response_parameters(context), draws),
    .response = values)
}
