# Extract model structure without evaluating the fitting call. Keep this
# boundary separate from recovery so new fitters need only small adapters.
model_context <- function(object, component = NULL) {
  UseMethod("model_context")
}

model_context.default <- function(object, component = NULL) {
  tt <- stats::terms(object)
  ff <- stats::formula(object)
  if (is.list(ff)) {
    if (is.null(component)) component <- 1L
    ff <- ff[[component]]
  }
  list(model = object, component = component, formula = ff, terms = tt,
    data = stats::model.frame(object), provenance = "evaluated",
    call = object$call, envir = environment(ff),
    na.action = stats::na.action(object))
}
model_context.gam <- model_context.default
model_context.scam <- model_context.default
model_context.gamm <- function(object, component = NULL) {
  model_context(object$gam, component)
}
model_context.list <- function(object, component = NULL) {
  if (!is_gamm4(object)) {
    cli::cli_abort("No model context adapter for this list.")
  }
  model_context(object$gam, component)
}
model_context.gamlss <- function(object, component = NULL) {
  if (is.null(component)) component <- "gam1"
  if (!is.character(component) || !component %in% names(object) ||
      !inherits(object[[component]], "gam")) {
    cli::cli_abort("Select an embedded GJRM GAM using {.arg component}.")
  }
  ctx <- model_context(object[[component]])
  ctx$call$data <- object$call$data
  ctx$component <- component
  ctx
}

# The attribute belongs to a local model copy and is serializable for workers.
with_model_envir <- function(model, envir = NULL) {
  if (!is.null(envir)) {
    if (!is.environment(envir)) {
      cli::cli_abort("{.arg envir} must be an environment or NULL.")
    }
    attr(model, "gratia.envir") <- envir
  }
  model
}
model_envir <- function(model, envir = NULL) {
  if (is.null(envir)) envir <- attr(model, "gratia.envir", exact = TRUE)
  if (is.null(envir)) envir <- model_context(model)$envir
  if (is.null(envir)) envir <- baseenv()
  if (!is.environment(envir)) cli::cli_abort("{.arg envir} must be an environment.")
  envir
}

# Smooth metadata contains both deparsed calls and literal nonstandard names.
term_expression <- function(label, model = NULL) {
  raw <- if (!is.null(model)) names(model$var.summary) else character()
  if (label %in% raw) return(as.name(label))
  tryCatch(str2lang(label), error = function(e) as.name(label))
}

expression_failure <- function(label, error) {
  cli::cli_abort(c("Cannot evaluate model expression {.val {label}}.",
    "x" = conditionMessage(error),
    "i" = "Supply raw covariates in {.arg data} and functions or constants in {.arg envir}, or supply an evaluated coordinate column."),
    class = "gratia_expression_error", parent = error)
}

# Evaluate against the original input, never sequentially against newly added
# columns: this avoids collisions between calls and literal variable names.
evaluate_terms <- function(data, labels, model, envir = NULL,
                           evaluated = FALSE) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  out <- data
  env <- model_envir(model, envir)
  for (label in unique(labels)) {
    expr <- term_expression(label, model)
    deps <- all.vars(expr)
    if (label %in% names(data) &&
        (evaluated || is.symbol(expr) || !all(deps %in% names(data)))) next
    ans <- tryCatch({
      # Covariate vectors must come from data, not a same-named global vector.
      missing <- setdiff(intersect(deps, names(model$var.summary)), names(data))
      if (length(missing)) stop("Missing covariate(s): ", paste(missing, collapse = ", "))
      eval(expr, envir = data, enclos = env)
    }, error = identity)
    if (inherits(ans, "error")) expression_failure(label, ans)
    if (is.null(ans) || NROW(ans) != NROW(data)) {
      expression_failure(label, simpleError("The expression must return one value or matrix row per observation."))
    }
    out[[label]] <- ans
  }
  out
}

prepare_smooth_data <- function(model, smooth, data, envir = NULL) {
  evaluate_terms(data, terms_in_smooth(smooth), model, envir,
    evaluated = !is.null(attr(data, "terms")) ||
      isTRUE(attr(data, "gratia.evaluated")))
}

# Raw recovery is deliberately conservative. Never infer an inverse transform.
recover_raw_data <- function(model, data = NULL, envir = NULL, vars = model_vars(model)) {
  if (!is.null(data)) {
    if (!all(vars %in% names(data))) {
      cli::cli_abort(c("Raw model covariates are missing from {.arg data}.",
        "i" = "Required: {.val {setdiff(vars, names(data))}}."),
        class = "gratia_data_recovery_error")
    }
    return(data[, vars, drop = FALSE])
  }
  ctx <- model_context(model)
  if (all(vars %in% names(ctx$data))) return(ctx$data[, vars, drop = FALSE])
  candidate <- tryCatch({
    d <- eval(ctx$call$data, model_envir(model, envir))
    if (!is.data.frame(d) && !is.list(d)) stop("The fitting data are unavailable.")
    d <- as.data.frame(d)
    if (!is.null(ctx$call$subset)) {
      i <- eval(ctx$call$subset, d, model_envir(model, envir))
      d <- d[i, , drop = FALSE]
    }
    if (!all(vars %in% names(d))) stop("The fitting data lack required raw covariates.")
    tt <- stats::delete.response(ctx$terms)
    labels <- vapply(as.list(attr(tt, "variables"))[-1L],
      function(x) paste(deparse(x, width.cutoff = 500L), collapse = ""), character(1))
    check <- evaluate_terms(d, labels, model, envir)
    check <- check[, labels, drop = FALSE]
    # Match by fitting row identity, and verify evaluated values as well.
    i <- match(rownames(ctx$data), rownames(check))
    if (anyNA(i) || !isTRUE(all.equal(unname(as.list(check[i, , drop = FALSE])),
        unname(as.list(ctx$data[, labels, drop = FALSE])), check.attributes = FALSE))) {
      stop("Recovered data do not agree with the stored fitting observations.")
    }
    d[i, vars, drop = FALSE]
  }, error = identity)
  if (inherits(candidate, "error")) {
    cli::cli_abort(c("Cannot recover raw model covariates.",
      "x" = conditionMessage(candidate),
      "i" = "Supply reference {.arg data} and, if needed, {.arg envir}; stored transformed values cannot be inverted automatically."),
      class = "gratia_data_recovery_error", parent = candidate)
  }
  candidate
}

# Materialize model-frame expressions without evaluating a response. Returning
# the columns in terms order preserves positional offset and model.matrix use.
evaluated_model_frame <- function(model, data, envir = NULL) {
  tt <- stats::delete.response(stats::terms(model))
  exprs <- as.list(attr(tt, "variables"))[-1L]
  labels <- vapply(exprs, function(x) paste(deparse(x, width.cutoff = 500L),
    collapse = ""), character(1))
  # Use fitted prediction expressions (e.g. poly's coefficients) when present.
  pred <- as.list(attr(tt, "predvars"))[-1L]
  if (length(pred) != length(exprs)) pred <- exprs
  out <- data
  env <- model_envir(model, envir)
  stored <- !is.null(attr(data, "terms")) || isTRUE(attr(data, "gratia.evaluated"))
  for (i in seq_along(exprs)) {
    label <- labels[i]
    if (label %in% names(data) && (stored || is.symbol(exprs[[i]]) ||
        !all(all.vars(exprs[[i]]) %in% names(data)))) next
    ans <- tryCatch(eval(pred[[i]], data, env), error = identity)
    if (inherits(ans, "error")) expression_failure(label, ans)
    if (NROW(ans) != NROW(data)) expression_failure(label,
      simpleError("The expression must return one value or matrix row per observation."))
    out[[label]] <- ans
  }
  out <- as.data.frame(out[, labels, drop = FALSE])
  attr(out, "terms") <- tt
  out
}

# Native GAM prediction accepts a prepared model frame. This avoids evaluating
# local calls again inside mgcv, after their environment has been discarded.
predict_model <- function(object, newdata, ..., envir = NULL) {
  if (missing(newdata)) return(stats::predict(object, ...))
  object <- with_model_envir(object, envir)
  dots <- list(...)
  tt <- stats::delete.response(stats::terms(object))
  exprs <- as.list(attr(tt, "variables"))[-1L]
  if (!any(vapply(exprs, is.call, logical(1)))) {
    return(stats::predict(object, newdata = newdata, ...))
  }
  mf <- evaluated_model_frame(object, newdata)
  # mgcv and scam perform their own class/level checks on this frame.
  if (inherits(object, c("gam", "scam"))) {
    dots$newdata.guaranteed <- TRUE
  }
  do.call(stats::predict, c(list(object = object, newdata = mf), dots))
}
