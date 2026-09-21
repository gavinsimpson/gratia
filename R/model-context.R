# Extract model structure without evaluating the fitting call. Keep this
# boundary separate from recovery so new fitters need only small adapters.
model_context <- function(object, component = NULL) {
  UseMethod("model_context")
}

#' @export
model_context.default <- function(object, component = NULL) {
  tt <- stats::terms(object)
  ff <- stats::formula(object)
  if (is.list(ff)) {
    # An explicit component gets its own predictors and evaluation environment.
    # Without one, retain combined terms for full-model prediction.
    selected <- if (is.null(component)) 1L else component
    if (is.character(selected)) selected <- match(selected, names(ff))
    if (length(selected) != 1L || is.na(selected) || !selected %in% seq_along(ff)) {
      cli::cli_abort("Unknown model component.")
    }
    ff <- ff[[selected]]
    if (!is.null(component) && is.list(object$pterms)) {
      pt <- stats::delete.response(object$pterms[[selected]])
      expressions <- as.list(attr(pt, "variables"))[-1L]
      indices <- lss_eta_index(object)[[selected]]
      for (sm in object$smooth) {
        if (any(smooth_coef_indices(sm) %in% indices)) {
          expressions <- c(expressions,
            lapply(terms_in_smooth(sm), term_expression, model = object))
        }
      }
      # Build a one-sided formula from language objects, preserving offsets.
      rhs <- Reduce(function(x, y) call("+", x, y), expressions, init = 1)
      component_formula <- stats::as.formula(call("~", rhs), env = environment(ff))
      tt <- stats::terms(component_formula)
    }
  }
  list(model = object, component = component, formula = ff, terms = tt,
    data = stats::model.frame(object), provenance = "evaluated",
    call = object$call, envir = environment(ff),
    na.action = stats::na.action(object))
}
#' @export
model_context.gam <- model_context.default
#' @export
model_context.scam <- model_context.default
#' @export
model_context.gamm <- function(object, component = NULL) {
  model_context(object$gam, component)
}
#' @export
model_context.list <- function(object, component = NULL) {
  if (!is_gamm4(object)) {
    cli::cli_abort("No model context adapter for this list.")
  }
  model_context(object$gam, component)
}
#' @export
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
  if (is.null(envir)) envir <- if (is.null(model)) globalenv() else model_context(model)$envir
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
                           evaluated = FALSE, fallback = NULL) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  out <- data
  recovered <- character()
  env <- model_envir(model, envir)
  for (label in unique(labels)) {
    expr <- term_expression(label, model)
    deps <- all.vars(expr)
    if (label %in% names(data) &&
        (evaluated || is.symbol(expr) || !all(deps %in% names(data)))) next
    ans <- tryCatch({
      # Covariate vectors must come from data, not a same-named global vector.
      missing <- setdiff(intersect(deps, names(model$var.summary)), names(data))
      if (length(missing)) stop("Variable(s) ", paste0("'", missing, "'", collapse = ", "), " not found in 'data'.")
      eval(expr, envir = data, enclos = env)
    }, error = identity)
    if (inherits(ans, "error")) {
      # Only a caller handling known training observations supplies fallback.
      # Equal row counts alone never authorize using it for new observations.
      can_recover <- !is.null(fallback) && label %in% names(fallback) &&
        identical(rownames(data), rownames(fallback))
      if (!can_recover) expression_failure(label, ans)
      recovered <- c(recovered, paste0(label, ": ", conditionMessage(ans)))
      ans <- fallback[[label]]
    }
    if (is.null(ans) || NROW(ans) != NROW(data)) {
      expression_failure(label, simpleError("The expression must return one value or matrix row per observation."))
    }
    out[[label]] <- ans
  }
  if (length(recovered)) {
    cli::cli_inform(c("Using stored evaluated columns for fitting observations.",
      "i" = paste(recovered, collapse = "; "),
      "i" = "Supply {.arg envir} to evaluate these expressions from raw covariates."),
      class = "gratia_expression_recovery")
  }
  out
}

prepare_smooth_data <- function(model, smooth, data = NULL, envir = NULL) {
  # A missing data argument denotes training data supplied by the adapter.
  # Explicit data, including same-length new data, never get this fallback.
  fallback <- NULL
  if (is.null(data)) {
    data <- model_context(model)$data
    fallback <- stats::model.frame(model)
  }
  evaluate_terms(data, terms_in_smooth(smooth), model, envir, fallback = fallback,
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
  tt <- prediction_terms(model)
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
    ans <- tryCatch({
      # Missing predictor columns must not resolve to same-named global vectors.
      missing <- setdiff(intersect(all.vars(exprs[[i]]), model_vars(model)), names(data))
      if (length(missing)) stop("Variable(s) ", paste0("'", missing, "'", collapse = ", "), " not found in 'data'.")
      eval(pred[[i]], data, env)
    }, error = identity)
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

# A multivariate model can have additional responses in the combined terms
# object. Exclude them too, unless they are predictors in another equation.
prediction_terms <- function(model) {
  tt <- stats::delete.response(stats::terms(model))
  ff <- stats::formula(model)
  if (!is.list(ff)) ff <- list(ff)
  responses <- unique(unlist(lapply(ff, function(f) {
    if (length(f) == 3L) all.vars(f[[2L]]) else character()
  })))
  responses <- setdiff(responses, model_vars(model))
  labels <- attr(tt, "term.labels")
  drop <- which(labels %in% responses)
  if (length(drop)) tt <- stats::drop.terms(tt, drop, keep.response = FALSE)
  tt
}
