# Only endpoint prediction is distributed. The caller accumulates these block
# summaries in their original order and generates all interval draws.
ale_block_summary <- function(object, reference, focal, breaks, bin, target, p,
                              exclude, dots) {
  predict_at <- function(nd, type) {
    do.call(predict_model, c(list(object = object, newdata = nd, type = type,
      exclude = exclude, na.action = stats::na.pass), dots))
  }
  lower <- upper <- as.data.frame(reference)
  lower[[focal]] <- breaks[bin]
  upper[[focal]] <- breaks[bin + 1L]
  # Reevaluate raw modified coordinates rather than stored expression columns.
  attr(lower, "terms") <- attr(upper, "terms") <- NULL
  attr(lower, "gratia.evaluated") <- attr(upper, "gratia.evaluated") <- NULL
  lo <- ale_endpoint(lower, target, p, predict_at)
  hi <- ale_endpoint(upper, target, p, predict_at)
  delta <- hi$value - lo$value
  D <- hi$gradient - lo$gradient
  rowsum(cbind(delta, D), bin, reorder = FALSE)
}

ale_parallel_available <- function() {
  rlang::is_installed(c("mirai", "carrier")) &&
    isTRUE(mirai::daemons_set())
}

ale_reference_batch <- function(object, blocks, focal, breaks, target, p,
                                exclude, dots) {
  lapply(blocks, function(block) {
    list(index = block$index,
      group = ale_block_summary(object, block$reference, focal, breaks,
        block$bin, target, p, exclude, dots))
  })
}

ale_reference_summaries <- function(object, reference, focal, grid, target, p,
                                    exclude, dots, block_size) {
  starts <- seq.int(1L, nrow(reference), by = block_size)
  n <- min(length(starts), mirai::info()[["connections"]])
  batches <- split(seq_along(starts), rep(seq_len(n), length.out = length(starts)))
  object <- ale_worker_model(object)
  variables <- as.list(attr(prediction_terms(object), "variables"))[-1L]
  needed <- unique(unlist(lapply(variables, all.vars)))
  reference <- reference[, intersect(needed, names(reference)), drop = FALSE]
  if (target$response) {
    # The adapter needs four bindings; its call frame also retains the model.
    environment(target$evaluate) <- list2env(mget(
      c("transform", "derivative", "post", "bounds"),
      envir = environment(target$evaluate), inherits = FALSE),
      parent = asNamespace("gratia"))
  }
  tasks <- lapply(batches, function(indices) {
    blocks <- lapply(indices, function(i) {
      rows <- seq.int(starts[i], min(nrow(reference), starts[i] + block_size - 1))
      list(index = i, reference = smooth_task_data(reference[rows, , drop = FALSE]),
        bin = grid$bin[rows])
    })
    list(object = object, blocks = blocks, focal = focal, breaks = grid$breaks,
      target = target, p = p, exclude = exclude, dots = dots)
  })
  # Send the model once per worker batch, rather than once per reference block.
  batches <- purrr::map(tasks, purrr::in_parallel(function(task) {
    do.call(get("ale_reference_batch", envir = asNamespace("gratia")), task)
  }))
  groups <- vector("list", length(starts))
  for (batch in batches) for (result in batch) groups[[result$index]] <- result$group
  groups
}

# Fresh workers must resolve exactly the same deterministic functions. A short
# allow-list keeps custom methods, dynamic lookups and stochastic formulas in
# the caller, including namespace-qualified calls outside this list.
ale_formula_functions <- function() {
  list(base = c("+", "-", "*", "/", "^", "%%", "%/%", ":", "(", "I",
    "abs", "sign", "sqrt", "exp", "expm1", "log", "log10", "log2", "log1p",
    "sin", "cos", "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh",
    "tanh", "asinh", "acosh", "atanh", "pmin", "pmax", "floor", "ceiling",
    "round", "trunc", "<", ">", "<=", ">=", "==", "!=", "!", "&", "|",
    "ifelse", "c", "cbind"),
    stats = c("offset", "plogis", "qlogis", "pnorm", "qnorm", "dnorm", "poly"))
}

# Inspect function bindings without invoking active bindings or forcing lazy
# values, which can consume random numbers merely when their name is looked up.
ale_lookup_function <- function(name, envir) {
  while (!identical(envir, emptyenv())) {
    if (exists(name, envir = envir, inherits = FALSE)) {
      if (bindingIsActive(name, envir) ||
          rlang::env_binding_are_lazy(envir, name)) return(NULL)
      return(get0(name, envir = envir, mode = "function", inherits = FALSE))
    }
    envir <- parent.env(envir)
  }
  NULL
}

ale_safe_expression <- function(expr, envir, variables) {
  if (missing(expr)) return(FALSE)
  if (is.symbol(expr)) return(as.character(expr) %in% variables)
  if (!is.call(expr)) return(is.atomic(expr) || is.null(expr))
  allowed <- ale_formula_functions()
  head <- expr[[1L]]
  if (is.symbol(head)) {
    name <- as.character(head)
    package <- if (name %in% allowed$base) "base" else "stats"
    if (!name %in% allowed[[package]]) return(FALSE)
    canonical <- get(name, envir = asNamespace(package), inherits = FALSE)
    actual <- ale_lookup_function(name, envir)
    if (!identical(actual, canonical)) return(FALSE)
  } else if (is.call(head) && length(head) == 3L &&
      identical(head[[1L]], as.name("::")) &&
      is.symbol(head[[2L]]) && is.symbol(head[[3L]])) {
    package <- as.character(head[[2L]])
    if (!package %in% names(allowed) ||
        !as.character(head[[3L]]) %in% allowed[[package]]) return(FALSE)
  } else return(FALSE)
  # Missing optional arguments are valid R syntax but cannot be inspected as
  # ordinary values. Keep such expressions on the caller's prediction path.
  tryCatch(all(vapply(as.list(expr)[-1L], ale_safe_expression, logical(1L),
    envir = envir, variables = variables)), error = function(e) FALSE)
}

ale_plain_column <- function(x) {
  (is.null(x) || (is.atomic(x) && typeof(x) %in% c("double", "integer",
    "logical", "character"))) &&
    all(class(x) %in% c("numeric", "integer", "logical", "character", "factor",
      "ordered", "AsIs", "matrix", "array", "NULL"))
}

ale_safe_contrast <- function(x, envir) {
  if (is.null(x)) return(TRUE)
  if (is.matrix(x)) return(is.numeric(x) && ale_plain_column(x))
  allowed <- c("contr.treatment", "contr.sum", "contr.helmert", "contr.poly", "contr.SAS")
  is.character(x) && length(x) == 1L && !is.na(x) && x %in% allowed &&
    identical(ale_lookup_function(x, envir),
      get(x, envir = asNamespace("stats"), inherits = FALSE))
}

ale_predict_in_caller <- function(object, reference, target = NULL, dots = list()) {
  # An additional model class can also supply formula or terms methods used
  # during prediction, even when it inherits mgcv's predict method unchanged.
  if (!all(class(object) %in% c("gam", "bam", "glm", "lm"))) return(TRUE)
  lookup <- new.env(parent = globalenv())
  lookup$predict <- stats::predict
  for (cl in class(object)) {
    method <- utils::getS3method("predict", cl, optional = TRUE, envir = lookup)
    if (is.null(method)) next
    env <- environment(method)
    if (!isNamespace(env) || getNamespaceName(env) != "mgcv" ||
        !identical(method, get0(paste0("predict.", cl), envir = env,
          inherits = FALSE))) return(TRUE)
    break
  }
  if (any(vapply(object$smooth, smooth_task_predict_in_caller, logical(1L),
    model = object))) return(TRUE)
  # Native prediction can invoke this callback even for link-scale requests.
  if (!is.null(object$family$predict)) return(TRUE)
  if (!is.null(target) && target$response) {
    links <- lapply(c("identity", "log", "logit", "probit", "cauchit",
      "cloglog", "sqrt", "1/mu^2", "inverse"), stats::make.link)
    evaluate <- environment(target$evaluate)
    canonical <- vapply(links, function(link) {
      identical(evaluate$transform, link$linkinv) &&
        identical(evaluate$derivative, link$mu.eta)
    }, logical(1L))
    if (!any(canonical)) return(TRUE)
  }
  if (!all(class(reference) %in% c("data.frame", "tbl_df", "tbl"))) return(TRUE)
  tt <- prediction_terms(object)
  expressions <- as.list(attr(tt, "variables"))[-1L]
  needed <- unique(unlist(lapply(expressions, all.vars)))
  predictors <- reference[intersect(needed, names(reference))]
  if (!all(vapply(predictors, ale_plain_column, logical(1L))) ||
      !all(vapply(dots, ale_plain_column, logical(1L)))) return(TRUE)
  env <- model_envir(object)
  contrasts <- c(object$contrasts, lapply(predictors, attr, which = "contrasts"))
  if (!all(vapply(contrasts, ale_safe_contrast, logical(1L), envir = env))) return(TRUE)
  pred <- as.list(attr(tt, "predvars"))[-1L]
  all_expressions <- c(expressions, pred)
  !all(vapply(all_expressions, ale_safe_expression, logical(1L),
    envir = env, variables = names(reference)))
}

# Compatible formulas depend only on supplied columns and known namespace
# functions. Remove fitting environments from the worker copy, never the model
# used by the caller for recovery, custom prediction or interval calculations.
ale_worker_model <- function(object) {
  compact <- function(x) {
    if (inherits(x, c("formula", "terms"))) {
      environment(x) <- asNamespace("stats")
    } else if (is.list(x)) x[] <- lapply(x, compact)
    x
  }
  for (name in c("formula", "terms", "pterms", "pred.formula")) {
    object[[name]] <- compact(object[[name]])
  }
  # do.call() can store an evaluated formula in the fitting call as well.
  object$call$formula <- compact(object$call$formula)
  object$smooth <- lapply(object$smooth, smooth_task_smooth)
  attr(object$model, "terms") <- compact(attr(object$model, "terms"))
  attr(object, "gratia.envir") <- asNamespace("stats")
  object
}
