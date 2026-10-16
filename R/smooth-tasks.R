# Map explicit calculation inputs without capturing a fitted model or caller
# environment. The caller owns the daemon pool and all random draws.
smooth_task_map <- function(tasks, worker) {
  if (length(tasks) > 1L &&
      rlang::is_installed(c("mirai", "carrier")) &&
      isTRUE(mirai::daemons_set())) {
    purrr::map(tasks, purrr::in_parallel(
      function(task) do.call(get(worker, envir = asNamespace("gratia")), task),
      worker = worker))
  } else {
    purrr::map(tasks, function(task) {
      do.call(get(worker, envir = asNamespace("gratia")), task)
    })
  }
}

# Prediction needs the model class and these flags, not the fitting data.
smooth_matrix_model <- function(model) {
  out <- structure(list(dinfo = if (!is.null(model$dinfo)) TRUE else NULL),
    class = class(model))
  attr(out, "gratia.scam.centre") <- attr(model, "gratia.scam.centre")
  out
}

# Custom prediction methods can depend on caller functions, data or S3
# registrations. Evaluate those bases in the caller and send their matrices.
# Only methods in namespaces loaded by smooth_predict_matrix() are safe on
# fresh workers. A gam fit can use scam bases without having the scam class.
smooth_task_predict_in_caller <- function(smooth, model = NULL) {
  lookup <- new.env(parent = globalenv())
  lookup$Predict.matrix <- mgcv::Predict.matrix
  packages <- c("mgcv", if (inherits(model, "scam")) "scam")
  needs_caller <- function(classes) {
    for (class in classes) {
      method <- utils::getS3method("Predict.matrix", class,
        optional = TRUE, envir = lookup)
      if (is.null(method)) next
      env <- environment(method)
      name <- paste0("Predict.matrix.", class)
      if (!isNamespace(env) ||
          !getNamespaceName(env) %in% packages ||
          !identical(method, get0(name, envir = env, inherits = FALSE))) {
        return(TRUE)
      }
    }
    FALSE
  }
  if (needs_caller(c(class(smooth), "default"))) return(TRUE)
  # Factor-smooth methods dispatch again using their original basis class.
  if (is.list(smooth[["base"]]) &&
      needs_caller(smooth[["base"]][["bs"]])) return(TRUE)
  # Tensor products dispatch separately for each marginal basis.
  margins <- smooth[["margin"]]
  is.list(margins) && any(vapply(margins, function(margin) {
    is.list(margin) && smooth_task_predict_in_caller(margin, model)
  }, logical(1L)))
}

# Prepared coordinates no longer need model-frame terms or their fitting
# environment. Keep column attributes, including factor contrasts and matrix
# columns. Normalize the outer class as grouped or spatial frames need
# attributes that no longer describe these selected worker coordinates.
smooth_task_data <- function(data) {
  out <- lapply(data, identity)
  frame_class <- if (inherits(data, "tbl_df")) {
    c("tbl_df", "tbl", "data.frame")
  } else "data.frame"
  attributes(out) <- list(names = names(data),
    row.names = .row_names_info(data, 0L), class = frame_class)
  out
}

# Random-effect margins keep a formula environment that can contain the whole
# fitting session. Ordinary factor formulas use only supplied columns and base
# formula operators. Leave formulas containing other calls untouched, because
# a custom prediction method may need functions from their environment.
smooth_task_smooth <- function(smooth) {
  if (!is.list(smooth)) return(smooth)
  if (inherits(smooth, "random.effect") && inherits(smooth$form, "formula")) {
    allowed <- c(smooth$term, "~", "+", "-", "*", ":", "(")
    if (all(all.names(smooth$form) %in% allowed)) {
      environment(smooth$form) <- baseenv()
    }
  }
  if (is.list(smooth[["margin"]])) {
    smooth[["margin"]] <- lapply(smooth[["margin"]], smooth_task_smooth)
  }
  smooth
}

# The first worker pass returns compact designs and SEs. Draw only for terms
# with a valid positive SE, as in the serial interval calculation, then send
# each design with its own coefficient deviations for calibration. Keeping
# random draws in the caller preserves their order and the caller's RNG kind.
smooth_interval_map <- function(results, columns, V, level, n_sim, ncores,
                                estimate, critical) {
  tasks <- lapply(seq_along(results), function(i) {
    result <- results[[i]]
    valid <- !is.na(result$data[[estimate]]) & !is.na(result$data$.se) &
      rowSums(is.na(result$X)) == 0L
    deviations <- if (any(valid & result$data$.se > 0)) {
      simultaneous_coefficient_deviations(V, n_sim, ncores)[, columns[[i]], drop = FALSE]
    } else NULL
    list(result = result, level = level, n_sim = n_sim,
      deviations = deviations, estimate = estimate, critical = critical)
  })
  smooth_task_map(tasks, "calculate_smooth_interval")
}

calculate_smooth_interval <- function(result, level, n_sim, deviations,
                                      estimate, critical) {
  bounds <- simultaneous_intervals(result$data[[estimate]], result$data$.se,
    result$X, result$V, level = level, n_sim = n_sim, deviations = deviations)
  if (critical) {
    crit <- rep(bounds$critical, nrow(result$data))
    crit[is.na(bounds$lower)] <- NA_real_
    tibble::add_column(result$data, .crit = crit,
      .lower_ci = bounds$lower, .upper_ci = bounds$upper)
  } else {
    tibble::add_column(result$data,
      .lower_ci = bounds$lower, .upper_ci = bounds$upper, .after = 6L)
  }
}
