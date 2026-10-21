# Coefficients are stored as time x design column x response component. The
# ordinary coefficient and temporal deviation share the fitted covariance.
sdmtmb_time_varying <- function(object, component, fixed, X, covariance) {
  if (is.null(object$time_varying)) return(NULL)
  Z <- object$tmb_data$X_rw_ik
  spec <- object$design_specs$time_varying
  dimensions <- dim(object$parlist$b_rw_t)
  lookup <- object$time_lu
  columns <- colnames(Z)
  if (!is.matrix(Z) || !is.data.frame(lookup) || is.null(spec$terms) ||
      !all(c("year_i", "time_from_data") %in% names(lookup)) ||
      length(dimensions) != 3L || dimensions[2L] != ncol(Z) ||
      dimensions[1L] != nrow(lookup) || dimensions[3L] < as.integer(component) ||
      is.null(columns) || anyDuplicated(columns) ||
      length(attr(Z, "assign")) != ncol(Z) ||
      length(attr(X, "assign")) != ncol(X) ||
      !identical(columns, spec$columns) ||
      !setequal(lookup$year_i, seq_len(dimensions[1L]) - 1L)) {
    stop("Cannot align sdmTMB time-varying coefficients and stored designs.",
      call. = FALSE)
  }
  index <- array(sdmtmb_parameter_indices(object, "b_rw_t"), dimensions)
  index <- matrix(index[, , as.integer(component), drop = FALSE],
    dimensions[1L], dimensions[2L])
  assignment <- attr(Z, "assign")
  ordinary_assignment <- attr(X, "assign")
  labels <- attr(spec$terms, "term.labels")
  ordinary_labels <- attr(object$terms[[as.integer(component)]], "term.labels")
  baseline <- match(columns, colnames(X))
  if (identical(as.integer(object$tmb_data$random_walk), 1L)) {
    baseline[] <- NA_integer_
  } else {
    # Matching names alone cannot detect different factor contrasts or bases.
    for (j in seq_along(labels)) {
      ordinary_term <- match(labels[j], ordinary_labels)
      if (!is.na(ordinary_term) && !setequal(
          columns[assignment == j], colnames(X)[ordinary_assignment == ordinary_term])) {
        stop("Cannot align sdmTMB time-varying baseline for '", labels[j],
          "': use compatible coding in the main and time-varying formulas.",
          call. = FALSE)
      }
    }
    for (j in which(!is.na(baseline))) {
      if (!isTRUE(all.equal(unname(Z[, j]), unname(X[, baseline[j]]),
          check.attributes = FALSE, tolerance = 1e-8))) {
        stop("Cannot align sdmTMB time-varying baseline for '", columns[j],
          "': use compatible coding in the main and time-varying formulas.",
          call. = FALSE)
      }
    }
  }
  indices <- unique(c(as.vector(index), fixed[baseline[!is.na(baseline)]]))
  A <- matrix(0, length(index), length(indices))
  A[cbind(seq_len(length(index)), match(as.vector(index), indices))] <- 1
  for (j in which(!is.na(baseline))) {
    rows <- seq_len(dimensions[1L]) + (j - 1L) * dimensions[1L]
    A[rows, match(fixed[baseline[j]], indices)] <- 1
  }
  consumed <- ordinary_labels[vapply(seq_along(ordinary_labels), function(j) {
    take <- which(ordinary_assignment == j)
    length(take) > 0L && all(take %in% baseline)
  }, logical(1))]
  list(
    labels = paste0("time_varying(",
      ifelse(columns == "(Intercept)", "Intercept",
        ifelse(columns == "Intercept", "`Intercept`", columns)), ")"),
    columns = columns, consumed = consumed, indices = indices, transform = A,
    parameters = unname(object$last.par.best[indices]),
    covariance = if (covariance) sdmtmb_joint_covariance(object, indices),
    time = object$time,
    times = lookup$time_from_data[match(seq_len(dimensions[1L]) - 1L, lookup$year_i)]
  )
}

sdmtmb_time_varying_effects <- function(
  object, terms = NULL, data = NULL, unnest = TRUE,
  back_transform = FALSE, transform = lifecycle::deprecated(), ...
) {
  back_transform <- parametric_back_transform(back_transform, transform,
    supplied = !missing(back_transform))
  available <- names(parametric_terms(object))
  if (is.null(terms)) terms <- mixed_default_parametric_terms(object)
  if (!all(terms %in% available)) {
    stop("Term is not in the parametric part of model: ",
      paste(setdiff(terms, available), collapse = ", "), call. = FALSE)
  }
  temporal <- object$time_varying
  suffix <- if (object$predictor == 1L) "" else paste0(".", object$predictor - 1L)
  labels <- paste0(temporal$labels, suffix)
  ordinary <- object
  ordinary$time_varying <- NULL
  results <- list()
  want <- setdiff(terms, labels)
  if (length(want)) {
    results[[1L]] <- parametric_effects(ordinary, terms = want,
      data = data, unnest = unnest, back_transform = back_transform, ...)
  }
  selected <- which(labels %in% terms)
  if (length(selected)) {
    times <- temporal$times
    keep <- seq_along(times)
    if (!is.null(data)) {
      if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
      if (temporal$time %in% names(data)) {
        requested <- data[[temporal$time]]
        if (anyNA(requested) || any(!requested %in% times)) {
          stop("Time-varying evaluation requires non-missing stored times; ",
            "refit with 'extra_time' to add time steps.", call. = FALSE)
        }
        keep <- which(times %in% requested)
      }
    }
    estimate <- drop(temporal$transform %*% temporal$parameters)
    variance <- rowSums((temporal$transform %*% temporal$covariance) * temporal$transform)
    for (j in selected) {
      rows <- keep + (j - 1L) * length(times)
      effect <- tibble::tibble(
        .term = labels[j], .type = "time_varying", .value = times[keep],
        .partial = unname(estimate[rows]), .se = unname(sqrt(pmax(0, variance[rows])))
      )
      if (!is.numeric(times) && !inherits(times, "Date") && !inherits(times, "POSIXt")) {
        effect$.value <- factor(effect$.value, levels = unique(times), ordered = TRUE)
      }
      if (!unnest) {
        effect <- tidyr::nest(effect, data = c(".value", ".partial", ".se"))
      }
      info <- list(time = temporal$time, coefficient = temporal$columns[j],
        model_label = if (has_multiple_predictors(object)) {
          mixed_model_label(object, object$component)
        })
      attr(effect, "time_varying_info") <- stats::setNames(list(info), labels[j])
      attr(effect, "back_transform") <- back_transform
      results[[length(results) + 1L]] <- effect
    }
  }
  mixed_bind_parametric_effects(results)
}
