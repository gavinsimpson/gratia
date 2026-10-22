# Compare only nonempty, aligned, finite numerical results.
check_example_values <- function(values, expected, se, n) {
  if (!is.data.frame(values) ||
      !all(c(".estimate", ".se") %in% names(values))) {
    stop("Smooth check requires .estimate and .se columns.", call. = FALSE)
  }
  if (length(n) != 1L || !is.finite(n) || n < 1L ||
      n != floor(n) || nrow(values) != n ||
      length(expected) != n || length(se) != n ||
      length(values[[".estimate"]]) != n || length(values[[".se"]]) != n) {
    stop("Smooth check requires nonempty results with matching row counts.",
      call. = FALSE)
  }
  vectors <- list(values[[".estimate"]], values[[".se"]], expected, se)
  if (!all(vapply(vectors, function(x) is.numeric(x) &&
      all(is.finite(x)), logical(1)))) {
    stop("Smooth check requires finite numeric estimates and standard errors.",
      call. = FALSE)
  }
  errors <- c(estimate_error = max(abs(values[[".estimate"]] - expected)),
    standard_error_error = max(abs(values[[".se"]] - se)))
  if (any(!is.finite(errors))) {
    stop("Smooth check produced a non-finite comparison error.", call. = FALSE)
  }
  errors
}

# Keep this independent of gratia's equation-index helper: use the original
# GAM bases and the dimensions stored by GJRM's parent fit.
check_example_smooths <- function(fit, estimate = gratia::smooth_estimates,
    tolerance = 1e-10) {
  withCallingHandlers({
    if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance <= 0) {
      stop("Supply one positive finite comparison tolerance.", call. = FALSE)
    }
    cuts <- if (is.null(fit$VC$K1)) 0L else fit$VC$K1 - 1L
    if (!is.null(fit$VC$K2)) cuts <- cuts + fit$VC$K2 - 1L
    offset <- cuts
    selected <- 0L
    checks <- list()
    labels <- gratia::smooths(fit)
    fixed <- isTRUE(fit$fixed.margins) || isTRUE(fit$VC$fixed.margins)
    for (equation in 1:9) {
      gam <- fit[[paste0("gam", equation)]]
      width <- fit[[paste0("X", equation, ".d2")]]
      if (is.null(width)) width <- 0L
      keep <- inherits(gam, "gam") && !(fixed && equation < 5L)
      if (keep) {
        ordinal <- (equation == 1L && !is.null(fit$VC$K1)) ||
          (equation == 2L && !is.null(fit$VC$K2))
        for (sm in gam$smooth) {
          selected <- selected + 1L
          # Retain every applicable observation, including all random-effect
          # levels. Both calculations must use the same factor-by rows.
          newdata <- gam$model
          if (!is.null(sm$by.level)) {
            newdata <- newdata[!is.na(newdata[[sm$by]]) &
              newdata[[sm$by]] == sm$by.level, , drop = FALSE]
          }
          if (!is.data.frame(newdata) || nrow(newdata) == 0L) {
            stop("No evaluation rows for equation ", equation, ", smooth ",
              sm$label, ".", call. = FALSE)
          }
          local <- seq.int(sm$first.para, sm$last.para)
          if (ordinal) local <- local - 1L
          columns <- if (fixed) local else offset + local
          X <- mgcv::PredictMat(sm, newdata)
          values <- estimate(fit, select = selected, data = newdata,
            overall_uncertainty = FALSE)
          expected <- drop(X %*% fit$coefficients[columns])
          V <- fit$Vb[columns, columns, drop = FALSE]
          se <- sqrt(rowSums((X %*% V) * X))
          errors <- check_example_values(values, expected, se, nrow(newdata))
          if (any(errors >= tolerance)) {
            stop("Smooth estimates or standard errors disagree in equation ",
              equation, ", smooth ", sm$label, ".", call. = FALSE)
          }
          checks[[length(checks) + 1L]] <- data.frame(
            equation = equation, smooth = labels[selected],
            evaluation_rows = nrow(newdata),
            estimate_error = unname(errors[["estimate_error"]]),
            standard_error_error = unname(errors[["standard_error_error"]]))
        }
      }
      offset <- offset + width
    }
    if (!length(checks) || selected != length(labels)) {
      stop("Numerical checks did not cover every fitted smooth.", call. = FALSE)
    }
    do.call(rbind, checks)
  }, warning = function(w) {
    stop("Numerical smooth check raised a warning: ", conditionMessage(w),
      call. = FALSE)
  })
}
