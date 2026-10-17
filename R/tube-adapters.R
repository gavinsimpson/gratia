tube_check_saved_controls <- function(dots) {
  controls <- intersect(dots, c("simultaneous_method", "simultaneous_domain", "tube_control"))
  if (length(controls)) {
    cli::cli_abort("Interval computation controls require the fitted model; recompute the intervals before drawing saved estimates.")
  }
  invisible(NULL)
}

tube_record <- function(info, domain, coordinate, identity) {
  c(list(
    identity = identity, coordinate = coordinate,
    interval = domain$interval, domain_source = domain$source
  ), info)
}

tube_smooth_intervals <- function(estimates, smooths, model, ci_level,
                                  unconditional, frequentist, overall_uncertainty,
                                  domain, control) {
  tube_check_model(model)
  V <- get_vcov(model, unconditional = unconditional, frequentist = frequentist)
  records <- vector("list", length(estimates))
  for (i in seq_along(estimates)) {
    sm <- smooths[[i]]
    pieces <- estimates[[i]]$data
    sizes <- vapply(pieces, nrow, integer(1))
    d <- dplyr::bind_rows(pieces)
    if (!nrow(d)) cli::cli_abort("Tube calibration needs evaluation points for each selected smooth.")
    coordinate <- sm$term[1L]
    resolved <- tube_domain(domain, coordinate, d[[coordinate]])
    basis <- tube_basis(model, sm, d, resolved$interval)
    design <- smooth_uncertainty_design(
      sm, basis$evaluate(d[[coordinate]]),
      model, V, overall_uncertainty
    )
    cols <- design$cols
    evaluate <- function(x, deriv = 0L) {
      X <- basis$evaluate(x, deriv)
      out <- smooth_uncertainty_design(sm, X, model, V, overall_uncertainty)$X
      if (deriv > 0L) out[, !cols %in% smooth_coef_indices(sm)] <- 0
      out
    }
    band <- tube_band(
      d$.estimate, d$.se, design$X, V[cols, cols, drop = FALSE],
      evaluate, resolved$interval, basis$knots, ci_level, control
    )
    d$.crit <- rep(band$critical, nrow(d))
    d$.lower_ci <- band$lower
    d$.upper_ci <- band$upper
    start <- 0L
    for (j in seq_along(pieces)) {
      estimates[[i]]$data[[j]] <- d[start + seq_len(sizes[j]), , drop = FALSE]
      start <- start + sizes[j]
    }
    records[[i]] <- tube_record(band$info, resolved, coordinate, sm$label)
  }
  attr(estimates, "tube_records") <- records
  estimates
}

tube_derivative_intervals <- function(results, tasks, model, level, domain, control) {
  tube_check_model(model)
  records <- vector("list", length(results))
  for (i in seq_along(results)) {
    task <- tasks[[i]]
    sm <- task$sm
    coordinate <- task$focal
    if (!identical(coordinate, sm$term)) {
      cli::cli_abort("Tube derivative bands currently differentiate on the smooth coordinate; use wrt = 'smooth' or simulation.")
    }
    d <- results[[i]]$data
    resolved <- tube_domain(domain, coordinate, d[[coordinate]])
    deltas <- if (task$order == 1L) {
      switch(task$tube_type,
        forward = c(0, task$h),
        backward = c(-task$h, 0),
        central = c(-task$h / 2, task$h / 2)
      )
    } else {
      switch(task$tube_type,
        forward = c(0, task$h, 2 * task$h),
        backward = c(-2 * task$h, -task$h, 0),
        central = c(-task$h, 0, task$h)
      )
    }
    limits <- resolved$interval + range(deltas)
    basis <- tube_basis(model, sm, task$data, limits)
    evaluate <- tube_derivative_evaluator(basis, task$order, task$tube_type, task$h)
    X <- evaluate(d[[coordinate]])
    # Same finite-difference functional, evaluated by integration rather than
    # cancellation-prone subtraction. The default simulation path is unchanged.
    d$.derivative <- stats::setNames(drop(X %*% task$betas), names(d$.derivative))
    d$.se <- stats::setNames(sqrt(pmax(0, rowSums((X %*% task$V) * X))), names(d$.se))
    breaks <- as.vector(outer(basis$knots, deltas, "-"))
    band <- tube_band(
      d$.derivative, d$.se, X, task$V, evaluate,
      resolved$interval, breaks, level, control
    )
    d$.crit <- rep(band$critical, nrow(d))
    d$.lower_ci <- band$lower
    d$.upper_ci <- band$upper
    results[[i]] <- d
    records[[i]] <- c(
      tube_record(band$info, resolved, coordinate, sm$label),
      list(order = task$order, stencil = task$tube_type, step = task$h)
    )
  }
  attr(results, "tube_records") <- records
  results
}

tube_difference_intervals <- function(results, tasks, model, smooths, data,
                                      by_var, level, domain, control) {
  tube_check_model(model)
  records <- vector("list", length(results))
  for (i in seq_along(results)) {
    task <- tasks[[i]]
    d <- results[[i]]$data
    candidates <- smooths[vapply(smooths, function(sm) {
      by_level(sm) %in%
        c(task$f1, task$f2)
    }, logical(1))]
    if (length(candidates) != 2L) {
      cli::cli_abort("Tube differences require one selected smooth for each compared factor level.")
    }
    coordinate <- candidates[[1L]]$term[1L]
    resolved <- tube_domain(domain, coordinate, d[[coordinate]])
    columns <- match(names(task$coefs), names(stats::coef(model)))
    # Inspect terms as well as returned rows: one evaluation point cannot
    # reveal a group slope that varies elsewhere in the coverage interval.
    tube_check_contrast_terms(model, columns, candidates, coordinate)
    basis <- lapply(candidates, function(sm) {
      template <- data[data[[by_var]] == by_level(sm), , drop = FALSE]
      list(
        basis = tube_basis(model, sm, template, resolved$interval),
        positions = match(smooth_coef_indices(sm), columns),
        sign = if (by_level(sm) == task$f1) 1 else -1
      )
    })
    if (any(vapply(basis, function(b) anyNA(b$positions), logical(1)))) {
      cli::cli_abort("Tube contrast coefficients do not match the selected smooths.")
    }
    evaluate_smooths <- function(x, deriv = 0L) {
      out <- matrix(0, length(x), length(columns))
      for (b in basis) {
        out[, b$positions] <- out[, b$positions, drop = FALSE] +
          b$sign * b$basis$evaluate(x, deriv)
      }
      out
    }
    X <- task$first - task$second
    remainder <- X - evaluate_smooths(d[[coordinate]])
    constant <- remainder[1L, ]
    if (max(abs(sweep(remainder, 2L, constant))) > 1e-8 * max(1, abs(X))) {
      cli::cli_abort("Tube differences require fixed nuisance values throughout each comparison.")
    }
    evaluate <- function(x, deriv = 0L) {
      out <- evaluate_smooths(x, deriv)
      if (deriv == 0L) out <- sweep(out, 2L, constant, "+")
      out
    }
    band <- tube_band(
      d$.diff, d$.se, X, task$V, evaluate,
      resolved$interval, unlist(lapply(basis, function(b) b$basis$knots)),
      level, control
    )
    results[[i]] <- tibble::add_column(d,
      .lower_ci = band$lower,
      .upper_ci = band$upper, .after = 6L
    )
    records[[i]] <- c(
      tube_record(band$info, resolved, coordinate, d$.smooth[1L]),
      list(level_1 = task$f1, level_2 = task$f2)
    )
  }
  attr(results, "tube_records") <- records
  results
}

tube_check_contrast_terms <- function(model, columns, smooths, coordinate) {
  variables <- all.vars(str2lang(coordinate))
  parametric <- columns[columns <= model$nsdf]
  if (length(parametric)) {
    if (!is.numeric(model$assign) || !inherits(model$pterms, "terms")) {
      cli::cli_abort("Cannot verify constant group contributions for this model; use simulation instead.")
    }
    ids <- unique(model$assign[parametric])
    labels <- attr(model$pterms, "term.labels")[ids[ids > 0L]]
    varying <- vapply(labels, function(term) {
      any(all.vars(str2lang(term)) %in% variables)
    }, logical(1))
    if (any(varying)) {
      cli::cli_abort("Tube differences require group contributions that are constant along the smooth coordinate; use simulation for group slopes.")
    }
  }
  selected <- unlist(lapply(smooths, smooth_coef_indices))
  for (sm in model$smooth) {
    extra <- setdiff(intersect(smooth_coef_indices(sm), columns), selected)
    if (length(extra) && any(unlist(lapply(sm$term, function(term) all.vars(str2lang(term)))) %in% variables)) {
      cli::cli_abort("Tube differences require additional smooth contributions to be constant along the smooth coordinate; use simulation instead.")
    }
  }
  invisible(NULL)
}
