# Prepare each smooth once, then retain the joint covariance for every pair.
difference_smooth_derivatives <- function(model, select, ids, smooths,
    data, n, n_2d, n_3d, n_4d, focal, wrt, type, order, eps,
    unconditional, frequentist, interval, level, n_sim, n_cores, seed) {
  if (is.null(focal) && any(vapply(smooths, smooth_dim, numeric(1L)) > 1L)) {
    cli::cli_abort("Supply {.arg focal} for partial derivatives of smooth differences.")
  }
  if (!is.null(focal) && (!is.character(focal) || length(focal) != 1L || is.na(focal))) {
    cli::cli_abort("{.arg focal} must be one coordinate name.")
  }
  focals <- vapply(smooths, function(sm) derivative_focal(model, sm, focal, wrt),
    character(1L))
  if (length(unique(focals)) != 1L) {
    cli::cli_abort("Compared smooths must use the same differentiation coordinate.")
  }
  focal <- focals[[1L]]
  h <- derivative_step(model, focal, data, eps, order, type)
  if (is.null(data)) {
    data <- dplyr::bind_rows(lapply(ids, function(id) {
      d <- smooth_data(model, id, n = n, n_2d = n_2d,
        n_3d = n_3d, n_4d = n_4d,
        var_order = smooth_plot_variables(model, id),
        back_transform = identical(wrt, "covariate"))
      # Retain the fitted range even when the requested grid has one point.
      reference <- if (wrt == "covariate") {
        recover_raw_data(model, vars = focal)[[focal]]
      } else prepare_smooth_data(model, model$smooth[[id]])[[focal]]
      bounds <- seq_min_max_eps(range(reference, na.rm = TRUE), 2L,
        order, type, h)
      x <- sort(unique(d[[focal]]))
      if (!is.numeric(x) || !length(x)) {
        cli::cli_abort("The differentiation coordinate must be a numeric vector.")
      }
      if (bounds[[1L]] > bounds[[2L]] ||
          (length(x) > 1L && bounds[[1L]] == bounds[[2L]])) {
        cli::cli_abort("{.arg eps} is too large for the automatic differentiation grid.")
      }
      adjusted <- seq(bounds[[1L]], bounds[[2L]], length.out = length(x))
      d[[focal]] <- adjusted[match(d[[focal]], x)]
      # Reevaluate expressions only for a grid generated on raw coordinates.
      attr(d, "gratia.evaluated") <- identical(wrt, "smooth")
      attr(d, "terms") <- NULL
      prepare_smooth_data(model, model$smooth[[id]], d)
    }))
  } else {
    data <- tibble::as_tibble(data)
    # Normalize factors before evaluating expressions that depend on them.
    for (sm in smooths) {
      by <- by_variable(sm)
      if (is.character(data[[by]])) {
        template <- prepare_smooth_data(model, sm)[[by]]
        data[[by]] <- factor(data[[by]], levels = levels(template),
          ordered = is.ordered(template))
      }
    }
    for (sm in smooths) data <- prepare_smooth_data(model, sm, data)
  }
  if (!is.numeric(data[[focal]]) || is.matrix(data[[focal]])) {
    cli::cli_abort("The differentiation coordinate must be a numeric vector.")
  }
  comparison <- difference_smooth_pairs(smooths, select, data, derivative = TRUE)
  by <- comparison$by
  variables <- comparison$variables
  match_variables <- variables
  if (wrt == "covariate") {
    raw <- unique(unlist(lapply(variables, function(v) {
      all.vars(term_expression(v, model))
    })))
    raw <- intersect(raw, names(model$var.summary))
    missing <- setdiff(raw, names(data))
    if (length(missing)) {
      cli::cli_abort("Supply raw covariates in {.arg data}: {.val {missing}}.")
    }
    # Compare the same independent raw inputs. Expressions may legitimately
    # differ between groups when they involve the comparison factor.
    match_variables <- setdiff(raw, by)
    variables <- union(variables, match_variables)
  }

  betas <- stats::coef(model)
  V <- get_vcov(model, unconditional = unconditional, frequentist = frequentist)
  matrices <- lapply(smooths, function(sm) {
    rows <- which(data[[by]] == by_level(sm))
    d <- data[rows, , drop = FALSE]
    stencil <- smooth_derivative_stencil(model, sm, d, focal, wrt, type, order, h)
    cols <- smooth_coef_indices(sm)
    X <- smooth_derivative_matrix(sm, stencil, model, h, order,
      rownames(d), names(betas)[cols])
    list(rows = rows, columns = cols, X = X)
  })
  matrix_levels <- vapply(smooths, by_level, character(1L))
  label <- smooth_label(smooths[[1L]])
  # Use the model's smooth label rather than parsing expressions with a regex.
  label <- sub(paste0(":", by, by_level(smooths[[1L]])), "", label, fixed = TRUE)
  tasks <- purrr::pmap(comparison$pairs, function(f1, f2) {
    rows <- difference_smooth_rows(data, by, f1, f2, match_variables)
    a <- matrices[[match(f1, matrix_levels)]]
    b <- matrices[[match(f2, matrix_levels)]]
    cols <- sort(union(a$columns, b$columns))
    first <- second <- matrix(0, length(rows$first), length(cols),
      dimnames = list(NULL, names(betas)[cols]))
    ia <- match(rows$first, a$rows)
    ib <- match(rows$second, b$rows)
    first[, match(a$columns, cols)] <- a$X[ia, , drop = FALSE]
    second[, match(b$columns, cols)] <- b$X[ib, , drop = FALSE]
    offset_a <- attr(a$X, "offset", exact = TRUE)
    offset_b <- attr(b$X, "offset", exact = TRUE)
    offset <- if (!is.null(offset_a) || !is.null(offset_b)) {
      (if (is.null(offset_a)) 0 else offset_a[ia]) -
        (if (is.null(offset_b)) 0 else offset_b[ib])
    } else NULL
    list(first = first, second = second, V = V[cols, cols, drop = FALSE],
      coefs = betas[cols], data = smooth_task_data(data[rows$first, variables, drop = FALSE]),
      select = label, by_var = by, f1 = f1, f2 = f2,
      interval = interval, ci_level = level, offset = offset)
  })
  calculate <- function() {
    results <- smooth_task_map(tasks, "calculate_difference")
    if (interval == "simultaneous") {
      columns <- lapply(tasks, function(task) match(names(task$coefs), names(betas)))
      results <- smooth_interval_map(results, columns, V, level, n_sim, n_cores,
        estimate = ".diff", critical = FALSE)
    }
    dplyr::bind_rows(results)
  }
  result <- if (interval == "simultaneous" && !is.null(seed)) {
    withr::with_seed(seed, calculate())
  } else calculate()
  result <- tibble::add_column(result, .focal = focal, .after = ".smooth")
  attr(result, "derivative_info") <- list(order = as.integer(order), focal = focal,
    wrt = wrt, type = type, eps = h)
  attr(result, "plot_variables") <- stats::setNames(
    list(smooth_plot_variables(model, ids[[1L]])), label)
  result
}
