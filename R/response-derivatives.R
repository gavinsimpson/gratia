# Shared original and shifted prediction rows for both uncertainty methods.
prepare_response_derivative_data <- function(object, focal, data, n, eps,
                                             order, type) {
  ## handle order
  if (!is.numeric(order) || length(order) != 1L || is.na(order) ||
      !order %in% c(1L, 2L)) {
    cli::cli_abort(c(
      "Only 1st or 2nd order partial derivatives are supported.",
      "i" = "{.arg order} must be {.val 1} or {.val 2}."
    ))
  }

  ## handle data
  need_data <- is.null(data)

  ## handle focal
  if (!is.character(focal) || length(focal) != 1L || is.na(focal) ||
      !focal %in% model_vars(object)) {
    cli::cli_abort("{.arg focal} must name one model covariate.")
  }

  ## sort out data
  if (need_data) {
    x <- object$var.summary[[focal]]
    x <- seq(x[1L], x[3L], length = n)
    tv <- typical_values(object,
      vars = !matches(focal), data = data
    )
    # if model only contains a single var, tv is empty
    data <- if (ncol(tv) > 0L) {
      expand_grid(.x = x, tv)
    } else {
      expand_grid(.x = x)
    }
  } else {
    data <- data |>
      select(all_of(model_vars(object))) |>
      rename(.x = all_of({{ focal }}))
  }
  data <- data |>
    add_column(.row = seq_len(nrow(data)), .before = 1L)

  # Choose the step on the raw focal scale before perturbing prediction rows.
  step_data <- data
  names(step_data)[names(step_data) == ".x"] <- focal
  eps <- derivative_step(object, focal, step_data, eps, order, type)

  # now shift values depending on method
  fd_data <- prepare_fdiff_data(
    data = data, eps = eps, type = type,
    order = order, focal = focal
  )

  list(data = data, fd_data = fd_data, eps = eps)
}

# Apply the same finite-difference formulas to fitted values or their
# coefficient gradients. Matrices retain their coefficient columns.
apply_response_fdiff <- function(values, fd_data, order, type, eps) {
  rows <- split(seq_len(nrow(fd_data)), fd_data$..type)
  pieces <- lapply(rows, function(i) {
    i <- i[base::order(fd_data$..orig[i])]
    if (is.matrix(values)) values[i, , drop = FALSE] else values[i]
  })
  names(pieces) <- paste0("..", names(pieces))
  response_fdiff_function(order, type)(pieces, eps = eps)
}

response_fdiff_function <- function(order, type) {
  if (order == 1L) {
    switch(type, forward = y_forward_diff_1, backward = y_backward_diff_1,
      central = y_central_diff_1)
  } else {
    switch(type, forward = y_forward_diff_2, backward = y_backward_diff_2,
      central = y_central_diff_2)
  }
}

response_derivatives_delta <- function(object, focal, data, order, type,
    scale, n, eps, level, unconditional, envir = NULL, freq = FALSE, ...) {
  object <- with_model_envir(object, envir)
  fam <- stats::family(object)
  supported <- c("gaussian", "poisson", "binomial", "gamma",
    "inverse_gaussian", "quasi", "quasipoisson", "quasibinomial",
    "negative_binomial", "tweedie", "beta_regression", "scaled_t")
  if (inherits(object, "scam") || inherits(fam, "general.family") ||
      !family_type(object) %in% supported) {
    cli::cli_abort(
      "Delta-method response derivatives are not supported for this model family or class."
    )
  }
  dots <- list(...)
  # These controls concern posterior draws only, as in conditional_differences().
  dots[c("draws", "n_cores", "burnin", "thin", "t_df", "rw_scale")] <- NULL
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    cli::cli_abort("Arguments in {.arg ...} must be named.")
  }
  if (any(names(dots) %in% c("newdata", "se.fit", "terms"))) {
    cli::cli_abort(c(
      "{.arg newdata}, {.arg se.fit}, and {.arg terms} cannot be supplied.",
      "i" = "Use {.arg data} for evaluation points and {.arg exclude} to omit terms."
    ))
  }
  # Ordinary BAM prediction avoids discrete lpmatrix dimension dropping when
  # exclusions leave only the intercept, and evaluates the shifted rows directly.
  if (inherits(object, "bam") && is.null(dots$discrete)) dots$discrete <- FALSE
  grid <- prepare_response_derivative_data(object, focal, data, n, eps,
    order, type)
  fd <- grid$fd_data
  predict_at <- function(type) {
    do.call(predict_model, c(list(object = object, newdata = fd,
      type = type), dots))
  }
  # Link predictions include formula offsets; the lpmatrix does not.
  eta <- as.numeric(predict_at("link"))
  X <- predict_at("lpmatrix")
  if (length(eta) != nrow(fd) || nrow(X) != nrow(fd) ||
      any(!is.finite(eta)) || any(!is.finite(X))) {
    cli::cli_abort(
      "Predictions must be finite and retain every derivative-grid row."
    )
  }
  fitted <- if (scale == "response") fam$linkinv(eta) else eta
  G <- if (scale == "response") X * as.numeric(fam$mu.eta(eta)) else X
  if (any(!is.finite(fitted)) || any(!is.finite(G))) {
    cli::cli_abort("Non-finite response predictions or coefficient gradients.")
  }
  estimate <- apply_response_fdiff(fitted, fd, order, type, grid$eps)
  J <- apply_response_fdiff(G, fd, order, type, grid$eps)
  V <- get_vcov(object, unconditional = unconditional, frequentist = freq)
  se <- sqrt(pmax(0, rowSums((J %*% V) * J)))
  crit <- coverage_normal(level)
  covariates <- grid$data
  names(covariates)[names(covariates) == ".x"] <- focal
  out <- bind_cols(
    tibble(.row = covariates$.row, .focal = focal,
      .derivative = as.numeric(estimate), .se = as.numeric(se),
      .lower_ci = as.numeric(estimate - crit * se),
      .upper_ci = as.numeric(estimate + crit * se)),
    select(covariates, -all_of(".row")))
  class(out) <- c("response_derivatives", class(out))
  attr(out, "uncertainty") <- "delta"
  out
}
