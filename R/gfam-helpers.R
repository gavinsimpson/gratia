# Grouped families have one linear predictor, but a distinct observation model
# for each row. Never change the fitted family's shared closure environment to
# install prediction indices: that would also change future training results.
is_gfam_family <- function(family) {
  is.function(family[["getfl"]]) &&
    startsWith(family[["family"]], "gfam{")
}

gfam_data_index <- function(model, data) {
  response <- stats::formula(model)[[2L]]
  label <- paste(deparse(response, width.cutoff = 500L), collapse = "")
  if (label %in% names(data)) {
    value <- data[[label]]
  } else if (is.call(response) && identical(response[[1L]], as.name("cbind")) &&
      length(response) == 3L) {
    # Membership can be evaluated without observed response values.
    expr <- response[[3L]]
    if (!all(all.vars(expr) %in% names(data))) return(NULL)
    value <- tryCatch(eval(expr, data, model_envir(model)), error = function(e) NULL)
  } else {
    return(NULL)
  }
  if (is.matrix(value)) {
    if (NCOL(value) != 2L) {
      stop("A grouped-family response matrix must have two columns.", call. = FALSE)
    }
    value <- value[, 2L]
  }
  value
}

gfam_validate_index <- function(index, n, n_family) {
  if (!is.numeric(index) || !is.null(dim(index)) || length(index) != n ||
      any(!is.na(index) & (!is.finite(index) | index != trunc(index) |
        index < 1L | index > n_family))) {
    stop("'family_index' must have one integer family-list position (1 to ",
      n_family, ") per prediction row; missing values are allowed.", call. = FALSE)
  }
  as.integer(index)
}

# Upstream gfam versions can leave component theta closures out of sync with
# aggregate fitted theta after an ordinary family with estimated scale. Rebuild
# native component parameter state locally, never modifying the fitted family.
gfam_component_family <- function(component, theta) {
  if (!length(theta)) return(component)
  if (!is.function(component$putTheta)) {
    stop("Grouped-family component cannot restore its fitted parameters.",
      call. = FALSE)
  }
  old <- environment(component$putTheta)
  if (isNamespace(old) || identical(old, globalenv()) ||
      identical(old, baseenv()) || identical(old, emptyenv())) {
    stop("Grouped-family parameters require a family-private environment.",
      call. = FALSE)
  }
  # Diagnostic factories can capture nested functions sharing the parameter
  # state. Clone the complete environment graph before updating that state.
  component <- unserialize(serialize(component, NULL))
  component$putTheta(theta)
  component
}

# Packed gfam nuisance parameters contain each extended component's n.theta
# estimated parameters, followed by log(scale) when its scale is estimated.
# getTheta() may also return fixed parameters, so its length is not a cursor.
gfam_components <- function(family) {
  families <- family$getfl()
  theta <- family$getTheta()
  cursor <- 1L
  out <- lapply(families, function(component) {
    n_theta <- if (inherits(component, "extended.family")) component$n.theta else 0L
    if (is.null(n_theta)) n_theta <- 0L
    estimated_scale <- isTRUE(component$scale < 0)
    n <- n_theta + as.integer(estimated_scale)
    if (cursor + n - 1L > length(theta)) {
      stop("Grouped-family nuisance parameters are incomplete.", call. = FALSE)
    }
    scale <- if (estimated_scale) exp(theta[[cursor + n_theta]]) else 1
    theta_index <- if (n_theta) seq.int(cursor, length.out = n_theta) else integer()
    component <- gfam_component_family(component, theta[theta_index])
    info <- list(family = component, scale = scale,
      theta = if (is.function(component$getTheta)) component$getTheta() else numeric(),
      theta_index = theta_index)
    cursor <<- cursor + n
    info
  })
  if (cursor - 1L != length(theta)) {
    stop("Grouped-family nuisance parameters are not aligned with its families.",
      call. = FALSE)
  }
  out
}

gfam_prediction_context <- function(model, data = NULL, family_index = NULL,
                                    na.action = stats::na.pass) {
  family <- stats::family(model)
  if (!is_gfam_family(family)) {
    stop("Expected a model fitted with 'mgcv::gfam()'.", call. = FALSE)
  }
  families <- family$getfl()
  training <- is.null(data)
  if (training) {
    recovered <- restore_model_rows(attr(families, "fi"), model)
    n <- length(model_row_map(model))
  } else {
    data <- as.data.frame(data)
    recovered <- gfam_data_index(model, data)
    n <- NROW(data)
  }
  if (!is.null(recovered)) {
    recovered <- gfam_validate_index(recovered, n, length(families))
  }
  if (!is.null(family_index)) {
    family_index <- gfam_validate_index(family_index, n, length(families))
    if (!is.null(recovered) && !identical(family_index, recovered)) {
      stop("'family_index' conflicts with the family membership in 'data'.",
        call. = FALSE)
    }
  } else {
    family_index <- recovered
  }
  if (is.null(family_index)) {
    stop("Grouped-family response prediction needs family membership; supply ",
      "the response index in 'data' or use 'family_index'.", call. = FALSE)
  }
  layout <- prediction_layout(model, data, na.action = na.action,
    required = if (!training) data.frame(.family_index = family_index) else NULL)
  index <- if (training) as.integer(attr(families, "fi")) else family_index[layout$input_rows]
  components <- gfam_components(family)
  fitted_families <- lapply(components, `[[`, "family")
  attributes(fitted_families) <- attributes(families)
  list(families = fitted_families, index = index, family_index = family_index,
    layout = layout, components = components)
}


# Convert every observation-by-draw entry, retaining the original dimensions.
# A whole gfam linkinv closure is training-row specific and indexes matrices as
# vectors. It is therefore unsuitable for either new rows or multiple draws.
gfam_category_expectations <- function(context) {
  !is.null(context) && any(vapply(context$families,
    function(f) distribution_spec(f)$categorical, logical(1L)))
}

# Expand ordinal expectations after row restoration, so each observation keeps
# its own family and category identities even when some predictors are missing.
gfam_expectation_table <- function(table, context, ci_level = NULL) {
  table$.prediction_order <- seq_len(nrow(table))
  parts <- lapply(seq_along(context$families), function(i) {
    rows <- which(table$.family == i)
    spec <- distribution_spec(context$families[[i]])
    eta <- matrix(table$.fitted[rows], ncol = 1L)
    evaluate <- function(x) distribution_output(spec, x, "expectation", "response")
    value <- evaluate(eta)
    labels <- distribution_table(spec, value, "expectation")
    out <- table[rows[labels$.row], , drop = FALSE]
    out$.parameter <- "expectation"
    out$.fitted <- labels$.fitted
    out$.category <- if (".category" %in% names(labels)) labels$.category else NA_character_
    if (!is.null(ci_level)) {
      step <- .Machine$double.eps^(1 / 3) * pmax(1, abs(eta))
      critical <- coverage_normal(ci_level)
      if (spec$categorical) {
        transform <- spec$category_transform(eta, gradient = TRUE)
        slope <- transform$logit_gradient[[1L]]
        limits <- categorical_probability_limits(transform,
          abs(slope) * table$.se[rows], critical)
        se <- limits$se
        lower <- limits$lower
        upper <- limits$upper
      } else {
        slope <- (evaluate(eta + step) - evaluate(eta - step)) / (2 * as.numeric(step))
        se <- abs(slope) * table$.se[rows]
        lower <- evaluate(eta - critical * table$.se[rows])
        upper <- evaluate(eta + critical * table$.se[rows])
      }
      out$.se <- as.numeric(se)
      out$.lower_ci <- as.numeric(pmin(lower, upper))
      out$.upper_ci <- as.numeric(pmax(lower, upper))
    }
    out
  })
  missing <- table[is.na(table$.family), , drop = FALSE]
  missing$.category <- NA_character_
  missing$.parameter <- "expectation"
  out <- dplyr::bind_rows(c(parts, list(missing)))
  out <- out[order(out$.prediction_order), , drop = FALSE]
  out$.prediction_order <- NULL
  out
}

gfam_response_means <- function(eta, context, quantity = "expectation") {
  out <- eta
  out[] <- NA_real_
  for (i in seq_along(context$families)) {
    rows <- which(context$index == i)
    if (!length(rows)) next
    family <- context$families[[i]]
    if (quantity == "expectation" && startsWith(family$family, "Ordered Categorical")) {
      stop("Grouped ordinal response-scale fitted draws are not supported; ",
        "use 'scale = \"linear_predictor\"'.", call. = FALSE)
    }
    values <- as.vector(eta[rows, , drop = FALSE])
    spec <- distribution_spec(family)
    pars <- spec$to_parameters(matrix(values, ncol = 1L))
    means <- if (quantity == "expectation") as.numeric(spec$expectation(pars)) else
      as.numeric(pars)
    if (!is.numeric(means) || length(means) != length(values) ||
        (is.matrix(means) && NCOL(means) != 1L)) {
      stop("Grouped-family fitted draws require a scalar response mean per row; ",
        "use the linear predictor scale for this component.", call. = FALSE)
    }
    out[rows, ] <- means
  }
  out
}


# Link/matrix prediction does not need family response predictions. Removing
# these hooks on a local list also avoids upstream observed-response NA checks.
# Current discrete bam prediction is not reliable for grouped families; the
# ordinary prediction path retains identical coefficient and offset semantics.
gfam_predict_eta <- function(model, data, type = c("link", "lpmatrix"), ...) {
  type <- match.arg(type)
  model <- model_used_rows(model)
  model$family$predict <- NULL
  model$family$setInd <- NULL
  dots <- list(...)
  if (inherits(model, "bam")) dots$discrete <- FALSE
  do.call(predict_model, c(list(object = model, newdata = data, type = type), dots))
}
