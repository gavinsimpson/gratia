# Observation alignment is internal: calculate on the model frame, then restore
# excluded rows once at the public boundary. Never infer exclusions from values.
model_row_map <- function(model) {
  n <- NROW(stats::model.frame(model))
  unname(stats::naresid(stats::na.action(model), seq_len(n)))
}

slice_observations <- function(x, i) {
  if (is.null(x)) {
    return(NULL)
  }
  out <- vctrs::vec_slice(x, i)
  censor <- attr(x, "censor")
  if (!is.null(censor)) {
    attr(out, "censor") <- censor[i]
  }
  out
}

restore_model_rows <- function(x, model) {
  slice_observations(x, model_row_map(model))
}

# gamm/gamm4 can store expanded fitted values alongside compact responses.
# Normalize each component before calling methods which apply na.action again.
model_used_rows <- function(model) {
  map <- model_row_map(model)
  n <- NROW(stats::model.frame(model))
  for (nm in c(
    "y",
    "fitted.values",
    "linear.predictors",
    "residuals",
    "weights",
    "prior.weights"
  )) {
    x <- model[[nm]]
    if (is.null(x)) {
      next
    }
    if (NROW(x) == n) {
      next
    }
    if (length(map) != n && NROW(x) == length(map)) {
      model[[nm]] <- slice_observations(x, which(!is.na(map)))
    } else {
      stop(
        "Model component '",
        nm,
        "' is not aligned with its model frame.",
        call. = FALSE
      )
    }
  }
  compact_offset <- function(x) {
    if (is.null(x) || NROW(x) %in% c(1L, n)) {
      return(x)
    }
    if (NROW(x) == length(map)) {
      return(slice_observations(x, which(!is.na(map))))
    }
    stop("Model offset is not aligned with its model frame.", call. = FALSE)
  }
  model$offset <- if (is.list(model$offset)) {
    lapply(model$offset, compact_offset)
  } else {
    compact_offset(model$offset)
  }
  model$na.action <- NULL
  model
}

# Residual augmentation accepts the retained rows or the original fitting rows,
# in fitting order. It is not prediction at arbitrary new observations.
residual_rows <- function(x, model, data, partial = FALSE) {
  if (NROW(data) == NROW(x)) {
    return(x)
  }
  restored <- restore_model_rows(x, model)
  if (NROW(data) == NROW(restored)) {
    return(restored)
  }
  msg <- if (partial) {
    "Length of model residuals not equal to number of rows in 'data'"
  } else {
    "Length of model residuals does not equal number of rows in 'data'"
  }
  stop(msg, call. = FALSE)
}

# Prepare compact prediction data and an explicit map back to requested rows.
# A single retained observation supplies output types when all rows are missing;
# its calculated results are discarded, never used as predictions for those rows.
prediction_layout <- function(model, data = NULL, na.action = stats::na.pass) {
  training <- is.null(data)
  if (training) {
    data <- model$model
    if (is.null(data)) {
      stop("`data` must be supplied if not available from 'model'")
    }
    map <- model_row_map(model)
    rows <- seq_along(map)
    output_data <- slice_observations(data, map)
  } else {
    data <- as.data.frame(data)
    mf <- delete_response(model, data = data)
    valid <- finite_predictor_rows(mf)
    action <- match.fun(na.action)
    # Apply the requested action to a row index and the prediction model frame.
    acted <- action(data.frame(.index = seq_len(NROW(data)), mf))
    omitted <- attr(acted, "na.action")
    drop_rows <- inherits(omitted, "omit") && !inherits(omitted, "exclude")
    rows <- if (drop_rows) acted$.index else seq_len(NROW(data))
    output_data <- data[rows, , drop = FALSE]
    map <- match(rows, which(valid))
    data <- data[valid, , drop = FALSE]
  }
  if (!NROW(data)) {
    template <- model$model[1L, , drop = FALSE]
    data <- data[NA_integer_, , drop = FALSE]
    for (nm in names(template)) {
      data[[nm]] <- template[[nm]]
    }
    attr(data, "gratia.evaluated") <- TRUE
  }
  # Internally .row always indexes the compact prediction data.
  data$.row <- NULL
  list(
    data = data,
    map = map,
    rows = rows,
    output_data = output_data,
    training = training
  )
}

restore_prediction_table <- function(x, layout, include_data = FALSE) {
  if (identical(layout$map, seq_len(NROW(layout$data)))) {
    rows <- if (include_data && ".row" %in% names(layout$output_data)) {
      layout$output_data$.row
    } else {
      layout$rows
    }
    x$.row <- rows[x$.row]
    return(x)
  }
  keys <- intersect(c(".draw", ".parameter", ".category", ".y"), names(x))
  # Retain each output's existing ordering (row-major or parameter-major).
  groups <- if (length(keys)) {
    unique(as.data.frame(x[keys]))
  } else {
    data.frame(.group = 1L)
  }
  # LSS fitted tables interleave parameters within rows. Sampling tables
  # always group by draw, even when only one observation could be evaluated.
  row_major <- ".parameter" %in% names(x) && !".draw" %in% names(x)
  if (row_major) {
    gi <- rep(seq_len(NROW(groups)), times = length(layout$map))
    ri <- rep(seq_along(layout$map), each = NROW(groups))
  } else {
    gi <- rep(seq_len(NROW(groups)), each = length(layout$map))
    ri <- rep(seq_along(layout$map), times = NROW(groups))
  }
  wanted <- groups[gi, , drop = FALSE]
  wanted$.row <- layout$map[ri]
  actual <- as.data.frame(x[c(keys, ".row")])
  if (!length(keys)) {
    actual$.group <- 1L
  }
  wanted <- wanted[names(actual)]
  idx <- vctrs::vec_match(wanted, actual)
  out <- vctrs::vec_slice(x, idx)
  out$.row <- layout$rows[ri]
  for (key in keys) {
    out[[key]] <- groups[[key]][gi]
  }
  if (include_data) {
    for (nm in intersect(names(layout$output_data), names(out))) {
      out[[nm]] <- slice_observations(layout$output_data[[nm]], ri)
    }
  }
  out
}

# Missing prediction parameters must never reach family RNGs: some warn, while
# others (notably ordinal RNGs) can silently generate plausible invalid values.
missing_safe_rd <- function(fun, n_response = 1L) {
  force(fun)
  function(mu, wt, scale) {
    n <- NROW(mu)
    if (is.null(wt)) {
      wt <- rep(1, n)
    }
    if (length(wt) == 1L) {
      wt <- rep(wt, n)
    }
    if (length(wt) != n) {
      stop("Weights must have one value per prediction row.")
    }
    valid <- stats::complete.cases(mu) & !is.na(wt)
    if (all(valid)) {
      return(fun(mu = mu, wt = wt, scale = scale))
    }
    if (!any(valid)) {
      # Discover the response shape without consuming random numbers.
      # Multivariate normal RNGs return a matrix with one column per response;
      # all other supported RNGs return one value per observation.
      if (n_response > 1L) {
        return(matrix(
          NA_real_,
          n,
          n_response,
          dimnames = list(NULL, colnames(mu))
        ))
      }
      return(rep(NA_real_, n))
    }
    ans <- fun(
      mu = slice_observations(mu, which(valid)),
      wt = wt[valid],
      scale = scale
    )
    slice_observations(ans, match(seq_len(n), which(valid)))
  }
}

# Native spline routines generally cannot accept NAs, even when the missing
# column belongs to another smooth. Keep evaluation local to this smooth.
predict_mat_rows <- function(smooth, data) {
  vars <- term_names(smooth)
  valid <- finite_predictor_rows(data[, vars, drop = FALSE])
  if (all(valid)) {
    return(mgcv::PredictMat(smooth, data))
  }
  if (!any(valid)) {
    nc <- if (!is.null(smooth$last.para)) {
      smooth$last.para - smooth$first.para + 1L
    } else {
      smooth$df
    }
    return(matrix(NA_real_, NROW(data), nc))
  }
  x <- mgcv::PredictMat(smooth, data[valid, , drop = FALSE])
  map <- match(seq_len(NROW(data)), which(valid))
  out <- slice_observations(x, map)
  off <- attr(x, "offset")
  if (!is.null(off)) {
    attr(out, "offset") <- off[map]
  }
  out
}

# Nonfinite transformed predictors cannot enter native spline routines. Keep
# their rows in the result, just as for missing predictors. Responses are not
# included here because infinities can legitimately encode censoring.
finite_predictor_rows <- function(data) {
  valid <- stats::complete.cases(data)
  for (x in data) {
    if (is.numeric(x)) {
      finite <- is.finite(x)
      if (is.matrix(x)) finite <- rowSums(!finite) == 0L
      valid <- valid & finite
    }
  }
  valid
}
