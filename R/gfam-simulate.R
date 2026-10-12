# Group membership is an observation-level parameter, so keep it alongside eta
# and weights when filtering missing rows. A closure over the unsliced membership
# vector cannot safely be passed through missing_safe_rd().
simulate_gfam <- function(object, nsim, data = NULL, weights = NULL,
                          family_index = NULL, ...) {
  context <- gfam_prediction_context(object, data = data,
    family_index = family_index,
    na.action = list(...)$na.action %||% stats::na.pass)
  layout <- context$layout
  n <- if (is.null(data)) length(model_row_map(object)) else NROW(data)
  if (is.null(weights)) {
    weights <- if (is.null(data)) {
      restore_model_rows(model_used_rows(object)$prior.weights, object)
    } else rep(1, n)
  }
  if (length(weights) == 1L) weights <- rep(weights, n)
  if (length(weights) != n) {
    stop("Weights must have one value per prediction row.", call. = FALSE)
  }
  # map is a compact row index for each requested output position. Rows omitted
  # by prediction na.action are absent from rows; excluded rows have NA in map.
  compact_weights <- rep(NA_real_, NROW(layout$data))
  valid <- which(!is.na(layout$map))
  compact_weights[layout$map[valid]] <- weights[layout$rows[valid]]
  eta <- gfam_predict_eta(object, data = layout$data, type = "link", ...)
  generators <- lapply(seq_along(context$components), function(i) {
    component <- context$components[[i]]
    rd <- fix_family_rd(component$family)$rd
    if (is.null(rd) && any(context$index == i, na.rm = TRUE)) {
      stop("Don't yet know how to simulate from grouped component ", i,
        " <", component$family$family, ">.", call. = FALSE)
    }
    rd
  })
  sims <- replicate(nsim, {
    values <- gfam_response_draw(eta, compact_weights, context, generators)
    slice_observations(values, layout$map)
  }, simplify = FALSE)
  sims <- as.data.frame(sims)
  names(sims) <- paste0("sim_", seq_len(nsim))
  sims
}

gfam_response_draw <- function(eta, weights, context, generators) {
  out <- rep(NA_real_, length(eta))
  valid <- is.finite(eta) & !is.na(weights) & !is.na(context$index)
  if (any(!is.finite(weights[!is.na(weights)])) ||
      any(weights[!is.na(weights)] < 0)) {
    stop("Weights must be finite and non-negative, or missing.", call. = FALSE)
  }
  for (i in seq_along(context$components)) {
    rows <- which(valid & context$index == i)
    if (!length(rows)) next
    component <- context$components[[i]]
    ft <- family_type(component$family)
    wt <- weights[rows]
    if (ft == "binomial" && any(wt != floor(wt))) {
      stop("Binomial component weights must be integer trial counts.", call. = FALSE)
    }
    if (ft %in% c("gaussian", "cnorm", "clog") && any(wt == 0)) {
      stop("Gaussian and logistic component precision weights must be positive.",
        call. = FALSE)
    }
    latent <- grepl("^ordered_categorical", ft) ||
      grepl("^zero_inflated_poisson", ft)
    mu <- if (latent) eta[rows] else component$family$linkinv(eta[rows])
    value <- generators[[i]](mu = mu, wt = wt, scale = component$scale)
    if ((!is.numeric(value) && !is.logical(value)) || !is.null(dim(value)) ||
        length(value) != length(rows)) {
      stop("Grouped component ", i, " <", component$family$family,
        "> must generate one numeric response per observation.", call. = FALSE)
    }
    out[rows] <- value
  }
  out
}
