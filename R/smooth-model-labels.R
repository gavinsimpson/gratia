# Model-part labels are display metadata, independent of smooth labels and
# selection. Use coefficient membership, never parse mgcv's s.1(x) labels:
# predictors may have no smooths, or share a smooth's coefficients.
has_multiple_predictors <- function(object) {
  n <- attr(object, "gratia.n_predictors", exact = TRUE)
  if (is.null(n)) n <- length(lss_eta_index(object))
  n > 1L
}

smooth_model_labels <- function(object, smooths) {
  if (!has_multiple_predictors(object)) {
    return(rep(NA_character_, length(smooths)))
  }
  override <- attr(object, "gratia.model_label", exact = TRUE)
  lpi <- lss_eta_index(object)
  labels <- if (!inherits(object, "gratia_mixed_smooth") &&
      is.null(override) && length(lpi) > 1L) {
    mgcv_predictor_labels(family(object), length(lpi))
  }
  vapply(smooths, function(sm) {
    component <- attr(sm, "gratia.component", exact = TRUE)
    if (is.null(component) && inherits(object, "gratia_mixed_smooth")) {
      component <- object$component
    }
    if (!is.null(component)) return(mixed_model_label(object, component))
    if (!is.null(override)) return(override)
    if (length(lpi) < 2L) return(NA_character_)
    belongs <- vapply(lpi, function(i) all(smooth_coef_indices(sm) %in% i),
      logical(1))
    if (!any(belongs)) return(NA_character_)
    paste(labels[belongs], collapse = "; ")
  }, character(1))
}

mgcv_predictor_labels <- function(family, n) {
  labels <- switch(family_name(family),
    gaulss = c("Mean (mu)", "Standard deviation (sigma)"),
    gammals = c("Mean (mu)", "Scale (phi)"),
    gumbls = c("Location (mu)", "Log scale (beta)"),
    gevlss = c("Location (mu)", "Scale (sigma)", "Shape (xi)"),
    shash = c("Location (mu)", "Scale (sigma)", "Skewness (epsilon)",
      "Tail weight (delta)"),
    twlss = c("Mean (mu)", "Power (p)", "Dispersion (phi)"),
    ziplss = c("Poisson mean (mu)", "Presence probability"),
    multinom = paste0("Category ", seq_len(n), " (log odds)"),
    `Multivariate normal` = paste0("Mean (response ", seq_len(n), ")")
  )
  if (length(labels) != n) labels <- paste("Linear predictor", seq_len(n))
  labels
}

# GJRM's embedded GAMs use working Gaussian families. The parent margin and
# equation number identify the fitted parameters mu, sigma and nu instead.
# Retain parameter names where a distribution-specific role is not known.
gjrm_predictor_label <- function(object, eta) {
  margin <- object$margins[1L]
  labels <- c("Parameter: mu", "Parameter: sigma", "Parameter: nu")
  if (length(margin)) {
    labels <- switch(margin,
      N = c("Mean (mu)", "Standard deviation (sigma)"),
      tN = c("Location (mu)", "Standard deviation (sigma)"),
      LN = c("Log-location (mu)", "Log-scale (sigma)"),
      GU =, rGU =, LO = c("Location (mu)", "Scale (sigma)"),
      P =, PO =, tP =, ZTP = "Mean (mu)",
      TW = c("Mean (mu)", "Dispersion (sigma)", "Power (nu)"),
      GP =, GPII =, GPo =, DGP =, DGPII = c("Shape (mu)", "Scale (sigma)"),
      DGP0 = "Scale (sigma)",
      WEI =, FISK = c("Scale (mu)", "Shape (sigma)"),
      BE = c("Mean (mu)", "Dispersion (sigma)"),
      DAGUM =, SM = c("Scale (mu)", "Shape (sigma)", "Shape (nu)"),
      labels
    )
  }
  if (eta > length(labels)) return(paste("Linear predictor", eta))
  labels[[eta]]
}

smooth_plot_subtitle <- function(object, by_subtitle = NULL) {
  model <- unique(object[[".model"]])
  model <- model[!is.na(model)]
  parts <- c(model, by_subtitle)
  if (!length(parts)) return(NULL)
  paste(parts, collapse = "; ")
}
