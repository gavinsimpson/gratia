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
  if (!isTRUE(object$univar.gamlss)) return(gjrm_joint_predictor_label(object, eta))
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


# Joint equations place response predictors first, then scale and shape
# predictors, followed by the copula parameters. Keep unrecognised roles explicit.
gjrm_joint_predictor_label <- function(object, eta) {
  if (isTRUE(object$fixed.margins) || isTRUE(object$VC$fixed.margins)) {
    return(if (eta == 5L) "Dependence (theta)" else paste("Linear predictor", eta))
  }
  margins <- object$margins
  n_response <- length(margins)
  if (!n_response) return(paste("Linear predictor", eta))
  labels <- vapply(seq_len(n_response), function(j) {
    margin <- margins[j]
    if ((j == 1L && !is.null(object$VC$K1)) ||
        (j == 2L && !is.null(object$VC$K2))) {
      role <- "Ordinal predictor"
    } else if (margin %in% c("probit", "logit", "cloglog")) {
      role <- "Probability"
    } else {
      role <- gjrm_predictor_label(list(margins = margin, univar.gamlss = TRUE), 1L)
    }
    paste0("Response ", j, ": ", role)
  }, character(1))
  m2 <- object$VC$m2 %||% c("tN", "N", "GU", "rGU", "LO", "LN", "WEI", "IG",
    "GA", "GAi", "BE", "FISK", "GP", "GPII", "GPo")
  m2d <- object$VC$m2d %||% c("tNBI", "tNBII", "tPIG", "NBI", "NBII", "PIG",
    "DGP", "DGPII", "BB")
  m3 <- object$VC$m3 %||% c("DAGUM", "SM", "TW")
  m3d <- object$VC$m3d %||% c("DEL", "SICHEL")
  m1d <- object$VC$m1d %||% c("P", "PO", "tP", "ZTP", "DGP0")
  known <- c(m1d, m2, m2d, m3, m3d, "probit", "logit", "cloglog")
  if (any(!margins %in% known)) return(paste("Linear predictor", eta))
  parameter_count <- 1L + as.integer(margins %in% c(m2, m2d, m3, m3d)) +
    as.integer(margins %in% c(m3, m3d))
  for (parameter in 2:3) {
    for (j in which(parameter_count >= parameter)) {
      role <- gjrm_predictor_label(list(margins = margins[j], univar.gamlss = TRUE), parameter)
      labels <- c(labels, paste0("Response ", j, ": ", role))
    }
  }
  switching <- inherits(object, "SemiParROY") || identical(object$Model, "ROY")
  trivariate <- isTRUE(object$triv) || inherits(object, "SemiParTRIV")
  if (!switching && !trivariate && identical(object$copula %||% object$BivD, "T") &&
      all(margins %in% c(m2, m3))) {
    labels <- c(labels, "Copula degrees of freedom")
  }
  dependence <- if (switching) {
    c("Dependence (responses 1 and 2)", "Dependence (responses 1 and 3)")
  } else if (trivariate && isTRUE(object$VC$Chol)) {
    paste("Cholesky parameter", 1:3)
  } else if (trivariate) {
    c("Dependence (responses 1 and 2)", "Dependence (responses 1 and 3)",
      "Dependence (responses 2 and 3)")
  } else "Dependence (theta)"
  labels <- c(labels, dependence)
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
