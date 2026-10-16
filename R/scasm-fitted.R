# Reuse native limits on each linear predictor, then use the same parameter
# transformations as the existing fitted-value adapters.
scasm_fitted_bootstrap <- function(object, data, fit, scale, ci_level,
                                   extra_fns, unconditional, ...) {
  if (!nrow(fit)) return(fit)
  limits <- predict_model(object, newdata = data, type = "link",
    se.fit = ci_level, unconditional = unconditional, ...)
  flatten <- function(x) if (is.matrix(x)) as.vector(t(x)) else as.vector(x)
  lower <- flatten(limits$ll)
  upper <- flatten(limits$ul)
  if (length(lower) != nrow(fit) || length(upper) != nrow(fit)) {
    cli::cli_abort("Native bootstrap limits do not match the fitted prediction rows.")
  }
  if (scale == "response") {
    if (!".parameter" %in% names(fit)) {
      link <- inv_link(object)
      lower <- link(lower)
      upper <- link(upper)
    } else {
      links <- lss_links(object, inverse = TRUE)
      for (parameter in unique(fit$.parameter)) {
        rows <- fit$.parameter == parameter
        transform <- function(x) {
          eta <- links[[parameter]](x)
          if (parameter == "power" && family_type(object) == "twlss") {
            bounds <- get_twlss_bounds(object)
            extra_fns[[parameter]](eta, a = bounds[1], b = bounds[2])
          } else extra_fns[[parameter]](eta)
        }
        lower[rows] <- transform(lower[rows])
        upper[rows] <- transform(upper[rows])
      }
    }
  }
  fit$.lower_ci <- lower
  fit$.upper_ci <- upper
  fit
}

scasm_fitted_bootstrap_supported <- function(fun, scale) {
  any(vapply(list(fit_vals_default, fit_vals_general_lss, fit_vals_ziplss,
    fit_vals_twlss), function(adapter) identical(fun, adapter), logical(1))) ||
    (identical(fun, fit_vals_ocat) && scale != "response")
}
