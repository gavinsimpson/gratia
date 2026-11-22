# Keep exhaustive combinations in local and CI runs, with representative cases
# on CRAN. The Makefile and CI workflows set NOT_CRAN explicitly.
full_test_run <- function() identical(Sys.getenv("NOT_CRAN"), "true")

# These files cover the public operations and native model adapters with
# representative cases. The full suite also runs every other test file.
cran_test_filter <- function(filter = NULL, invert = FALSE) {
  files <- c(
    "assemble-gamlss", "b-spline-tensor", "basis-size",
    "by-variables", "censored-families", "concurvity",
    "conditional-differences", "data-combos-complete", "dependence-core",
    "derivatives", "dharma-residuals", "diagnostic-core",
    "difference-smooth-derivatives", "difference-smooths-simultaneous", "distribution-helpers",
    "distributional-fitted-samples", "draw-core", "edf",
    "evenly", "family-quantiles", "family-utils",
    "fderiv", "fitted-parameters", "fixed-effects-smooths", "function-core",
    "gamm4", "gfam-context", "gfam-diagnostics",
    "gjrm-diagnostics-families", "gjrm-joint-adapter", "gjrm-joint-diagnostics",
    "gjrm-joint-smooths", "glmmtmb", "grouped-histogram-outlines",
    "log-lik", "lp-matrix", "mixed-dependence", "mrf",
    "parallel-core", "parametric-effects-constant", "posterior-samples",
    "quantile-residuals-missing", "random-effect-samples", "residual-correlation-gaps",
    "response-derivative-identities", "response-derivatives-delta", "rootograms-gaussian",
    "samplers", "sampling-interface", "scalable-density", "scales",
    "scam-gam-api", "scasm-integration", "scasm-intervals",
    "scasm-sampling", "sdmtmb", "sdmtmb-diagnostic-adapters",
    "shape-review-evidence", "simultaneous-intervals", "smooth-coefs",
    "smooth-plot-variables-sos", "soap-components", "soap-derivatives-offset",
    "surface-3d", "surrogate-residuals", "third-party-smooths",
    "transformed-smooths", "tube-intervals", "utilities"
  )
  if (!is.null(filter)) files <- grep(filter, files, value = TRUE, invert = invert)
  if (!length(files)) return(NULL)
  paste0("^(", paste(files, collapse = "|"), ")$")
}

test_cases_for_cran <- function(full, cran) {
  if (full_test_run()) full else cran
}

# Structural and numerical plot assertions still run on CRAN. Render selected
# examples there; the full suite renders every requested combination.
expect_plot_render <- function(plot, cran = FALSE) {
  if (full_test_run() || cran) expect_silent(ggplot2::ggplotGrob(plot))
}
