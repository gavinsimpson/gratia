# Keep exhaustive combinations in local and CI runs, with representative cases
# on CRAN. The Makefile and CI workflows set NOT_CRAN explicitly.
full_test_run <- function() identical(Sys.getenv("NOT_CRAN"), "true")

# These files exercise the main workflows and selected native model adapters.
# The help examples cover basic GJRM, glmmTMB and sdmTMB usage on CRAN.
cran_test_filter <- function(filter = NULL, invert = FALSE) {
  files <- c(
    "b-spline-tensor", "by-variables",
    "conditional-differences", "diagnostic-core", "difference-smooth-derivatives",
    "draw-core", "fixed-effects-smooths", "function-core",
    "gamm4", "gfam-context", "gfam-diagnostics",
    "log-lik", "mixed-dependence", "mrf",
    "response-derivatives-delta", "rootograms-gaussian", "sampling-interface",
    "scales", "scam-gam-api", "scasm-integration", "simultaneous-intervals",
    "soap-components", "soap-derivatives-offset", "surface-3d",
    "surrogate-residuals", "third-party-smooths"
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
