# Keep exhaustive combinations in local and CI runs, with representative cases
# on CRAN. The Makefile and CI workflows set NOT_CRAN explicitly.
full_test_run <- function() identical(Sys.getenv("NOT_CRAN"), "true")

test_cases_for_cran <- function(full, cran) {
  if (full_test_run()) full else cran
}

# Structural and numerical plot assertions still run on CRAN. Render selected
# examples there; the full suite renders every requested combination.
expect_plot_render <- function(plot, cran = FALSE) {
  if (full_test_run() || cran) expect_silent(ggplot2::ggplotGrob(plot))
}
