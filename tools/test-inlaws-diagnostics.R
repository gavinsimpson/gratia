# Run explicitly with an installed copy of inlaws; no inlaws dependency is
# required for the package's ordinary tests or for gratia users.
if (!requireNamespace("inlaws", quietly = TRUE))
  stop("Install inlaws from gam-mafia/inlaws before running these integration tests.")
if (!is.function(inlaws::nbls()$gratia_diagnostics))
  stop("Install a native diagnostic-interface revision of inlaws before running this suite.")
pkgload::load_all(".", quiet = TRUE)
library("testthat")
testthat::test_file("tests/integration/test-inlaws-diagnostics.R",
  env = environment(), reporter = "summary", stop_on_failure = TRUE,
  stop_on_warning = TRUE)
