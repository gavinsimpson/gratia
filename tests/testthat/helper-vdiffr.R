# Fitted-model SVGs use R's reference BLAS for their visual baselines.
# Ordinary CI runs numerical/build assertions; the dedicated macOS job verifies
# the BLAS configuration and checks images. Keep functional assertions outside skips.

# Honour the explicit switch in local runs as well as CI. Functional checks
# belong before this call (or in their own test), because a skip ends the test.
expect_doppelganger <- function(title, fig, ...) {
  testthat::skip_on_cran()
  testthat::skip_if(identical(Sys.getenv("VDIFFR_RUN_TESTS"), "false"),
    "`VDIFFR_RUN_TESTS` set to false")
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, ...)
}
