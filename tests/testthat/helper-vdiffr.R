# Custom expect_doppelganger that skips if vdiffr not installed, otherwise it
# runs vdiffr::expect_doppelganger as normal.
#
# The first part of the `if()` is for CI - I set `VDIFFR_RUN_TESTS` in some of
# my GitHub Actions workflows to stop vdiffr tests from runing at all because
# estimated GAMs are slightly sensitive to the linear algebra stack in use on
# various systems. The sensitivity is usually in trivial decimal places but this
# causes sufficient differences in plots that non-visible differences are actual
# failures in the snapshot tests. There are also actual differences due to CPU
# architecture, linear algebra stack; for example the default basis is a
# low-rank thin plate regression spline (TPRS), where an eigendcomposition is
# used on the full TPRS basis. As the signs of eigenvectors are not defined
# basis functions can become flipped, and consequently the signs of basis
# function coefs can also be flipped. Finally, there are actual differences in
# model estimates that are statistically trivial but do cause small visible
# differences in the snapshots.
#
# The end result is that testing snapshots on CI is basically a whack-a-mole
# game of false positives.
#
# As such, all vdiffr tests are turned off for Linux and Windows runners on
# GitHUb Actions, just like they don't fail on CRAN. This way I only need to
# concern myself with deviations on macOS runners as this is also my dev
# platform of choice as of May 2024.

# Need a local wrapper to allow conditional use of vdiffr, and also
if ((nzchar(Sys.getenv("CI")) || !nzchar(Sys.getenv("NOT_CRAN"))) &&
  identical(Sys.getenv("VDIFFR_RUN_TESTS"), "false")) {
  # if we are running tests remotely AND
  # we are opting out of using vdiffr
  # assigning a dummy function

  expect_doppelganger <- function(title, fig, ...) {
    testthat::skip("`VDIFFR_RUN_TESTS` set to false on this remote check")
  }
} else {
  expect_doppelganger <- function(title, fig, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, ...)
  }
}