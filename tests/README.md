# Test tiers

The CRAN suite keeps numerical regressions, argument checks and small plot-build
checks. Larger plotting examples and visual permutations use `skip_on_cran()`
before any expensive work. No production behavior depends on the test tier.

Run the core suite explicitly:

```r
Sys.setenv(NOT_CRAN = "false", VDIFFR_RUN_TESTS = "false")
testthat::test_local()
```

Run the broader numerical/integration suite without image comparisons:

```r
Sys.setenv(NOT_CRAN = "true", VDIFFR_RUN_TESTS = "false")
testthat::test_local()
```

For visual comparisons, set `VDIFFR_RUN_TESTS = "true"`. The paper-sized HGAM
examples also run by default locally. Ordinary CI skips those paper examples;
its weekly/manual integration job enables them with `GRATIA_RUN_HGAM=true`.
To reproduce ordinary CI locally, also set `CI=true` for that R session.

The weekly/manual job publishes total and per-test elapsed times in its GitHub
Actions summary, plus an `hgam-test-timings` CSV artifact retained for 90 days.
Per-test times include work performed before a visual snapshot skip; the total
also includes shared setup and runner overhead.

The R-CMD-check matrix runs the broader suite on its release/development
platforms and CRAN-core checks on Linux and macOS oldrel. Coverage uses the
broader suite. A separate
macOS job runs visual comparisons with a fixed R version and Apple Accelerate
BLAS, matching the local baseline environment; image differences
should be reviewed, not automatically accepted. Existing platform restrictions
on legacy numerical snapshots remain in place.

## Fixtures and assertions

- `testthat/setup.R` contains core fixtures. `secondary_models()` loads and caches
  `testthat/fixtures/secondary-models.R` only when a secondary test requests it,
  after checking `skip_on_cran()`. Its environment retains the raw data needed
  for formula evaluation. Do not add a second automatically sourced setup file
  for expensive fixtures: that would still fit them on CRAN.
- The three large drawing files, `test-bam-ar.R`, and `test-soap-films.R` are
  secondary. `test-draw-core.R` retains small numerical/build checks for their
  distinct paths, alongside the existing focused regression tests.
- A snapshot skip terminates its enclosing test. Put numerical assertions before
  snapshots, or in separate tests. `expect_doppelganger()` honours the explicit
  `VDIFFR_RUN_TESTS` switch both locally and on CI.
- Prefer small data/grids/draw counts to exclusions when they preserve the
  contract. The family-quantile tests still cover all eight families on CRAN;
  only the costly Tweedie integration fixtures use fewer observations.
- Measure shared setup and top-level code as well as test-body durations. A
  reported skip may happen after substantial computation.

For an installed-package check, export `NOT_CRAN=false` (core) or `NOT_CRAN=true`
(broader suite) before `R CMD check`. The existing Makefile targets
`check-test-cran` and `check` respectively set these variables.

## Builder diagnostics

Each scasm reference shape has a separate named test, so fitting errors identify
the shape even when fitting stops before an assertion.

All package checks use the normal testthat reporter. The full Windows ARM
R-release check runs alongside the Windows x64 check, verifies that R is native
ARM, and retains the repositories provided by the ARM R installation.

Only the focused Windows ARM sdmTMB compatibility job enables the temporary
START/DONE checkpoints around Student-t fitting, diagnostic and predictive
calls. They write directly to stderr and flush immediately. An abrupt process
exit leaves the last START without its matching DONE. This job uses
sdmTMB >= 1.2.0 with RTMB and records dependency versions.

To enable the same checkpoints on another platform:

```r
Sys.setenv(GRATIA_TEST_DIAGNOSTICS = "true")
testthat::test_local(
  filter = "^sdmtmb-student-predictive$"
)
```

The checkpoints are disabled unless `GRATIA_TEST_DIAGNOSTICS=true` is set
explicitly. Calls expected to raise an error also have no DONE; their enclosing
tests still finish normally.
