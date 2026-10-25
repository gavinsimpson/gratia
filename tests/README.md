# Test tiers

The CRAN suite uses small examples of public functions, representative numerical
comparisons and argument checks. It reuses the shared models and checks fitted
values, covariance calculations, densities and generated responses against
native model results or independent formulas. No production behavior depends
on the test tier.

The full suite checks broader combinations of model classes, families,
constructors, coordinate transformations, labels, themes and interval methods.
Large simulation convergence comparisons, native likelihood-profile matrices
and research sampler variants also belong there. File-level `skip_on_cran()`
guards run before the fixtures or plotting work in these combination files.
The weekly GitHub Actions jobs and local full runs retain these checks.

`test-function-core.R` keeps compact moment, plotting, theme, label and utility
checks alongside the existing focused numerical tests. `test-parallel-core.R`
uses one worker pool to compare derivatives, partial derivatives, smooth samples,
smooth differences and ALE with their serial results, including random-number
state and worker error handling. Additional worker/model combinations run only
in the full suite.

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
examples are excluded by default, locally and in CI. Both `make test` and
`make test-full` explicitly keep them disabled. Their weekly/manual integration
job enables them with `GRATIA_RUN_HGAM=true` to periodically check that the
paper's code still runs.

Run only the paper examples locally when that check is specifically needed:

```sh
make test-hgam-paper
```

The target uses the installed-package test runner with
`GRATIA_TEST_FILTER='^hgam-paper$'`, so it does not rerun the rest of the suite.
As in the weekly paper check, visual snapshot comparisons are disabled.

The weekly/manual job publishes total and per-test elapsed times in its GitHub
Actions summary, plus an `hgam-test-timings` CSV artifact retained for 90 days.
Per-test times include work performed before a visual snapshot skip; the total
also includes shared setup and runner overhead.

Every push and pull request to main runs the CRAN-core package checks on Linux,
macOS and Windows with R release. Visual comparisons and the paper-sized HGAM
examples are disabled in these checks. The complete pkgdown website also builds
on every push and pull request; pushes update the development website.

The weekly workflow runs on Mondays at 04:17 UTC. It checks main against the
last successful scheduled run and skips the expensive jobs when the commit has
not changed. A failed suite is retried the following week. A manual weekly run
always runs the full suite on the selected ref.

The weekly suite includes broader package checks on Linux, macOS and Windows,
Windows ARM, R devel, macOS oldrel, development glmmTMB, the separate sdmTMB
compatibility jobs, coverage, visual comparisons and the paper-sized HGAM
examples. The oldrel job uses the CRAN-core tests; the other package checks and
coverage use the broader suite. Optional inlaws, bayesplot and loo integration
checks run after the weekly Linux R-release package check, so their failures
cannot prevent that package check from running. The individual secondary,
sdmTMB and coverage workflows can also be run manually.

The weekly macOS visual job uses a fixed R version and Apple Accelerate BLAS,
matching the local baseline environment; image differences should be reviewed,
not automatically accepted. Existing platform restrictions on legacy numerical
snapshots remain in place.

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

To select the CRAN tests through devtools without adding `--as-cran`, explicitly
disable devtools' `cran` option:

```r
devtools::check(cran = FALSE, env_vars = c(NOT_CRAN = "false"))
```

## Measuring test runtime

Use installed copies so each run checks a fixed set of sources and snapshot
cleanup cannot change the checkout. Install each revision into a separate
library with `R CMD INSTALL --install-tests --library=LIBRARY SOURCE`, then
profile the tests with the same R installation, dependencies and environment.
Keep the machine free of other tests and model fits during timing runs.

The following command runs the CRAN suite serially and saves total CPU and
elapsed times, per-test and per-file CSV files, result objects and session
information. The timer includes shared setup and file-level work, and excludes
installation. The profiling script always excludes the HGAM paper file.

```sh
NOT_CRAN=false VDIFFR_RUN_TESTS=false GRATIA_RUN_HGAM=false \
  TESTTHAT_PARALLEL=false TESTTHAT_CPUS=2 OMP_NUM_THREADS=1 \
  OPENBLAS_NUM_THREADS=1 Rscript --vanilla tools/profile-tests.R \
  /tmp/gratia-test-profile /tmp/gratia-test-library
```

Set `NOT_CRAN=true` to profile the full numerical and integration suite with
the same script. Visual comparisons remain disabled in this command; their
dedicated job checks the stored images.

The second 25 October 2026 comparison used serial installed-package runs on Apple
Silicon/macOS, R 4.6.1, mgcv 1.9-4 and Accelerate BLAS, with the environment
shown above. Both runs completed without failures or warnings; R reported
these times before and after the changes:

```text
   user  system elapsed
439.577  17.502 497.936

   user  system elapsed
203.185   9.750 222.086
```

Elapsed time fell by 55.4%, and CPU time by 53.4%. Broader plot and coordinate
matrices, model/family combinations, native likelihood profiles and research
sampler tests now run in the full tier. The CRAN worker check shares one pool
across the main operations. These measurements cover the test suite; package
installation, examples and vignettes are outside the timer.

The revised CRAN suite passed 23,326 assertions, compared with 35,699 before
this second reduction. A separate run used `trace()` to count entry into every
exported function and confirmed calls to all 154; it passed the same 23,326
assertions without failures, errors or warnings. That instrumented run was not
used for timing.

The full numerical/integration suite passed 39,106 assertions with no failures,
errors or warnings using serial test files, including its internal worker
comparisons. Every previous full-suite test group retained its passing
assertions, and the compact checks added 56. Image comparisons and HGAM paper
tests were excluded. The two new core files also passed with parallel test-file
execution; a full parallel attempt stalled while the runner was waiting for
completion messages from idle workers, so it was interrupted and rerun serially.

## Builder diagnostics

Each scasm reference shape has a separate named test, so fitting errors identify
the shape even when fitting stops before an assertion.

Package checks use the normal testthat reporter. The macOS oldrel check also
writes test locations and call checkpoints to a separate progress file, keeping
them out of captured messages and snapshots. It first runs the focused sdmTMB
threshold tests with a 15-minute limit. Progress files, `test-all.Rout*`, and
check logs are uploaded even when a step fails or is cancelled.

To run the same focused tests locally:

```sh
NOT_CRAN=false VDIFFR_RUN_TESTS=false GRATIA_TEST_DIAGNOSTICS=true \
  GRATIA_TEST_PROGRESS=/tmp/gratia-threshold-progress.log \
  Rscript --vanilla tools/test-sdmtmb-thresholds.R
```

The report includes dependency versions, per-test times, and total elapsed time.
An unfinished fitting or prediction call leaves a START checkpoint without DONE.

The weekly full Windows ARM R-release check runs alongside the Windows x64 check,
verifies that R is native ARM, and retains the repositories provided by the
ARM R installation.

The focused Windows ARM sdmTMB compatibility job also enables the temporary
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
