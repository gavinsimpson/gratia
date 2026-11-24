# Test tiers

The CRAN suite uses small examples of public functions, representative numerical
comparisons and argument checks. It reuses the shared models and checks fitted
values, covariance calculations, densities and generated responses against
native model results or independent formulas. No production behavior depends
on the test tier.

The full suite checks broader combinations of model classes, families,
constructors, coordinate transformations, labels, themes and interval methods.
Large simulation convergence comparisons, native likelihood-profile matrices
and research sampler variants also belong there. The CRAN check driver,
`make test` and the profiler select representative test files through
`cran_test_filter()` in `testthat/helper-cran.R`. Selected files can use
`skip_on_cran()` or smaller case lists for expensive repetitions. When adding
a feature, include a small test for its public behaviour in the CRAN selection.
For GJRM, glmmTMB and sdmTMB, the help examples fit and draw native models on
CRAN when the packages are available. The weekly GitHub Actions jobs and local
full runs include the detailed adapter tests.

`test-function-core.R` keeps compact moment, plotting, theme, label and utility
checks alongside the existing focused numerical tests. The full suite includes
`test-parallel-core.R`, which compares derivatives, partial derivatives, smooth
samples, smooth differences and ALE with their serial results. It also checks
random-number state and worker error handling.

Run the CRAN selection explicitly:

```sh
make test
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

## Weekly GitHub Actions checks

**Weekly checks** runs on Mondays at 04:17 UTC. It includes package checks with
released dependencies on Linux, macOS and Windows, including Windows ARM and
macOS R oldrel. It also runs the separate sdmTMB compatibility jobs, coverage,
visual comparisons and the paper-sized HGAM examples. The oldrel job uses the
CRAN-core tests. The other package checks and coverage use the broader suite.
The optional bayesplot and loo integration checks run after the Linux R-release
package check, so their failures cannot prevent that package check from running.

**Weekly upstream compatibility** runs on Mondays at 04:27 UTC. It checks
R devel, development glmmTMB and inlaws from GitHub. Both scheduled workflows
run even when `main` has no new commits, so they can detect changes in upstream
dependencies.

Weekly package checks record the R, runner and dependency versions in the job
summary, including the commit for packages installed from GitHub. On failure,
the check logs and test progress are available as an artifact. The package check
step has a 90-minute limit so a stalled test leaves time to collect those logs.

To run all weekly and upstream checks on demand, use the default `check=all`
setting in **Weekly checks**. Use the `check` input to run a single check.

```sh
gh workflow run weekly-checks.yaml --ref main
gh workflow run weekly-checks.yaml --ref main -f check=glmmtmb-development
```

Other choices include `r-devel`, `package-windows-arm`, `visual`,
`paper-examples`, `sdmtmb-linux`, `sdmtmb-windows-arm` and `coverage`. To run
only the spatial tests from the Windows ARM package job, use
`-f check=windows-arm-spatial`. To run only the upstream checks, use
**Weekly upstream compatibility**, or run
`gh workflow run weekly-upstream.yaml --ref main`. That workflow also accepts
`-f check=glmmtmb-development` to run just the development glmmTMB check.
The secondary, sdmTMB and coverage workflows also support manual runs.

A fresh dispatch uses the current commit of the selected ref. A rerun from
an earlier workflow run uses that run's original commit. Dispatch a fresh run
against `main` to verify a fix that has since been merged.

The weekly macOS visual job uses R 4.6.2 with R's reference BLAS to match the
snapshot baselines. Review image differences before accepting them. Some
legacy numerical snapshots have platform restrictions.

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
  contract. The full suite checks family-quantile plots for eight families,
  with fewer observations in the Tweedie fits to limit the cost of numerical
  integration.
- Measure shared setup and top-level code as well as test-body durations. A
  reported skip may happen after substantial computation.

For an installed-package check, export `NOT_CRAN=false` (core) or `NOT_CRAN=true`
(broader suite) before `R CMD check`. Both `make check` and
`make check-test-cran` select the CRAN tests.

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
sampler tests moved to the full tier. The CRAN worker check shared one pool
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
