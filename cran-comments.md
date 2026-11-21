The checks below apply to the 7 November candidate for gratia 0.12.0.
Results for the integrated release source are pending.

The 7 November candidate was checked under:

* R 4.6.2 on macOS Tahoe 26.6.1 (aarch64), locally,
* R 4.6.2 on Windows Server 2022 (x86_64), via winbuilder r-release, and
* R-devel (2026-11-06 r90845) on Windows Server 2022 (x86_64), via winbuilder.

All three checks completed on 7 November 2026 with Status: OK.

Reverse dependency checks completed on 7 November 2026, with no regressions
against the CRAN version of gratia (0.11.2). Details are below.

## R CMD check results

0 errors | 0 warnings | 0 notes

### Local release check

`make check-release` passed for commit `f74ce435b`, using R's reference BLAS
and mgcv 1.9-5. The target builds the source package and runs
`R CMD check --as-cran`, enabling the local tests, visual snapshots and HGAM
paper tests with `NOT_CRAN=true`, `VDIFFR_RUN_TESTS=true` and
`GRATIA_RUN_HGAM=true`.

All 39,694 test assertions passed, with no failures, test warnings or skips.
Help examples, including `--run-donttest`, saved example-output comparisons,
vignette rebuilding, compiled-code checks, and PDF and HTML manuals passed.
The installed package size (6.7 MB) was reported as INFO.

The target sets `OMP_NUM_THREADS=1` and `OMP_THREAD_LIMIT=1`. OpenMP printed
runtime notices when a test requested two threads and ran with one instead.
The tests completed with one OpenMP thread.

### Winbuilder r-release

The [winbuilder r-release log](https://win-builder.r-project.org/sZQBNEARvWsO/00check.log)
reports Status: OK for gratia 0.12.0 under R 4.6.2. Tests, examples, saved
example-output comparisons, vignette rebuilding, compiled-code checks, and
PDF and HTML manuals all passed, with no errors, warnings or NOTEs.

### Winbuilder r-devel

The [winbuilder r-devel log](https://win-builder.r-project.org/ndk7OhlaPOhy/00check.log)
reports Status: OK for gratia 0.12.0 under R-devel (2026-11-06 r90845).
Tests, examples, saved example-output comparisons, vignette rebuilding,
compiled-code checks, and PDF and HTML manuals all passed, with no errors,
warnings or NOTEs.

## Reverse dependency check results

The [GitHub Actions run](https://github.com/gavinsimpson/gratia/actions/runs/42411631120)
checked gratia 0.11.2-74 at commit `7331531c9` on
`gls/revdep-0-11-2-74`, using the "most dependents" setting. gratia and all
six reverse dependencies completed with no errors, warnings or NOTEs. Symbol
stripping was disabled, and the previous compiled-code NOTEs for gratia and
galamm were absent.

The optional suggested packages cmdstanr and INLA (for gdpar), and gurobi
(for smimodel), were unavailable. The workflow uses
`_R_CHECK_FORCE_SUGGESTS_=FALSE`, so checks continued without them; functionality
requiring those packages was not covered.

These results predate the subsequent plot-subtitle and documentation changes
on the release candidate branch.

```text
------- Check results summary ------
Check status summary:
                  OK
  Source packages  1
  Reverse depends  6

Check results summary:
gratia ... OK
rdepends_cgaim ... OK
rdepends_galamm ... OK
rdepends_gdam ... OK
rdepends_gdpar ... OK
rdepends_smimodel ... OK
rdepends_stgam ... OK

------- Check for regressions ------
No changes between old and new version
```
