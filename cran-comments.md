## Resubmission

Version 0.12.0 went unreleased because the test suite took 16 minutes 20
seconds during CRAN's Windows incoming check. Version 0.12.1 runs
representative tests of the main package functions on CRAN and keeps the
detailed tests in the full suite for local and GitHub Actions runs.
[0.12.0 Windows incoming log](https://win-builder.r-project.org/incoming_pretest/gratia_0.12.0_20261122_163602/Windows/00check.log)

## R CMD check results

Version 0.12.1 passed `R CMD check --as-cran` on macOS Tahoe 26.7.3 (arm64)
with R 4.6.2. The check reported 0 errors, 0 warnings and 0 notes. Tests
took 61 seconds. `NOT_CRAN=false` selected the CRAN tests,
`VDIFFR_RUN_TESTS=false` disabled visual comparisons, and
`GRATIA_RUN_HGAM=false` kept the paper examples out of the run.

The previous 0.12.0 candidate passed Win-builder on Windows Server 2022
with 0 errors, 0 warnings and 0 notes on both R-release and R-devel.
[Win-builder R-release log](https://win-builder.r-project.org/1J1NcXZUjvSk/00check.log)
[Win-builder R-devel log](https://win-builder.r-project.org/dMs6fW9SAqi4/00check.log)

## CRAN check NOTE for 0.11.2

The Debian Linux R-devel checks with GCC and Clang report an Rd contents NOTE
for gratia 0.11.2 because `fixef.Rd` documents arguments without a `\usage`
section. Version 0.12.1 removes the `fixef()` re-export and its help page,
resolving the NOTE.
[CRAN check results for gratia](https://cran.r-project.org/web/checks/check_results_gratia.html)

## Reverse dependency check results

The GitHub Actions workflow used the `most` setting to check six CRAN reverse
dependencies against gratia `0.11.2` from CRAN and the previous `0.12.0`
candidate. Five checked cleanly with both versions. galamm had the same
compiled-code NOTE on both because the runner strips shared-library symbols.
No regressions were found. Changes since that candidate cover package tests,
documentation and release metadata.
[GitHub Actions comparison](https://github.com/gavinsimpson/gratia/actions/runs/44926990710/job/136923763025)

With `_R_CHECK_FORCE_SUGGESTS_=FALSE`, checks continued without the suggested
packages cmdstanr and INLA (gdpar), and gurobi (smimodel). Functionality that
requires these packages was outside the scope of the checks.
