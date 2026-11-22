## R CMD check results

The checks below cover the source for gratia `0.12.0`.

All checks finished with 0 errors and 0 warnings.

- Local macOS Tahoe 26.7.3 (arm64), R 4.6.2, using `R CMD check --as-cran`.
  0 NOTEs. Visual snapshots ran.
- Win-builder on Windows Server 2022 (x86_64), R-devel (2026-11-17 r90926).
  0 NOTEs. [Win-builder R-devel log](https://win-builder.r-project.org/dMs6fW9SAqi4/00check.log)
- Win-builder on Windows Server 2022 (x86_64), R-release (R 4.6.2).
  0 NOTEs. [Win-builder R-release log](https://win-builder.r-project.org/1J1NcXZUjvSk/00check.log)
- The package has also been checked on Linux, Windows, and MacOS X through GitHub Actions. 0 NOTES.

## Reverse dependency check results

The GitHub Actions workflow used the `most` setting to check six CRAN reverse
dependencies against gratia `0.11.2` from CRAN and the `0.12.0` release
candidate. Five checked cleanly with both versions. galamm had the same
compiled-code NOTE with both versions because the runner strips shared-library
symbols. No regressions were found. [GitHub Actions comparison](https://github.com/gavinsimpson/gratia/actions/runs/44926990710/job/136923763025)

With `_R_CHECK_FORCE_SUGGESTS_=FALSE`, checks continued without the suggested
packages cmdstanr and INLA (gdpar), and gurobi (smimodel). Functionality that
requires these packages was outside the scope of the checks.
