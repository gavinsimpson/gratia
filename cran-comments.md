## R CMD check results

The checks below cover the source for gratia `0.12.0`.

All checks finished with 0 errors and 0 warnings.

- Local macOS Tahoe 26.7.3 (arm64), R 4.6.2, using `R CMD check --as-cran`.
  0 NOTEs. Visual snapshots ran. `GRATIA_RUN_HGAM=false` skipped the HGAM paper tests.
  With `OMP_THREAD_LIMIT=1`, OpenMP reported that a test requesting two
  threads ran with one.
- Win-builder on Windows Server 2022 (x86_64), R-devel (2026-11-17 r90926).
  0 NOTEs. [Win-builder R-devel log](https://win-builder.r-project.org/dMs6fW9SAqi4/00check.log)
- Win-builder on Windows Server 2022 (x86_64), R-release (R 4.6.2).
  0 NOTEs. [Win-builder R-release log](https://win-builder.r-project.org/1J1NcXZUjvSk/00check.log)
- Earlier GitHub Actions run on Ubuntu 26.04.1 (x86_64), R 4.6.2.
  2 NOTEs. [GitHub Actions check log](https://github.com/gavinsimpson/gratia/actions/runs/44926990710/job/136918387105)

### GitHub Actions NOTEs

- S7 1.0.0 printed an extra `attr(*, "class")= chr "gg"` line in each of the
  `gratia_palette()` and `gratia_theme_get()` examples. Those two calls now use
  `\dontdiff{...}` to exclude their printed output from the saved-output
  comparison. That comparison passed on both Win-builder runs above.
- The GitHub Actions compiled-code check reported `gratia.so: no symbols`.
  The runner strips shared-library symbols with `_R_SHLIB_STRIP_=true`.

## Reverse dependency check results

The GitHub Actions workflow used the `most` setting to check six CRAN reverse
dependencies against gratia `0.11.2` from CRAN and the `0.12.0` release
candidate. Five checked cleanly with both versions. galamm had the same
compiled-code NOTE with both versions because the runner strips shared-library
symbols. No regressions were found. [GitHub Actions comparison](https://github.com/gavinsimpson/gratia/actions/runs/44926990710/job/136923763025)

With `_R_CHECK_FORCE_SUGGESTS_=FALSE`, checks continued without the suggested
packages cmdstanr and INLA (gdpar), and gurobi (smimodel). Functionality that
requires these packages was outside the scope of the checks.
