# Smoothing-method extension: validation

Validated with R 4.6.1 and mgcv 1.9-4. Every fitting investigation, test and
package check ran outside the sandbox. The final archive and checked source
hashes match the worktree. All 302 tracked visual snapshot references are
preserved unchanged.

## Results

* Focused smoothing and fixed-contribution tests: 310 passing assertions;
  no failures, warnings or skips.
* Final installed-archive full suite: `[ FAIL 0 | WARN 0 | SKIP 197 | PASS 17806 ]`.
* Final `R CMD build` and `R CMD check --as-cran --no-manual`: Status OK,
  with no errors, warnings or notes. Vignettes were built and rebuilt;
  ordinary examples and `--run-donttest` examples passed.
* Eleven Gaussian fits each used 20,000 mixture draws. Independent
  conditional refits, finite differences, numerical integration and the
  total-variance calculation passed. All previously recorded non-timing
  study results are identical, including seeded sampling summaries.
* The fitting-method inventory and independent NCV loss/sandwich checks
  passed. The separate 200-repetition NCV experiment retained every fit;
  all reported full convergence. Its poor covariance calibration remains
  a limitation of using native Vg, as described in smoothing-methods.md.

## Options and skipped validation

`VDIFFR_RUN_TESTS=false` deliberately skipped 197 visual comparison tests.
Every skip in the final suite has that reason. `NOT_CRAN=true` and an unset
`CI` allowed the other guarded tests to run. All required suggested packages
were available; `_R_CHECK_FORCE_SUGGESTS_=true` did not skip dependencies.

`--no-manual` omitted PDF reference-manual compilation. It did not skip
vignettes, examples or tests. `_R_CHECK_CRAN_INCOMING_REMOTE_=false` disabled
remote CRAN incoming checks; local incoming checks still ran. The final
archive check ran the complete non-visual test suite; `--no-tests` was not
used.

## Earlier validation runs

An earlier archive check passed 17,801 assertions before the final five
scalar-Vg assertions were added. It had one timing NOTE: the existing
parametric_effects example took 5.64 seconds, exceeding the five-second
threshold reported by --as-cran. The final archive reported no timing NOTE.

A separate source-suite attempt used the wrong installed library in mirai
workers and failed because that older gratia lacked sampling_mh_worker.
This was a validation library-path error. The final archive check installed
the matching package and passed the parallel-chain tests. The source-suite
snapshot cleanup also removed skipped visual references; these were restored
from the clean starting revision and verified before building the final
archive. No snapshot-reference changes are part of this work.

## Reproduction

```sh
Rscript notes/posterior-chains/smoothing-methods.R --load-all
Rscript notes/posterior-chains/smoothing-study.R --load-all 20000
Rscript notes/posterior-chains/smoothing-ncv-study.R 200
```

For parallel tests, use a matching installed checkout or run R CMD check on
the built archive. A source namespace loaded by pkgload does not replace an
older package on a worker's library search path.

Final check directory: `/private/tmp/gratia-smoothing-final-check-d4601fea688a`.

## Review before commit

The regression review found no unintended edits. Package source hashes still
match the successful final archive check. All previously saved non-timing
Gaussian study results and the repeated-sampling NCV summaries were verified.
The new helpers remain unexported, and no public Rd topics were added.

Pkgdown reference validation passed for all 148 public topics and 201 index
labels. Every indexed topic and local reference link resolves, sampling aliases
point to their canonical pages, and the sampling migration article builds.
The reference configuration required no changes.

Reference pages were built with examples = FALSE because the package check
already ran the examples. This choice omits example execution from the
pkgdown build only. Pandoc reported deprecation warnings for the --katex and
--mathjax options supplied by pkgdown; rendering and index validation passed.
