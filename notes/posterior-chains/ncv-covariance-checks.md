# NCV covariance investigation: validation

Validated on 15 October 2026 using R 4.6.1 and mgcv 1.9-4, on
`gls/posterior-chains`. All tests, model-fitting experiments and package checks
were run outside the sandbox.

## Numerical investigation

* 200 weighted and 200 unweighted Gaussian response refits; all converged.
  No fitted result was excluded or clipped. The weighted fitted smoothing
  parameters reproduce the previous experiment exactly.
* 20,000 fixed-smoothing response simulations per design independently verify
  the total-gradient covariance and expected individual-gradient crossproduct.
* Response-unit, criterion-normalization and log-base checks passed.
* Saved results also reproduce the native `Vg` construction within 1e-7.
* Row-deletion fits and finite differences independently verify the Gaussian
  prediction and gradient implementation.

## Tests and package checks

* Focused NCV and smoothing tests at review: 297 assertions passed, no failures,
  warnings or skips. The new Gaussian file contributes 39 assertions, including
  11 jackknife checks.
* `R CMD build` completed, including vignette building.
* `R CMD check --as-cran --no-manual`: **Status: OK**. Full installed-archive
  tests passed 17,834 assertions with no failures or warnings. Examples,
  including `--run-donttest` examples, and vignette rebuilding passed.
* There were 197 visual-test skips because `VDIFFR_RUN_TESTS=false` was
  deliberately set. Visual snapshot comparisons were not performed.
  Snapshot cleanup messages concerned the disposable check copy; the source
  snapshots are unchanged.
* `--no-manual` skipped PDF manual compilation.
  `_R_CHECK_CRAN_INCOMING_REMOTE_=false` disabled remote incoming checks.
  These omissions did not produce package-check warnings or notes.
* All package implementation hashes match the checked archive. After that full
  check, the test file gained 11 independent jackknife assertions; the focused
  tests above passed with those additions. The full suite was not repeated
  for the test-only addition. Research notes gained the paper and jackknife
  comparisons; the study script gained an assertion checked against all 400
  saved fits.
* `pkgdown::check_pkgdown()` found no reference-index configuration problems.
  A full `pkgdown::build_reference(lazy = FALSE, examples = FALSE)` build
  succeeded in a temporary directory. Every local topic link from the reference
  index resolves to a generated page. Reference examples were not rerun in
  this website build; package-check examples had already passed.
  New helpers remain unexported, so there are no new public reference topics.
  Pandoc reported deprecation warnings for pkgdown's `--katex` option;
  these do not indicate a missing topic or an invalid reference index.
* `git diff --check` passed; no existing package code, public documentation,
  namespace entries or snapshots were edited.

The fitted-mean covariance approximation still overestimates variances by
20–38% in these examples. Interval coverage, other response families and
dependent observations have not been validated. The helpers remain internal.

Review found no regression or unrelated edits. The independent row-deletion
test reproduces the native Gaussian jackknife covariance and confirms that
enabling it leaves the smoothing covariance calculation unchanged.
