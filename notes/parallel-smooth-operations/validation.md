# Validation

The implementation was developed in the isolated worktree on
`gls/parallel-smooth-operations`, created explicitly from `main` at
`34d65fd69984916bac4d2382d804e9d536069b85`. All R test and check commands ran
outside the sandbox. Development builds were installed in private temporary
libraries; fresh mirai workers loaded the same package path as their caller.

The final installed build passed the standard CI suite: **26,728 assertions in
1,931 tests, with no failures, errors or warnings**. The results recorded 188
skipped test cases. The configuration used `NOT_CRAN=true`, `CI=true` and
`VDIFFR_RUN_TESTS=false`: broader numerical and integration tests ran, while
visual comparisons, the separate large HGAM paper examples and existing
platform or CI exclusions were skipped. Existing numerical and plot-build
checks still ran.

To reproduce that suite from the repository root:

```sh
NOT_CRAN=true CI=true VDIFFR_RUN_TESTS=false Rscript tools/run-tests.R
```

The new tests use actual one- and two-worker mirai pools. They check complete
serial and parallel outputs, supplied row order and repeats, selections,
factor-by smooths, transformed covariates and local expression environments,
pointwise and simultaneous intervals, bootstrap correction, and delegated model
classes. Worker-side traces and process IDs establish that derivative, partial
derivative, smooth sample and comparison calculations execute in workers. A
worker-only error through a public function must propagate; a serial retry
would incorrectly succeed. The caller's pool remains available after failures.

Random-number checks cover `set.seed()`, explicit seeds, an initially absent
`.Random.seed`, another RNG kind, and empty or zero-variance derivative terms.
Simultaneous draws remain successive draws in selection order and are generated
only for results with valid positive standard errors. `derivatives()` retains
its existing `set.seed()` workflow; no public seed argument was added. The
other functions retain their explicit `seed` controls and restore the caller's
state, including its absence. No worker starts a restarted term-specific random
stream.

Supplied joint coefficient draws are checked against direct `PredictMat()`
projections for every term. Reordered and nonconsecutive draw identities,
chain iterations and retained diagnostic draws are checked too. MH sampling
finishes before term workers start. Its values and metadata agree across worker
counts; comparisons exclude only elapsed seconds, which depend on scheduling,
and verify that those times remain finite and nonnegative.

Input tests verify that prepared frames retain factor contrasts and matrix
column attributes, grouped and rowwise inputs work, ordinary random-effect
formulas shed fitting environments, custom formulas retain theirs, and SCAM's
numeric margin metadata remains unchanged. No plot annotations were changed.

The custom-prediction regression tests use fresh one- and two-worker pools whose
global environments contain neither the custom prediction method nor its helper.
Serial and worker results, including the caller's final RNG state, agree for
univariate smooths, tensor margins and factor-smooth bases. The custom method
uses a caller function and `NextMethod()`, and posterior values are also checked
against direct caller prediction matrices. Method detection covers aliases of
package functions and factor-smooth basis classes. A separate test fits a scam
basis with `mgcv::gam()` and verifies agreement while scam remains unloaded in
the workers. Custom matrices are prepared in the caller; worker S3 registrations
are unchanged.

The final source package was built with `R CMD build --no-build-vignettes` and
checked with:

```sh
R CMD check --no-tests --no-manual --no-vignettes gratia_0.11.2-53.tar.gz
```

The check completed with **zero errors, two warnings and one note**. Code,
dependencies, compiled code, documentation and examples passed. Both warnings
are expected consequences of `--no-build-vignettes`: the source package contains
vignette sources but no generated `inst/doc` files, and therefore lacks matching
HTML/PDF vignette outputs. They are not failures of package calculations.
`--no-vignettes` skipped executing vignette code and rebuilding those outputs;
`--no-manual` skipped the PDF manual; `--no-tests` skipped the check's test phase,
which was run separately as the complete installed-package suite above.

The example-reference note contains only a blank-line difference and optional
`carrier` namespace-loading text; it contains no numerical differences.

Roxygen documentation was regenerated. `pkgdown::check_pkgdown()` found no
problems with the reference index. Its Pandoc calls emitted deprecation messages
for `--katex`; these were tooling messages, not missing topics or failed pages.
The change adds no public documentation topic and needs no reference-index entry.

Performance measurements and their input/output sizes are recorded separately
in [README.md](README.md), [results.csv](results.csv), [timings.csv](timings.csv)
and [session.txt](session.txt).
