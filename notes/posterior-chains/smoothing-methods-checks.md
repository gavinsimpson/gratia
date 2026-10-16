# ML/REML/fREML smoothing uncertainty: validation

This record covers the NCV removal committed as `cacdf9e62`. The subsequent
[ordinary and extended-family extension](smoothing-families-checks.md) has its
own validation record.

Validated on 15 October 2026 with R 4.6.1 and mgcv 1.9-4. All model-fitting
investigations, tests and package checks run outside the sandbox.

## Scope and preserved research

The feature retains the internal Gaussian identity prototype for gam ML/REML
and non-discrete bam ML/REML/fREML. Dispersion remains fixed within a fit.
NCV and QNCV uncertainty propagation, their dedicated tests, study scripts
and results have been removed from this work. Their research is preserved in
`/Users/gavin/work/git/gratia/research-archives/ncv-uncertainty-2026-10-15.tar.gz`
with a separate manifest. The archive contents and copied-file checksum were
verified before removing the dedicated files.

The retained rejection tests use actual NCV/QNCV gam and discrete NCV bam
fits. Changing the stored call or supplying V.sp cannot bypass the fitted
method restriction. The conditional evaluator and Gaussian draw algorithm
are unchanged.

## Completed validation

* Focused smoothing and prescribed-contribution tests: 285 assertions pass
  without failures, warnings or skips, including nine NCV exclusion checks.
* The supported fitting-method inventory has been regenerated without NCV
  cases or Vg metadata.
* Eleven supported Gaussian fits each used 20,000 mixture draws. All saved
  non-timing values in the mixture and quadrature tables agree exactly with
  the previous results. Conditional refits, finite differences, numerical
  integration and the law of total variance remain verified.
* The existing benchmark timings were retained to keep this change focused
  on scope; the validation rerun is logged separately.
* Review before commit found no regression or unrelated edit. The retained
  conditional evaluator and draw algorithm are unchanged.
* The full pkgdown reference and migration article build passes: all 148
  public topics and 201 index labels resolve, including sampling aliases.
  Reference examples were omitted with `examples = FALSE` because package
  checks already ran them. Pandoc reports deprecation warnings for pkgdown's
  `--katex` and `--mathjax` options; rendering and index checks pass.

## Full package check

`R CMD build` and `R CMD check --as-cran --no-manual` completed with
**Status: OK**, with no errors, warnings or notes. The installed-archive
suite passed 17,781 assertions with no failures or warnings. Ordinary examples,
`--run-donttest` examples and vignette building/rebuilding passed.

`VDIFFR_RUN_TESTS=false` deliberately skipped 197 visual tests. Visual
comparisons were not performed, and source snapshots remain unchanged.
`--no-manual` skipped PDF reference-manual compilation.
`_R_CHECK_CRAN_INCOMING_REMOTE_=false` disabled remote incoming checks.
These options did not skip non-visual tests, examples or vignettes.

Package source and test hashes match the checked archive. Subsequent edits
only update research notes, which are excluded from the package archive.
Final check directory:
`/private/tmp/gratia-smoothing-likelihood-only-check-124c365b71b40`.

## Reproduction and next step

```sh
Rscript notes/posterior-chains/smoothing-methods.R --load-all
Rscript notes/posterior-chains/smoothing-study.R --load-all 20000
```

The next work is repeated-simulation interval-coverage validation of the
Gaussian approximation across supported fitting methods. Conditional moments
alone do not validate coverage. See [smoothing-methods.md](smoothing-methods.md)
for the proposed comparison and remaining restrictions. The prototype remains
internal and propagates approximate estimation uncertainty.
