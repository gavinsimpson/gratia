# Ordinary and extended-family uncertainty: validation

Validated on 15 October 2026 with R 4.6.1 and mgcv 1.9-4. All fitting
investigations, targeted tests, installed-package tests and package checks ran
outside the sandbox.

## Scope and review

The NCV removal was reviewed separately and committed as `cacdf9e62` on
`gls/posterior-chains`. Its Gaussian conditional evaluator and sampling
algorithm were retained. The subsequent family extension remains internal;
public sampling interfaces, exports and reference topics are unchanged.

The extension uses a cached original design and penalties with native
conditional normal fits. It preserves raw responses, weights, fitting offsets,
coefficient coordinates and shared/fixed smoothing mappings. Family
initialisation uses copied environments and retains fitted nuisance parameters.
The audit distinguishes the family-parameter covariance blocks in gam from
BAM's separately updated parameters, and separates global dispersion from
component scales. The tested censored-Poisson BAM configurations remain
rejected because conditional reconstruction does not agree with the fit.
An independent Poisson objective and gradient check confirms that disagreement.

Review found no unrelated edits. Research documentation and inventories have
been updated; the original Gaussian numerical study and its saved values are
retained. The Gaussian setup and 100 seeded draws for each of a gam ML and
non-discrete bam fREML fit are exactly identical to the committed implementation,
including their returned metadata.

## Tests and package checks

* Targeted smoothing tests: **992 assertions passed**, with no failures,
  errors, warnings or skips.
* The installed-package full suite passed. The archive check independently
  passed **18,540 assertions**, with no failures or warnings.
* Tests include independent fixed-smoothing refits, prediction covariance in
  different coefficient bases, shared and fixed parameters, tensor products,
  weights, offsets, missing rows, binomial encodings, censored and grouped
  responses, retained family state, covariance ordering, deterministic draws
  and explicit failures for unsupported configurations.
* `R CMD build` and `R CMD check --as-cran --no-manual` completed with
  **Status: OK**, without errors, warnings or notes. Ordinary examples,
  `--run-donttest` examples, and vignette building/rebuilding passed.
* The full pkgdown reference and migration article build passes: **148 public
  topics and 201 index labels** resolve, including sampling aliases and local
  reference links.

`VDIFFR_RUN_TESTS=false` deliberately skipped **197 visual tests** in both
full runs. Visual comparisons were not performed. Unused snapshot cleanup was
confined to installed test copies; source snapshots remain unchanged.
`--no-manual` skipped compilation of the PDF reference manual.
`_R_CHECK_CRAN_INCOMING_REMOTE_=false` disabled remote incoming checks.
These choices did not skip non-visual tests, examples or vignettes.

Pkgdown reference examples used `examples = FALSE` because the package check
already ran them. Pandoc reports deprecation warnings for pkgdown's `--katex`
and `--mathjax` options; reference rendering and index validation pass. These
are separate from the warning-free package check.

The checked package sources and tests match the current files by checksum.
Subsequent research-note edits are excluded from the package archive.
Final check directory:
`/private/tmp/gratia-smoothing-families-reviewed-check-16b3b49cc8f19`.
Reference output:
`/private/tmp/gratia-smoothing-families-commit-reference`.

## Research inventory and next step

See [the family report](smoothing-families.md),
[complete results](smoothing-families.csv),
[additional rejected BAM fits](smoothing-family-rejections.csv), and
[independent Poisson check](smoothing-cpois-check.txt).
The final inventory runs separately from other tests and checks so its local
setup and sampling timings are not affected by concurrent validation.

The inventory passes 127 of 130 family/fitter/method combinations through
conditional reconstruction, a changed smoothing value and 25 mixture draws.
Only the three censored-Poisson BAM cases (ML, REML and fREML) are rejected.
All eight additional BAM Poisson probes also fail reconstruction. General
quasi families are included in this inventory alongside quasi-Poisson and
quasi-binomial. Maximum relative reconstruction errors at fitted smoothing values among
passing fits are 2.09e-5 for coefficients and 4.97e-6 for covariance.

```sh
Rscript notes/posterior-chains/smoothing-families.R --load-all
Rscript notes/posterior-chains/smoothing-cpois-check.R --load-all
Rscript notes/posterior-chains/smoothing-methods.R --load-all
```

Keep the extension internal until repeated-simulation validation assesses
interval calibration across the supported families and fitting methods.
Dispersion and family nuisance parameters stay fixed during conditional
updates. These draws propagate an approximate distribution of smoothing
estimates; they do not integrate a joint Bayesian posterior. Discrete BAM,
general families, and the remaining fitting/penalty restrictions are documented
in the family report.

## Review before commit

The review corrected the family-covariance flag: it now requires an actual
retained `V.sp` matrix with recognised parameter ordering. In particular,
an extended Fellner-Schall (`optimizer = "efs"`) fit without that matrix is
no longer marked as retaining a family-parameter covariance block. Focused
tests cover EFS, absent matrices, invalid dimensions and an available joint
covariance.

The reconstruction test now deliberately perturbs a stored coefficient,
rather than assuming mgcv's default convergence tolerance always produces an
inaccurate fit. The redundant test using an artificial family-parameter count
has been removed. The revised targeted suite passes 992 assertions without
failures or warnings. Exact Gaussian setup and seeded draws remain unchanged.

Additional review probes combine missing rows and call-level offsets for
negative binomial, beta, Tweedie, censored normal and grouped families. These
pass. The grouped ordinal example reports a native optimisation step failure
and is rejected by the reconstruction check, as required for an inaccurate
fit. No unrelated edits were found.

After these corrections, a fresh archive check and the reference build pass.
The checked source and test hashes match the files prepared for commit.
