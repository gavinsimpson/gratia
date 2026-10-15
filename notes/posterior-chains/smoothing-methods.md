# Smoothing uncertainty: fitting methods and NCV

This is an internal research extension, investigated with R 4.6.1 and mgcv
1.9-4. It does not add a public sampling method. The Gaussian prototype
propagates an approximate distribution of smoothing parameter estimates through
conditional coefficient distributions. It does not specify hyperpriors or
integrate a joint Bayesian posterior.

## Supported combinations

| Fitted model and actual method | Retained information | Internal calculation |
| --- | --- | --- |
| Gaussian identity `gam()`, ML or REML | `V.sp`, `sp.vcov()`, `db.drho`, `Vc` | Gaussian mixture at fixed fitted dispersion |
| Gaussian identity non-discrete `bam()`, ML or REML | Same quantities as ordinary `gam()` | Same mixture; stored derivatives use fitted coefficient coordinates |
| Gaussian identity non-discrete `bam()`, fREML | `V.sp` for log smoothing parameters; `sp.vcov()` can also include log dispersion | Same mixture; derivatives are reconstructed in fitted coefficient coordinates |
| Discrete `bam()`, fREML | Smoothing covariance and derivatives are available | Audit only; compressed fitting design has not been reconstructed for conditional updates |
| Gaussian identity `gam()`, NCV or QNCV | `Vg` and `db.drho`; no `V.sp` or `sp.vcov()` | Separate local delta-method contribution, with the restrictions below |
| Discrete `bam()`, NCV | No `Vg`, `V.sp` or `sp.vcov()` in the tested fits | No smoothing-estimator uncertainty propagation |
| Non-Gaussian or LSS models | Depends on family and fitter | Parameter-order audit only; no conditional mixture updates |

ML/REML tests include shared parameters, partly fixed penalties, tensor
products, known and estimated dispersion, unequal weights, offsets, and bounded
model-matrix blocks. For ML/REML `bam()`, conditionals at perturbed smoothing
parameters agree with independent fixed-parameter refits; derivatives also
agree with centred numerical differences.

The expanded [Gaussian study](smoothing-report.md) uses 20,000 draws for each
of 11 fits. Its largest relative errors at fitted smoothing parameters are
2.4e-13 for coefficient means and 3.5e-12 for conditional covariance. These
checks validate the conditional calculations and mixture moments. They do not
establish repeated-sampling coverage or accuracy of the fitted normal
approximation for every model.

The same restrictions as the earlier prototype remain: identifiable independent
Gaussian identity fits, positive fitted penalty multipliers, supported penalty
mappings and a finite positive definite covariance for the free log smoothing
parameters. Extra penalties, nonzero prescribed smooth contributions in BAM,
minimum smoothing parameters, edge correction, dependent observations and
prefit objects remain excluded. P-ML and P-REML have not been validated for this
mixture and are now rejected explicitly. With all smoothing parameters fixed,
there is no free smoothing covariance to propagate, so setup rejects the fit.

## Read the fitted method, not just the call

`sampling_smoothing_info()` records the requested method expression without
evaluating it, the actual `model$method`, whether the fitted object retains a
discrete design, and the stored score label and optimiser. These can differ:

* `gam(..., family = gaulss(), method = "ML")` actually fits by REML. Its stored
  score can still have the name `"ML"`.
* `bam(method = "ML", discrete = TRUE)` warns that discretisation is unavailable
  and fits a non-discrete ML model. REML has the same restriction.
* `bam(method = "NCV", discrete = FALSE)` warns and switches to a discrete fit.
* `gam(method = "ML", optimizer = "efs")` fits by REML and does not retain the
  required Hessian covariance in the tested example. The prototype rejects it.
* `gam(method = "fREML")` fails during fitting. fREML is a BAM method.
* With every smoothing multiplier fixed, non-discrete `bam(method = "fREML")`
  changes to REML. The retained scalar covariance is for log dispersion; there
  is no free log smoothing parameter to draw.
* Gaussian additive NCV fits can store optimiser `"magic"`, although
  `estimate.gam()` selects BFGS for NCV. The source and covariance construction
  were checked rather than inferring the calculation from this label.

See [smoothing-methods.csv](smoothing-methods.csv) for the complete inventory,
including native warnings and fitting failures.

## Covariance meaning and parameter ordering

For ordinary Gaussian ML/REML fits, `V.sp` and `sp.vcov()` put the free log
smoothing parameters first. An estimated log dispersion follows; it is absent
when dispersion is known. ML and REML use different smoothing-selection
criteria, but the Gaussian coefficient conditional at specified smoothing
parameters and dispersion has the same precision construction.

The matrices need not have identical numerical values. In installed mgcv,
`gam.fit3.post.proc()` constructs `V.sp` using eigenvalue handling and a 0.1
regularisation of criterion curvature. `sp.vcov()` instead inverts the stored
Hessian with its own `reg` adjustment (default 0.001); its implementation adds
`reg` to the Hessian matrix, not just its diagonal. The prototype continues to
use `V.sp`, so this extension does not change the smoothing draws for existing
supported inputs. `sp.vcov(edge.correct = FALSE)` is recorded separately; it is
not substituted for `V.sp`.

For fREML BAM, `V.sp` omits the dispersion row and column, although the stored
Hessian and `sp.vcov()` can include them. The prototype uses the marginal
covariance of free log smoothing parameters and holds dispersion fixed in each
conditional calculation. This is different from conditioning the log-smoothing
normal approximation on a fixed log dispersion.

Shared and fixed penalties satisfy

```
log(full.sp) = L %*% log(sp) + lsp0
```

`sp` contains free parameters; `full.sp` contains the multipliers for individual
penalty blocks. Setup uses `attr(V.sp, "L")` and retains the fitted additive
log offset `lsp0`. It does not sample fixed multipliers independently or treat
`attr(V.sp, "spind")` as indices into the free covariance: in shared-parameter
fits those indices can include a dispersion position.

For the audited extended-family `gam()` fits, estimated family parameters
precede free log smoothing parameters. A separately estimated log dispersion
comes last. Examples are:

| Family | Additional scalar entries before log smoothing parameters | Separate final log dispersion |
| --- | --- | --- |
| `nb()` with estimated theta | log negative-binomial theta | No |
| `nb(theta = 3)` | None; theta is fixed | No |
| `tw()` | Internal transformed power parameter | Yes when estimated |
| `scat()` | Two internally transformed family parameters | No |
| `gaulss()` | None outside the joint coefficient vector | No |

The audit records `family$n.theta`, internal values from `getTheta()`, and
response-scale values from `getTheta(TRUE)` where available. Its explicit
matrix positions prevent treating family parameters as log smoothing parameters.
It leaves extended-family BAM scalar ordering unclassified: these fitters
update family parameters separately, and that ordering has not been validated
here. Every non-Gaussian conditional mixture remains rejected, including LSS
models with scale coefficients. Dispersion and additional family parameters
are never silently sampled by the Gaussian prototype.

`db.drho` also needs a coordinate check. Ordinary `gam()` and ML/REML `bam()`
derivatives agree with conditional mean derivatives after applying the
smoothing mapping. Non-discrete fREML BAM retains raw derivatives in an
internal fitting basis. Setup therefore reconstructs derivatives from the
Gaussian precision for all supported fits. ML/REML `Vc` includes the native
coefficient covariance correction; fREML BAM uses transformed derivatives
internally when constructing its `Vc`. Neither matrix is evidence that a
coefficient sampler has integrated smoothing uncertainty.

## NCV is a separate estimation problem

In installed mgcv, `gam.fit3()` first constructs a matrix from crossproducts
of the per-prediction NCV gradient contributions. At the end of `bfgs()`, mgcv
multiplies this on both sides by its inverse-curvature approximation and
applies the smoothing mapping. The final fitted `Vg` is therefore an
approximate covariance of log smoothing parameter estimators, rather than an
inverse NCV Hessian presented as a posterior covariance.

For the independent, unshared Gaussian leave-one-out examples, separate finite
differences of each exact weighted squared prediction error reproduce this
sandwich construction to 1.3e-6 maximum absolute error. The total criterion
agrees within 4e-14. NCV and QNCV agree for this quadratic loss. The largest
absolute difference between `Vg` and the inverse stored NCV Hessian is 21.3 in
these examples, showing why those matrices cannot be interchanged. See
[smoothing-ncv-checks.csv](smoothing-ncv-checks.csv).

The one-parameter case is handled explicitly because native BFGS can store
`Vg` as a scalar rather than a one-by-one matrix.

`sampling_smoothing_ncv_delta()` verifies the stored matrix dimensions,
symmetry, positive semidefiniteness and coefficient mapping, then returns

```
J %*% Vg %*% t(J),  where J = db.drho
```

This is a candidate local propagation of smoothing-estimator variation into
the fitted coefficient mean. It neither samples coefficients nor constructs
a complete coefficient covariance. In these native NCV fits, `Vc` is exactly
this contribution alone, unlike ML/REML `Vc`. NCV can also replace `Vp` using
a jackknife calculation; the helper records that replacement and does not add
it to the propagated contribution. A repeated-sampling study is recorded in
[smoothing-ncv-repeated-summary.csv](smoothing-ncv-repeated-summary.csv).

The repeated-sampling study generated 200 independent Gaussian responses on
the same 240 covariate rows, with known relative precision weights and no
offset. All 200 fits reported full convergence; none were removed. Empirical
variances of the two fitted log smoothing parameters were 0.109 and 0.082.
Mean native `Vg` variances were 3.059 and 5.715, about 28 and 70 times larger.
Recomputing the exact Gaussian criterion curvature by centred differences
within the sandwich gave mean variances 2.941 and 5.236. This check does not
support attributing the discrepancy primarily to the BFGS curvature
approximation. The native sandwich construction agreed with independently
calculated contributions within 1.3e-11 over these repetitions.

The [follow-up covariance investigation](ncv-covariance-report.md) identifies
the dominant missing term: large negative covariance between the fitted
leave-one-out gradient contributions. An exact Gaussian calculation at fixed
smoothing parameters includes this dependence. Across 400 refits, its
fitted-mean approximation reduces the variance ratios to 1.2–1.4, while
remaining internal because calibration is incomplete.

This experiment does not validate native `Vg` as the covariance for a useful
normal uncertainty sampler in this setting. It is a reason to retain only the
internal local sensitivity audit and to investigate calibration before
offering NCV smoothing-mixture draws. Refining the Hessian inside a sandwich
is distinct from treating the inverse Hessian itself as posterior covariance.

The local calculation has a meaningful estimator-covariance interpretation
when the NCV covariance approximation and linearisation are adequate. The
algebra checks do not establish calibrated intervals, a reliable nonlinear
normal mixture, or a Bayesian distribution for smoothing parameters. It stays
internal and separate from the Gaussian mixture. In particular:

* The independent leave-one-out calculation has been checked. Arbitrary
  overlapping prediction sets, correlated observations, neighbourhood
  covariance adjustments and the interaction with jackknife coefficient
  uncertainty require further statistical validation. The helper exposes a
  sensitivity contribution, not an endorsement of these configurations.
* In mgcv 1.9-4, the tested shared-parameter and partly fixed NCV `gam()` fits
  fail with `subscript out of bounds` inside mgcv. The helper rejects fitted
  mappings it cannot identify rather than guessing a free-parameter order.
* With nonzero fitting offsets, the retained native leave-one-out predictors
  omit the offset in the tested Gaussian example, and the reported criterion
  uses those predictors. The independently calculated loss is 22.66 versus the
  reported 52.75; removing the offset from the correct predictions reproduces
  the retained predictors to 1.6e-15. The helper rejects nonzero offsets until
  this native convention is resolved. See
  [smoothing-ncv-offset.csv](smoothing-ncv-offset.csv).
* Discrete BAM NCV does not retain the gradient-contribution covariance `Vg`.
  Its working-model NCV curvature cannot supply the missing covariance. A
  valid extension would need the contributions, any neighbourhood adjustment,
  parameter mapping and compressed coefficient derivatives. No inverse-Hessian
  fallback is used.

The source audit covered `estimate.gam()`, `gam.outer()`, `gam.fit3()`,
`gam.fit3.post.proc()`, `bfgs()`, `bam.fit()`, `Sl.postproc()`, `bgam.fitd()` and
`sp.vcov()` in the installed package. Statistical context is provided by the
[mgcv smoothing-covariance documentation](https://stat.ethz.ch/R-manual/R-devel/RHOME/library/mgcv/html/sp.vcov.html),
[NCV documentation](https://www.stat.ethz.ch/R-manual/R-devel/library/mgcv/html/NCV.html)
and Wood's [NCV paper](https://arxiv.org/abs/2404.16490).

See [smoothing-methods-checks.md](smoothing-methods-checks.md) for the final
test and package-check results, chosen options and skipped validation.

Reproduce from this checkout:

```
Rscript notes/posterior-chains/smoothing-methods.R --load-all
Rscript notes/posterior-chains/smoothing-study.R --load-all 20000
Rscript notes/posterior-chains/smoothing-ncv-study.R 200
```
