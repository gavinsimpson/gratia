# Smoothing uncertainty: ML, REML and fREML

This is an internal research extension, investigated with R 4.6.1 and mgcv
1.9-4. It does not add a public sampling method. The internal prototype
propagates an approximate distribution of smoothing parameter estimates through
conditional coefficient distributions. Gaussian identity conditionals are exact;
other ordinary and extended families use native coefficient normal approximations.
It does not specify hyperpriors or
integrate a joint Bayesian posterior.

## Supported combinations

| Fitted model and actual method | Retained information | Internal calculation |
| --- | --- | --- |
| Gaussian identity `gam()`, ML or REML | `V.sp`, `sp.vcov()`, `db.drho`, `Vc` | Gaussian mixture at fixed fitted dispersion |
| Gaussian identity non-discrete `bam()`, ML or REML | Same quantities as ordinary `gam()` | Same mixture; stored derivatives use fitted coefficient coordinates |
| Gaussian identity non-discrete `bam()`, fREML | `V.sp` for log smoothing parameters; `sp.vcov()` can also include log dispersion | Same mixture; derivatives are reconstructed in fitted coefficient coordinates |
| Discrete `bam()`, fREML | Smoothing covariance and derivatives are available | Audit only; compressed fitting design has not been reconstructed for conditional updates |
| Other ordinary and extended-family `gam()`, ML or REML | Family parameters can precede log smoothing parameters; dispersion can follow | Native conditional normal updates at fixed fitted nuisance parameters |
| Other ordinary and extended-family non-discrete `bam()`, ML, REML or fREML | Family parameters updated separately, absent from smoothing covariance | Native normal updates; must reproduce the original coefficients and covariance |
| General families, including LSS | Depends on family and fitter | Audit only; no conditional mixture updates |

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
fits, positive fitted penalty multipliers, supported penalty
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
* `gam(method = "ML", optimizer = "efs")` fits by REML and does not retain the
  required Hessian covariance in the tested example. The prototype rejects it.
* `gam(method = "fREML")` fails during fitting. fREML is a BAM method.
* With every smoothing multiplier fixed, non-discrete `bam(method = "fREML")`
  changes to REML. The retained scalar covariance is for log dispersion; there
  is no free log smoothing parameter to draw.

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
Non-discrete BAM omits separately updated family parameters from these matrices.
For extended families, a separate dispersion is estimated only when `family$scale`
is negative; fixed global scale overrides the scale.estimated flag in bam.
Component scales in gfam belong to its family-parameter vector. The native
conditional extension holds all these nuisance parameters fixed and propagates
only the retained free log-smoothing covariance. In gam this is a marginal block
of the joint covariance; BAM omits the joint family-parameter block, so that
dependence cannot be recovered from it. See [the family inventory and
validation](smoothing-families.md), including the rejected censored-Poisson bam
examples and the dense-design requirement for non-Gaussian updates.

`db.drho` also needs a coordinate check. Ordinary `gam()` and ML/REML `bam()`
derivatives agree with conditional mean derivatives after applying the
smoothing mapping. Non-discrete fREML BAM retains raw derivatives in an
internal fitting basis. Setup therefore reconstructs derivatives from the
Gaussian precision for the exact Gaussian identity path. Other families use
native conditional fits and do not apply those Gaussian derivative formulae.
ML/REML `Vc` includes the native
coefficient covariance correction; fREML BAM uses transformed derivatives
internally when constructing its `Vc`. Neither matrix is evidence that a
coefficient sampler has integrated smoothing uncertainty.

## Scope and next validation step

Smoothing-parameter uncertainty is restricted to actual ML, REML and fREML
fits. NCV and QNCV are excluded because their uncertainty approximations
have not been validated sufficiently for this feature. Their research
implementations and results are archived separately from the package work.

The next step is repeated-simulation validation of the retained conditional
mixture. Conditional reconstruction, mappings and mixture moments have been
checked; calibration of the resulting intervals has not. Compare conditional
coefficient draws, mgcv's `unconditional = TRUE` correction and the internal
mixture across ML/REML gam and ML/REML/fREML non-discrete bam fits.
Include ordinary and extended families as well as the original Gaussian cases.
Include small samples, weakly identified smooths, shared and fixed penalties,
tensor products, and known versus fitted dispersion. Assess bias and interval
coverage for smooth values, derivatives and predictions, reporting Monte Carlo
uncertainty in coverage estimates.

Keep fitted dispersion and family parameters fixed in the initial comparison
and label the result
as approximate estimation-uncertainty propagation. The comparison should
determine whether the approximation is adequate before adding a public
sampling option. Public integration would then need consistent chains, seeds,
supplied-draw handling and downstream posterior calculations.

See [smoothing-methods-checks.md](smoothing-methods-checks.md) for validation
results and deliberately omitted checks.

Reproduce from this checkout:

```sh
Rscript notes/posterior-chains/smoothing-methods.R --load-all
Rscript notes/posterior-chains/smoothing-study.R --load-all 20000
Rscript notes/posterior-chains/smoothing-families.R --load-all
```
