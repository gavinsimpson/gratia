# Why NCV's Vg overestimates variance in the Gaussian experiment

The main cause is the covariance construction, not a missing sample-size or
dispersion multiplier. In mgcv 1.9-4, the final `gam()` object contains a
sandwich proxy built from crossproducts of individual NCV gradient
contributions. Those contributions are dependent because all the leave-one-out
predictions use coefficients estimated from overlapping training observations.
In this experiment their covariance has a large negative contribution, which
the row crossproduct omits.

An internal Gaussian calculation that includes this dependence reduces the
estimated-to-empirical variance ratios from 28–70 to about 1.2–1.4 using the
fitted mean and dispersion. This identifies the dominant discrepancy; it does
not establish calibrated intervals or a posterior distribution. No public
sampler or existing native `Vg` calculation has been replaced.

## Interpretation and scaling rechecked

The installed source was inspected again: `gam.fit3()` forms
`crossprod(ncv1)`, and `bfgs()` multiplies it on both sides by its inverse
criterion-curvature approximation, applying the smoothing mapping. Thus the
final `gam()$Vg` is already in the coordinates of free **natural-log smoothing
parameters**, rather than being a raw gradient crossproduct or a covariance of
smoothing parameters on their original scale. In the experiments there are two
free smoothing parameters and no additional family or log-dispersion entry in
`Vg`. Native `Vc` remains the propagated contribution `J Vg J'` alone, with
`J = db.drho`, as in the earlier audit.

Let `g_i` be the column vector obtained by differentiating observation i's
leave-one-out weighted squared error with respect to log smoothing parameters.
Let `H` be the Hessian of their **sum**. The inspected implementation is

```
G = matrix with row i equal to g_i'
Vg = H_BFGS^{-1} crossprod(G) H_BFGS^{-1}
```

For an estimator that minimises the summed criterion, a local covariance uses
`Cov(sum_i g_i)` between the inverse Hessians. This is generally different from
`E[crossprod(G)]`:

```
E[crossprod(G)] = sum_i Cov(g_i) + sum_i E(g_i) E(g_i)'
Cov(sum_i g_i) = sum_i Cov(g_i) + sum_{i != l} Cov(g_i, g_l)
```

The row-crossproduct formula also has an algebraic interpretation as local
variation under independent multipliers applied to the held-out loss terms.
If those multipliers change by delta_xi, keeping the training data fixed,
the first-order change is `delta_rho = -H^{-1} G' delta_xi`. Unit multiplier
variances give `H^{-1} crossprod(G) H^{-1}`. That perturbation differs from
regenerating the training responses and refitting the predictions, as in the
empirical experiment. It is not a posterior distribution for rho.

Independent observations do not make these fitted loss derivatives
independent. The expectation of an individual derivative can also be nonzero
at a population optimum: only their sum has expectation zero.

The investigation checked the following potential scaling explanations:

* Natural logs are used in both the fitted object and empirical variance.
  Converting the log10 covariance back by multiplying by `log(10)^2` agrees
  within 6e-17.
* Dividing the summed criterion by n divides its Hessian by n and its gradient
  covariance by n squared. The resulting sandwich is unchanged within 2e-16.
* Multiplying responses and means by three, and dispersion by nine, leaves
  log-smoothing covariance unchanged within 5e-12. Scaling a fixed-dispersion
  Gaussian squared loss into a negative log likelihood likewise rescales both
  Hessian and gradient; it supplies no additional dispersion correction to a
  consistently constructed sandwich.
* Replacing BFGS curvature with independently differentiated exact Gaussian
  curvature reduces the original ratios only to 27 and 64. Curvature error is
  not the dominant source.
* An unweighted comparison also has excessive native ratios, 23 and 50.
  Relative-precision weights do not create the problem.

The empirical comparisons condition on the covariates and basis design and
regenerate independent Gaussian responses. They assess repeated-sampling
uncertainty of the smoothing estimator under that design; they are not a
comparison to a Bayesian hyperparameter posterior.

## Relation to the NCV paper

Section 6 of [Wood's arXiv v4 paper](https://arxiv.org/html/2404.16490v4#S6)
derives uncertainty for the coefficient estimator beta. Equations (5) and
(6) expand it with respect to the responses. Equation (7) constructs its
covariance using response neighbourhoods; the subsequent correction replaces
fitted residuals with cross-validated residuals. Appendix D explains the
finite-sample bias under penalization. These are coefficient-variance results,
not a derivation of the smoothing-estimator covariance stored as `Vg`.

Our independent Gaussian response experiment addresses a different dependence:
NCV gradient contributions share training observations even when the responses
are independent. The paper's response-expansion approach is useful for keeping
the source of randomness explicit, but its coefficient correction cannot
simply be applied as a multiplier to `Vg`. The Gaussian moment calculation
below includes this dependence directly. No claim is made that the paper
establishes the calibration of that new approximation.

## Where the jackknife enters

NCV already computes approximate changes in coefficient estimates when
observations are omitted. With smoothing parameters held at their fitted
values, these changes can also be used to estimate coefficient covariance.
This is distinct from the `Vg` calculation, whose coordinates are log
smoothing parameters. In the Gaussian leave-one-out comparison, enabling
`nei = list(jackknife = TRUE)` leaves coefficients, smoothing parameters,
`Vg` and `Vc` unchanged, while replacing `Vp`.

For the independent Gaussian case in mgcv 1.9-4, let row i of Delta contain
the coefficient change when observation i is omitted, holding the fitted
smoothing parameters fixed. The inspected code constructs

```
Vj = crossprod(Delta) * ((n - 1) / n)^0.8
alpha = trace(Vj) / trace(Ve)
Vp_new = Vj + alpha * (Vp_original - Ve)
```

The final term is a scaled smoothing-bias correction. The original `Vp` is
preserved as `attr(Ve, "Vp")`, and alpha as `attr(Ve, "inv.learn")`.
A regression test reconstructs this result using separate row-deletion
Gaussian coefficient fits. The neighbourhood calculation for dependent
responses additionally uses neighbourhood covariance and cross-validated
residual corrections; the independent-data formula above is not its formula.

The 400-refit investigation explicitly sets `jackknife = FALSE` to isolate
smoothing-estimator uncertainty. Its revised gradient covariance is unchanged
when the coefficient jackknife is enabled. Neither calculation reoptimises
smoothing parameters for every omitted observation. A jackknife of the entire
smoothing estimator, requiring such reoptimisation, would be a separate
experiment. Adding a log-smoothing contribution to the replaced `Vp` would
require checking dependence between the coefficient and smoothing estimators;
the internal prototype therefore continues to return that contribution
separately.

## Exact Gaussian calculation at fixed smoothing parameters

For positive known relative-precision weights W, write

```
z = sqrt(W) y
Z = sqrt(W) X
P = Z'Z + sum_j exp(rho_j) S_j
A = Z P^{-1} Z'
D = diag(1 - diag(A))
B = D^{-1} (I - A)
C(rho, z) = ||B z||^2 = z' K z, where K = B'B
```

Offsets are excluded in this prototype because the earlier audit found a
native NCV offset discrepancy. Let `K_j` denote the derivative of K with
respect to rho_j. Then the total gradient component is `z' K_j z`. When
`z ~ N(m, phi I)`, expanding the Gaussian second and fourth moments gives

```
Cov(g_j, g_k) = 2 phi^2 tr(K_j K_k) + 4 phi m' K_j K_k m
```

The first term accounts for quadratic noise variation, and the second for the
linear variation about a nonzero mean. These moments are exact at a fixed
rho, known m and phi. Multiplication by inverse criterion Hessians is a
**local approximation** for the covariance of the nonlinear minimiser.

The internal implementation differentiates the Gaussian smoothing matrix and
its leave-one-out leverage denominators. Independent row-deletion fits verify
the predictions; finite differences verify individual gradient contributions
and the matrices `K_j`. Simulations of 20,000 new responses at fixed smoothing
parameters verify both the total-gradient covariance and the different mean
row crossproduct. Across the weighted and unweighted experiments, the largest
variance-standardised covariance error is 0.012, and the corresponding error
for the row-crossproduct expectation is 0.004.

## Where the missing variance terms go

All terms below use the same population-optimal smoothing parameters and
expected Hessian in the original weighted design. The values are in
log-smoothing covariance units, after multiplying by the inverse Hessian on
both sides.

| Contribution | Parameter 1 | Parameter 2 |
| --- | ---: | ---: |
| Expected native row crossproduct | 2.90664 | 4.91264 |
| Subtract squared means of individual contributions | -0.03686 | -0.09005 |
| Add covariance between distinct observations' contributions | -2.72023 | -4.72998 |
| Covariance of the total gradient after propagation | 0.14955 | 0.09261 |
| Independent response-simulation check of the last row | 0.14982 | 0.09199 |

The negative covariance between contributions supplies most of the
cancellation. Centring individual contributions alone does not solve the
problem. A universal rescaling of `Vg` cannot reconstruct this cancellation;
it depends on the fitted design, smoothing parameters and response model.

The fixed-parameter calculation and repeated-refit experiment are distinct.
In the latter, fitted smoothing parameters and observed curvature vary across
responses, which is why its mean proxy values are not the first table row.
See [ncv-covariance-decomposition.csv](ncv-covariance-decomposition.csv) and
[ncv-covariance-simulation.csv](ncv-covariance-simulation.csv).

## Repeated-refit results

Each design uses 200 independent responses. All 400 fits report full
convergence, and none are removed or clipped. The weighted experiment exactly
reproduces the previous 200 fitted log-smoothing pairs. The unweighted
comparison uses the same covariates and noise draws with weights set to one.
The following numbers are mean estimated variance divided by empirical
variance of fitted log smoothing parameters.

| Construction | Weighted parameter 1 | Weighted parameter 2 | Unweighted parameter 1 | Unweighted parameter 2 |
| --- | ---: | ---: | ---: | ---: |
| Native Vg | 27.96 | 69.58 | 22.82 | 49.89 |
| Native row crossproduct, exact Hessian | 26.88 | 63.75 | 20.96 | 45.49 |
| Gaussian total-gradient covariance, true mean and dispersion | 1.30 | 1.22 | 1.20 | 1.22 |
| Gaussian total-gradient covariance, fitted mean and dispersion | 1.23 | 1.38 | 1.21 | 1.37 |
| Gaussian total-gradient covariance, unpenalised basis mean and fitted dispersion | 1.59 | 1.56 | 1.65 | 1.61 |

The remaining differences matter for use as an uncertainty approximation.
Even the calculation supplied with the true mean and dispersion still uses a
local approximation to a nonlinear estimator. Estimating those moments also
changes the result; a noisier unpenalised mean estimate increases the
estimated variance here. The fitted-mean version is much closer than native
`Vg`, but the remaining 20–38% excess and the limited designs do not justify a
public sampling method or a claim of interval calibration.

See [ncv-covariance-summary.csv](ncv-covariance-summary.csv) for numerical
results and [ncv-covariance-repetitions.csv](ncv-covariance-repetitions.csv) for
every fitted result. No arbitrary factor was fitted to make estimated
variances agree with empirical variances.

## Internal prototype and restrictions

`sampling_ncv_gaussian_setup()`, `sampling_ncv_gaussian_eval()`,
`sampling_ncv_gaussian_moments()` and `sampling_ncv_gaussian_covariance()` are
unexported research helpers. The covariance helper returns the log-smoothing
estimator approximation, its gradient moments and Hessian, and the local
coefficient-mean contribution at fitted smoothing parameters. It does not
return coefficient draws or add that contribution to `Vp`. Such addition
would need a separate treatment of the dependence with coefficient
estimation and any native jackknife correction.

The prototype supports independent Gaussian identity `gam()` fits with ordinary
leave-one-out NCV or QNCV, positive relative-precision weights, no offsets,
`gamma = 1` and identifiable unshared, non-fixed smoothing mappings. It checks
that its exact criterion and gradient match the fitted native quantities.
Explicit neighbourhood expressions other than a literal jackknife-only list
are rejected rather than evaluated from an old fitting call. Non-Gaussian
families, discrete BAM, shared/fixed mappings and arbitrary neighbourhoods
remain outside this prototype.

The implementation stores dense observation matrices and defaults to at most
1,000 rows; increasing that limit is an explicit research choice. It requires
positive definite local criterion curvature and does not repair invalid
curvature by clipping eigenvalues. The mean and dispersion used in the moment
calculation are explicit, and smoothing uncertainty is not Bayesian
integration.

The source audit concerns installed mgcv 1.9-4. The general NCV and coefficient
uncertainty context is described in the [mgcv NCV documentation](https://www.stat.ethz.ch/R-manual/R-devel/library/mgcv/html/NCV.html)
and [Wood's NCV paper](https://arxiv.org/abs/2404.16490). The Gaussian gradient
moment derivation and comparisons above are this investigation's calculations;
they do not attribute a validated smoothing-posterior interpretation to Vg.

Reproduce outside the sandbox from this checkout:

```sh
Rscript notes/posterior-chains/ncv-covariance-study.R --load-all 200
```

Validation commands, results and deliberately omitted checks are recorded in
[ncv-covariance-checks.md](ncv-covariance-checks.md).
