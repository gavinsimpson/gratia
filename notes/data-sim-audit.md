# Audit of `data_sim()` and expanded mgcv generators

Date: 15 October 2026. Implementation and checks use R 4.6.1 and mgcv 1.9-4.

## What the existing distributions generate

Write `g` for the raw example function and `s` for `scale`.

| `dist` | Conditional distribution | Interpretation |
|---|---|---|
| `normal` | Normal mean `g`, variance `s^2` | `scale` is a standard deviation. |
| `poisson` | Poisson mean `exp(s * g)` | `scale` changes the predictor, not dispersion. |
| `binary` | Bernoulli probability `plogis(s * (g - 5))` | The centring constant is suited to the original four-term example. |
| `negbin` | Negative binomial mean `exp(s * g)`, size `theta` | Variance is `mu + mu^2 / theta`. |
| `gamma` | Gamma mean `exp(g / 3 + 0.1)`, shape `1 / s` | Variance is `s * mu^2`; `scale` is dispersion. |
| `tweedie` | Tweedie mean `exp(g / 3 + 0.1)`, power `power`, dispersion `s` | Variance is `s * mu^power`; this generator requires power strictly between 1 and 2. |
| `ocat`, `ordered categorical` | Centre `g`, add standard logistic noise, and apply cut points | The resulting category probabilities are differences of logistic CDF values. |

The scalar distributions are correctly parameterised. Conditional moment checks
on constant predictors agree with these formulas. The legacy ordinal path first
generates a Gaussian response and then replaces it with categories; that normal
draw does not affect the response law, but consumes random numbers. Consequently
`scale` does not control ordinal noise. That draw is retained in the compatibility
path so unaffected ordinal examples retain their seeded output.

The original `f0`, `f1`, etc. columns are raw component values. Except for normal
responses, summing them generally does not give the returned `f`, because `f`
includes distribution-specific transformations or centring. These component
columns must not be treated as already transformed fitted terms.

## Confirmed implementation problems and their corrections

### Repeated transformation in `eg6`

The previous code called a response generator once while creating the four-term
example, added group effects to its returned `f`, and called the generator again.
For Poisson, negative binomial, binary, Gamma and Tweedie responses, the first
call had already transformed the signal. The second call transformed it again.
The first call also used default `theta` and `power`, and the final `f` was lost.

The template now creates covariates and the raw signal without drawing responses,
adds the group effects, and samples once. The returned `f` is the predictor used
for the response. The seven `eg6` stored reference datasets are updated. Comparing
all 49 model/distribution combinations against the baseline implementation
identified changes only in `eg6`; unaffected stored references remain in place.
All 42 unaffected combinations also have exactly identical column values and
types when compared directly with the original implementation. The final review
extended this comparison to `gwf2`, `lwf6`, recycled scale and size vectors, and
infinite negative binomial size: all 104 unaffected scalar/vector cases are
exactly identical to the baseline.

The four group effects remain `3, 6, 9, 12`. They are fixed realised effects in
this reference example, not newly sampled Gaussian random effects.

### Grouped families

The previous nested simulation selected its seed using a uniform random number
between zero and one. Conversion to an integer seed therefore selected zero,
which made covariates repeat across outer seeds. Response generation used
`case_when()`, which also drew from distributions absent from the requested rows.

The new code generates the template once in the outer RNG stream and samples
only the rows belonging to each component. Membership is an integer family-list
position. Thus repeated family names remain distinct, and the ordinal alias is
normalised before labels are assigned. Unknown families and insufficient sample
sizes receive direct errors. The legacy grouped path ignores `dist` throughout,
so selecting `dist = "ocat"` cannot replace its mixed response with categories.

Historical defaults remain Gaussian SD 0.5, Gamma dispersion 0.5, Tweedie
dispersion 1.5, negative binomial size 3, Tweedie power 1.5, and ordinal cuts
`c(1, 2, 2.5)`. Explicit `scale`, `theta`, `power`, `cuts` and `n_cat` are honoured.
For a supplied grouped `scale`, the value is Gaussian SD or Gamma/Tweedie
dispersion. Positive-mean responses retain the grouped example's common log
predictor. This is deliberately separate from the legacy standalone generators'
mean calibration. The grouped snapshot changes because the former seed handling
was incorrect.

mgcv fixes the first ordinal cut at -1. If generating cuts begin at `a`,
equivalent fitting shifts the ordinal predictor by `-(a + 1)`. In particular,
the historical grouped cuts beginning at 1 need an ordinal-specific intercept
shift of -2. A grouped fitting formula that forces all family intercepts to
be equal will not represent that generating law exactly.

### Validation, RNG state and documentation

`eg5` and `eg6` previously required sample sizes divisible by four. They now repeat
the four group positions to the requested length. Model names, sample sizes,
relevant distribution parameters and ordinal cut points are validated directly.
Negative predictor multipliers remain valid for the legacy Poisson, negative
binomial and Bernoulli generators.

An explicit seed now restores an absent RNG state as well as an existing state,
including when generation errors. `seed = NULL` continues to advance the caller's
RNG. The reference helper forwards `power` instead of ignoring it. Documentation
also corrects the variable order in the varying-coefficient example: its signal
is `f2(x2) * x1`.

## The additive family-object interface

The appended arguments are `family`, `predictors`, `parameters` and `observation`.
An explicit family cannot be combined with the legacy distribution controls.
This avoids assigning another meaning to `scale`.

1. A template creates covariates and raw true functions without sampling a response.
2. `predictors`, if supplied, receives that template and returns link-scale
   predictors in mgcv's order. Otherwise a documented family-specific calibration
   is used.
3. A family registry validates named generating parameters and an adapter samples
   the complete response.
4. An optional observation rule applies censoring.

The response remains a tibble column, including matrix columns where required.
The `truth` attribute records all linear predictors, natural distribution
parameters, original template values and complete responses. Binomial responses
are proportions with a `trials` column; fitting uses `weights = trials`.
Multinomial categories are numbered from zero. Cox responses include observed
time and `status`; complete event and censoring times are retained as truth.

Auxiliary parameters fixed in a family constructor are read without modifying
its private parameter environment. Estimation starting values are not silently
accepted as generating truth. For example, `nb()` needs `parameters =
list(theta = 3)`, whereas `nb(theta = 3)` already fixes the size. Parameters are
expressed on natural scales; the detailed names, defaults and predictor order
are in `?data_sim_families`.

The adapters account for conventions that prevent a universal call to
`family$rd(linkinv(eta))`: `gaulss()` uses inverse SD internally, `gammals()`
requires mean and log dispersion, `shash()` uses its tail adjustment in the scale,
and `multinom()` expects logits. Multinomial probabilities use a stable softmax.
`gumbls()` models location, whereas its native RNG accepts the mean. The adapter
uses the location quantile directly, records both location and mean, and tests
the Gumbel CDF independently. The mean is location plus Euler's constant times
scale. GEV generation uses the exact zero-shape limit and `expm1()` for small
shapes. Its default shape is zero under either supported shape link; the identity
link permits finite shapes outside the default logit link's (-1, 0.5) bounds.

`ziP()` and `ziplss()` generate mgcv's hurdle law: a presence indicator followed
by a positive Poisson response conditioned on being positive. They do not simply
add structural zeros to unconditioned Poisson draws. Inverting upper-tail
probabilities also permits very small positive Poisson intensities without
rounding all positive responses to zero.

The new Tweedie paths use gratia's `rtw()`, which aligns event-level Gamma shapes
when power varies by observation. In the inspected mgcv 1.9-4 `rTweedie()` source,
scales are expanded by Poisson event counts but vector Gamma shapes are not.
Legacy scalar-parameter simulations retain their existing sampler and RNG
sequence. Legacy vector controls retain R's recycling rules; Tweedie vectors use
the corrected sampler for both power and dispersion. The family interface
requires scalar or fully aligned observation-level values instead. Gaussian
zero variance remains available, including in grouped simulations. Unused
grouped theta and power controls are not validated as though they were active.
A `negbin()` size search range requires a separately supplied generating size.

The inspected `bcg(theta = ...)` constructor also packs its fixed log-scale
parameters incorrectly in mgcv 1.9-4. For fixed Box-Cox parameters, the adapter
reads the intended scale from the last packed log value; it does not change the
family object. The supported generating case is still restricted to power zero.

## Family coverage and observation mechanisms

Coverage includes standard Gaussian, Poisson, binomial, Gamma and inverse
Gaussian families; `nb`/`negbin`, `tw`/`Tweedie`, `betar`, `scat`, `ocat`, `ziP`;
`gaulss`, `gammals`, `gumbls`, `gevlss`, `shash`, `twlss`, `ziplss`, `multinom`
and `mvn`; `cnorm`, `clog`, `cpois`; the lognormal case of `bcg`; and `cox.ph`.

Grouped family objects reuse these scalar response generators and allow separate
parameters for repeated family types. General families are excluded because
their response/predictor structures do not fit mgcv's grouped scalar model.
Censored-family components can generate complete scalar responses; observed
censoring matrices are not supported inside grouped responses.

For censored families, complete responses are sampled before left, right or
interval censoring. Matrix encodings follow each family's conventions, including
zero as the second entry for left-censored `bcg` responses and half-integer
interval endpoints for Poisson counts. Interval rounding is applied only where
the whole interval lies inside the detection limits.

For `cox.ph`, `H0(t) = (t / baseline_scale)^baseline_shape` and
`S(t | eta) = exp(-H0(t) * exp(eta))`. The baseline parameters default to one.
Independent exponential censoring defaults to rate 0.2; rate zero disables it.
This supplies event-time data for the Cox partial-likelihood model, rather than
attempting to sample event times from a partial likelihood without a baseline.

Two boundaries remain:

- Quasi families specify moments rather than a complete response distribution.
  The caller must select an explicit generating law.
- Nonzero Box-Cox powers are rejected. Their inverse transformation is defined
  only on part of the real line, so unrestricted Gaussian transformed draws
  cannot always be inverted. Silently rejecting draws would instead simulate a
  truncated law. This implementation does not make that substitution.

A separate mgcv 1.9-4 fitting issue occurs when `gfam()` has no free auxiliary
parameters, for example a binomial component and a negative binomial component
with fixed size. A clean mgcv-only example fails with `exp(theta)` receiving a
non-numeric value. The generator does not alter that family object. Fitting a
fresh grouped family with `nb()` estimating its size works with the generated
data; fixed generating parameters do not require fixing them during fitting.

Time-varying-covariate survival pseudodata for `cox.pht` and stratified Cox
baselines are outside this event-time generator.

## Shape-constrained reference examples

Nine univariate templates cover increasing, decreasing, convex, concave,
positive, and the four combinations of monotonicity with curvature. Each returns
an analytic function and its first two derivatives on [0, 1]. Tests compare the
analytic derivatives with finite differences and check their advertised signs.

`sc_additive` adapts the Poisson example from mgcv's `scasm` help.
`sc_tensor` supplies a surface increasing in both covariates.
`sc_location_scale` supplies a Gamma model with a concave log mean and increasing
log dispersion, including derivatives of those true functions.

`?data_sim_shapes` provides data and fitting recipes, including `bs = "sc"`
and `xt` specifications. The positivity recipe removes the intercept so the
constrained smooth represents the whole function. Routine recipes use no
bootstrap replicates; a separate test verifies native constrained bootstrap
coefficient draws. Fitted reference models are not stored.

## What a redesign without compatibility constraints would change

The following inconsistencies remain intentionally available through legacy calls.
They are not recommended as conventions for future APIs.

| Existing inconsistency | Replacement in an unconstrained redesign |
|---|---|
| `scale` means SD, predictor multiplier or dispersion | Separate `signal_multiplier`, `signal_intercept` and distribution parameters such as `sigma` or `dispersion`. |
| Gaussian SD differs from mgcv's Gaussian dispersion | Use variance consistently for dispersion; offer `sigma` only as an explicit named alternative. |
| Distribution names mix aliases and abbreviations | Use instantiated family objects as the primary selector, with clearly documented conveniences. |
| `theta` means only NB size here but means many parameters in mgcv | Use distribution-specific names: `size`, `precision`, `df`, `sigma`, `power`. |
| Bernoulli subtracts 5; Gamma/Tweedie divide by 3 and add 0.1 | Put every signal transformation in the template or an explicit predictor specification. |
| Raw component columns coexist with transformed `f` | Distinguish `signal`, `eta`, natural parameters and term components with a uniform return structure. |
| Ordinal simulation discards a Gaussian response | Generate one logistic latent response directly for every ordinal call. |
| Grouped distributions use a distinct mean calibration | Compose shared templates with ordinary family specifications and explicit group membership. |
| A model named `gfam` mixes a design choice with a response model | Select the design with `model` and grouping with `family = gfam(...)`. |
| `eg6` calls fixed group effects random effects | Name these realised group effects, and offer separately specified random-effect draws in a future design. |

The appended family interface implements the separation while leaving existing
calls available. It does not introduce a generic `...` argument that could hide
misspelled distribution parameters. Exact control of a simulation should use
explicit predictors and generating parameters rather than depend on calibration
defaults.

## Evidence and validation

The added tests exercise all supported families at one and multiple observations;
analytic scalar moments; ordinal, hurdle and multinomial probabilities;
multivariate covariance; varying Tweedie power; censoring encodings; Cox survival
transforms; repeated grouped components; constructor parameter conflicts; family
state preservation; invalid inputs; and RNG restoration after success and error.

Shape tests exercise every univariate recipe, additive and tensor models, Gamma
location-scale models, fitted linear constraints, gratia smooth estimates and
derivatives, and a small native bootstrap.

## Validation results

- All four simulation test files pass, including the legacy references, cut
  points, new family laws and shape-constrained fitting recipes.
- Independent fitting checks accept the generated binomial proportions,
  censored Gaussian/Poisson/lognormal matrices, multinomial category codes,
  multivariate normal matrices and Cox event times. Generated grouped binomial
  and negative binomial data fit when the fitting family estimates NB size.
- The pkgdown reference index includes both new public topics and validates.
- Final `R CMD check`: **0 errors, 0 warnings, 0 notes**. The build used
  `--no-build-vignettes`; the check used `--no-tests --no-manual
  --ignore-vignettes`. These options skip vignette building/checking and the
  typeset PDF manual. Tests are run separately using the installed-package
  test runner. The `SKIPPED` lines for tests and vignettes are expected results
  of those options, not package problems. Help examples and their saved output
  comparisons were checked successfully.
- The final complete ordinary-CI suite passes with no failures or warnings,
  using `NOT_CRAN=true VDIFFR_RUN_TESTS=false CI=true Rscript tools/run-tests.R`.
  All package test files were visited. The runner reports 199 intentional
  skips: visual snapshots, the long HGAM paper examples and existing CI
  exclusions. The slow
  HGAM examples passed in the earlier complete local run; they were not
  repeated in this final CI run. All tests and package checks ran outside the
  sandbox.
- The final run includes the diagnostic-caption expectation updates now on
  `main`, as well as the corrected acceptance of seed zero. The earlier
  caption and QQ/worm failures are resolved.
- Review corrected the Gumbel location/mean conversion, GEV identity-link
  defaults and shape bounds, legacy vector controls, zero Gaussian noise,
  Tweedie vector sampling, and the distinction between a negative binomial
  size search range and a fixed generating size. Infinite size retains R's
  Poisson limit. A new test initially assumed identical Poisson and negative
  binomial RNG sequences at this limit; the final test instead checks the
  analytic Poisson moments and support, and passes.

## Sources

- [mgcv family catalogue](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/family.mgcv.html)
- [Shape-constrained fitting](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/scasm.html)
- [B-spline shape constraints](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.construct.bs.smooth.spec.html)
- [Gumbel location and mean](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gumbls.html)
- [GEV parameters and links](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gevlss.html)
- [Hurdle Poisson parameterisation](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/ziplss.html)
- [Censored Box-Cox family](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/bcg.html)
- Installed mgcv 1.9-4 source, including constructor closures and random generators.
