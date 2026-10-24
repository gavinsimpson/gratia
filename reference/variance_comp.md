# Variance components and fitted term variances for GAMs

Extract variance components for smooth penalties and random effects, or
calculate the variances and covariances of fitted term contributions.

## Usage

``` r
# S3 method for class 'gamm4'
variance_comp(
  object,
  rescale = TRUE,
  coverage = 0.95,
  interval_method = c("profile", "none"),
  type = c("penalty", "terms"),
  n_draws = 1000L,
  seed = NULL,
  unconditional = FALSE,
  include_total = TRUE,
  ...
)

# S3 method for class 'scam'
variance_comp(
  object,
  rescale = TRUE,
  coverage = 0.95,
  type = c("penalty", "terms"),
  n_draws = 1000L,
  seed = NULL,
  unconditional = FALSE,
  include_total = TRUE,
  ...
)

variance_comp(object, ...)

# S3 method for class 'gam'
variance_comp(
  object,
  rescale = TRUE,
  coverage = 0.95,
  type = c("penalty", "terms"),
  n_draws = 1000L,
  seed = NULL,
  unconditional = FALSE,
  include_total = TRUE,
  ...
)

# S3 method for class 'gamm'
variance_comp(
  object,
  rescale = TRUE,
  coverage = 0.95,
  type = c("penalty", "terms"),
  n_draws = 1000L,
  seed = NULL,
  unconditional = FALSE,
  include_total = TRUE,
  ...
)
```

## Arguments

- object:

  a model fitted by
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html), or
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html). See
  [gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
  for mixed-model variance components.

- rescale:

  logical; for numerical stability reasons the penalty matrices of
  smooths are rescaled before fitting. If `rescale = TRUE`, this
  rescaling is undone, resulting in variance components that are on
  their original scale. This is needed if comparing with other mixed
  model software, such as
  [`lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html).

- coverage:

  numeric; a value between 0 and 1 indicating the (approximate) coverage
  of the confidence interval that is returned.

- interval_method:

  character; for `gamm4()` fits, `"profile"` computes likelihood profile
  intervals and `"none"` returns estimates only.

- type:

  character; `"penalty"` (the default) returns variance parameters
  associated with penalties and random effects. `"terms"` returns sample
  variances of fitted term contributions and their covariance matrix.

- n_draws:

  integer; number of joint posterior coefficient draws for
  `type = "terms"`. Defaults to 1000. Use zero for point estimates only;
  otherwise at least two draws are required. Ignored for
  `type = "penalty"`.

- seed:

  optional random seed for term intervals. Supplying a seed preserves
  the caller's random-number state.

- unconditional:

  logical; include the available correction for smoothing-parameter
  uncertainty in term intervals. Defaults to `FALSE`. If the corrected
  covariance is unavailable, a warning is issued and the uncorrected
  covariance is used. SCAM fits require `FALSE`.

- include_total:

  logical; include total rows for `type = "terms"`? Defaults to `TRUE`.
  Ignored for `type = "penalty"`.

- ...:

  arguments passed to other methods. For `gamm4()` fits, additional
  arguments are passed to
  [`stats::profile()`](https://rdrr.io/r/stats/profile.html), for
  example `maxpts`. With `type = "terms"`, no additional arguments are
  accepted.

## Value

A tibble of class `variance_comp`, with `.component`, `.variance`,
`.std_dev`, `.lower_ci` and `.upper_ci`. Both confidence limits are on
the standard deviation scale. For `type = "terms"`, obtain the named
covariance matrix with `attr(result, "covariance")` and posterior
variance draws with `attr(result, "variance_draws")` when intervals were
computed. Term results also include proportion estimates and their
interval columns; `attr(result, "proportion_draws")` contains the paired
posterior proportions.

## Details

For SCAM fits with `type = "penalty"`, `.variance` is the fitted
response scale divided by the smoothing parameter, optionally undoing
penalty rescaling. This ratio describes the Gaussian penalty on
unrestricted fitting parameters; it is not a Gaussian variance for the
transformed smooth coefficients. SCAM's GCV and UBRE fits do not supply
the likelihood-based smoothing-parameter covariance needed for
confidence limits, so both limit columns are `NA`.

With `type = "penalty"`, models fitted by
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) have one row per
penalty, including penalties supplied through `paraPen`. Fixed smoothing
parameters are included, and terms sharing a smoothing parameter each
have their own rows. A smooth with several penalties has several rows.
Unpenalized parametric terms have no variance parameter and are omitted.

The variance component is the fitted scale parameter divided by the
smoothing parameter. With `rescale = TRUE`, the smoothing parameter is
first divided by the penalty's scaling factor. These components describe
the penalty on coefficients, rather than the variation in fitted term
values over the observations.

Confidence intervals use the Hessian of the likelihood-based smoothness
selection criterion, transformed to the log standard deviation scale.
Shared smoothing parameters retain their shared uncertainty. A fixed
smoothing parameter can still have an interval when the model scale is
estimated. The scale is included as a separate row when its uncertainty
is available. Unavailable intervals are `NA`.

For [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) fits,
components are extracted from `lme`. Smooth variances are expressed on
the same penalty scale as for `gam()` and `bam()`, with one row per
penalty. Additional random effects have one row per coefficient
variance, labelled by grouping factor and coefficient; their covariances
are omitted. Intervals use
[`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html) when
the fitted covariance parameters support this calculation. Bounds are
`NA` when that information is unavailable, including fits containing
`pdTens` tensor-product blocks. The estimated residual scale is labelled
`scale`; a fixed scale is omitted. With residual variance weights,
`scale` is the baseline variance before those weights are applied. For
models fitted by penalized quasi-likelihood (PQL), these are approximate
components of the working mixed model.

For [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html) fits,
components are extracted from `mer`. Smooth components use the same
penalty scale and labels as the other methods; additional random effects
retain their mixed-model variances. By default, intervals are computed
separately for each standard deviation using
[`stats::profile()`](https://rdrr.io/r/stats/profile.html) and
[`stats::confint()`](https://rdrr.io/r/stats/confint.html). The
profiling uses the fitted random-effects design matrices. Gaussian
models are refitted by maximum likelihood for these profiles, including
models fitted by REML; the reported point estimates remain those of the
original fit. Profiling can take substantially longer than extracting
estimates. Use `interval_method = "none"` for estimates without
intervals. Failed profiles warn and return `NA` bounds for the affected
component. Profiles are unavailable for generalized mixed models with an
estimated scale.

With `type = "terms"`, each component is a fitted term's contribution to
the linear predictor, evaluated over the observations used to fit the
model. Parametric formula terms are included, with all coefficients of a
factor, polynomial or interaction combined into one term. Each smooth
has one row, including smooths with several penalties. Additional random
effects in `gamm()` and `gamm4()` contribute one component per grouping
factor and coefficient, evaluated at their fitted values. Formula terms
follow the model's contrast coding. Duplicated component labels receive
numeric suffixes.

Term variances and covariances use equal observation weights and the
sample denominator, the number of fitting observations minus one.
Likelihood weights do not change this averaging. Intercepts, offsets and
residual variation are excluded. `rescale` and `interval_method` apply
only to `type = "penalty"`; no profiling is performed for term
variances.

A final row labelled `total` gives the variance of the sum of the term
contributions, including additional fitted random effects. It includes
covariance between terms: the total is the sum of all entries of their
covariance matrix, rather than just its diagonal. Models with several
linear predictors have one total per predictor, labelled `total`,
`total.1`, and so on in predictor order. A term shared by several
predictors contributes to each of their totals. An intercept-only
predictor has total variance zero. These labels are reserved; a formula
term with the same name receives a numeric suffix so it remains distinct
from the total. Use `include_total = FALSE` to omit the total rows and
their columns in the posterior draw matrices. Totals are still
calculated for proportions; the individual term estimates and intervals
are unchanged.

Proportions allocate the total variance using \$\$p_j =
\frac{\operatorname{Cov}(f_j, \sum_k f_k)} {\operatorname{Var}(\sum_k
f_k)}.\$\$ Each term receives its own variance plus its covariance with
the other terms. This splits each pair's covariance contribution equally
between its two terms, so the individual proportions sum to one.
Proportions can be negative or greater than one when terms offset each
other's variation; they are not restricted to the interval from zero to
one.

`.proportion` reports these allocations, with equal-tailed posterior
bounds in `.proportion_lower_ci` and `.proportion_upper_ci`. These
bounds are on the proportion scale. Each posterior proportion uses the
term contributions and total from the same joint draw. Individual
interval limits need not sum to one. The total row's proportion is one.
For additional linear predictors, the columns receive suffixes `.1`,
`.2`, and so on, matching the total labels; for example, `.proportion.1`
and `.proportion_lower_ci.1`. A shared term has a separate allocation
for each predictor. Entries for unrelated terms and totals are `NA`.

Proportions are undefined when the total variance is zero and are
returned as `NA`, including for an intercept-only predictor. Posterior
draws with zero total variance retain `NA` proportions and are omitted
when computing proportion intervals; if none are defined, both bounds
are `NA`.

Term intervals are equal-tailed posterior intervals, computed by drawing
the full coefficient vector from its approximate Gaussian posterior and
recalculating each term's variance over the same fitting observations.
All terms use the same coefficient draw in each replicate, preserving
their dependence. Each total is calculated by summing the term
contributions within that draw and then computing their variance.
Additional fitted random effects in mixed models are drawn conditional
on the GAM coefficients, retaining their covariance with the GAM terms
and with each other. Smoothing parameters, random-effect covariance
parameters and the response scale are held at their fitted values,
unless `unconditional = TRUE` supplies a correction for
smoothing-parameter uncertainty in the GAM coefficient covariance. PQL
and generalized mixed models use the Gaussian approximation at the
fitted working model. For SCAM fits, draws are made jointly on the
unrestricted fitting scale and transformed before evaluating the terms.

Point estimates remain the variances of the fitted contributions.
Interval bounds are quantiles of the posterior term standard deviations;
square the bounds to obtain intervals for the term variances. Use `seed`
for reproducible draws and `n_draws = 0` to skip intervals and return
`NA` bounds.

The `covariance` attribute contains the covariance matrix of the fitted
term contributions, with rows and columns in `.component` order,
excluding the total rows. Its diagonal equals `.variance` for the
individual terms. For a model with a single linear predictor, the sum of
all matrix entries equals the `total` row's `.variance`. Adding only the
diagonal omits covariance between terms. This matrix describes variation
across observations, rather than estimation uncertainty. When intervals
are computed, the `variance_draws` attribute contains a matrix with one
row per posterior draw and one column per component, on the variance
scale. Rows are paired across terms. Its column names match
`.component`; the `coverage` attribute records the requested interval
coverage. The total columns include covariance between term
contributions within each draw. Summing the individual term variances
would omit that covariance. The `proportion_draws` attribute is a named
list with one matrix per total (`total`, `total.1`, and so on). Each
matrix has the same rows and columns as `variance_draws`, with `NA`
entries for unrelated components. Its rows retain the pairing with the
coefficient and variance draws. Proportions and their intervals are
added only for `type = "terms"`. The `term_groups` attribute lists the
component labels belonging to each predictor, named by its total label.
Use
[`draw.variance_comp()`](https://gavinsimpson.github.io/gratia/reference/draw.variance_comp.md)
to plot components or proportions with their stored intervals.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

[`mgcv::gam.vcomp()`](https://rdrr.io/pkg/mgcv/man/gam.vcomp.html),
[`draw.variance_comp()`](https://gavinsimpson.github.io/gratia/reference/draw.variance_comp.md)
