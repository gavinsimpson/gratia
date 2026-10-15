Grouped-family posterior response sampling
==========================================

`posterior_samples()` now supports supplied evaluation data for models fitted
with `mgcv::gfam()`, including non-discrete `gam` and discrete `bam` fits. The
ordinary prediction path evaluates discrete `bam` fits at the supplied original
covariates. It does not substitute the compressed fitting design.

Each joint coefficient draw supplies one linear predictor per evaluation row.
The row's own component family supplies its inverse link, fitted nuisance
parameters and response generator. Formula offsets are evaluated at supplied
covariates. Fitting-call offsets remain attached to fitting observations when
`data = NULL`; mgcv does not transfer those offsets to new observations.

Membership can be recovered from the second column of a response matrix or the
index expression in `cbind(y, index)`, with no observed `y` required. Alternatively
supply `family_index`, one family-list position per requested evaluation row.
Conflicting explicit and recovered memberships fail. Neither training order nor
training row counts supply membership implicitly for new data.

Fitting prior weights remain the default at fitting observations. New-data
weights default to one; an explicit scalar or vector overrides either default.
Gaussian component weights control precision and binomial component weights are
integer trial counts. Missing weights suppress only the affected response draws.
Missing covariates and membership follow the prediction `na.action` convention.
Excluded fitting positions are restored once in every draw. Output preserves
component labels, global draw identifiers, chains, iterations and attached
coefficient diagnostics.

Posterior response generation fills one preallocated response buffer and builds
the output table once. Diagnostic calculations can still request one response
table at a time. Both paths call component generators in the same order and
produce identical responses and output columns under the same seed, including
missing predictors, weights, memberships and restored fitting exclusions.

Grouped ordinal response draws are available, although ordinal response-scale
fitted means and their derivatives remain unsupported because category
probabilities cannot be stored as the existing scalar fitted value. Censored
grouped components still require an observation process for posterior response
sampling; `simulate()` retains latent uncensored component draws. Families that
mgcv cannot fit as gfam components are not added by this prediction change.

Grouped derivative sampling now retains membership-expression variables when
constructing shifted evaluation rows. Supply membership in `data`; an explicit
`family_index` control is not supported by `derivative_samples()` and produces
an informative error. Other covariates and offsets are held at the supplied
values while the focal covariate is shifted. Smooth draws remain contributions
to the shared linear predictor. Fixed-parameter `predicted_samples()` continues
to use the same component generators through `simulate()`.

Validation
----------

`tests/testthat/test-sampling-grouped.R` independently reconstructs weighted
binomial, Gaussian and Poisson draws from native prediction matrices and R's
random generators. It covers gam/bam, reordered and repeated fitting subsets,
new covariates, offsets, no observed responses, explicit membership, matrix
responses, ordinal components, missing values and weights, fitting exclusions,
chains, supplied draws, diagnostics, joins, derivative/smooth/predicted workflows
and precise argument errors. Fully observed fitting predictions agree exactly
with the previous diagnostic posterior sampler under identical seeds for both
user-supplied and Gaussian-generated coefficient draws.

The focused test run passed 86 assertions with no failures, warnings or skips.

## Prediction timings

The installed final source evaluates 200 new covariate rows using 1,000
supplied Gaussian coefficient draws across four chains, from a fit to 2,000
observations with binomial, Gaussian and Poisson components. Fixed-parameter
responses use 1,000 replicates. Coefficient sampling is excluded. BLAS/OpenMP
threads are limited to one; these runs follow the longer study and precede
package validation. Times are medians over three seeds, in seconds.

| Fit | Expected-response draws | Posterior response draws | Fixed-coefficient response draws |
|:----|------------------------:|-------------------------:|--------------------------------:|
| gam | 0.021 | 0.174 | 0.245 |
| bam | 0.020 | 0.172 | 0.234 |

All six fit/seed runs completed without warnings or errors.

Reproduce with grouped-benchmark.R followed by grouped-summary.R, after
installing this branch and setting R_LIBS to its installation. Raw results:
grouped-benchmarks.csv; runtime details: grouped-benchmark-environment.txt.
