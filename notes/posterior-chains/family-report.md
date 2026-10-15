# Family evaluator: implementation and effective sample comparisons

`mh_backend = "family"` uses the same compiled MH loop as the specialised
gratia backend, calling the fitted family's R evaluators for the likelihood.
It prepares the design, smoothing penalty and covariance factorisation once
for all chains. Standard and extended families use `dev.resids()` and
`linkinv()` with their validity checks; general families use `ll(deriv = 0)`.
Response attributes and predictor mappings are retained. For `mvn()`, setup
also reconstructs the design cross-products required by its evaluator.

The evaluator supports independent-observation gam and bam fits, including
discrete bam evaluated at original covariates. It caches the full design
matrix. Block controls apply only to the specialised gratia backend.
Quasi families, Cox proportional hazards, GAMM components, AR1 dependence,
paraPen, H, pre-fit G objects and singular covariance matrices are rejected.
Smoothing, dispersion and additional family parameters are fixed. Scale or
covariance parameters represented by fitted coefficients are sampled.

## Reproduce the measurements

Install this branch and set R_LIBS to that installation. Existing rows are
resumed; remove family-benchmarks.csv and family-coefficients.csv to start
a fresh study. Run from the repository root:

```sh
Rscript notes/posterior-chains/family-study.R
Rscript notes/posterior-chains/family-summary.R
```

Each model/backend/seed runs in a fresh R process with BLAS/OpenMP threads
limited to one. All samplers retain 4,000 draws over four serial chains,
discarding 1,000 burn-in iterations per chain. Three seeds use the same
simulated data and fitted model. The timed public post_draws() call includes
setup and burn-in: family/gratia reuse setup, while mgcv repeats its setup
inside each chain. Model fitting and compilation are excluded. Diagnostics,
prediction and response generation are recorded separately.

The throughput measure is the minimum coefficient bulk or tail ESS divided
by elapsed time for setup and sampling. Ratios below pair matching seeds and
report the median of three ratios; values above one favour the family backend.
R-hat is the maximum over coefficients, seeds and backends in that model.

## Family versus mgcv

| Model | Observations | Coefficients | Bulk ESS/s ratio | Tail ESS/s ratio | Maximum R-hat |
|:------|-------------:|-------------:|-----------------:|-----------------:|--------------:|
| Poisson | 2000 | 10 | 1.18 | 1.21 | 1.002 |
| Poisson | 10000 | 30 | 1.00 | 1.06 | 1.005 |
| Binomial | 10000 | 10 | 0.84 | 0.85 | 1.002 |
| Sparse binomial | 2000 | 30 | 0.55 | 0.38 | 1.018 |
| Multinomial | 2000 | 20 | 1.09 | 1.06 | 1.003 |
| Censored normal | 2000 | 10 | 1.14 | 1.17 | 1.002 |
| Tweedie | 1000 | 10 | 1.28 | 1.21 | 1.002 |
| Grouped binomial/Poisson | 2000 | 10 | 0.90 | 0.87 | 1.002 |
| Gaussian location-scale | 2000 | 20 | 1.18 | 1.33 | 1.004 |

## Family versus specialised gratia

The seconds below are median elapsed times for setup and sampling. ESS ratios
again divide family throughput by specialised gratia throughput.

| Model | Observations | Gratia seconds | Family seconds | Bulk ESS/s ratio | Tail ESS/s ratio |
|:------|-------------:|---------------:|---------------:|-----------------:|-----------------:|
| Poisson | 2000 | 0.421 | 1.575 | 0.27 | 0.27 |
| Poisson | 10000 | 2.418 | 8.381 | 0.29 | 0.29 |
| Binomial | 10000 | 3.612 | 5.262 | 0.69 | 0.69 |
| Sparse binomial | 2000 | 0.987 | 1.393 | 0.71 | 0.71 |

## Interpretation

The family backend broadens coverage without duplicating family likelihoods.
It is an explicit option rather than a replacement for mgcv: throughput
depends on the family, model size and mixing. The specialised gratia backend
remains useful where its supported likelihoods give higher throughput.
The tables retain cases where the family backend is slower. In this study,
multinomial, Gaussian location-scale, censored-normal and Tweedie models
gain throughput relative to mgcv in the median comparisons. Binomial and
grouped models lose throughput. The larger Poisson example gives little
consistent gain. The specialised backend is faster than family callbacks
on all four common-family examples. Keep mgcv as the default and family
callbacks as an explicit option; these results do not justify automatic
backend selection based only on a family name.

The sparse-binomial runs have R-hat above 1.01 in several chains and variable
tail ESS. Four thousand retained draws with this warmup do not give stable
tail comparisons for that case; longer runs are needed before ranking the
backends there. The remaining rows describe these simulated targets and
three seeds, rather than establishing a universal speed ordering.

family-benchmarks.csv contains per-run timing, ESS, R-hat and process peak
memory. Peak memory also includes model fitting, diagnostics, prediction and
response generation. Unavailable transformation costs are NA, with the
reason in fitted_note or posterior_note. Gaussian location-scale prediction
and response timings have been rerun with supplied evaluation data after
adding joint LSS prediction support. See lss-audit.md for family details.
family-coefficients.csv retains coefficient summaries
and Monte Carlo errors. family-benchmark-environment.txt records the runtime.
family-validation.txt records the tests and package check. Tests in gratia
cover mgcv families; tests for inlaws remain with that package.
