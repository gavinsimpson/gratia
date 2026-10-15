# Posterior prediction for location-scale-shape models

All seven installed mgcv LSS families now support supplied evaluation data.
The [support audit](lss-audit.md) describes parameter conventions, expected
responses, fitting limitations and the functions that consume joint draws.

## Prediction and response costs

Each model was fitted to 2,000 simulated observations with linear predictors
containing four to eight coefficients in total. These timings evaluate 200
genuinely new covariate rows without observed responses. Fitted and posterior
response sampling reuse 1,000 Gaussian coefficient draws over four chains.
Fixed-coefficient response sampling draws 1,000 replicated responses. The
table reports medians over three seeds, in seconds. Coefficient sampling and
diagnostics are excluded; parameter transformations, response generation, tidy
tables and row/chain identities are included. BLAS/OpenMP threads were limited
to one, and validation processes had finished before timing.

Fitted expectations use bounded batches of matrix products and one output
table. Response generation retains one call per coefficient draw, preserving
the random-number order of the existing fitting-observation sampler.

| Family | Expected-response draws | Posterior response draws | Fixed-coefficient response draws |
|:-------|------------------------:|-------------------------:|--------------------------------:|
| gaulss | 0.021 | 0.476 | 0.080 |
| gammals | 0.019 | 0.474 | 0.085 |
| gumbls | 0.023 | 0.486 | 0.096 |
| gevlss | 0.033 | 0.502 | 0.102 |
| shash | 0.053 | 0.490 | 0.103 |
| twlss | 0.019 | 0.741 | 0.294 |
| ziplss | 0.028 | 0.529 | 0.133 |

All 21 family/seed runs completed all three transformations without warnings or errors.

## Affected MH benchmark

The family-backend study reran all six Gaussian location-scale cases: three
seeds with mgcv and family backends, 2,000 fitting observations, 20 coefficients,
4,000 retained draws over four serial chains and 1,000 burn-in iterations per
chain. Prediction evaluates the first 200 rows using those retained draws.
Median fitted transformation cost is 0.188 seconds and median posterior response cost is 2.003 seconds.
The family/mgcv minimum-coefficient ESS/s ratios are 1.18 for bulk and 1.33 for tails; maximum R-hat is 1.004.
These paired ratios use the median over three seeds. The public sampling
time includes setup and burn-in; prediction and response costs are separate.
Process peak memory includes fitting, diagnostics and transformations.

Raw timing and diagnostic results are in family-benchmarks.csv and
family-coefficients.csv. The LSS transformation rows are in
lss-prediction-benchmarks.csv, with failure notes retained if a transformation
cannot run. lss-benchmark-environment.txt records package/runtime details.
See lss-validation.txt for the final tests and source-package check.

## Reproduce

Install this branch and set R_LIBS to its installation, then run:

```sh
Rscript notes/posterior-chains/family-study.R
Rscript notes/posterior-chains/family-summary.R
Rscript notes/posterior-chains/lss-prediction-benchmark.R
Rscript notes/posterior-chains/lss-summary.R
```
