# Posterior response generation performance

This study compares the previous LSS posterior-response calculation with the
optimised implementation in the same R process. The reference calculation
repeats model-matrix column extraction and constructs/restores one tibble per
coefficient draw. The current calculation caches invariant design blocks,
fills response values directly, restores rows by position and builds one table.

Both calculations retain the same matrix-vector products, distribution-parameter
transformations and one family-generator call per coefficient draw. Every
timed output is checked for exact equality, including responses, row positions,
chain/iteration/global draw identities and attached sampling information.

## Paired elapsed times

Each family is fitted to 2,000 observations. Each transformation evaluates 200
new covariate rows using 1,000 supplied Gaussian coefficient draws over four
chains. Coefficient generation is excluded; prediction setup, parameter
transformations, response generation, table construction and metadata are timed.
Three seeds are evaluated with five repetitions each, alternating execution
order and collecting garbage before each call. BLAS/OpenMP threads are limited
to one; the longer sampler study and validation tests finished before these timings.

Times are medians in seconds. Speedup is the median previous/current ratio
across matched seed/repetition pairs.

| Family | Previous | Current | Speedup |
|:-------|---------:|--------:|--------:|
| gaulss | 0.464 | 0.077 | 5.93 |
| gammals | 0.479 | 0.091 | 5.19 |
| gumbls | 0.479 | 0.089 | 5.59 |
| gevlss | 0.564 | 0.115 | 4.81 |
| shash | 0.504 | 0.100 | 5.09 |
| twlss | 0.736 | 0.313 | 2.32 |
| ziplss | 0.536 | 0.132 | 4.00 |

## Allocation traffic

Rprofmem records total R allocation traffic for one representative gaulss
transformation after timing. This measures allocated bytes and allocation
counts during the call, not maximum resident memory or the size of live objects.

| Implementation | Allocated MB | Allocations | Output MB |
|:---------------|-------------:|------------:|----------:|
| previous | 66.23 | 38245 | 6.42 |
| current | 52.32 | 26226 | 6.42 |

The public result still stores every requested observation-by-draw response.
Streaming diagnostic calculations retain their one-draw-at-a-time interface
and share the cached numeric generator. The optimisation does not vectorise
random generators across draws, which could change seeded responses.

Grouped-family responses use the same single-table construction; their new-data
timings and support boundaries are recorded separately in grouped-support.md.

Reproduce after installing the branch and setting R_LIBS:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 Rscript notes/posterior-chains/response-performance.R
Rscript notes/posterior-chains/response-summary.R
```

Raw paired timings: response-performance.csv. Allocation measurements:
response-allocations.csv. Runtime details: response-environment.txt.
