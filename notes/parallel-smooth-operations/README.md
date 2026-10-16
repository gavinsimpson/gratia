# Parallel calculations across smooths

The implementation uses one internal mapper around `purrr::in_parallel()` for
the four entry points. With multiple tasks, installed `mirai` and `carrier`, and
a caller-configured daemon pool, workers calculate each smooth or comparison.
Otherwise the same task functions run serially. The mapper leaves daemon setup
and shutdown to the caller and propagates errors.
Direct `in_parallel()` calls would repeat package and daemon availability checks
and worker helper lookup in four functions. The small mapper keeps result order
and error propagation consistent while still requiring explicit task inputs; it
adds no public scheduling controls.

Model selection and preparation run once in the caller. Derivative workers
construct each smooth's finite-difference matrices, estimates and standard
errors. Sample workers multiply each smooth's prediction matrix by its columns
of one shared set of joint coefficient draws. Difference workers use only the
matched rows and active coefficient columns for their factor-level comparison.
Transformed covariates are evaluated in the caller. Prepared coordinates, smooth
definitions and small model flags cross the process boundary. Ordinary paths
avoid sending the fitted model and fitting data. Simple random-effect formula
environments are detached; custom random-effect formulas containing function
calls retain their environment for prediction compatibility and can still carry
large payloads.

Custom prediction methods are evaluated in the caller, including methods used
by tensor margins and factor-smooth bases. The resulting matrices are sent to
workers for derivative and posterior calculations. This preserves methods that
use local functions, data stored with a function or inherited S3 methods without
changing the caller's worker registrations. Prediction methods supplied by mgcv
continue to run in workers, as do scam methods for models fitted by scam. A gam
fit using a scam basis prepares its matrix in the caller because its workers do
not otherwise load scam.

Simultaneous intervals use two worker passes. The first returns compact design
matrices, estimates and standard errors. The caller generates coefficient
deviations in the established order, only for results that need them, and the
second worker pass computes interval limits. Full-model deviations are still
generated sequentially before selecting each term's columns. This preserves the
existing random sequence and seeded serial/parallel agreement, including calls
with zero standard errors. It also retains the serial cost of covariance
factorization and simulation, with a temporary matrix of `n_sim` rows and one
column per model coefficient. The compact design matrices travel back to the
caller and out again for interval calibration.

`smooth_samples()` completes shared coefficient sampling before starting term
tasks. It preserves correlations between terms, supplied draws, chain identities
and sampling diagnostics. MH chain workers and term workers therefore operate
in succession. Existing sampling thread controls keep their meaning.

These choices make worker inputs smaller than repeatedly sending fitted models,
full prediction matrices or full draw matrices. Multiple comparisons can still
repeat coefficient columns and prediction rows. Building all task inputs and
combining the returned draws also uses memory in the caller; adding workers does
not reduce the size of the final result.

For a smooth with `k` active coefficients, sample inputs contain `n_draws * k`
coefficient values, whereas the returned result has `n_grid * n_draws` rows.
Dense grids and many draws can therefore make returning and combining results
more costly than multiplying the prediction and draw matrices.

## Reproduce the benchmark

From the repository root, install the development build into a private library,
then run:

```sh
mkdir -p /tmp/gratia-parallel-library
R CMD INSTALL --library=/tmp/gratia-parallel-library .
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
  VECLIB_MAXIMUM_THREADS=1 R_LIBS=/tmp/gratia-parallel-library \
  Rscript notes/parallel-smooth-operations/benchmark.R
```

The script fits three `bam()` models to 50,000 observations outside the timed
regions: four univariate smooths with `k = 40`, two tensor-product smooths with
`k = c(6, 6)`, and five factor-by smooths with `k = 30` (ten pairwise comparisons).
The cheap cases use 100 evaluation rows per term or comparison and pointwise
intervals; the sampling case uses 20 draws. Expensive derivative cases use 6,000
rows per term and 3,000 simulations for simultaneous intervals; differences use
1,800 rows per comparison and 3,000 simulations; sampling uses 1,000 rows per
term and 500 draws, returning two million rows.

Each reported serial and warmed parallel time is the median of three complete
function calls. Fitting, garbage collection, payload inspection, daemon startup
and worker namespace loading are outside these times. Two real worker processes
use the same development installation as the caller. OpenMP and BLAS thread
limits are set before R starts and inherited by workers. The script owns its
pool and stops it on exit. First parallel calls are recorded separately after
namespace loading; only the first workload is the first calculation in that
pool.

Payload inspection traces every internal mapping pass in a separate untimed
run. Send and return figures sum R serialization lengths for each individual
task across all passes; they describe task contents rather than the transport's
complete wire protocol. Fitted model and final result sizes use `object.size()`;
fitted model sizes exclude referenced environments. These are object and payload
sizes, not measurements of peak resident memory across the caller and workers.

The complete measurements are in [results.csv](results.csv), individual timing
repetitions in [timings.csv](timings.csv), and package versions, BLAS information,
development installation path and worker process IDs in [session.txt](session.txt).

## Measured results

Measured on 2026-10-16 with R 4.6.1 on macOS arm64, ten physical cores and
Accelerate BLAS, using two workers. Times are elapsed seconds.

| Workload | Serial median | First parallel call | Warmed parallel median | Serial / warmed |
| --- | ---: | ---: | ---: | ---: |
| Derivatives, 100 rows | 0.023 | 0.027 | 0.018 | 1.28 |
| Derivatives, 6,000 rows, simultaneous | 2.865 | 2.084 | 1.542 | 1.86 |
| Partial derivatives, 100 rows | 0.014 | 0.013 | 0.014 | 1.00 |
| Partial derivatives, 6,000 rows, simultaneous | 1.317 | 0.863 | 0.769 | 1.71 |
| Samples, 100 rows, 20 draws | 0.032 | 0.033 | 0.024 | 1.33 |
| Samples, 1,000 rows, 500 draws | 0.326 | 0.377 | 0.418 | 0.78 |
| Differences, 100 rows | 0.020 | 0.023 | 0.023 | 0.87 |
| Differences, 1,800 rows, simultaneous | 2.028 | 1.371 | 1.296 | 1.56 |

Daemon startup took 0.187 seconds and worker namespace loading took a further
0.915 seconds, for 1.102 seconds of setup. Adding that setup to the first parallel
call times would exceed the corresponding serial times in all three expensive
interval cases. Reusing configured workers matters for these workloads.

The dense simultaneous interval cases benefited from distributing matrix
calculations and calibration: warmed elapsed time fell by 35–46%. Cheap cases
took only 14–32 milliseconds serially; their differences are small in absolute
terms and three repetitions do not establish a reliable advantage. The case
returning two million sample rows became slower with workers. The final result
occupies 152.6 MiB in R, and worker returns serialize to 45.8 MiB despite total
sample task inputs of only 1.93 MiB. Returning and combining that output is a
material cost. Two workers therefore help the expensive interval calculations
measured here, while this large sample output favors serial execution.

Fitted model objects occupy 7.17 MiB (four univariate smooths), 5.48 MiB (two
tensor-product smooths) and 7.17 MiB (five factor-by smooths), excluding their
environments. The table below includes every worker pass. Sizes are MiB
(`2^20` bytes).

| Expensive workload | Total task inputs | Largest task input | Total worker returns | Final R object |
| --- | ---: | ---: | ---: | ---: |
| Derivatives | 14.70 | 3.20 | 12.31 | 4.40 |
| Partial derivatives | 6.69 | 2.94 | 5.88 | 2.75 |
| Samples | 1.93 | 0.48 | 45.78 | 152.59 |
| Differences | 39.93 | 2.33 | 12.27 | 3.33 |

The comparison workload has ten pairs, with overlapping coefficient columns and
prediction rows repeated across pairs. These measurements support using an
existing worker pool for expensive interval calculations; worker count alone
does not determine performance.
