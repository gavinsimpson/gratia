# Posterior-chain implementation and experiments

The public interface extends the existing sampling functions. The default MH
backend remains mgcv. `mh_backend = "gratia"` selects the new evaluator for the
five supported family/link combinations and for independent-observation bam
fits. MALA, elliptical slice sampling, the compressed-design adapter, and the
log-smoothing mixture remain internal research implementations.

## Reproducing the studies

Install this worktree into a separate R library, then set `R_LIBS` to that
library when running these scripts from the repository root. The benchmark
runner uses R and `/usr/bin/time` (macOS or Linux):

```sh
Rscript notes/posterior-chains/run-benchmarks.R
Rscript notes/posterior-chains/parallel-study.R
Rscript notes/posterior-chains/block-study.R
Rscript notes/posterior-chains/feasibility.R
```

The measurements were made on 14 October 2026 with R 4.6.1, mgcv 1.9-4,
Apple clang 17 (`-O2`), and Apple Accelerate BLAS on arm64 macOS.
`benchmarks.csv` records setup and sampling time, minimum coefficient bulk/tail
ESS per second, maximum R-hat, prediction/response-generation time, and process
peak resident memory. Every sampler runs in a fresh process. Peak memory includes
R, package loading, model fitting, and sampling; it is not a measurement of the
sampler alone. The runner limits BLAS/OpenMP threads to one.

Each case retains 1,000 draws over four serial chains with 250 warmup iterations
per chain. The reference R, compiled MH, MALA, and elliptical implementations
reuse their prepared evaluator. Reported total time includes that setup. The
mgcv baseline includes its own setup in each chain. These are individual timing
runs; the CSV retains the convergence measures needed to interpret them.

## Compiled MH

Seconds for setup plus sampling; speedup is mgcv time divided by compiled time:

| Family | Observations | Coefficients | mgcv MH | Compiled MH | Speedup |
|---|---:|---:|---:|---:|---:|
| gaussian | 1,000 | 10 | 0.187 | 0.030 | 6.23 |
| poisson | 1,000 | 10 | 0.311 | 0.054 | 5.76 |
| poisson | 10,000 | 10 | 1.923 | 0.546 | 3.52 |
| poisson | 1,000 | 30 | 0.368 | 0.073 | 5.04 |
| poisson | 10,000 | 30 | 2.431 | 0.687 | 3.54 |
| poisson | 100,000 | 30 | 23.728 | 7.782 | 3.05 |
| binomial | 1,000 | 10 | 0.229 | 0.093 | 2.46 |
| sparse | 1,000 | 30 | 0.282 | 0.134 | 2.10 |
| gamma | 1,000 | 10 | 0.355 | 0.055 | 6.45 |
| nb | 1,000 | 10 | 0.338 | 0.111 | 3.05 |

Keep the compiled evaluator: it reduces repeated allocation and R calls, and
provides useful gains in the tested large datasets. It also supplies the
new bam path. Keep mgcv as the default while the new backend has a deliberately
smaller set of supported families and penalties. The Gaussian approximation
remains the efficient direct-sampling option when it is appropriate.

## Alternative samplers

The MALA prototype whitens coefficients using the fitted covariance, uses the
analytic likelihood gradient, adjusts its step size only during warmup, and
includes the forward/reverse proposal correction. It did not improve effective
samples per second enough to justify a public method in these cases.

The elliptical prototype uses `N(beta_hat, Vp)` as a reference and divides the
full target by that density. It therefore retains the original smoothing
penalties and unpenalised directions. It can compete on bulk ESS in some cases,
but tail ESS and the sparse binomial example do not give a consistent advantage
over compiled MH. Retain both as research code rather than adding method names
that imply a demonstrated improvement. HMC/NUTS remains future work.

The sparse binomial runs have elevated R-hat and low tail ESS for several
samplers. Their 1,000 retained draws are insufficient for reliable summaries.
This case demonstrates why a requested raw draw count needs convergence and
Monte Carlo error diagnostics.

Small Gaussian and Poisson tests independently check posterior moments, including
numerical integration for the Poisson intercept model. Additional tests compare
analytic gradients with finite differences and compare likelihood differences
with mgcv for all five family/link combinations. Tests also cover all supported
constructors in ordinary and discrete bam fits, including fitted dispersion
away from one in fixed-shape negative-binomial models.

## Parallel chains

`parallel.csv` compares the actual public interface on a 10,000-row Poisson model
with 30 coefficients. Every run retains 1,000 draws and uses 1,000 burn-in
iterations per chain. With an already-running, warmed mirai pool, mgcv takes
2.212 seconds for one chain and 1.678 seconds for four; compiled MH takes
0.825 and 0.655 seconds. Creating a temporary pool takes 3.088 and 1.955 seconds
respectively, so startup outweighs the gain for these short runs. Reuse the
caller's mirai pool for repeated sampling. The sampler never resizes or stops it.

The serial and parallel runs produce identical sampled values for a fixed seed
and backend. Elapsed-time diagnostics naturally differ. Each chain receives its
own RNG stream before dispatch. Sampling-created pools have their own compute
profile and are removed on success or failure; outstanding work is cancelled
without shutting down caller-owned workers.

## Memory and discrete bam designs

`block.csv` records equal likelihood values for dense and blockwise evaluation
of a 10,000-row, 20-coefficient discrete bam fit. The stored target is about
2.54 MB with a cached matrix and 1.31 MB with blocks. Twenty value/gradient
evaluations take 0.018 versus 0.915 seconds: repeatedly constructing bases is
expensive. The block path is useful when a full design does not fit in memory;
it should not be chosen as a speed optimisation.

`compressed.R` captures the representation used by mgcv's discrete prediction
routine without modifying its namespace. Its transpose sums repeated coefficient
map entries before removing dropped positions. Likelihoods and gradients agree
with the dense matrix constructed from that same representation, including a
tensor-product example with a dropped coefficient.

`compressed.csv` also compares this discretised design with evaluation at
original covariates. The additive example changes the tested log density by
about 0.01; the tensor-product example changes it by about -60.5. The compressed
representation is smaller, but these differences prevent treating it as an
interchangeable optimisation of the original-covariate target. Keep it private
until the intended discretised target and coefficient mappings have broader
validation.

## Log-smoothing parameter uncertainty

`feasibility.txt` records the retained information for Gaussian REML gam,
fREML bam, and discrete fREML bam fits with two estimated smoothing parameters.
All retain `db.drho` and `Vc`. The gam stores a 3-by-3 `V.sp` with `spind = 1:2`;
the bam examples store a 2-by-2 `V.sp`, while `sp.vcov()` returns 3-by-3 matrices.
The extra dimension in these examples concerns log dispersion. Array dimensions
must be checked before extracting a smoothing block.

The Gaussian mixture prototype draws approximate log-smoothing parameters from
the fitted `V.sp` block, refits the conditional coefficient mode and covariance
at each draw with dispersion held fixed, and then draws coefficients. It records
failed refits explicitly. The study runs 100 mixture draws and compares their
coefficient standard deviations with fixed-smoothing Gaussian draws and the
existing `unconditional = TRUE` correction. This establishes a route for further
work, rather than validating a replacement for the corrected covariance.

This approximation uses the fitted distribution of log-smoothing estimates;
it is not joint posterior MCMC with specified hyperpriors. Refitting for every
smoothing draw is also costly for large bam models. A future method needs a
validated parameter mapping and a less expensive way to update the conditional
coefficient distribution. Changing only the MH proposal covariance would leave
its conditional posterior target unchanged.


## Sampling argument migration

The same branch now uses `n_draws` for retained draws and `n_grid` for
constructed evaluation grids. Generating samples requires an explicit count;
supplied coefficient draws determine their count and chain mapping. Obsolete
names and positional controls produce migration errors. MH workers use
`n_cores`; Gaussian sampling threads use `n_threads`.

See [the migration vignette](../../vignettes/sampling-migration.Rmd) for the
complete mappings and examples. The benchmark scripts use the renamed
arguments; the measurements above describe the original sampling implementation
and were not rerun for this interface change. The subsequent validation record
is in `harmonisation-validation.txt`.
