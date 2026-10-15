# Longer comparisons on difficult conditional posteriors

This study extends the earlier 4,000-draw family comparison to 16,000 retained
coefficient draws across four serial chains, with 4,000 burn-in iterations per
chain. Each run therefore performs 32,000 iterations including burn-in. Three
seeds (281–283) sample the same fitted target for each model. Smoothing
parameters and dispersion remain fixed at their fitted values.

## Reproduce the study

Install the merged source at commit `c8c123508a56202b16539281914ed6848dd28e11`
in a private R library, then run from the repository root:

```sh
R_LIBS=/private/tmp/gratia-difficult-lib Rscript notes/posterior-chains/difficult-study.R
R_LIBS=/private/tmp/gratia-difficult-lib Rscript notes/posterior-chains/difficult-summary.R
```

The driver saves every completed run and resumes completed model/method/seed
combinations. It checks draw and burn-in counts when resuming. To repeat the
study from scratch, move the existing `difficult-*.csv` result files before
running it again. The scripts do not install packages or change model code.
`GRATIA_DIFFICULT_DRAWS` and `GRATIA_DIFFICULT_BURNIN` can shorten a trial run;
those settings describe a different study and must have separate output files.
All R processes for the recorded study ran outside the sandbox.

Every model/method/seed uses a fresh R process, with BLAS/OpenMP thread counts
limited to one. `/usr/bin/time` measures peak resident memory of that complete
process, including fitting, diagnostics, prediction and response generation.
It does not isolate the sampler's working memory. Other development work was
running during the first few cases; timings are empirical elapsed costs on this
machine, not measurements from an otherwise idle dedicated machine. The parent
validation runs and additional performance study waited for this study to
finish. Package and R versions are recorded in `difficult-environment.txt`.

## Targets and calculations

- `sparse`: the earlier family-study binomial example, with 2,000 observations
  and 30 coefficients. The linear predictor drops by six between x = 0.4
  and x = 0.8, making successes scarce in that region.
- `rare_poisson`: 1,500 observations and 25 coefficients. The log mean is
  shifted down by 3.5 everywhere, and by another four in the central region.
- `sparse_high_p`: the sparse binomial construction with 1,500 observations
  and 50 coefficients, to increase the dimension relative to the data.

All models include the same known fitting offset. Data and fitted models are
identical across methods and sampling seeds. MH uses the existing independence
Student-t proposal with 40 degrees of freedom and random-walk scale 0.25.
The three MH backends use their existing chain workers. The specialised and
family backends prepare their target and proposal factorisation once for all
chains; mgcv setup remains inside each chain. For mgcv, the separate setup
column is zero because its setup cost is included in sampling time.

MALA and elliptical slice sampling use the existing internal prototypes, the
same specialised target, and the fitted coefficient covariance. MALA tunes
its step size only during warm-up. The elliptical sampler divides the target
by its proper fitted Gaussian reference. Neither prototype was changed for
this comparison. Both start each chain at the fitted mode; the specialised
and family MH chains use their existing dispersed initial states. Independent
L'Ecuyer streams are allocated by chain before sampling.

At 21 common evaluation points, the study times `fitted_samples()`,
`derivative_samples()` and `posterior_samples()` using the same supplied
coefficient matrix. It verifies draw counts and chain identities, and compares
expected responses and response derivatives against direct model-matrix
calculations and the inverse-link chain rule. The response derivative check
uses central differences with epsilon 1e-5. These checks run in every benchmark
process and cause that run to fail if the two calculations disagree.

Diagnostics cover every coefficient and nine derived quantities: expected
responses at three points, the average expected response over the grid, two
response contrasts and three response derivatives. Points include the sparse
central region and both sides of it. Each variable has mean, standard deviation,
rank-normalised R-hat, bulk/tail ESS and Monte Carlo standard errors for the
mean and standard deviation in `difficult-diagnostics.csv`. Per-chain
acceptance rates, tuned MALA step sizes and elliptical likelihood-evaluation
counts are saved in `difficult-chains.csv` where those quantities apply.

Throughput divides the minimum ESS across variables in a group by setup plus
sampling time, including burn-in. The tables report the median throughput and
time across seeds, and the worst R-hat across variables and seeds. The diagnostic
screen requires all three runs to have R-hat below 1.01 and both bulk and tail
ESS of at least 400 for every variable. Runs failing that screen are retained
in the results and cannot provide a reliable method ranking. The screen is a
check on these runs, not a guarantee of posterior accuracy.

Transformation timings use the installed merged snapshot before the response
allocation optimisation. They are baseline costs; the separate paired response
benchmark measures that optimisation. Fitting, diagnostics and transformations
are excluded from sampler ESS per second and recorded separately, allowing
end-to-end costs to be calculated for a particular workflow.

## Results

All 45 runs completed. Every method/model combination passed the diagnostic
screen in all three seeds, for both coefficients and the nine derived
quantities. Worst R-hat was 1.0054 for coefficients and 1.0044 for derived
quantities. The smallest bulk/tail ESS across all runs was 915/403 for
coefficients and 998/455 for derived quantities. The largest Monte Carlo
standard error for a mean, divided by that variable's posterior standard
deviation, was 4.1% for coefficients and 2.3% for derived quantities.

| Target | Method | Seconds | Coefficient R-hat | Quantity R-hat | Bulk ESS/s, coefficients | Tail ESS/s, coefficients | Bulk ESS/s, quantities | Tail ESS/s, quantities | Pass all runs |
|:--|:--|--:|--:|--:|--:|--:|--:|--:|:--|
| sparse | mgcv | 6.15 | 1.0047 | 1.0033 | 298.0 | 149.0 | 324.2 | 163.2 | yes |
| sparse | gratia | 10.78 | 1.0027 | 1.0026 | 181.1 | 92.5 | 200.6 | 102.4 | yes |
| sparse | family | 6.43 | 1.0027 | 1.0026 | 314.7 | 155.5 | 348.7 | 178.0 | yes |
| sparse | mala | 11.82 | 1.0043 | 1.0036 | 158.9 | 193.8 | 166.9 | 199.5 | yes |
| sparse | elliptical | 19.70 | 1.0035 | 1.0035 | 83.8 | 44.0 | 92.4 | 49.9 | yes |
| rare_poisson | mgcv | 5.94 | 1.0019 | 1.0017 | 466.8 | 251.4 | 499.2 | 271.4 | yes |
| rare_poisson | gratia | 4.75 | 1.0023 | 1.0031 | 685.5 | 360.7 | 798.1 | 486.2 | yes |
| rare_poisson | family | 5.40 | 1.0023 | 1.0031 | 586.5 | 308.6 | 696.6 | 424.4 | yes |
| rare_poisson | mala | 6.62 | 1.0054 | 1.0034 | 337.8 | 505.9 | 371.1 | 497.9 | yes |
| rare_poisson | elliptical | 11.33 | 1.0020 | 1.0021 | 246.4 | 165.1 | 291.8 | 205.2 | yes |
| sparse_high_p | mgcv | 6.22 | 1.0031 | 1.0027 | 413.3 | 187.5 | 455.3 | 206.7 | yes |
| sparse_high_p | gratia | 9.55 | 1.0018 | 1.0025 | 255.0 | 95.0 | 262.0 | 98.0 | yes |
| sparse_high_p | family | 6.98 | 1.0018 | 1.0025 | 348.5 | 129.9 | 365.9 | 137.8 | yes |
| sparse_high_p | mala | 11.81 | 1.0038 | 1.0044 | 150.7 | 192.8 | 147.8 | 206.4 | yes |
| sparse_high_p | elliptical | 17.85 | 1.0045 | 1.0037 | 125.8 | 53.5 | 129.8 | 59.6 | yes |

The sparse binomial baseline had 656 successes, the rare Poisson target had
34 events in total, and the higher-dimensional binomial target had 529
successes. These are three specified simulated targets; the larger coefficient
count alone did not make the last target mix worse than the baseline.

The previous 4,000-draw study had a worst sparse-binomial R-hat of 1.018.
At 16,000 draws and longer burn-in, all five methods pass the screen on that
same target. This supports allowing the diagnostic results and Monte Carlo
precision of the required quantity to determine run length. A requested draw
count is the number retained, and does not specify the effective sample size.

The recorded means and diagnostics match between the specialised and family
MH backends in every paired run. Across the other 1,188 variable-level
comparisons with specialised MH, the largest mean discrepancy was 3.76 times
the square root of the two squared Monte Carlo standard errors. This is a
comparison with another simulation estimate, not with an exact posterior
integral. Matching seeds also share random-number streams across methods, so
these scaled differences are descriptive and are not calibrated test statistics.

Median peak process memory was 660 MB for the sparse baseline, 625 MB for
rare Poisson and 656 MB for the higher-dimensional case. These numbers include
the complete workflow described above. They should not be interpreted as the
memory needed for the sampler alone.

## Recommendation

Keep all three MH backends available and leave the alternative samplers
internal. The specialised backend has the best bulk throughput on rare
Poisson, while mgcv or family MH has the best bulk throughput on the two
binomial examples. The family callback adds broad model support and can be
competitive on these targets; compiled family-specific code is not an
automatic speed advantage.

MALA's recorded median coefficient tail throughput exceeds the best MH result
by about 25% on the sparse baseline, 40% on rare Poisson and 3% on the
higher-dimensional case, while its bulk throughput is lower. The last
difference is small relative to timing variation. Its advantage for derived
quantity tails is smaller, and it approximately ties MH on the
higher-dimensional case. This makes an optimised MALA loop a useful next
experiment for quantities that depend on posterior tails. The current MALA
and elliptical loops are in R, whereas MH loops are compiled. These timings
compare the implementations that exist now; a compiled alternative could
change the throughput comparison.

The existing elliptical implementation has lower bulk and tail throughput
than MH in all three examples, providing no reason to expose it from this
study. None of these measurements changes the conditional target or introduces
smoothing-parameter uncertainty.

A specific remaining optimisation in the specialised evaluator is that the
shared C++ likelihood function calculates and stores an observation score
even for MH density evaluations, which never use the gradient. For binomial
data this includes a logistic transformation in addition to the log-likelihood
calculation. Skipping that unused work could improve the MH timings; this
study leaves the evaluator unchanged and does not claim a measured gain.

`difficult-validation.txt` records the completed counts and worst diagnostics.
The detailed CSVs retain every run and variable, including the full spread
across seeds, rather than selecting the fastest or best-mixing run.

## Check against the later main integration

The longer study uses the fixed c8c12350 installation. Later changes from
main have also been integrated into the working branch. The short script
`difficult-integration-parity.R` checks whether that integration preserves the
three fitted targets and seeded sampling results before applying this study's
conclusions to the final source. It retains 400 draws across four chains with
100 burn-in iterations per chain, using seed 281 and each of the three public
MH backends. Separate reference and final R processes compare numeric draws,
draw identities and acceptance information exactly, excluding elapsed times.

The script also checks fitted coefficients, smoothing parameters, dispersion,
responses, weights, offsets, design matrices and fitted coefficient covariance.
Its header gives the two commands to write a reference from the study library
and check it against the final installation. Both commands passed against the
final source after integrating main through b8c9fafa2. All three targets and
all nine MH results agreed exactly, including coefficient values, draw
identities and acceptance information. The longer study was not rerun:
these models have the same targets and seeded sampler behaviour after integration.
