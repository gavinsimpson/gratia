# Internal smoothing-parameter uncertainty study

Generated with R 4.6.1; mgcv 1.9.4. Each supported fit uses 20000 draws; 600 fitting observations; offsets and unequal prior weights.

The research implementation samples an approximate fitted marginal normal distribution for the free log-smoothing parameters, then samples the exact Gaussian conditional coefficient distribution at each value. Dispersion stays fixed. This mixture uses an approximate normal distribution of fitted log-smoothing parameters; it does not specify hyperpriors or sample a joint posterior for coefficients, smoothing parameters, and dispersion.

## Covariance and coordinate audit

For these Gaussian gam fits, V.sp contains free log-smoothing parameters first and an additional log-dispersion component last when dispersion is estimated. fREML bam's V.sp omits the log-dispersion component, although sp.vcov() includes it. The prototype uses the marginal free-log-smoothing covariance block and holds dispersion at its fitted value; it does not condition the log-smoothing approximation on a particular log-dispersion draw.

Shared and partly fixed smoothing parameters are expanded with attr(V.sp, 'L'), with fixed multipliers retained in an additive log-scale offset. attr(V.sp, 'spind') is not used as a free-parameter index: for the shared gam example it includes a position belonging to log dispersion. The old feasibility prototype did not handle this case.

In the installed mgcv, fREML bam stores db.drho before transforming derivatives to the fitted coefficient basis. Those raw derivatives disagree with derivatives of fitted coefficient means. The prototype calculates derivatives in fitted coordinates from the conditional precision, independently verified by centred numerical differences. gam and ML/REML bam derivatives agree with db.drho after the smoothing mapping is applied. Vc already uses the transformations internally and remains the reference for unconditional = TRUE.

Across supported fits, the largest relative coefficient error at fitted sp was 2.4e-13 and the largest relative conditional covariance error was 3.5e-12.

## Marginal uncertainty

The table averages marginal coefficient standard deviations. Mixture variances use E[Var(beta | log sp)] + Var[E(beta | log sp)], separating conditional uncertainty from variation in conditional means. The nonlinear mixture and mgcv's unconditional correction are different approximations, so agreement is assessed rather than assumed. A positive between-mean contribution does not guarantee a larger variance than at fitted sp: averaging the conditional covariance can reduce that component, as in the linked examples.

| Fit | Free sp | Conditional SD | mgcv unconditional SD | Mixture SD | Variance from conditional means | Sampling seconds |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| gam_REML | 2 | 0.1486 | 0.1508 | 0.1494 | 1.7% | 2.96 |
| gam_ML | 2 | 0.1470 | 0.1496 | 0.1483 | 1.9% | 2.76 |
| bam_fREML | 2 | 0.1339 | 0.1354 | 0.1350 | 2.1% | 3.50 |
| bam_ML | 2 | 0.1325 | 0.1350 | 0.1339 | 2.2% | 3.26 |
| bam_REML | 2 | 0.1339 | 0.1360 | 0.1349 | 1.9% | 3.41 |
| linked_bam_ML | 1 | 0.1460 | 0.1464 | 0.1459 | 0.4% | 2.85 |
| partly_fixed_bam_ML | 1 | 0.1274 | 0.1297 | 0.1288 | 2.0% | 2.57 |
| linked_gam | 1 | 0.1617 | 0.1620 | 0.1615 | 0.3% | 2.64 |
| linked_bam | 1 | 0.1461 | 0.1464 | 0.1460 | 0.4% | 2.44 |
| partly_fixed | 1 | 0.1430 | 0.1451 | 0.1440 | 1.5% | 2.78 |
| tensor | 2 | 0.1076 | 0.1124 | 0.1103 | 5.1% | 3.19 |

For the linked gam coefficient s(x).4, independent numerical integration gives mean 0.147033 and variance 0.079669; mixture conditional moments give 0.147032 and 0.079669, and sampled coefficients give 0.146890 and 0.079885.

Across supported fits, the largest relative difference between empirical coefficient covariance and the total-variance calculation was 2.7%. This difference includes coefficient-sampling Monte Carlo error.

## Cached conditional updates

The design matrix is evaluated in bounded row blocks once. Weighted crossproducts, penalty blocks, smoothing mappings, offsets, and the fitted dispersion are cached; each subsequent update solves a coefficient-sized precision system. It does not refit the model or re-evaluate model-call data or weight expressions.

For 100 identical log-smoothing draws, cached conditional means took 0.003 seconds versus 1.392 seconds for fixed-sp refits; the largest coefficient difference was 2.4e-13. These are single-run local timings, not a general speed guarantee.

## Remaining restrictions

The prototype remains internal and supports identifiable independent Gaussian identity gam and non-discrete bam fits with ML, REML, or fREML covariance information. BAM fits with nonzero prescribed smooth contributions, additional penalties, minimum smoothing parameters, edge-corrected smoothing covariances, prefit objects, GAMM components, dependent observations, nonpositive penalty multipliers, and unsupported mappings are rejected. Numerically invalid smoothing draws fail explicitly rather than being silently discarded or clipped.

Discrete bam is audited but not sampled: its compressed fitting design can define a different conditional target from evaluation at the original covariates. Reconstructing the compressed design and covariance consistently is required before extending this prototype. Non-Gaussian models need conditional modes and covariance updates, including family-parameter handling, beyond the Gaussian crossproduct solution.

The log-smoothing covariance is a fitted approximation. Boundary fits and very diffuse approximations can require more careful treatment. Dispersion uncertainty and its dependence with smoothing parameters remain separate work; proposal covariance changes in conditional MH still do not integrate smoothing uncertainty.

See [smoothing-methods.md](smoothing-methods.md) for the ML/REML/fREML fitting-method audit and family-parameter ordering. These internal draws propagate approximate estimation uncertainty; they do not perform Bayesian integration.

Reproduce with: Rscript notes/posterior-chains/smoothing-study.R --load-all 20000

Session details:

```
R version 4.6.1 (2026-06-24)
Platform: aarch64-apple-darwin23
Running under: macOS Tahoe 26.6.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] C.UTF-8/C.UTF-8/C.UTF-8/C/C.UTF-8/C.UTF-8

time zone: Europe/Copenhagen
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
[1] mgcv_1.9-4       nlme_3.1-171     gratia_0.11.2-50 testthat_3.3.2

loaded via a namespace (and not attached):
 [1] Matrix_1.7-6       mvnfast_0.2.8      gtable_0.3.6       dplyr_1.2.1
 [5] compiler_4.6.1     brio_1.1.5         tidyselect_1.2.1   Rcpp_1.1.2
 [9] stringr_1.6.0      parallel_4.6.1     tidyr_1.3.2        splines_4.6.1
[13] scales_1.4.0       lattice_0.23-1     ggplot2_4.0.3      R6_2.6.1
[17] generics_0.1.4     patchwork_1.3.2    tibble_3.3.1       desc_1.4.3
[21] rprojroot_2.1.1    pillar_1.11.1      RColorBrewer_1.1-3 rlang_1.3.0
[25] stringi_1.8.9      S7_0.2.2           otel_0.2.0         pkgload_1.5.3
[29] cli_3.6.6          withr_3.0.3        magrittr_2.0.5     grid_4.6.1
[33] lifecycle_1.0.5    ggokabeito_0.1.0   vctrs_0.7.3        KernSmooth_2.23-27
[37] glue_1.8.1         farver_2.1.2       pkgbuild_1.4.8     purrr_1.2.2
[41] tools_4.6.1        pkgconfig_2.0.3
```
