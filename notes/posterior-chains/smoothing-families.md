# Ordinary and extended-family smoothing uncertainty

Investigated with R 4.6.1 and mgcv 1.9.4. This work remains internal.

127 of 130 family/fitter/method combinations passed conditional reconstruction, a perturbed-smoothing update and 25 mixture draws. See [the complete results](smoothing-families.csv) for parameter positions, native warnings, failures and timings.

Across passing fits, the largest relative reconstruction errors were 2.09e-05 for coefficients and 4.97e-06 for covariance. Each fit uses 320 observations (600 for ziP), offsets and unequal weights except the ziP example. Fits use epsilon = 1e-8 and maxit = 100; default BAM tolerances did not reconstruct the beta-regression ML fit accurately enough. These are individual local timings, not a repeated-simulation calibration study.

## Conditional calculations

The Gaussian identity path retains its cached exact conditional distribution. Other ordinary and extended families use mgcv's native normal approximation for coefficients at each sampled smoothing value. Setup caches the original fitted design as a matrix term and the original penalty blocks, preserving coefficient coordinates, unpenalised directions and smoothing mappings. It does not reconstruct data expressions, knots or constraints from the fitting call. The native iteratively reweighted fit updates coefficients and their covariance, with dispersion and family parameters fixed at their fitted values.

Raw model-frame responses and weights are essential. Passing initialised binomial responses and prior weights back through family initialisation would multiply trial counts twice. Censoring limits, grouped-family labels, factors, retained fitting rows and fitting offsets are carried into the conditional setup. Copies of family environments prevent updates to fitted objects. For grouped families, initialisation can propose new cutpoints; these are replaced by the original fitted parameters before coefficient fitting.

Some native family callbacks resolve helpers through the search path. The copied environments receive missing mgcv helper functions so sampling does not require attaching mgcv. This leaves the original fitted family's environments unchanged. A native initialiser that replaces the family object is rejected explicitly.

Ordinary fixed-theta negbin fits can have nonunit fitted dispersion in bam, while gam forces unit dispersion. The native mode is unchanged; its coefficient covariance is rescaled to the original fitted dispersion. The reconstruction check independently verifies both quantities.

## Families and covariance ordering

The inventory covers ordinary Gaussian, Poisson, binomial, Gamma, inverse Gaussian, fixed-theta negative binomial and fixed-power Tweedie families; general quasi, quasi-Poisson and quasi-binomial; and non-default Gaussian, Poisson and binomial links. Binomial tests include proportions with trial weights, success/failure matrices and factors. Quasi families supply an approximate sampling distribution based on quasi-likelihood, rather than a likelihood posterior.

All installed extended-family constructors are exercised: betar, cnorm, clog, cpois, bcg, nb, ocat, scat, tw and ziP, plus gfam with ordinary and extended components. The censoring examples include exact, left, right and interval observations. Grouped-family examples include a Gaussian scale and either negative-binomial theta or ordinal cutpoints.

| Family | Fitted nuisance parameters retained during every update |
| --- | --- |
| betar | Beta precision |
| cnorm, clog | Normal standard deviation or logistic scale, stored on the log scale |
| cpois | None |
| bcg | Box-Cox power and log standard deviation |
| nb | Negative-binomial size, stored on the log scale |
| ocat | Log cutpoint increments; first cutpoint fixed at -1 |
| scat | Transformed Student degrees of freedom and scale |
| tw | Transformed Tweedie power, plus separately fitted global dispersion |
| ziP | Intercept and transformed slope linking presence to log Poisson intensity |
| gfam | Component family parameters and any component dispersions |

For gam, estimated family parameters precede log smoothing parameters in V.sp and its mapping. A separate log dispersion follows where estimated. Non-discrete bam updates family parameters separately: they are absent from V.sp, sp.vcov and db.drho. Its fREML V.sp also omits separate dispersion. Family-fixed scale can override bam's scale.estimated flag: for extended families a separate dispersion is present only when family$scale is negative. Component scales in gfam belong to its family-parameter vector. The audit records these distinctions. For gam, the free log-smoothing block is marginal within the retained joint covariance. BAM has no joint family-parameter block from which to recover that dependence; its retained smoothing covariance is used as supplied. Neither fitter generates joint nuisance-parameter draws, and equivalence of these covariance approximations is not assumed.

## Restrictions and validation

The censored-Poisson bam examples fail the reconstruction check: native conditional refits at the fitted smoothing values do not reproduce the fitted coefficients. [Additional probes](smoothing-family-rejections.csv) with vector responses, unit weights, absent offsets and tighter fitting tolerances also failed. This configuration remains rejected; no different mode or covariance is silently substituted. See the CSV for native convergence warnings. Censored-Poisson gam fits pass the same independent conditional-refit tests. An [independent uncensored Poisson check](smoothing-cpois-check.txt) gives a maximum absolute penalised-objective gradient of 193 at the BAM coefficients versus 6.8e-8 at a fixed-smoothing ordinary Poisson reference. The objective falls from 357.2 to 315.4. Thus the disagreement is also present without the conditional reconstruction code; accepting the original covariance without checking the fitted mode would conceal it. Reproduce with Rscript notes/posterior-chains/smoothing-cpois-check.R --load-all.

General families with several linear predictors remain outside this extension. Only actual ML, REML and fREML fits with identifiable covariance ordering are accepted. NCV and QNCV remain excluded. Discrete bam, additional penalties, dependent observations, minimum smoothing parameters, edge-corrected covariance and unsupported fitting constraints retain their existing errors. Non-Gaussian updates cache a dense design, with a default limit of 20 million entries; the internal dense_limit control can change that limit. Gaussian identity updates still use bounded row blocks and cached crossproducts.

Very diffuse normal approximations can produce numerically impossible smoothing draws or failed conditional fits. Such draws stop with an indexed error; they are not clipped, discarded or replaced. Every accepted native conditional must converge and have finite coefficients and positive definite covariance. Setup also checks the coefficients and covariance at the fitted smoothing values before generating any draws. If that check fails, the error reports both discrepancies and suggests tighter fitting tolerances; it never relaxes agreement automatically.

Tests compare changed-smoothing predictions and their joint covariance against independent fixed-parameter fits, accounting for different coefficient bases in gam and bam. They cover shared and partly fixed smoothing parameters, tensor products, ML/REML/fREML ordering, weights, offsets, missing rows, family state, deterministic draws and zero log-smoothing covariance. Existing exact Gaussian tests and seeded results remain unchanged.

This mixture propagates an approximate distribution of smoothing estimates through conditional normal coefficient distributions. Coefficient distributions are recomputed at sampled smoothing values with the original fitted dispersion and family parameters. No joint nuisance-parameter draws are made. It is approximate estimation-uncertainty propagation, not Bayesian integration. The next statistical step is repeated-simulation validation of intervals against conditional draws and mgcv's unconditional correction, including difficult and weakly identified fits. Public integration should follow that assessment.

Reproduce: Rscript notes/posterior-chains/smoothing-families.R --load-all
