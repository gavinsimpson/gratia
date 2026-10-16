# MH covariance adjustment: comparison protocol

This is an internal experiment on gls/posterior-chains. It adds no exported
function, public method or new interpretation of MH acceptance probabilities.

## Adjustment

The complete retained coefficient vector must be in fitted coefficient order.
Compute its pooled mean m and empirical covariance C over all chains. If
C = R_c' R_c and Vc = R_u' R_u are upper Cholesky decompositions, transform
row draws as (beta - m) R_c^{-1} R_u + m. No random numbers are generated.
The empirical covariance after transformation is Vc, and the mean is unchanged.

Use the actual fitted Vc, without fallback to Vp or a newly constructed
smoothing covariance. Reject incomplete, nonfinite, singular or numerically
ill-conditioned inputs. Use one common transformation for every chain.
Acceptance rates refer to the original conditional run. Preserve its diagnostics
in the audit and recalculate diagnostics for the transformed coefficients when
requested. Chain identifiers and globally unique draw identifiers are unchanged.

## Repeated datasets

Reuse the earlier coverage study's deterministic responses, covariates, weights,
offsets, knots, truths and replicate numbers 1:200. Do not regenerate responses
with new seeds or select datasets based on interval coverage.

Cases are normal_known (strong smooth, known dispersion), normal_weak,
normal_null and binomial_rare. Fit gam by REML and non-discrete bam by fREML.
The Gaussian weak and null cases estimate dispersion. The Bernoulli case has
300 observations and a baseline log odds of -3 before signal and offset.

Compare conditional MH, its covariance-adjusted draws, and unconditional
Gaussian draws. Include analytic conditional Gaussian intervals as a control.
Each simulated method retains 16,000 draws over four chains. MH applies 4,000
burn-in iterations per chain, thin = 1, the specialised gratia backend, the
default t degrees of freedom (40) and random-walk scale (.25). Gaussian draws
use the mgcv generator and Vc. Global dispersion and additional family
parameters are fixed. Separate seeds are used for Gaussian and MH draws;
adjusted draws are a deterministic transformation of the same MH sample.

The earlier two-replicate pilot checks execution and mixing. It is not included
as an additional simulation replication. After the main run, compare the first
five datasets in every case at 64,000 retained draws and 16,000 burn-in iterations
per chain. This is a Monte Carlo resolution check, not a further coverage study.

## Outcomes and denominators

At the same 11 evaluation points as before, calculate pointwise 95% equal-tail
intervals for the linear predictor, the centred first smooth, its derivative
and the expected response. Derivative means the smooth derivative on the linear
predictor scale; it excludes the known offset derivative. It is not a response
derivative. Expected-response intervals use the complete predictor and offset,
with the identity or inverse-logit transformation. Response noise is not added.

Report coverage, average width and paired differences separately for interior
points, endpoints and all points. Average each outcome within a dataset first;
Monte Carlo standard errors treat independent datasets, not points, as units.
Record failures and native warnings by stage. A fit-level failure stops the
comparison for that fit; the original stage and attempted denominator remain
reported. No failed draw is discarded, replaced or clipped.

Report coefficient and derived-quantity rank-normalised R-hat and bulk/tail ESS.
The descriptive screen requires R-hat < 1.01 and bulk and tail ESS >= 400 for
all recorded coefficients and derived quantities in both original and adjusted
MH. Report coverage both for all returned fits and the same matched subset
passing this screen; do not drop poorly mixing runs without accounting for them.
Gaussian diagnostics are also recorded.

## Limitations to assess

The covariance match is algebraic, not evidence of correct posterior coverage.
The target covariance is itself an approximation. Adjusting empirical covariance
adds Monte Carlo dependence between all draws. More draws do not address
smoothing bias, missing changes in posterior means, or incorrect tail shape.
Cholesky transformation depends on fitted coefficient order; other transports
with the same covariance can give different nonnormal distributions. The map
can shrink directions where MH variance already exceeds Vc. A pooled map
preserves differences between chain means but need not preserve each
coefficient's R-hat or ESS. Diagnostics must therefore be retained and recomputed.

Quasi families, LSS families, discrete bam, ML fits and joint nuisance-parameter
uncertainty are outside this comparison. Their absence is a study scope choice,
not a claim that every such fit lacks Vc. Availability of Vc and a valid
conditional MH target is required before applying the adjustment elsewhere.


## Separate diagnostic follow-up

After the complete primary comparison, rare-binary GAM dataset 73 had tail ESS
below 400. A post hoc run retained 64,000 draws and 16,000 burn-in iterations
per chain for both GAM and BAM on that dataset. It is recorded separately,
with a resumable follow-up script. The primary intervals, diagnostic exclusions
and coverage denominators are retained unchanged.
