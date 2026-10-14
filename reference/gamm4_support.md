# Support for models fitted by gamm4

Models fitted by
[`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html) (version
0.2-7 or later) have class `"gamm4"`. Pass the fitted object directly to
gratia methods; historical unclassed lists are not supported.

## Details

Smooths, parametric effects, predictions, derivatives, simulations, and
posterior draws describe the population GAM component. Terms supplied
through `random` are excluded, including their realized effects,
predictive variation, and uncertainty.
[`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
and
[`conditional_differences()`](https://gavinsimpson.github.io/gratia/reference/conditional_differences.md)
condition on covariate values, not on realized group effects. Group
variables occurring only in `random` are not required in prediction
data, so new groups receive the same population predictions. Use the
`mer` component with lme4 for predictions conditional on group effects
and simulations including their variation. Diagnostics and response
simulations use the full fitted mixed model by default. With
`conditioning = "conditional"`, all fitted smooth and additional random
effects are held fixed for response references. `"marginal"` draws new
effects from their fitted distribution. `"gam_component"` explicitly
selects the extracted GAM calculations used by earlier versions.
Marginal GLMM checks average over random effects using lme4's joint
response simulations. Residuals then use ranks in a shared reference
sample, as described in
[diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).
Gaussian identity-link models instead use analytic CDFs of the marginal
response distribution.

Posterior response draws also default to the full conditional model.
They draw fixed and fitted random effects together, keeping covariance
parameters at their fitted values. These Gaussian approximations
currently use fitting observations only. To supply user coefficient
draws or prediction data to
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
select `conditioning = "gam_component"` explicitly.

Coefficient draws from
[`post_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md),
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
[`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
and
[`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
still use the fitted GAM covariance matrix, including its uncertainty
due to fitting the mixed model. User draws must have one column per GAM
coefficient. Metropolis-Hastings sampling is unsupported: the GAM
likelihood does not include the additional random effects. Smoothness
uncertainty-corrected covariance matrices and alternative EDF are
unavailable from gamm4; requested corrections warn and fall back to the
available covariance matrix or EDF. Variance components are extracted
from the mixed-model component, with smooth labels and random-effect
term names (separate penalties on a tensor smooth retain their grouping
suffix); interval bounds are `NA` because they are not supplied by the
fit.
