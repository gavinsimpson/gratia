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
and simulations including their variation. Diagnostics use the fitted
GAM component, whose working residuals reflect the conditional
mixed-model fit; they are not a substitute for mixed-model diagnostics.

Gaussian draws use the fitted GAM coefficient covariance matrix,
including its uncertainty due to fitting the mixed model. User draws
must have one column per GAM coefficient. Metropolis-Hastings sampling
is unsupported: the GAM likelihood does not include the additional
random effects. Smoothness uncertainty-corrected covariance matrices and
alternative EDF are unavailable from gamm4; requested corrections warn
and fall back to the available covariance matrix or EDF. Variance
components are extracted from the mixed-model component, with smooth
labels and random-effect term names (separate penalties on a tensor
smooth retain their grouping suffix); interval bounds are `NA` because
they are not supplied by the fit.
