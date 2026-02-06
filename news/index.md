# Changelog

## gratia (development version)

### User visible changes

- *gratia* now only handles shape constrained models fitted using *scam*
  version 1.2-21 or later. This is due to breaking changes in *scam*,
  and new functionality in that package, which simplifies *gratia*.

- *gratia* also now depends on *dplyr* version 1.2.0 or later.

### New features

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now works for constrained factor smooths (`bs = "sz"`), treating them
  in the same way as random factor smooths (`bs = "fs"`). Lack of
  support reported by [@vrest-png](https://github.com/vrest-png)
  [\#362](https://github.com/gavinsimpson/gratia/issues/362)

  Also handles higher order `bs = "sz"` terms.

- [`draw.derivatives()`](https://gavinsimpson.github.io/gratia/reference/draw.derivatives.md)
  can now differentiate random and constrained factor smooths through
  the colour aesthetic via new argument `differentiate_factor_smooths`.

- [`model_terms()`](https://gavinsimpson.github.io/gratia/reference/model_terms.md)
  returns the names of all terms in a model.
  [\#388](https://github.com/gavinsimpson/gratia/issues/388)

### Bug fixes

- [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
  would fail if supplied a numeric vector of data to condition on via
  the `condition` argument.
  [\#366](https://github.com/gavinsimpson/gratia/issues/366)

- *gratia* worked inconsistently with tensor product smooths containing
  a random effect `bs = "re"` marginal. Reported
  [\#358](https://github.com/gavinsimpson/gratia/issues/358) and fixed
  by [@asgersvenning](https://github.com/asgersvenning)
  [\#360](https://github.com/gavinsimpson/gratia/issues/360), with
  additional changes by Gavin Simpson.

- Argument `mvn_method` was not being passed on to the lower workhorse
  functions that did the sampling.
  [\#381](https://github.com/gavinsimpson/gratia/issues/381)

## gratia 0.11.1

CRAN release: 2025-08-25

### User visible changes

- [`residuals_hist_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_hist_plot.md)
  and hence
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  now centre the middle bin of the histogram at 0. In part this was due
  to *ggplot2*’s new binning algorithm leading to potentially odd
  choices for the bin breaks in the context of model residuals.

### New features

- [`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md)
  now supports more of *mgcv*’s families:
  1.  [`scat()`](https://rdrr.io/pkg/mgcv/man/scat.html),
  2.  [`nb()`](https://rdrr.io/pkg/mgcv/man/negbin.html)
  3.  [`betar()`](https://rdrr.io/pkg/mgcv/man/Beta.html),
  4.  [`tw()`](https://rdrr.io/pkg/mgcv/man/Tweedie.html).
- Several user friendliness improvements in
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md):
  - now better handles the case where there are multiple smooths for
    which partial derivatives are required,
  - correctly identifies smooths that involve random effect terms
    (i.e. any smooth or tensor product marginal smooths with
    `bs %in% c("re", "fs")`) and ignores them,
  - identifies and ignores univariate smooths, and
  - displays more informative error messages to explain what was wrong.

  Part of discussion with [@BenFN121](https://github.com/BenFN121) in
  [\#356](https://github.com/gavinsimpson/gratia/issues/356).

### Bug fixes

- [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  threw an error when the `select` argument was used.
  [\#356](https://github.com/gavinsimpson/gratia/issues/356) reported by
  [@BenFN121](https://github.com/BenFN121)

- [`draw.conditional_values()`](https://gavinsimpson.github.io/gratia/reference/draw.conditional_values.md)
  was setting a label on the fill aesthetic even if that aesthetic was
  not being used. In *ggplot2* v4.0.0 this resulted in a warning, which
  is fixed.

## gratia 0.11.0

CRAN release: 2025-08-18

*gratia* now has a [paper](https://doi.org/10.21105/joss.06962)
describing the package in the [Journal of Open Source
Software](https://joss.theoj.org/). If you use *gratia* in your work,
please cite this paper rather than the generic citation previously
created by [`citation()`](https://rdrr.io/r/utils/citation.html):

> Simpson, G.L. (2024) gratia: An R package for exploring generalized
> additive models. *The Journal of Open Source Software* **9**, 6962.

### User visible changes

- Experimental support for computing with parallel processes via the
  *purrr* 📦 in
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  and
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md),
  though it is of limited use in the latter. Parallel processing will
  operate if a call to
  [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
  has been issued in the current R session.

- Rug plots in
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now only show the unique data values to avoid needless overplotting.
  This behaviour is controlled via new argument `distinct_rug`. Setting
  this to `FALSE` will get the previous behaviour.

- [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  gains a [`print()`](https://rdrr.io/r/base/print.html) method and thus
  no longer prints its attributes.

- `citation("gratia")` now suggests to use Simpson (2024) *Journal of
  Open Source Software* **9**(104), 9862, when citing the package.

- [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  now returns a data frame, bringing it into line with
  `stats::simulate.lm()`.

- With soap film smooths,
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  no longer plots points that are deemed outside the boundary of the
  soap film (as determined by
  [`mgcv::inSide()`](https://rdrr.io/pkg/mgcv/man/inSide.html)).
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  continues to return these points however, because it might be
  confusing to suddenly lose evaluation points. This behaviour can be
  controlled by the argument `clip`, which is available to
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`smooth_estimates.gam()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  currently.

- [`draw.compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/draw.compare_smooths.md)
  now uses “Partial effect” as the y-axis label, in common with other
  partial effect plots in *gratia*.

### New features

- [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
  is a new generic function for assembling sets of plots from model
  objects. It is effectively
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  but without actually plotting on the current device. Instead it
  returns a list of `ggplot` objects. Currently, a method for `"gam"`
  models,
  [`assemble.gam()`](https://gavinsimpson.github.io/gratia/reference/assemble.gam.md),
  is provided which
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  uses. Part of the wish of [@ha0ye](https://github.com/ha0ye)
  [\#35](https://github.com/gavinsimpson/gratia/issues/35)

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  with `method = "simulate"` can now handle *mgcv*’s multivariate normal
  models fitted with `family = mvn()`and Tweedie location scale models
  fitted with `family = twlss()`.

- `fix_family_rd()` is an (currently) internal function that extends
  [`mgcv::fix.family.rd()`](https://rdrr.io/pkg/mgcv/man/fix.family.link.html).
  This function adds a `rfoo()` function to the `$rd` component of the
  [`family()`](https://rdrr.io/r/stats/family.html) object stored in the
  model. This function is used to generate new values of the response at
  supplied values of the linear predictor. `fix_family_rd()` calls
  [`mgcv::fix.family.rd()`](https://rdrr.io/pkg/mgcv/man/fix.family.link.html)
  so it is backward compatible with *mgcv*’s behaviour, but it is
  designed to extend the behaviour through support for a richer set of
  models.

  Currently, `fix_family_rd()` adds support for multivariate normal
  models fitted with `family = mvn()` and location-scale Tweedie models
  fitted with `family = twlss()`, but the latter is currently slow.

  The functionality of `fix_family_rd()` may move to another package or
  even to *mgcv* if suitable.

- [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  can now be used to compare named smooths using
  `select = c("s(x):f1", "s(x):f2")` etc, where the vector of smooth
  names must match exactly something returned by
  [`smooths()`](https://gavinsimpson.github.io/gratia/reference/smooths.md).
  Smooths to be compared must have the same covariate (`x` in the
  example) and same factor-by variable (`f` in the example). Tensor
  product factor-by smooths are handled likewise. This effectively
  handles the problem of
  [\#315](https://github.com/gavinsimpson/gratia/issues/315), reported
  by [@3rd3](https://github.com/3rd3).

  Additionally,
  [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  with `group_means = TRUE` can now include group means specified using
  a random effect smooth, e.g. following the above example for `select`,
  a random effect smooth `s(f, bs = "re")`, with name `s(f)`, is looked
  for in the model matrix and its effects included in the comparisons.

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  gains argument `.observed_only` which allows simple filtering of the
  combinations of values specified in the slice to those that are in the
  data frame supplied or in the model frame of a fitted GAM.

- [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md),
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  and
  [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  now simulate for all linear predictors (i.e., all responses) in
  [`mvn()`](https://rdrr.io/pkg/mgcv/man/mvn.html) and
  [`multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html) models as
  these are all location parameter linear predictors.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now works for GAMs fitted with families
  [`ziP()`](https://rdrr.io/pkg/mgcv/man/ziP.html),
  [`mvn()`](https://rdrr.io/pkg/mgcv/man/mvn.html), and
  [`multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html). The
  non-working of `fitted-values()` for
  [`ziP()`](https://rdrr.io/pkg/mgcv/man/ziP.html) models was reported
  in [\#341](https://github.com/gavinsimpson/gratia/issues/341) by
  [@rroyaute](https://github.com/rroyaute)

- [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md),
  `derivatives_samples()`, and
  [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
  now all work with models fitted by
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html).

- [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  can now take a vector of coefficients for the basis functions when
  using the default method:
  `basis(s(x), data = df, coefficients = rnorm(10))`. Wish of
  [\#136](https://github.com/gavinsimpson/gratia/issues/136)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  can now handle soap film smooths with known boundary values, and with
  nested boundaries.

- [`scale_fill_partial_effect()`](https://gavinsimpson.github.io/gratia/reference/scale_fill_partial_effect.md)
  is a *ggplot* scale function that implements the default diverging
  red-blue gradient used by *gratia* when plotting partial effect
  surfaces for multivariate smooths.

- [`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md)
  computes probability integral transform (PIT) and randomised quantile
  residuals for some GAMs and GLMs. Currently only models fitted with a
  [`gaussian()`](https://rdrr.io/r/stats/family.html),
  [`binomial()`](https://rdrr.io/r/stats/family.html),
  [`Gamma()`](https://rdrr.io/r/stats/family.html), or
  [`poisson()`](https://rdrr.io/r/stats/family.html) family are
  supported.

- [`residuals_linpred_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_linpred_plot.md)
  and
  [`residuals_hist_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_hist_plot.md)
  can now use PIT or randomised quantile residuals in their diagnostic
  plots.

### Bug fixes

- [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
  would fail if a variable it was conditioning on was also a name of a
  function. [\#323](https://github.com/gavinsimpson/gratia/issues/323)
  (but not really fixed when this was initially closed)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  was only keeping one set of factor levels when there were multiple
  parametric factor terms in a model. Reported by
  [@tamas-ferenci](https://github.com/tamas-ferenci) as part of
  discussion to
  [\#284](https://github.com/gavinsimpson/gratia/issues/284)

- [`mh_draws()`](https://gavinsimpson.github.io/gratia/reference/mh_draws.md)
  would fail in the case of drawing only `n = 1` samples.
  [\#328](https://github.com/gavinsimpson/gratia/issues/328) Reported by
  [@zsusswein](https://github.com/zsusswein)

- [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  now works for the [`ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html)
  family, which has a non-standard `rd` function in its
  [`family()`](https://rdrr.io/r/stats/family.html) object. Reported by
  [@hhp94](https://github.com/hhp94),
  [\#319](https://github.com/gavinsimpson/gratia/issues/319)

- [`gaussian_draws()`](https://gavinsimpson.github.io/gratia/reference/gaussian_draws.md)
  would fail in some cases with *gamm4* models because the coefficients
  were returned as a vector, not the expected matrix.
  [\#332](https://github.com/gavinsimpson/gratia/issues/332) reported by
  [@JaredStufft-Elion](https://github.com/JaredStufft-Elion)

- [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  would generate very many warnings when plotting spline on the sphere
  smoothers (`bs = "sos"`) because
  [`geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  was creating tiles that extended beyond valid geographical coordinates
  for the tile centre coordinates that it was being provided. The
  function that creates the data to evaluate the spline at now tries to
  avoid this. Note this fix is not a complete solution.
  [\#334](https://github.com/gavinsimpson/gratia/issues/334) Reported by
  [@StefanoMezzini](https://github.com/StefanoMezzini)

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  failed to extract the link an inverse link functions for
  [`mvn()`](https://rdrr.io/pkg/mgcv/man/mvn.html) and
  [`multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html) models.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  now work correctly for models fitted with the
  [`ziP()`](https://rdrr.io/pkg/mgcv/man/ziP.html) family.
  [\#341](https://github.com/gavinsimpson/gratia/issues/341) Reported by
  [@rroyaute](https://github.com/rroyaute)

- [`basis.scam()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  was incorrectly trying to identify the smooths named in `select`.

- [`basis.scam()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  would fail if the SCAM contained any non-constrained (i.e.  ordinary)
  smooths.

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  was displaying more significant digits in the p values than it was
  accurate to in terms of reporting p values as “\<0.001”. A new
  argument `digits` controls how many digits are used when formatting p
  values, with default of `3`.

- [`theta()`](https://gavinsimpson.github.io/gratia/reference/theta.md)
  now works for models fitted with the
  [`gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html) family. Previously
  it would fail because it assumes all `family()$getTheta` functions had
  an argument `trans` to transform the theta values to the correct
  scale.

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  was not showing the model intercept, and was displaying slightly
  different results from those shown by
  [`summary.gam()`](https://rdrr.io/pkg/mgcv/man/summary.gam.html).

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  was not adding any known boundary condition that was part of a soap
  film smooth.

- [`draw.gamlss()`](https://gavinsimpson.github.io/gratia/reference/draw.gamlss.md)
  (for [`GJRM::gamlss()`](https://rdrr.io/pkg/GJRM/man/gamlss.html)
  models) now doesn’t fail with an error if there is more than a single
  smooth in any of the linear predictors.

## gratia 0.10.0

CRAN release: 2024-12-19

### New features

- [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
  and its
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method compute and plot predictions from a fitted GAM that are
  conditional on one or more covariates. The function is a wrapper
  around
  [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  but allows the user simple ways to specify which covariates to
  condition on and at what values those covariates should take. It
  provides similar functionality to
  [`marginaleffects::plot_predictions()`](https://marginaleffects.com/man/r/plot_predictions.html),
  but is simpler. See
  [\#300](https://github.com/gavinsimpson/gratia/issues/300).

- [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  and
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  can now allow the smooth to be reparameterized such that the resulting
  basis has an identity matrix. This more clearly highlights the penalty
  null space, the functions that the penalty has no effect on.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  gain argument `caption`, which, if set to `FALSE` will not plot the
  smooth basis type as a caption on the plot.
  [\#307](https://github.com/gavinsimpson/gratia/issues/307)

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  and
  [`qq_plot.gam()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  now allow the user to set a random seed that is used when generating
  reference quantiles with `method = "uniform"` or
  `method = "simulate"`.

### Bug fixes

- [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
  was ignoring the `scale` argument.
  [\#293](https://github.com/gavinsimpson/gratia/issues/293) Reported by
  [@jonathonmellor](https://github.com/jonathonmellor)

- Argument `level` to
  [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
  was included accidentally. As of v0.9.2.9002 this argument is
  deprecated and using it will now generate a warning.
  [\#291](https://github.com/gavinsimpson/gratia/issues/291)

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  was not plotting cyclic P spline smooths. Reported by
  [@Zuckerbrot](https://github.com/Zuckerbrot)
  [\#297](https://github.com/gavinsimpson/gratia/issues/297)

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  would fail for `"fs"` smooths with other parametric effects in the
  model. Reported by [@mahonmb](https://github.com/mahonmb)
  [\#301](https://github.com/gavinsimpson/gratia/issues/301)

- Partial residuals in
  [`partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/partial_residuals.md)
  and
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  were wrong for GAMs fitted with `family = binomial()` where the
  `weights` argument contained the binomial sample sizes because the
  prior weights were being used to form weighted working residuals. Now
  working weights are used instead. Reported by
  [@emchuron](https://github.com/emchuron)
  [\#273](https://github.com/gavinsimpson/gratia/issues/273)

- Internal function `gammals_link()` was expecting `"theta"` as a
  synonym for the scale parameter but the master table has `"phi"` coded
  as the synonym. Now both work as expected.

- [`level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
  assumed that `level` would have only a single value even though it
  could handle multiple levels.
  [\#321](https://github.com/gavinsimpson/gratia/issues/321)

## gratia 0.9.2

CRAN release: 2024-06-25

### Breaking changes

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  slightly escaped the great renaming that happened for 0.9.0. Columns
  `type` and `term` did not gain a prefix `.`. This is now rectified and
  these two columns are now `.type` and `.term`.

### User visible changes

- Plots of random effects are now labelled with their smooth label.
  Previously, the title was taken fro the variable involved in the
  smooth, but this doesn’t work for terms like
  `s(subject, continuous_var, bs = "re")` for random slopes, which
  previously would have the title `"subject"`. Now such terms will have
  title `"s(subject,continuous_var)"`. Simple random intercept terms,
  `s(subject, bs = "re")`, are now titled `"s(subject)"`.
  [\#287](https://github.com/gavinsimpson/gratia/issues/287)

- The vignettes

  1.  `custom-plotting.Rmd`, and
  2.  `posterior-simulation.Rmd` were moved to `vignettes/articles` and
      thus are no longer available as package vignettes. Instead, they
      are accessible as Articles through the package website:
      <https://gavinsimpson.github.io/gratia/>

### New features

- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  now works for [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) models
  with multiple linear predictors, but currently only the location
  parameter is supported. The parameter is indicated through a new
  variable `.parameter` in the returned object.

### Bug fixes

- [`partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/partial_residuals.md)
  was computing partial residuals from the *deviance* residuals. For
  compatibility with
  [`mgcv::plot.gam()`](https://rdrr.io/pkg/mgcv/man/plot.gam.html),
  partial residuals are now computed from the *working* residuals.
  Reported by [@wStockhausen](https://github.com/wStockhausen)
  [\#273](https://github.com/gavinsimpson/gratia/issues/273)

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  was not passing the `ci_col` argument on
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  and
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md).
  Reported by Sate Ahmed.

- Couldn’t pass `mvn_method` on to posterior sampling functions from
  user facing functions
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md),
  [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md),
  and `repsonse_derivatives()`. Reported by
  [@stefgehrig](https://github.com/stefgehrig)
  [\#279](https://github.com/gavinsimpson/gratia/issues/279)

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  works again for quantile GAMs fitted by `qgam()`.

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  was not applying `shift` to the estimate and upper and lower interval.
  [\#280](https://github.com/gavinsimpson/gratia/issues/280) reported by
  [@TIMAVID](https://github.com/TIMAVID) &
  [@rbentham](https://github.com/rbentham)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  and
  [`draw.parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/draw.parametric_effects.md)
  would forget about the levels of factors (intentionally), but this
  would lead to problems with ordered factors where the ordering of
  levels was not preserved. Now,
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  returns a named list of factor levels as attribute `"factor_levels"`
  containing the required information and the order of levels is
  preserved when plotting.
  [\#284](https://github.com/gavinsimpson/gratia/issues/284) Reported by
  [@mhpob](https://github.com/mhpob)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  would fail if there were parametric terms in the model but they were
  all interaction terms (which we don’t currently handle).
  [\#282](https://github.com/gavinsimpson/gratia/issues/282)

## gratia 0.9.0

CRAN release: 2024-03-27

### Breaking changes

- Many functions now return objects with different named variables. In
  order to avoid clashes with variable names used in user’s models or
  data, a period (`.`) is now being used as a prefix for generated
  variable names. The functions whose names have changed are:
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
  [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md),
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md),
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md),
  and
  [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md).
  In addition,
  [`add_confint()`](https://gavinsimpson.github.io/gratia/reference/add_confint.md)
  also adds newly-named variables.

  ``` R
  1. `est` is now `.estimate`,
  2. `lower` and `upper` are now `.lower_ci` and `.upper_ci`,
  3. `draw` and `row` and now `.draw` and `.row` respectively,
  4. `fitted`, `se`, `crit` are now `.fitted`, `.se`, `.crit`, respectively
  5. `smooth`, `by`, and `type` in `smooth_estimates()` are now `.smooth`,
     `.by`, `.type`, respectively.
  ```

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  and
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  now work more like
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md);
  in place of the `var` and `data` columns, *gratia* now stores the data
  variables at which the derivatives were evaluated as columns in the
  object with their actual variable names.

- The way spline-on-the-sphere (SOS) smooths (`bs = "sos"`) are plotted
  has changed to use
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  instead of the previously-used
  [`ggplot2::coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html).
  This changed has been made as a result of
  [`coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
  being soft-deprecated (“superseded”) for a few minor versions of
  ggplot2 by now already, and changes to the guides system in version
  3.5.0 of ggplot2.

  The axes on plots created with
  [`coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
  never really worked correctly and changing the angle of the tick
  labels never worked. As
  [`coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
  is superseded, it didn’t receive the updates to the guides system and
  a side effect of these changes, the code that plotted SOS smooths was
  producing a warning with the release of ggplot2 version 3.5.0.

  The projection settings used to draw SOS smooths was previously
  controlled via arguments `projection` and `orientation`. These
  arguments do not affect
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
  Instead the projection used is controlled through new argument `crs`,
  which takes a PROJ string detailing the projection to use or an
  integer that refers to a known coordinate reference system (CRS). The
  default projection used is `+proj=ortho +lat_0=20 +lon_0=XX` where
  `XX` is the mean of the longitude coordinates of the data points.

#### Defunct and deprecated functions and arguments

##### Defunct

- [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  was deprecated in gratia version 0.7.0. This function and all it’s
  methods have been removed from the package. Use
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  instead.

##### Deprecated functions

The following functions were deprecated in version 0.9.0 of gratia. They
will eventually be removed from the package as part of a clean up ahead
of an eventual 1.0.0 release. These functions will become defunct by
version 0.11.0 or 1.0.0, whichever is released soonest.

- [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md)
  has been deprecated. Use
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  instead.

- [`datagen()`](https://gavinsimpson.github.io/gratia/reference/datagen.md)
  has been deprecated. It never really did what it was originally
  designed to do, and has been replaced by
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md).

##### Deprecated arguments

To make functions in the package more consistent, the arguments
`select`, `term`, and `smooth` are all used for the same thing and hence
the latter two have been deprecated in favour of `select`. If a
deprecated argument is used, a warning will be issued but the value
assigned to the argument will be assigned to `select` and the function
will continue.

### User visible changes

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  now uses a single call to the RNG to generate draws from the posterior
  of smooths. Previous to version 0.9.0,
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  would do a separate call to
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html) for
  each smooth. As a result, the result of a call to
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  on a model with multiple smooths will now produce different results to
  those generated previously. To regain the old behaviour, add
  `rng_per_smooth = TRUE` to the
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  call.

  Note, however, that using per-smooth RNG calls with `method = "mh"`
  will be very inefficient as, with that method, posterior draws for all
  coefficients in the model are sampled at once. So, only use
  `rng_per_smooth = TRUE` with `method = "gaussian"`.

- The output of
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  and its
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method have changed for tensor product smooths that involve one or
  more 2D marginal smooths. Now, if no covariate values are supplied via
  the `data` argument,
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  identifies if one of the marginals is a 2d surface and allows the
  covariates involved in that surface to vary fastest, ahead of terms in
  other marginals. This change has been made as it provides a better
  default when nothing is provided to `data`.

  This also affects
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md).

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now has some level of support for location, scale, shape families.
  Supported families are
  [`mgcv::gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html),
  [`mgcv::gammals()`](https://rdrr.io/pkg/mgcv/man/gammals.html),
  [`mgcv::gumbls()`](https://rdrr.io/pkg/mgcv/man/gumbls.html),
  [`mgcv::gevlss()`](https://rdrr.io/pkg/mgcv/man/gevlss.html),
  [`mgcv::shash()`](https://rdrr.io/pkg/mgcv/man/shash.html),
  [`mgcv::twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html), and
  [`mgcv::ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html).

- *gratia* now requires *dplyr* versions \>= 1.1.0 and *tidyselect* \>=
  1.2.0.

- A new vignette *Posterior Simulation* is available, which describes
  how to do posterior simulation from fitted GAMs using {gratia}.

### New features

- Soap film smooths using basis `bs = "so"` are now handled by
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md),
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  etc. [\#8](https://github.com/gavinsimpson/gratia/issues/8)

- [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
  is a new function for computing derivatives of the response with
  respect to a (continuous) focal variable. First or second order
  derivatives can be computed using forward, backward, or central finite
  differences. The uncertainty in the estimated derivative is determined
  using posterior sampling via
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  and hence can be derived from a Gaussian approximation to the
  posterior or using a Metropolis Hastings sampler (see below.)

- [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
  is the work horse function behind
  [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md),
  which computes and returns posterior draws of the derivatives of any
  additive combination of model terms. Requested by
  [@jonathanmellor](https://github.com/jonathanmellor)
  [\#237](https://github.com/gavinsimpson/gratia/issues/237)

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  can now simulate response data from gamma, Tweedie and ordered
  categorical distributions.

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  gains two new example models `"gwf2"`, simulating data only from Gu &
  Wahba’s *f2* function, and `"lwf6"`, example function 6 from Luo &
  Wahba (1997 JASA 92(437), 107-116).

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  can also simulate data for use with GAMs fitted using
  `family = gfam()` for grouped families where different types of data
  in the response are handled.
  [\#266](https://github.com/gavinsimpson/gratia/issues/266) and part of
  [\#265](https://github.com/gavinsimpson/gratia/issues/265)

- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  and
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  can now use the Metropolis Hastings sampler from
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html), instead
  of a Gaussian approximation, to sample from the posterior distribution
  of the model or specific smooths respectively.

- [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  is a new function in the family of
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  and
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md).
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  returns draws from the posterior distribution of the response,
  combining the uncertainty in the estimated expected value of the
  response and the dispersion of the response distribution. The
  difference between
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  and
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
  is that the latter only includes variation due to drawing samples from
  the conditional distribution of the response (the uncertainty in the
  expected values is ignored), while the former includes both sources of
  uncertainty.

- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  can new use a matrix of user-supplied posterior draws. Related to
  [\#120](https://github.com/gavinsimpson/gratia/issues/120)

- [`add_fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md),
  [`add_predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md),
  [`add_posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md),
  and
  [`add_smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md)
  are new utility functions that add the respective draws from the
  posterior distribution to an existing data object for the covariate
  values in that object: `obj |> add_posterior_draws(model)`.
  [\#50](https://github.com/gavinsimpson/gratia/issues/50)

- [`basis_size()`](https://gavinsimpson.github.io/gratia/reference/basis_size.md)
  is a new function to extract the basis dimension (number of basis
  functions) for smooths. Methods are available for objects that inherit
  from classes `"gam"`, `"gamm"`, and `"mgcv.smooth"` (for individual
  smooths).

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  gains a method for data frames and tibbles.

- [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  gains a method for data frames and tibbles.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now works with models fitted using the
  [`mgcv::ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) family. The
  predicted probability for each category is returned, alongside a Wald
  interval created using the standard error (SE) of the estimated
  probability. The SE and estimated probabilities are transformed to the
  logit (linear predictor) scale, a Wald credible interval is formed,
  which is then back-transformed to the response (probability) scale.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now works for GAMMs fitted using
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html). Fitted
  (predicted) values only use the GAM part of the model, and thus
  exclude the random effects.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  work for models fitted using the
  [`cnorm()`](https://rdrr.io/pkg/mgcv/man/cnorm.html) family.

- A worm plot can now be drawn in place of the QQ plot with
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  via new argument `use_worm = TRUE`.
  [\#62](https://github.com/gavinsimpson/gratia/issues/62)

- [`smooths()`](https://gavinsimpson.github.io/gratia/reference/smooths.md)
  now works for models fitted with
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html).

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  now returns the basis dimension for each smooth and gains an argument
  `stars` which if `TRUE` add significance stars to the output plus a
  legend is printed in the tibble footer. Part of wish of
  [@noamross](https://github.com/noamross)
  [\#214](https://github.com/gavinsimpson/gratia/issues/214)

- New
  [`add_constant()`](https://gavinsimpson.github.io/gratia/reference/add_constant.md)
  and
  [`transform_fun()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md)
  methods for
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md).

- [`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  gains arguments `lower` and `upper` to modify the lower and / or upper
  bound of the interval over which evenly spaced values will be
  generated.

- [`add_sizer()`](https://gavinsimpson.github.io/gratia/reference/add_sizer.md)
  is a new function to add information on whether the derivative of a
  smooth is significantly changing (where the credible interval excludes
  0). Currently, methods for
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  and
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  objects are implemented. Part of request of
  [@asanders11](https://github.com/asanders11)
  [\#117](https://github.com/gavinsimpson/gratia/issues/117)

- [`draw.derivatives()`](https://gavinsimpson.github.io/gratia/reference/draw.derivatives.md)
  gains arguments `add_change` and `change_type` to allow derivatives of
  smooths to be plotted with indicators where the credible interval on
  the derivative excludes 0. Options allow for periods of decrease or
  increase to be differentiated via `change_type = "sizer"` instead of
  the default `change_type = "change"`, which emphasises either type of
  change in the same way. Part of wish of
  [@asanders11](https://github.com/asanders11)
  [\#117](https://github.com/gavinsimpson/gratia/issues/117)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can now group factor by smooths for a given factor into a single
  panel, rather than plotting the smooths for each level in separate
  panels. This is achieved via new argument `grouped_by`. Requested by
  [@RPanczak](https://github.com/RPanczak)
  [\#89](https://github.com/gavinsimpson/gratia/issues/89)

  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  can now also group factor by smooths for a given factor into a single
  panel.

- The underlying plotting code used by `draw_smooth_estimates()` for
  most univariate smooths can now add change indicators to the plots of
  smooths if those change indicators are added to the object created by
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  using
  [`add_sizer()`](https://gavinsimpson.github.io/gratia/reference/add_sizer.md).
  See the example in
  [`?draw.smooth_estimates`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md).

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  can, when evaluating a 3D or 4D tensor product smooth, identify if one
  or more 2D smooths is a marginal of the tensor product. If users do
  not provide covariate values at which to evaluate the smooths,
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  will focus on the 2D marginal smooth (or the first if more than one is
  involved in the tensor product), instead of following the ordering of
  the terms in the definition of the tensor product.
  [\#191](https://github.com/gavinsimpson/gratia/issues/191)

  For example, in `te(z, x, y, bs = c(cr, ds), d = c(1, 2))`, the second
  marginal smooth is a 2D Duchon spline of covariates `x` and `y`.
  Previously,
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  would have generated `n` values each for `z` and `x` and `n_3d` values
  for `y`, and then evaluated the tensor product at all combinations of
  those generated values. This would ignore the structure implicit in
  the tensor product, where we are likely to want to know how the
  surface estimated by the Duchon spline of `x` and `y` smoothly varies
  with `z`. Previously
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  would generate surfaces of `z` and `x`, varying by `y`. Now,
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  correctly identifies that one of the marginal smooths of the tensor
  product is a 2D surface and will focus on that surface varying with
  the other terms in the tensor product.

  This improved behaviour is needed because in some
  [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) models it is not
  always possible to do the obvious thing and reorder the smooths when
  defining the tensor product to be
  `te(x, y, z, bs = c(ds, cr), d = c(2, 1))`. When `discrete = TRUE` is
  used with [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) the terms
  in the tensor product may get rearranged during model setup for
  maximum efficiency (See *Details* in
  [`?mgcv::bam`](https://rdrr.io/pkg/mgcv/man/bam.html)).

  Additionally,
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now also works the same way.

- New function
  [`null_deviance()`](https://gavinsimpson.github.io/gratia/reference/null_deviance.md)
  that extracts the null deviance of a fitted model.

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md),
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
  [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md),
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md),
  and
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  now all work for models fitted with
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html). Where it
  matters, current support extends only to univariate smooths.

- [`generate_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
  is a new low-level function for generating posterior draws from fitted
  model coefficients. `generate_daws()` is an S3 generic function so is
  extensible by users. Currently provides a simple interface to a simple
  Gaussian approximation sampler
  ([`gaussian_draws()`](https://gavinsimpson.github.io/gratia/reference/gaussian_draws.md))
  and the simple Metropolis Hastings sample
  ([`mh_draws()`](https://gavinsimpson.github.io/gratia/reference/mh_draws.md))
  available via
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).
  [\#211](https://github.com/gavinsimpson/gratia/issues/211)

- [`smooth_label()`](https://gavinsimpson.github.io/gratia/reference/smooth_label.md)
  is a new function for extracting the labels ‘mgcv’ creates for smooths
  from the smooth object itself.

- [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  has a default method that works with
  [`s()`](https://rdrr.io/pkg/mgcv/man/s.html),
  [`te()`](https://rdrr.io/pkg/mgcv/man/te.html),
  [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html), and
  [`ti()`](https://rdrr.io/pkg/mgcv/man/te.html), which create a smooth
  specification.

- [`transform_fun()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md)
  gains argument `constant` to allow for the addition of a constant
  value to objects (e.g. the estimate and confidence interval). This
  enables a single `obj |> transform_fun(fun = exp, constant = 5)`
  instead of separate calls to
  [`add_constant()`](https://gavinsimpson.github.io/gratia/reference/add_constant.md)
  and then
  [`transform_fun()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md).
  Part of the discussion of
  [\#79](https://github.com/gavinsimpson/gratia/issues/79)

- [`model_constant()`](https://gavinsimpson.github.io/gratia/reference/model_constant.md)
  is a new function that simply extracts the first coefficient from the
  estimated model.

### Bug fixes

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  and related family functions for the
  [`ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) weren’t correctly
  identifying the family name and as a result would throw an error even
  when passed an object of the correct family.

  [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  now work correctly for the
  [`betar()`](https://rdrr.io/pkg/mgcv/man/Beta.html) family in a fitted
  GAM.

- The [`print()`](https://rdrr.io/r/base/print.html) method for
  [`lp_matrix()`](https://gavinsimpson.github.io/gratia/reference/lp_matrix.md)
  now converts the matrix to a data frame before conversion to a tibble.
  This makes more sense as it results in more typical behaviour as the
  columns of the printed object are doubles.

- Constrained factor smooths (`bs = "sz"`) where the factor is not the
  first variable mentioned in the smooth (i.e. `s(x, f, bs = "sz")` for
  continuous `x` and factor `f`) are now plottable with
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md).
  [\#208](https://github.com/gavinsimpson/gratia/issues/208)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  was unable to handle special parametric terms like `poly(x)` or
  `log(x)` in formulas. Reported by [@fhui28](https://github.com/fhui28)
  [\#212](https://github.com/gavinsimpson/gratia/issues/212)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  now works better for location, scale, shape models. Reported by
  [@pboesu](https://github.com/pboesu)
  [\#45](https://github.com/gavinsimpson/gratia/issues/45)

- `parametric_effects` now works when there are missing values in one or
  more variables used in a fitted GAM.
  [\#219](https://github.com/gavinsimpson/gratia/issues/219)

- [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
  was incorrectly using `.data` with *tidyselect* selectors.

- [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  could not handle logical variables in a GAM fit as mgcv stores these
  as numerics in the `var.summary`. This affected
  [`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  and
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md).
  [\#222](https://github.com/gavinsimpson/gratia/issues/222)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  would fail when two or more ordered factors were in the model.
  Reported by [@dsmi31](https://github.com/dsmi31)
  [\#221](https://github.com/gavinsimpson/gratia/issues/221)

- Continuous by smooths were being evaluated with the median value of
  the `by` variable instead of a value of 1.
  [\#224](https://github.com/gavinsimpson/gratia/issues/224)

- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  (and hence
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md))
  now handles models with offset terms in the formula. Offset terms
  supplied via the `offset` argument are ignored by
  `mgcv:::predict.gam()` and hence are ignored also by `gratia`.
  Reported by [@jonathonmellor](https://github.com/jonathonmellor)
  [\#231](https://github.com/gavinsimpson/gratia/issues/231)
  [\#233](https://github.com/gavinsimpson/gratia/issues/233)

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  would fail on a `"fs"` smooth when a multivariate base smoother was
  used *and* the factor was not the last variable specified in the
  definition of the smooth:
  `s(x1, x2, f, bs = "fs", xt = list(bs = "ds"))` would work, but
  `s(f, x1, x2, bs = "fs", xt = list(bs = "ds"))` (or any ordering of
  variables that places the factor not last) would emit an obscure
  error. The ordering of the terms involved in the smooth now doesn’t
  matter. Reported by [@chrisaak](https://github.com/chrisaak)
  [\#249](https://github.com/gavinsimpson/gratia/issues/249).

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  would fail when plotting a multivariate base smoother used in an
  `"sz"` smooth. Now, this use case is identified and a message printed
  indicating that (currently) gratia doesn’t know how to plot such a
  smooth. Reported by [@chrisaak](https://github.com/chrisaak)
  [\#249](https://github.com/gavinsimpson/gratia/issues/249).

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  would fail when plotting a multivariate base smoother used in an
  `"fs"` smooth. Now, this use case is identified and a message printed
  indicating that (currently) gratia doesn’t know how to plot such a
  smooth. Reported by [@chrisaak](https://github.com/chrisaak)
  [\#249](https://github.com/gavinsimpson/gratia/issues/249).

- [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
  would fail with `order = 2` and was only computing forward finite
  differences, regardless of `type` for `order = 1`. Partly reported by
  [@samlipworth](https://github.com/samlipworth)
  [\#251](https://github.com/gavinsimpson/gratia/issues/251).

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  was normalizing the penalty to the range 0–1, not the claimed and
  documented -1–1 with argument `normalize = TRUE`. This is now fixed.

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  was failing when `data` was supplied that contained more variables
  than were used in the smooth that was being sampled. Hence this
  generally fail unless a single smooth was being sampled from or the
  model contained only a single smooth. The function never intended to
  retain all the variables in `data` but was written in such a way that
  it would fail when relocating the data columns to the end of the
  posterior sampling object.
  [\#255](https://github.com/gavinsimpson/gratia/issues/255)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  would fail when plotting a univariate tensor product smooth
  (e.g. `te(x)`, `ti(x)`, or
  [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html)). Reported by
  [@wStockhausen](https://github.com/wStockhausen)
  [\#260](https://github.com/gavinsimpson/gratia/issues/260)

- `plot.smooth()` was not printing the factor level in subtitles for
  ordered factor by smooths.

## gratia 0.8.2

CRAN release: 2024-01-09

- Small fixes for CRAN.

## gratia 0.8.1

CRAN release: 2023-02-02

### User visible changes

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  now returns objects with variables involved in smooths that have their
  correct name. Previously variables were named `.x1`, `.x2`, etc.
  Fixing [\#126](https://github.com/gavinsimpson/gratia/issues/126) and
  improving compatibility with
  [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md)
  and
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  allowed the variables to be named correctly.

- *gratia* now depends on version 1.8-41 or later of the *mgcv* package.

### New features

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can now handle tensor products that include a marginal random effect
  smooth. Beware plotting such smooths if there are many levels,
  however, as a separate surface plot will be produced for each level.

### Bug fixes

- Additional fixes for changes in dplyr 1.1.0.

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  now works when sampling from posteriors of multiple smooths with
  different dimension.
  [\#126](https://github.com/gavinsimpson/gratia/issues/126) reported by
  [@Aariq](https://github.com/Aariq)

## gratia 0.8.0

### User visible changes

- {gratia} now depends on R version 4.1 or later.

- A new vignette “Data slices” is supplied with {gratia}.

- Functions in {gratia} have harmonised to use an argument named `data`
  instead of `newdata` for passing new data at which to evaluate
  features of smooths. A message will be printed if `newdata` is used
  from now on. Existing code does not need to be changed as `data` takes
  its value from `newdata`.

  Note that due to the way `...` is handled in R, if your R script uses
  the `data` argument, *and* is run with versions of gratia prior to 8.0
  (when released; 0.7.3.8 if using the development version) the
  user-supplied data will be silently ignored. As such, scripts using
  `data` should check that the installed version of gratia is \>= 0.8
  and package developers should update to depend on versions \>= 0.8 by
  using `gratia (>= 0.8)` in `DESCRIPTION`.

- The order of the plots of smooths has changed in
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  so that they again match the order in which smooths were specified in
  the model formula. See *Bug Fixes* below for more detail or
  [\#154](https://github.com/gavinsimpson/gratia/issues/154).

### New features

- Added basic support for GAMLSS (distributional GAMs) fitted with the
  [`gamlss()`](https://rdrr.io/pkg/GJRM/man/gamlss.html) function from
  package GJRM. Support is currently restricted to a
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method.

- [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  can now include the group means in the difference, which many users
  expected. To include the group means use `group_means = TRUE` in the
  function call, e.g.
  `difference_smooths(model, smooth = "s(x)", group_means = TRUE`).
  Note: this function still differs from `plot_diff()` in package
  *itsadug*, which essentially computes differences of model
  predictions. The main practical difference is that other effects
  beyond the factor by smooth, including random effects, may be included
  with `plot_diff()`.

  This implements the main wish of
  [\#108](https://github.com/gavinsimpson/gratia/issues/108)
  ([@dinga92](https://github.com/dinga92)) and
  [\#143](https://github.com/gavinsimpson/gratia/issues/143)
  ([@mbolyanatz](https://github.com/mbolyanatz)) despite my
  protestations that this was complicated in some cases (it isn’t; the
  complexity just cancels out.)

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  has been totally revised. Now, the user provides the values for the
  variables they want in the slice and any variables in the model that
  are not specified will be held at typical values (i.e. the value of
  the observation that is closest to the median for numeric variables,
  or the modal factor level.)

  Data slices are now produced by passing `name` = `value` pairs for the
  variables and their values that you want to appear in the slice. For
  example

      m <- gam(y ~ s(x1) + x2 + fac)
      data_slice(model, x1 = evenly(x1, n = 100), x2 = mean(x2))

  The `value` in the pair can be an expression that will be looked up
  (evaluated) in the `data` argument or the model frame of the fitted
  model (the default). In the above example, the resulting slice will be
  a data frame of 100 observations, comprising `x1`, which is a vector
  of 100 values spread evenly over the range of `x1`, a constant value
  of the mean of `x2` for the `x2` variable, and a constant factor
  level, the model class of `fac`, for the `fac` variable of the model.

- [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  is a new function for computing partial derivatives of multivariate
  smooths (e.g. `s(x,z)`, `te(x,z)`) with respect to one of the margins
  of the smooth. Multivariate smooths of any dimension are handled, but
  only one of the dimensions is allowed to vary. Partial derivatives are
  estimated using the method of finite differences, with forward,
  backward, and central finite differences available. Requested by
  [@noamross](https://github.com/noamross)
  [\#101](https://github.com/gavinsimpson/gratia/issues/101)

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  provides a simple overview of model terms for fitted GAMs.

- The new `bs = "sz"` basis that was released with *mgcv* version
  1.18-41 is now supported in
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md),
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  and this basis has its own unique plotting method.
  [\#202](https://github.com/gavinsimpson/gratia/issues/202)

- [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  now has a method for fitted GAM(M)s which can extract the estimated
  basis from the model and plot it, using the estimated coefficients for
  the smooth to weight the basis.
  [\#137](https://github.com/gavinsimpson/gratia/issues/137)

  There is also a new
  [`draw.basis()`](https://gavinsimpson.github.io/gratia/reference/draw.basis.md)
  method for plotting the results of a call to
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md).
  This method can now also handle bivariate bases.

  [`tidy_basis()`](https://gavinsimpson.github.io/gratia/reference/tidy_basis.md)
  is a lower level function that does the heavy lifting in
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md),
  and is now exported.
  [`tidy_basis()`](https://gavinsimpson.github.io/gratia/reference/tidy_basis.md)
  returns a tidy representation of a basis supplied as an object
  inheriting from class `"mgcv.smooth"`. These objects are returned in
  the `$smooth` component of a fitted GAM(M) model.

- [`lp_matrix()`](https://gavinsimpson.github.io/gratia/reference/lp_matrix.md)
  is a new utility function to quickly return the linear predictor
  matrix for an estimated model. It is a wrapper to
  `predict(..., type = "lpmatrix")`

- [`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  is a synonym for
  [`seq_min_max()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  and is preferred going forward. Gains argument `by` to produce
  sequences over a covariate that increment in units of `by`.

- [`ref_level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
  and
  [`level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
  are new utility functions for extracting the reference or a specific
  level of a factor respectively. These will be most useful when
  specifying covariate values to condition on in a data slice.

- [`model_vars()`](https://gavinsimpson.github.io/gratia/reference/model_vars.md)
  is a new, public facing way of returning a vector of variables that
  are used in a model.

- [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  will now use the user-supplied data as points at which to evaluate a
  pair of smooths. Also note that the argument `newdata` has been
  renamed `data`.
  [\#175](https://github.com/gavinsimpson/gratia/issues/175)

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  now uses better labels for plot titles to avoid long labels with even
  modest factor levels.

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now works for factor-smooth interaction (`"fs"`) smooths.

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  methods now allow the angle of tick labels on the x axis of plots to
  be rotated using argument `angle`. Requested by
  [@tamas-ferenci](https://github.com/tamas-ferenci)
  [\#87](https://github.com/gavinsimpson/gratia/issues/87)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and related functions
  ([`draw.parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/draw.parametric_effects.md),
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md))
  now add the basis to the plot using a caption.
  [\#155](https://github.com/gavinsimpson/gratia/issues/155)

- [`smooth_coefs()`](https://gavinsimpson.github.io/gratia/reference/smooth_coefs.md)
  is a new utility function for extracting the coefficients for a
  particular smooth from a fitted model.
  [`smooth_coef_indices()`](https://gavinsimpson.github.io/gratia/reference/smooth_coef_indices.md)
  is an associated function that returns the indices (positions) in the
  vector of model coefficients (returned by `coef(gam_model)`) of those
  coefficients that pertain to the stated smooth.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now better handles patchworks of plots where one or more of those
  plots has fixed aspect ratios.
  [\#190](https://github.com/gavinsimpson/gratia/issues/190)

### Bug fixes

- `draw.posterior_smooths` now plots posterior samples with a fixed
  aspect ratio if the smooth is isotropic.
  [\#148](https://github.com/gavinsimpson/gratia/issues/148)

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now ignores random effect smooths (for which derivatives don’t make
  sense anyway).
  [\#168](https://github.com/gavinsimpson/gratia/issues/168)

- `confint.gam(...., method = "simultaneous")` now works with factor by
  smooths where `parm` is passed the full name of a specific smooth
  `s(x)faclevel`.

- The order of plots produced by
  [`gratia::draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  again matches the order in which the smooths entered the model
  formula. Recent changes to the internals of
  [`gratia::draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  when the switch to
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  was undertaken lead to a change in behaviour resulting from the use of
  [`dplyr::group_split()`](https://dplyr.tidyverse.org/reference/group_split.html),
  and it’s coercion internally of a character vector to a factor. This
  factor is now created explicitly, and the levels set to the correct
  order. [\#154](https://github.com/gavinsimpson/gratia/issues/154)

- Setting the `dist` argument to set response or smooth values to `NA`
  if they lay too far from the support of the data in multivariate
  smooths, this would lead an incorrect scale for the response guide.
  This is now fixed.
  [\#193](https://github.com/gavinsimpson/gratia/issues/193)

- Argument `fun` to
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  was not being applied to any parametric terms. Reported by
  [@grasshoppermouse](https://github.com/grasshoppermouse)
  [\#195](https://github.com/gavinsimpson/gratia/issues/195)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  was adding the uncertainty for all linear predictors to smooths when
  `overall_uncertainty = TRUE` was used. Now
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  only includes the uncertainty for those linear predictors in which a
  smooth takes part.
  [\#158](https://github.com/gavinsimpson/gratia/issues/158)

- [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  works when provided with a single data point at which to evaluate the
  derivative. [\#199](https://github.com/gavinsimpson/gratia/issues/199)

- [`transform_fun.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md)
  was addressing the wrong variable names when trying to transform the
  confidence interval.
  [\#201](https://github.com/gavinsimpson/gratia/issues/201)

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  doesn’t fail with an error when used with a model that contains an
  offset term.
  [\#198](https://github.com/gavinsimpson/gratia/issues/198)

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  no longer uses
  [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md),
  which is soft deprecated.
  [\#167](https://github.com/gavinsimpson/gratia/issues/167)

- [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  and
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md)
  could compute the wrong deviance residuals used to generate the
  theoretical quantiles for some of the more exotic families
  (distributions) available in *mgcv*. This also affected
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  but only for the QQ plot; the residuals shown in the other plots and
  the deviance residuals shown on the y-axis of the QQ plot were
  correct. Only the generation of the reference intervals/quantiles was
  affected.

## gratia 0.7.3

CRAN release: 2022-05-09

### User visible changes

- Plots of smooths now use “Partial effect” for the y-axis label in
  place of “Effect”, to better indicate what is displayed.

### New features

- [`confint.fderiv()`](https://gavinsimpson.github.io/gratia/reference/confint.fderiv.md)
  and
  [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  now return their results as a tibble instead of a common-or-garden
  data frame. The latter mostly already did this.

- Examples for
  [`confint.fderiv()`](https://gavinsimpson.github.io/gratia/reference/confint.fderiv.md)
  and
  [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  were reworked, in part to remove some inconsistent output in the
  examples when run on M1 macs.

### Bug fixes

- [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md)
  failed when passed non-standard model “names” like
  `compare_smooths(m_gam, m_gamm$gam)` or
  `compare_smooths(l[[1]], l[[2]])` even if the evaluated objects were
  valid GAM(M) models. Reported by Andrew Irwin
  [\#150](https://github.com/gavinsimpson/gratia/issues/150)

## gratia 0.7.2

CRAN release: 2022-03-17

### New features

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  can now handle splines on the sphere (`s(lat, long, bs = "sos")`) with
  special plotting methods using
  [`ggplot2::coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
  to handle the projection to spherical coordinates. An orthographic
  projection is used by default, with an essentially arbitrary (and
  northern hemisphere-centric) default for the orientation of the view.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  insures that `data` (and hence the returned object) is a tibble rather
  than a common or garden data frame.

### Bug fixes

- `draw.posterior_smooths()` was redundantly plotting duplicate data in
  the rug plot. Now only the unique set of covariate values are used for
  drawing the rug.

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  was not passing the `scale` argument in the bivariate example setting
  (`"eg2"`).

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  methods for [`gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) and
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html) fits were
  not passing arguments on to
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md).

- [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  would produce a subtitle with data for a continuous by smooth as if it
  were a factor by smooth. Now the subtitle only contains the name of
  the continuous by variable.

## gratia 0.7.1

Due to an issue with the size of the package source tarball, which
wasn’t discovered until after submission to CRAN, 0.7.1 was never
released.

### New features

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md):
  {gratia} can now handle smooths of 3 or 4 covariates when plotting.
  For smooths of 3 covariates, the third covariate is handled with
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  and a set (default `n` = 16) of small multiples is drawn, each a 2d
  surface evaluated at the specified value of the third covariate. For
  smooths of 4 covariates,
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  is used to draw the small multiples, with the default producing 4 rows
  by 4 columns of plots at the specific values of the third and fourth
  covariates. The number of small multiples produced is controlled by
  new arguments `n_3d` (default = `n_3d = 16`) and `n_4d` (default
  `n_4d = 4`, yielding `n_4d * n_4d` = 16 facets) respectively.

  This only affects plotting;
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  has been able to handle smooths of any number of covariates for a
  while.

  When handling higher-dimensional smooths, actually drawing the plots
  on the default device can be slow, especially with the default value
  of `n = 100` (which for 3D or 4D smooths would result in 160,000 data
  points being plotted). As such it is recommended that you reduce `n`
  to a smaller value: `n = 50` is a reasonable compromise of resolution
  and speed.

- [`model_concurvity()`](https://gavinsimpson.github.io/gratia/reference/model_concurvity.md)
  returns concurvity measures from
  [`mgcv::concurvity()`](https://rdrr.io/pkg/mgcv/man/concurvity.html)
  for estimated GAMs in a tidy format. The synonym
  [`concrvity()`](https://gavinsimpson.github.io/gratia/reference/model_concurvity.md)
  is also provided. A
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method is provided which produces a bar plot or a heatmap of the
  concurvity values depending on whether the overall concurvity of each
  smooth or the pairwise concurvity of each smooth in the model is
  requested.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  gains argument `resid_col = "steelblue3"` that allows the colour of
  the partial residuals (if plotted) to be changed.

### Bug fixes

- [`model_edf()`](https://gavinsimpson.github.io/gratia/reference/edf.md)
  was not using the `type` argument. As a result it only ever returned
  the default EDF type.

- [`add_constant()`](https://gavinsimpson.github.io/gratia/reference/add_constant.md)
  methods weren’t applying the constant to all the required variables.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md),
  [`draw.parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/draw.parametric_effects.md)
  now actually work for a model with only parametric effects.
  [\#142](https://github.com/gavinsimpson/gratia/issues/142) Reported by
  [@Nelson-Gon](https://github.com/Nelson-Gon)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  would fail for a model with only parametric terms because
  [`predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)
  returns empty arrays when passed `exclude = character(0)`.

## gratia 0.7.0

CRAN release: 2022-02-07

### Major changes

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now uses
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  internally and consequently uses its
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method and underlying plotting code. This has simplified the code
  compared to
  [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  and its methods, which will allow for future development and addition
  of features more easily than if
  [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  had been retained.

  Similarly, `evaluate_parametric_terms()` is now deprecated in favour
  of
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md),
  which is also used internally by
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  if parametric terms are present in the model (and
  `parametric = TRUE`).

  While a lot of code has been reused so differences between plots as a
  result of this change should be minimal, some corner cases may have
  been missed. File an Issue if you notice something that has changed
  that you think shouldn’t.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now plots 2D isotropic smooths (TPRS and Duchon splines) with
  equally-scaled x and y coordinates using `coord_equal(ratio = 1)`.
  Alignment of these plots will be a little different now when plotting
  models with multiple smooths. See Issue
  [\#81](https://github.com/gavinsimpson/gratia/issues/81).

#### Deprecated functions

From version 0.7.0, the following functions are considered deprecated
and their use is discouraged:

- [`fderiv()`](https://gavinsimpson.github.io/gratia/reference/fderiv.md)
  is *soft*-deprecated in favour of
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md),
- [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  is *soft*-deprecated in favour of
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
- [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md)
  is *soft*-deprecated in favour of
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md).

The first call to one of these functions will generate a warning,
pointing to the newer, alternative, function. It is safe to ignore these
warnings, but these deprecated functions will no longer receive updates
and are thus at risk of being removed from the package at some future
date. The newer alternatives can handle more types of models and
smooths, especially so in the case of
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md).

### New features

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  provides a tidy wrapper around
  [`predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html) for
  generating fitted values from the model. New covariate values can be
  provided via argument `data`. A credible interval on the fitted values
  is returned, and values can be on the link (linear predictor) or
  response scale.

  Note that this function returns expected values of the response.
  Hence, “fitted values” is used instead of “predictions” in the case of
  new covariate values to differentiate these values from the case of
  generating new response values from a fitted model.

- [`rootogram()`](https://gavinsimpson.github.io/gratia/reference/rootogram.md)
  and its
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method produce rootograms as diagnostic plots for fitted models.
  Currently only for models fitted with
  [`poisson()`](https://rdrr.io/r/stats/family.html),
  [`nb()`](https://rdrr.io/pkg/mgcv/man/negbin.html),
  [`negbin()`](https://rdrr.io/pkg/mgcv/man/negbin.html),
  [`gaussian()`](https://rdrr.io/r/stats/family.html) families.

- New helper functions
  [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md),
  [`factor_combos()`](https://gavinsimpson.github.io/gratia/reference/factor_combos.md)
  and
  [`data_combos()`](https://gavinsimpson.github.io/gratia/reference/data_combos.md)
  for quickly creating data sets for producing predictions from fitted
  models where some covariates are fixed at come typical or
  representative values.

  [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  is a new helper function to return typical values for the covariates
  of a fitted model. It returns the value of the observation closest to
  the median for numerical covariates or the modal level of a factor
  while preserving the levels of that factor.
  [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  is useful in preparing data slices or scenarios for which fitted
  values from the estimated model are required.

  [`factor_combos()`](https://gavinsimpson.github.io/gratia/reference/factor_combos.md)
  extracts and returns the combinations of levels of factors found in
  data used to fit a model. Unlike
  [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md),
  [`factor_combos()`](https://gavinsimpson.github.io/gratia/reference/factor_combos.md)
  returns all the combinations of factor levels observed in the data,
  not just the modal level. Optionally, all combinations of factor
  levels can be returned, not just those in the observed data.

  [`data_combos()`](https://gavinsimpson.github.io/gratia/reference/data_combos.md)
  combines returns the factor data from
  [`factor_combos()`](https://gavinsimpson.github.io/gratia/reference/factor_combos.md)
  plus the typical values of numerical covariates. This is useful if you
  want to generate predictions from the model for each combination of
  factor terms while holding any continuous covariates at their median
  values.

- [`nb_theta()`](https://gavinsimpson.github.io/gratia/reference/nb_theta.md)
  is a new extractor function that returns the theta parameter of a
  fitted negative binomial GAM (families
  [`nb()`](https://rdrr.io/pkg/mgcv/man/negbin.html) or
  [`negbin()`](https://rdrr.io/pkg/mgcv/man/negbin.html)). Additionally,
  [`theta()`](https://gavinsimpson.github.io/gratia/reference/theta.md)
  and
  [`has_theta()`](https://gavinsimpson.github.io/gratia/reference/has_theta.md)
  provide additional functionality.
  [`theta()`](https://gavinsimpson.github.io/gratia/reference/theta.md)
  is an experimental function for extracting any additional parameters
  from the model or family.
  [`has_theta()`](https://gavinsimpson.github.io/gratia/reference/has_theta.md)
  is useful for checking if any additional parameters are available from
  the family or model.

- [`edf()`](https://gavinsimpson.github.io/gratia/reference/edf.md)
  extracts the effective degrees of freedom (EDF) of a fitted model or a
  specific smooth in the model. Various forms for the EDF can be
  extracted.

- [`model_edf()`](https://gavinsimpson.github.io/gratia/reference/edf.md)
  returns the EDF of the overall model. If supplied with multiple
  models, the EDFs of each model are returned for comparison.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can now show a “rug” plot on a bivariate smooth by drawing small
  points with high transparency over the smooth surface at the data
  coordinates.

  In addition, the rugs on plots of factor by smooths now show the
  locations of covariate values for the specific level of the factor and
  not over all levels. This better reflects what data were used to
  estimate the smooth, even though the basis for each smooth was set up
  using all of the covariate locations.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  now allow some aspects of the plot to be changed: the fill (but not
  colour) and alpha attributes of the credible interval, and the line
  colour for the smooth can now be specified using arguments `ci_col`,
  `ci_alpha`, and `smooth_col` respectively.

- Partial residuals can now be plotted on factor by smooths. To allow
  this, the partial residuals are filtered so that only residuals
  associated with a particular level’s smooth are drawn on the plot of
  the smooth.

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  uses
  [`check_user_select_smooths()`](https://gavinsimpson.github.io/gratia/reference/check_user_select_smooths.md)
  to handle user-specified selection of smooth terms. As such it is more
  flexible than previously, and allows for easier selection of smooths
  to evaluate.

- [`fixef()`](https://gavinsimpson.github.io/gratia/reference/fixef.md)
  is now imported (and re-exported) from the *nlme* package, with
  methods for models fitted with
  [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html), to extract fixed
  effects estimates from fitted models.
  [`fixed_effects()`](https://gavinsimpson.github.io/gratia/reference/fixef.gam.md)
  is an alias for
  [`fixef()`](https://gavinsimpson.github.io/gratia/reference/fixef.md).

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  can now handle 2D smooths. Additionally, the number of posterior draws
  to plot can now be specified when plotting using new argument
  `n_samples`, which will result in `n_samples` draws being selected at
  random from the set of draws for plotting. New argument `seed` allows
  the selection of draws to be repeatable.

### Bug fixes

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  was not filtering user-supplied data for the by level of the specific
  smooth when used with by factor smooths. This would result in the
  smooth being evaluated at all rows of the user-supplied data, and
  therefore would result in `nrow(user_data) * nlevels(by_variable)`
  rows in the returned object instead of `nrow(user_data)` rows.

- The
  [`add_confint()`](https://gavinsimpson.github.io/gratia/reference/add_confint.md)
  method for
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  had the upper and lower intervals reversed.
  [\#107](https://github.com/gavinsimpson/gratia/issues/107) Reported by
  [@Aariq](https://github.com/Aariq)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  were both ignoring the `dist` argument that allows covariate values
  that lie too far from the support of the data to be excluded when
  returning estimated values from the smooth and plotting it.
  [\#111](https://github.com/gavinsimpson/gratia/issues/111) Reported by
  [@Aariq](https://github.com/Aariq)

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  with a factor by GAM would return samples for the first factor level
  only. Reported by [@rroyaute](https://github.com/rroyaute) in
  discussion of
  [\#121](https://github.com/gavinsimpson/gratia/issues/121)

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  would fail if the model contained random effect “smooths”. These are
  now ignored with a message when running
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md).
  Reported by [@isabellaghement](https://github.com/isabellaghement) in
  [\#121](https://github.com/gavinsimpson/gratia/issues/121)

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  were failing on models fitted with `family = scat()`. Reported by
  [@Aariq](https://github.com/Aariq)
  [\#130](https://github.com/gavinsimpson/gratia/issues/130)

## gratia 0.6.0

CRAN release: 2021-04-18

### Major changes

- The {cowplot} package has been replaced by the {patchwork} package for
  producing multi-panel figures in
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  and
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md).
  This shouldn’t affect any code that used {gratia} only, but if you
  passed additional arguments to
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html)
  or used the `align` or `axis` arguments of
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  and
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
  you’ll need to adapt code accordingly.

  Typically, you can simply delete the `align` or `axis` arguments and
  {patchwork} will just work and align plots nicely. Any arguments
  passed via `...` to
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html)
  will just be ignored by
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  unless those passed arguments match any of the arguments of
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

### New features

- The {patchwork} package is now used for multi-panel figures. As such,
  {gratia} no longer Imports from the {cowplot} package.

- Worm plot diagnostic plots are available via new function
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md).
  Worm plots are detrended Q-Q plots, where deviation from the Q-Q
  reference line are emphasized as deviations around the line occupy the
  full height of the plot.

  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md)
  methods are available for models of classes `"gam"`, `"glm"`, and
  `"lm"`. ([\#62](https://github.com/gavinsimpson/gratia/issues/62))

- Smooths can now be compared across models using
  [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md),
  and comparisons visualised with the associated
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method. ([\#85](https://github.com/gavinsimpson/gratia/issues/85)
  [@dill](https://github.com/dill))

  This feature is a bit experimental; the returned object uses nested
  lists and may change in the future if users find this confusing.

- The reference line in
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  with `method = "normal"` was previously drawn as a line with intercept
  0 and slope 1, to match the other methods. This was inconsistent with
  [`stats::qqplot()`](https://rdrr.io/r/stats/qqnorm.html) which drew
  the line through the 1st and 3rd quartiles.
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  with `method = "normal"` now uses this robust reference line.
  Reference lines for the other methods remain drawn with slope 1 and
  intercept 0.

- [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  with `method = "normal"` now draws a point-wise reference band using
  the standard error of the order statistic.

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  now plots the penalty matrix heatmaps in a more-logical orientation,
  to match how the matrices might be written down or printed to the R
  console.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  now work for models fitted with the
  [`gumbls()`](https://rdrr.io/pkg/mgcv/man/gumbls.html) and
  [`shash()`](https://rdrr.io/pkg/mgcv/man/shash.html) families.
  ([\#84](https://github.com/gavinsimpson/gratia/issues/84))

- [`extract_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  is a lower level utility function related to
  [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  and is now exported.

### User visible changes

- The default method name for generating reference quantiles in
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  was changed from `"direct"` to `"uniform"`, to avoid confusion with
  the [`mgcv::qq.gam()`](https://rdrr.io/pkg/mgcv/man/qq.gam.html) help
  page description of the methods. Accordingly using `method = "direct"`
  is deprecated and a message to this effect is displayed if used.

- The way smooths/terms are selected in
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  has been switched to use the same mechanism as
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)’s
  `select` argument. To get a partial match to `term`, you now need to
  also specify `partial_match = TRUE` in the call to
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md).

### Bug fixes

- [`transform_fun()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md)
  had a copy paste bug in the definition of the then generic.
  ([\#96](https://github.com/gavinsimpson/gratia/issues/96)
  [@Aariq](https://github.com/Aariq))

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  with user-supplied `newdata` would fail for factor by smooths with
  `interval = "simultaneous"` and would introduce rows with derivative
  == 0 with `interval = "confidence"` because it didn’t subset the rows
  of `newdata` for the specific level of the by factor when computing
  derivatives.
  ([\#102](https://github.com/gavinsimpson/gratia/issues/102)
  [@sambweber](https://github.com/sambweber))

- [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  can now handle random effect smooths defined using an ordered factor.
  ([\#99](https://github.com/gavinsimpson/gratia/issues/99)
  [@StefanoMezzini](https://github.com/StefanoMezzini))

## gratia 0.5.1

CRAN release: 2021-01-23

### New features

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  can now handle

  - bivariate and multivariate thinplate regression spline smooths,
    e.g.  `s(x, z, a)`,
  - tensor product smooths
    ([`te()`](https://rdrr.io/pkg/mgcv/man/te.html),
    [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html), &
    [`ti()`](https://rdrr.io/pkg/mgcv/man/te.html)), e.g. `te(x, z, a)`
  - factor smooth interactions, e.g. `s(x, f, bs = "fs")`
  - random effect smooths, e.g. `s(f, bs = "re")`

- [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  provides a tidy representation of the penalty matrices of smooths. The
  tidy representation is most suitable for plotting with
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method is provided, which represents the penalty matrix as a heatmap.

### User visible changes

- The `newdata` argument to
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  has been changed to `data` as was originally intended.

## gratia 0.5.0

CRAN release: 2021-01-10

### New features

- Partial residuals for models can be computed with
  [`partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/partial_residuals.md).
  The partial residuals are the weighted residuals of the model added to
  the contribution of each smooth term (as returned by
  `predict(model, type = "terms")`.

  Wish of [\#76](https://github.com/gavinsimpson/gratia/issues/76)
  ([@noamross](https://github.com/noamross))

  Also, new function
  [`add_partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/add_partial_residuals.md)
  can be used to add the partial residuals to data frames.

- Users can now control to some extent what colour or fill scales are
  used when plotting smooths in those
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  methods that use them. This is most useful to change the fill scale
  when plotting 2D smooths, or to change the discrete colour scale used
  when plotting random factor smooths (`bs = "fs"`).

  The user can pass scales via arguments `discrete_colour` and
  `continuous_fill`.

- The effects of certain smooths can be excluded from data simulated
  from a model using
  [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  and
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
  by passing `exclude` or `terms` on to
  [`predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html). This
  allows for excluding random effects, for example, from model predicted
  values that are then used to simulate new data from the conditional
  distribution. See the example in
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md).

  Wish of [\#74](https://github.com/gavinsimpson/gratia/issues/74)
  ([@hgoldspiel](https://github.com/hgoldspiel))

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and related functions gain arguments `constant` and `fun` to allow for
  user-defined constants and transformations of smooth estimates and
  confidence intervals to be applied.

  Part of wish of Wish of
  [\#79](https://github.com/gavinsimpson/gratia/issues/79).

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  now works for 2D smooths also.

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  is an early version of code to replace (or more likely supersede)
  [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md).
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  can currently only handle 1D smooths of the standard types.

### User visible changes

- The meaning of `parm` in `confint.gam` has changed. This argument now
  requires a smooth label to match a smooth. A vector of labels can be
  provided, but partial matching against a smooth label only works with
  a single `parm` value.

  The default behaviour remains unchanged however; if `parm` is `NULL`
  then all smooths are evaluated and returned with confidence intervals.

- `data_class()` is no longer exported; it was only ever intended to be
  an internal function.

### Bug Fixes

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  was failing on a tensor product smooth due to matching issues.
  Reported by [@tamas-ferenci](https://github.com/tamas-ferenci)
  [\#88](https://github.com/gavinsimpson/gratia/issues/88)

  This also fixes
  [\#80](https://github.com/gavinsimpson/gratia/issues/80)

  1.  which was a related issue with selecting a specific smooth.

- The **vdiffr** package is now used conditionally in package tests.
  Reported by Brian Ripley
  [\#93](https://github.com/gavinsimpson/gratia/issues/93)

## gratia 0.4.1

CRAN release: 2020-05-30

### User visible changes

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  with `scales = "fixed"` now applies to all terms that can be plotted,
  including 2d smooths.

  Reported by [@StefanoMezzini](https://github.com/StefanoMezzini)
  [\#73](https://github.com/gavinsimpson/gratia/issues/73)

### Bug fixes

- [`dplyr::combine()`](https://dplyr.tidyverse.org/reference/defunct.html)
  was deprecated. Switch to
  [`vctrs::vec_c()`](https://vctrs.r-lib.org/reference/vec_c.html).

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  with `scales = "fixed"` wasn’t using fixed scales where 2d smooths
  were in the model.

  Reported by [@StefanoMezzini](https://github.com/StefanoMezzini)
  [\#73](https://github.com/gavinsimpson/gratia/issues/73)

## gratia 0.4.0

### New features

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can include partial residuals when drawing univariate smooths. Use
  `residuals = TRUE` to add partial residuals to each univariate smooth
  that is drawn. This feature is not available for smooths of more than
  one variable, by smooths, or factor-smooth interactions (`bs = "fs"`).

- The coverage of credible and confidence intervals drawn by
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can be specified via argument `ci_level`. The default is arbitrarily
  `0.95` for no other reason than (rough) compatibility with
  [`plot.gam()`](https://rdrr.io/pkg/mgcv/man/plot.gam.html).

  This change has had the effect of making the intervals slightly
  narrower than in previous versions of *gratia*; intervals were drawn
  at ± 2 × the standard error. The default intervals are now drawn at ±
  ~1.96 × the standard error.

- New function
  [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  for computing differences between factor smooth interactions. Methods
  available for [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
  [`gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) and
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html). Also has
  a [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method, which can handle differences of 1D and 2D smooths currently
  (handling 3D and 4D smooths is planned).

- New functions
  [`add_fitted()`](https://gavinsimpson.github.io/gratia/reference/add_fitted.md)
  and
  [`add_residuals()`](https://gavinsimpson.github.io/gratia/reference/add_residuals.md)
  to add fitted values (expectations) and model residuals to an existing
  data frame. Currently methods available for objects fitted by
  [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  is a tidy reimplementation of
  [`mgcv::gamSim()`](https://rdrr.io/pkg/mgcv/man/gamSim.html) with the
  added ability to use sampling distributions other than the Gaussian
  for all models implemented. Currently Gaussian, Poisson, and Bernoulli
  sampling distributions are available.

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  can handle continuous by variable smooths such as in varying
  coefficient models.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  now work for all families available in *mgcv*, including the location,
  scale, shape families, and the more specialised families described in
  [`?mgcv::family.mgcv`](https://rdrr.io/pkg/mgcv/man/family.mgcv.html).

- [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md),
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md),
  [`family()`](https://rdrr.io/r/stats/family.html),
  [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  methods for models fitted using `gamm4()` from the **gamm4** package.

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  can generate data for a 1-d slice (a single variable varying).

- The colour of the points, reference lines, and simulation band in
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  can now be specified via arguments

  - `point_col`,
  - `point_alpha`,
  - `ci_col`
  - `ci_alpha`
  - `line_col`

  These are passed on to
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md),
  [`observed_fitted_plot()`](https://gavinsimpson.github.io/gratia/reference/observed_fitted_plot.md),
  [`residuals_linpred_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_linpred_plot.md),
  and
  [`residuals_hist_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_hist_plot.md),
  which also now take the new arguments were applicable.

- Added utility functions
  [`is_factor_term()`](https://gavinsimpson.github.io/gratia/reference/is_factor_term.md)
  and
  [`term_variables()`](https://gavinsimpson.github.io/gratia/reference/term_variables.md)
  for working with models.
  [`is_factor_term()`](https://gavinsimpson.github.io/gratia/reference/is_factor_term.md)
  identifies is the named term is a factor using information from the
  [`terms()`](https://rdrr.io/r/stats/terms.html) object of the fitted
  model.
  [`term_variables()`](https://gavinsimpson.github.io/gratia/reference/term_variables.md)
  returns a character vector of variable names that are involved in a
  model term. These are strictly for working with parametric terms in
  models.

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  now works for models fitted by
  [`glm()`](https://rdrr.io/r/stats/glm.html) and
  [`lm()`](https://rdrr.io/r/stats/lm.html), as do the underlying
  functions it calls, especially `qq_plot`.

  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  also works for models fitted with family
  [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html). Further
  location scale models and models fitted with extended family functions
  will be supported in upcoming releases.

### User visible changes

- [`datagen()`](https://gavinsimpson.github.io/gratia/reference/datagen.md)
  is now an *internal* function and is no longer exported. Use
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  instead.

- [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md)
  is now much stricter and can only evaluate main effect terms,
  i.e. those whose order, as stored in the `terms` object of the model
  is `1`.

### Bug fixes

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  was not getting the x-axis label for factor by smooths correctly, and
  instead was using `NA` for the second and subsequent levels of the
  factor.

- The
  [`datagen()`](https://gavinsimpson.github.io/gratia/reference/datagen.md)
  method for class `"gam"` couldn’t possibly have worked for anything
  but the simplest models and would fail even with simple factor by
  smooths. These issues have been fixed, but the behaviour of
  [`datagen()`](https://gavinsimpson.github.io/gratia/reference/datagen.md)
  has changed, and the function is now not intended for use by users.

- Fixed an issue where in models terms of the form `factor1:factor2`
  were incorrectly identified as being numeric parametric terms.
  [\#68](https://github.com/gavinsimpson/gratia/issues/68)

## gratia 0.3.1

CRAN release: 2020-03-29

### New features

- New functions
  [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  to access the link function and its inverse from fitted models and
  family functions.

  Methods for classes: `"glm"`, `"gam"`, `"bam"`, `"gamm"` currently.
  [\#58](https://github.com/gavinsimpson/gratia/issues/58)

- Adds explicit [`family()`](https://rdrr.io/r/stats/family.html)
  methods for objects of classes `"gam"`, `"bam"`, and `"gamm"`.

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now handles non-numeric when creating shifted data for finite
  differences. Fixes a problem with `stringsAsFactors = FALSE` default
  in R-devel. [\#64](https://github.com/gavinsimpson/gratia/issues/64)

### Bug fixes

- Updated *gratia* to work with *tibble* versions \>= 3.0

## gratia 0.3.0

CRAN release: 2020-01-19

### New features

- *gratia* now uses the *mvnfast* package for random draws from a
  multivariate normal distribution
  ([`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html)).
  Contributed by Henrik Singmann

  2.  [\#28](https://github.com/gavinsimpson/gratia/issues/28)

- New function
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  for generating tidy representations of basis expansions from an
  *mgcv*-like definition of a smooth,
  e.g. [`s()`](https://rdrr.io/pkg/mgcv/man/s.html),
  [`te()`](https://rdrr.io/pkg/mgcv/man/te.html),
  [`ti()`](https://rdrr.io/pkg/mgcv/man/te.html), or
  [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html). The basic smooth types
  also have a simple
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for plotting the basis.
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  is a simple wrapper around
  [`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html)
  with some post processing of the basis model matrix into a tidy
  format. [\#42](https://github.com/gavinsimpson/gratia/issues/42)

- New function
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  to draw samples of entire smooth functions from their posterior
  distribution. Also has a
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for plotting the posterior samples.

### Bug fixes

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  would produce empty plots between the panels for the parametric terms
  if there were 2 or more parametric terms in a model. Reported by
  [@sklayn](https://github.com/sklayn)
  [\#39](https://github.com/gavinsimpson/gratia/issues/39).

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now works with factor by smooths, including ordered factor by smooths.
  The function also now works correctly for complex models with multiple
  covariates/smooths.
  [\#47](https://github.com/gavinsimpson/gratia/issues/47)

  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  also now handles `'fs'` smooths. Reported by
  [@tomand-uio](https://github.com/tomand-uio)
  [\#57](https://github.com/gavinsimpson/gratia/issues/57).

- [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md)
  and hence
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  would fail on a [`ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html)
  model because i) *gratia* didn’t handle parametric terms in models
  with multiple linear predictors correctly, and ii) *gratia* didn’t
  convert to the naming convention of *mgcv* for terms in higher linear
  predictors. Reported by [@pboesu](https://github.com/pboesu)
  [\#45](https://github.com/gavinsimpson/gratia/issues/45)
