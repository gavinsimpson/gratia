# gratia (development version)

## User visible changes

* Rug plots in `draw.gam()` now only show the unique data values to avoid 
  needless overplotting. This behaviour is controlled via new argument
  `distinct_rug`. Setting this to `FALSE` will get the previous behaviour.

* `simulate.gam()` gains a `print()` method and thus no longer prints its
  attributes.

* `citation("gratia")` now suggests to use Simpson (2024) *Journal of Open
  Source Software* **9**(104), 9862, when citing the package.

* `simulate.gam()` now returns a data frame, bringing it into line with
  `stats::simulate.lm()`.

## New features

* `appraise()` with `method = "simulate"` can now handle *mgcv*'s multivariate
  normal models fitted with `family = mvn()`and Tweedie location scale models
  fitted with `family = twlss()`.

* `fix_family_rd()` is an (currently) internal function that extends 
  `mgcv::fix.family.rd()`. This function adds a `rfoo()` function to the `$rd` 
  component of the `family()` object stored in the model. This function is used
  to generate new values of the response at supplied values of the linear
  predictor. `fix_family_rd()` calls `mgcv::fix.family.rd()` so it is backward 
  compatible with *mgcv*'s behaviour, but it is designed to extend the
  behaviour through support for a richer set of models.

  Currently, `fix_family_rd()` adds support for multivariate normal models 
  fitted with `family = mvn()` and location-scale Tweedie models fitted with
  `family = twlss()`, but the latter is currently very slow.

  The functionality of `fix_family_rd()` may move to another package or even 
  to *mgcv* if suitable.

* `difference_smooths()` can now be used to compare named smooths using
  `select = c("s(x):f1", "s(x):f2")` etc, where the vector of smooth names must
  match exactly something returned by `smooths()`. Smooths to be compared must
  have the same covariate (`x` in the example) and same factor-by variable
  (`f` in the example). Tensor product factor-by smooths are handled likewise.
  This effectively handles the problem of #315, reported by @3rd3.

  Additionally, `difference_smooths()` with `group_means = TRUE` can now
  include group means specified using a random effect smooth, e.g. following
  the above example for `select`, a random effect smooth `s(f, bs = "re")`, with
  name `s(f)`, is looked for in the model matrix and its effects included in the
  comparisons.

* `data_slice()` gains argument `.observed_only` which allows simple filtering
  of the combinations of values specified in the slice to those that are in the
  data frame supplied or in the model frame of a fitted GAM.

* `posterior_samples()`, `predicted_samples()`, `fitted_samples()`, and
  `simulate.gam()` now simulate for all linear predictors (i.e., all responses)
  in `mvn()` and `multinom()` models as these are all location parameter linear
  predictors.

* `fitted_values()` now works for GAMs fitted with families `ziP()`, `mvn()`,
  and `multinom()`. The non-working of `fitted-values()` for `ziP()` models was
  reported in #341 by @rroyaute

## Bug fixes

* `conditional_values()` would fail if a variable it was conditioning was also
  a name of a function. #323 (but not really fixed when this was initially
  closed)

* `parametric_effects()` was only keeping one set of factor levels when there 
  were multiple parametric factor terms in a model. Reported by @tamas-ferenci 
  as part of discussion to #284

* `mh_draws()` would fail in the case of drawing only `n = 1` samples. #328
  Reported by @zsusswein

* `simulate.gam()` now works for the `ocat()` family, which has a non-standard
  `rd` function in its `family()` object. Reported by @hhp94, #319

* `gaussian_draws()` would fail in some cases with *gamm4* models because the
  coefficients were returned as a vector, not the expect matrix. #332 reported
  by @JaredStufft-Elion

* `draw.smooth_estimates()` would generate very many warnings when plotting
  spline on the sphere smoothers (`bs = "sos"`) because `geom_tile()` was
  creating tiles that extended beyond valid geographical coordinates given the
  tile centre coordinates that it was being provided. The function that creates
  the data to evaluate the spline at now tries to avoid this. Note this fix is
  not a complete solution. #334 Reported by @StefanoMezzini

* `link()` and `inv_link()` failed to extract the link an inverse link functions
  for `mvn()` and `multinom()` models.

* `link()` and `inv_link()` now work correctly for models fitted with the
  `ziP()` family. #341 Reported by @rroyaute

* `basis.scam` was incorrectly trying to identify the smooths named in `select`.

* `overview()` was displaying more significant digits in the p values than it
  was accurate to in terms of reporting p values as "<0.001". A new argument
  `digits` controls how many digits are used when formatting p values, with 
  default of `3`.

* `theta()` now works for models fitted with the `gfam()` family. Previously it
  would fail because it assumes all `family()$getTheta` functions had an
  argument `trans` to transform the theta values to the correct scale.

# gratia 0.10.0

## New features

* `conditional_values()` and its `draw()` method compute and plot predictions 
  from a fitted GAM that are conditional on one or more covariates. The 
  function is a wrapper around `fitted_values()` but allows the user simple ways
  to specify which covariates to condition on and at what values those
  covariates should take. It provides similar functionality to
  `marginaleffects::plot_predictions()`, but is simpler. See #300.

* `penalty()` and `basis()` can now allow the smooth to be reparameterized such
  that the resulting basis has an identity matrix. This more clearly highlights
  the penalty null space, the functions that the penalty has no effect on.

* `draw.gam()` and `draw.smooth_estimates()` gain argument `caption`, which, if
  set to `FALSE` will not plot the smooth basis type as a caption on the plot.
  #307

* `appraise()` and `qq_plot.gam()` now allow the user to set a random seed that
  is used when generating reference quantiles with `method = "uniform"` or
  `method = "simulate"`.

## Bug fixes

* `derivative_samples()` was ignoring the `scale` argument. #293 Reported by
  @jonathonmellor

* Argument `level` to `derivative_samples()` was included accidentally. As of
  v0.9.2.9002 this argument is deprecated and using it will now generate a
  warning. #291

* `draw()` was not plotting cyclic P spline smooths. Reported by @Zuckerbrot
  #297

* `derivatives()` would fail for `"fs"` smooths with other parametric effects in
  the model. Reported by @mahonmb #301

* Partial residuals in `partial_residuals()` and `draw.gam()` were wrong for
  GAMs fitted with `family = binomial()` where the `weights` argument contained
  the binomial sample sizes because the prior weights were being used to form
  weighted working residuals. Now working weights are used instead. Reported by
  @emchuron #273

* Internal function `gammals_link()` was expecting `"theta"` as a synonym for
  the scale parameter but the master table has `"phi"` coded as the synonym.
  Now both work as expected.

* `level()` assumed that `level` would have only a single value even though it
  could handle multiple levels. #321

# gratia 0.9.2

## Breaking changes

* `parametric_effects()` slightly escaped the great renaming that happened for
  0.9.0. Columns `type` and `term` did not gain a prefix `.`. This is now
  rectified and these two columns are now `.type` and `.term`.

## User visible changes

* Plots of random effects are now labelled with their smooth label. Previously,
  the title was taken fro the variable involved in the smooth, but this doesn't
  work for terms like `s(subject, continuous_var, bs = "re")` for random slopes, 
  which previsouly would have the title `"subject"`. Now such terms will have
  title `"s(subject,continuous_var)"`. Simple random intercept terms,
  `s(subject, bs = "re")`, are now titled `"s(subject)"`. #287

* The vignettes
    1. `custom-plotting.Rmd`, and
    2. `posterior-simulation.Rmd`
  were moved to `vignettes/articles` and thus are no longer available as package
  vignettes. Instead, they are accessible as Articles through the package
  website: <https://gavinsimpson.github.io/gratia/>

## New features

* `fitted_samples()` now works for `gam()` models with multiple linear
  predictors, but currently only the location parameter is supported. The
  parameter is indicated through a new variable `.parameter` in the returned
  object.

## Bug fixes

* `partial_residuals()` was computing partial residuals from the *deviance*
  residuals. For compatibility with `mgcv::plot.gam()`, partial residuals are
  now computed from the *working* residuals. Reported by @wStockhausen #273

* `appraise()` was not passing the `ci_col` argument on `qq_plot()` and
  `worm_plot()`. Reported by Sate Ahmed.

* Couldn't pass `mvn_method` on to posterior sampling functions from user facing
  functions `fitted_samples()`, `posterior_samples()`, `smooth_samples()`,
  `derivative_samples()`, and `repsonse_derivatives()`. Reported by @stefgehrig
  #279

* `fitted_values()` works again for quantile GAMs fitted by `qgam()`.

* `confint.gam()` was not applying `shift` to the estimate and upper and lower
  interval. #280 reported by @TIMAVID & @rbentham

* `parametric_effects()` and `draw.parametric_effects()` would forget about the
  levels of factors (intentionally), but this would lead to problems with
  ordered factors where the ordering of levels was not preserved. Now,
  `parametric_effects()` returns a named list of factor levels as attribute
  `"factor_levels"` containing the required information and the order of levels
  is preserved when plotting. #284 Reported by @mhpob

* `parametric_effects()` would fail if there were parametric terms in the model
  but they were all interaction terms (which we don't currently handle). #282

# gratia 0.9.0

## Breaking changes

* Many functions now return objects with different named variables. In order to
  avoid clashes with variable names used in user's models or data, a period
  (`.`) is now being used as a prefix for generated variable names. The
  functions whose names have changed are: `smooth_estimates()`,
  `fitted_values()`, `fitted_samples()`, `posterior_samples()`, `derivatives()`,
  `partial_derivatives()`, and `derivative_samples()`. In addition,
  `add_confint()` also adds newly-named variables.

      1. `est` is now `.estimate`,
      2. `lower` and `upper` are now `.lower_ci` and `.upper_ci`,
      3. `draw` and `row` and now `.draw` and `.row` respectively,
      4. `fitted`, `se`, `crit` are now `.fitted`, `.se`, `.crit`, respectively
      5. `smooth`, `by`, and `type` in `smooth_estimates()` are now `.smooth`,
         `.by`, `.type`, respectively.

* `derivatives()` and `partial_derivatives()` now work more like
  `smooth_estimates()`; in place of the `var` and `data` columns, *gratia* now
  stores the data variables at which the derivatives were evaluated as columns
  in the object with their actual variable names.

* The way spline-on-the-sphere (SOS) smooths (`bs = "sos"`) are plotted has
  changed to use `ggplot2::coord_sf()` instead of the previously-used
  `ggplot2::coord_map()`. This changed has been made as a result of
  `coord_map()` being soft-deprecated ("superseded") for a few minor versions of
  ggplot2 by now already, and changes to the guides system in version 3.5.0 of
  ggplot2.
  
  The axes on plots created with `coord_map()` never really worked
  correctly and changing the angle of the tick labels never worked. As
  `coord_map()` is superseded, it didn't receive the updates to the guides
  system and a side effect of these changes, the code that plotted SOS smooths
  was producing a warning with the release of ggplot2 version 3.5.0.

  The projection settings used to draw SOS smooths was previously controlled via
  arguments `projection` and `orientation`. These arguments do not affect
  `ggplot2::coord_sf()`, Instead the projection used is controlled through new
  argument `crs`, which takes a PROJ string detailing the projection to use or
  an integer that refers to a known coordinate reference system (CRS). The
  default projection used is `+proj=ortho +lat_0=20 +lon_0=XX` where `XX` is the
  mean of the longitude coordinates of the data points.

### Defunct and deprecated functions and arguments

#### Defunct

* `evaluate_smooth()` was deprecated in gratia version 0.7.0. This function and
  all it's methods have been removed from the package. Use `smooth_estimates()`
  instead.

#### Deprecated functions

The following functions were deprecated in version 0.9.0 of gratia. They will
eventually be removed from the package as part of a clean up ahead of an
eventual 1.0.0 release. These functions will become defunct by version 0.11.0 or
1.0.0, whichever is released soonest.

* `evaluate_parametric_term()` has been deprecated. Use `parametric_effects()`
  instead.

* `datagen()` has been deprecated. It never really did what it was originally
  designed to do, and has been replaced by `data_slice()`.

#### Deprecated arguments

To make functions in the package more consistent, the arguments `select`,
`term`, and `smooth` are all used for the same thing and hence the latter two
have been deprecated in favour of `select`. If a deprecated argument is used, a
warning will be issued but the value assigned to the argument will be assigned
to `select` and the function will continue.

## User visible changes

* `smooth_samples()` now uses a single call to the RNG to generate draws from
  the posterior of smooths. Previous to version 0.9.0, `smooth_samples()` would
  do a separate call to `mvnfast::rmvn()` for each smooth. As a result, the
  result of a call to `smooth_samples()` on a model with multiple smooths will
  now produce different results to those generated previously. To regain the
  old behaviour, add `rng_per_smooth = TRUE` to the `smooth_samples()` call.
  
  Note, however, that using per-smooth RNG calls with `method = "mh"` will be
  very inefficient as, with that method, posterior draws for all coefficients
  in the model are sampled at once. So, only use `rng_per_smooth = TRUE` with
  `method = "gaussian"`.

* The output of `smooth_estimates()` and its `draw()` method have changed for
  tensor product smooths that involve one or more 2D marginal smooths. Now,
  if no covariate values are supplied via the `data` argument,
  `smooth_estimates()` identifies if one of the marginals is a 2d surface and
  allows the covariates involved in that surface to vary fastest, ahead of terms
  in other marginals. This change has been made as it provides a better default
  when nothing is provided to `data`.

  This also affects `draw.gam()`.

* `fitted_values()` now has some level of support for location, scale, shape
  families. Supported families are `mgcv::gaulss()`, `mgcv::gammals()`,
  `mgcv::gumbls()`, `mgcv::gevlss()`, `mgcv::shash()`, `mgcv::twlss()`, and
  `mgcv::ziplss()`.

* *gratia* now requires *dplyr* versions >= 1.1.0 and *tidyselect* >= 1.2.0.

* A new vignette *Posterior Simulation* is available, which describes how to
  do posterior simulation from fitted GAMs using {gratia}.

## New features

* Soap film smooths using basis `bs = "so"` are now handled by `draw()`,
  `smooth_estimates()` etc. #8

* `response_derivatives()` is a new function for computing derivatives of the
  response with respect to a (continuous) focal variable. First or second
  order derivatives can be computed using forward, backward, or central
  finite differences. The uncertainty in the estimated derivative is determined
  using posterior sampling via `fitted_samples()`, and hence can be derived
  from a Gaussian approximation to the posterior or using a Metropolis Hastings
  sampler (see below.)

* `derivative_samples()` is the work horse function behind
  `response_derivatives()`, which computes and returns posterior draws of the
  derivatives of any additive combination of model terms. Requested by
  @jonathanmellor #237

* `data_sim()` can now simulate response data from gamma, Tweedie and ordered
  categorical distributions.

* `data_sim()` gains two new example models `"gwf2"`, simulating data only from
  Gu & Wabha's *f2* function, and `"lwf6"`, example function 6 from Luo & Wabha
  (1997 JASA 92(437), 107-116).

* `data_sim()` can also simulate data for use with GAMs fitted using
  `family = gfam()` for grouped families where different types of data in
  the response are handled. #266 and part of #265

* `fitted_samples()` and `smooth_samples()` can now use the Metropolis Hastings
  sampler from `mgcv::gam.mh()`, instead of a Gaussian approximation, to sample
  from the posterior distribution of the model or specific smooths
  respectively.

* `posterior_samples()` is a new function in the family of `fitted_samples()`
  and `smooth_samples()`. `posterior_samples()` returns draws from the
  posterior distribution of the response, combining the uncertainty in the
  estimated expected value of the response and the dispersion of the response
  distribution. The difference between `posterior_samples()` and
  `predicted_samples()` is that the latter only includes variation due to
  drawing samples from the conditional distribution of the response (the
  uncertainty in the expected values is ignored), while the former includes
  both sources of uncertainty.

* `fitted_samples()` can new use a matrix of user-supplied posterior draws.
  Related to #120

* `add_fitted_samples()`, `add_predicted_samples()`, `add_posterior_samples()`,
  and `add_smooth_samples()` are new utility functions that add the respective
  draws from the posterior distribution to an existing data object for the
  covariate values in that object: `obj |> add_posterior_draws(model)`. #50

* `basis_size()` is a new function to extract the basis dimension (number of
  basis functions) for smooths. Methods are available for objects that inherit
  from classes `"gam"`, `"gamm"`, and `"mgcv.smooth"` (for individual smooths).

* `data_slice()` gains a method for data frames and tibbles.

* `typical_values()` gains a method for data frames and tibbles.

* `fitted_values()` now works with models fitted using the `mgcv::ocat()`
  family. The predicted probability for each category is returned, alongside a
  Wald interval created using the standard error (SE) of the estimated
  probability. The SE and estimated probabilities are transformed to the logit
  (linear predictor) scale, a Wald credible interval is formed, which is then
  back-transformed to the response (probability) scale.

* `fitted_values()` now works for GAMMs fitted using `mgcv::gamm()`. Fitted
  (predicted) values only use the GAM part of the model, and thus exclude the
  random effects.

* `link()` and `inv_link()` work for models fitted using the `cnorm()` family.

* A worm plot can now be drawn in place of the QQ plot with `appraise()` via
  new argument `use_worm = TRUE`. #62

* `smooths()` now works for models fitted with `mgcv::gamm()`.

* `overview()` now returns the basis dimension for each smooth and gains an
  argument `stars` which if `TRUE` add significance stars to the output plus a
  legend is printed in the tibble footer. Part of wish of @noamross #214

* New `add_constant()` and `transform_fun()` methods for `smooth_samples()`.

* `evenly()` gains arguments `lower` and `upper` to modify the lower and / or
  upper bound of the interval over which evenly spaced values will be generated.

* `add_sizer()` is a new function to add information on whether the derivative
  of a smooth is significantly changing (where the credible interval excludes
  0). Currently, methods for `derivatives()` and `smooth_estimates()` objects
  are implemented. Part of request of @asanders11 #117

* `draw.derivatives()` gains arguments `add_change` and `change_type` to allow
  derivatives of smooths to be plotted with indicators where the credible
  interval on the derivative excludes 0. Options allow for periods of decrease
  or increase to be differentiated via `change_type = "sizer"` instead of the
  default `change_type = "change"`, which emphasises either type of change in
  the same way. Part of wish of @asanders11 #117

* `draw.gam()` can now group factor by smooths for a given factor into a single
  panel, rather than plotting the smooths for each level in separate panels.
  This is achieved via new argument `grouped_by`. Requested by @RPanczak #89

  `draw.smooth_estimates()` can now also group factor by smooths for a given
  factor into a single panel.

* The underlying plotting code used by `draw_smooth_estimates()` for most
  univariate smooths can now add change indicators to the plots of smooths if
  those change indicators are added to the object created by
  `smooth_estimates()` using `add_sizer()`. See the example in
  `?draw.smooth_estimates`.

* `smooth_estimates()` can, when evaluating a 3D or 4D tensor product smooth,
  identify if one or more 2D smooths is a marginal of the tensor product. If
  users do not provide covariate values at which to evaluate the smooths,
  `smooth_estimates()` will focus on the 2D marginal smooth (or the first if
  more than one is involved in the tensor product), instead of following the
  ordering of the terms in the definition of the tensor product. #191

  For example, in `te(z, x, y, bs = c(cr, ds), d = c(1, 2))`, the second
  marginal smooth is a 2D Duchon spline of covariates `x` and `y`. Previously,
  `smooth_estimates()` would have generated `n` values each for `z` and `x` and
  `n_3d` values for `y`, and then evaluated the tensor product at all
  combinations of those generated values. This would ignore the structure
  implicit in the tensor product, where we are likely to want to know how the
  surface estimated by the Duchon spline of `x` and `y` smoothly varies with
  `z`. Previously `smooth_estimates()` would generate surfaces of `z` and `x`,
  varying by `y`. Now, `smooth_estimates()` correctly identifies that one of the
  marginal smooths of the tensor product is a 2D surface and will focus on that
  surface varying with the other terms in the tensor product.

  This improved behaviour is needed because in some `bam()` models it is not
  always possible to do the obvious thing and reorder the smooths when defining
  the tensor product to be `te(x, y, z, bs = c(ds, cr), d = c(2, 1))`. When
  `discrete = TRUE` is used with `bam()` the terms in the tensor product may
  get rearranged during model setup for maximum efficiency (See *Details* in
  `?mgcv::bam`).

  Additionally, `draw.gam()` now also works the same way.

* New function `null_deviance()` that extracts the null deviance of a fitted
  model.

* `draw()`, `smooth_estimates()`, `fitted_values()`, `data_slice()`, and
  `smooth_samples()` now all work for models fitted with `scam::scam()`.
  Where it matters, current support extends only to univariate smooths.

* `generate_draws()` is a new low-level function for generating posterior draws
  from fitted model coefficients. `generate_daws()` is an S3 generic function so
  is extensible by users. Currently provides a simple interface to a simple
  Gaussian approximation sampler (`gaussian_draws()`) and the simple Metropolis
  Hasting sample (`mh_draws()`) available via `mgcv::gam.mh()`. #211

* `smooth_label()` is a new function for extracting the labels 'mgcv' creates for
  smooths from the smooth object itself.

* `penalty()` has a default method that  works with `s()`, `te()`, `t2()`, and
  `ti()`, which create a smooth specification.

* `transform_fun()` gains argument `constant` to allow for the addition of a
  constant value to objects (e.g. the estimate and confidence interval). This
  enables a single `obj |> transform_fun(fun = exp, constant = 5)` instead of
  separate calls to `add_constant()` and then `transform_fun()`. Part of the
  discussion of #79

* `model_constant()` is a new function that simply extracts the first
  coefficient from the estimated model.

## Bug fixes

* `link()`, `inv_link()`, and related family functions for the `ocat()` weren't
  correctly identifying the family name and as a result would throw an error
  even when passed an object of the correct family.

  `link()` and `inv_link()` now work correctly for the `betar()` family in a
  fitted GAM.

* The `print()` method for `lp_matrix()` now converts the matrix to a data frame
  before conversion to a tibble. This makes more sense as it results in more
  typical behaviour as the columns of the printed object are doubles.

* Constrained factor smooths (`bs = "sz"`) where the factor is not the first
  variable mentioned in the smooth (i.e. `s(x, f, bs = "sz")` for continuous
  `x` and factor `f`) are now plotable with `draw()`. #208

* `parametric_effects()` was unable to handle special parametric terms like
  `poly(x)` or `log(x)` in formulas. Reported by @fhui28 #212

* `parametric_effects()` now works better for location, scale, shape models.
  Reported by @pboesu #45

* `parametric_effects` now works when there are missing values in one or more
  variables used in a fitted GAM. #219

* `response_derivatives()` was incorrectly using `.data` with *tidyselect*
  selectors.

* `typical_values()` could not handle logical variables in a GAM fit as mgcv
  stores these as numerics in the `var.summary`. This affected `evenly()` and
  `data_slice()`. #222

* `parametric_effects()` would fail when two or more ordered factors were in
  the model. Reported by @dsmi31 #221

* Continuous by smooths were being evaluated with the median value of the `by`
  variable instead of a value of 1. #224

* `fitted_samples()` (and hence `posterior_samples()`) now handles models with
  offset terms in the formula. Offset terms supplied via the `offset` argument
  are ignored by `mgcv:::predict.gam()` and hence are ignored also by `gratia`.
  Reported by @jonathonmellor #231 #233

* `smooth_estimates()` would fail on a `"fs"` smooth when a multivariate base
  smoother was used *and* the factor was not the last variable specified in the
  definition of the smooth: `s(x1, x2, f, bs = "fs", xt = list(bs = "ds"))`
  would work, but `s(f, x1, x2, bs = "fs", xt = list(bs = "ds"))` (or any
  ordering of variables that places the factor not last) would emit an obscure
  error. The ordering of the terms involved in the smooth now doesn't matter.
  Reported by @chrisaak #249.

* `draw.gam()` would fail when plotting a multivariate base smoother used in an
  `"sz"` smooth. Now, this use case is identified and a message printed
  indicating that (currently) gratia doesn't know how to plot such a smooth.
  Reported by @chrisaak #249.

* `draw.gam()` would fail when plotting a multivariate base smoother used in an
  `"fs"` smooth. Now, this use case is identified and a message printed
  indicating that (currently) gratia doesn't know how to plot such a smooth.
  Reported by @chrisaak #249.

* `derivative_samples()` would fail with `order = 2` and was only computing
  forward finite differences, regardless of `type` for `order = 1`. Partly
  reported by @samlipworth #251.

* The `draw()` method for `penalty()` was normalizing the penalty to the range
  0--1, not the claimed and documented -1--1 with argument `normalize = TRUE`.
  This is now fixed.

* `smooth_samples()` was failing when `data` was supplied that contained more
  variables than were used in the smooth that was being sampled. Hence this
  generally fail unless a single smooth was being sampled from or the model
  contained only a single smooth. The function never intended to retain all the
  variables in `data` but was written in such a way that it would fail when
  relocating the data columns to the end of the posterior sampling object. #255

* `draw.gam()` and `draw.smooth_estimates()` would fail when plotting a
  univariate tensor product smooth (e.g. `te(x)`, `ti(x)`, or `t2()`). Reported
  by @wStockhausen #260

* `plot.smooth()` was not printing the factor level in subtitles for ordered 
  factor by smooths.

# gratia 0.8.2

* Small fixes for CRAN.

# gratia 0.8.1

## User visible changes

* `smooth_samples()` now returns objects with variables involved in smooths
  that have their correct name. Previously variables were named `.x1`, `.x2`,
  etc. Fixing #126 and improving compatibility with `compare_smooths()` and
  `smooth_estimates()` allowed the variables to be named correctly.

* *gratia* now depends on version 1.8-41 or later of the *mgcv* package.

## New features

* `draw.gam()` can now handle tensor products that include a marginal random
  effect smooth. Beware plotting such smooths if there are many levels,
  however, as a separate surface plot will be produced for each level.

## Bug fixes

* Additional fixes for changes in dplyr 1.1.0.

* `smooth_samples()` now works when sampling from posteriors of multiple smooths
  with different dimension. #126 reported by @Aariq

# gratia 0.8.0

## User visible changes

* {gratia} now depends on R version 4.1 or later.

* A new vignette "Data slices" is supplied with {gratia}.

* Functions in {gratia} have harmonised to use an argument named `data` instead
  of `newdata` for passing new data at which to evaluate features of smooths. A
  message will be printed if `newdata` is used from now on. Existing code does
  not need to be changed as `data` takes its value from `newdata`.
  
  Note that due to the way `...` is handled in R, if your R script uses the
  `data` argument, *and* is run with versions of gratia prior to 8.0 (when
  released; 0.7.3.8 if using the development version) the user-supplied data
  will be silently ignored. As such, scripts using `data` should check that the
  installed version of gratia is >= 0.8 and package developers should update
  to depend on versions >= 0.8 by using `gratia (>= 0.8)` in `DESCRIPTION`.

* The order of the plots of smooths has changed in `draw.gam()` so that they
  again match the order in which smooths were specified in the model formula.
  See *Bug Fixes* below for more detail or #154.

## New features

* Added basic support for GAMLSS (distributional GAMs) fitted with the
  `gamlss()` function from package GJRM. Support is currently restricted to a
  `draw()` method.

* `difference_smooths()` can now include the group means in the difference,
  which many users expected. To include the group means use `group_means = TRUE`
  in the function call, e.g.
  `difference_smooths(model, smooth = "s(x)", group_means = TRUE`). Note: this
  function still differs from `plot_diff()` in package *itsadug*, which
  essentially computes differences of model predictions. The main practical
  difference is that other effects beyond the factor by smooth, including random
  effects, may be included with `plot_diff()`.

  This implements the main wish of #108 (@dinga92) and #143 (@mbolyanatz)
  despite my protestations that this was complicated in some cases (it isn't;
  the complexity just cancels out.)

* `data_slice()` has been totally revised. Now, the user provides the values for
  the variables they want in the slice and any variables in the model that are
  not specified will be held at typical values (i.e. the value of the
  observation that is closest to the median for numeric variables, or the modal
  factor level.)

  Data slices are now produced by passing `name` = `value` pairs for the
  variables and their values that you want to appear in the slice. For example

  ```
  m <- gam(y ~ s(x1) + x2 + fac)
  data_slice(model, x1 = evenly(x1, n = 100), x2 = mean(x2))
  ```

  The `value` in the pair can be an expression that will be looked up
  (evaluated) in the `data` argument or the model frame of the fitted model
  (the default). In the above example, the resulting slice will be a data frame
  of 100 observations, comprising `x1`, which is a vector of 100 values spread
  evenly over the range of `x1`, a constant value of the mean of `x2` for the
  `x2` variable, and a constant factor level, the model class of `fac`, for the
  `fac` variable of the model.

* `partial_derivatives()` is a new function for computing partial derivatives
  of multivariate smooths (e.g. `s(x,z)`, `te(x,z)`) with respect to one of
  the margins of the smooth. Multivariate smooths of any dimension are handled,
  but only one of the dimensions is allowed to vary. Partial derivatives are
  estimated using the method of finite differences, with forward, backward,
  and central finite differences available. Requested by @noamross #101

* `overview()` provides a simple overview of model terms for fitted GAMs.

* The new `bs = "sz"` basis that was released with *mgcv* version 1.18-41 is
  now supported in `smooth_estimates()`, `draw.gam()`, and
  `draw.smooth_estimates()` and this basis has its own unique plotting method.
  #202

* `basis()` now has a method for fitted GAM(M)s which can extract the estimated
  basis from the model and plot it, using the estimated coefficients for the
  smooth to weight the basis. #137

  There is also a new `draw.basis()` method for plotting the results of a call
  to `basis()`. This method can now also handle bivariate bases.

  `tidy_basis()` is a lower level function that does the heavy lifting in
  `basis()`, and is now exported. `tidy_basis()` returns a tidy representation
  of a basis supplied as an object inheriting from class `"mgcv.smooth"`. These
  objects are returned in the `$smooth` component of a fitted GAM(M) model.

* `lp_matrix()` is a new utility function to quickly return the linear predictor
  matrix for an estimated model. It is a wrapper to
  `predict(..., type = "lpmatrix")`

* `evenly()` is a synonym for `seq_min_max()` and is preferred going forward.
  Gains argument `by` to produce sequences over a covariate that increment in
  units of `by`.

* `ref_level()` and `level()` are new utility functions for extracting the
  reference or a specific level of a factor respectively. These will be most
  useful when specifying covariate values to condition on in a data slice.

* `model_vars()` is a new, public facing way of returning a vector of variables
  that are used in a model.

* `difference_smooths()` will now use the user-supplied data as points at
  which to evaluate a pair of smooths. Also note that the argument `newdata` has
  been renamed `data`. #175

* The `draw()` method for `difference_smooths()` now uses better labels for
  plot titles to avoid long labels with even modest factor levels.

* `derivatives()` now works for factor-smooth interaction (`"fs"`) smooths.

* `draw()` methods now allow the angle of tick labels on the x axis of plots to
  be rotated using argument `angle`. Requested by @tamas-ferenci #87

* `draw.gam()` and related functions (`draw.parametric_effects()`,
  `draw.smooth_estimates()`) now add the basis to the plot using a caption.
  #155

* `smooth_coefs()` is a new utility function for extracting the coefficients
  for a particular smooth from a fitted model. `smooth_coef_indices()` is an
  associated function that returns the indices (positions) in the vector of
  model coefficients (returned by `coef(gam_model)`) of those coefficients that
  pertain to the stated smooth.

* `draw.gam()` now better handles patchworks of plots where one or more of
  those plots has fixed aspect ratios. #190

## Bug fixes

* `draw.posterior_smooths` now plots posterior samples with a fixed aspect ratio
  if the smooth is isotropic. #148

* `derivatives()` now ignores random effect smooths (for which derivatives
  don't make sense anyway). #168

* `confint.gam(...., method = "simultaneous")` now works with factor by smooths
  where `parm` is passed the full name of a specific smooth `s(x)faclevel`.

* The order of plots produced by `gratia::draw.gam()` again matches the order
  in which the smooths entered the model formula. Recent changes to the
  internals of `gratia::draw.gam()` when the switch to `smooth_estimates()` was
  undertaken lead to a change in behaviour resulting from the use of
  `dplyr::group_split()`, and it's coercion internally of a character vector to
  a factor. This factor is now created explicitly, and the levels set to the
  correct order. #154

* Setting the `dist` argument to set response or smooth values to `NA` if they
  lay too far from the support of the data in multivariate smooths, this would
  lead an incorrect scale for the response guide. This is now fixed. #193

* Argument `fun` to `draw.gam()` was not being applied to any parametric terms.
  Reported by @grasshoppermouse #195

* `draw.gam()` was adding the uncertainty for all linear predictors to smooths
  when `overall_uncertainty = TRUE` was used. Now `draw.gam()` only includes the
  uncertainty for those linear predictors in which a smooth takes part. #158

* `partial_derivatives()` works when provided with a single data point at
  which to evaluate the derivative. #199

* `transform_fun.smooth_estimates()` was addressing the wrong variable names
  when trying to transform the confidence interval. #201

* `data_slice()` doesn't fail with an error when used with a model that contains
  an offset term. #198

* `confint.gam()` no longer uses `evaluate_smooth()`, which is soft deprecated.
  #167

* `qq_plot()` and `worm_plot()` could compute the wrong deviance residuals used
  to generate the theoretical quantiles for some of the more exotic families
  (distributions) available in *mgcv*. This also affected `appraise()` but only
  for the QQ plot; the residuals shown in the other plots and the deviance
  residuals shown on the y-axis of the QQ plot were correct. Only the
  generation of the reference intervals/quantiles was affected.

# gratia 0.7.3

## User visible changes

* Plots of smooths now use "Partial effect" for the y-axis label in place of
  "Effect", to better indicate what is displayed.

## New features

* `confint.fderiv()` and `confint.gam()` now return their results as a tibble
  instead of a common-or-garden data frame. The latter mostly already did this.

* Examples for `confint.fderiv()` and `confint.gam()` were reworked, in part to
  remove some inconsistent output in the examples when run on M1 macs.

## Bug fixes

* `compare_smooths()` failed when passed non-standard model "names" like
  `compare_smooths(m_gam, m_gamm$gam)` or `compare_smooths(l[[1]], l[[2]])`
  even if the evaluated objects were valid GAM(M) models. Reported by Andrew
  Irwin #150

# gratia 0.7.2

## New features

* `draw.gam()` and `draw.smooth_estimates()` can now handle splines on the
  sphere (`s(lat, long, bs = "sos")`) with special plotting methods using
  `ggplot2::coord_map()` to handle the projection to spherical coordinates. An
  orthographic projection is used by default, with an essentially arbitrary
  (and northern hemisphere-centric) default for the orientation of the view.

* `fitted_values()` insures that `data` (and hence the returned object) is a
  tibble rather than a common or garden data frame.

## Bug fixes

* `draw.posterior_smooths()` was redundantly plotting duplicate data in the rug
  plot. Now only the unique set of covariate values are used for drawing the
  rug.

* `data_sim()` was not passing the `scale` argument in the bivariate example
  setting (`"eg2"`).

* `draw()` methods for `gamm()` and `gamm4::gamm4()` fits were not passing 
  arguments on to `draw.gam()`.

* `draw.smooth_estimates()` would produce a subtitle with data for a continuous
  by smooth as if it were a factor by smooth. Now the subtitle only contains the
  name of the continuous by variable.

# gratia 0.7.1

Due to an issue with the size of the package source tarball, which wasn't
discovered until after submission to CRAN, 0.7.1 was never released.

## New features

* `draw.gam()` and `draw.smooth_estimates()`: {gratia} can now handle smooths
  of 3 or 4 covariates when plotting. For smooths of 3 covariates, the third
  covariate is handled with `ggplot2::facet_wrap()` and a set (default `n` = 16)
  of small multiples is drawn, each a 2d surface evaluated at the specified
  value of the third covariate. For smooths of 4 covariates,
  `ggplot2::facet_grid()` is used to draw the small multiples, with the default
  producing 4 rows by 4 columns of plots at the specific values of the third
  and fourth covariates. The number of small multiples produced is controlled
  by new arguments `n_3d` (default = `n_3d = 16`) and `n_4d` (default
  `n_4d = 4`, yielding `n_4d * n_4d` = 16 facets) respectively.

  This only affects plotting; `smooth_estimates()` has been able to handle
  smooths of any number of covariates for a while.

  When handling higher-dimensional smooths, actually drawing the plots on the
  default device can be slow, especially with the default value of `n = 100`
  (which for 3D or 4D smooths would result in 160,000 data points being
  plotted). As such it is recommended that you reduce `n` to a smaller value:
  `n = 50` is a reasonable compromise of resolution and speed.

* `model_concurvity()` returns concurvity measures from `mgcv::concurvity()`
  for estimated GAMs in a tidy format. The synonym `concrvity()` is also
  provided. A `draw()` method is provided which produces a bar plot or a heatmap
  of the concurvity values depending on whether the overall concurvity of each
  smooth or the pairwise concurvity of each smooth in the model is requested.

* `draw.gam()` gains argument `resid_col = "steelblue3"` that allows the colour
  of the partial residuals (if plotted) to be changed.

## Bug fixes

* `model_edf()` was not using the `type` argument. As a result it only ever
   returned the default EDF type.

* `add_constant()` methods weren't applying the constant to all the required
  variables.

* `draw.gam()`, `draw.parametric_effects()` now actually work for a model with
  only parametric effects. #142 Reported by @Nelson-Gon

* `parametric_effects()` would fail for a model with only parametric terms
  because `predict.gam()` returns empty arrays when passed 
  `exclude = character(0)`.

# gratia 0.7.0

## Major changes

* `draw.gam()` now uses `smooth_estimates()` internally and consequently uses
  its `draw()` method and underlying plotting code. This has simplified the code
  compared to `evaluate_smooth()` and its methods, which will allow for future
  development and addition of features more easily than if `evaluate_smooth()`
  had been retained.

  Similarly, `evaluate_parametric_terms()` is now deprecated in favour of
  `parametric_effects()`, which is also used internally by `draw.gam()` if
  parametric terms are present in the model (and `parametric = TRUE`).

  While a lot of code has been reused so differences between plots as a result
  of this change should be minimal, some corner cases may have been missed. File
  an Issue if you notice something that has changed that you think shouldn't.

* `draw.gam()` now plots 2D isotropic smooths (TPRS and Duchon splines) with
  equally-scaled x and y coordinates using `coord_equal(ratio = 1)`. Alignment
  of these plots will be a little different now when plotting models with
  multiple smooths. See Issue #81.

### Deprecated functions

From version 0.7.0, the following functions are considered deprecated and their
use is discouraged:

* `fderiv()` is *soft*-deprecated in favour of `derivatives()`,
* `evaluate_smooth()` is *soft*-deprecated in favour of `smooth_estimates()`,
* `evaluate_parametric_term()` is *soft*-deprecated in favour of
  `parametric_effects()`.

The first call to one of these functions will generate a warning, pointing to
the newer, alternative, function. It is safe to ignore these warnings, but
these deprecated functions will no longer receive updates and are thus at risk
of being removed from the package at some future date. The newer alternatives
can handle more types of models and smooths, especially so in the case of
`smooth_estimates()`.

## New features

* `fitted_values()` provides a tidy wrapper around `predict.gam()` for
  generating fitted values from the model. New covariate values can be provided
  via argument `data`. A credible interval on the fitted values is returned, and
  values can be on the link (linear predictor) or response scale.

  Note that this function returns expected values of the response. Hence,
  "fitted values" is used instead of "predictions" in the case of new covariate
  values to differentiate these values from the case of generating new response
  values from a fitted model.

* `rootogram()` and its `draw()` method produce rootograms as diagnostic plots
  for fitted models. Currently only for models fitted with `poisson()`,
  `nb()`, `negbin()`, `gaussian()` families.

* New helper functions `typical_values()`, `factor_combos()` and
  `data_combos()` for quickly creating data sets for producing predictions from
  fitted models where some covariatess are fixed at come typical or
  representative values.

    `typical_values()` is a new helper function to return typical values for the
    covariates of a fitted model. It returns the value of the observation
    closest to the median for numerical covariates or the modal level of a
    factor while preserving the levels of that factor. `typical_values()` is
    useful in preparing data slices or scenarios for which fitted values from
    the estimated model are required.
    
    `factor_combos()` extracts and returns the combinations of levels of factors
    found in data used to fit a model. Unlike `typical_values()`,
    `factor_combos()` returns all the combinations of factor levels observed
    in the data, not just the modal level. Optionally, all combinations of
    factor levels can be returned, not just those in the observed data.
    
    `data_combos()` combines returns the factor data from `factor_combos()` plus
    the typical values of numerical covariates. This is useful if you want to
    generate predictions from the model for each combination of factor terms
    while holding any continuous covariates at their median values.

* `nb_theta()` is a new extractor function that returns the theta parameter of
  a fitted negative binomial GAM (families `nb()` or `negbin()`). Additionally,
  `theta()` and `has_theta()` provide additional functionality. `theta()` is an
  experimental function for extracting any additional parameters from the model
  or family. `has_theta()` is useful for checking if any additional parameters
  are available from the family or model. 

* `edf()` extracts the effective degrees of freedom (EDF) of a fitted model or a
  specific smooth in the model. Various forms for the EDF can be extracted.

* `model_edf()` returns the EDF of the overall model. If supplied with multiple
  models, the EDFs of each model are returned for comparison.

* `draw.gam()` can now show a "rug" plot on a bivariate smooth by drawing small
  points with high transparency over the smooth surface at the data coordinates.

  In addition, the rugs on plots of factor by smooths now show the locations of
  covariate values for the specific level of the factor and not over all levels.
  This better reflects what data were used to estimate the smooth, even though
  the basis for each smooth was set up using all of the covariate locations.

* `draw.gam()` and `draw.smooth_estimates()` now allow some aspects of the plot
  to be changed: the fill (but not colour) and alpha attributes of the credible
  interval, and the line colour for the smooth can now be specified using
  arguments `ci_col`, `ci_alpha`, and `smooth_col` respectively.

* Partial residuals can now be plotted on factor by smooths. To allow this, the
  partial residuals are filtered so that only residuals associated with a
  particular level's smooth are drawn on the plot of the smooth.

* `smooth_estimates()` uses `check_user_select_smooths()` to handle
  user-specified selection of smooth terms. As such it is more flexible than
  previously, and allows for easier selection of smooths to evaluate.

* `fixef()` is now imported (and re-exported) from the *nlme* package, with
  methods for models fitted with `gam()` and `gamm()`, to extract fixed effects
  estimates from fitted models. `fixed_effects()` is an alias for `fixef()`.

* The `draw()` method for `smooth_samples()` can now handle 2D smooths.
  Additionally, the number of posterior draws to plot can now be specified when
  plotting using new argument `n_samples`, which will result in `n_samples`
  draws being selected at random from the set of draws for plotting. New
  argument `seed` allows the selection of draws to be repeatable.

## Bug fixes

* `smooth_estimates()` was not filtering user-supplied data for the by level of
  the specific smooth when used with by factor smooths. This would result in the
  smooth being evaluated at all rows of the user-supplied data, and therefore
  would result in `nrow(user_data) * nlevels(by_variable)` rows in the returned
  object instead of `nrow(user_data)` rows.

* The `add_confint()` method for `smooth_estimates()` had the upper and lower
  intervals reversed. #107 Reported by @Aariq

* `draw.gam()` and `smooth_estimates()` were both ignoring the `dist` argument
  that allows covariate values that lie too far from the support of the data to
  be excluded when returning estimated values from the smooth and plotting it.
  #111 Reported by @Aariq

* `smooth_samples()` with a factor by GAM would return samples for the first
  factor level only. Reported by @rroyaute in discussion of #121

* `smooth_samples()` would fail if the model contained random effect "smooths".
  These are now ignored with a message when running `smooth_samples()`.
  Reported by @isabellaghement in #121

* `link()`, `inv_link()` were failing on models fitted with `family = scat()`.
  Reported by @Aariq #130

# gratia 0.6.0

## Major changes

* The {cowplot} package has been replaced by the {patchwork} package for
  producing multi-panel figures in `draw()` and `appraise()`. This shouldn't
  affect any code that used {gratia} only, but if you passed additional
  arguments to `cowplot::plot_grid()` or used the `align` or `axis` arguments of
  `draw()` and `appraise()`, you'll need to adapt code accordingly.

  Typically, you can simply delete the `align` or `axis` arguments and
  {patchwork} will just work and align plots nicely. Any arguments passed via
  `...` to `cowplot::plot_grid()` will just be ignored by
  `patchwork::wrap_plots()` unless those passed arguments match any of the
  arguments of `patchwork::wrap_plots()`.

## New features

* The {patchwork} package is now used for multi-panel figures. As such, {gratia}
  no longer Imports from the {cowplot} package.

* Worm plot diagnostic plots are available via new function `worm_plot()`. Worm
  plots are detrended Q-Q plots, where deviation from the Q-Q reference line are
  emphasized as deviations around the line occupy the full height of the plot.

  `worm_plot()` methods are available for models of classes `"gam"`, `"glm"`,
  and `"lm"`. (#62)

* Smooths can now be compared across models using `compare_smooths()`, and
  comparisons visualised with the associated `draw()` method. (#85 @dill)

  This feature is a bit experimental; the returned object uses nested lists and
  may change in the future if users find this confusing.

* The reference line in `qq_plot()` with `method = "normal"` was previously
  drawn as a line with intercept 0 and slope 1, to match the other methods. This
  was inconsistent with `stats::qqplot()` which drew the line through the 1st
  and 3rd quartiles. `qq_plot()` with `method = "normal"` now uses this robust
  reference line. Reference lines for the other methods remain drawn with slope
  1 and intercept 0.

* `qq_plot()` with `method = "normal"` now draws a point-wise reference band
  using the standard error of the order statistic.

* The `draw()` method for `penalty()` now plots the penalty matrix heatmaps in a
  more-logical orientation, to match how the matrices might be written down or
  printed to the R console.

* `link()`, and `inv_link()` now work for models fitted with the `gumbls()` and
  `shash()` families. (#84)

* `extract_link()` is a lower level utility function related to `link()` and
  `inv_link()`, and is now exported.

## User visible changes

* The default method name for generating reference quantiles in `qq_plot()` was
  changed from `"direct"` to `"uniform"`, to avoid confusion with the
  `mgcv::qq.gam()` help page description of the methods. Accordingly using
  `method = "direct"` is deprecated and a message to this effect is displayed if
  used.

* The way smooths/terms are selected in `derivatives()` has been switched to use
  the same mechanism as `draw.gam()`'s `select` argument. To get a partial match
  to `term`, you now need to also specify `partial_match = TRUE` in the call to
  `derivatives()`.

## Bug fixes

* `transform_fun()` had a copy paste bug in the definition of the then generic.
  (#96 @Aariq)

* `derivatives()` with user-supplied `newdata` would fail for factor by smooths
  with `interval = "simultaneous"` and would introduce rows with derivative == 0
  with `interval = "confidence"` because it didn't subset the rows of `newdata`
  for the specific level of the by factor when computing derivatives.
  (#102 @sambweber)

* `evaluate_smooth()` can now handle random effect smooths defined using an
  ordered factor. (#99 @StefanoMezzini)

# gratia 0.5.1

## New features

* `smooth_estimates()` can now handle

    * bivariate and multivariate thinplate regression spline smooths, e.g. 
      `s(x, z, a)`,
    * tensor product smooths (`te()`, `t2()`, & `ti()`), e.g. `te(x, z, a)`
    * factor smooth interactions, e.g. `s(x, f, bs = "fs")`
    * random effect smooths, e.g. `s(f, bs = "re")`

* `penalty()` provides a tidy representation of the penalty matrices of
  smooths. The tidy representation is most suitable for plotting with
  `ggplot()`.

  A `draw()` method is provided, which represents the penalty matrix as a
  heatmap.

## User visible changes

* The `newdata` argument to `smooth_estimates()` has been changed to `data` as
  was originally intended.

# gratia 0.5.0

## New features

* Partial residuals for models can be computed with `partial_residuals()`. The
  partial residuals are the weighted residuals of the model added to the
  contribution of each smooth term (as returned by `predict(model,
  type = "terms")`.
  
  Wish of [#76](https://github.com/gavinsimpson/gratia/issues/76) (@noamross)

  Also, new function `add_partial_residuals()` can be used to add the partial
  residuals to data frames.

* Users can now control to some extent what colour or fill scales are used when
  plotting smooths in those `draw()` methods that use them. This is most useful
  to change the fill scale when plotting 2D smooths, or to change the discrete
  colour scale used when plotting random factor smooths (`bs = "fs"`).
  
  The user can pass scales via arguments `discrete_colour` and
  `continuous_fill`.

* The effects of certain smooths can be excluded from data simulated from a
  model using `simulate.gam()` and `predicted_samples()` by passing `exclude` or
  `terms` on to `predict.gam()`. This allows for excluding random effects, for
  example, from model predicted values that are then used to simulate new data
  from the conditional distribution. See the example in `predicted_samples()`.
  
  Wish of [#74](https://github.com/gavinsimpson/gratia/issues/74) (@hgoldspiel)

* `draw.gam()` and related functions gain arguments `constant` and `fun` to
  allow for user-defined constants and transformations of smooth estimates and
  confidence intervals to be applied.
  
  Part of wish of Wish of
  [#79](https://github.com/gavinsimpson/gratia/issues/79).

* `confint.gam()` now works for 2D smooths also.

* `smooth_estimates()` is an early version of code to replace (or more likely
  supersede) `evaluate_smooth()`. `smooth_estimates()` can currently only handle
  1D smooths of the standard types. 

## User visible changes

* The meaning of `parm` in `confint.gam` has changed. This argument now requires
  a smooth label to match a smooth. A vector of labels can be provided, but
  partial matching against a smooth label only works with a single `parm` value.
  
  The default behaviour remains unchanged however; if `parm` is `NULL` then all
  smooths are evaluated and returned with confidence intervals.

* `data_class()` is no longer exported; it was only ever intended to be an
  internal function.

## Bug Fixes

* `confint.gam()` was failing on a tensor product smooth due to matching issues.
  Reported by @tamas-ferenci
  [#88](https://github.com/gavinsimpson/gratia/issues/88)
  
  This also fixes [#80](https://github.com/gavinsimpson/gratia/issues/80)
  (@butellyn) which was a related issue with selecting a specific smooth.

* The **vdiffr** package is now used conditionally in package tests.
  Reported by Brian Ripley
  [#93](https://github.com/gavinsimpson/gratia/issues/93)

# gratia 0.4.1

## User visible changes

* `draw.gam()` with `scales = "fixed"` now applies to all terms that can be
  plotted, including 2d smooths.

  Reported by @StefanoMezzini
  [#73](https://github.com/gavinsimpson/gratia/issues/73)

## Bug fixes

* `dplyr::combine()` was deprecated. Switch to `vctrs::vec_c()`.

* `draw.gam()` with `scales = "fixed"` wasn't using fixed scales where 2d
  smooths were in the model.

  Reported by @StefanoMezzini
  [#73](https://github.com/gavinsimpson/gratia/issues/73)

# gratia 0.4.0

## New features

* `draw.gam()` can include partial residuals when drawing univariate smooths.
  Use `residuals = TRUE` to add partial residuals to each univariate smooth that
  is drawn. This feature is not available for smooths of more than one variable,
  by smooths, or factor-smooth interactions (`bs = "fs"`).

* The coverage of credible and confidence intervals drawn by `draw.gam()` can be
  specified via argument `ci_level`. The default is arbitrarily `0.95` for no
  other reason than (rough) compatibility with `plot.gam()`.
  
  This change has had the effect of making the intervals slightly narrower than
  in previous versions of *gratia*; intervals were drawn at &plusmn; 2 &times;
  the standard error. The default intervals are now drawn at &plusmn; ~1.96
  &times; the standard error.

* New function `difference_smooths()` for computing differences between factor
  smooth interactions. Methods available for `gam()`, `bam()`, `gamm()` and
  `gamm4::gamm4()`. Also has a `draw()` method, which can handle differences of
  1D and 2D smooths currently (handling 3D and 4D smooths is planned).

* New functions `add_fitted()` and `add_residuals()` to add fitted values
  (expectations) and model residuals to an existing data frame. Currently
  methods available for objects fitted by `gam()` and `bam()`.

* `data_sim()` is a tidy reimplementation of `mgcv::gamSim()` with the added
  ability to use sampling distributions other than the Gaussian for all models
  implemented. Currently Gaussian, Poisson, and Bernoulli sampling distributions
  are available.

* `smooth_samples()` can handle continuous by variable smooths such as in
  varying coefficient models.

* `link()` and `inv_link()` now work for all families available in *mgcv*,
  including the location, scale, shape families, and the more specialised
  families described in `?mgcv::family.mgcv`.

* `evaluate_smooth()`, `data_slice()`, `family()`, `link()`, `inv_link()`
  methods for models fitted using `gamm4()` from the **gamm4** package.

* `data_slice()` can generate data for a 1-d slice (a single variable varying).

* The colour of the points, reference lines, and simulation band in `appraise()`
  can now be specified via arguments

    * `point_col`,
    * `point_alpha`,
    * `ci_col`
    * `ci_alpha`
    * `line_col`

  These are passed on to `qq_plot()`, `observed_fitted_plot()`,
  `residuals_linpred_plot()`, and `residuals_hist_plot()`, which also now take
  the new arguments were applicable.

* Added utility functions `is_factor_term()` and `term_variables()` for working
  with models. `is_factor_term()` identifies is the named term is a factor using
  information from the `terms()` object of the fitted model. `term_variables()`
  returns a character vector of variable names that are involved in a model
  term. These are strictly for working with parametric terms in models.

* `appraise()` now works for models fitted by `glm()` and `lm()`, as do the
  underlying functions it calls, especially `qq_plot`.
  
  `appraise()` also works for models fitted with family `gaulss()`. Further
  location scale models and models fitted with extended family functions will
  be supported in upcoming releases.

## User visible changes

* `datagen()` is now an *internal* function and is no longer exported. Use
  `data_slice()` instead.

* `evaluate_parametric_term()` is now much stricter and can only evaluate main
  effect terms, i.e. those whose order, as stored in the `terms` object of the
  model is `1`.

## Bug fixes

* The `draw()` method for `derivatives()` was not getting the x-axis label for
  factor by smooths correctly, and instead was using `NA` for the second and
  subsequent levels of the factor.

* The `datagen()` method for class `"gam"` couldn't possibly have worked for
  anything but the simplest models and would fail even with simple factor by
  smooths. These issues have been fixed, but the behaviour of `datagen()` has
  changed, and the function is now not intended for use by users.

* Fixed an issue where in models terms of the form `factor1:factor2` were
  incorrectly identified as being numeric parametric terms.
  [#68](https://github.com/gavinsimpson/gratia/issues/68)

# gratia 0.3.1

## New features

* New functions `link()` and `inv_link()` to access the link function and its
  inverse from fitted models and family functions.
  
  Methods for classes: `"glm"`, `"gam"`, `"bam"`, `"gamm"` currently.
  [#58](https://github.com/gavinsimpson/gratia/issues/58)

* Adds explicit `family()` methods for objects of classes `"gam"`, `"bam"`, and
  `"gamm"`.

* `derivatives()` now handles non-numeric when creating shifted data for finite
  differences. Fixes a problem with `stringsAsFactors = FALSE` default in
  R-devel. [#64](https://github.com/gavinsimpson/gratia/issues/64)

## Bug fixes

* Updated *gratia* to work with *tibble* versions >= 3.0

# gratia 0.3.0

## New features

* *gratia* now uses the *mvnfast* package for random draws from a multivariate
  normal distribution (`mvnfast::rmvn()`). Contributed by Henrik Singmann
  (@singmann) [#28](https://github.com/gavinsimpson/gratia/issues/28)

* New function `basis()` for generating tidy representations of basis expansions
  from an *mgcv*-like definition of a smooth, e.g. `s()`, `te()`, `ti()`, or
  `t2()`. The basic smooth types also have a simple `draw()` method for plotting
  the basis. `basis()` is a simple wrapper around `mgcv::smoothCon()` with some
  post processing of the basis model matrix into a tidy format.
  [#42](https://github.com/gavinsimpson/gratia/issues/42)

* New function `smooth_samples()` to draw samples of entire smooth functions
  from their posterior distribution. Also has a `draw()` method for plotting the
  posterior samples.

## Bug fixes

* `draw.gam()` would produce empty plots between the panels for the parametric
    terms if there were 2 or more parametric terms in a model. Reported by
    @sklayn [#39](https://github.com/gavinsimpson/gratia/issues/39).

* `derivatives()` now works with factor by smooths, including ordered factor by
    smooths. The function also now works correctly for complex models with
    multiple covariates/smooths.
    [#47](https://github.com/gavinsimpson/gratia/issues/47)

    `derivatives()` also now handles `'fs'` smooths.  Reported by
    @tomand-uio [#57](https://github.com/gavinsimpson/gratia/issues/57).

* `evaluate_parametric_term()` and hence `draw.gam()` would fail on a `ziplss()`
  model because i) *gratia* didn't handle parametric terms in models with
  multiple linear predictors correctly, and ii) *gratia* didn't convert to the
  naming convention of *mgcv* for terms in higher linear predictors. Reported by
  @pboesu [#45](https://github.com/gavinsimpson/gratia/issues/45)
