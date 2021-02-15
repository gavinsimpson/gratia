# gratia 0.5.1.9001

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
  and `"lm"`.

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

## Bug fixes

* `transform_fun()` had a copy paste bug in the definition of the then generic.
  (#96 @Aariq)

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
  contribution of each smooth term (as returned by `predict(model, type = "terms")`.
  
  Wish of [#76](https://github.com/gavinsimpson/gratia/issues/76) (@noamross)

  Also, new function `add_partial_residuals()` can be used to add the partial
  residuals to data frames.

* Users can now control to some extent what colour or fill scales are used when
  plotting smooths in those `draw()` methods that use them. This is most useful
  to change the fill scale when plotting 2D smooths, or to change the discrete
  colour scale used when plotting random factor smooths (`bs = "fs"`).
  
  The user can pass scales via arguments `discrete_colour` and
  `continuous_fill`.

* The effects of certain smooths can be excluded from data simulated from a model
  using `simulate.gam()` and `predicted_samples()` by passing `exclude` or `terms`
  on to `predict.gam()`. This allows for excluding random effects, for example, from
  model predicted values that are then used to simulate new data from the conditional
  distribution. See the example in `predicted_samples()`.
  
  Wish of [#74](https://github.com/gavinsimpson/gratia/issues/74) (@hgoldspiel)

* `draw.gam()` and related functions gain arguments `constant` and `fun` to allow
  for user-defined constants and transformations of smooth estimates and
  confidence intervals to be applied.
  
  Part of wish of Wish of [#79](https://github.com/gavinsimpson/gratia/issues/79).

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

* `data_class()` is no longer exported; it was only ever intended to be an internal
  function.

## Bug Fixes

* `confint.gam()` was failing on a tensor product smooth due to matching issues.
  Reported by @tamas-ferenci [#88](https://github.com/gavinsimpson/gratia/issues/88)
  
  This also fixes [#80](https://github.com/gavinsimpson/gratia/issues/80)
  (@butellyn) which was a related issue with selecting a specific smooth.

* The **vdiffr** package is now used conditionally in package tests.
  Reported by Brian Ripley [#93](https://github.com/gavinsimpson/gratia/issues/93)

# gratia 0.4.1

## User visible changes

* `draw.gam()` with `scales = "fixed"` now applies to all terms that can be
  plotted, including 2d smooths.

  Reported by @StefanoMezzini [#73](https://github.com/gavinsimpson/gratia/issues/73)

## Bug fixes

* `dplyr::combine()` was deprecated. Switch to `vctrs::vec_c()`.

* `draw.gam()` with `scales = "fixed"` wasn't using fixed scales where 2d smooths
  were in the model.

  Reported by @StefanoMezzini [#73](https://github.com/gavinsimpson/gratia/issues/73)

# gratia 0.4.0

## New features

* `draw.gam()` can now include partial residuals when drawing univariate smooths.
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
  (expectations) and model residuals to an existing data frame. Currently methods
  available for objects fitted by `gam()` and `bam()`.

* `data_sim()` is a tidy reimplementation of `mgcv::gamSim()` with the added
  ability to use sampling distributions other than the Gaussian for all models
  implemented. Currently Gaussian, Poisson, and Bernoulli sampling distributions
  are available.

* `smooth_samples()` can handle continuous by variable smooths such as in
  varying coefficient models.

* `link()` and `inv_link()` now work for all families available in *mgcv*,
  including the location, scale, shape families, and the more specialised
  families described in `?mgcv::family.mgcv`.

* `evaluate_smooth()`, `data_slice()`, `family()`, `link()`, `inv_link()` methods
  for models fitted using `gamm4()` from the **gamm4** package.

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
  differences. Fixes a problem with `stringsAsFactors = FALSE` default in R-devel.
  [#64](https://github.com/gavinsimpson/gratia/issues/64)

## Bug fixes

* Updated *gratia* to work with *tibble* versions >= 3.0

# gratia 0.3.0

## New features

* *gratia* now uses the *mvnfast* package for random draws from a multivariate
  normal distribution (`mvnfast::rmvn()`). Contributed by Henrik Singmann (@singmann)
  [#28](https://github.com/gavinsimpson/gratia/issues/28)

* New function `basis()` for generating tidy representations of basis expansions
  from an *mgcv*-like definition of a smooth, e.g. `s()`, `te()`, `ti()`, or
  `t2()`. The basic smooth types also have a simple `draw()` method for plotting
  the basis. `basis()` is a simple wrapper around `mgcv::smoothCon()` with some
  post processing of the basis model matrix into a tidy format. [#42](https://github.com/gavinsimpson/gratia/issues/42)

* New function `smooth_samples()` to draw samples of entire smooth functions from
  their posterior distribution. Also has a `draw()` method for plotting the
  posterior samples.

## Bug fixes

* `draw.gam()` would produce empty plots between the panels for the parametric
    terms if there were 2 or more parametric terms in a model. Reported by
    @sklayn [#39](https://github.com/gavinsimpson/gratia/issues/39).

* `derivatives()` now works with factor by smooths, including ordered factor by
    smooths. The function also now works correctly for complex models with
    multiple covariates/smooths. [#47](https://github.com/gavinsimpson/gratia/issues/47)

    `derivatives()` also now handles `'fs'` smooths.  Reported by
    @tomand-uio [#57](https://github.com/gavinsimpson/gratia/issues/57).

* `evaluate_parametric_term()` and hence `draw.gam()` would fail on a `ziplss()` model
  because i) *gratia* didn't handle parametric terms in models with multiple linear
  predictors correctly, and ii) *gratia* didn't convert to the naming convention of
  *mgcv* for terms in higher linear predictors. Reported by @pboesu [#45](https://github.com/gavinsimpson/gratia/issues/45).
