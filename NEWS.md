# gratia 0.3.1 (in development)

## New features

* New functions `link()` and `inv_link()` to access the link function and its
  inverse from fitted models and family functions.
  
  Methods for classes: `"glm"`, `"gam"`, `"bam"`, `"gamm"` currently. [#58](https://github.com/gavinsimpson/gratia/issues/58)

* Adds explicit `family()` methods for objects of classes `"gam"`, `"bam"`, and
  `"gamm"`.

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
