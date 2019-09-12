# gratia (development version)

## New features

* *gratia* now uses the *mvnfast* package for random draws from a multivariate
    normal distribution (`mvnfast::rmvn()`). Contributed by Henrik Singmann
    (@singmann) #28

* New function `basis()` for generating tidy representations of basis expansions
   from an *mgcv*-like definition of a smooth, e.g. `s()`, `te()`, `ti()`, or
   `t2()`. The basic smooth types also have a simple `draw()` method for plotting
   the basis. `basis()` is a simple wrapper around `mgcv::smoothCon()` with some
   post processing of the basis model matrix into a tidy format. #42

## Bug fixes

* `draw.gam()` would produce empty plots between the panels for the parametric
    terms if there were 2 or more parametric terms in a model. Reported by
    @sklayn [#39](https://github.com/gavinsimpson/gratia/issues/39).

