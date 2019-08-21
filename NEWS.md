# gratia (development version)

## New features

* *gratia* now uses the *mvnfast* package for random draws from a multivariate
    normal distribution (`mvnfast::rmvn()`). Contributed by Henrik Singmann
    (@singmann) #28

## Bug fixes

* `draw.gam()` would produce empty plots between the panels for the parametric
    terms if there were 2 or more parametric terms in a model. Reported by
    @sklayn [#39](https://github.com/gavinsimpson/gratia/issues/39).

