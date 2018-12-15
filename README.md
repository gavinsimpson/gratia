# gratia

The *gratia* package for R provides ggplot-based graphics and useful functions for GAMs fitted using the mgcv package.

## Build status

[![Build Status](https://travis-ci.org/gavinsimpson/gratia.svg?branch=master)](https://travis-ci.org/gavinsimpson/gratia) [![Build status](https://ci.appveyor.com/api/projects/status/w7pj8773t5b8fxkb/branch/master?svg=true)](https://ci.appveyor.com/project/gavinsimpson/gratia/branch/master) [![codecov](https://codecov.io/gh/gavinsimpson/gratia/branch/master/graph/badge.svg)](https://codecov.io/gh/gavinsimpson/gratia)

## Features

The main features of *gratia* are currently

* A *ggplot2*-based replacement for `mgcv:::plot.gam()`: `draw(gam_model)`.

    Note specialist smoothers (`bs %in% c("sos","mrf","so")`) are not supported, but univariate, *factor* and *continuous* `by`-variable smooths, simple random effect smooths (`bs = 're'`), factor-smooth interaction smooths (`bs = "fs"`), and bivariate tensor product smooths are supported,

* Estimatation of derivatives of fitted smoothers: `fderiv(gam_model)`,

* Estimation of point-wise across-the-function confidence intervals and simultaneous intervals for smooths: `confint(gam_model)`.

## Installing *gratia*

*gratia* is under active development and has not yet had its first release to CRAN. The easiest way to install the package is via the `install_github()` function from package *devtools*. Make sure you have *devtools* installed, then run

```r
devtools::install_github("gavinsimpson/gratia")
```

to install the package.

## History

*gratia* grew out of an earlier package, *schoenberg*, itself a development of the earlier package *tsgam*, which was originally intended to be used with GAMs fitted to time series. As I was developing *tsgam* however it became clear that the package could be used more generally and that the name "tsgam" was no longer appropriate. To avoid breaking blog posts I had written using *tsgam* I decided to copy the git repo and all the history to a new repo for the package under the name *schoenberg*. At a later date someone released another package called *schoenberg* to CRAN, so that scuppered that idea. Now I'm calling the package *gratia*. Hopefully I won't have to change it again…

## Why *gratia*?

In naming his [*greta*](https://github.com/greta-dev/greta) package, Nick Golding observed the recent phenomena of naming statistical modelling software, such as Stan or Edward, after individuals that played a prominent role in the development of the field. This lead Nick to name his Tensor Flow-based package *greta* after [*Grete Hermann*](https://greta-stats.org/articles/webpages/why_greta.html).

In the same spirit, *gratia* is named in recognition of the contributions of [Grace Wahba](https://en.wikipedia.org/wiki/Grace_Wahba), who did pioneering work on the penalised spline models that are at the foundation of the way GAMs are estimated in *mgcv*. I wanted to name the package *grace*, to explicitly recognise Grace's contributions, but unfortunately there was already a package named *Grace* on CRAN. So I looked elsewhere for inspiration.

The English word "grace" derives from the Latin *gratia*, meaning "favor, charm, thanks" ([according to Merriam Webster](https://www.merriam-webster.com/dictionary/grace)).

The chair that Grace Wabha currently holds is named after [Isaac J Schoenberg](https://en.wikipedia.org/wiki/Isaac_Jacob_Schoenberg), a former University Madison-Wisconsin Professor of Mathematics, who in a 1946 paper provided the first mathematical reference to "splines". (Hence the previous name for the package.)
