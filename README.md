# schoenberg

The *schoenburg* package for R provides ggplot-based graphics and useful functions for GAMs fitted using the mgcv package.

## Installing *schoenberg*

*schoenberg* is under active development and has not yet had its first release to CRAN. The easiest way to install the package is via the `install_github()` function from package *devtools*. Make sure you have *devtools* installed, then run

```r
devtools::install_github("gavinsimpson/schoenberg")
```

to install the package.

## History

*schoenberg* grew out of an earlier package, *tsgam*, which was originally intended to be used with GAMs fitted to time series. As I was developing *tsgam* however it became clear that the package could be used more generally and that the name "tsgam" was no longer appropriate. To avoid breaking blog posts I had written using *tsgam* I decided to copy the git repo and all the history to a new repo for the package under the name *schoenberg*.

## Why *schoenberg*?

In naming his [*greta*](https://github.com/greta-dev/greta) package, Nick Golding observed the recent phenomena of naming statistical modelling software, such as Stan or Edward, after individuals that played a prominent role in the development of the field. This lead Nick to name his Tensor Flow-based package *greta* after [*Grete Hermann*](https://greta-dev.github.io/greta/why_greta.html).

In the same spirit, *schoenberg* is named in recognition of the contributions of Grace Wahba, who did pioneering work on the penalised spline models that are at the foundation of the way GAMs are estimated in *mgcv*. I wanted to name the package *grace*, to more explicitly recognise Grace's contributions, but unfortunately there was already a package named *Grace* on CRAN. So I looked elsewhere for inspiration.

[Grace Wahba](https://en.wikipedia.org/wiki/Grace_Wahba) is the IJ Schoenberg-Hilldale Professor of Statistics at the University of Wisconsin-Madison, where she has worked since 1967. The chair is named after [Isaac J Schoenberg](https://en.wikipedia.org/wiki/Isaac_Jacob_Schoenberg), a former University Madison-Wisconsin Professor of Mathematics, who in a 1946 paper provided the first mathematical reference to "splines".

The name *schoenberg* links and recognises two pioneers in the field of splines.
