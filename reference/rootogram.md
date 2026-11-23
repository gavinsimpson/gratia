# Rootograms to assess goodness of model fit

A rootogram is a model diagnostic tool that assesses the goodness of fit
of a statistical model. The observed values of the response are compared
with those expected from the fitted model. For discrete, count
responses, the frequency of each count (0, 1, 2, etc) in the observed
data and expected from the conditional distribution of the response
implied by the model are compared. For continuous variables, the
observed and expected frequencies are obtained by grouping the data into
bins. The rootogram is drawn using
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
graphics. The design closely follows Kleiber & Zeileis (2016).

## Usage

``` r
# S3 method for class 'gamm4'
rootogram(object, ..., conditioning = NULL)

rootogram(object, ...)

# S3 method for class 'gam'
rootogram(object, max_count = NULL, breaks = "Sturges", ...)

# S3 method for class 'scam'
rootogram(object, ...)
```

## Arguments

- object:

  an R object

- ...:

  arguments passed to other methods

- conditioning:

  for [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html),
  `"conditional"` (default) checks the response distribution at all
  fitted effects, `"marginal"` averages over new effects, and
  `"gam_component"` checks the extracted GAM.

- max_count:

  integer; the largest count to consider

- breaks:

  for continuous responses, how to group the response. Can be anything
  that is acceptable as the `breaks` argument of
  [`graphics::hist.default()`](https://rdrr.io/r/graphics/hist.html)

## References

Kleiber, C., Zeileis, A., (2016) Visualizing Count Data Regressions
Using Rootograms. *Am. Stat.* **70**, 296–303.
[doi:10.1080/00031305.2016.1173590](https://doi.org/10.1080/00031305.2016.1173590)

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Examples

``` r
# Simulate Poisson counts with a mean that varies smoothly with x.
set.seed(6)
dat <- data.frame(x = runif(120))
dat$y <- rpois(nrow(dat), exp(1 + sin(2 * pi * dat$x)))
m <- mgcv::gam(y ~ s(x, k = 6), family = poisson(),
  data = dat, method = "REML")
# Compare observed and fitted frequencies for each count.
draw(rootogram(m))
```
