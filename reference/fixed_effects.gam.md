# Extract fixed effects estimates from a fitted model

Extract fixed effects estimates from a fitted model

## Usage

``` r
fixed_effects(object, ...)

# S3 method for class 'gam'
fixed_effects(object, ...)

# S3 method for class 'gamm'
fixed_effects(object, ...)

# S3 method for class 'gamm4'
fixed_effects(object, ...)

# S3 method for class 'lm'
fixed_effects(object, ...)

# S3 method for class 'glm'
fixed_effects(object, ...)

# Default S3 method
fixed_effects(object, ...)

# S3 method for class 'scam'
fixed_effects(object, ...)
```

## Arguments

- object:

  a fitted model. Supported classes are models fitted by
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html),
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html),
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html),
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html), and
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

- ...:

  arguments passed to other methods

## Details

Starting with gratia 0.12.1, use `fixed_effects()` for the models listed
above if you previously called
[`gratia::fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html) or
a [`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html) method
supplied by gratia. For models fitted directly with nlme or lme4, use
[`nlme::fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html) or
[`lme4::fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html),
respectively.

## Examples

``` r
load_mgcv()

# run example if lme4 is available
if (require("lme4")) {
  data(sleepstudy, package = "lme4")
  m <- gam(
    Reaction ~ Days + s(Subject, bs = "re") +
      s(Days, Subject, bs = "re"),
    data = sleepstudy, method = "REML"
  )
  fixed_effects(m)
}
#> Loading required package: lme4
#> Loading required package: Matrix
#> (Intercept)        Days 
#>   251.40510    10.46729 
```
