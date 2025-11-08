# Extract fixed effects estimates from a fitted GAM

Extract fixed effects estimates from a fitted GAM

## Usage

``` r
# S3 method for class 'gam'
fixef(object, ...)

# S3 method for class 'gamm'
fixef(object, ...)

# S3 method for class 'lm'
fixef(object, ...)

# S3 method for class 'glm'
fixef(object, ...)

fixed_effects(object, ...)

# Default S3 method
fixed_effects(object, ...)
```

## Arguments

- object:

  a fitted GAM

- ...:

  arguments passed to other methods

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
  fixef(m)
}
#> Loading required package: lme4
#> Loading required package: Matrix
#> (Intercept)        Days 
#>   251.40510    10.46729 
```
