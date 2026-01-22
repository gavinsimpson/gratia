# Extract the model constant term

**\[experimental\]** Extracts the model constant term(s), the model
intercept, from a fitted model object.

## Usage

``` r
model_constant(model, ...)

# S3 method for class 'gam'
model_constant(model, lp = NULL, ...)

# S3 method for class 'gamlss'
model_constant(model, ...)

# S3 method for class 'glm'
model_constant(model, ...)
```

## Arguments

- model:

  a fitted model for which a
  [`coef()`](https://rdrr.io/r/stats/coef.html) method exists.

- ...:

  arguments passed to other methods.

- lp:

  numeric; which linear predictors to extract constant terms for.

## Examples

``` r
load_mgcv()

# simulate a small example
df <- data_sim("eg1", seed = 42)

# fit the GAM
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

# extract the estimate of the constant term
model_constant(m)
#> [1] 7.495
#> attr(,"par_names")
#> [1] "location"
# same as coef(m)[1L]
coef(m)[1L]
#> (Intercept) 
#>       7.495 
```
