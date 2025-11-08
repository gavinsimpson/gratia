# Add residuals from a GAM to a data frame

Add residuals from a GAM to a data frame

## Usage

``` r
# S3 method for class 'gam'
add_residuals(data, model, value = ".residual", type = "deviance", ...)
```

## Arguments

- data:

  a data frame containing values for the variables used to fit the
  model. Passed to
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html) as
  `newdata`.

- model:

  a fitted model for which a
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html) method is
  available. S3 method dispatch is performed on the `model` argument.

- value:

  character; the name of the variable in which model predictions will be
  stored.

- type:

  character; the type of residuals to return. See
  [`mgcv::residuals.gam()`](https://rdrr.io/pkg/mgcv/man/residuals.gam.html)
  for options.

- ...:

  additional arguments passed to
  [`mgcv::residuals.gam()`](https://rdrr.io/pkg/mgcv/man/residuals.gam.html).

## Value

A data frame (tibble) formed from `data` and residuals from `model`.

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", seed = 1)
df <- df[, c("y", "x0", "x1", "x2", "x3")]
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

##
add_residuals(df, m)
#> # A tibble: 400 x 6
#>          y     x0     x1     x2    x3 .residual
#>      <dbl>  <dbl>  <dbl>  <dbl> <dbl>     <dbl>
#>  1  3.34   0.266  0.659  0.859  0.367   -2.56  
#>  2 -0.0758 0.372  0.185  0.0344 0.741   -3.22  
#>  3 10.7    0.573  0.954  0.971  0.934    2.40  
#>  4  8.73   0.908  0.898  0.745  0.673    0.0785
#>  5 15.0    0.202  0.944  0.273  0.701   -0.693 
#>  6  7.67   0.898  0.724  0.677  0.848   -0.714 
#>  7  7.58   0.945  0.370  0.348  0.706   -0.259 
#>  8  8.51   0.661  0.781  0.947  0.859    1.78  
#>  9 10.6    0.629  0.0111 0.339  0.446    1.50  
#> 10  3.72   0.0618 0.940  0.0317 0.677   -3.32  
#> # i 390 more rows
```
