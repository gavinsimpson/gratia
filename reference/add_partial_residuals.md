# Add partial residuals

Add partial residuals

## Usage

``` r
add_partial_residuals(data, model, ...)

# S3 method for class 'gam'
add_partial_residuals(data, model, select = NULL, partial_match = FALSE, ...)
```

## Arguments

- data:

  a data frame containing values for the variables used to fit the
  model. Passed to
  [`stats::residuals()`](https://rdrr.io/r/stats/residuals.html) as
  `newdata`.

- model:

  a fitted model for which a
  [`stats::residuals()`](https://rdrr.io/r/stats/residuals.html) method
  is available. S3 method dispatch is performed on the `model` argument.

- ...:

  arguments passed to other methods.

- select:

  character, logical, or numeric; which smooths to plot. If `NULL`, the
  default, then all model smooths are drawn. Numeric `select` indexes
  the smooths in the order they are specified in the formula and stored
  in `object`. Character `select` matches the labels for smooths as
  shown for example in the output from `summary(object)`. Logical
  `select` operates as per numeric `select` in the order that smooths
  are stored.

- partial_match:

  logical; should smooths be selected by partial matches with `select`?
  If `TRUE`, `select` can only be a single string to match against.

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", seed = 1)
df <- df[, c("y", "x0", "x1", "x2", "x3")]
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

## add partial residuals
add_partial_residuals(df, m)
#> # A tibble: 400 x 9
#>          y     x0     x1     x2    x3 `s(x0)` `s(x1)` `s(x2)` `s(x3)`
#>      <dbl>  <dbl>  <dbl>  <dbl> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1  3.34   0.266  0.659  0.859  0.367  -2.38   -2.00   -5.36  -2.52  
#>  2 -0.0758 0.372  0.185  0.0344 0.741  -2.79   -5.15   -6.45  -3.29  
#>  3 10.7    0.573  0.954  0.971  0.934   2.99    5.75   -1.07   2.28  
#>  4  8.73   0.908  0.898  0.745  0.673  -0.734   2.84   -1.11   0.0287
#>  5 15.0    0.202  0.944  0.273  0.701  -0.752   2.54    3.94  -0.750 
#>  6  7.67   0.898  0.724  0.677  0.848  -1.46    0.432  -0.567 -0.812 
#>  7  7.58   0.945  0.370  0.348  0.706  -1.33   -1.57    2.08  -0.318 
#>  8  8.51   0.661  0.781  0.947  0.859   2.21    3.44   -1.42   1.68  
#>  9 10.6    0.629  0.0111 0.339  0.446   2.01   -0.445   4.13   1.51  
#> 10  3.72   0.0618 0.940  0.0317 0.677  -4.02   -0.123  -6.67  -3.37  
#> # i 390 more rows

## add partial residuals for selected smooths
add_partial_residuals(df, m, select = "s(x0)")
#> # A tibble: 400 x 6
#>          y     x0     x1     x2    x3 `s(x0)`
#>      <dbl>  <dbl>  <dbl>  <dbl> <dbl>   <dbl>
#>  1  3.34   0.266  0.659  0.859  0.367  -2.38 
#>  2 -0.0758 0.372  0.185  0.0344 0.741  -2.79 
#>  3 10.7    0.573  0.954  0.971  0.934   2.99 
#>  4  8.73   0.908  0.898  0.745  0.673  -0.734
#>  5 15.0    0.202  0.944  0.273  0.701  -0.752
#>  6  7.67   0.898  0.724  0.677  0.848  -1.46 
#>  7  7.58   0.945  0.370  0.348  0.706  -1.33 
#>  8  8.51   0.661  0.781  0.947  0.859   2.21 
#>  9 10.6    0.629  0.0111 0.339  0.446   2.01 
#> 10  3.72   0.0618 0.940  0.0317 0.677  -4.02 
#> # i 390 more rows
```
