# Partial residuals

Partial residuals

## Usage

``` r
partial_residuals(object, ...)

# S3 method for class 'gam'
partial_residuals(object, select = NULL, partial_match = FALSE, ...)
```

## Arguments

- object:

  an R object, typically a model. Currently only objects of class
  `"gam"` (or that inherit from that class) are supported.

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
## load mgcv
load_mgcv()

## example data - Gu & Wahba four term model
df <- data_sim("eg1", n = 400, seed = 42)
## fit the model
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

## extract partial residuals
partial_residuals(m)
#> # A tibble: 400 x 4
#>    `s(x0)`  `s(x1)` `s(x2)`  `s(x3)`
#>      <dbl>    <dbl>   <dbl>    <dbl>
#>  1 -0.3527 -1.321   -2.180   0.6077 
#>  2 -0.1233  0.5013  -1.775   0.9613 
#>  3  1.429   1.515    5.609   0.9910 
#>  4 -1.110  -1.700   -0.8882 -0.6593 
#>  5 -2.120  -0.01378 -2.733  -3.012  
#>  6  1.254  -1.224    3.915   0.07275
#>  7 -0.5220  3.023   -0.8197 -1.019  
#>  8  1.398   0.2184   7.055   1.897  
#>  9  2.797   0.4969   7.329   2.498  
#> 10  1.151  -0.2267   0.7202  0.7437 
#> # i 390 more rows

## and for a select term
partial_residuals(m, select = "s(x2)")
#> # A tibble: 400 x 1
#>    `s(x2)`
#>      <dbl>
#>  1 -2.180 
#>  2 -1.775 
#>  3  5.609 
#>  4 -0.8882
#>  5 -2.733 
#>  6  3.915 
#>  7 -0.8197
#>  8  7.055 
#>  9  7.329 
#> 10  0.7202
#> # i 390 more rows

## or with partial matching
partial_residuals(m, select = "x", partial_match = TRUE) # returns all
#> # A tibble: 400 x 4
#>    `s(x0)`  `s(x1)` `s(x2)`  `s(x3)`
#>      <dbl>    <dbl>   <dbl>    <dbl>
#>  1 -0.3527 -1.321   -2.180   0.6077 
#>  2 -0.1233  0.5013  -1.775   0.9613 
#>  3  1.429   1.515    5.609   0.9910 
#>  4 -1.110  -1.700   -0.8882 -0.6593 
#>  5 -2.120  -0.01378 -2.733  -3.012  
#>  6  1.254  -1.224    3.915   0.07275
#>  7 -0.5220  3.023   -0.8197 -1.019  
#>  8  1.398   0.2184   7.055   1.897  
#>  9  2.797   0.4969   7.329   2.498  
#> 10  1.151  -0.2267   0.7202  0.7437 
#> # i 390 more rows
```
