# Return the linear prediction matrix of a fitted GAM

`lp_matrix()` is a wrapper to `predict(..., type = "lpmatrix")` for
returning the linear predictor matrix for the model training data (when
`data = NULL`), or user-specified data values supplied via `data`.

## Usage

``` r
lp_matrix(model, ...)

# S3 method for class 'gam'
lp_matrix(model, data = NULL, ...)
```

## Arguments

- model:

  a fitted model

- ...:

  arguments passed to other methods and `predict` methods including
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)
  and
  [`mgcv::predict.bam()`](https://rdrr.io/pkg/mgcv/man/predict.bam.html)

- data:

  a data frame of values at which to return the linear prediction
  matrix.

## Value

The linear prediction matrix is returned as a matrix. The object
returned is of class `"lp_matrix"`, which inherits from classes
`"matrix"` and `"array"`. The special class allows the printing of the
matrix to be controlled, which we do by printing the matrix as a tibble.

## Details

The linear prediction matrix \\\mathbf{X}\_p\\ is a matrix that maps
values of parameters \\\hat{\mathbf{\beta}}\_p\\ to values on the linear
predictor of the model \\\hat{\eta}\_p = \mathbf{X}\_p
\hat{\mathbf{\beta}}\_p\\. \\\mathbf{X}\_p\\ is the model matrix where
spline covariates have been replaced by the values of the basis
functions evaluated at the respective covariates. Parametric covariates
are also included.

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", seed = 1)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df)

# linear prediction matrix for observed data
xp <- lp_matrix(m)
## IGNORE_RDIFF_BEGIN
xp
#> Linear prediction matrix (400 x 37)
#>   `(Intercept)` `s(x0).1` `s(x0).2` `s(x0).3` `s(x0).4` `s(x0).5` `s(x0).6`
#>           <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#> 1             1     0.961     0.227    0.706     -0.135     0.457  -0.146  
#> 2             1     0.651    -0.241    0.0684    -0.308     0.394  -0.00994
#> 3             1    -0.385    -0.549    0.0660    -0.204    -0.416  -0.247  
#> 4             1    -1.27      0.156   -1.53       0.222    -1.60    0.198  
#> 5             1     1.05      0.420    1.11       0.214     0.893   0.0351 
#> # i 395 more rows
## IGNORE_RDIFF_END

# the object `xp` *is* a matrix
class(xp)
#> [1] "lp_matrix" "matrix"    "array"    
# but we print like a tibble to avoid spamming the R console

# linear predictor matrix for new data set
ds <- data_slice(m, x2 = evenly(x2))
xp <- lp_matrix(m, data = ds)
## IGNORE_RDIFF_BEGIN
xp
#> Linear prediction matrix (100 x 37)
#>   `(Intercept)` `s(x0).1` `s(x0).2` `s(x0).3` `s(x0).4` `s(x0).5` `s(x0).6`
#>           <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#> 1             1     0.170    -0.542   -0.0371   -0.0534     0.144    -0.353
#> 2             1     0.170    -0.542   -0.0371   -0.0534     0.144    -0.353
#> 3             1     0.170    -0.542   -0.0371   -0.0534     0.144    -0.353
#> 4             1     0.170    -0.542   -0.0371   -0.0534     0.144    -0.353
#> 5             1     0.170    -0.542   -0.0371   -0.0534     0.144    -0.353
#> # i 95 more rows
## IGNORE_RDIFF_END
```
