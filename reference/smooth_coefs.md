# Coefficients for a particular smooth

Returns a vector of model coefficients of the parametric terms that
represent the supplied smooth.

## Usage

``` r
smooth_coefs(object, ...)

# S3 method for class 'gam'
smooth_coefs(object, select, term = deprecated(), ...)

# S3 method for class 'bam'
smooth_coefs(object, select, term = deprecated(), ...)

# S3 method for class 'gamm'
smooth_coefs(object, select, term = deprecated(), ...)

# S3 method for class 'gamm4'
smooth_coefs(object, select, term = deprecated(), ...)

# S3 method for class 'list'
smooth_coefs(object, select, term = deprecated(), ...)

# S3 method for class 'mgcv.smooth'
smooth_coefs(object, model, ...)

# S3 method for class 'scam'
smooth_coefs(object, select, term = deprecated(), ...)
```

## Arguments

- object:

  a fitted GAM(M) object, or, for the `"mgcv.smooth"` method, an object
  that inherits from class `mgcv.smooth`.

- ...:

  arguments passed to other methods.

- select:

  character; the label of the smooth whose coefficients will be
  returned.

- term:

  **\[deprecated\]** Use `select` instead.

- model:

  a fitted GAM(M) object.

## Value

A numeric vector of model coefficients.

## See also

[`smooth_coef_indices()`](https://gavinsimpson.github.io/gratia/reference/smooth_coef_indices.md)
for extracting the indices of the coefficients for a particular smooth.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", seed = 2)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

## IGNORE_RDIFF_BEGIN
smooth_coefs(m, select = "s(x2)")
#>   s(x2).1   s(x2).2   s(x2).3   s(x2).4   s(x2).5   s(x2).6   s(x2).7   s(x2).8 
#> -6.533373  9.694277  2.194078 -1.967280 -2.374874  1.207638 -1.572586  9.269744 
#>   s(x2).9 
#>  5.622738 
## IGNORE_RDIFF_END
```
