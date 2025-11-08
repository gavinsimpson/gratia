# Indices of the parametric terms for a particular smooth

Returns a vector of indices of the parametric terms that represent the
supplied smooth. Useful for extracting model coefficients and columns of
their covariance matrix.

## Usage

``` r
smooth_coef_indices(smooth)
```

## Arguments

- smooth:

  an object that inherits from class `mgcv.smooth`

## Value

A numeric vector of indices.

## See also

[`smooth_coefs()`](https://gavinsimpson.github.io/gratia/reference/smooth_coefs.md)
for extracting the coefficients for a particular smooth.

## Author

Gavin L. Simpson
