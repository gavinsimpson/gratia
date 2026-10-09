# Dimension of a smooth

Extracts the dimension of an estimated smooth.

## Usage

``` r
# S3 method for class 'gamm4'
smooth_dim(object)

smooth_dim(object)

# S3 method for class 'gam'
smooth_dim(object)

# S3 method for class 'gamm'
smooth_dim(object)

# S3 method for class 'mgcv.smooth'
smooth_dim(object)
```

## Arguments

- object:

  an R object. See Details for list of supported objects.

## Value

A numeric vector of dimensions for each smooth.

## Details

This is a generic function with methods for objects of class `"gam"`,
`"gamm"`, and `"mgcv.smooth"`.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Author

Gavin L. Simpson
