# Generate data over the range of variables used in smooths

**\[deprecated\]**

For each smooth in a GAM, generate new data over the range of the
variables involved in a smooth. This function is deprecated as it is
only useful for a very narrow use-case. Use
[`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
instead.

## Usage

``` r
datagen(x, ...)

# S3 method for class 'mgcv.smooth'
datagen(x, n = 100, data, ...)

# S3 method for class 'fs.interaction'
datagen(x, n = 100, data, ...)

# S3 method for class 'gam'
datagen(x, smooth = NULL, n = 200, ...)

# S3 method for class 'gamm'
datagen(x, ...)

# S3 method for class 'list'
datagen(x, ...)
```

## Arguments

- x:

  an object for which new data is required. Currently objects of classes
  `"gam"`, and `"gamm"` are supported, as are smooths from **mgcv**
  inheriting from class `"mgcv.smooth"`.

- ...:

  arguments passed to methods

- n:

  numeric; the number of data values to generate per term in each
  smooth.

- data:

  data frame; for `"mgcv.smooth"` objects, the data used to fit the GAM
  need to be supplied.

## Value

A data frame of new values spread over the range of the observed values.

## Author

Gavin L. Simpson
