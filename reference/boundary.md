# Extract the boundary of a soap film smooth

**\[experimental\]**

## Usage

``` r
boundary(x, ...)

# S3 method for class 'soap.film'
boundary(x, ...)

# S3 method for class 'gam'
boundary(x, select, ...)
```

## Arguments

- x:

  an R object. Currently only objects that inherit from classes
  `"soap.film"` and `"gam"`.

- ...:

  arguments passed to other methods.

- select:

  character; the label of the soap film smooth from which to extract the
  boundary.

## Value

A list of lists or data frames specifying the loops that define the
boundary of the soap film smooth.

## See also

[mgcv::soap](https://rdrr.io/pkg/mgcv/man/smooth.construct.so.smooth.spec.html)
