# Identify a smooth term by its label

Identify a smooth term by its label

## Usage

``` r
# S3 method for class 'gamm4'
which_smooths(object, ...)

which_smooths(object, ...)

# Default S3 method
which_smooths(object, ...)

# S3 method for class 'gam'
which_smooths(object, terms, ...)

# S3 method for class 'bam'
which_smooths(object, terms, ...)

# S3 method for class 'gamm'
which_smooths(object, terms, ...)
```

## Arguments

- object:

  a fitted GAM.

- ...:

  arguments passed to other methods.

- terms:

  character; one or more (partial) term labels with which to identify
  required smooths.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
