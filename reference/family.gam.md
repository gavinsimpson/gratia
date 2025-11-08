# Extract family objects from models

Provides a [`stats::family()`](https://rdrr.io/r/stats/family.html)
method for a range of GAM objects.

## Usage

``` r
# S3 method for class 'gam'
family(object, ...)

# S3 method for class 'gamm'
family(object, ...)

# S3 method for class 'bam'
family(object, ...)

# S3 method for class 'list'
family(object, ...)
```

## Arguments

- object:

  a fitted model. Models fitted by
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html), and
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html) are
  currently supported.

- ...:

  arguments passed to other methods.
