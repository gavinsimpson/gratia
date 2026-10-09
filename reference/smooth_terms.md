# List the variables involved in smooths

**\[experimental\]**

## Usage

``` r
# S3 method for class 'gamm4'
smooth_terms(object, ...)

smooth_terms(object, ...)
```

## Arguments

- object:

  an R object the result of a call to
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html), or
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html), or that
  inherits from classes `"gam"` or `"mgcv.smooth"`, or
  `"fs.interaction"`.

- ...:

  arguments passed to other methods. Currently unused.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
