# List the variables involved in smooths

**\[experimental\]**

## Usage

``` r
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
