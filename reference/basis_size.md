# Extract basis dimension of a smooth

Extract basis dimension of a smooth

## Usage

``` r
basis_size(object, ...)

# S3 method for class 'mgcv.smooth'
basis_size(object, ...)

# S3 method for class 'gam'
basis_size(object, ...)

# S3 method for class 'gamm'
basis_size(object, ...)
```

## Arguments

- object:

  A fitted GAM(M). Currently
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) (and anything
  that inherits from the `"gam"` class, e.g.
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html)) and
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) are
  supported.

- ...:

  Arguments passed to other methods.

## Examples

``` r
load_mgcv()

df <- data_sim("eg1", n = 200, seed = 1)
m <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df)

basis_size(m)
#> s(x0) s(x1) s(x2) s(x3) 
#>     9     9     9     9 
```
