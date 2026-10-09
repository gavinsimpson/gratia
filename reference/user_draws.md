# Handle user-supplied posterior draws

Handle user-supplied posterior draws

## Usage

``` r
# S3 method for class 'gamm4'
user_draws(model, draws, ...)

user_draws(model, draws, ...)

# S3 method for class 'gam'
user_draws(model, draws, index = NULL, ...)
```

## Arguments

- model:

  a fitted model from
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html), or
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html), or a
  model inheriting from these classes. See
  [gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
  for sampling from gamm4 fits.

- draws:

  matrix; user supplied posterior draws to be used when
  `method = "user"`.

- ...:

  arguments passed to methods.

- index:

  a vector to index (subset) the columns of `draws`.

## Details

The supplied `draws` must be a matrix (currently), with 1 column per
model coefficient, and 1 row per posterior draw. The `"gam"` method has
argument `index`, which can be used to subset (select) coefficients
(columns) of `draws`. `index` can be any valid way of selecting
(indexing) columns of a matrix. `index` is useful if you have a set of
posterior draws for the entire model (say from
[`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html)) and you
wish to use those draws for an individual smooth, via
[`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md).

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
