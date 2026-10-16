# Handle user-supplied posterior draws

Handle user-supplied posterior draws

## Usage

``` r
# S3 method for class 'gamm4'
user_draws(model, ..., draws, n_draws = NULL, chains = NULL)

user_draws(model, ..., draws)

# S3 method for class 'gam'
user_draws(model, ..., n_draws = NULL, chains = NULL, draws, index = NULL)

# S3 method for class 'scam'
user_draws(
  model,
  ...,
  draws = NULL,
  n_draws = NULL,
  chains = NULL,
  index = NULL
)
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

- ...:

  arguments passed to methods.

- draws:

  matrix; user supplied posterior draws to be used when
  `method = "user"`.

- n_draws:

  positive integer; total retained draws across all chains. Required
  when generating draws. With `method = "user"`, the supplied matrix
  determines the count; an explicit count must agree with its rows.

- chains:

  positive integer or `NULL`; number of chains. `NULL` uses one chain,
  or the chain mapping in supplied draws. `n_draws` must be divisible by
  `chains`. Burn-in is applied separately to each MH chain.

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

For SCAM models, supplied draws must contain finite transformed
coefficients in the order returned by `coef(model)`. If column names are
supplied they must match exactly. Entries identified by `model$p.ident`
must be nonnegative. Draws returned with `parametrized = FALSE` cannot
be supplied directly for prediction; transform them first.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
