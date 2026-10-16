# Convert posterior samples for MCMC diagnostics

Coefficient matrices retain their chain mapping in an attribute. Sampled
quantities use their `.chain`, `.iteration`, and `.draw` columns. A
plain matrix without a mapping is treated as one chain; chain boundaries
are never inferred from its number of rows. Filtering must leave the
same consecutive iterations in every chain. Missing observations remain
missing variables.

## Usage

``` r
as_mcmc_draws(
  x,
  format = c("draws_array", "draws_df", "mcmc.list", "array"),
  value = NULL
)
```

## Arguments

- x:

  A coefficient matrix from
  [`post_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
  or a table from
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md),
  or
  [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md).

- format:

  One of `"draws_array"`, `"draws_df"`, `"mcmc.list"`, or `"array"`. The
  first two require posterior; `"mcmc.list"` requires coda.

- value:

  Column containing sampled values. By default the appropriate column is
  selected from `.fitted`, `.response`, `.value`, or `.derivative`.

## Value

An iteration by chain by variable array, a posterior draws object, or a
coda list of chains. Variable names identify sampled quantities and
their observation rows, parameters, or smooths.
