# Extract coefficient-sampler diagnostics

Diagnostics attached to fitted values or response samples describe the
coefficient chains before transformation or response simulation.
Response noise can conceal poor mixing of coefficient chains. Use
[`as_mcmc_draws()`](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)
to examine particular derived quantities as well.

## Usage

``` r
sampling_diagnostics(x)

# S3 method for class 'sampling_diagnostics'
print(x, ...)
```

## Arguments

- x:

  Output from a gratia sampling function; for the print method, a
  `sampling_diagnostics` object.

- ...:

  Further arguments passed to the print methods for the summaries.

## Value

A `sampling_diagnostics` list with `chains` (acceptance rates and
elapsed seconds), `coefficients` (posterior summaries, or `NULL` if not
requested), and `draws` (a coefficient matrix with chain identities, or
`NULL` if unavailable). Coefficient matrices provide their own draws;
other sampling outputs need `diagnostics = TRUE` to retain them.
Likelihood matrices from
[`log_lik()`](https://gavinsimpson.github.io/gratia/reference/log_lik.md)
also require `diagnostics = TRUE` for coefficient plots; use
[`as_mcmc_draws()`](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)
for their log-density draws.

## Details

Coefficient draws are retained when sampling with `diagnostics = TRUE`,
allowing
[`draw.sampling_diagnostics()`](https://gavinsimpson.github.io/gratia/reference/draw.sampling_diagnostics.md)
to plot the original coefficient chains even when `x` contains response
samples, fitted values, smooths or derivatives. These plots do not
describe the transformed quantities. Numerical summaries require
posterior, but plotting does not.

## See also

[`draw.sampling_diagnostics()`](https://gavinsimpson.github.io/gratia/reference/draw.sampling_diagnostics.md),
[`as_mcmc_draws()`](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)
