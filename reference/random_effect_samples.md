# Draw new random effects from an estimated GAM

Sample zero-mean Gaussian random effects using the fitted smoothing
parameters and dispersion. These are new effects, rather than posterior
draws of the effects estimated for the fitting groups.

## Usage

``` r
random_effect_samples(model, data = NULL, select = NULL, n_draws, seed = NULL)
```

## Arguments

- model:

  An ordinary [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) or
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) fit with one
  linear predictor.

- data:

  Evaluation data. With `NULL`, use the fitting observations. Supplied
  data need only contain predictors of the selected smooths. Group
  labels may be new; repeated labels share an effect within a draw.

- select:

  Exact smooth labels, or `NULL` for all `re` and `fs` terms.

- n_draws:

  Positive integer giving the number of draws.

- seed:

  Optional integer seed. An explicit seed preserves the caller's
  random-number state; `NULL` advances the current state.

## Value

A tibble of class `random_effect_samples`, with `.row`, `.draw`,
`.smooth`, `.value` (the link-scale contribution), and covariates. The
`random_effect_info` attribute describes the fixed covariance estimates.

## Details

Standard `re` random intercepts, slopes and factor interactions, and
`fs` random curves are supported. Each curve is drawn jointly over its
evaluation points, including the components penalising its constant and
linear parts. Custom `re` penalties supplied through `xt$S` are
unsupported. Fits using `min.sp` are also unsupported: mgcv does not
retain the fitted lower bounds needed to reconstruct their random-effect
covariance.

For stored penalties \\S_j\\, smoothing parameters \\\lambda_j\\, and
dispersion \\\phi\\, coefficient covariance is \$\$\phi(\sum_j\lambda_j
S_j)^{-1}.\$\$ Smoothing parameters and dispersion remain fixed. As in
[`mgcv::gam.vcomp()`](https://rdrr.io/pkg/mgcv/man/gam.vcomp.html),
random-effect covariance uses `reml.scale` when it is finite and `sig2`
otherwise. Response generators retain their usual `sig2` dispersion.
Different smooths and different groups have independent new
coefficients, as specified by these model terms. Covariance between
fitted coefficients is not used here.

Use `marginalise` in
[`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
or
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
to integrate response means over these distributions. Use it in
[`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md),
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
or
[`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
to simulate new-group responses.

## Examples

``` r
library("mgcv")
m <- gam(travel ~ s(Rail, bs = "re"), data = nlme::Rail,
  method = "REML")
random_effect_samples(m, n_draws = 3, seed = 42)
#> # A tibble: 54 x 6
#>     .row .draw .smooth .value travel Rail 
#>    <int> <int> <chr>    <dbl>  <dbl> <ord>
#>  1     1     1 s(Rail)  34.0      55 1    
#>  2     2     1 s(Rail)  34.0      53 1    
#>  3     3     1 s(Rail)  34.0      54 1    
#>  4     4     1 s(Rail)  15.7      26 2    
#>  5     5     1 s(Rail)  15.7      37 2    
#>  6     6     1 s(Rail)  15.7      32 2    
#>  7     7     1 s(Rail)  37.5      78 3    
#>  8     8     1 s(Rail)  37.5      91 3    
#>  9     9     1 s(Rail)  37.5      85 3    
#> 10    10     1 s(Rail)  -1.56     92 4    
#> # i 44 more rows

# Compare zero random effects with response means averaged over groups.
set.seed(2)
d <- expand.grid(x = seq(0, 1, length.out = 15), id = factor(1:12))
d$y <- rpois(nrow(d), exp(0.3 + d$x + rnorm(12, sd = 0.6)[d$id]))
p <- gam(y ~ x + s(id, bs = "re"), data = d, family = poisson(),
  method = "REML")
nd <- data.frame(x = c(0, 0.5, 1), id = d$id[1])
fitted_values(p, data = nd, exclude = "s(id)")
#> # A tibble: 3 x 8
#>    .row     x id    .parameter .fitted   .se .lower_ci .upper_ci
#>   <int> <dbl> <fct> <chr>        <dbl> <dbl>     <dbl>     <dbl>
#> 1     1   0   1     mean          1.73 0.209      1.15      2.61
#> 2     2   0.5 1     mean          2.61 0.194      1.79      3.82
#> 3     3   1   1     mean          3.93 0.202      2.64      5.84
fitted_values(p, data = nd, marginalise = TRUE)
#> # A tibble: 3 x 8
#>       x id     .row .parameter .fitted   .se .lower_ci .upper_ci
#>   <dbl> <fct> <int> <chr>        <dbl> <dbl>     <dbl>     <dbl>
#> 1   0   1         1 mean          2.14 0.447      1.26      3.02
#> 2   0.5 1         2 mean          3.22 0.624      2.00      4.44
#> 3   1   1         3 mean          4.85 0.979      2.93      6.77
fitted_values(p, data = nd, marginalise = TRUE,
  integration = "monte_carlo", seed = 42)
#> # A tibble: 3 x 8
#>       x id     .row .parameter .fitted   .se .lower_ci .upper_ci
#>   <dbl> <fct> <int> <chr>        <dbl> <dbl>     <dbl>     <dbl>
#> 1   0   1         1 mean          2.12 0.444      1.25      2.99
#> 2   0.5 1         2 mean          3.20 0.619      1.98      4.41
#> 3   1   1         3 mean          4.81 0.971      2.91      6.71
nd$id <- c("new", "new", "other")
predicted_samples(p, data = nd, marginalise = TRUE, n_draws = 3, seed = 42)
#> # A tibble: 9 x 3
#>    .row .draw .response
#>   <int> <int>     <dbl>
#> 1     1     1         5
#> 2     2     1         6
#> 3     3     1         4
#> 4     1     2         1
#> 5     2     2         3
#> 6     3     2         4
#> 7     1     3         1
#> 8     2     3         3
#> 9     3     3        19
```
