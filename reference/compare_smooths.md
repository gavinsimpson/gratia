# Compare smooths across models

Compare smooths across models

## Usage

``` r
compare_smooths(
  model,
  ...,
  select = NULL,
  smooths = deprecated(),
  n = 100,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  partial_match = FALSE
)
```

## Arguments

- model:

  Primary model for comparison.

- ...:

  Additional models to compare smooths against those of `model`.

- select:

  character; select which smooths to compare. The default (`NULL`) means
  all smooths in `model` will be compared. Numeric `select` indexes the
  smooths in the order they are specified in the formula and stored in
  `model`. Character `select` matches the labels for smooths as shown
  for example in the output from `summary(object)`. Logical `select`
  operates as per numeric `select` in the order that smooths are stored.

- smooths:

  **\[deprecated\]** Use `select` instead.

- n:

  numeric; the number of points over the range of the covariate at which
  to evaluate the smooth.

- data:

  a data frame of covariate values at which to evaluate the smooth.

- unconditional:

  logical; should confidence intervals include the uncertainty due to
  smoothness selection? If `TRUE`, the corrected Bayesian covariance
  matrix will be used.

- overall_uncertainty:

  logical; should the uncertainty in the model constant term be included
  in the standard error of the evaluate values of the smooth?

- partial_match:

  logical; should smooths be selected by partial matches with `select`?
  If `TRUE`, `select` can only be a single string to match against.

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", seed = 2)

## models to compare smooths across - artificially create differences
m1 <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
  data = dat, method = "REML"
)
m2 <- gam(y ~ s(x0, bs = "ts") + s(x1, bs = "ts") + s(x2, bs = "ts") +
  s(x3, bs = "ts"), data = dat, method = "REML")

## build comparisons
comp <- compare_smooths(m1, m2)
comp
#> # A tibble: 8 x 5
#>   .model .smooth .type         .by   data              
#>   <chr>  <chr>   <chr>         <chr> <list>            
#> 1 m1     s(x0)   TPRS          NA    <tibble [100 x 3]>
#> 2 m2     s(x0)   TPRS (shrink) NA    <tibble [100 x 3]>
#> 3 m1     s(x1)   TPRS          NA    <tibble [100 x 3]>
#> 4 m2     s(x1)   TPRS (shrink) NA    <tibble [100 x 3]>
#> 5 m1     s(x2)   TPRS          NA    <tibble [100 x 3]>
#> 6 m2     s(x2)   TPRS (shrink) NA    <tibble [100 x 3]>
#> 7 m1     s(x3)   TPRS          NA    <tibble [100 x 3]>
#> 8 m2     s(x3)   TPRS (shrink) NA    <tibble [100 x 3]>
## notice that the result is a nested tibble

draw(comp)
```
