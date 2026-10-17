# Conditional moments of a GAM response distribution

Construct covariate combinations using
[`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
conventions, then estimate response moments with
[`fitted_moments()`](https://gavinsimpson.github.io/gratia/reference/fitted_moments.md).
Other covariates are held at typical values; observations are not
averaged over those covariates.

## Usage

``` r
conditional_moments(
  model,
  condition = NULL,
  data = NULL,
  moments = c("mean", "variance", "cv"),
  n_vals = 100,
  complete = TRUE,
  ...
)

# S3 method for class 'gam'
conditional_moments(
  model,
  condition = NULL,
  data = NULL,
  moments = c("mean", "variance", "cv"),
  n_vals = 100,
  complete = TRUE,
  ...
)
```

## Arguments

- model:

  a fitted GAM object.

- condition:

  either a character vector or a list supplying the names of covariates,
  and possibly their values, to condition up. The order of the values
  determines how these are plotted via the
  [`draw.conditional_values()`](https://gavinsimpson.github.io/gratia/reference/draw.conditional_values.md)
  method; the first element is mapped to the *x* channel, the second
  element to the *colour* channel, the third to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  **if no fourth element is present**, if present, the fourth element is
  mapped to the rows and the third element is mapped to the columns of
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).

- data:

  optional data frame used to generate condition values and determine
  covariate classes. Explicit values in `condition` take precedence.
  Other covariates use the fitted model's typical values. The rows of
  `data` are not used directly as a prediction grid.

- moments:

  Character vector selecting `"mean"`, `"variance"`, `"sd"`, `"cv"`,
  `"skewness"`, `"kurtosis"`, or `"excess_kurtosis"`. `"all"` requests
  all seven. CV is SD divided by a strictly positive mean; kurtosis is
  the fourth central moment divided by squared variance.

- n_vals:

  numeric; number of values to generate for numeric variables named in
  `condition`.

- complete:

  logical; if `TRUE` (the default), retain all combinations of
  conditions. If `FALSE`, retain only combinations of factor levels
  among the variables named in `condition` that occur in the fitted
  model's data. Numeric conditions are not filtered, and variables
  absent from `condition` remain at their typical values. Supplied
  `data` does not change which combinations count as observed. An error
  is raised if no combinations remain.

- ...:

  Arguments to
  [`fitted_moments()`](https://gavinsimpson.github.io/gratia/reference/fitted_moments.md),
  including `uncertainty`, `nuisance`, `n_draws`, `seed`, `scores`, and
  `exclude`.

## Value

A tibble of class `"conditional_moments"`, with the columns returned by
[`fitted_moments()`](https://gavinsimpson.github.io/gratia/reference/fitted_moments.md)
and the plotting channels of
[`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md).

## Examples

``` r
library("mgcv")
m <- gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
conditional_moments(m, condition = "wt", moments = c("mean", "variance")) |>
  draw()
```
