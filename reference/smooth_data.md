# Generate regular data over the covariates of a smooth

Generate regular data over the covariates of a smooth

## Usage

``` r
smooth_data(
  model,
  id,
  n = 100,
  n_2d = NULL,
  n_3d = NULL,
  n_4d = NULL,
  offset = NULL,
  include_all = FALSE,
  var_order = NULL
)
```

## Arguments

- model:

  a fitted model

- id:

  the number ID of the smooth within `model` to process.

- n:

  numeric; the number of new observations to generate.

- n_2d:

  numeric; the number of new observations to generate for the second
  dimension of a 2D smooth. *Currently ignored*.

- n_3d:

  numeric; the number of new observations to generate for the third
  dimension of a 3D smooth.

- n_4d:

  numeric; the number of new observations to generate for the dimensions
  higher than 2 (!) of a *k*D smooth (*k* \>= 4). For example, if the
  smooth is a 4D smooth, each of dimensions 3 and 4 will get `n_4d` new
  observations.

- offset:

  numeric; value of the model offset to use.

- include_all:

  logical; include all covariates involved in the smooth? if `FALSE`,
  only the covariates involved in the smooth will be included in the
  returned data frame. If `TRUE`, a representative value will be
  included for all other covariates in the model that aren't actually
  used in the smooth. This can be useful if you want to pass the
  returned data frame on to
  [`mgcv::PredictMat()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html).

- var_order:

  character; the order in which the terms in the smooth should be
  processed. Only useful for tensor products with at least one 2d
  marginal smooth.

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", seed = 42)
m <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df)

# generate data over range of x1 for smooth s(x1)
smooth_data(m, id = 2)
#> # A tibble: 100 x 1
#>           x1
#>        <dbl>
#>  1 0.0004050
#>  2 0.01046  
#>  3 0.02052  
#>  4 0.03057  
#>  5 0.04063  
#>  6 0.05069  
#>  7 0.06074  
#>  8 0.07080  
#>  9 0.08086  
#> 10 0.09091  
#> # i 90 more rows

# generate data over range of x1 for smooth s(x1), with typical value for
# other covariates in the model
smooth_data(m, id = 2, include_all = TRUE)
#> # A tibble: 100 x 4
#>           x1     x0     x2     x3
#>        <dbl>  <dbl>  <dbl>  <dbl>
#>  1 0.0004050 0.4883 0.4708 0.4879
#>  2 0.01046   0.4883 0.4708 0.4879
#>  3 0.02052   0.4883 0.4708 0.4879
#>  4 0.03057   0.4883 0.4708 0.4879
#>  5 0.04063   0.4883 0.4708 0.4879
#>  6 0.05069   0.4883 0.4708 0.4879
#>  7 0.06074   0.4883 0.4708 0.4879
#>  8 0.07080   0.4883 0.4708 0.4879
#>  9 0.08086   0.4883 0.4708 0.4879
#> 10 0.09091   0.4883 0.4708 0.4879
#> # i 90 more rows
```
