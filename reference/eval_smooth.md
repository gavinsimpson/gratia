# S3 methods to evaluate individual smooths

S3 methods to evaluate individual smooths

## Usage

``` r
eval_smooth(smooth, ...)

# S3 method for class 'mgcv.smooth'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  dist = NULL,
  ...
)

# S3 method for class 'soap.film'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  clip = TRUE,
  ...
)

# S3 method for class 'scam_smooth'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  dist = NULL,
  ...
)

# S3 method for class 'fs.interaction'
eval_smooth(
  smooth,
  model,
  n = 100,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  ...
)

# S3 method for class 'sz.interaction'
eval_smooth(
  smooth,
  model,
  n = 100,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  ...
)

# S3 method for class 'random.effect'
eval_smooth(
  smooth,
  model,
  n = 100,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  ...
)

# S3 method for class 'mrf.smooth'
eval_smooth(
  smooth,
  model,
  n = 100,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  ...
)

# S3 method for class 't2.smooth'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  dist = NULL,
  ...
)

# S3 method for class 'tensor.smooth'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  dist = NULL,
  ...
)
```

## Arguments

- smooth:

  currently an object that inherits from class `mgcv.smooth`.

- ...:

  arguments passed to other methods

- model:

  a fitted model; currently only
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) models are
  supported.

- n:

  numeric; the number of points over the range of the covariate at which
  to evaluate the smooth.

- n_3d, n_4d:

  numeric; the number of points over the range of last covariate in a 3D
  or 4D smooth. The default is `NULL` which achieves the standard
  behaviour of using `n` points over the range of all covariate,
  resulting in `n^d` evaluation points, where `d` is the dimension of
  the smooth. For `d > 2` this can result in very many evaluation points
  and slow performance. For smooths of `d > 4`, the value of `n_4d` will
  be used for all dimensions `> 4`, unless this is `NULL`, in which case
  the default behaviour (using `n` for all dimensions) will be observed.

- data:

  an optional data frame of values to evaluate `smooth` at.

- unconditional:

  logical; should confidence intervals include the uncertainty due to
  smoothness selection? If `TRUE`, the corrected Bayesian covariance
  matrix will be used.

- overall_uncertainty:

  logical; should the uncertainty in the model constant term be included
  in the standard error of the evaluate values of the smooth?

- dist:

  numeric; if greater than 0, this is used to determine when a location
  is too far from data to be plotted when plotting 2-D smooths. The data
  are scaled into the unit square before deciding what to exclude, and
  `dist` is a distance within the unit square. See
  [`mgcv::exclude.too.far()`](https://rdrr.io/pkg/mgcv/man/exclude.too.far.html)
  for further details.

- clip:

  logical; should evaluation points be clipped to the boundary of a soap
  film smooth? The default is `FALSE`, which will return `NA` for any
  point that is deemed to lie outside the boundary of the soap film.
