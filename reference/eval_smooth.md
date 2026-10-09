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
  n_2d = NULL,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  dist = NULL,
  envir = NULL,
  back_transform = FALSE,
  ...
)

# S3 method for class 'soap.film'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_2d = NULL,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  clip = TRUE,
  envir = NULL,
  back_transform = FALSE,
  ...
)

# S3 method for class 'scam_smooth'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_2d = NULL,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  dist = NULL,
  envir = NULL,
  back_transform = FALSE,
  ...
)

# S3 method for class 'fs.interaction'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_2d = NULL,
  data = NULL,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  envir = NULL,
  back_transform = FALSE,
  ...
)

# S3 method for class 'sz.interaction'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_2d = NULL,
  data = NULL,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  envir = NULL,
  back_transform = FALSE,
  ...
)

# S3 method for class 'random.effect'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_2d = NULL,
  data = NULL,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  envir = NULL,
  back_transform = FALSE,
  ...
)

# S3 method for class 'mrf.smooth'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_2d = NULL,
  data = NULL,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  envir = NULL,
  back_transform = FALSE,
  ...
)

# S3 method for class 't2.smooth'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_2d = NULL,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  dist = NULL,
  envir = NULL,
  back_transform = FALSE,
  ...
)

# S3 method for class 'tensor.smooth'
eval_smooth(
  smooth,
  model,
  n = 100,
  n_2d = NULL,
  n_3d = NULL,
  n_4d = NULL,
  data = NULL,
  unconditional = FALSE,
  frequentist = FALSE,
  overall_uncertainty = TRUE,
  dist = NULL,
  envir = NULL,
  back_transform = FALSE,
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
  to evaluate a univariate smooth.

- n_2d:

  numeric; the number of points along each of the first two axes of a
  smooth surface, including surface panels of higher-dimensional
  smooths. The default is 50 in plotting and plot-preparation functions.
  If `NULL`, use `n` instead. Ignored when evaluation `data` are
  supplied. Factor levels are retained, and curves with only one
  continuous covariate use `n`.

- n_3d, n_4d:

  numeric; the number of points along the third axis of a 3D smooth
  (`n_3d`, default 16), or each axis after the first two for smooths of
  dimension four or higher (`n_4d`, default 4). If `NULL`, use `n` for
  those axes. The first two surface axes use `n_2d`.

- data:

  an optional data frame of values to evaluate `smooth` at.

- unconditional:

  logical; if `TRUE` (and only if `frequentist == FALSE`) then the
  bayesian smoothing parameter uncertainty-corrected covariance matrix
  is returned, if available. Whether it is available depends on which
  smoothness selection method was used to fit the model.

- frequentist:

  logical; if `FALSE`, the default, the bayesian covariance matrix is
  returned, otherwise the frequentist covariance matrix.

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

- envir:

  an optional environment supplying functions and constants used in
  model expressions. The available model formula environment is used
  when `NULL`. Covariate observations should be supplied in `data`.

- back_transform:

  logical; generate automatic grids on the original covariate scale? The
  default, `FALSE`, preserves evenly spaced model-scale grids (e.g. in
  `log(x)`). With `TRUE`, recover raw fitting covariates and generate
  grids on their original scale, then evaluate the model expressions
  forwards. Each axis must depend on one distinct original model
  covariate. Functions such as `foo(x)` require no inverse. Fitted
  prediction parameters are retained where provided by the model,
  including for `scale(x)`. When `data` are supplied, use those rows
  without generating a grid; `TRUE` requires raw coordinates for each
  axis. Returned model-coordinate columns keep their usual names and raw
  coordinates are retained in auxiliary `.gratia_raw_*` columns. Draw
  these estimates with `draw(object, back_transform = TRUE)` to display
  the raw coordinates. Estimates containing only transformed coordinates
  give an informative error if drawn on the original scale. This option
  does not change effect values or their intervals at the chosen
  evaluation points.

- clip:

  logical; should evaluation points be clipped to the boundary of a soap
  film smooth? The default is `FALSE`, which will return `NA` for any
  point that is deemed to lie outside the boundary of the soap film.
