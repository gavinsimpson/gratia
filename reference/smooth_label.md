# Extract the label for a smooth used by 'mgcv'

The label 'mgcv' uses for smooths is useful in many contexts, including
selecting smooths or labelling plots. `smooth_label()` extracts this
label from an 'mgcv' smooth object, i.e. an object that inherits from
class `"mgcv.smooth"`. These would typically be found in the `$smooth`
component of a GAM fitted by
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) or
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html), or related
functions.

## Usage

``` r
smooth_label(object, ...)

# S3 method for class 'gam'
smooth_label(object, id, ...)

# S3 method for class 'mgcv.smooth'
smooth_label(object, ...)
```

## Arguments

- object:

  an R object. Currently, methods for class `"gam"` and for mgcv smooth
  objects inheriting from class `"mgcv.smooth"` are supported.

- ...:

  arguments passed to other methods.

- id:

  numeric; the indices of the smooths whose labels are to be extracted.
  If missing, labels for all smooths in the model are returned.

## Value

A character vector.

## Examples

``` r
load_mgcv()
df <- data_sim("gwf2", n = 100)
m <- gam(y ~ s(x), data = df, method = "REML")

# extract the smooth
sm <- get_smooths_by_id(m, id = 1)[[1]]

# extract the label
smooth_label(sm)
#> [1] "s(x)"

# or directly on the fitted GAM
smooth_label(m$smooth[[1]])
#> [1] "s(x)"

# or extract labels by idex/position
smooth_label(m, id = 1)
#> [1] "s(x)"
```
