# Add indicators of significant change after SiZeR

Add indicators of significant change after SiZeR

## Usage

``` r
add_sizer(object, type = c("change", "sizer"), ...)

# S3 method for class 'derivatives'
add_sizer(object, type = c("change", "sizer"), ...)

# S3 method for class 'smooth_estimates'
add_sizer(object, type = c("change", "sizer"), derivatives = NULL, ...)
```

## Arguments

- object:

  an R object. Currently supported methods are for classes
  `"derivatives"`.

- type:

  character; `"change"` adds a single variable to `object` indicating
  where the credible interval on the derivative excludes 0. `"sizer"`
  adds two variables indicating whether the derivative is positive or
  negative.

- ...:

  arguments passed to other methods

- derivatives:

  an object of class `"derivatives"`, resulting from a call to
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md).

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 42)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

## first derivatives of all smooths using central finite differences
d <- derivatives(m, type = "central") |>
  add_sizer()

# default adds a .change column
names(d)
#>  [1] ".smooth"     ".by"         ".fs"         ".derivative" ".se"        
#>  [6] ".crit"       ".lower_ci"   ".upper_ci"   ".change"     "x0"         
#> [11] "x1"          "x2"          "x3"         
```
