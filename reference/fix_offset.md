# Fix the names of a data frame containing an offset variable.

Removes evaluated offset columns while retaining their raw input
variables. Raw inputs are recovered from fitting data if needed;
transformed offset values are never renamed as raw values. A scalar
`offset_val` overrides the raw input only for a single offset with one
unambiguous input variable.

## Usage

``` r
fix_offset(model, newdata, offset_val = NULL)
```

## Arguments

- model:

  a fitted GAM.

- newdata:

  data frame; new values at which to predict at.

- offset_val:

  numeric, optional; if provided, then the offset variable in `newdata`
  is set to this constant value before returning `newdata`

## Value

The original `newdata` is returned with fixed names and possibly
modified offset variable.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", n = 400, dist = "normal", seed = 2)
m <- gam(y ~ s(x0) + s(x1) + offset(x2), data = df, method = "REML")
names(model.frame(m))
#> [1] "y"          "offset(x2)" "x0"         "x1"        
names(fix_offset(m, model.frame(m), offset_val = 1L))
#> [1] "y"  "x2" "x0" "x1"
```
