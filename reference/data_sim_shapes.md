# Reference functions for shape-constrained GAMs

[`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
supplies analytic reference functions for
[`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html). All
covariates are uniform on `[0, 1]`. Univariate templates return `f1`,
the true function, and its analytic derivatives `d1` and `d2`. The
response contains sampling variation; the derivative columns describe
the truth.

## Univariate templates

|  |  |  |
|----|----|----|
| Model suffix after `sc_` | Function | `xt` in `s(x, bs = "sc", xt = ...)` |
| `increasing` | `plogis(8 * (x - 0.5))` | `"m+"` |
| `decreasing` | `1 - plogis(8 * (x - 0.5))` | `"m-"` |
| `convex` | `(x - 0.5)^2` | `"c+"` |
| `concave` | `1 - (x - 0.5)^2` | `"c-"` |
| `positive` | `0.2 + exp(-16 * (x - 0.5)^2)` | `"+"`, with no intercept |
| `increasing_convex` | `exp(2 * x) / exp(2)` | `c("m+", "c+")` |
| `increasing_concave` | `log1p(4 * x) / log(5)` | `c("m+", "c-")` |
| `decreasing_convex` | `exp(-2 * x)` | `c("m-", "c+")` |
| `decreasing_concave` | `1 - x^2` | `c("m-", "c-")` |

Remove the intercept for the positivity recipe so the constrained smooth
represents the whole function. An unconstrained intercept combined with
a centred smooth does not express the same positivity requirement.

## Several terms and distribution parameters

`sc_additive` adapts mgcv's example. Its raw terms are `gw_f0(x0)`,
`2 * plogis(20 * (x1 - 0.4))`, `gw_f2(x2)` and `10 * (x3 - 0.55)^4`. The
true predictor is their sum minus 1, divided by 4. The second term is
increasing and the fourth is convex. Use `predictors = function(d) d$f`
with `family = poisson()` to sample mgcv's Poisson example.

`sc_tensor` uses `f = x + z + x * z`, increasing in both covariates. Its
`dx` and `dz` columns give the true partial derivatives.

`sc_location_scale` uses log mean `sin(pi * x) / 2` and log dispersion
`-2 + z`, returned in `f` and `f_scale`. It defaults to
[`gammals()`](https://rdrr.io/pkg/mgcv/man/gammals.html) and records
derivatives in `d1`, `d2` and `d_scale`. Constraints act on the
specified predictors; in particular the concavity concerns the log mean.

The recipes use the default `bs = 0` for fitting without bootstrap
replicates. To obtain native constraint-respecting bootstrap coefficient
draws, fit with a positive `bs`, such as `bs = 100`.

## See also

[`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md),
[data_sim_families](https://gavinsimpson.github.io/gratia/reference/data_sim_families.md),
[`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html)

## Examples

``` r
library("mgcv")
d <- data_sim("sc_increasing", n = 200, seed = 2)
m <- scasm(y ~ s(x, bs = "sc", xt = "m+", k = 8), data = d)

d <- data_sim("sc_positive", n = 200, seed = 2)
m <- scasm(y ~ s(x, bs = "sc", xt = "+", k = 8) - 1, data = d)

# \donttest{
d <- data_sim("sc_additive", family = poisson(), seed = 2,
  predictors = function(d) d$f)
m <- scasm(y ~ s(x0) + s(x1, bs = "sc", xt = "m+") + s(x2) +
  s(x3, bs = "sc", xt = "c+"), family = poisson(), data = d)
#> Error in H[jj, ]: subscript out of bounds

d <- data_sim("sc_tensor", seed = 2)
m <- scasm(y ~ te(x, z, bs = c("sc", "sc"), xt = list("m+", "m+"),
  k = c(6, 6)), data = d)

d <- data_sim("sc_location_scale", seed = 2)
m <- scasm(list(y ~ s(x, bs = "sc", xt = "c-"),
  ~s(z, bs = "sc", xt = "m+")), family = gammals(), data = d)
# }
```
