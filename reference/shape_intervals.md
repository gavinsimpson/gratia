# Enforce named shapes on estimates and interval endpoints

Apply shape-enforcing operators to an existing estimate and its lower
and upper endpoints. Construction and calibration of the initial
uncertainty remain the caller's responsibility.

## Usage

``` r
shape_intervals(
  estimate,
  lower,
  upper,
  data,
  constraints,
  interval = c("simultaneous", "pointwise"),
  level = 0.95
)
```

## Arguments

- estimate, lower, upper:

  finite numeric vectors, one value per row of `data`. The lower
  endpoints must not exceed the upper endpoints.

- data:

  a data frame of finite numeric coordinates. Rows may be in any order
  but must form a complete Cartesian product without duplicates.
  Coordinates may be irregularly spaced. A single coordinate gives a
  curve.

- constraints:

  a named list, with coordinate names as names and character vectors of
  restrictions as elements. Codes are `"m+"` (nondecreasing), `"m-"`
  (nonincreasing), `"c+"` (convex), `"c-"` (concave), and `"+"`
  (nonnegative). Curvature restrictions may occur in at most one
  coordinate. Monotonicity may be imposed in several coordinates.
  Positivity applies to the function, regardless of which coordinate
  carries its code.

- interval:

  character; whether the input is a `"simultaneous"` band or
  `"pointwise"` limits. This describes the input, and does not calibrate
  it.

- level:

  numeric; the nominal level of the input, strictly between zero and
  one.

## Value

A tibble containing `data`, `.estimate`, `.lower_ci`, and `.upper_ci`,
in the original row order. The `interval_info` attribute records the
operator, restrictions, input interval type and coverage scope. Standard
errors and Gaussian critical values are not recalculated.

## Details

The operator first thresholds at zero if requested, then sorts each
monotone coordinate in the order given by `constraints`, then takes the
greatest convex minorant in the curvature coordinate. Concavity uses the
negative of the greatest convex minorant of the negative values. The
minorant uses actual coordinate distances. Sorting gives equal mass to
each grid coordinate, including on irregular grids; it does not
approximate Lebesgue-weighted rearrangement on such grids.

These finite-grid operators preserve order, leave a function with the
requested shapes unchanged, and do not increase maximum absolute
distance. Consequently, whenever the entire initial band contains a
target with the requested shapes on the grid, the transformed band also
contains it. Its maximum width cannot increase. This is a deterministic
statement about a whole-grid event; it does not guarantee nominal
pointwise coverage after transforming pointwise limits. Nor does it
correct smoothing bias or an invalid initial uncertainty calculation.
For a posterior supported on the restricted functions the analogous
event inclusion applies to posterior probability; this operation does
not construct that posterior.

The guarantees concern values on the supplied grid. Linear interpolation
of curves, or multilinear interpolation on rectangles, also has the
named directional shapes between grid points. The output is not a fitted
spline and need not obey coefficient inequalities, centring, integral
constraints, or other equalities. Use
[`shape_confidence_set()`](https://gavinsimpson.github.io/gratia/reference/shape_confidence_set.md)
to represent intersections with such restrictions. Several curvature
coordinates are unsupported: sequential directional convexification need
not preserve earlier convexity. Extreme numeric ranges that prevent
reliable slope comparisons or interpolation in double precision produce
an error.

## References

Chernozhukov, V., Fernandez-Val, I. and Galichon, A. (2009). Improving
point and interval estimators of monotone functions by rearrangement.
*Biometrika*, **96**, 559–575.
[doi:10.1093/biomet/asp030](https://doi.org/10.1093/biomet/asp030) .

Chen, X., Chernozhukov, V., Fernandez-Val, I., Kostyshak, S. and Luo, Y.
(2021). Shape-Enforcing Operators for Generic Point and Interval
Estimators of Functions. *Journal of Machine Learning Research*,
**22**(220), 1–42. <https://www.jmlr.org/papers/v22/20-513.html>.

## Examples

``` r
x <- c(0, 0.1, 0.4, 0.8, 1)
f <- c(0.1, 0.4, 0.2, 0.9, 0.7)
shape_intervals(f, f - 0.3, f + 0.3, data.frame(x = x),
  constraints = list(x = c("+", "m+")))
#> # A tibble: 5 x 4
#>       x .estimate .lower_ci .upper_ci
#>   <dbl>     <dbl>     <dbl>     <dbl>
#> 1   0         0.1       0         0.4
#> 2   0.1       0.2       0         0.5
#> 3   0.4       0.4       0.1       0.7
#> 4   0.8       0.7       0.4       1  
#> 5   1         0.9       0.6       1.2
```
