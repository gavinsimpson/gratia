# Intersect a band with linear restrictions on coefficients

Represent the set of coefficient vectors whose evaluated functions are
inside an initial band and satisfy supplied linear inequalities and
equalities.

## Usage

``` r
shape_confidence_set(
  X,
  lower,
  upper,
  Ain = NULL,
  bin = NULL,
  C = NULL,
  d = NULL,
  level = 0.95,
  interpretation = c("confidence", "credible"),
  band_type = c("unspecified", "simultaneous", "pointwise")
)

in_shape_set(object, coefficients, tolerance = 1e-08)
```

## Arguments

- X:

  A finite numeric matrix that maps coefficients to function values at
  the evaluation points. Each column corresponds to a coefficient.

- lower, upper:

  Numeric vectors of lower and upper limits, one per row of `X`.
  Infinite limits are allowed. Missing values are not allowed.

- Ain:

  A finite numeric matrix defining inequalities `Ain %*% b >= bin`. It
  must have the same number of columns as `X`. `NULL` means no
  inequalities.

- bin:

  A finite numeric vector, one entry per row of `Ain`. If `NULL`, zero
  is used for every inequality.

- C:

  A finite numeric matrix defining equalities `C %*% b = d`. It must
  have the same number of columns as `X`. `NULL` means no equalities.

- d:

  A finite numeric vector, one entry per row of `C`. If `NULL`, zero is
  used for every equality.

- level:

  A number strictly between zero and one recording the nominal level of
  the initial band. This does not calibrate the band.

- interpretation:

  Is the initial band a confidence or credible band?

- band_type:

  Record whether the initial limits were calibrated jointly
  (`"simultaneous"`), separately at each evaluation point
  (`"pointwise"`), or whether their calibration is unspecified
  (`"unspecified"`, the default).

- object:

  A set created by `shape_confidence_set()`.

- coefficients:

  A finite numeric vector, or a finite numeric matrix with one candidate
  coefficient vector in each column. Rows must correspond to columns of
  `object$X` in the same order; names are not used for matching.

- tolerance:

  A finite non-negative number used as an absolute tolerance in function
  values and constraint residuals. The default is `1e-8`. Setting it to
  zero uses exact floating-point comparisons. Multiplying a constraint
  row by a constant changes the effect of an absolute tolerance.

## Value

`shape_confidence_set()` returns a list of class
`"shape_confidence_set"` containing `X`, `lower`, `upper`, `Ain`, `bin`,
`C`, `d`, `level`, `interpretation`, and `band_type`. Missing constraint
matrices are stored as matrices with zero rows. `in_shape_set()` returns
a logical vector, one value per candidate coefficient vector.

## Details

The returned object represents the set \$\$\\b: \mathrm{lower} \le Xb
\le \mathrm{upper},\quad A\_{in}b \ge b\_{in},\quad Cb=d\\.\$\$ The
matrices are retained explicitly; the function does not optimise over
the set, test whether it is empty, or draw samples from it. Redundant
and inconsistent restrictions are allowed. Inconsistent restrictions
represent an empty set.

The initial band restricts function values only at the supplied
evaluation points. The extent of the shape restriction depends on the
supplied coefficient inequalities. For example, appropriate spline
coefficient inequalities can guarantee monotonicity throughout a
spline's domain, whereas differences between evaluated function values
guarantee it only on that grid. This function does not infer
restrictions from a fitted model or extend restrictions between
evaluation points.

If the true coefficient vector satisfies the supplied restrictions, it
is in the intersection exactly when its evaluated function is in the
initial band. Thus intersection preserves the coverage of an initially
valid simultaneous confidence band, for truths satisfying those
restrictions. Pointwise limits alone do not supply a simultaneous
confidence set. For credible bands, the same posterior probability
statement requires the posterior to be supported entirely on the
restricted coefficient space. Intersection does not fix smoothing bias,
invalid initial calibration, or an incorrect restriction.

Coordinatewise minima and maxima of functions in this set need not
themselves belong to the set. This matters for coupled restrictions such
as an integral equal to one: two ordered functions with that integral
must coincide almost everywhere. Retaining the whole set avoids
assigning such equalities to separate endpoint curves.

## Examples

``` r
# Two non-negative masses whose sum is one.
region <- shape_confidence_set(
  X = diag(2), lower = c(0.1, 0.2), upper = c(0.8, 0.9),
  Ain = diag(2), C = matrix(c(1, 1), nrow = 1), d = 1,
  band_type = "simultaneous"
)
in_shape_set(region, c(0.4, 0.6))
#> [1] TRUE
in_shape_set(region, c(0.4, 0.7))
#> [1] FALSE
```
