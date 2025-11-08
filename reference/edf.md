# Effective degrees of freedom for smooths and GAMs

Extracts the effective degrees of freedom (EDF) for model smooth terms
or overall EDF for fitted GAMs

## Usage

``` r
edf(object, ...)

# S3 method for class 'gam'
edf(
  object,
  select = NULL,
  smooth = deprecated(),
  type = c("default", "unconditional", "alternative"),
  partial_match = FALSE,
  ...
)

model_edf(object, ..., type = c("default", "unconditional", "alternative"))
```

## Arguments

- object:

  a fitted model from which to extract smooth-specific EDFs.

- ...:

  arguments passed to methods.

- select:

  character, logical, or numeric; which smooths EDF to extract. If
  `NULL`, the default, EDFs for all smooths will be returned. Numeric
  `select` indexes the smooths in the order they are specified in the
  formula and stored in `object`. Character `select` matches the labels
  for smooths as shown for example in the output from `summary(object)`.
  Logical `select` operates as per numeric `select` in the order that
  smooths are stored.

- smooth:

  **\[deprecated\]** Use `select` instead.

- type:

  character: which type of EDF to return. `"default"` returns the
  standard EDF; `"unconditional"` selects the EDF corrected for
  smoothness parameter selection, if available; `"alternative"` returns
  the alternative formulation for EDF from Wood (2017, pp. 252)

- partial_match:

  logical; should smooths be selected by partial matches with `select`?
  If `TRUE`, `select` can only be a single string to match against.

## Details

Multiple formulations for the effective degrees of freedom are
available. The additional uncertainty due to selection of smoothness
parameters can be taken into account when computing the EDF of smooths.
This form of the EDF is available with `type = "unconditional"`.

Wood (2017; pp. 252) describes an alternative EDF for the model
\$\$\mathrm{EDF} = 2\mathrm{tr}(\mathbf{F}) -
\mathrm{tr}(\mathbf{FF}),\$\$ where \\\mathrm{tr}\\ is the matrix trace
and \\\mathbf{F}\\ is a matrix mapping unpenalised coefficient estimates
to the penalized coefficient estimates. The trace of \\\mathbf{F}\\ is
effectively the average shrinkage of the coefficients multiplied by the
number of coefficients (Wood, 2017). Smooth-specific EDFs then are
obtained by summing up the relevant elements of
\\\mathrm{diag}(2\mathbf{F} - \mathbf{FF})\\.

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", n = 400, seed = 42)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

# extract the EDFs for all smooths
edf(m)
#> # A tibble: 4 x 2
#>   .smooth   .edf
#>   <chr>    <dbl>
#> 1 s(x0)   3.4248
#> 2 s(x1)   3.2213
#> 3 s(x2)   7.9049
#> 4 s(x3)   1.8847

# or selected smooths
edf(m, select = c("s(x0)", "s(x2)"))
#> # A tibble: 2 x 2
#>   .smooth   .edf
#>   <chr>    <dbl>
#> 1 s(x0)   3.4248
#> 2 s(x2)   7.9049

# accounting for smoothness parameter uncertainty
edf(m, type = "unconditional")
#> # A tibble: 4 x 2
#>   .smooth   .edf
#>   <chr>    <dbl>
#> 1 s(x0)   3.7697
#> 2 s(x1)   3.8728
#> 3 s(x2)   8.0670
#> 4 s(x3)   2.8834

# over EDF of the model, including the intercept
model_edf(m)
#> # A tibble: 1 x 2
#>   .model   .edf
#>   <chr>   <dbl>
#> 1 m      17.436

# can get model EDF for multiple models
m2 <- gam(y ~ s(x0) + s(x1) + s(x3), data = df, method = "REML")
model_edf(m, m2)
#> # A tibble: 2 x 2
#>   .model    .edf
#>   <chr>    <dbl>
#> 1 m      17.436 
#> 2 m2      7.5777
```
