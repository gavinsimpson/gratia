# Describe a model family's distribution parameters

Inspect the meaning and order of distribution parameters independently
of the numerical representation used by the fitting package.

## Usage

``` r
family_parameters(object, ...)
```

## Arguments

- object:

  A fitted mgcv model, an existing GAM wrapper, or an mgcv family
  object. Other fitting packages require their own specification.

- ...:

  Arguments reserved for methods.

## Value

A tibble with `parameter`, `symbol`, `definition`, `native`,
`predictor`, `source`, `link`, `component`, `returned`, and
`uncertainty`. Predictor indices follow the fitting package's equation
order. Global parameters have no predictor and are omitted from fitted
parameter output; they remain fixed during coefficient sampling. Derived
rows identify constraints such as a multinomial reference category. The
`package` and `family` attributes identify the parameterisation
provider. The `parameterisation` attribute describes the mathematical
distribution. `expectation_condition` records conditions for a finite
expected response.

## Details

[`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
and
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
default to the parameters marked `returned`. On the response scale these
are natural distribution parameters, which can differ from the fitting
package's native response predictions. For example, Gaussian LSS output
contains standard deviation, whereas mgcv predicts reciprocal standard
deviation. GEV location is not its expectation. `link` describes the
native representation: for example, `ziplss()` uses an identity link for
log rate, while natural rate is the exponential of that predictor. The
sinh-arcsinh skewness and tailweight parameters control distribution
shape; they are not the response's standardized skewness and kurtosis.

[`link()`](https://gavinsimpson.github.io/gratia/reference/link.md) and
[`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
retain the fitting package's native conventions and their existing
aliases. Use the definitions here to interpret them.

## See also

[`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md),
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)

## Examples

``` r
family_parameters(mgcv::gevlss())
#> # A tibble: 3 x 10
#>   parameter symbol definition   native predictor source link  component returned
#>   <chr>     <chr>  <chr>        <chr>      <int> <chr>  <chr> <chr>     <lgl>   
#> 1 location  mu     Location pa~ locat~         1 predi~ iden~ NA        TRUE    
#> 2 scale     sigma  Positive sc~ log s~         2 predi~ iden~ NA        TRUE    
#> 3 shape     xi     Distributio~ shape          3 predi~ logit NA        TRUE    
#> # i 1 more variable: uncertainty <chr>
family_parameters(mgcv::gaulss())
#> # A tibble: 2 x 10
#>   parameter symbol definition   native predictor source link  component returned
#>   <chr>     <chr>  <chr>        <chr>      <int> <chr>  <chr> <chr>     <lgl>   
#> 1 mean      mu     Mean of the~ mean           1 predi~ iden~ NA        TRUE    
#> 2 sd        sigma  Standard de~ recip~         2 predi~ logb  NA        TRUE    
#> # i 1 more variable: uncertainty <chr>
family_parameters(mgcv::ziplss())
#> # A tibble: 2 x 10
#>   parameter   symbol definition native predictor source link  component returned
#>   <chr>       <chr>  <chr>      <chr>      <int> <chr>  <chr> <chr>     <lgl>   
#> 1 rate        lambda Poisson r~ log r~         1 predi~ iden~ NA        TRUE    
#> 2 presence_p~ p      Probabili~ clogl~         2 predi~ iden~ NA        TRUE    
#> # i 1 more variable: uncertainty <chr>
```
