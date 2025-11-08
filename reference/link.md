# Extract link and inverse link functions from models

Returns the link or its inverse from an estimated model, and provides a
simple way to extract these functions from complex models with multiple
links, such as location scale models.

## Usage

``` r
link(object, ...)

# S3 method for class 'family'
link(object, parameter = NULL, which_eta = NULL, ...)

# S3 method for class 'gam'
link(object, parameter = NULL, which_eta = NULL, ...)

# S3 method for class 'bam'
link(object, parameter = NULL, which_eta = NULL, ...)

# S3 method for class 'gamm'
link(object, ...)

# S3 method for class 'glm'
link(object, ...)

# S3 method for class 'list'
link(object, ...)

inv_link(object, ...)

# S3 method for class 'family'
inv_link(object, parameter = NULL, which_eta = NULL, ...)

# S3 method for class 'gam'
inv_link(object, parameter = NULL, which_eta = NULL, ...)

# S3 method for class 'bam'
inv_link(object, parameter = NULL, which_eta = NULL, ...)

# S3 method for class 'gamm'
inv_link(object, ...)

# S3 method for class 'list'
inv_link(object, ...)

# S3 method for class 'glm'
inv_link(object, ...)

extract_link(family, ...)

# S3 method for class 'family'
extract_link(family, inverse = FALSE, ...)

# S3 method for class 'general.family'
extract_link(family, parameter, inverse = FALSE, which_eta = NULL, ...)
```

## Arguments

- object:

  a family object or a fitted model from which to extract the family
  object. Models fitted by
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html),
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html), and
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html) are
  currently supported.

- ...:

  arguments passed to other methods.

- parameter:

  character; which parameter of the distribution. Usually `"location"`
  but `"scale"` and `"shape"` may be provided for location scale models.
  Other options include `"mu"` as a synonym for `"location"`, `"sigma"`
  for the scale parameter in
  [`mgcv::gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html), `"pi"`
  for the zero-inflation term in
  [`mgcv::ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html),
  `"power"` for the
  [`mgcv::twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html) power
  parameter, `"xi"`, the shape parameter for
  [`mgcv::gevlss()`](https://rdrr.io/pkg/mgcv/man/gevlss.html),
  `"epsilon"` or `"skewness"` for the skewness and `"delta"` or
  `"kurtosis"` for the kurtosis parameter for
  [`mgcv::shash()`](https://rdrr.io/pkg/mgcv/man/shash.html), or `"phi"`
  for the scale parameter of
  [`mgcv::gammals()`](https://rdrr.io/pkg/mgcv/man/gammals.html) &
  [`mgcv::twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html).

- which_eta:

  numeric; the linear predictor to extract for families
  [`mgcv::mvn()`](https://rdrr.io/pkg/mgcv/man/mvn.html) and
  [`mgcv::multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html).

- family:

  a family object, the result of a call to
  [`family()`](https://rdrr.io/r/stats/family.html).

- inverse:

  logical; return the inverse of the link function?

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()

link(gaussian())
#> function (mu) 
#> mu
#> <environment: namespace:stats>
link(nb())
#> function (mu) 
#> log(mu)
#> <environment: namespace:stats>

inv_link(nb())
#> function (eta) 
#> pmax(exp(eta), .Machine$double.eps)
#> <environment: namespace:stats>

dat <- data_sim("eg1", seed = 4234)
mod <- gam(list(y ~ s(x0) + s(x1) + s(x2) + s(x3), ~1),
  data = dat,
  family = gaulss
)

link(mod, parameter = "scale")
#> function (mu) 
#> log(1/mu - 0.01)
#> <environment: 0x559dd71561e0>
inv_link(mod, parameter = "scale")
#> function (eta) 
#> 1/(exp(eta) + 0.01)
#> <environment: 0x559dd71561e0>

## Works with `family` objects too
link(shash(), parameter = "skewness")
#> function (mu) 
#> mu
#> <environment: namespace:stats>
```
