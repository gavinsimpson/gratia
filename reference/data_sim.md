# Simulate example data for fitting GAMs

A tidy reimplementation of the functions implemented in
[`mgcv::gamSim()`](https://rdrr.io/pkg/mgcv/man/gamSim.html) that can be
used to fit GAMs. An new feature is that the sampling distribution can
be applied to all the example types.

## Usage

``` r
data_sim(
  model = "eg1",
  n = 400,
  scale = NULL,
  theta = 3,
  power = 1.5,
  dist = c("normal", "poisson", "binary", "negbin", "tweedie", "gamma", "ocat",
    "ordered categorical"),
  n_cat = 4,
  cuts = c(-1, 0, 5),
  seed = NULL,
  gfam_families = c("binary", "tweedie", "normal")
)
```

## Arguments

- model:

  character; either `"egX"` where `X` is an integer `1:7`, or the name
  of a model. See Details for possible options.

- n:

  numeric; the number of observations to simulate.

- scale:

  numeric; the level of noise to use.

- theta:

  numeric; the dispersion parameter \\\theta\\ to use. The default is
  entirely arbitrary, chosen only to provide simulated data that
  exhibits extra dispersion beyond that assumed by under a Poisson.

- power:

  numeric; the Tweedie power parameter.

- dist:

  character; a sampling distribution for the response variable.
  `"ordered categorical"` is a synonym of `"ocat"`.

- n_cat:

  integer; the number of categories for categorical response. Currently
  only used for `distr %in% c("ocat", "ordered categorical")`.

- cuts:

  numeric; vector of cut points on the latent variable, excluding the
  end points `-Inf` and `Inf`. Must be one fewer than the number of
  categories: `length(cuts) == n_cat - 1`.

- seed:

  numeric; the seed for the random number generator. Passed to
  [`base::set.seed()`](https://rdrr.io/r/base/Random.html).

- gfam_families:

  character; a vector of distributions to use in generating data with
  grouped families for use with `family = gfam()`. The allowed
  distributions as as per `dist`.

## Details

`data_sim()` can simulate data from several underlying models of known
true functions. The available options currently are:

- `"eg1"`: a four term additive true model. This is the classic Gu &
  Wahba four univariate term test model. See
  [`gw_functions`](https://gavinsimpson.github.io/gratia/reference/gw_functions.md)
  for more details of the underlying four functions.

- `"eg2"`: a bivariate smooth true model.

- `"eg3"`: an example containing a continuous by smooth (varying
  coefficient) true model. The model is \\\hat{y}\_i =
  f_2(x\_{1i})x\_{2i}\\ where the function \\f_2()\\ is \\f_2(x) = 0.2
  \* x^{11} \* (10 \* (1 - x))^6 + 10 \* (10 \* x)^3 \* (1 - x)^{10}\\.

- `"eg4"`: a factor by smooth true model. The true model contains a
  factor with 3 levels, where the response for the *n*th level follows
  the *n*th Gu & Wahba function (for \\n \in {1, 2, 3}\\).

- `"eg5"`: an additive plus factor true model. The response is a linear
  combination of the Gu & Wahba functions 2, 3, 4 (the latter is a null
  function) plus a factor term with four levels.

- `"eg6"`: an additive plus random effect term true model.

- ´"eg7"`: a version of the model in `"eg1"\`, but where the covariates
  are correlated.

- `"gwf2"`: a model where the response is Gu & Wahba's \\f_2(x_i)\\ plus
  noise.

- `"lwf6"`: a model where the response is Luo & Wahba's "example 6"
  function \\sin(2(4x-2)) + 2 exp(-256(x-0.5)^2)\\ plus noise.

- `"gfam"`: simulates data for use with GAMs with
  `family = gfam(families)`. See example in
  [`mgcv::gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html). If this
  model is specified then `dist` is ignored and `gfam_families` is used
  to specify which distributions are included in the simulated data. Can
  be a vector of any of the families allowed by `dist`. For
  `"ocat" %in% gfam_families` (or `"ordered categorical"`), 4 classes
  are assumed, which can't be changed. Link functions used are
  `"identity"` for `"normal"`, `"logit"` for `"binary"`, `"ocat"`, and
  `"ordered categorical"`, and `"exp"` elsewhere.

The random component providing noise or sampling variation can follow
one of the distributions, specified via argument `dist`

- `"normal"`: Gaussian,

- `"poisson"`: Poisson,

- `"binary"`: Bernoulli,

- `"negbin"`: Negative binomial,

- `"tweedie"`: Tweedie,

- `"gamma"`: gamma , and

- `"ordered categorical"`: ordered categorical

Other arguments provide the parameters for the distribution.

## References

Gu, C., Wahba, G., (1993). Smoothing Spline ANOVA with Component-Wise
Bayesian "Confidence Intervals." *J. Comput. Graph. Stat.* **2**,
97–117.

Luo, Z., Wahba, G., (1997). Hybrid adaptive splines. *J. Am. Stat.
Assoc.* **92**, 107–116.

## Examples

``` r
data_sim("eg1", n = 100, seed = 1)
#> # A tibble: 100 x 10
#>          y       x0      x1      x2       x3       f      f0     f1     f2    f3
#>      <dbl>    <dbl>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>  <dbl>  <dbl> <dbl>
#>  1 14.532  0.26551  0.65472 0.26751 0.67371  13.713  1.4814  3.7041 8.5277     0
#>  2 16.113  0.37212  0.35320 0.21865 0.094858 12.735  1.8408  2.0267 8.8680     0
#>  3  9.5835 0.57285  0.27026 0.51680 0.49260   6.4103 1.9478  1.7169 2.7456     0
#>  4 15.687  0.90821  0.99268 0.26895 0.46155  16.349  0.56879 7.2817 8.4980     0
#>  5  8.2216 0.20168  0.63349 0.18117 0.37522  12.792  1.1841  3.5501 8.0578     0
#>  6  9.9034 0.89839  0.21321 0.51858 0.99110   4.9081 0.62765 1.5318 2.7487     0
#>  7  5.9362 0.94468  0.12937 0.56278 0.17635   4.6020 0.34587 1.2953 2.9609     0
#>  8 10.839  0.66080  0.47812 0.12916 0.81344   9.7565 1.7502  2.6019 5.4045     0
#>  9 16.883  0.62911  0.92407 0.25637 0.068447 16.909  1.8377  6.3481 8.7237     0
#> 10  7.3603 0.061786 0.59876 0.71794 0.40045   6.3401 0.38578 3.3119 2.6424     0
#> # i 90 more rows

# an ordered categorical response
data_sim("eg1", n = 100, dist = "ocat", n_cat = 4, cuts = c(-1, 0, 5))
#> # A tibble: 100 x 11
#>        y      x0        x1       x2       x3         f      f0     f1         f2
#>    <int>   <dbl>     <dbl>    <dbl>    <dbl>     <dbl>   <dbl>  <dbl>      <dbl>
#>  1     3 0.51910 0.96261   0.44232  0.93060   3.9183   1.9964  6.8566 3.2808    
#>  2     1 0.73659 0.73986   0.96773  0.39218  -2.3512   1.4725  4.3917 0.00015734
#>  3     1 0.13467 0.73325   0.48459  0.15885  -0.25763  0.82112 4.3340 2.8028    
#>  4     4 0.65699 0.53576   0.25246  0.31995   5.2436   1.7616  2.9198 8.7777    
#>  5     3 0.70506 0.0022730 0.25969  0.30697   3.0598   1.5991  1.0046 8.6716    
#>  6     2 0.45774 0.60894   0.54202  0.10781  -0.017586 1.9824  3.3800 2.8356    
#>  7     3 0.71911 0.83680   0.64988  0.97933   1.9537   1.5446  5.3313 3.2933    
#>  8     3 0.93467 0.75152   0.33642  0.49690   3.0976   0.40759 4.4954 6.4102    
#>  9     1 0.25543 0.45273   0.060950 0.093075 -3.0971   1.4381  2.4731 1.2073    
#> 10     3 0.46229 0.53579   0.45131  0.21177  -0.17267  1.9860  2.9200 3.1369    
#> # i 90 more rows
#> # i 2 more variables: f3 <dbl>, latent <dbl>
```
