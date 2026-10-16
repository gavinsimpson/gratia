# Surrogate residuals for ordered categorical GAMs

Draw continuous residuals for models fitted with
[`mgcv::ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html), using the
latent-variable method of Liu and Zhang (2018). No additional package is
required.

## Usage

``` r
surrogate_residuals(model, seed = NULL, ...)

# S3 method for class 'gam'
surrogate_residuals(model, seed = NULL, conditioning = NULL, ...)

# Default S3 method
surrogate_residuals(model, seed = NULL, ...)
```

## Arguments

- model:

  a fitted model object.

- seed:

  integer or `NULL`; random seed for randomised residuals. With `NULL`,
  calculations use the current random-number state. An explicit seed
  preserves the caller's state, including when none was previously
  initialised.

- ...:

  arguments passed to other methods.

- conditioning:

  character or `NULL`; only `"conditional"` is supported. The default,
  `NULL`, holds fitted random effects fixed in the same way.

## Value

A numeric vector with one residual per observation. With `na.exclude`,
excluded fitting observations are restored as `NA`; with `na.omit`, only
observations used for fitting are returned.

## Details

An ordered categorical model represents category `j` by an interval
between two thresholds on a continuous latent scale. For each
observation, a surrogate response is drawn from the fitted logistic
distribution restricted to its observed category's interval. The
residual subtracts the fitted latent mean from this surrogate response.

Equivalently, draw a probability uniformly between the fitted
probabilities `Pr(Y < y)` and `Pr(Y <= y)`, then apply
[`stats::qlogis()`](https://rdrr.io/r/stats/Logistic.html). Under the
assumed model with known parameters, the residuals have a logistic
distribution with location zero and scale one: mean zero and variance
`pi^2 / 3`. Fitted parameters affect this reference approximation. The
identity link reported by
[`ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) describes the latent
mean; its latent distribution is logistic.

This calculation shares the randomised probabilities used by
[`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md),
which instead applies [`qnorm()`](https://rdrr.io/r/stats/Normal.html)
for normal scores. Both tails are calculated using log probabilities to
retain accuracy when ordinary probabilities round to zero or one.
Fitting weights do not change the scale of the latent logistic
distribution.

Supported fits are `gam` and `bam` models with a single
[`ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) family. Fitted random
effects are held fixed. Grouped families and other ordinal model classes
are not supported. Use `type = "surrogate"` in
[`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
[`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md),
[`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md)
and other residual diagnostic plots. QQ and worm plots require
`method = NULL` and use logistic reference quantiles. For
[`ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html), the linear predictor
is the fitted latent mean, so
[`residuals_linpred_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_linpred_plot.md)
plots residuals against these fitted values.

Each call draws one residual per observation. Set `seed` for
reproducibility; repeating plots with different seeds helps assess
whether a pattern persists across residual draws. An explicit seed
preserves the caller's random-number state. With `seed = NULL`, sampling
uses and advances the current state.

## References

Liu, D. and Zhang, H. (2018). Residuals and Diagnostics for Ordinal
Regression Models: A Surrogate Approach. Journal of the American
Statistical Association, 113(522), 845–854.
[doi:10.1080/01621459.2017.1292915](https://doi.org/10.1080/01621459.2017.1292915)
.

## See also

[`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md),
[`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
[diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md)

## Examples

``` r
library("mgcv")
set.seed(359)
dat <- data.frame(x = runif(200, -2, 2))
latent <- sin(dat$x) + rlogis(nrow(dat))
dat$y <- findInterval(latent, c(-1, 0, 2)) + 1L
fit <- gam(y ~ s(x), data = dat, family = ocat(R = 4), method = "REML")
head(surrogate_residuals(fit, seed = 42))
#> [1]  3.8326093  0.8855870 -0.3476107  0.1092911 -0.4888706  1.4880031
appraise(fit, type = "surrogate", seed = 42)
```
