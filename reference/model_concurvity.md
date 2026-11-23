# Concurvity of an estimated GAM

Concurvity of an estimated GAM

## Usage

``` r
model_concurvity(model, ...)

# S3 method for class 'gam'
model_concurvity(
  model,
  terms = everything(),
  type = c("all", "estimate", "observed", "worst"),
  pairwise = FALSE,
  ...
)

concrvity(
  model,
  terms = everything(),
  type = c("all", "estimate", "observed", "worst"),
  pairwise = FALSE,
  ...
)

# S3 method for class 'gamm4'
model_concurvity(model, ...)

# S3 method for class 'scam'
model_concurvity(model, ...)
```

## Arguments

- model:

  a fitted GAM. Currently only objects of class `"gam"` are supported

- ...:

  arguments passed to other methods.

- terms:

  currently ignored

- type:

  character;

- pairwise:

  logical; extract pairwise concurvity of model terms?

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Examples

``` r
# Simulate predictors that are related nonlinearly
load_mgcv()
set.seed(8)
n <- 100
t <- sort(runif(n))
x <- gw_f2(t) + rnorm(n, sd = 3)
y <- sin(4 * pi * t) + exp(x / 20) + rnorm(n, sd = 0.3)
df <- data.frame(t = t, x = x, y = y)

# Fit both smooths and plot their overall concurvity
m <- gam(y ~ s(t, k = 8) + s(x, k = 8), data = df, method = "REML")
draw(model_concurvity(m))
```
