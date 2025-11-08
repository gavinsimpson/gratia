# Simulator for tweedie LSS models

Simulate random deviates from a Tweedie distribution with given
parameters \\\mu\\, \\p\\, and \\\phi\\. Works with vector values for
all parameters, unlike the version on *mgcv*.

## Usage

``` r
rtw(mu, p, phi)
```

## Arguments

- mu:

  numeric vector of mean values of Tweedie distribution.

- p:

  numeric vector of values for the power parameter of the Tweedie
  distribution.

- phi:

  numeric vector of values for the scale parameter \\\phi\\ of the
  Tweedie distribution.
