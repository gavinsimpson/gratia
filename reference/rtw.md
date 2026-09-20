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

## Details

Parameters must be finite numeric vectors of length one or a common
nonzero length. Length-one parameters are expanded to the common length;
partial recycling is not supported. If all parameters are empty, an
empty numeric vector is returned; mixing empty and nonempty parameters
is an error.
