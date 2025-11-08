# Is an object one of mgcv's family objects?

Checks to determine if `object` is a family object of one of the three
or so types that *mgcv* produces.

## Usage

``` r
is_mgcv_family(object)
```

## Arguments

- object:

  the object to test.

## Value

A logical vector of length 1, indicating if `object` is one of *mgcv*'s
(`TRUE`), or otherwise (`FALSE`).
