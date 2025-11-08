# Check if objects are smooths or are a particular type of smooth

Check if objects are smooths or are a particular type of smooth

## Usage

``` r
is_mgcv_smooth(smooth)

stop_if_not_mgcv_smooth(smooth)

check_is_mgcv_smooth(smooth)

is_mrf_smooth(smooth)
```

## Arguments

- smooth:

  an R object, typically a list

## Details

Check if a smooth inherits from class `"mgcv.smooth"`.
`stop_if_not_mgcv_smooth()` is a wrapper around `is_mgcv_smooth()`,
useful when programming for checking if the supplied object is one of
mgcv's smooths, and throwing a consistent error if not.
`check_is_mgcv_smooth()` is similar to `stop_if_not_mgcv_smooth()` but
returns the result of `is_mgcv_smooth()` invisibly.
