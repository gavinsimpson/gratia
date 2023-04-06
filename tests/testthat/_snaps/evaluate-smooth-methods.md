# evaluate_smooth is deprecated

    Code
      evaluate_smooth(m1, smooth = "s(x0)")
    Warning <lifecycle_warning_deprecated>
      `evaluate_smooth()` was deprecated in gratia 0.7.0.
      i Please use `smooth_estimates()` instead.
    Output
      # A tibble: 100 x 5
         smooth by_variable     x0    est    se
         <chr>  <fct>        <dbl>  <dbl> <dbl>
       1 s(x0)  <NA>        0.0131 -0.929 0.422
       2 s(x0)  <NA>        0.0230 -0.881 0.396
       3 s(x0)  <NA>        0.0329 -0.834 0.372
       4 s(x0)  <NA>        0.0429 -0.786 0.348
       5 s(x0)  <NA>        0.0528 -0.738 0.326
       6 s(x0)  <NA>        0.0627 -0.690 0.305
       7 s(x0)  <NA>        0.0727 -0.643 0.287
       8 s(x0)  <NA>        0.0826 -0.595 0.270
       9 s(x0)  <NA>        0.0925 -0.548 0.255
      10 s(x0)  <NA>        0.102  -0.501 0.242
      # i 90 more rows

