# smooth estimates can evaluate a so soap film

    Code
      print(sm_so)
    Output
      # A tibble: 8,465 x 9
         .smooth .type     .by   .estimate    .se .bndry .loop      v     w
         <chr>   <chr>     <chr>     <dbl>  <dbl> <lgl>  <int>  <dbl> <dbl>
       1 s(v,w)  Soap film <NA>     -0.879 0.0931 FALSE     NA 0.0121  -0.9
       2 s(v,w)  Soap film <NA>     -0.919 0.0958 FALSE     NA 0.0555  -0.9
       3 s(v,w)  Soap film <NA>     -0.960 0.0987 FALSE     NA 0.0989  -0.9
       4 s(v,w)  Soap film <NA>     -1.00  0.101  FALSE     NA 0.142   -0.9
       5 s(v,w)  Soap film <NA>     -1.04  0.103  FALSE     NA 0.186   -0.9
       6 s(v,w)  Soap film <NA>     -1.09  0.105  FALSE     NA 0.229   -0.9
       7 s(v,w)  Soap film <NA>     -1.13  0.105  FALSE     NA 0.273   -0.9
       8 s(v,w)  Soap film <NA>     -1.17  0.105  FALSE     NA 0.316   -0.9
       9 s(v,w)  Soap film <NA>     -1.22  0.104  FALSE     NA 0.360   -0.9
      10 s(v,w)  Soap film <NA>     -1.26  0.103  FALSE     NA 0.403   -0.9
      # i 8,455 more rows

# smooth estimates can evaluate a so soap film no clipping

    Code
      print(sm_so)
    Output
      # A tibble: 10,160 x 9
         .smooth .type     .by   .estimate   .se .bndry .loop      v     w
         <chr>   <chr>     <chr>     <dbl> <dbl> <lgl>  <int>  <dbl> <dbl>
       1 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.9    -0.9
       2 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.857  -0.9
       3 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.813  -0.9
       4 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.770  -0.9
       5 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.726  -0.9
       6 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.683  -0.9
       7 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.639  -0.9
       8 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.596  -0.9
       9 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.553  -0.9
      10 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.509  -0.9
      # i 10,150 more rows

