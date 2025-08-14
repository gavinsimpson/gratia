# smooth estimates can evaluate a so soap film

    Code
      print(sm_so)
    Output
      # A tibble: 8,627 x 9
         .smooth .type     .by   .estimate    .se .bndry .loop      v        w
         <chr>   <chr>     <chr>     <dbl>  <dbl> <lgl>  <int>  <dbl>    <dbl>
       1 s(v,w)  Soap film <NA>     0.0815 0.0901 FALSE     NA -0.886 -0.154  
       2 s(v,w)  Soap film <NA>     0.0898 0.0904 FALSE     NA -0.886 -0.136  
       3 s(v,w)  Soap film <NA>     0.0981 0.0907 FALSE     NA -0.886 -0.118  
       4 s(v,w)  Soap film <NA>     0.106  0.0911 FALSE     NA -0.886 -0.0994 
       5 s(v,w)  Soap film <NA>     0.115  0.0916 FALSE     NA -0.886 -0.0812 
       6 s(v,w)  Soap film <NA>     0.122  0.0920 FALSE     NA -0.886 -0.0631 
       7 s(v,w)  Soap film <NA>     0.130  0.0924 FALSE     NA -0.886 -0.0449 
       8 s(v,w)  Soap film <NA>     0.137  0.0928 FALSE     NA -0.886 -0.0268 
       9 s(v,w)  Soap film <NA>     0.140  0.0930 FALSE     NA -0.886 -0.00860
      10 s(v,w)  Soap film <NA>     0.144  0.0932 FALSE     NA -0.886  0.00956
      # i 8,617 more rows

# smooth estimates can evaluate a so soap film no clipping

    Code
      print(sm_so)
    Output
      # A tibble: 10,160 x 9
         .smooth .type     .by   .estimate   .se .bndry .loop      v      w
         <chr>   <chr>     <chr>     <dbl> <dbl> <lgl>  <int>  <dbl>  <dbl>
       1 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.898
       2 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.880
       3 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.862
       4 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.844
       5 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.826
       6 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.808
       7 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.789
       8 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.771
       9 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.753
      10 s(v,w)  Soap film <NA>         NA    NA FALSE     NA -0.886 -0.735
      # i 10,150 more rows

# smooth estimates can evaluate a nested so soap film

    Code
      print(sm_so)
    Output
      # A tibble: 7,507 x 9
         .smooth .type     .by   .estimate    .se .bndry .loop      x       y
         <chr>   <chr>     <chr>     <dbl>  <dbl> <lgl>  <int>  <dbl>   <dbl>
       1 s(x,y)  Soap film <NA>     -0.360 0.0254 FALSE     NA -0.974 -0.210 
       2 s(x,y)  Soap film <NA>     -0.357 0.0256 FALSE     NA -0.974 -0.190 
       3 s(x,y)  Soap film <NA>     -0.351 0.0252 FALSE     NA -0.974 -0.170 
       4 s(x,y)  Soap film <NA>     -0.345 0.0249 FALSE     NA -0.974 -0.150 
       5 s(x,y)  Soap film <NA>     -0.341 0.0248 FALSE     NA -0.974 -0.131 
       6 s(x,y)  Soap film <NA>     -0.337 0.0247 FALSE     NA -0.974 -0.111 
       7 s(x,y)  Soap film <NA>     -0.334 0.0248 FALSE     NA -0.974 -0.0913
       8 s(x,y)  Soap film <NA>     -0.330 0.0249 FALSE     NA -0.974 -0.0716
       9 s(x,y)  Soap film <NA>     -0.327 0.0251 FALSE     NA -0.974 -0.0519
      10 s(x,y)  Soap film <NA>     -0.324 0.0252 FALSE     NA -0.974 -0.0322
      # i 7,497 more rows

