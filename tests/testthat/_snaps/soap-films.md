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

