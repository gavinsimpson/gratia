# derivatives works for sz smooths

    Code
      print(d)
    Output
      # A tibble: 500 x 11
         .smooth .by   .fs   .derivative   .se .crit .lower_ci .upper_ci      x2 fac  
         <chr>   <chr> <chr>       <dbl> <dbl> <dbl>     <dbl>     <dbl>   <dbl> <fct>
       1 s(x2)   <NA>  <NA>         21.5  5.56  1.96      10.6      32.4 0.00131 <NA> 
       2 s(x2)   <NA>  <NA>         21.5  5.55  1.96      10.7      32.4 0.0114  <NA> 
       3 s(x2)   <NA>  <NA>         21.5  5.49  1.96      10.8      32.3 0.0215  <NA> 
       4 s(x2)   <NA>  <NA>         21.5  5.35  1.96      11.0      31.9 0.0316  <NA> 
       5 s(x2)   <NA>  <NA>         21.4  5.11  1.96      11.3      31.4 0.0417  <NA> 
       6 s(x2)   <NA>  <NA>         21.2  4.80  1.96      11.8      30.6 0.0517  <NA> 
       7 s(x2)   <NA>  <NA>         20.9  4.42  1.96      12.3      29.6 0.0618  <NA> 
       8 s(x2)   <NA>  <NA>         20.6  4.00  1.96      12.7      28.4 0.0719  <NA> 
       9 s(x2)   <NA>  <NA>         20.1  3.59  1.96      13.1      27.2 0.0820  <NA> 
      10 s(x2)   <NA>  <NA>         19.5  3.25  1.96      13.2      25.9 0.0921  <NA> 
      # i 490 more rows
      # i 1 more variable: x0 <dbl>

