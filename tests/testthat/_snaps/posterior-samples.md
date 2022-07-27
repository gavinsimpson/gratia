# fitted_samples example output doesn't change

    Code
      fs
    Output
      # A tibble: 5,000 x 3
           row  draw fitted
         <int> <int>  <dbl>
       1     1     1   4.76
       2     2     1   5.70
       3     3     1   4.16
       4     4     1  11.0 
       5     5     1  11.1 
       6     6     1   4.47
       7     7     1   5.67
       8     8     1   6.86
       9     9     1   9.76
      10    10     1   7.39
      # ... with 4,990 more rows
      # i Use `print(n = ...)` to see more rows

# smooth_samples example output doesn't change

    Code
      samples
    Output
      # A tibble: 1,000 x 8
         smooth term  type  by_variable     .x1   row  draw  value
         <chr>  <chr> <chr> <chr>         <dbl> <int> <int>  <dbl>
       1 s(x0)  s(x0) TPRS  <NA>        0.00131     1     1 -1.07 
       2 s(x0)  s(x0) TPRS  <NA>        0.00633     2     1 -1.04 
       3 s(x0)  s(x0) TPRS  <NA>        0.0114      3     1 -1.02 
       4 s(x0)  s(x0) TPRS  <NA>        0.0164      4     1 -0.997
       5 s(x0)  s(x0) TPRS  <NA>        0.0214      5     1 -0.975
       6 s(x0)  s(x0) TPRS  <NA>        0.0264      6     1 -0.952
       7 s(x0)  s(x0) TPRS  <NA>        0.0314      7     1 -0.929
       8 s(x0)  s(x0) TPRS  <NA>        0.0364      8     1 -0.906
       9 s(x0)  s(x0) TPRS  <NA>        0.0415      9     1 -0.882
      10 s(x0)  s(x0) TPRS  <NA>        0.0465     10     1 -0.859
      # ... with 990 more rows
      # i Use `print(n = ...)` to see more rows

