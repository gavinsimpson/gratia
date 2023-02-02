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

# smooth_samples example output doesn't change

    Code
      samples
    Output
      # A tibble: 1,000 x 8
         smooth term  type  by_variable   row  draw  value      x0
         <chr>  <chr> <chr> <chr>       <int> <int>  <dbl>   <dbl>
       1 s(x0)  s(x0) TPRS  <NA>            1     1 -1.07  0.00131
       2 s(x0)  s(x0) TPRS  <NA>            1     2 -1.42  0.00131
       3 s(x0)  s(x0) TPRS  <NA>            1     3 -1.49  0.00131
       4 s(x0)  s(x0) TPRS  <NA>            1     4 -1.32  0.00131
       5 s(x0)  s(x0) TPRS  <NA>            1     5 -0.994 0.00131
       6 s(x0)  s(x0) TPRS  <NA>            2     1 -1.04  0.00633
       7 s(x0)  s(x0) TPRS  <NA>            2     2 -1.38  0.00633
       8 s(x0)  s(x0) TPRS  <NA>            2     3 -1.46  0.00633
       9 s(x0)  s(x0) TPRS  <NA>            2     4 -1.30  0.00633
      10 s(x0)  s(x0) TPRS  <NA>            2     5 -0.976 0.00633
      # ... with 990 more rows

