# smooth_samples works for a continuous by GAM

    Code
      sm
    Output
      # A tibble: 500 x 8
         smooth term     type  by_variable   row  draw    value       x2
         <chr>  <chr>    <chr> <chr>       <int> <int>    <dbl>    <dbl>
       1 s(x2)  s(x2):x1 TPRS  <NA>            1     1  0.153   0.000206
       2 s(x2)  s(x2):x1 TPRS  <NA>            1     2 -0.453   0.000206
       3 s(x2)  s(x2):x1 TPRS  <NA>            1     3  0.0733  0.000206
       4 s(x2)  s(x2):x1 TPRS  <NA>            1     4 -0.144   0.000206
       5 s(x2)  s(x2):x1 TPRS  <NA>            1     5  0.00382 0.000206
       6 s(x2)  s(x2):x1 TPRS  <NA>            2     1  0.236   0.0103  
       7 s(x2)  s(x2):x1 TPRS  <NA>            2     2 -0.209   0.0103  
       8 s(x2)  s(x2):x1 TPRS  <NA>            2     3  0.222   0.0103  
       9 s(x2)  s(x2):x1 TPRS  <NA>            2     4  0.0197  0.0103  
      10 s(x2)  s(x2):x1 TPRS  <NA>            2     5  0.143   0.0103  
      # i 490 more rows

# smooth_samples works for a simple GAM

    Code
      sm
    Output
      # A tibble: 500 x 8
         smooth term  type  by_variable   row  draw  value     x0
         <chr>  <chr> <chr> <chr>       <int> <int>  <dbl>  <dbl>
       1 s(x0)  s(x0) TPRS  <NA>            1     1 -1.20  0.0131
       2 s(x0)  s(x0) TPRS  <NA>            1     2 -1.89  0.0131
       3 s(x0)  s(x0) TPRS  <NA>            1     3 -1.46  0.0131
       4 s(x0)  s(x0) TPRS  <NA>            1     4 -1.28  0.0131
       5 s(x0)  s(x0) TPRS  <NA>            1     5 -0.319 0.0131
       6 s(x0)  s(x0) TPRS  <NA>            2     1 -1.16  0.0230
       7 s(x0)  s(x0) TPRS  <NA>            2     2 -1.86  0.0230
       8 s(x0)  s(x0) TPRS  <NA>            2     3 -1.48  0.0230
       9 s(x0)  s(x0) TPRS  <NA>            2     4 -1.25  0.0230
      10 s(x0)  s(x0) TPRS  <NA>            2     5 -0.346 0.0230
      # i 490 more rows

# smooth_samples works for a simple GAM multi rng calls

    Code
      sm
    Output
      # A tibble: 500 x 8
         smooth term  type  by_variable   row  draw value     x0
         <chr>  <chr> <chr> <chr>       <int> <int> <dbl>  <dbl>
       1 s(x0)  s(x0) TPRS  <NA>            1     1 -1.36 0.0131
       2 s(x0)  s(x0) TPRS  <NA>            1     2 -1.17 0.0131
       3 s(x0)  s(x0) TPRS  <NA>            1     3 -3.29 0.0131
       4 s(x0)  s(x0) TPRS  <NA>            1     4 -3.10 0.0131
       5 s(x0)  s(x0) TPRS  <NA>            1     5 -2.00 0.0131
       6 s(x0)  s(x0) TPRS  <NA>            2     1 -1.28 0.0230
       7 s(x0)  s(x0) TPRS  <NA>            2     2 -1.18 0.0230
       8 s(x0)  s(x0) TPRS  <NA>            2     3 -3.16 0.0230
       9 s(x0)  s(x0) TPRS  <NA>            2     4 -2.98 0.0230
      10 s(x0)  s(x0) TPRS  <NA>            2     5 -1.91 0.0230
      # i 490 more rows

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
      # i 4,990 more rows

# smooth_samples example output doesn't change

    Code
      samples
    Output
      # A tibble: 1,000 x 8
         smooth term  type  by_variable   row  draw value      x0
         <chr>  <chr> <chr> <chr>       <int> <int> <dbl>   <dbl>
       1 s(x0)  s(x0) TPRS  <NA>            1     1 -1.28 0.00131
       2 s(x0)  s(x0) TPRS  <NA>            1     2 -1.25 0.00131
       3 s(x0)  s(x0) TPRS  <NA>            1     3 -1.02 0.00131
       4 s(x0)  s(x0) TPRS  <NA>            1     4 -1.62 0.00131
       5 s(x0)  s(x0) TPRS  <NA>            1     5 -1.32 0.00131
       6 s(x0)  s(x0) TPRS  <NA>            2     1 -1.24 0.00633
       7 s(x0)  s(x0) TPRS  <NA>            2     2 -1.21 0.00633
       8 s(x0)  s(x0) TPRS  <NA>            2     3 -1.00 0.00633
       9 s(x0)  s(x0) TPRS  <NA>            2     4 -1.58 0.00633
      10 s(x0)  s(x0) TPRS  <NA>            2     5 -1.29 0.00633
      # i 990 more rows

