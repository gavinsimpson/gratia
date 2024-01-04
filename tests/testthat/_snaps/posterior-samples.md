# smooth_samples works for a continuous by GAM

    Code
      sm
    Output
      # A tibble: 500 x 9
         .smooth .term    .type .by    .row .draw   .value       x2    x1
         <chr>   <chr>    <chr> <chr> <int> <int>    <dbl>    <dbl> <int>
       1 s(x2)   s(x2):x1 TPRS  x1        1     1  1.23    0.000206     1
       2 s(x2)   s(x2):x1 TPRS  x1        1     2  0.543   0.000206     1
       3 s(x2)   s(x2):x1 TPRS  x1        1     3 -1.19    0.000206     1
       4 s(x2)   s(x2):x1 TPRS  x1        1     4 -1.21    0.000206     1
       5 s(x2)   s(x2):x1 TPRS  x1        1     5 -0.519   0.000206     1
       6 s(x2)   s(x2):x1 TPRS  x1        2     1  1.49    0.0103       1
       7 s(x2)   s(x2):x1 TPRS  x1        2     2  0.653   0.0103       1
       8 s(x2)   s(x2):x1 TPRS  x1        2     3 -0.661   0.0103       1
       9 s(x2)   s(x2):x1 TPRS  x1        2     4 -0.716   0.0103       1
      10 s(x2)   s(x2):x1 TPRS  x1        2     5 -0.00637 0.0103       1
      # i 490 more rows

# smooth_samples works for a simple GAM

    Code
      sm
    Output
      # A tibble: 500 x 8
         .smooth .term .type .by    .row .draw .value      x0
         <chr>   <chr> <chr> <chr> <int> <int>  <dbl>   <dbl>
       1 s(x0)   s(x0) TPRS  <NA>      1     1  -1.62 0.00162
       2 s(x0)   s(x0) TPRS  <NA>      1     2  -3.62 0.00162
       3 s(x0)   s(x0) TPRS  <NA>      1     3  -1.67 0.00162
       4 s(x0)   s(x0) TPRS  <NA>      1     4  -2.51 0.00162
       5 s(x0)   s(x0) TPRS  <NA>      1     5  -1.56 0.00162
       6 s(x0)   s(x0) TPRS  <NA>      2     1  -1.51 0.0117 
       7 s(x0)   s(x0) TPRS  <NA>      2     2  -3.42 0.0117 
       8 s(x0)   s(x0) TPRS  <NA>      2     3  -1.64 0.0117 
       9 s(x0)   s(x0) TPRS  <NA>      2     4  -2.40 0.0117 
      10 s(x0)   s(x0) TPRS  <NA>      2     5  -1.49 0.0117 
      # i 490 more rows

# smooth_samples works for a simple GAM multi rng calls

    Code
      sm
    Output
      # A tibble: 500 x 8
         .smooth .term .type .by    .row .draw .value      x0
         <chr>   <chr> <chr> <chr> <int> <int>  <dbl>   <dbl>
       1 s(x0)   s(x0) TPRS  <NA>      1     1  -1.60 0.00162
       2 s(x0)   s(x0) TPRS  <NA>      1     2  -2.33 0.00162
       3 s(x0)   s(x0) TPRS  <NA>      1     3  -1.58 0.00162
       4 s(x0)   s(x0) TPRS  <NA>      1     4  -2.24 0.00162
       5 s(x0)   s(x0) TPRS  <NA>      1     5  -2.19 0.00162
       6 s(x0)   s(x0) TPRS  <NA>      2     1  -1.48 0.0117 
       7 s(x0)   s(x0) TPRS  <NA>      2     2  -2.22 0.0117 
       8 s(x0)   s(x0) TPRS  <NA>      2     3  -1.53 0.0117 
       9 s(x0)   s(x0) TPRS  <NA>      2     4  -2.19 0.0117 
      10 s(x0)   s(x0) TPRS  <NA>      2     5  -2.14 0.0117 
      # i 490 more rows

# fitted_samples example output doesn't change

    Code
      fs
    Output
      # A tibble: 5,000 x 3
          .row .draw .fitted
         <int> <int>   <dbl>
       1     1     1    4.67
       2     2     1    4.82
       3     3     1    4.55
       4     4     1   10.9 
       5     5     1   11.0 
       6     6     1    4.46
       7     7     1    6.20
       8     8     1    6.25
       9     9     1    9.81
      10    10     1    6.36
      # i 4,990 more rows

# smooth_samples example output doesn't change

    Code
      samples
    Output
      # A tibble: 1,000 x 8
         .smooth .term .type .by    .row .draw .value      x0
         <chr>   <chr> <chr> <chr> <int> <int>  <dbl>   <dbl>
       1 s(x0)   s(x0) TPRS  <NA>      1     1  -1.74 0.00131
       2 s(x0)   s(x0) TPRS  <NA>      1     2  -1.31 0.00131
       3 s(x0)   s(x0) TPRS  <NA>      1     3  -1.28 0.00131
       4 s(x0)   s(x0) TPRS  <NA>      1     4  -1.17 0.00131
       5 s(x0)   s(x0) TPRS  <NA>      1     5  -1.49 0.00131
       6 s(x0)   s(x0) TPRS  <NA>      2     1  -1.70 0.00633
       7 s(x0)   s(x0) TPRS  <NA>      2     2  -1.28 0.00633
       8 s(x0)   s(x0) TPRS  <NA>      2     3  -1.25 0.00633
       9 s(x0)   s(x0) TPRS  <NA>      2     4  -1.15 0.00633
      10 s(x0)   s(x0) TPRS  <NA>      2     5  -1.44 0.00633
      # i 990 more rows

