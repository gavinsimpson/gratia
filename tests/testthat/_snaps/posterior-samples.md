# smooth_samples works for a continuous by GAM

    Code
      sm
    Output
      # A tibble: 500 x 9
         .smooth .term    .type .by    .row .draw  .value       x2    x1
         <chr>   <chr>    <chr> <chr> <int> <int>   <dbl>    <dbl> <int>
       1 s(x2)   s(x2):x1 TPRS  x1        1     1  0.326  0.000206     1
       2 s(x2)   s(x2):x1 TPRS  x1        1     2 -0.955  0.000206     1
       3 s(x2)   s(x2):x1 TPRS  x1        1     3  0.151  0.000206     1
       4 s(x2)   s(x2):x1 TPRS  x1        1     4 -0.307  0.000206     1
       5 s(x2)   s(x2):x1 TPRS  x1        1     5  0.0128 0.000206     1
       6 s(x2)   s(x2):x1 TPRS  x1        2     1  0.494  0.0103       1
       7 s(x2)   s(x2):x1 TPRS  x1        2     2 -0.441  0.0103       1
       8 s(x2)   s(x2):x1 TPRS  x1        2     3  0.471  0.0103       1
       9 s(x2)   s(x2):x1 TPRS  x1        2     4  0.0437 0.0103       1
      10 s(x2)   s(x2):x1 TPRS  x1        2     5  0.298  0.0103       1
      # i 490 more rows

# smooth_samples works for a simple GAM

    Code
      sm
    Output
      # A tibble: 500 x 8
         .smooth .term .type .by    .row .draw .value      x0
         <chr>   <chr> <chr> <chr> <int> <int>  <dbl>   <dbl>
       1 s(x0)   s(x0) TPRS  <NA>      1     1 -0.754 0.00162
       2 s(x0)   s(x0) TPRS  <NA>      1     2 -2.20  0.00162
       3 s(x0)   s(x0) TPRS  <NA>      1     3 -1.78  0.00162
       4 s(x0)   s(x0) TPRS  <NA>      1     4 -1.79  0.00162
       5 s(x0)   s(x0) TPRS  <NA>      1     5 -1.89  0.00162
       6 s(x0)   s(x0) TPRS  <NA>      2     1 -0.753 0.0117 
       7 s(x0)   s(x0) TPRS  <NA>      2     2 -2.11  0.0117 
       8 s(x0)   s(x0) TPRS  <NA>      2     3 -1.72  0.0117 
       9 s(x0)   s(x0) TPRS  <NA>      2     4 -1.69  0.0117 
      10 s(x0)   s(x0) TPRS  <NA>      2     5 -1.76  0.0117 
      # i 490 more rows

# smooth_samples works for a simple GAM multi rng calls

    Code
      sm
    Output
      # A tibble: 500 x 8
         .smooth .term .type .by    .row .draw .value      x0
         <chr>   <chr> <chr> <chr> <int> <int>  <dbl>   <dbl>
       1 s(x0)   s(x0) TPRS  <NA>      1     1  -1.85 0.00162
       2 s(x0)   s(x0) TPRS  <NA>      1     2  -2.36 0.00162
       3 s(x0)   s(x0) TPRS  <NA>      1     3  -2.14 0.00162
       4 s(x0)   s(x0) TPRS  <NA>      1     4  -2.28 0.00162
       5 s(x0)   s(x0) TPRS  <NA>      1     5  -1.93 0.00162
       6 s(x0)   s(x0) TPRS  <NA>      2     1  -1.73 0.0117 
       7 s(x0)   s(x0) TPRS  <NA>      2     2  -2.22 0.0117 
       8 s(x0)   s(x0) TPRS  <NA>      2     3  -2.08 0.0117 
       9 s(x0)   s(x0) TPRS  <NA>      2     4  -2.19 0.0117 
      10 s(x0)   s(x0) TPRS  <NA>      2     5  -1.82 0.0117 
      # i 490 more rows

# fitted_samples example output doesn't change

    Code
      fs
    Output
      # A tibble: 5,000 x 4
          .row .draw .parameter .fitted
         <int> <int> <chr>        <dbl>
       1     1     1 location      4.94
       2     2     1 location      5.68
       3     3     1 location      4.19
       4     4     1 location     11.0 
       5     5     1 location     11.2 
       6     6     1 location      4.48
       7     7     1 location      5.84
       8     8     1 location      6.72
       9     9     1 location      9.68
      10    10     1 location      7.29
      # i 4,990 more rows

# smooth_samples example output doesn't change

    Code
      samples
    Output
      # A tibble: 1,000 x 8
         .smooth .term .type .by    .row .draw .value      x0
         <chr>   <chr> <chr> <chr> <int> <int>  <dbl>   <dbl>
       1 s(x0)   s(x0) TPRS  <NA>      1     1  -1.46 0.00131
       2 s(x0)   s(x0) TPRS  <NA>      1     2  -1.34 0.00131
       3 s(x0)   s(x0) TPRS  <NA>      1     3  -1.16 0.00131
       4 s(x0)   s(x0) TPRS  <NA>      1     4  -1.37 0.00131
       5 s(x0)   s(x0) TPRS  <NA>      1     5  -1.15 0.00131
       6 s(x0)   s(x0) TPRS  <NA>      2     1  -1.41 0.00633
       7 s(x0)   s(x0) TPRS  <NA>      2     2  -1.30 0.00633
       8 s(x0)   s(x0) TPRS  <NA>      2     3  -1.13 0.00633
       9 s(x0)   s(x0) TPRS  <NA>      2     4  -1.34 0.00633
      10 s(x0)   s(x0) TPRS  <NA>      2     5  -1.13 0.00633
      # i 990 more rows

