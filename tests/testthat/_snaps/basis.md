# basis() throws error if n coefs is wrong

    Code
      basis(s(x1), data = su_eg1, coefficients = 1:3)
    Condition
      Error:
      ! Incorrect number of `coefficients`
      x Provided 3 coefficients for 10 basis functions.

# basis() works with supplied coefs

    Code
      withr::with_seed(seed = 2, basis(s(x1), data = su_eg1, coefficients = rnorm(10,
        sd = 4)))
    Output
      # A tibble: 10,000 x 6
         .smooth .type .by   .bf    .value    x1
         <chr>   <chr> <chr> <fct>   <dbl> <dbl>
       1 s(x1)   TPRS  <NA>  1      1.86   0.531
       2 s(x1)   TPRS  <NA>  2     -0.243  0.531
       3 s(x1)   TPRS  <NA>  3      1.13   0.531
       4 s(x1)   TPRS  <NA>  4      0.323  0.531
       5 s(x1)   TPRS  <NA>  5      0.104  0.531
       6 s(x1)   TPRS  <NA>  6     -0.198  0.531
       7 s(x1)   TPRS  <NA>  7     -0.158  0.531
       8 s(x1)   TPRS  <NA>  8     -0.109  0.531
       9 s(x1)   TPRS  <NA>  9      7.94   0.531
      10 s(x1)   TPRS  <NA>  10    -0.0759 0.531
      # i 9,990 more rows

