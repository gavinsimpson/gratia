# first order response derivatives works

    Code
      print(yd)
    Output
      # A tibble: 50 x 9
          .row .focal .derivative .lower_ci .upper_ci    x0    x1      x2    x3
         <int> <chr>        <dbl>     <dbl>     <dbl> <dbl> <dbl>   <dbl> <dbl>
       1     1 x2            19.8      16.3      23.2 0.480 0.478 0.00127 0.537
       2     2 x2            24.9      19.3      30.2 0.480 0.478 0.0216  0.537
       3     3 x2            31.6      23.7      39.3 0.480 0.478 0.0420  0.537
       4     4 x2            40.5      30.9      50.7 0.480 0.478 0.0624  0.537
       5     5 x2            51.7      40.9      64.0 0.480 0.478 0.0828  0.537
       6     6 x2            64.9      53.6      78.0 0.480 0.478 0.103   0.537
       7     7 x2            78.5      66.6      92.1 0.480 0.478 0.123   0.537
       8     8 x2            88.9      75.2     104.  0.480 0.478 0.144   0.537
       9     9 x2            91.2      74.3     110.  0.480 0.478 0.164   0.537
      10    10 x2            80.9      62.1     102.  0.480 0.478 0.185   0.537
      # i 40 more rows

# second order response derivatives works

    Code
      print(yd)
    Output
      # A tibble: 50 x 9
          .row .focal .derivative .lower_ci .upper_ci    x0    x1      x2    x3
         <int> <chr>        <dbl>     <dbl>     <dbl> <dbl> <dbl>   <dbl> <dbl>
       1     1 x2            209.     105.       309. 0.480 0.478 0.00127 0.537
       2     2 x2            291.     186.       415. 0.480 0.478 0.0216  0.537
       3     3 x2            379.     270.       528. 0.480 0.478 0.0420  0.537
       4     4 x2            505.     406.       642. 0.480 0.478 0.0624  0.537
       5     5 x2            616.     487.       753. 0.480 0.478 0.0828  0.537
       6     6 x2            690.     479.       899. 0.480 0.478 0.103   0.537
       7     7 x2            625.     275.       963. 0.480 0.478 0.123   0.537
       8     8 x2            345.     -61.5      744. 0.480 0.478 0.144   0.537
       9     9 x2           -165.    -577.       228. 0.480 0.478 0.164   0.537
      10    10 x2           -857.   -1296.      -465. 0.480 0.478 0.185   0.537
      # i 40 more rows

