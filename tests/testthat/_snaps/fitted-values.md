# fitted values works for a ziP() model

    Code
      fitted_values(m_ziP, quantity = "expectation")
    Output
      # A tibble: 400 x 10
          .row     x0     x1     x2    x3 .parameter  .fitted    .se .lower_ci
         <int>  <dbl>  <dbl>  <dbl> <dbl> <chr>         <dbl>  <dbl>     <dbl>
       1     1 0.266  0.659  0.859  0.367 expectation  0.521  0.252    0.0262 
       2     2 0.372  0.185  0.0344 0.741 expectation  0.0525 0.0251   0.00330
       3     3 0.573  0.954  0.971  0.934 expectation  0.470  0.380   -0.274  
       4     4 0.908  0.898  0.745  0.673 expectation  0.608  0.255    0.108  
       5     5 0.202  0.944  0.273  0.701 expectation 23.6    1.68    20.3    
       6     6 0.898  0.724  0.677  0.848 expectation  0.381  0.155    0.0767 
       7     7 0.945  0.370  0.348  0.706 expectation  0.344  0.138    0.0734 
       8     8 0.661  0.781  0.947  0.859 expectation  0.0913 0.0551  -0.0167 
       9     9 0.629  0.0111 0.339  0.446 expectation  0.367  0.175    0.0242 
      10    10 0.0618 0.940  0.0317 0.677 expectation  2.47   0.741    1.02   
      # i 390 more rows
      # i 1 more variable: .upper_ci <dbl>

# fitted values works for a mvn() model

    Code
      fitted_values(m_mvn)
    Output
      # A tibble: 600 x 11
          .row      x0    x1    x2     x3 .parameter .fitted .y          .se .lower_ci
         <int>   <dbl> <dbl> <dbl>  <dbl> <chr>        <dbl> <chr>     <dbl>     <dbl>
       1     1 0.114   0.353 0.281 0.642  mean          2.62 response1 0.194     2.24 
       2     2 0.622   0.981 0.174 0.410  mean          8.88 response1 0.241     8.41 
       3     3 0.609   0.539 0.170 0.921  mean          4.75 response1 0.182     4.39 
       4     4 0.623   0.444 0.561 0.451  mean          4.12 response1 0.188     3.76 
       5     5 0.861   0.949 0.429 0.914  mean          7.53 response1 0.220     7.10 
       6     6 0.640   0.452 0.389 0.0264 mean          4.13 response1 0.190     3.76 
       7     7 0.00950 0.191 0.844 0.342  mean          1.48 response1 0.285     0.923
       8     8 0.233   0.992 0.209 0.256  mean          8.57 response1 0.248     8.08 
       9     9 0.666   0.548 0.509 0.777  mean          4.69 response1 0.187     4.32 
      10    10 0.514   0.769 0.395 0.296  mean          6.64 response1 0.177     6.30 
      # i 590 more rows
      # i 1 more variable: .upper_ci <dbl>

# fitted values works for a multinom() model

    Code
      fitted_values(m_multinom)
    Output
      # A tibble: 3,000 x 9
          .row    x1      x2 .parameter  .fitted .category    .se .lower_ci .upper_ci
         <int> <dbl>   <dbl> <chr>         <dbl> <chr>      <dbl>     <dbl>     <dbl>
       1     1 0.721 0.0778  probability   0.191 category1 0.0268    0.144      0.249
       2     2 0.876 0.844   probability   0.119 category1 0.0214    0.0831     0.168
       3     3 0.761 0.00681 probability   0.184 category1 0.0294    0.133      0.249
       4     4 0.886 0.351   probability   0.176 category1 0.0245    0.133      0.230
       5     5 0.456 0.686   probability   0.179 category1 0.0202    0.143      0.222
       6     6 0.166 0.254   probability   0.136 category1 0.0206    0.101      0.182
       7     7 0.325 0.174   probability   0.174 category1 0.0227    0.134      0.223
       8     8 0.509 0.707   probability   0.177 category1 0.0204    0.141      0.221
       9     9 0.728 1.000   probability   0.118 category1 0.0231    0.0798     0.171
      10    10 0.990 0.950   probability   0.153 category1 0.0388    0.0914     0.245
      # i 2,990 more rows

