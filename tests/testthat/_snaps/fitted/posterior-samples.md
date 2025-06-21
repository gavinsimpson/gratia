# posterior sampling funs work with offsets in formula issue 233

    Code
      print(fs)
    Output
      # A tibble: 500 x 4
          .row .draw .parameter .fitted
         <int> <int> <chr>        <dbl>
       1     1     1 location      2.15
       2     2     1 location      2.15
       3     3     1 location      2.15
       4     4     1 location      2.15
       5     5     1 location      2.15
       6     6     1 location      2.15
       7     7     1 location      2.15
       8     8     1 location      2.15
       9     9     1 location      2.15
      10    10     1 location      2.15
      # i 490 more rows

# posterior sampling works for a mvn() model

    Code
      print(fs)
    Output
      # A tibble: 3,000 x 4
          .row .draw .parameter .fitted
         <int> <int> <chr>        <dbl>
       1     1     1 response1     2.56
       2     2     1 response1     8.98
       3     3     1 response1     4.59
       4     4     1 response1     3.92
       5     5     1 response1     7.40
       6     6     1 response1     3.92
       7     7     1 response1     1.65
       8     8     1 response1     8.78
       9     9     1 response1     4.49
      10    10     1 response1     6.66
      # i 2,990 more rows

# posterior sampling works for a multinom() model

    Code
      print(fs)
    Output
      # A tibble: 15,000 x 4
          .row .draw .parameter .fitted
         <int> <int> <chr>        <dbl>
       1     1     1 response1    0.239
       2     2     1 response1    0.129
       3     3     1 response1    0.244
       4     4     1 response1    0.227
       5     5     1 response1    0.169
       6     6     1 response1    0.141
       7     7     1 response1    0.207
       8     8     1 response1    0.165
       9     9     1 response1    0.102
      10    10     1 response1    0.116
      # i 14,990 more rows

