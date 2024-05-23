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

