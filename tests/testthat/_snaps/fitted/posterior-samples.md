# posterior sampling funs work with offsets in formula issue 233

    Code
      print(fs)
    Output
      # A tibble: 500 x 4
          .row .draw .parameter .fitted
         <int> <int> <chr>        <dbl>
       1     1     1 mean          2.15
       2     2     1 mean          2.15
       3     3     1 mean          2.15
       4     4     1 mean          2.15
       5     5     1 mean          2.15
       6     6     1 mean          2.15
       7     7     1 mean          2.15
       8     8     1 mean          2.15
       9     9     1 mean          2.15
      10    10     1 mean          2.15
      # i 490 more rows

# posterior sampling works for a mvn() model

    Code
      print(fs)
    Output
      # A tibble: 3,000 x 5
          .row .draw .parameter .fitted .y       
         <int> <int> <chr>        <dbl> <chr>    
       1     1     1 mean          2.56 response1
       2     2     1 mean          8.98 response1
       3     3     1 mean          4.59 response1
       4     4     1 mean          3.92 response1
       5     5     1 mean          7.40 response1
       6     6     1 mean          3.92 response1
       7     7     1 mean          1.65 response1
       8     8     1 mean          8.78 response1
       9     9     1 mean          4.49 response1
      10    10     1 mean          6.66 response1
      # i 2,990 more rows

# posterior sampling works for a multinom() model

    Code
      print(fs)
    Output
      # A tibble: 15,000 x 5
          .row .draw .parameter  .fitted .category
         <int> <int> <chr>         <dbl> <chr>    
       1     1     1 probability   0.239 category1
       2     2     1 probability   0.129 category1
       3     3     1 probability   0.244 category1
       4     4     1 probability   0.227 category1
       5     5     1 probability   0.169 category1
       6     6     1 probability   0.141 category1
       7     7     1 probability   0.207 category1
       8     8     1 probability   0.165 category1
       9     9     1 probability   0.102 category1
      10    10     1 probability   0.116 category1
      # i 14,990 more rows

