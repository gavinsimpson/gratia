# posterior sampling funs work with offsets in formula issue 233

    Code
      print(ps)
    Output
      # A tibble: 500 x 3
          .row .draw .response
         <int> <int>     <dbl>
       1     1     1         1
       2     2     1         0
       3     3     1         6
       4     4     1        12
       5     5     1         7
       6     6     1         0
       7     7     1         0
       8     8     1         0
       9     9     1         1
      10    10     1         4
      # i 490 more rows

# posterior sampling works for a mvn() model

    Code
      print(ps)
    Output
      # A tibble: 3,000 x 4
          .row .draw .parameter .response
         <int> <int> <chr>          <dbl>
       1     1     1 response1       1.67
       2     1     1 response2       7.61
       3     2     1 response1       9.16
       4     2     1 response2       7.17
       5     3     1 response1       5.61
       6     3     1 response2       8.56
       7     4     1 response1       4.61
       8     4     1 response2       2.65
       9     5     1 response1       7.46
      10     5     1 response2       3.31
      # i 2,990 more rows

---

    Code
      print(ys)
    Output
      # A tibble: 3,000 x 4
          .row .draw .parameter .response
         <int> <int> <chr>          <dbl>
       1     1     1 response1      4.58 
       2     1     2 response1      2.27 
       3     1     3 response1      1.56 
       4     1     4 response1      0.883
       5     1     5 response1      4.52 
       6     2     1 response1      8.08 
       7     2     2 response1      9.48 
       8     2     3 response1      8.93 
       9     2     4 response1      8.23 
      10     2     5 response1      8.02 
      # i 2,990 more rows

# posterior sampling works for a multinom() model

    Code
      print(ps)
    Output
      # A tibble: 5,000 x 3
          .row .draw .response
         <int> <int>     <dbl>
       1     1     1         1
       2     2     1         2
       3     3     1         2
       4     4     1         2
       5     5     1         2
       6     6     1         0
       7     7     1         2
       8     8     1         2
       9     9     1         1
      10    10     1         2
      # i 4,990 more rows

---

    Code
      print(ys)
    Output
      # A tibble: 5,000 x 3
          .row .draw .response
         <int> <int>     <dbl>
       1     1     1         2
       2     2     1         2
       3     3     1         1
       4     4     1         2
       5     5     1         2
       6     6     1         2
       7     7     1         2
       8     8     1         0
       9     9     1         2
      10    10     1         2
      # i 4,990 more rows

