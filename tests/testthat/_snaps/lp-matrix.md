# print() output is as expected

    Code
      print(lp_matrix(m_gam))
    Output
      Linear prediction matrix (1000 x 37)
        `(Intercept)` s(x0).~1 s(x0)~2 s(x0)~3 s(x0)~4 s(x0)~5 s(x0)~6 s(x0)~7 s(x0)~8
        <matrix>      <matrix> <matri> <matri> <matri> <matri> <matri> <matri> <matri>
      1 1             -0.9524~ -0.198~ -0.618~  0.207~  0.388~  0.149~  0.592~  0.290~
      2 1             -0.6226~  0.261~ -0.043~  0.299~  0.379~  0.027~  0.334~  0.460~
      3 1              0.3782~  0.530~ -0.087~  0.186~ -0.400~  0.237~ -0.021~  0.446~
      4 1              1.1366~ -0.097~  1.380~ -0.247~ -1.437~ -0.161~ -1.408~ -0.787~
      5 1             -1.0624~ -0.412~ -1.032~ -0.091~  0.781~  0.031~  0.689~  0.100~
      # ... with 995 more rows, 28 more variables: `s(x0).9` <matrix>, ...
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

