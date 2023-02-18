# print() output is as expected

    Code
      print(lp_matrix(m_gam))
    Output
      Linear prediction matrix (1000 x 37)
        `(Intercept)` s(x0).~1 s(x0)~2 s(x0)~3 s(x0)~4 s(x0)~5 s(x0)~6 s(x0)~7 s(x0)~8
                <dbl>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
      1             1   -0.952 -0.198  -0.619   0.208    0.389  0.149   0.592    0.291
      2             1   -0.623  0.261  -0.0440  0.300    0.380  0.0275  0.335    0.461
      3             1    0.378  0.530  -0.0877  0.186   -0.401  0.237  -0.0210   0.446
      4             1    1.14  -0.0973  1.38   -0.248   -1.44  -0.161  -1.41    -0.788
      5             1   -1.06  -0.412  -1.03   -0.0915   0.781  0.0316  0.689    0.101
      # ... with 995 more rows, 28 more variables: `s(x0).9` <dbl>, ...

