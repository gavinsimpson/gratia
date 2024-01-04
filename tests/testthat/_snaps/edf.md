# edf() works for a simple GAM

    Code
      print(edfs)
    Output
      # A tibble: 4 x 2
        .smooth  .edf
        <chr>   <dbl>
      1 s(x0)    4.23
      2 s(x1)    3.50
      3 s(x2)    8.61
      4 s(x3)    1.00

# edf() works when selecting smooths

    Code
      print(edfs)
    Output
      # A tibble: 2 x 2
        .smooth  .edf
        <chr>   <dbl>
      1 s(x0)    4.23
      2 s(x2)    8.61

# edf() works with type unconditional

    Code
      print(edfs)
    Output
      # A tibble: 4 x 2
        .smooth  .edf
        <chr>   <dbl>
      1 s(x0)    4.69
      2 s(x1)    3.79
      3 s(x2)    8.70
      4 s(x3)    1.03

# edf() works with type alternative

    Code
      print(edfs)
    Output
      # A tibble: 4 x 2
        .smooth  .edf
        <chr>   <dbl>
      1 s(x0)    5.20
      2 s(x1)    4.34
      3 s(x2)    8.96
      4 s(x3)    1.00

# model_edf() works as expected for a single model

    Code
      print(medf)
    Output
      # A tibble: 1 x 2
        .model  .edf
        <chr>  <dbl>
      1 m_gam   18.3

---

    Code
      print(medf)
    Output
      # A tibble: 2 x 2
        .model      .edf
        <chr>      <dbl>
      1 m_gam      18.3 
      2 m_1_smooth  3.62

# model_edf() works for a single model type default

    Code
      print(medf)
    Output
      # A tibble: 1 x 2
        .model  .edf
        <chr>  <dbl>
      1 m_gam   18.3

# model_edf() works for a single model type alternative

    Code
      print(medf)
    Output
      # A tibble: 1 x 2
        .model  .edf
        <chr>  <dbl>
      1 m_gam   20.5

# model_edf() works for a single model, type unconditional

    Code
      print(medf)
    Output
      # A tibble: 1 x 2
        .model  .edf
        <chr>  <dbl>
      1 m_gam   19.2

