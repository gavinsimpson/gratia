# variance_comp works for a gam

    Code
      print(vc)
    Output
      # A tibble: 5 x 5
        .component   .variance .std_dev .lower_ci .upper_ci
        <chr>            <dbl>    <dbl>     <dbl>     <dbl>
      1 s(x0)         99.4       9.97    3.85e+ 0   2.58e 1
      2 s(x1)         44.1       6.64    2.85e+ 0   1.55e 1
      3 s(x2)      11014.      105.      6.27e+ 1   1.76e 2
      4 s(x3)          0.00412   0.0642  8.93e-16   4.61e12
      5 scale          4.30      2.07    1.98e+ 0   2.17e 0

# variance_comp works for a gam with rescaling

    Code
      print(vc)
    Output
      # A tibble: 5 x 5
        .component   .variance .std_dev .lower_ci .upper_ci
        <chr>            <dbl>    <dbl>     <dbl>     <dbl>
      1 s(x0)         99.4       9.97    3.85e+ 0   2.58e 1
      2 s(x1)         44.1       6.64    2.85e+ 0   1.55e 1
      3 s(x2)      11014.      105.      6.27e+ 1   1.76e 2
      4 s(x3)          0.00412   0.0642  8.93e-16   4.61e12
      5 scale          4.30      2.07    1.98e+ 0   2.17e 0

# variance_comp works for a single term gam

    Code
      print(vc)
    Output
      # A tibble: 2 x 5
        .component .variance .std_dev .lower_ci .upper_ci
        <chr>          <dbl>    <dbl>     <dbl>     <dbl>
      1 s(x0)          138.     11.7       3.92     35.2 
      2 scale           13.4     3.66      3.38      3.97

# variance_comp works for a continuous by gam

    Code
      print(vc)
    Output
      # A tibble: 2 x 5
        .component .variance .std_dev .lower_ci .upper_ci
        <chr>          <dbl>    <dbl>     <dbl>     <dbl>
      1 s(x2):x1     6575.      81.1      41.6     158.  
      2 scale           4.08     2.02      1.88      2.17

# variance_comp works for a factor by gam

    Code
      print(vc)
    Output
      # A tibble: 5 x 5
        .component  .variance .std_dev .lower_ci .upper_ci
        <chr>           <dbl>    <dbl>     <dbl>     <dbl>
      1 s(x2):fac1  125.       11.2     3.93e+ 0   3.19e 1
      2 s(x2):fac2  181.       13.4     4.18e+ 0   4.32e 1
      3 s(x2):fac3 5507.       74.2     4.14e+ 1   1.33e 2
      4 s(x0)         0.00514   0.0717  1.05e-29   4.88e26
      5 scale         4.17      2.04    1.90e+ 0   2.19e 0

