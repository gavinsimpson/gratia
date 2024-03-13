# difference smooths works with a decomposed model issue 223

    Code
      print(d)
    Output
      # A tibble: 7,500 x 10
         .smooth  .by   .level_1 .level_2 .diff   .se .lower_ci .upper_ci Sepal.Length
         <chr>    <chr> <chr>    <chr>    <dbl> <dbl>     <dbl>     <dbl>        <dbl>
       1 ti(Sepa~ Spec~ setosa   versico~ 0.360 0.408    -0.439     1.16           4.3
       2 ti(Sepa~ Spec~ setosa   versico~ 0.344 0.389    -0.419     1.11           4.3
       3 ti(Sepa~ Spec~ setosa   versico~ 0.327 0.370    -0.398     1.05           4.3
       4 ti(Sepa~ Spec~ setosa   versico~ 0.310 0.351    -0.378     0.998          4.3
       5 ti(Sepa~ Spec~ setosa   versico~ 0.294 0.332    -0.358     0.945          4.3
       6 ti(Sepa~ Spec~ setosa   versico~ 0.277 0.313    -0.337     0.891          4.3
       7 ti(Sepa~ Spec~ setosa   versico~ 0.260 0.294    -0.317     0.837          4.3
       8 ti(Sepa~ Spec~ setosa   versico~ 0.243 0.276    -0.297     0.784          4.3
       9 ti(Sepa~ Spec~ setosa   versico~ 0.227 0.257    -0.276     0.730          4.3
      10 ti(Sepa~ Spec~ setosa   versico~ 0.210 0.238    -0.256     0.676          4.3
      # i 7,490 more rows
      # i 1 more variable: Sepal.Width <dbl>

