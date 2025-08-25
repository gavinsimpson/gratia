# partial derivatives works with factor by

    Code
      pd
    Output
      # A tibble: 200 x 11
         .smooth    .focal .by   .fs   .partial_deriv    .se .crit .lower_ci .upper_ci
         <chr>      <chr>  <chr> <chr>          <dbl>  <dbl> <dbl>     <dbl>     <dbl>
       1 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       2 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       3 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       4 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       5 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       6 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       7 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       8 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       9 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
      10 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
      # i 190 more rows
      # i 2 more variables: x1 <dbl>, z1 <fct>

# partial derivatives works with selected factor by

    Code
      pd
    Output
      # A tibble: 100 x 11
         .smooth    .focal .by   .fs   .partial_deriv    .se .crit .lower_ci .upper_ci
         <chr>      <chr>  <chr> <chr>          <dbl>  <dbl> <dbl>     <dbl>     <dbl>
       1 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       2 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       3 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       4 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       5 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       6 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       7 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       8 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
       9 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
      10 te(x1,x2)~ x1     z1    <NA>         -0.0225 0.0254  1.96   -0.0723    0.0274
      # i 90 more rows
      # i 2 more variables: x1 <dbl>, z1 <fct>

# partial derivatives throws error with incorrect focal

    Code
      partial_derivatives(m_partial_deriv, focal = rep("x1", 1L))
    Output
      
    Message
      ! Ignoring univariate smooths & those involving random effects.
    Output
      
    Condition
      Error in `partial_derivatives()`:
      ! `focal` must have same length as number of chosen smooths
      i The relevant smooths are: te(x1,x2):z1A and te(x1,x2):z1B
      i The supplied `focal` should be length: 2
      x Your supplied `focal` was length: 1

