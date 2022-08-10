# print() output is as expected for GAM

    Code
      print(overview(m_gam))
    Output
      
      Generalized Additive Model with 4 terms
      
        term  type    edf statistic p.value
        <chr> <chr> <dbl>     <dbl> <chr>  
      1 s(x0) TPRS   4.23    16.1   < 0.001
      2 s(x1) TPRS   3.50   169.    < 0.001
      3 s(x2) TPRS   8.61   201.    < 0.001
      4 s(x3) TPRS   1.00     0.599 0.43875

# print() output is as expected for GAMM

    Code
      print(overview(m_gamm))
    Output
      
      Generalized Additive Mixed Model with 4 terms
      
        term  type    edf statistic p.value
        <chr> <chr> <dbl>     <dbl> <chr>  
      1 s(x0) TPRS   4.23    19.4   <0.001 
      2 s(x1) TPRS   3.50   209.    <0.001 
      3 s(x2) TPRS   8.61   209.    <0.001 
      4 s(x3) TPRS   1.00     0.604 0.4373 

# print() output is as expected for BAM

    Code
      print(overview(m_bam))
    Output
      
      Big Additive Model with 4 terms
      
        term  type    edf statistic p.value
        <chr> <chr> <dbl>     <dbl> <chr>  
      1 s(x0) TPRS   4.23    16.1   < 0.001
      2 s(x1) TPRS   3.50   169.    < 0.001
      3 s(x2) TPRS   8.61   201.    < 0.001
      4 s(x3) TPRS   1.00     0.604 0.43732

