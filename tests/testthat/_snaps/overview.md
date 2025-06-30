# print() output is as expected for GAM

    Code
      print(overview(m_gam))
    Output
      
      Generalized Additive Model with 4 terms
      
        term  type      k   edf statistic p.value
        <chr> <chr> <dbl> <dbl>     <dbl> <chr>  
      1 s(x0) TPRS      9  4.23    16.1   <0.001 
      2 s(x1) TPRS      9  3.50   169.    <0.001 
      3 s(x2) TPRS      9  8.61   201.    <0.001 
      4 s(x3) TPRS      9  1.00     0.599 0.439  

---

    Code
      print(overview(m_gam, stars = TRUE))
    Output
      
      Generalized Additive Model with 4 terms
      
        term  type      k   edf statistic p.value stars    
        <chr> <chr> <dbl> <dbl>     <dbl> <chr>   <noquote>
      1 s(x0) TPRS      9  4.23    16.1   <0.001  ***      
      2 s(x1) TPRS      9  3.50   169.    <0.001  ***      
      3 s(x2) TPRS      9  8.61   201.    <0.001  ***      
      4 s(x3) TPRS      9  1.00     0.599 0.439            
      
      # 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# print() output is as expected for GAMM

    Code
      print(overview(m_gamm))
    Output
      
      Generalized Additive Mixed Model with 4 terms
      
        term  type      k   edf statistic p.value
        <chr> <chr> <dbl> <dbl>     <dbl> <chr>  
      1 s(x0) TPRS      9  4.23    19.4   <0.001 
      2 s(x1) TPRS      9  3.50   209.    <0.001 
      3 s(x2) TPRS      9  8.61   209.    <0.001 
      4 s(x3) TPRS      9  1.00     0.604 0.437  

---

    Code
      print(overview(m_gamm, stars = TRUE))
    Output
      
      Generalized Additive Mixed Model with 4 terms
      
        term  type      k   edf statistic p.value
        <chr> <chr> <dbl> <dbl>     <dbl> <chr>  
      1 s(x0) TPRS      9  4.23    19.4   <0.001 
      2 s(x1) TPRS      9  3.50   209.    <0.001 
      3 s(x2) TPRS      9  8.61   209.    <0.001 
      4 s(x3) TPRS      9  1.00     0.604 0.437  

# print() output is as expected for BAM

    Code
      print(overview(m_bam))
    Output
      
      Big Additive Model with 4 terms
      
        term  type      k   edf statistic p.value
        <chr> <chr> <dbl> <dbl>     <dbl> <chr>  
      1 s(x0) TPRS      9  4.23    16.1   <0.001 
      2 s(x1) TPRS      9  3.50   169.    <0.001 
      3 s(x2) TPRS      9  8.61   201.    <0.001 
      4 s(x3) TPRS      9  1.00     0.604 0.437  

---

    Code
      print(overview(m_bam, stars = TRUE))
    Output
      
      Big Additive Model with 4 terms
      
        term  type      k   edf statistic p.value stars    
        <chr> <chr> <dbl> <dbl>     <dbl> <chr>   <noquote>
      1 s(x0) TPRS      9  4.23    16.1   <0.001  ***      
      2 s(x1) TPRS      9  3.50   169.    <0.001  ***      
      3 s(x2) TPRS      9  8.61   201.    <0.001  ***      
      4 s(x3) TPRS      9  1.00     0.604 0.437            
      
      # 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

