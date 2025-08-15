# quantile residuals works for a GAM and pit

    Code
      head(quantile_residuals(m_gam, type = "pit", seed = 1), n = 10)
    Output
       [1] 0.17316586 0.04283083 0.94379902 0.71976658 0.46129449 0.64269592
       [7] 0.53693208 0.06973594 0.02772852 0.42894804

# quantile residuals works for a poisson GAM and quantile

    Code
      head(quantile_residuals(b_pois, type = "quantile", seed = 1), n = 10)
    Output
       [1]  1.9057123 -0.8627816  0.6520413  0.9163218  0.6258978  0.9575311
       [7]  0.3101725  1.0874592 -0.3150668 -1.2923274

# quantile residuals fails correctly for a nb GAM

    Code
      head(quantile_residuals(m_nb, type = "quantile", seed = 1), n = 10)
    Condition
      Error in `h()`:
      ! error in evaluating the argument 'x' in selecting a method for function 'head': Quantile residuals are not available for this family.

