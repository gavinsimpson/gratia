# cdf fun works for gaussian cdf

    Code
      cdf_gaussian(q = c(-1, 3, 2.3), mu = c(0, 2, 1.5), wt = rep(1, 3), scale = c(1,
        2, 1))
    Output
      [1] 0.1586553 0.7602499 0.7881446

# cdf fun works for poisson cdf

    Code
      cdf_poisson(q = c(0, 1, 3), mu = c(0.1, 1.3, 3.5), wt = rep(1, 3), scale = c(1,
        1, 1))
    Output
      [1] 0.9048374 0.6268231 0.5366327

# cdf fun works for binomial cdf

    Code
      cdf_binomial(q = c(0.01, 0.7, 0.8), mu = c(0.1, 0.5, 0.7), wt = rep(1, 3),
      scale = c(1, 1, 1))
    Output
      [1] 0.9 0.5 0.3

# cdf fun works for gamma cdf

    Code
      cdf_gamma(q = c(1.1, 3.2, 2.3), mu = c(2, 2, 1.5), wt = rep(1, 3), scale = c(1,
        2, 1))
    Output
      [1] 0.4230502 0.7940968 0.7841849

