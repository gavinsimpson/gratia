# rtw works with scalar p and phi

    Code
      withr::with_seed(seed = 34657, rtw(mu = runif(10, min = 0, max = 10), p = 1.5,
      phi = 1.1))
    Output
       [1] 23.5188629  1.5611091  0.2695351  1.2625794  2.4824889  5.4061374
       [7]  5.1371450  0.6377823  2.8042417  5.9204689

# rtw works with vector p and phi

    Code
      withr::with_seed(seed = 34657, rtw(mu = runif(10, min = 0, max = 10), p = runif(
        10, min = 1, max = 2), phi = runif(10, 1, 2)))
    Output
       [1]  1.6760667 26.0809763  3.0301311  0.7551897  7.9348421  4.8814385
       [7] 13.6470470  3.2858650  5.6785218 73.8935738

# rtw fails with negative mu

    Code
      rtw(mu = -2, p = 1.1, phi = 1.1)
    Condition
      Error in `rtw()`:
      ! mean 'mu' must be non-negative

# rtw fails with p outside below range

    Code
      rtw(mu = 2, p = 1L, phi = 1.1)
    Condition
      Error in `rtw()`:
      ! 'p' must be in interval (1, 2)

# rtw fails with p outside above range

    Code
      rtw(mu = 2, p = 3L, phi = 1.1)
    Condition
      Error in `rtw()`:
      ! 'p' must be in interval (1, 2)

# rtw fails with phi negative

    Code
      rtw(mu = 2, p = 1.5, phi = -1)
    Condition
      Error in `rtw()`:
      ! scale parameter 'phi' must be positive

