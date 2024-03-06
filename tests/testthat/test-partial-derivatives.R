# uses su_eg2 & su_m_bivar_te

# partial derivatives
test_that("partial derivatives works with single data point", {
  # data slice through te(x,z) holding z == 0.4
  ds <- data_slice(su_m_bivar_te, x = 0.0204313075, z = 0.4)
  expect_silent(pd_x <- partial_derivatives(su_m_bivar_te,
    data = ds,
    type = "central", focal = "x"
  ))
})

# partial derivatives
test_that("partial derivatives works with single data point sim interval", {
  # data slice through te(x,z) holding z == 0.4
  ds <- data_slice(su_m_bivar_te, x = 0.0204313075, z = 0.4)
  expect_silent(pd_x <- partial_derivatives(su_m_bivar_te,
    data = ds,
    type = "central", focal = "x", interval = "simultaneous", seed = 2
  ))
})
