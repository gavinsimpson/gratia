# test conditional_values() and related functions

# constants
cv_names <- c(".row", ".fitted", ".se", ".lower_ci", ".upper_ci")

# conditional_values()
test_that("conditional_values for a GAM with vector condition", {
  cv <- conditional_values(m_gam, condition = "x2", data = su_eg1)
  expect_s3_class(cv, "conditional_values")
  expect_named(cv, c(cv_names[1], paste0("x", c(2,0,1,3)), cv_names[-1]))

  plt <- cv |> draw()
  skip_on_ci()
  skip_on_cran()
  expect_doppelganger("conditional values m_gam numeric cond", plt)
})

test_that("conditional_values for a GAM with vector factor condition", {
  cv <- conditional_values(su_m_factor_by, condition = "fac", data = su_eg4)
  expect_s3_class(cv, "conditional_values")
  expect_named(cv, c(cv_names[1], "fac", "x2", "x0", cv_names[-1]))
})

test_that("conditional_values for a GAM with 2 element vector condition", {
  cv <- conditional_values(su_m_factor_by, condition = c("x2", "fac"),
    data = su_eg4
  )
  expect_s3_class(cv, "conditional_values")
  expect_named(cv, c(cv_names[1], "x2", "fac", "x0", cv_names[-1]))
})
