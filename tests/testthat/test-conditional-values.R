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

test_that("conditional_values works with vector condition", {
  cv <- conditional_values(
    m_gam,
    condition = c("x2", "x1")
  )
  expect_s3_class(cv, "conditional_values")
  expect_named(cv, c(cv_names[1], "x2", "x1", "x0", "x3", cv_names[-1]))

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(cv))
})

test_that("conditional_values works with complex list condition", {
  cv <- conditional_values(
    m_gam,
    condition = list("x2", x1 = "fivenum", x0 = "quartile", x3 = "threenum")
  )
  expect_s3_class(cv, "conditional_values")
  expect_named(cv, c(cv_names[1], "x2", "x1", "x0", "x3", cv_names[-1]))

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(cv))
})

test_that("conditional_values works with factor by model", {
  cv <- conditional_values(
    su_m_factor_by,
    condition = list("fac", x2 = "fivenum")
  )
  expect_s3_class(cv, "conditional_values")
  expect_named(cv, c(cv_names[1], "fac", "x2", "x0", cv_names[-1]))

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(cv))
})

test_that("conditional_values works with supplied factor levels", {
  cv <- conditional_values(
    su_m_factor_by,
    condition = list("x2", fac = 2:3)
  )
  expect_s3_class(cv, "conditional_values")
  expect_named(cv, c(cv_names[1], "x2", "fac", "x0", cv_names[-1]))

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(cv))
})
