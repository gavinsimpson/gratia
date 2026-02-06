
test_that("model_constant returns the intercept estimate", {
  expect_silent(b <- model_constant(m_gam))
  expect_type(b, "double")
  ref <- unname(coef(m_gam)[1L])
  attr(ref, "par_names") <- "location"
  expect_identical(b, ref)
  expect_named(b, expected = NULL)
})

test_that("model_constant works for a GAMLSS", {
  expect_length(model_constant(m_accel), 2L)
  expect_length(model_constant(m_gam), 1L)
})

test_that("model vars works for various GAMs", {
  expect_silent(mvars <- model_vars(m_gam))
  expect_identical(mvars, paste0("x", 0:3))
  expect_silent(mvars <- model_vars(m_bam))
  expect_identical(mvars, paste0("x", 0:3))
  expect_silent(mvars <- model_vars(m_gamm))
  expect_identical(mvars, paste0("x", 0:3))
  expect_silent(mvars <- model_vars(m_gamm4))
  expect_identical(mvars, paste0("x", 0:3))
})

test_that("model_terms works", {
  expect_silent(tm <- model_terms(m_gam))
  expect_type(tm, "character")
  expect_identical(tm, c("(Intercept)", "s(x0)", "s(x1)", "s(x2)", "s(x3)"))
  expect_silent(tm <- model_terms(m_gamm))
  expect_type(tm, "character")
  expect_identical(tm, c("(Intercept)", "s(x0)", "s(x1)", "s(x2)", "s(x3)"))
  expect_silent(tm <- model_terms(su_m_factor_by))
  expect_type(tm, "character")
  expect_identical(
    tm,
    c(
      "(Intercept)", "fac2", "fac3", "s(x2):fac1", "s(x2):fac2", "s(x2):fac3",
      "s(x0)"
    )
  )
  expect_silent(tm <- model_terms(m_only_para))
  expect_type(tm, "character")
  expect_identical(
    tm,
    c(
      "(Intercept)", "fac2", "fac3", "ffB", "ffC", "ffD", "x0", "x1", "x2",
      "fac2:ffB", "fac3:ffB", "fac2:ffC", "fac3:ffC", "fac2:ffD", "fac3:ffD"
    )
  )
})
