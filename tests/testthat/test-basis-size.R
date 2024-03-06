# Test overview() & methods

test_that("basis_size works for a GAM", {
  expect_silent(bs <- basis_size(m_gam))
  expect_type(bs, "double")
  expect_identical(length(bs), 4L)
  expect_named(bs, smooths(m_gam))
})
