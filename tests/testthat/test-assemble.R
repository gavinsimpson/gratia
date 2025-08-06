test_that("assemble.gam works for m_1_smooth", {
  skip_on_cran()
  expect_snapshot(assemble(m_1_smooth))
})

test_that("assemble.gam works for m_gam with angled labels", {
  skip_on_cran()
  expect_snapshot(assemble(m_gam))
})

test_that("assemble.gam works for m_2_fac with angled labels", {
  skip_on_cran()
  expect_snapshot(assemble(m_2_fac, parametric = TRUE, envir = teardown_env(),
    data = df_2_fac))
})

test_that("assemble.gam works for m_para_sm with angled labels", {
  skip_on_cran()
  expect_snapshot(assemble(m_para_sm, parametric = TRUE, envir = teardown_env(),
    data = df_2_fac))
})
