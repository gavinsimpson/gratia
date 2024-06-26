## Test compare_smooths()

# uses
# * su_m_quick_eg1, and
# * su_m_quick_eg1_shrink
# from setup.R

test_that("compare_smooths() works for a pair of GAMs", {
  expect_silent(cs <- compare_smooths(
    su_m_quick_eg1,
    su_m_quick_eg1_shrink
  ))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models
})

test_that("compare_smooths() errors when passed a single model", {
  expect_error(
    compare_smooths(su_m_quick_eg1),
    "Need at least two models to compare smooths"
  )
})

test_that("draw.compare_smooths() can plot a comparison of smooths", {
  expect_silent(cs <- compare_smooths(
    su_m_quick_eg1,
    su_m_quick_eg1_shrink
  ))
  expect_silent(plt1 <- draw(cs))

  expect_silent(plt2 <- draw(cs, nrow = 2, ncol = 3))

  expect_silent(plt3 <- draw(cs) & theme(legend.position = "bottom"))

  skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("compare smooths - all smooths", plt1)
  expect_doppelganger("compare smooths - set nrow ncol", plt2)
  expect_doppelganger("compare smooths - bottom legend", plt3)
})

test_that("compare_smooths() works when a model is not a simple object", {
  expect_silent(cs <- compare_smooths(m_gam, m_gamm$gam))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models

  expect_silent(cs <- compare_smooths(m_gam, m_gamm[["gam"]]))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models
})

test_that("compare_smooths() works when a model is not a simple object", {
  l1 <- list(model1 = m_gam)
  l2 <- list(model2 = m_gamm$gam)

  expect_silent(cs <- compare_smooths(l1[[1]], l2[[1]]))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models

  expect_silent(cs <- compare_smooths(m_gam, l2[["model2"]]))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models

  expect_silent(cs <- compare_smooths(m_gam, l2$model2))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models
})
