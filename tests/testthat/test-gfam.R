# testing gratia's ability to handle gfam models - not much yet
test_that("gfam data sim and model works", {
  expect_silent(
    gfam_df <- data_sim("gfam", n = 400, seed = 2,
      gfam_families = c("binary", "tweedie", "normal"))
  )
  expect_silent(
    m_gfam <- gam(cbind(y, index) ~ s(x0) + s(x1) + s(x2) + s(x3),
      family = gfam(list(binomial, tw, gaussian)),
      data = gfam_df, method = "REML")
  )
})
