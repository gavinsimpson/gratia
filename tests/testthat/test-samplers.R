# Tests for samplers

# post_draws()
test_that("post_draws() works for a GAM", {
  expect_silent(drws1 <- post_draws(m_gam, n = 20, method = "gaussian",
    seed = 2))
  expect_silent(drws2 <- post_draws(m_gam, n = 20, method = "gaussian",
    seed = 2))
  expect_type(drws1, "double")
  expect_true(is.matrix(drws1))
  expect_identical(dim(drws1), c(20L, 37L))
  expect_identical(drws1, drws2)
})

test_that("post_draws() works for a GAM", {
  expect_silent(drws1 <- post_draws(m_gam, n = 20, method = "mh",
    burnin = 100, thin = 2, t_df = 4, rw_scale = 0.3, seed = 2))
  expect_silent(drws2 <- post_draws(m_gam, n = 20, method = "mh",
    burnin = 100, thin = 2, t_df = 4, rw_scale = 0.3, seed = 2))
  expect_type(drws1, "double")
  expect_true(is.matrix(drws1))
  expect_identical(dim(drws1), c(20L, 37L))
  expect_identical(drws1, drws2)
})

test_that("post_draws() fails for INLA", {
  expect_error(post_draws(m_gam, method = "inla"),
    "'method = \"inla\"' is not yet implemented.")
})

test_that("post_draws() works for a GAM with mgcv mvn()", {
  expect_silent(drws1 <- post_draws(m_gam, n = 20, method = "gaussian",
    seed = 2, mvn_method = "mgcv"))
  expect_silent(drws2 <- post_draws(m_gam, n = 20, method = "gaussian",
    seed = 2, mvn_method = "mgcv"))
  expect_type(drws1, "double")
  expect_true(is.matrix(drws1))
  expect_identical(dim(drws1), c(20L, 37L))
  expect_identical(drws1, drws2)
})

# generate_draws()
test_that("generate_draws() works for a GAM", {
  expect_silent(drws1 <- generate_draws(m_gam, n = 20, method = "gaussian",
    seed = 2))
  expect_silent(drws2 <- generate_draws(m_gam, n = 20, method = "gaussian",
    seed = 2))
  expect_type(drws1, "double")
  expect_true(is.matrix(drws1))
  expect_identical(dim(drws1), c(20L, 37L))
  expect_identical(drws1, drws2)
})

test_that("generate_draws() works for a GAM with MH", {
  expect_silent(drws1 <- generate_draws(m_gam, n = 20, method = "mh",
    burnin = 100, thin = 2, t_df = 4, rw_scale = 0.3, seed = 2))
  expect_silent(drws2 <- generate_draws(m_gam, n = 20, method = "mh",
    burnin = 100, thin = 2, t_df = 4, rw_scale = 0.3, seed = 2))
  expect_type(drws1, "double")
  expect_true(is.matrix(drws1))
  expect_identical(dim(drws1), c(20L, 37L))
  expect_identical(drws1, drws2)
})

test_that("generate_draws() fails for INLA", {
  expect_error(generate_draws(m_gam, method = "inla"),
    "'method = \"inla\"' is not yet implemented.")
})

test_that("generate_draws() works for a GAM with mgcv mvn()", {
  expect_silent(drws1 <- generate_draws(m_gam, n = 20, method = "gaussian",
    seed = 2, mvn_method = "mgcv"))
  expect_silent(drws2 <- generate_draws(m_gam, n = 20, method = "gaussian",
    seed = 2, mvn_method = "mgcv"))
  expect_type(drws1, "double")
  expect_true(is.matrix(drws1))
  expect_identical(dim(drws1), c(20L, 37L))
  expect_identical(drws1, drws2)
})

# user_draws()
test_that("user_draws() works for a GAM", {
  expect_silent(drws1 <- generate_draws(m_gam, n = 20, method = "gaussian",
    seed = 2))
  expect_silent(udrws <- user_draws(m_gam, draws = drws1))
  expect_type(udrws, "double")
  expect_true(is.matrix(udrws))
  expect_identical(dim(udrws), c(20L, 37L))
  expect_identical(udrws, drws1)
})

test_that("user_draws() fails for incorrect matrix of draws", {
  expect_silent(drws1 <- generate_draws(m_gam, n = 20, method = "gaussian",
    seed = 2))
  expect_error(user_draws(m_gam, draws = drws1[, 1:10]),
    "Supplied 'draws' doesn't match number of model coefficients.
Number of model coefs: 37
Number of columns in 'draws': 10")
})
