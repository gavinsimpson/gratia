## Test basis() and related functions

data(ref_sims, package = "gratia")

new_sims <- create_reference_simulations()

test_that("test data_sim() produces tibbles", {
  for (i in names(ref_sims)) {
    expect_s3_class(new_sims[[!!(i)]],
      class = c("tbl_df", "tbl", "data.frame")
    )
  }
})

test_that("test data_sim() reproduces reference output", {
  skip_on_ci()
  skip_on_cran()
  for (i in names(ref_sims)) {
    expect_equal(new_sims[[!!(i)]], ref_sims[[!!(i)]])
  }
})

test_that("data sim for gfam works", {
  expect_silent(sim <- data_sim("gfam", n = 400, seed = 2))
  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(sim))
})
