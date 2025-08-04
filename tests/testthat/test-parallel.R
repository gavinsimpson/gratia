skip_if_not_installed("mirai")
skip_if_not_installed("carrier")

# set up daemons
# ensures only 1 additional process on CRAN
mirai::daemons(1, dispatcher = FALSE)
on.exit(mirai::daemons(0), add = TRUE)

test_that("parallel smooth estimates works", {
  sm <- smooth_estimates(m_gam)
  expect_s3_class(sm, "smooth_estimates")
})

test_that("parallel smooth estimates works", {
  bf <- basis(m_gam)
  expect_s3_class(bf, "basis")
})
