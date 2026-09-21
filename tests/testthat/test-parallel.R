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

test_that("parallel evaluation forwards local expression environments", {
  d <- transformed_data()
  fun <- function(x) log(x + 1)
  m <- mgcv::gam(y ~ s(fun(x), k = 5) + s(sqrt(z), k = 5), data = d)
  a <- smooth_estimates(m, data = d, envir = environment(), overall_uncertainty = FALSE)
  reference <- d; reference[["fun(x)"]] <- fun(d$x)
  reference[["sqrt(z)"]] <- sqrt(d$z)
  ref <- predict(m, reference, type = "terms", newdata.guaranteed = TRUE)
  for (j in seq_along(m$smooth)) {
    expect_equal(a$.estimate[a$.smooth == m$smooth[[j]]$label], as.numeric(ref[, j]))
  }
  expect_s3_class(basis(m, data = d, envir = environment()), "basis")
})
