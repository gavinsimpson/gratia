## Test simulate() methods

data(smallAges)
smallAges$Error[1] <- 1.1
sw <- scam(Date ~ s(Depth, k = 5, bs = "mpd"),
  data = smallAges,
  weights = 1 / smallAges$Error, gamma = 1.4
)

test_that("simulate() works with a gam", {
  sims <- simulate(m_gam, nsim = 5, seed = 42)
  expect_identical(nrow(sims), 1000L)
  expect_identical(ncol(sims), 5L)

  expect_message(
    sims <- simulate(m_gam,
      nsim = 5, seed = 42,
      newdata = su_eg1
    ),
    "Use of the `newdata` argument is deprecated.
Instead, use the data argument `data`.\n"
  )
  expect_identical(nrow(sims), 1000L)
  expect_identical(ncol(sims), 5L)
})

test_that("simulate() works with a gamm", {
  sims <- simulate(m_gamm, nsim = 5, seed = 42)
  expect_identical(nrow(sims), 1000L)
  expect_identical(ncol(sims), 5L)
})

## monotonic spline age-depth model using scam() from pkg scam
test_that("simulate() works with a scam", {
  sims <- simulate(sw, nsim = 5, seed = 42)
  expect_identical(nrow(sims), 12L)
  expect_identical(ncol(sims), 5L)

  sims <- simulate(sw, nsim = 5, seed = 42, data = smallAges)
  expect_identical(nrow(sims), 12L)
  expect_identical(ncol(sims), 5L)
})

test_that("simulate() works with no .Random.seed", {
  rm(".Random.seed", envir = globalenv())
  sims <- simulate(m_gam, nsim = 5, seed = 42)
  expect_identical(nrow(sims), 1000L)
  expect_identical(ncol(sims), 5L)

  rm(".Random.seed", envir = globalenv())
  sims <- simulate(m_gamm, nsim = 5, seed = 42)
  expect_identical(nrow(sims), 1000L)
  expect_identical(ncol(sims), 5L)

  rm(".Random.seed", envir = globalenv())
  sims <- simulate(sw, nsim = 5, seed = 42)
  expect_identical(nrow(sims), 12L)
  expect_identical(ncol(sims), 5L)
})

test_that("simulate() works with out a seed", {
  sims <- simulate(m_gam, nsim = 5)
  expect_identical(nrow(sims), 1000L)
  expect_identical(ncol(sims), 5L)

  sims <- simulate(m_gamm, nsim = 5)
  expect_identical(nrow(sims), 1000L)
  expect_identical(ncol(sims), 5L)

  sims <- simulate(sw, nsim = 5)
  expect_identical(nrow(sims), 12L)
  expect_identical(ncol(sims), 5L)
})

test_that("simulate() fails if we don't have an rd function", {
  skip_on_cran()

  expect_error(simulate(m_censor),
    "Don't yet know how to simulate from family <cnorm(2.42)>",
    fixed = TRUE
  )
})
