# uses su_eg2 & su_m_bivar_te

# setup
df <- withr::with_seed(
  seed = 123,
  data.frame(
    y  = rnorm(n),
    x1 = runif(n, 0, 10),
    x2 = runif(n, -pi, pi),
    z1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    z2 = factor(sample(letters[1:10], n, replace = TRUE))
  )
)

m_partial_deriv <- bam(
  y ~ te(x1, x2, by = z1, bs = c("tp", "cc"), k = c(4, 4)) +
    te(x1, x2, z2, by = z1, bs = c("tp", "cc", "re"), k = c(4, 4, 4), m = 1),
  family = gaussian(),
  data = df
)

# partial derivatives
test_that("partial derivatives works with single data point", {
  # data slice through te(x,z) holding z == 0.4
  ds <- data_slice(su_m_bivar_te, x = 0.0204313075, z = 0.4)
  expect_silent(
    pd_x <- partial_derivatives(
      su_m_bivar_te,
      data = ds,
      type = "central", focal = "x"
    )
  )
})

# partial derivatives
test_that("partial derivatives works with single data point sim interval", {
  # data slice through te(x,z) holding z == 0.4
  ds <- data_slice(su_m_bivar_te, x = 0.0204313075, z = 0.4)
  expect_silent(
    pd_x <- partial_derivatives(
      su_m_bivar_te,
      data = ds,
      type = "central", focal = "x", interval = "simultaneous", seed = 2
    )
  )
})

test_that("partial derivatives works with factor by", {
  expect_message(
    pd <- partial_derivatives(m_partial_deriv, focal = rep("x1", 2)),
    regexp = "Ignoring univariate smooths & those involving random effects"
  )

  expect_snapshot(pd)
})

test_that("partial derivatives works with selected factor by", {
  expect_no_message(
    pd <- partial_derivatives(
      m_partial_deriv, select = "te(x1,x2):z1A", focal = "x1"
    )
  )

  expect_snapshot(pd)
})

test_that("partial derivatives throws error with incorrect focal", {
  expect_snapshot(
    partial_derivatives(m_partial_deriv, focal = rep("x1", 1L)),
    error = TRUE
  )
})
