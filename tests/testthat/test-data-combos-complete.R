test_that("data_combos honours complete and preserves variable selection", {
  withr::local_seed(42)
  d <- data.frame(
    a = factor(rep(c("a", "a", "b"), each = 20)),
    b = factor(rep(c("x", "y", "y"), each = 20)),
    x = runif(60)
  )
  d$y <- as.integer(d$a) + as.integer(d$b) + d$x + rnorm(60)
  m <- gam(y ~ a + b + x, data = d, method = "REML")
  observed <- data_combos(m, complete = FALSE, data = d)
  complete <- data_combos(m, complete = TRUE, data = d)

  expect_equal(nrow(observed), 3L)
  expect_equal(nrow(complete), 4L)
  expect_false(any(observed$a == "b" & observed$b == "x"))
  expect_true(any(complete$a == "b" & complete$b == "x"))
  expect_equal(observed[c("a", "b")], factor_combos(m, complete = FALSE))
  expect_equal(complete[c("a", "b")], factor_combos(m, complete = TRUE))
  expect_equal(data_combos(m, data = d), complete)
  expect_equal(observed$x, rep(typical_values(m, data = d)$x, 3))
  expect_equal(
    data_combos(m, vars = c(a, x), complete = FALSE, data = d),
    observed[c("a", "x")]
  )
  expect_equal(
    data_combos(m, vars = -x, complete = FALSE, data = d),
    observed[c("a", "b")]
  )
})
