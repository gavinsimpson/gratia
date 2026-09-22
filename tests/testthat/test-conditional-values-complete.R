conditional_combos_fixture <- function() {
  withr::local_seed(42)
  d <- data.frame(
    a = factor(rep(c("a", "a", "b"), c(40, 10, 10))),
    b = factor(rep(c("x", "y", "y"), c(40, 10, 10))),
    x = runif(60)
  )
  d$y <- as.integer(d$a) + as.integer(d$b) + d$x + rnorm(60)
  list(model = gam(y ~ a + b + x, data = d, method = "REML"), data = d)
}

test_that("conditional_values filters observed factor combinations", {
  f <- conditional_combos_fixture()
  condition <- c("x", "a", "b")
  default <- conditional_values(f$model, condition, n_vals = 7)
  complete <- conditional_values(f$model, condition, n_vals = 7, complete = TRUE)
  observed <- conditional_values(f$model, condition, n_vals = 7, complete = FALSE)

  expect_identical(default, complete)
  expect_equal(nrow(complete), 28L)
  expect_equal(nrow(observed), 21L)
  expect_false(any(observed$a == "b" & observed$b == "x"))
  keep <- !(complete$a == "b" & complete$b == "x")
  # Filtering precedes prediction, so fitted_values regenerates .row.
  cols <- setdiff(names(complete), ".row")
  expect_equal(observed[cols], complete[keep, cols])
  expect_equal(observed$.row, seq_len(21))
  expect_identical(attr(observed, "channels"), attr(complete, "channels"))
  expect_s3_class(observed, "conditional_values")
  expect_identical(levels(observed$a), levels(f$data$a))
  expect_identical(levels(observed$b), levels(f$data$b))
})

test_that("observed conditions preserve numeric values and factor subsets", {
  f <- conditional_combos_fixture()
  cv <- conditional_values(f$model,
    condition = list(x = c(0.25, 0.5, 0.75), a = "b", "b"),
    complete = FALSE)
  expect_equal(cv$x, c(0.25, 0.5, 0.75))
  expect_true(all(cv$a == "b" & cv$b == "y"))

  expect_error(conditional_values(f$model,
    condition = list("x", a = "b", b = "x"), complete = FALSE),
    "No observed combinations", fixed = TRUE)
})

test_that("only conditioned factors constrain observed combinations", {
  f <- conditional_combos_fixture()
  # Projecting observed pairs onto a produces duplicate a levels; these must
  # not multiply predictions or constrain the typical value of b.
  cv <- conditional_values(f$model, c("x", "a"), n_vals = 7, complete = FALSE)
  expect_identical(cv,
    conditional_values(f$model, c("x", "a"), n_vals = 7))
  expect_equal(nrow(cv), 14L)
  expect_true(any(cv$a == "b" & cv$b == "x"))

  expect_identical(
    conditional_values(f$model, "x", n_vals = 7, complete = FALSE),
    conditional_values(f$model, "x", n_vals = 7))
  numeric_model <- gam(y ~ x, data = f$data, method = "REML")
  expect_identical(
    conditional_values(numeric_model, "x", n_vals = 7, complete = FALSE),
    conditional_values(numeric_model, "x", n_vals = 7))
})

test_that("observed combinations come from the fitted model", {
  f <- conditional_combos_fixture()
  extra <- f$data[f$data$a == "b", ]
  extra$b[] <- "x"
  supplied <- rbind(f$data, extra)
  cv <- conditional_values(f$model, c("x", "a", "b"),
    data = supplied, n_vals = 7, complete = FALSE)
  expect_equal(nrow(cv), 21L)
  expect_false(any(cv$a == "b" & cv$b == "x"))
})

test_that("conditional_values validates complete", {
  f <- conditional_combos_fixture()
  for (value in list(NA, NULL, logical(), c(TRUE, FALSE), 1, "FALSE")) {
    expect_error(conditional_values(f$model, "x", complete = value),
      "must be a single non-missing logical value", fixed = TRUE)
  }
})
