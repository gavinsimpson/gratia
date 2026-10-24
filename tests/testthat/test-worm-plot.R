## Test worm_plot() methods

delayedAssign("m", diagnostic_binomial_model)
delayedAssign("dat", diagnostic_binomial_data())

types <- c("deviance", "response", "pearson")
methods <- c("uniform", "simulate", "normal")

# uniform randomisation of uniform quantiles
test_that("worm_plot() uniform method works", {
  plt <- withr::with_seed(42, worm_plot(m))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("worm_plot uniform randomisation", plt)
})

test_that("worm_plot() uniform method works with response residuals", {
  plt <- withr::with_seed(42, worm_plot(m, type = "response"))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger(
    "worm_plot uniform randomisation response residuals",
    plt
  )
})

test_that("worm_plot() uniform method works with pearson residuals", {
  plt <- withr::with_seed(42, worm_plot(m, type = "pearson"))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger(
    "worm_plot uniform randomisation pearson residuals",
    plt
  )
})

# normality assumption
test_that("worm_plot() normal method works", {
  plt <- worm_plot(m, method = "normal")

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("worm_plot normality assumption", plt)
})

test_that("worm_plot() normal method works", {
  plt <- worm_plot(m, method = "normal", type = "response")

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger(
    "worm_plot normality assumption response residuals",
    plt
  )
})

test_that("worm_plot() normal method works", {
  plt <- worm_plot(m, method = "normal", type = "pearson")

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger(
    "worm_plot normality assumption pearson residuals",
    plt
  )
})

# simulate data to get quantiles
test_that("worm_plot() simulate method works", {
  plt <- withr::with_seed(42, worm_plot(m, method = "simulate"))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("worm_plot data simulation", plt)
})

test_that("worm_plot() simulate method works", {
  plt <- withr::with_seed(42, worm_plot(m,
    method = "simulate",
    type = "response"
  ))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("worm_plot data simulation response residuals", plt)
})

test_that("worm_plot() simulate method works", {
  plt <- withr::with_seed(42, worm_plot(m,
    method = "simulate",
    type = "pearson"
  ))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("worm_plot data simulation pearson residuals", plt)
})

test_that("worm_plot() fails if unsupported residuals requested", {
  expect_error(worm_plot(m, type = "scaled.pearson"),
    paste(
      "'arg' should be one of",
      paste(dQuote(types), collapse = ", ")
    ),
    fixed = TRUE
  )
})

test_that("worm_plot() fails if unsupported method requested", {
  expect_error(worm_plot(m, method = "foo"),
    paste(
      "'arg' should be one of",
      paste(dQuote(methods), collapse = ", ")
    ),
    fixed = TRUE
  )
})

test_that("worm_plot() prints message if direct method requested", {
  expect_message(worm_plot(m, method = "direct"),
    "`method = \"direct\"` is deprecated, use `\"uniform\"`",
    fixed = TRUE
  )
})

test_that("worm_plot.default fails with error", {
  expect_error(
    worm_plot(dat),
    "Diagnostic data are not available for <data.frame>"
  )
})
