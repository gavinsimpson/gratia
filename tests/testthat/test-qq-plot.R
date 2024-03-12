## Test qq_plot() methods

## simulate binomial data...
n.samp <- 200
dat <- data_sim("eg1", n = n.samp, dist = "binary", scale = .33, seed = 0)
p <- binomial()$linkinv(dat$f) # binomial p
n <- withr::with_seed(0, sample(c(1, 3), n.samp, replace = TRUE)) # binomial n
dat <- withr::with_seed(0, transform(dat, y = rbinom(n, n, p), n = n))
m <- gam(y / n ~ s(x0) + s(x1) + s(x2) + s(x3),
  family = binomial, data = dat, weights = n,
  method = "REML"
)

types <- c("deviance", "response", "pearson")
methods <- c("uniform", "simulate", "normal")

test_that("qq_plot() uniform method works", {
  skip_if(packageVersion("mgcv") < "1.8.36")
  plt <- withr::with_seed(42, qq_plot(m)) # randomisation of uniform quantiles

  skip_on_ci()
  expect_doppelganger("qq_plot uniform randomisation", plt)
})

test_that("qq_plot() uniform method works with response residuals", {
  skip_if(packageVersion("mgcv") < "1.8.36")
  plt <- withr::with_seed(42, qq_plot(m, type = "response"))

  skip_on_ci()
  expect_doppelganger("qq_plot uniform randomisation response residuals", plt)
})

test_that("qq_plot() uniform method works with pearson residuals", {
  skip_if(packageVersion("mgcv") < "1.8.36")
  plt <- withr::with_seed(42, qq_plot(m, type = "pearson"))

  skip_on_ci()
  expect_doppelganger("qq_plot uniform randomisation pearson residuals", plt)
})

test_that("qq_plot() normal method works", {
  # normality assumption
  plt <- withr::with_seed(42, qq_plot(m, method = "normal"))

  skip_on_ci()
  expect_doppelganger("qq_plot normality assumption", plt)
})

test_that("qq_plot() normal method works", {
  plt <- withr::with_seed(42, qq_plot(m,
    method = "normal",
    type = "response"
  ))

  skip_on_ci()
  expect_doppelganger("qq_plot normality assumption response residuals", plt)
})

test_that("qq_plot() normal method works", {
  plt <- withr::with_seed(42, qq_plot(m, method = "normal", type = "pearson"))

  skip_on_ci()
  expect_doppelganger("qq_plot normality assumption pearson residuals", plt)
})

test_that("qq_plot() simulate method works", {
  # simulate data to get quantiles
  plt <- withr::with_seed(42, qq_plot(m, method = "simulate"))

  skip_on_ci()
  expect_doppelganger("qq_plot data simulation", plt)
})

test_that("qq_plot() simulate method works", {
  plt <- withr::with_seed(42, qq_plot(m,
    method = "simulate",
    type = "response"
  ))

  skip_on_ci()
  expect_doppelganger("qq_plot data simulation response residuals", plt)
})

test_that("qq_plot() simulate method works", {
  plt <- withr::with_seed(42, qq_plot(m,
    method = "simulate",
    type = "pearson"
  ))

  skip_on_ci()
  expect_doppelganger("qq_plot data simulation pearson residuals", plt)
})

test_that("qq_plot() fails if unsupported residuals requested", {
  expect_error(qq_plot(m, type = "scaled.pearson"),
    paste(
      "'arg' should be one of",
      paste(dQuote(types), collapse = ", ")
    ),
    fixed = TRUE
  )
})

test_that("qq_plot() fails if unsupported method requested", {
  expect_error(qq_plot(m, method = "foo"),
    paste(
      "'arg' should be one of",
      paste(dQuote(methods), collapse = ", ")
    ),
    fixed = TRUE
  )
})

test_that("qq_plot() prints message if direct method requested", {
  expect_message(qq_plot(m, method = "direct"),
    "`method = \"direct\"` is deprecated, use `\"uniform\"`",
    fixed = TRUE
  )
})

test_that("qq_plot.default fails with error", {
  expect_error(
    qq_plot(dat),
    "Unable to produce a Q-Q plot for <data.frame>"
  )
})

test_that("pearson_residuals fails if no var_fun available", {
  expect_error(pearson_residuals(var_fun = NULL),
    "Pearson residuals are not available for this family.",
    fixed = TRUE
  )
})
