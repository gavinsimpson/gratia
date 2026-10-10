# Test methods for GJRM models
# Fit an example GJRM model using the example from ?GJRM::gamlss
skip_on_cran()
skip_if_not_installed("GJRM", minimum_version = "0.2-6")

test_that("draw works for a simple GJRM gamlss", {
  skip_if_not_installed("GJRM", minimum_version = "0.2-6")
  skip_on_cran()
  skip_on_os("win")

  # follow example from ?GJRM::gamlss
  suppressPackageStartupMessages(library("GJRM"))

  dataSim <- withr::with_seed(0, {
    n <- 400
    x1 <- round(runif(n))
    x2 <- runif(n)
    x3 <- runif(n)
    f1 <- function(x) cos(pi * 2 * x) + sin(pi * x)
    y1 <- -1.55 + 2 * x1 + f1(x2) + rnorm(n)
    data.frame(y1, x1, x2, x3)
  })
  eq.mu <- y1 ~ x1 + s(x2) + s(x3)
  eq.s <- ~ s(x3)
  fl <- list(eq.mu, eq.s)
  m_gamlss_gjrm <- GJRM::gamlss(fl, data = dataSim)
  expect_silent(plt <- draw(m_gamlss_gjrm))
  expect_doppelganger("draw-simple-gjrm-gamlss", plt)
})

test_that("GJRM layouts count smooth and parametric panels when rendering", {
  dataSim <- withr::with_seed(0, {
    n <- 100
    x1 <- round(runif(n))
    x2 <- runif(n)
    x3 <- runif(n)
    y1 <- -1.55 + 2 * x1 + cos(pi * 2 * x2) + sin(pi * x2) + rnorm(n)
    data.frame(y1, x1, x2, x3)
  })
  m <- GJRM::gamlss(list(y1 ~ x1 + s(x2), ~ s(x3, k = 6)), data = dataSim)

  # Two smooths plus the parametric x1 effect need three panels.
  plt <- draw(m)
  expect_length(plt, 3L)
  expect_equal(plt$patches$layout$ncol, 2)
  expect_equal(plt$patches$layout$nrow, 2)
  expect_silent(patchwork::patchworkGrob(plt))

  # Count only the panels drawn when parametric effects are disabled.
  plt <- draw(m, parametric = FALSE)
  expect_length(plt, 2L)
  expect_equal(plt$patches$layout$nrow, 1)
  expect_silent(patchwork::patchworkGrob(plt))

  # Keep user-supplied dimensions and let patchwork infer the other one.
  plt <- draw(m, ncol = 1)
  expect_equal(plt$patches$layout$ncol, 1)
  expect_null(plt$patches$layout$nrow)
  expect_silent(patchwork::patchworkGrob(plt))
  plt <- draw(m, nrow = 1)
  expect_equal(plt$patches$layout$nrow, 1)
  expect_null(plt$patches$layout$ncol)
  expect_silent(patchwork::patchworkGrob(plt))
})
