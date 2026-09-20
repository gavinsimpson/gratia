test_that("partial derivatives focal works", {
  withr::local_seed(42)
  d <- data.frame(x = runif(200), z = runif(200), w = runif(200))
  d$y <- sin(6 * d$x) + d$z^2 + rnorm(200, sd = 0.3)
  m <- gam(y ~ te(x, z, k = c(4, 4)) + te(w, z, k = c(4, 4)),
    data = d, method = "REML"
  )
  pd <- function(...) partial_derivatives(..., n = 5, type = "central")
  a <- pd(m, select = "te(x,z)")
  b <- pd(m, select = "te(w,z)")
  both <- pd(m, focal = c("x", "w"))
  for (col in c(".partial_deriv", ".se", ".lower_ci", ".upper_ci")) {
    expect_equal(both[[col]], c(a[[col]], b[[col]]))
  }
  expect_identical(both$.focal, rep(c("x", "w"), each = 5))
  expect_identical(a$.focal, rep("x", 5))
  expect_identical(b$.focal, rep("w", 5))
  mixed <- gam(y ~ s(w, k = 5) + te(x, z, k = c(4, 4)),
    data = d, method = "REML"
  )
  expect_equal(pd(mixed), pd(mixed, select = "te(x,z)"))
  expect_error(pd(m, focal = c("w", "x")), "not in smooth")
  expect_error(pd(m, focal = c(NA_character_, "w")), "missing")
  nd <- data.frame(x = seq(0.1, 0.9, length.out = 5), z = 0.5, w = 0.5)
  expect_equal(pd(m, select = "te(x,z)", data = nd)$.focal, rep("x", 5))
  nd$w <- nd$x
  expect_error(pd(m, select = "te(x,z)", data = nd), "Problematic variables: w")
})
