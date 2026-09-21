test_that("smooth and raw derivatives obey the chain rule", {
  d <- transformed_data()
  m <- mgcv::gam(y ~ s(log(x), k = 7), data = d)
  nd <- data.frame(x = seq(1.5, 4.5, length.out = 10))
  a <- derivatives(m, data = nd, wrt = "smooth", type = "central")
  b <- derivatives(m, data = nd, wrt = "covariate", type = "central")
  expect_equal(b$.derivative, a$.derivative / nd$x, tolerance = 1e-6)
  expect_equal(b$.se, a$.se / nd$x, tolerance = 1e-6)
  expect_identical(attr(a, "wrt"), "smooth")
  expect_identical(attr(b, "wrt"), "covariate")
  # Independent differences of the full native prediction provide an oracle.
  h <- 1e-4
  for (ord in 1:2) {
    ans <- derivatives(m, data = nd, wrt = "covariate", type = "central", eps = h, order = ord)
    upper <- predict(m, transform(nd, x = x + h))
    lower <- predict(m, transform(nd, x = x - h))
    ref <- if (ord == 1) (upper - lower) / (2 * h) else
      (upper - 2 * predict(m, nd) + lower) / h^2
    expect_equal(unname(ans$.derivative), as.numeric(ref), tolerance = 1e-5)
  }
})

test_that("derivatives resolve local functions and require ambiguous focal inputs", {
  d <- transformed_data()
  fun <- function(x) log(x + 1)
  m <- mgcv::gam(y ~ s(fun(x), k = 5), data = d)
  expect_s3_class(derivatives(m, n = 10), "derivatives")
  expect_error(derivatives(m, data = d["x"], wrt = "covariate"), class = "gratia_expression_error")
  expect_s3_class(derivatives(m, data = d["x"], wrt = "covariate", envir = environment()), "derivatives")
  m <- mgcv::gam(y ~ s(I(x / z), k = 6), data = d)
  expect_error(derivatives(m, wrt = "covariate"), "focal")
  expect_s3_class(derivatives(m, data = d[c("x", "z")], wrt = "covariate", focal = "x"), "derivatives")
})

test_that("partial derivatives reevaluate tensor margins on the raw scale", {
  d <- transformed_data()
  m <- mgcv::gam(y ~ te(log(x), sqrt(z), k = c(4, 4)), data = d)
  nd <- data.frame(x = seq(1.5, 4.5, length.out = 10), z = 2)
  a <- partial_derivatives(m, data = nd, focal = "log(x)", type = "central")
  b <- partial_derivatives(m, data = nd, focal = "x", wrt = "covariate", type = "central")
  expect_equal(b$.partial_deriv, a$.partial_deriv / nd$x, tolerance = 1e-6)
  expect_error(partial_derivatives(m, data = nd, wrt = "covariate"), "focal")
})
