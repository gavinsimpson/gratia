test_that("parametric effects constant works", {
  withr::local_seed(42)
  d <- data.frame(x = runif(200), z = runif(200))
  d$y <- sin(6 * d$x) + 2 * d$z + rnorm(200, sd = 0.3)
  m <- gam(y ~ s(x, k = 8) + z, data = d, method = "REML")
  p <- parametric_effects(m, data = d)
  ci <- add_confint(p)
  cols <- c(".partial", ".lower_ci", ".upper_ci")
  expect_equal(add_constant(p, 10)$.partial, p$.partial + 10)
  for (col in cols) {
    expect_equal(add_constant(ci, 10)[[col]], ci[[col]] + 10)
  }
  for (input in list(p, ci)) {
    base <- draw(input)[[1]]$data
    shifted <- draw(input, constant = 10)[[1]]$data
    transformed <- draw(input, constant = 1, fun = exp)[[1]]$data
    for (col in cols) {
      expect_equal(shifted[[col]], base[[col]] + 10)
      expect_equal(transformed[[col]], exp(base[[col]] + 1))
    }
  }
  base <- assemble(m, data = d, parametric = TRUE)[["z"]]$data
  shifted <- assemble(m, data = d, parametric = TRUE, constant = 10)[["z"]]$data
  transformed <- assemble(m, data = d, parametric = TRUE,
    constant = 1, fun = exp
  )[["z"]]$data
  for (col in cols) {
    expect_equal(shifted[[col]], base[[col]] + 10)
    expect_equal(transformed[[col]], exp(base[[col]] + 1))
  }
})
