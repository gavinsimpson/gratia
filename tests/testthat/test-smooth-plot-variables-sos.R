test_that("spherical variable validation retains fitted transformed coordinates", {
  skip_if_not_installed("sf")
  data <- withr::with_seed(8401, {
    d <- data.frame(latitude = runif(120, -50, 50),
      longitude = runif(120, -120, 120))
    d$y <- sin(d$latitude / 40) + cos(d$longitude / 70) +
      rnorm(nrow(d), sd = 0.1)
    d
  })
  model <- mgcv::gam(y ~ s(I(latitude + 0), I(longitude + 0),
    bs = "sos", k = 10), data = data)
  variables <- smooth_variable(model$smooth[[1L]])
  expect_true(any(grepl(" ", variables, fixed = TRUE)))
  estimates <- smooth_estimates(model, variables = variables, n_2d = 5)
  expect_silent(plot <- draw(estimates, variables = variables))
  expect_s3_class(plot, "patchwork")
  expect_silent(ggplot2::ggplot_build(plot[[1L]]))

  # Names on the vector do not change the requested covariate order.
  named_variables <- stats::setNames(variables, c("latitude", "longitude"))
  expect_silent(smooth_estimates(model, variables = named_variables, n_2d = 5))
  expect_silent(draw(estimates, variables = named_variables))

  expect_error(smooth_estimates(model, variables = rev(variables), n_2d = 5),
    "cannot reorder")
  expect_error(draw(estimates, variables = rev(variables)), "cannot reorder")
})
