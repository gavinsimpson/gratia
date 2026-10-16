test_that("shape controls require scalar logical flags", {
  d <- withr::with_seed(904, {
    x <- seq(0, 1, length.out = 100)
    data.frame(x = x, y = exp(x) + rnorm(100, sd = 0.1))
  })
  model <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 6), data = d)
  for (flag in c("overall_uncertainty", "frequentist", "unconditional")) {
    for (value in list(1, 0, NA, logical(), c(TRUE, FALSE))) {
      args <- list(object = model, interval = "simultaneous",
        interval_method = "shape", n_sim = 20, seed = 1)
      args[[flag]] <- value
      expect_error(do.call(smooth_estimates, args), "single TRUE or FALSE")
    }
  }
  expect_error(confint(model, parm = "s(x)", type = "simultaneous",
    interval_method = "shape", nsim = 20, seed = 1, shift = 1),
    "single TRUE or FALSE")
  for (unnest in c(TRUE, FALSE)) {
    estimates <- smooth_estimates(model, n = 11, unnest = unnest)
    expect_error(draw(estimates, interval_method = "shape"),
      "require the fitted model")
    expect_error(draw(estimates, frequentist = TRUE),
      "require the fitted model")
    expect_s3_class(draw(estimates), "ggplot")
  }
})

test_that("captions distinguish pc replacements from requested shapes", {
  d <- withr::with_seed(905, {
    x <- seq(0, 1, length.out = 100)
    data.frame(x = x, y = -x + rnorm(100, sd = 0.1))
  })
  pc <- list(list(), list(x = matrix(0.5, 1, 1), matrix(1, 1, 1), -100))
  model <- mgcv::scasm(y ~ s(x, bs = "sc", xt = "m+", k = 6, pc = pc),
    data = d)
  expect_warning(info <- smooth_shape_constraints(model), "not retained")
  expect_false(info[[1]]$margins[[1]]$retained)
  expect_true(info[[1]]$linear_constraints)
  caption <- scasm_shape_caption(info[[1]], FALSE)
  expect_match(caption, "increasing; requested, not retained", fixed = TRUE)
  expect_match(caption, "general linear constraints retained", fixed = TRUE)
})
