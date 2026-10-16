test_that("AR1 posterior response draws retain correlation and segment resets", {
  withr::local_seed(62)
  d <- data.frame(x = runif(300), y = rnorm(300),
    start = rep(c(TRUE, rep(FALSE, 149)), 2))
  m <- mgcv::bam(y ~ s(x, k = 5), data = d, rho = 0.7,
    AR.start = start, discrete = TRUE)
  m$Vp[] <- 0
  # A fixed coefficient vector gives an independent oracle for the joint
  # response generator, including its exact RNG order.
  fixed <- matrix(coef(m), nrow = 1L)
  samples <- posterior_samples(m, n_draws = 1, method = "user", draws = fixed, seed = 11)
  expected <- withr::with_seed(11, response_distribution(m)$rd())
  expect_equal(samples$.response, unname(expected))
  samples <- posterior_samples(m, n_draws = 1200, method = "user",
    draws = matrix(rep(coef(m), 1200), nrow = 1200, byrow = TRUE), seed = 11)
  values <- matrix(samples$.response, nrow = nrow(d))
  errors <- sweep(values, 1L, fitted(m), `-`) / sqrt(m$sig2 / m$prior.weights)
  expect_equal(cor(errors[20, ], errors[21, ]), 0.7, tolerance = 0.07)
  expect_equal(cor(errors[150, ], errors[151, ]), 0, tolerance = 0.07)
  expect_equal(apply(errors[c(1, 20, 150, 151), ], 1L, var), rep(1, 4),
    tolerance = 0.1, ignore_attr = TRUE)
})

test_that("raw and streamed AR1 posterior checks agree across batches", {
  withr::local_seed(63)
  d <- data.frame(x = runif(120), off = runif(120), y = rnorm(120),
    start = rep(c(TRUE, rep(FALSE, 59)), 2))
  d$x[c(3, 15)] <- NA
  m <- mgcv::bam(y ~ s(x, k = 5) + offset(off), data = d, rho = 0.6,
    AR.start = start, discrete = TRUE, na.action = na.exclude)
  raw <- posterior_predictive_plot(m, n = 3, seed = 8, scalable = FALSE)
  expect_equal(raw$data$.row[raw$data$.draw == 0], which(!is.na(model_row_map(m))))
  for (size in c(1L, 2L, 5L)) {
    streamed <- posterior_predictive_plot(m, n = 3, seed = 8,
      scalable = TRUE, batch_size = size)
    expect_equal(lapply(raw$layers, `[[`, "data"), lapply(streamed$layers, `[[`, "data"))
  }
  before <- .Random.seed
  posterior_samples(m, n_draws = 2, seed = 8)
  expect_identical(.Random.seed, before)
  expect_error(posterior_samples(m, data = d, n_draws = 2), "require fitting observations")
})

test_that("working AR1 corrections do not silently produce independent PPC", {
  withr::local_seed(64)
  d <- data.frame(x = runif(80), y = rpois(80, 2))
  m <- mgcv::bam(y ~ s(x, k = 5), data = d, family = poisson(),
    rho = 0.5, discrete = TRUE)
  expect_error(posterior_samples(m, n_draws = 2, seed = 2), "working AR1 correction")
  for (scalable in c(FALSE, TRUE)) {
    expect_error(posterior_predictive_plot(m, n = 2, seed = 2, scalable = scalable),
      "working AR1 correction")
  }
})
