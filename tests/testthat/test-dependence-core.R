test_that("native joint laws yield shared finite-ensemble calibration", {
  withr::local_seed(9)
  nr <- 9L
  d <- new_response_distribution(NULL,
    rd = function() rnorm(nr, mean = seq_len(nr) / 10), independent = FALSE,
    conditioning = "marginal")
  memory <- prepare_marginal_response_distribution(d, seed = 17, n = 49L)
  disk <- prepare_marginal_response_distribution(d, seed = 17, n = 49L, max_cells = 50)
  y <- c(-100, -1, 0, .2, .4, .7, 1, 2, 100)
  expect_equal(disk$mean, memory$mean)
  expect_equal(disk$variance, memory$variance)
  expect_equal(disk$cdf(y), memory$cdf(y))
  expect_equal(disk$qf(seq(.1, .9, length.out = nr)),
    memory$qf(seq(.1, .9, length.out = nr)))
  expect_equal(distribution_quantile_residuals(disk, y, "quantile", seed = 3),
    distribution_quantile_residuals(memory, y, "quantile", seed = 3))
  expect_true(all(is.finite(distribution_quantile_residuals(memory, y,
    "quantile", seed = 3))))
  expect_identical(memory$rd, d$rd)
  expect_identical(memory$calibration, "simulation ranks")
  before <- .Random.seed
  prepare_marginal_response_distribution(d, seed = 17, n = 19L)
  expect_identical(.Random.seed, before)
  expect_error(prepare_marginal_response_distribution(new_response_distribution(NULL),
    n = 19L), "valid joint")
})

test_that("ensemble ranks randomize ties and the held-out observation", {
  counter <- 0L
  d <- new_response_distribution(NULL, rd = function() {
    counter <<- counter + 1L
    rep((counter - 1L) %% 5L, 4L)
  })
  d <- prepare_marginal_response_distribution(d, n = 20L)
  y <- c(-1, 0, 2, 5)
  expected <- withr::with_seed(42,
    (c(0, 0, 8, 20) + runif(4) * c(1, 5, 5, 1)) / 21)
  expect_equal(distribution_quantile_residuals(d, y, "pit", seed = 42), expected)
  expect_equal(distribution_quantile_residuals(d, y, "quantile", seed = 42),
    qnorm(expected))
  expect_equal(d$cdf(y), c(0, .2, .6, 1))
  expect_equal(d$cdf_left(y), c(0, 0, .4, 1))
})

test_that("unknown joint laws keep marginal checks without independent bands", {
  d <- distribution_from_parameters("poisson", rep(2, 30))
  d$independent <- FALSE
  d$joint_available <- FALSE
  d$marginal_rd <- d$rd
  d["rd"] <- list(NULL)
  d$dependence_reason <- "working correlation only"
  model <- new_diagnostic_proxy(rep(0:4, 6), d$mean, rep(log(2), 30), poisson(), d)
  expect_identical(diagnostic_reference_method(model), "marginal")
  expect_error(diagnostic_reference_method(model, "simulate"), "Joint response")
  for (fun in list(qq_plot, worm_plot, scalable_qq_plot, scalable_worm_plot)) {
    p <- fun(model, type = "response", n_simulate = 3, seed = 8)
    expect_false(any(c("lower", "upper") %in% names(p$data)))
    expect_identical(p$labels$subtitle, "Method: marginal")
    expect_silent(ggplot2::ggplot_build(p))
    p <- fun(model, type = "quantile", n_simulate = 3, seed = 8)
    expect_false(any(c("lower", "upper") %in% names(p$data)))
    expect_identical(p$labels$subtitle, "Method: std. normal")
  }
  normal <- qq_plot(model, type = "response", method = "normal")
  expect_false("lower" %in% names(normal$data))
  expect_identical(normal$labels$subtitle, "Method: std. normal")
})
