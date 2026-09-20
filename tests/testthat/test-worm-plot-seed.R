test_that("worm_plot honours seeds and preserves caller RNG state", {
  withr::local_seed(42)
  d <- data.frame(x = runif(100))
  d$y <- rpois(100, exp(d$x))
  m <- gam(y ~ s(x, k = 6), family = poisson(), data = d, method = "REML")
  for (method in c("uniform", "simulate")) {
    before <- .Random.seed
    a <- worm_plot(m, method = method, seed = 123)
    expect_identical(.Random.seed, before)
    b <- worm_plot(m, method = method, seed = 123)
    c <- worm_plot(m, method = method, seed = 124)
    expect_equal(a$data, b$data)
    expect_false(identical(a$data, c$data))
    expect_identical(.Random.seed, before)
    p <- appraise(m, use_worm = TRUE, method = method, seed = 123)
    q <- appraise(m, use_worm = TRUE, method = method, seed = 123)
    expect_equal(p[[1]]$data, q[[1]]$data)
    expect_equal(p[[1]]$data, a$data)
    expect_identical(.Random.seed, before)
  }
  before <- .Random.seed
  expect_error(worm_plot(m, seed = 123, level = 2), "Level")
  expect_identical(.Random.seed, before)
  invisible(worm_plot(m, seed = NULL))
  expect_false(identical(.Random.seed, before))
  # An explicit seed must not create persistent caller state where none existed.
  saved <- .Random.seed
  rm(".Random.seed", envir = .GlobalEnv)
  invisible(worm_plot(m, seed = 123))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  assign(".Random.seed", saved, envir = .GlobalEnv)
})
