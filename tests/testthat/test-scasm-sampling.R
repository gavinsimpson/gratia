test_that("ordinary GAM sampling does not report inequality constraints", {
  m <- m_1_smooth
  expect_false(has_inequality_constraints(m))
  expect_silent(post_draws(m, n_draws = 3, seed = 2, mvn_method = "mgcv"))
  expect_silent(post_draws(m, n_draws = 3, method = "mh", burnin = 5, seed = 2))

  # Empty inequality storage is not evidence of constrained coefficients.
  m$Ain <- matrix(numeric(), nrow = 0, ncol = length(coef(m)))
  m$bin <- numeric()
  expect_false(has_inequality_constraints(m))
  expect_silent(generate_draws(m, n_draws = 3, seed = 2, mvn_method = "mgcv"))
  expect_silent(generate_draws(m, n_draws = 3, method = "mh", burnin = 5,
    seed = 2))

  # A single field and all-zero, nonrestrictive inequalities are not sufficient.
  m$Ain <- matrix(0, nrow = 1, ncol = length(coef(m)))
  expect_false(has_inequality_constraints(m))
  m$bin <- 0
  expect_false(has_inequality_constraints(m))
})

test_that("Gaussian sampling warns once about actual GAM inequalities", {
  m <- scasm_sampling_fixture()
  expect_s3_class(m, "gam")
  expect_true(has_inequality_constraints(m))
  expect_true(all(drop(m$Ain %*% coef(m)) >= m$bin - 1e-7))

  # Warning and sampling must agree for every public Gaussian entry point.
  samplers <- list(gaussian_draws, post_draws, generate_draws)
  for (sampler in samplers) {
    warnings <- character()
    draws <- withCallingHandlers(
      withr::with_seed(42, sampler(m, n_draws = 200, seed = 42,
        mvn_method = "mgcv")),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    expect_length(warnings, 1)
    expect_match(warnings, "Gaussian draws do not enforce.*inequality")
    expect_identical(dim(draws), c(200L, length(coef(m))))
    # The warning concerns a real difference from constrained inference.
    feasible <- colSums(m$Ain %*% t(draws) - m$bin < -1e-7) == 0
    expect_true(any(!feasible))
  }
})

test_that("all MH backends reject GAM inequalities before sampling", {
  m <- scasm_sampling_fixture()
  for (backend in c("mgcv", "gratia", "family")) {
    expect_error(mh_draws(m, n_draws = 2, burnin = 0, mh_backend = backend),
      "not supported.*linear inequality constraints")
    expect_error(post_draws(m, n_draws = 2, method = "mh", burnin = 0,
      mh_backend = backend), "not supported.*linear inequality constraints")
    expect_error(generate_draws(m, n_draws = 2, method = "mh", burnin = 0,
      mh_backend = backend), "not supported.*linear inequality constraints")
    # Selecting unconstrained coefficients still uses a full-model MH chain.
    expect_error(post_draws(m, n_draws = 2, method = "mh", index = 1,
      mh_backend = backend), "not supported.*linear inequality constraints")
  }
})

test_that("native constrained bootstrap draws can be supplied explicitly", {
  m <- scasm_sampling_fixture()
  draws <- t(m$bs)
  expect_true(all(colSums(m$Ain %*% t(draws) - m$bin < -1e-7) == 0))
  expect_silent(actual <- post_draws(m, n_draws = nrow(draws), method = "user",
    draws = draws))
  expect_identical(actual, draws)
  expect_silent(selected <- post_draws(m, n_draws = nrow(draws), method = "user",
    draws = draws, index = 2:3))
  expect_identical(selected, draws[, 2:3, drop = FALSE])
})
