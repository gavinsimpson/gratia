test_that("joint intervals match a direct coefficient-draw reference", {
  X <- rbind(c(1, -1), c(1, 2))
  V <- matrix(c(1, .3, .3, 2), 2)
  se <- sqrt(diag(X %*% V %*% t(X)))
  estimate <- c(3, 7)
  draws <- withr::with_seed(42, mvnfast::rmvn(1000, c(0, 0), V, ncores = 1))
  maxima <- apply(abs(sweep(draws %*% t(X), 2L, se, `/`)), 1L, max)
  crit <- quantile(maxima, .9, type = 8)
  actual <- withr::with_seed(42, simultaneous_intervals(
    estimate, se, X, V, level = .9, n_sim = 1000, block_size = 1))
  expect_equal(actual$critical, crit)
  expect_equal(actual$lower, estimate - crit * se)
  expect_equal(actual$upper, estimate + crit * se)
  for (idx in list(2:1, c(2, 1, 2, 1))) {
    other <- withr::with_seed(42, simultaneous_intervals(
      estimate[idx], se[idx], X[idx, ], V, level = .9, n_sim = 1000))
    expect_equal(other$critical, crit)
    expect_equal(other$lower, actual$lower[idx])
  }
  one <- withr::with_seed(42, simultaneous_intervals(
    estimate[1], se[1], X[1, , drop = FALSE], V, n_sim = 20000))
  expect_equal(unname(one$critical), qnorm(.975), tolerance = .04)
})

test_that("joint intervals handle deterministic, missing and empty rows", {
  X <- rbind(c(1, 1), c(0, 0), c(NA, NA))
  V <- diag(2)
  actual <- withr::with_seed(1, simultaneous_intervals(
    c(2, 3, NA), c(sqrt(2), 0, NA), X, V, n_sim = 100))
  expect_equal(actual$lower[2], 3)
  expect_equal(actual$upper[2], 3)
  expect_true(is.na(actual$lower[3]))
  withr::local_seed(2)
  before <- .Random.seed
  zero <- simultaneous_intervals(3, 0, matrix(0, 1, 2), V)
  expect_identical(zero, list(critical = 0, lower = 3, upper = 3))
  missing <- simultaneous_intervals(NA_real_, NA_real_, matrix(NA_real_, 1, 2), V)
  expect_true(all(is.na(unlist(missing))))
  empty <- simultaneous_intervals(numeric(), numeric(), matrix(numeric(), 0, 2), V)
  expect_identical(empty$lower, numeric())
  expect_true(is.na(empty$critical))
  expect_identical(.Random.seed, before)
})

test_that("joint interval inputs and covariance are validated", {
  calc <- function(...) simultaneous_intervals(1, 1, matrix(1), matrix(1), ...)
  for (x in list(NA_real_, 0, 1, c(.8, .9))) {
    expect_error(calc(level = x), "level")
  }
  expect_error(calc(n_sim = 0), "n_sim")
  expect_error(calc(n_cores = 1.5), "n_cores")
  expect_error(calc(block_size = Inf), "block_size")
  expect_error(simultaneous_intervals(1, 1, matrix(1, 1, 2), matrix(1)), "Incompatible")
  expect_error(simultaneous_intervals(1, -1, matrix(1), matrix(1)), "non-negative")
  expect_error(simultaneous_intervals(Inf, 1, matrix(1), matrix(1)), "infinite")
  expect_error(simultaneous_intervals(1, 1, matrix(1), matrix(NA_real_)), "symmetric")
  expect_error(simultaneous_intervals(1, 1, matrix(1, 1, 2),
    matrix(c(1, 0, 1, 1), 2)), "symmetric")
  expect_error(simultaneous_intervals(1, 1, matrix(1, 1, 2), matrix(1, 2, 2)),
    "positive-definite")
})

test_that("modern derivative adapters preserve the original joint calculation", {
  # Reference to the pre-refactor calculation, including one batch per smooth.
  original <- function(x, Xi, level, Vb, n_sim, ncores) {
    draws <- mvnfast::rmvn(n_sim, rep(0, nrow(Vb)), Vb, ncores = ncores)
    errors <- tcrossprod(Xi, draws)
    maxima <- apply(abs(sweep(errors, 1, x$.se, `/`)), 2, max)
    crit <- quantile(maxima, level, type = 8)
    tibble::add_column(x, .crit = rep(crit, nrow(x)),
      .lower_ci = x$.derivative - crit * x$.se,
      .upper_ci = x$.derivative + crit * x$.se)
  }
  evaluate <- function() {
    out <- list()
    for (freq in c(FALSE, TRUE)) {
      for (unc in c(FALSE, TRUE)) {
        out[[length(out) + 1L]] <- derivatives(m_gam, n = 5,
          interval = "simultaneous", n_sim = 100, ncores = 1,
          unconditional = unc, frequentist = freq)
      }
    }
    out[[length(out) + 1L]] <- partial_derivatives(su_m_bivar_te,
      data = data_slice(su_m_bivar_te, x = c(.1, .5), z = .4),
      focal = "x", interval = "simultaneous", n_sim = 100, ncores = 1)
    list(result = out, rng = .Random.seed)
  }
  actual <- withr::with_seed(44, evaluate())
  local_mocked_bindings(derivative_simultaneous_int = original)
  expected <- withr::with_seed(44, evaluate())
  expect_equal(actual, expected, tolerance = 1e-12)
})
